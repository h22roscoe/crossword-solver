from flask import Flask, jsonify
from gensim.models import KeyedVectors
from keras.models import load_model

import numpy as np
import tensorflow as tf

cn_model = KeyedVectors.load_word2vec_format(
    './data/numberbatch-en.txt', binary = False)
app = Flask(__name__)
model = load_model('./data/model.h5')
# model = load_model('./data/weights.0.14.h5')
model._make_predict_function()
graph = tf.get_default_graph()

@app.route('/<word>')
def get_vec(word):
    vec = None
    word = word.lower()
    word2 = None
    if len(word.split()) > 1:
        word = max(word.split(), key = len)
        word2 = '_'.join(word.split())

    if word2 and word2 in cn_model.wv.vocab:
        vec = cn_model.wv[word2]
    elif word in cn_model.wv.vocab:
        vec = cn_model.wv[word]

    if vec is not None:
        return jsonify(vec.tolist())
    else:
        return jsonify(None)

@app.route('/classify/<word>')
def classify(word):
    input = None
    word = word.lower()
    word2 = None
    if len(word.split()) > 1:
        word = max(word.split(), key = len)
        word2 = '_'.join(word.split())

    if word2 and word2 in cn_model.wv.vocab:
        input = cn_model.wv[word2]
    elif word in cn_model.wv.vocab:
        input = cn_model.wv[word]

    if input is not None:
        with graph.as_default():
            ans = list(model.predict(np.asarray([input])))[0]
            return jsonify(list(map(lambda x: 0.0 if x < 1e-6 else round(x * 100, 5), ans)))

    return 'Couldn\'t find this word in the vector space'
