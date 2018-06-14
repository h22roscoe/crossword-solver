import matplotlib
matplotlib.use('Agg')
import matplotlib.pyplot as plt
from mpl_toolkits.mplot3d import Axes3D

import numpy as np
import pandas as pd

import csv
import random
import gensim
import keras
import requests

from keras.models import Sequential, Model
from keras.layers import Dense
from keras.models import load_model

from sklearn.decomposition import PCA

# cn_model = gensim.models.KeyedVectors.load_word2vec_format(
#     './data/numberbatch-en.txt', binary = False)
# google_model = gensim.models.KeyedVectors.load_word2vec_format(
#     './data/GoogleNews-vectors-negative300.bin.gz', binary = True)

def get_vec(word):
    ans = None
    if word is not None:
        r = requests.get('http://127.0.0.1:5000/' + word)
        ans = r.json()
        if ans is not None:
            ans = np.asarray(ans)
    return ans

def get_data(all_words):
    inputs, labels, ws = [], [], []
    for type, words in enumerate(all_words):
        for word in words:
            word = word.lower()
            word2 = None
            if len(word.split()) > 1:
                word = max(word.split(), key = len)
                word2 = '_'.join(word.split())
            w2vec = get_vec(word2)
            if word2 and w2vec is not None:# word2 in cn_model.wv.vocab:
                inputs.append(w2vec)#cn_model.wv[word2])
                labels.append(type)
                ws.append(word2)
            wvec = get_vec(word)
            if wvec is not None:#word in cn_model.wv.vocab:
                inputs.append(wvec)#cn_model.wv[word])
                labels.append(type)
                ws.append(word)
            # elif word in google_model.wv.vocab:
            #     inputs.append(google_model.wv[word])
            #     labels.append(type)
    return inputs, labels, ws

def get_all(inputs, labels, ws):
    anagrams = [(inputs[i], labels[i], ws[i]) for i, x in enumerate(labels) if x == types_map['Anagram']]
    odds = [(inputs[i], labels[i], ws[i]) for i, x in enumerate(labels) if x == types_map['Odds']]
    evens = [(inputs[i], labels[i], ws[i]) for i, x in enumerate(labels) if x == types_map['Evens']]
    hidden_words = [(inputs[i], labels[i], ws[i]) for i, x in enumerate(labels) if x == types_map['Hidden word']]
    subtexts = [(inputs[i], labels[i], ws[i]) for i, x in enumerate(labels) if x == types_map['Subtext']]
    dups = [(inputs[i], labels[i], ws[i]) for i, x in enumerate(labels) if x == types_map['Duplicate']]
    homophones = [(inputs[i], labels[i], ws[i]) for i, x in enumerate(labels) if x == types_map['Homophone']]
    eg_ofs = [(inputs[i], labels[i], ws[i]) for i, x in enumerate(labels) if x == types_map['Example of']]
    reversals = [(inputs[i], labels[i], ws[i]) for i, x in enumerate(labels) if x == types_map['Reversal']]
    insertions = [(inputs[i], labels[i], ws[i]) for i, x in enumerate(labels) if x == types_map['Insertion']]
    subtractions = [(inputs[i], labels[i], ws[i]) for i, x in enumerate(labels) if x == types_map['Subtraction']]
    charades = [(inputs[i], labels[i], ws[i]) for i, x in enumerate(labels) if x == types_map['Charade']]
    nones = [(inputs[i], labels[i], ws[i]) for i, x in enumerate(labels) if x == types_map['None']]
    return [anagrams, odds, evens, hidden_words, subtexts, dups, homophones,
            eg_ofs, reversals, insertions, subtractions, charades, nones]

class SampleEachSequence(keras.utils.Sequence):
    def __init__(self, inputs, labels, ws):
        self.all = get_all(inputs, labels, ws)

    def __len__(self):
        return 1000

    def __getitem__(self, idx):
        all_samples = sum(list(map(lambda ls: random.sample(ls, 4), self.all)), [])
        random.shuffle(all_samples)
        x_batch, y_batch, _ = zip(*all_samples)
        return np.asarray(x_batch), keras.utils.to_categorical(np.asarray(y_batch), num_classes = 13)

def plot_pca(inputs, labels, title):
    pca = PCA(n_components = 2)
    principal_components = pca.fit_transform(inputs)
    print("Explained variance ratio:", pca.explained_variance_ratio_)
    principal_df = pd.DataFrame(data = principal_components, columns = ['PC1', 'PC2'])
    labels_df = pd.Series(labels, name = 'target')

    final_df = pd.concat([principal_df, labels_df], axis = 1)

    fig = plt.figure(figsize = (8, 8))
    ax = fig.add_subplot(111)
    ax.set_xlabel('Principal Component 1', fontsize = 15)
    ax.set_ylabel('Principal Component 2', fontsize = 15)
    # ax.set_zlabel('Principal Component 3', fontsize = 15)

    ax.set_title(title, fontsize = 20)
    targets = types
    colors = ['red', 'thistle', 'grey', 'yellow', 'blue', 'lightcyan', 'purple', 'lime', 'violet', 'green', 'pink', 'brown', 'black']
    for target, color in zip(targets, colors):
        indices_to_keep = final_df['target'] == types_map[target]
        ax.scatter(final_df.loc[indices_to_keep, 'PC1'],
                   final_df.loc[indices_to_keep, 'PC2'],
                   # final_df.loc[indices_to_keep, 'PC3'],
                   c = color,
                   s = 50)
    ax.legend(targets)
    ax.grid()

    fig.savefig('./outputs/{}.pdf'.format(title))

types = ['Anagram', 'Odds', 'Evens', 'Hidden word', 'Subtext', 'Duplicate',
         'Homophone', 'Example of', 'Reversal', 'Insertion', 'Subtraction',
         'Charade', 'None']
types_map = {name: i for i, name in enumerate(types)}

with open('./data/indicators.csv', 'r') as f:
    reader = csv.reader(f)
    indicators = list(reader)

all_indicators = sum(indicators, [])

with open('./data/google-10000-english-no-swears.txt') as f:
    popular_words = f.read().splitlines()

nones = [word for word in popular_words if word not in all_indicators]

indicators.append(nones)
for type in indicators:
    random.shuffle(type)

train_inds, test_inds = [], []
for type in indicators:
    l = len(type)
    train_inds.append(type[:int(l * 0.8)])
    test_inds.append(type[int(l * 0.8):])

inputs, labels, ws = get_data(train_inds)

# model = Sequential()
# model.add(Dense(128, activation = 'tanh', input_dim = 300))
# model.add(Dense(64, activation = 'tanh'))
# model.add(Dense(13, activation = 'softmax'))
# model.compile(optimizer = 'rmsprop',
#               loss = 'categorical_crossentropy',
#               metrics = ['accuracy'])

model = load_model('./data/weights.0.14.h5')

# cb = keras.callbacks.ModelCheckpoint('./data/weights.{loss:.2f}.h5', monitor = 'loss', save_best_only = True)

model.summary()

# model.fit_generator(SampleEachSequence(inputs, labels, ws), epochs = 1, callbacks = [cb])

test_inputs, test_labels, test_ws = get_data(test_inds)

ans = model.predict(np.asarray(test_inputs))

successes = 0
total = 0
for a, label in zip(list(ans), test_labels):
    if a[label] > 0.1:
        successes += 1
    total += 1

print(successes / total)

# all = get_all(inputs, labels, ws)
# for ls in all:
#     random.shuffle(ls)
# cut_off_all = sum(list(map(lambda ls: ls[:8], all)), [])
# inputs, labels, _ = zip(*cut_off_all)
# plot_pca(inputs, labels, 'Input vector space in 2D')
#
# new_model = Model(model.inputs, model.layers[-2].output)
# new_model.summary()
# results = new_model.predict(np.asarray(inputs))
# plot_pca(results, labels, 'Latent vector space in 2D')

#model.save('./data/model.h5')
