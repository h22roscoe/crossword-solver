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
import progressbar

from keras.models import Sequential
from keras.layers import Dense

from sklearn.decomposition import PCA

tb_callback = keras.callbacks.TensorBoard(log_dir='./logs',
                                          write_graph = True,
                                          write_images = True)

cn_model = gensim.models.KeyedVectors.load_word2vec_format(
    './data/numberbatch-en.txt', binary = False)
# google_model = gensim.models.KeyedVectors.load_word2vec_format(
#     './data/GoogleNews-vectors-negative300.bin.gz', binary = True)

def get_data(all_words):
    inputs, labels, ws = [], [], []
    for type, words in enumerate(all_words):
        for word in words:
            word = word.lower()
            word2 = None
            if len(word.split()) > 1:
                word = max(word.split(), key = len)
                word2 = '_'.join(word.split())
            if word2 and word2 in cn_model.wv.vocab:
                inputs.append(cn_model.wv[word2])
                labels.append(type)
                ws.append(word2)
            if word in cn_model.wv.vocab:
                inputs.append(cn_model.wv[word])
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
inputs, labels, ws = get_data(indicators)

model = Sequential()
model.add(Dense(128, activation = 'tanh', input_dim = 300))
model.add(Dense(64, activation = 'tanh'))
model.add(Dense(13, activation = 'softmax'))
model.compile(optimizer = 'rmsprop',
              loss = 'categorical_crossentropy',
              metrics = ['accuracy'])

model.fit_generator(SampleEachSequence(inputs, labels, ws), callbacks = [tb_callback])

test, _, _ = get_data([['disturbed'], ['oddly'], ['regular'], ['found'], ['end'],
                       ['two'], ['sound'], ['example'], ['reversing'], ['inside'],
                       ['gone'], ['following'], ['china']])

ans = model.predict(np.asarray(test))
print(ans)
print(ans.argmax(axis = -1))

all = get_all(inputs, labels, ws)
map(lambda ls: random.shuffle(ls), all)
cut_off_all = sum(list(map(lambda ls: ls[:20], all)), [])
inputs, labels, _ = zip(*cut_off_all)

pca = PCA(n_components = 3)
principal_components = pca.fit_transform(inputs)
principal_df = pd.DataFrame(data = principal_components, columns = ['PC1', 'PC2', 'PC3'])
labels_df = pd.Series(labels, name = 'target')

final_df = pd.concat([principal_df, labels_df], axis = 1)

fig = plt.figure(figsize = (8, 8))
ax = fig.add_subplot(111, projection = '3d')
ax.set_xlabel('Principal Component 1', fontsize = 15)
ax.set_ylabel('Principal Component 2', fontsize = 15)
ax.set_ylabel('Principal Component 3', fontsize = 15)

ax.set_title('3 component PCA', fontsize = 20)
targets = types
colors = ['red', 'thistle', 'grey', 'yellow', 'blue', 'lightcyan', 'purple', 'lime', 'violet', 'green', 'pink', 'brown', 'black']
for target, color in zip(targets, colors):
    indices_to_keep = final_df['target'] == types_map[target]
    ax.scatter(final_df.loc[indices_to_keep, 'PC1'],
               final_df.loc[indices_to_keep, 'PC2'],
               final_df.loc[indices_to_keep, 'PC3'],
               c = color,
               s = 50)
ax.legend(targets)
ax.grid()

fig.savefig('./pca.png')

#model.save('./data/model.h5')
tb_callback.set_model(model)
