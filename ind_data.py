import pandas as pd
import tensorflow as tf
import gensim
import csv

model = gensim.models.KeyedVectors.load_word2vec_format('./data/numberbatch-en.txt', binary=False)
google_model = gensim.models.KeyedVectors.load_word2vec_format('./data/GoogleNews-vectors-negative300.bin.gz', binary=True)

TYPE = ['Anagram', 'Odds', 'Evens', 'Hidden word', 'Subtext', 'Duplicate', 'Homophone', 'Example of', 'Reversal', 'Insertion', 'Subtraction', 'Charade']
with open('./data/indicators.csv', 'r') as f:
    reader = csv.reader(f)
    TRAINING = list(reader)

train_data = zip(TRAINING, TYPE)

TEST = [['weird', 'machiavellian', 'abandon', 'accident', 'blend', 'bizarre', 'changed', 'maltreated'],
        ['oddly', 'alternately'],
        ['evenly', 'regular'],
        ['engaged', 'found'],
        ['topless', 'headless', 'tops', 'heartless', 'end'],
        ['double', 'two'],
        ['according', 'speech', 'sound'],
        ['perhaps', 'eg', 'example'],
        ['backwards', 'reflected', 'reversing'],
        ['surrounding', 'encasing', 'within', 'inside'],
        ['missing', 'losing', 'scrapped', 'gone'],
        ['before', 'following']]
test_data = zip(TEST, TYPE)

def load_data(y_name='Type'):
    '''Returns the ind dataset as (train_x, train_y), (test_x, test_y).'''
    train = pd.concat([make_dataframe_for_type(words, type) for words, type in train_data])
    train_x, train_y = train, train.pop(y_name)

    test = pd.concat([make_dataframe_for_type(words, type) for words, type in test_data])
    test_x, test_y = test, test.pop(y_name)

    return (train_x, train_y), (test_x, test_y)

def train_input_fn(features, labels, batch_size):
    '''An input function for training'''
    # Convert the inputs to a Dataset.
    dataset = tf.data.Dataset.from_tensor_slices((dict(features), labels))

    # Shuffle, repeat, and batch the examples.
    dataset = dataset.shuffle(10000).repeat().batch(batch_size)

    # Return the dataset.
    return dataset

def eval_input_fn(features, labels, batch_size):
    '''An input function for evaluation or prediction'''
    features = dict(features)
    if labels is None:
        # No labels, use only features.
        inputs = features
    else:
        inputs = (features, labels)

    # Convert the inputs to a Dataset.
    dataset = tf.data.Dataset.from_tensor_slices(inputs)

    # Batch the examples
    assert batch_size is not None, 'batch_size must not be None'
    dataset = dataset.batch(batch_size)

    # Return the dataset.
    return dataset

def make_dataframe_for_type(words, type):
    type_map = {name: i for i, name in enumerate(TYPE)}
    dfs = []
    for word in words:
        word = word.lower()
        if len(word.split()) > 1:
            word = max(word.split(), key = len)
            word2 = '_'.join(word.split())
        if word in model.wv.vocab:
            x = pd.DataFrame(pd.DataFrame(model.wv[word]).T, columns = map(str, range(300)))
            x['Type'] = pd.Series([type_map[type]])
            dfs.append(x)
        if word2 in model.wv.vocab:
            x = pd.DataFrame(pd.DataFrame(model.wv[word2]).T, columns = map(str, range(300)))
            x['Type'] = pd.Series([type_map[type]])
            dfs.append(x)
        elif word in google_model.wv.vocab:
            x = pd.DataFrame(pd.DataFrame(google_model.wv[word]).T, columns = map(str, range(300)))
            x['Type'] = pd.Series([type_map[type]])
            dfs.append(x)
    return pd.concat(dfs)
