from __future__ import absolute_import
from __future__ import division
from __future__ import print_function

import argparse
import tensorflow as tf
import pandas as pd

import ind_data

parser = argparse.ArgumentParser()
parser.add_argument('--batch_size', default = 100, type = int, help = 'batch size')
parser.add_argument('--train_steps', default = 1000, type = int,
                    help='number of training steps')

def main(argv):
    args = parser.parse_args(argv[1:])

    # Fetch the data
    (train_x, train_y), (test_x, test_y) = ind_data.load_data()

    # Feature columns describe how to use the input.
    my_feature_columns = []
    for key in train_x.keys():
        my_feature_columns.append(tf.feature_column.numeric_column(key = key))

    # Build 2 hidden layer DNN with 10, 10 units respectively.
    classifier = tf.estimator.DNNClassifier(
        feature_columns = my_feature_columns,
        # Two hidden layers of 10 nodes each.
        hidden_units = [256, 128],
        # The model must choose between 3 classes.
        n_classes = 12,
        activation_fn = tf.nn.tanh)

    # Train the Model.
    classifier.train(
        input_fn = lambda:ind_data.train_input_fn(train_x, train_y,
                                                 args.batch_size),
        steps = args.train_steps)

    # Evaluate the model.
    eval_result = classifier.evaluate(
        input_fn = lambda:ind_data.eval_input_fn(test_x, test_y,
                                                args.batch_size))

    print('\nTest set accuracy: {accuracy:0.3f}\n'.format(**eval_result))

    # Generate predictions from the model
    test = ['weird', 'oddly', 'regular', 'found', 'end', 'two', 'sound', 'example', 'reversing', 'inside', 'gone', 'following']
    expected = ind_data.TYPE
    test_data = [list(ind_data.model.wv[word]) for word in test]
    predict_x = { i: [test_data[x][int(i)] for x in range(len(test))] for i in map(str, range(300)) }

    predictions = classifier.predict(
        input_fn=lambda:ind_data.eval_input_fn(predict_x,
                                                labels=None,
                                                batch_size=args.batch_size))

    template = ('\nPrediction is "{}" ({:.1f}%), expected "{}"')
    for pred_dict, expec in zip(predictions, expected):
        class_id = pred_dict['class_ids'][0]
        probability = pred_dict['probabilities'][class_id]

        print(template.format(ind_data.TYPE[class_id],
                              100 * probability, expec))


if __name__ == '__main__':
    tf.logging.set_verbosity(tf.logging.INFO)
    tf.app.run(main)
