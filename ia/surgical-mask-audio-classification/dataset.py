from pathlib import Path
import numpy as np
from load import read_labels, load_labeled_data, load_data

# All audio samples have the same sample rate
SAMPLING_RATE = 16000

# Directory of the Kaggle dataset
DATA_DIR = Path('./data/')

def load_training_dataset():
    print("Loading provided training dataset")
    train_df = read_labels(DATA_DIR / 'train.txt')
    return load_labeled_data(DATA_DIR / 'train' / 'train', train_df)

def load_validation_dataset():
    print("Loading provided validation dataset")
    validation_df = read_labels(DATA_DIR / 'validation.txt')
    return load_labeled_data(DATA_DIR / 'validation' / 'validation', validation_df)

def load_labeled_dataset():
    train_data, train_labels = load_training_dataset()
    validation_data, validation_labels = load_validation_dataset()

    # Merge all labeled data into a single array
    data = np.stack(train_data + validation_data)
    labels = np.stack(train_labels + validation_labels)

    print("Loaded", len(data), "labeled samples")

    return data, labels

def load_test_dataset():
    print("Loading unlabeled dataset")

    test_df = read_labels(DATA_DIR / 'test.txt', column_names=['name'])

    # Index by name to make sure pandas doesn't
    # add an extra column in the submission
    test_df = test_df.set_index('name')

    return test_df, load_data(DATA_DIR / 'test' / 'test', test_df.index)
