"Experiments to determine best data processing method"
import numpy as np
from tqdm.contrib.concurrent import process_map

from dataset import load_labeled_dataset
from models import cross_validate_model, create_logistic_regression_model
from spectrogram import compute_spectrogram, compute_mel_spectrogram, compute_mfcc

data, labels = load_labeled_dataset()

# Choose a method to test
extract_features = compute_mfcc

def process_data(sample):
    "Helper function for parallel feature extraction"
    features = extract_features(sample)
    # Linearize the feature matrix
    return features.ravel()

print("Converting data using", extract_features.__name__)
data = process_map(process_data, data, max_workers=4)
data = np.stack(data)

clf = create_logistic_regression_model()
cross_validate_model(clf, data, labels, n_folds=2)
