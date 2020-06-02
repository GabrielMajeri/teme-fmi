"Data loading helper functions"

import numpy as np
import pandas as pd
from scipy.io import wavfile
from tqdm.auto import tqdm

def normalize_audio(samples):
    "Normalizes and converts to float the input 16-bit integer array"
    return samples.astype(np.float32) / 32768

def load_wav(path):
    "Load a sample wave file from disk"
    _sampling_rate, samples = wavfile.read(path)
    return normalize_audio(samples)

def load_data(data_dir, file_names):
    "Loads all .wav files from a given directory, based on their names"
    return [load_wav(data_dir / name) for name in tqdm(file_names)]

def read_labels(path, column_names=['name', 'label']):
    "Reads data labels from a CSV file"
    return pd.read_csv(path, names=column_names)

def load_labeled_data(data_dir, df):
    "Reads labeled data from a given directory"
    data = load_data(data_dir, df.name)
    labels = list(df.label)
    return data, labels
