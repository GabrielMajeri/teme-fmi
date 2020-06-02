import numpy as np
import librosa
from dataset import SAMPLING_RATE

def normalize_spectrogram(sg):
    "Normalizes and scales an given spectrogram"
    # Compute magnitude of complex values
    abs_sg = np.abs(sg)

    # Add an epsilon to avoid NaNs
    log_sg = np.log(abs_sg + 1e-6)

    return log_sg

# Size of the FFT window
WINDOW_SIZE = 4000

def compute_raw_spectrogram(y):
    "Computes the unscaled spectrogram of an input signal using Short-Time Fourier Transform"
    return librosa.stft(y, n_fft=WINDOW_SIZE)

def compute_spectrogram(y):
    "Computes the spectrogram of an input signal using the STFT"
    sg = compute_raw_spectrogram(y)
    return normalize_spectrogram(sg)

def compute_mel_spectrogram(y):
    "Computes the Mel-frequency scale spectrogram"
    sg = compute_raw_spectrogram(y)
    mel_sg = librosa.feature.melspectrogram(S=sg)
    return normalize_spectrogram(mel_sg)

def compute_mfcc(y):
    "Computes the Mel-frequency cepstral coeficients"
    coefs = librosa.feature.mfcc(y, sr=SAMPLING_RATE, n_mfcc=40, n_fft=WINDOW_SIZE)
    return coefs
