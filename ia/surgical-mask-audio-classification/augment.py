"Data augmentation functions"

from dataset import SAMPLING_RATE

import librosa
import numpy as np

# Maximum amount of time shifting to apply.
MAX_SHIFT = SAMPLING_RATE // 4

def random_time_shift(y):
    "Randomly shifts the audio samples forward or backwards, with wrap-around"
    shift = np.random.randint(-MAX_SHIFT, +MAX_SHIFT)
    return np.roll(y, shift)

# Time stretching rates to use
TIME_STRETCH_RATES = [0.8, 0.9, 1.1, 1.25]

def random_time_stretch(y):
    "Randomly stretches the audio without changing pitch"
    rate = np.random.choice(TIME_STRETCH_RATES)
    return librosa.effects.time_stretch(y, rate)

# How many half-steps to shift by
PITCH_SHIFT_STEPS = [-3.5, -2.5, -2, -1, 1, 2, 2.5, 3.5]

def random_pitch_shift(y):
    "Randomly shifts the pitch without changing the length of the audio"
    n_steps = np.random.choice(PITCH_SHIFT_STEPS)
    return librosa.effects.pitch_shift(y, SAMPLING_RATE, n_steps)

# How much % noise to mix
MAX_NOISE_PCT = 0.01

def random_noise(y):
    "Adds some random noise to the input signal"
    noise = np.random.randn(len(y)).astype(np.float32)
    alpha = np.random.uniform(0, MAX_NOISE_PCT)
    return (1 - alpha) * y + alpha * noise

def augment_sample(y):
    "Augments a sample by randomly modifying it"
    y = random_time_shift(y)
    y = random_time_stretch(y)
    y = random_pitch_shift(y)
    y = random_noise(y)

    y = librosa.util.fix_length(y, size=SAMPLING_RATE)
    return y

def mask_frequency(sg, start, end):
    "Mask a frequency range in the spectrogram"
    sg[start:end, :] = 0

def mask_time(sg, start, end):
    "Mask an interval of time in the spectrogram"
    sg[:, start:end] = 0

# How many frequency domain (horizontal) bands to create
NUM_FREQ_MASKS = 2
# How many time domain (verticla) bands to create
NUM_TIME_MASKS = 2
# Maximum length of band, as a percentage of the respective axis
MAX_MASK_PERCENT = 0.1

def augment_spectrogram(sg):
    """Augments a spectrogram by randomly masking out time and frequency ranges,
    as described in the "SpecAugment" paper.
    """
    sg = np.copy(sg)

    freq_len, time_len = sg.shape
    max_freq_len = np.ceil(MAX_MASK_PERCENT * freq_len)
    max_time_len = np.ceil(MAX_MASK_PERCENT * time_len)

    for _ in range(NUM_FREQ_MASKS):
        size = np.random.randint(max_freq_len)
        start = np.random.randint(freq_len - size)
        mask_frequency(sg, start, start + size)

    for _ in range(NUM_TIME_MASKS):
        size = np.random.randint(max_time_len)
        start = np.random.randint(time_len - size)
        mask_time(sg, start, start + size)

    return sg
