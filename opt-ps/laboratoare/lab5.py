import numpy as np
import matplotlib.pyplot as plt

## Exercițiul 1
print("Exercițiul 1")

sampling_freq = 44100
bin_distance = 1

# Distanța dintre bin-uri este de f_s / N
# Deci N este f_s / distanță
num_samples = int(sampling_freq / bin_distance)

print("Avem nevoie de", num_samples, "de eșantioane")


## Exercițiul 2
print("Exercițiul 2")

def construct_rectangular_window(width):
    "Construiește o fereastră dreptunghiulară de lățime dată"
    return np.ones(width)

def construct_hanning_window(width):
    "Construiește o fereastră Hanning de lățime dată"
    indices = np.arange(0, width)
    return 0.5 * (1 - np.cos(2 * np.pi * indices / width))

def construct_hamming_window(width):
    "Construiește o fereastră Hamming de lățime dată"
    indices = np.arange(width)
    return 0.54 - 0.46 * np.cos(2 * np.pi * indices / width)

def construct_blackman_window(width):
    "Construiește o fereastră Blackman de lățime dată"
    indices = np.arange(width)
    return (0.42 - 0.5 * np.cos(2 * np.pi * indices / width)
        + 0.08 * np.cos(4 * np.pi * indices / width))

def construct_flat_top_window(width):
    "Construiește o fereastră flat-top de lățime dată"
    indices = np.arange(width)
    return (0.22 - 0.42 * np.cos(2 * np.pi * indices / width)
        + 0.28 * np.cos(4 * np.pi * indices / width)
        - 0.08 * np.cos(6 * np.pi * indices / width)
        + 0.007 * np.cos(8 * np.pi * indices / width))

def extract_window(vector, window_width):
    "Extrage o porțiune de lungime dată de la mijlocul unui vector"
    midpoint = len(vector) // 2
    half_width = window_width // 2
    return vector[midpoint - half_width:midpoint + half_width]


print("Subpunctul a)")

# Construiesc ferestrele de dimensiunea cerută
window_width = 200
rectangular_window = construct_rectangular_window(window_width)
hanning_window = construct_hanning_window(window_width)
# Câteva ferestre în plus, pentru subpunctul c)
hamming_window = construct_hamming_window(window_width)
blackman_window = construct_blackman_window(window_width)
flat_top_window = construct_flat_top_window(window_width)

# Eșantionez sinusoida
t = np.linspace(-1, 1, 300)
frequency = 4
x = np.sin(2 * np.pi * frequency * t)

fig, ax = plt.subplots(3, 2)

ax[0][0].set_title("Sinusoida originală")
ax[0][0].plot(t, x)

# Ferestrele o să aibă doar o parte din nr. original de eșantioane
windowed_x = extract_window(x, window_width)

ax[0][1].set_title("Fereastră dreptunghiulară")
ax[0][1].plot(rectangular_window * windowed_x)

ax[1][0].set_title("Fereastră Hanning")
ax[1][0].plot(hanning_window * windowed_x)

ax[1][1].set_title("Fereastră Hamming")
ax[1][1].plot(hamming_window * windowed_x)

ax[2][0].set_title("Fereastră Blackman")
ax[2][0].plot(blackman_window * windowed_x)

ax[2][1].set_title("Fereastră flat top")
ax[2][1].plot(flat_top_window * windowed_x)

plt.tight_layout()
plt.show()


print("Subpunctul b)")

duration = 1
# Rata de eșantionare este 8000 Hz
sampling_rate = 8000
num_samples = int(sampling_rate * duration) + 1
t = np.linspace(0, duration, num_samples)

# Sinusoidă de 1000 Hz
x1 = np.sin(2 * np.pi * 1000 * t)
# Sinusoidă de 1100 Hz
x2 = np.sin(2 * np.pi * 1100 * t)

# Extrag o fereastră dreptunghiulară de lungime 1000
window_width = 1000
window = construct_rectangular_window(window_width)
windowed_x1 = window * extract_window(x1, window_width)
windowed_x2 = window * extract_window(x2, window_width)

# Aplic DFT pe ferestrele extrase
X1 = np.fft.rfft(windowed_x1)
X2 = np.fft.rfft(windowed_x2)

# Afișez grafic spectrele celor două sinusoide.
plt.plot(np.abs(X1), label='sinusoidă de frecvență 1000')
plt.plot(np.abs(X2), label='sinusoidă de frecvență 1100')

plt.legend()
plt.show()

# Observăm că la sinusoida de 1100 Hz apare foarte mult fenomenul de leakage,
# din cauza dimensiunii ferestrei.


## Exercițiul 3
print("Exercițiul 3")
# Am rezolvat exercițiul 4 (bonus) de la tema 2
