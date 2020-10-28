# Majeri Gabriel Grupa 332

import numpy as np
import matplotlib.pyplot as plt


## Exercițiul 1
print("Exercițiul 1")

low = 40
high = 200

# Vrem să avem și componentele de frecvență înaltă, așa că trebuie
# să eșantionăm cu dublul frecvenței Nyquist.
print("Frecvența de eșantionare trebuie să fie cel puțin:", 2 * high)
print()


## Exercițiul 2
print("Exercițiul 2")
center_frequency = 90
bandwidth = 10

print("Frecvența minimă de eșantionare:", 2 * bandwidth)

print("Intervalele de eșantionare sub-Nyquist:")

def compute_bounds(F_c, B, num_replicas):
    lower = (2 * F_c + B) / (num_replicas + 1)
    upper = (2 * F_c - B) / num_replicas
    return lower, upper

# Afișez intervalele în care trebuie să fie frecvența de eșantionare:
print(compute_bounds(center_frequency, bandwidth, 1))
print(compute_bounds(center_frequency, bandwidth, 2))
print(compute_bounds(center_frequency, bandwidth, 4))

# Frecvențele optime (calculate manual, în așa fel încât să fie
# centrate în 0 replicile):

# Scăzând 95, obțin o replică care trece de Oy dar este lipită de 0.
print("Optim pentru m = 1 este", 95.0)
# Scăzând 85 aduc replica în origine
print("Optim pentru m = 2 este", 85.0)
# Două intervale de 42.5 fac unul singur de 85
print("Optim pentru m = 4 este", 42.5)


print()


## Exercițiul 3
print("Exercițiul 3")

# Parametrii exercițiului
time_length = 1
sampling_freq = 1000
num_samples = int(time_length * sampling_freq) + 1

frequency = 10

def signal(t):
    return np.sin(frequency * 2 * np.pi * t)

# Eșantionăm semnalul
time = np.linspace(0, time_length, num_samples)
samples = signal(time)

def compute_fft(samples):
    """Funcție care calculează transformata Fourier pentru un semnal discret,
    alege o singură parte a spectrului simetric și calculează magnitudinea.
    """
    X = np.fft.fft(samples)
    X = X[:len(samples)//2]
    return np.abs(X)

X = compute_fft(samples)

# Afișez rezultatul transformării
plt.figure(dpi=200)
plt.plot(X)
plt.xlabel("ω (Hz)")
plt.ylabel("|X|")
plt.show()


## Exercițiul 4
print("Exercițiul 4")

samples = np.genfromtxt('trafic.csv')

window_size = 20
filtered = np.convolve(samples, np.ones(window_size), 'valid') / window_size

# Afișez cele două semnale pe același grafic
plt.figure(dpi=200)
plt.plot(samples, label='Original')
plt.plot(filtered, label='Filtrat')
plt.legend()
plt.show()

# Afișez pe același grafic domeniul de frecvență al semnalelor.
# Se poate vedea că semnalul filtrat a pierdut unele componente
# de frecvență înaltă.
X_orig = compute_fft(samples)
X_filt = compute_fft(filtered)

plt.figure(dpi=200)
plt.plot(X_orig, label='Original')
plt.plot(X_filt, label='Filtrat')
plt.legend()
plt.show()
