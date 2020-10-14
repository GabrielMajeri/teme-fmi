# Majeri Gabriel Grupa 332

import numpy as np
import matplotlib.pyplot as plt
import scipy.io.wavfile
import scipy.signal



## Exercițiul 1
print("Exercițiul 1")

sampling_rate = 2000

# Intervalul de timp între două eșantioane este T = 1/frecvență
period = 1 / sampling_rate
print(period, "secunde")

# Calculăm dimensiunea totală
# Fiecare eșantion ocupă 4 biți (jumătate de byte)
sample_size = 0.5

# Calculăm numărul total de eșantioane într-o oră de înregistrare
num_seconds = 3600
num_samples = sampling_rate * num_seconds

total_size = sample_size * num_samples

print(int(total_size), "bytes")



## Exercițiul 2
print("Exercițiul 2")

freq1 = 100
freq2 = 40

# Semnalul sumă obținut are două componente.
# Trebuie să alegem o frecvență de eșantionare care să fie cel puțin dublul
# celei mai mari frecvențe.

nyq_freq = 2 * max(freq1, freq2)

print("Minim", nyq_freq, "HZ")



## Exercițiul 3
print("Exercițiul 3")

duration = 0.03
sampling_rate = 200

# Axa Ox
timeline = np.arange(0, duration, step=0.0005)

# Coordonatele x ale eșantioanelor
num_samples = int(sampling_rate * duration) + 1
samples = np.linspace(0, duration, num_samples)

def signal_x(t):
    return np.cos(520 * np.pi * t + np.pi / 3)

def signal_y(t):
    return np.cos(280 * np.pi * t - np.pi / 3)

def signal_z(t):
    return np.cos(120 * np.pi * t + np.pi / 3)


value_x = signal_x(timeline)
value_y = signal_y(timeline)
value_z = signal_z(timeline)

samples_x = signal_x(samples)
samples_y = signal_y(samples)
samples_z = signal_z(samples)

# Afișez semnalele
fig, ax = plt.subplots(3, dpi=200)

fig.suptitle("Exemplu aliere semnale")

ax[0].set_title('x(t)')
ax[0].plot(timeline, value_x)
ax[0].scatter(samples, samples_x, c='orange')
ax[1].set_title('y(t)')
ax[1].plot(timeline, value_y)
ax[1].scatter(samples, samples_y, c='orange')
ax[2].set_title('z(t)')
ax[2].plot(timeline, value_z)
ax[2].scatter(samples, samples_z, c='orange')

# Las mai mult spațiu pe verticală între subplots
plt.subplots_adjust(hspace=1)

# Activez grid lines
for subplot in ax:
    subplot.grid(True)

plt.show()



## Exercițiul 4
print("Exercițiul 4")

signal_power_db = 90
signal_noise_ratio_db = 80

# Putem să ne folosim de proprietățile logaritmilor, ca să calculăm mai ușor:
noise_power = signal_power_db - signal_noise_ratio_db
print(noise_power, "dB")



## Exercițiul 5
print("Exercițiul 5")

rate, x = scipy.io.wavfile.read('sound.wav')
f,t,s = scipy.signal.spectrogram(x, fs=rate)

# Transformăm spectrograma în decibeli:
s = 10 * np.log10(s)

# Putem să folosim un threshold ca să rămână doar semnalele relevante:
s = np.clip(s, -5, 20)

fig = plt.figure()
plt.pcolormesh(t, f, s, shading='gouraud')
plt.ylabel('Frequency [Hz]')
plt.xlabel('Time [sec]')

plt.show()

print("Pentru a izola cele două semnale pe spectrogramă,")
print("putem folosi un algoritm de tip flood-fill,")
print("care să identifice și să umple zonele contigue din imagine.")

