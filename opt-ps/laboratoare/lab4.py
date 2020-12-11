import numpy as np
import matplotlib.pyplot as plt
from matplotlib.animation import FuncAnimation


# Funcția de aproximat
def f(t):
    # Formă de fierăstrău
    sawtooth = t - np.floor(t)

    # O sinusoidă defazată
    sine = np.sin(t + 0.25)

    # Zgomot
    np.random.seed(5)
    noise = 0.5 * np.random.random(size=t.shape)

    # Adun funcțiile componente
    return sawtooth + sine + noise


# Eșantionez funcția
duration = 2 * np.pi
sampling_rate = 30
num_samples = int(sampling_rate * duration) + 1

t = np.linspace(0, duration, num_samples)
x = f(t)

# Calculez DFT
X = np.fft.rfft(x)

# Reconstruiesc funcția
total = np.zeros(num_samples)

# Cod bazat pe https://matplotlib.org/3.3.3/api/animation_api.html

fig = plt.figure('Animație Fourier')


plt.plot(t, x, label='funcția originală')

# Elementele care vor fi animate
line, = plt.plot(t, np.zeros_like(t), c='orange')
text1 = plt.text(.15, -0.05, '', fontsize=11)
text2 = plt.text(.15, -0.35, '', fontsize=13)


def update(frame):
    # Reconstruiesc funcția folosind primele `frame` componente
    value = np.fft.irfft(X[:frame], n=num_samples)

    line.set_data(t, value)
    line.set_label(f'aproximare cu primele {frame} componente')

    # Regenerez legenda
    legend = plt.legend()

    c = X[frame]

    text1.set_text(f'Componenta {frame}:')
    text2.set_text(fr'$e^{{2 \pi i \cdot {frame} t ({c:.2f})}}$')

    return line, legend, text1, text2

animation = FuncAnimation(
    fig, update,
    frames=range(num_samples // 2),
    blit=True)

# Salvare animație în fișier
# animation.save('animatie_fourier.mp4')

plt.show()
