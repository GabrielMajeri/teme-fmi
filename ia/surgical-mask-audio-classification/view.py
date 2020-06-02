from librosa.display import specshow, waveplot
import matplotlib.pyplot as plt

def display_wave_form(samples):
    waveplot(samples)
    plt.show()
