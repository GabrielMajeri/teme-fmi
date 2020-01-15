"""
Algoritm genetic care optimizează o funcție obiectiv
"""

import copy
import math
import random

## Parametrii configurabili

# Numărul de cromozomi dintr-o populație
POPULATION_SIZE = 10

# Intervalul pe care încercăm să maximizăm funcția
FUNCTION_RANGE = (-1, 2)

# Numărul de zecimale de precizie
PRECISION = 5

# Probabilitatea ca un cromozom să fie ales pentru recombinare genetică (cross over)
RECOMBINATION_PROBA = 0.5
# Probabilitatea ca un cromozom să fie ales pentru o mutație
MUTATION_PROBA = 0.25

# Pași pentru care executăm algoritmul
STEPS = 50


DECIMAL_RANGE = 10 ** PRECISION

left, right = FUNCTION_RANGE
RANGE_LEN = right - left

# Numărul de biți în care reprezentăm genomul
GENOME_LENGTH = math.ceil(math.log2(RANGE_LEN * DECIMAL_RANGE))


# Funcția pe care vrem să o maximizăm
def objective(x):
    return -x ** 2 + x + 2

# Un individ din populație cu o serie de gene
class Chromosome:
    def __init__(self):
        self.bits = [False] * GENOME_LENGTH

    def __float__(self):
        value = 0
        for bit in self.bits:
            value = (value << 1) | bit
        return RANGE_LEN * value / (2 ** GENOME_LENGTH - 1)

    def fitness(self):
        return objective(float(self))

    def flip(self, index):
        self.bits[index] = not self.bits[index]

    @staticmethod
    def random():
        chromo = Chromosome()

        bits = random.getrandbits(GENOME_LENGTH)
        for i in range(GENOME_LENGTH):
            chromo.bits[i] = bool(bits % 2)
            bits //= 2

        return chromo

    def __repr__(self):
        return f'Chromo(x={float(self):.4f})'


def selection(population):
    fitnesses = [chromo.fitness() for chromo in population]
    selected = random.choices(population, weights=fitnesses, k=len(population))
    return selected


def split_population(population, proba):
    selected = []
    not_selected = []

    for chromo in population:
        if random.random() < proba:
            selected.append(chromo)
        else:
            not_selected.append(chromo)

    return selected, not_selected


def crossover(a, b):
    split_point = random.randint(0, GENOME_LENGTH - 1)
    a.bits[:split_point], b.bits[:split_point] = b.bits[:split_point], a.bits[:split_point]


population = [Chromosome.random() for _ in range(POPULATION_SIZE)]

for step in range(STEPS):
    population = selection(population)

    selected, population = split_population(population, RECOMBINATION_PROBA)

    if len(selected) >= 2:
        if len(selected) % 2 == 1:
            a = selected.pop()
            b = copy.deepcopy(selected[-1])
            crossover(a, b)
            population.append(b)

        for i in range(0, len(selected) - 1, 2):
            crossover(selected[i], selected[i + 1])
            population += [selected[i], selected[i + 1]]
    else:
        population += selected

    selected, population = split_population(population, MUTATION_PROBA)
    for chromo in selected:
        index = random.randint(0, GENOME_LENGTH - 1)
        chromo.flip(index)
        population.append(chromo)


    best = max(population, key=Chromosome.fitness)
    print(f"Step #{step + 1} = {best.fitness():.4f} for x = {float(best):.4f}")
