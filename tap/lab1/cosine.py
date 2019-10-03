from collections import Counter


def get_words(path):
    """Generator which yields the words from a text file."""
    with open(path) as fin:
        for line in fin:
            yield from line.strip().split()


def build_vocabulary(path):
    """Builds the frequency vector for the words in a given file."""
    return Counter(get_words(path))


def get_length(vocab):
    squared_length = sum(freq ** 2 for freq in vocab.values())
    return squared_length ** 0.5


vocab1 = build_vocabulary('cosine1.txt')
vocab2 = build_vocabulary('cosine2.txt')


all_words = vocab1.keys() | vocab2.keys()


product = sum(vocab1[word] * vocab2[word] for word in all_words)
cos_dist = product / (get_length(vocab1) * get_length(vocab2))

print(f"{cos_dist:.4f}")
