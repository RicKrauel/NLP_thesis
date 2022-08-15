import pickle
from scipy.stats import gaussian_kde
from numpy import arange

filename = '../../data/sentimerge_backoff.pk'

with open(filename, 'rb') as f:
    data, _ = pickle.load(f)

density = gaussian_kde(list(data.values()))

for x in arange(-1.6, 1.6, 0.01):
    print(x, float(density.evaluate(x)))