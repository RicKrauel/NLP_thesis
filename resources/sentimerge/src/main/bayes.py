from itertools import chain, combinations
import numpy
from scipy.stats import multivariate_normal
from scipy.optimize import minimize
logpdf = multivariate_normal.logpdf


def powerset(iterable):
    "powerset([1,2,3]) --> () (1,) (2,) (3,) (1,2) (1,3) (2,3) (1,2,3)"
    s = list(iterable)
    return list(chain.from_iterable([tuple(x) for x in combinations(s, r)] for r in range(1,len(s)+1)))


def posterior(lexicon,
              lex_list,
              initial = [0.6, 0.3, 0.3, 0.5, 0.5],
              tol = 10**-9,
              bounds = [(0.001,10),(0.001,5),(0.001,5),(0.001,5),(0.001,5)],
              verbose = True):
    """
    Finds the variances which maximise the probability of the data
    """
    overlap = []
    for entry in lexicon.values():
        sentiment = []
        source = []
        if len(entry) > 1:
            for i in range(len(lex_list)):
                try:
                    sentiment.append(entry[lex_list[i]])
                    source.append(i)
                except KeyError:
                    pass
            compressed = (tuple(source),tuple(sentiment))
            overlap.append(compressed)
    
    power = powerset(range(len(lex_list)))
    
    def likely(parameters):
        "e.g. likely([0.5, 0.1, 0.1, 0.1, 0.1])"
        #if len(parameters) != 5: raise TypeError("There are exactly four lexicons!")
        variance = parameters[0]
        error = parameters[1:]
        log_likely = 0
        covariance = {}
        for x in power:
            N = len(x)
            matrix = numpy.ones((N,N)) * variance
            for i in range(N):
                matrix[i,i] += error[x[i]]
            covariance[x] = matrix
        for source, sentiment in overlap:
            log_likely += logpdf(sentiment, mean=None, cov=covariance[source])
        return -log_likely
    
    # must use one of: L-BFGS-B, TNC and SLSQP to allow bounds
    if verbose:
        options = {'disp':True}
        callback = print
    else:
        options = {}
        callback = None
    result = minimize(likely, initial, tol=tol, method='L-BFGS-B', bounds=bounds, options=options, callback=callback)
    print(result)
    
    return result


def average(value, weight):
    """
    Calculates a weighted average
    """
    total = 0
    norm = 0
    for i in range(len(value)):
        total += value[i] * weight[i]
        norm += weight[i]
    return total / norm


def combine(lexicon, deviation):
    """
    Given variances for the component lexicons,
    estimates the latent sentiment score
    """
    weight = {x:1/y for x,y in deviation.items()}
    
    lex = {}
    lex_weight = {}
    
    for word, entry in lexicon.items():
        value_list = [0]
        weight_list = [weight['prior']]
        for source, sentiment in entry.items():
            value_list.append(sentiment)
            weight_list.append(weight[source])
        #if len(value_list) > 2:
        lex[word] = average(value_list, weight_list)
        lex_weight[word] = sum(weight_list)
    
    return (lex, lex_weight)


def save_readable(lexicon, lex_weight, filename):
    """
    Saves the lexicon in a human-readable format
    """
    with open(filename, 'w', encoding='utf8') as f:
        f.write('lemma\tPoS\tsentiment\tweight\n')
        alph = sorted(list(lexicon.items()))
        for string, sentiment in alph:
            text, pos = string.rsplit('_', 1)
            weight = lex_weight[string]
            f.write('{}\t{}\t{}\t{}\n'.format(text, pos, sentiment, weight))