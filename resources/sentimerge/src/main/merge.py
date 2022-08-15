from lexicon_convert import lexicon_convert
from stats import stats, stats_dash
from reweight import reweight, reweight_dash
from bayes import posterior, combine, save_readable

import pickle

GE = 'german'
PC = 'GermanPC'
WS = 'GermanSentiWS'
SP = 'GermanSentiSpin'

lex = lexicon_convert('../../data/merged_lex_hash.pm', verbose=False)
rms = stats(lex, (GE, PC, WS, SP), verbose=False)
reweight(lex, rms)
new = stats_dash(lex, (GE, WS, SP), verbose=False)
reweight_dash(lex, new)

with open('../../data/premerge.pk', 'wb') as f:
    pickle.dump(lex, f)

inf = posterior(lex, (GE, PC, WS, SP), tol=10**-9, initial=[ 0.52847201,  0.32771543,  0.31697963,  0.44575393,  0.60947068])

deviation = {'prior':inf['x'][0],
                  GE:inf['x'][1],
                  PC:inf['x'][2],
                  WS:inf['x'][3],
                  SP:inf['x'][4]}

new_lex, weight = combine(lex, deviation)

with open('../../data/sentimerge.pk', 'wb') as f:
    pickle.dump((new_lex, weight), f)

save_readable(new_lex, weight, '../../data/sentimerge.txt')