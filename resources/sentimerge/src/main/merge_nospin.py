from lexicon_convert import lexicon_convert
from stats import stats, stats_dash
from reweight import reweight, reweight_dash
from bayes import posterior, combine, save_readable

import pickle

GE = 'german'
PC = 'GermanPC'
WS = 'GermanSentiWS'
SP = 'GermanSentiSpin'

full_lex = lexicon_convert('../../data/merged_lex_hash.pm')

# Remove SentiSpin
lex = {}
for item, vals in full_lex.items():
    if SP in vals:
        if len(vals) == 1:
            continue
        else:
            del vals[SP]
    lex[item] = vals

rms = stats(lex, (GE, PC, WS))
reweight(lex, rms)
new = stats_dash(lex, (GE, WS))
reweight_dash(lex, new)

inf = posterior(lex, (GE, PC, WS), tol=10**-9,
                bounds = ((0.001,10),(0.001,5),(0.001,5),(0.001,5)),
                initial = (0.6, 0.3, 0.3, 0.3))

deviation = {'prior':inf['x'][0],
                  GE:inf['x'][1],
                  PC:inf['x'][2],
                  WS:inf['x'][3]}

new_lex, weight = combine(lex, deviation)

with open('../../data/sentimerge_nospin.pk', 'wb') as f:
    pickle.dump((new_lex, weight), f)

save_readable(new_lex, weight, '../../data/sentimerge_nospin.txt')