import pickle
from main.bayes import save_readable

main_file = '../../data/sentimerge_nospin.pk'
spin_file = '../../data/sentimerge.pk'
backoff_pickle = '../../data/sentimerge_backoff.pk'
backoff_text   = '../../data/sentimerge_backoff.txt'

with open(main_file, 'rb') as f:
    lex, wei = pickle.load(f)

with open(spin_file, 'rb') as f:
    lex_spin, wei_spin = pickle.load(f)


for item in lex_spin:
    if not item in lex:
        lex[item] = lex_spin[item]
        wei[item] = wei_spin[item]


with open(backoff_pickle, 'wb') as f:
    pickle.dump((lex, wei), f)

save_readable(lex, wei, backoff_text)