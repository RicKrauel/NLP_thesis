import pickle

in_file = '../../data/raw_lexicon.pk'

GE = 'german'
PC = 'GermanPC'
WS = 'GermanSentiWS'
SP = 'GermanSentiSpin'

out_file = {GE:'../../data/lexicon__ge.pk',
            PC:'../../data/lexicon__pc.pk',
            WS:'../../data/lexicon__ws.pk',
            SP:'../../data/lexicon__sp.pk'}

print('Loading data...')
with open(in_file, 'rb') as f:
    lexicon = pickle.load(f)

print('Filling in gaps...')
for entry in lexicon.values():
    for source, sentiment in entry.items():
        if type(sentiment) == str:
            if sentiment[:3] == 'POS':
                entry[source] = 1
            else:
                entry[source] = -1

print('Filtering and pickling data...')
filtered = {}
for source in [GE,PC,WS,SP]:
    new = {word:entry[source] for word,entry in lexicon.items() if source in entry}
    with open(out_file[source], 'wb') as f:
        pickle.dump(new, f)

print('Done!')