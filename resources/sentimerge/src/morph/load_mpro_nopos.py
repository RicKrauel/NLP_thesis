import pickle
from collections import Counter

input_file  = '../../data/morph/mpro_out.txt'
output_file = '../../data/morph/preproc_nopos.txt'
pickle_file = '../../data/morph/preproc_nopos.pk'

data = dict()

def bare(text):
    text = text.strip('_()0123456789')
    text = ''.join(text.split('$'))
    text = ''.join(text.split('_'))
    text = ''.join(text.split('+'))
    return text

with open(input_file, 'r') as f:
    for line in f:
        line = line.strip()
        if line != '.':  # Store lines that aren't '.'
            text = line
            continue
        if text[0] == ',':  # Ignore items that weren't analysed as a whole
            continue
        outputs = text.strip('{}').split('};{')  # Find all analyses
        featsets = []  # Save analyses as dictionaries
        for thing in outputs:
            terms = thing.split(',')
            feats = {}
            for x in terms:
                feature, _, value = x.partition('=')
                feats[feature] = value
            featsets.append(feats)
        
        # need to somehow pick one analysis? (or use them all?)
        first = featsets[0]
        ori = first['ori'].lower()
        lemmas = [bare(x) for x in first['ls'].split('#')]
        data[ori] = lemmas

# Check if lemmas occur on their own, or more than once
bound = set()
count = Counter()
single = set()

for string, lemmas in data.items():
    if len(lemmas) > 1:
        count.update(lemmas)
        for lem in lemmas:
            if not lem in data:
                bound.add(lem)

single = {x for x in count if count[x] == 1}

"""
print(len(data))
print(len([x for x in data if len(data[x]) > 1]))
print(len(bound))
print(len(single))
"""

for lem in bound - single:
    print(lem)

# Save lemmas
with open(output_file, 'w') as f:
    for string, lemmas in data.items():
        if len(lemmas) > 1:
            for lem in lemmas:
                if lem in single:
                    break
            else:
                f.write(string + '\t' + ','.join(lemmas) + '\n')

with open(pickle_file, 'wb') as f:
    pickle.dump(data, f)