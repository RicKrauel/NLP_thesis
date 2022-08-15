input_file  = '../../data/morph/mpro_out.txt'
output_file = '../../data/morph/preproc.txt'

data = dict()
tagged = dict()

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
        
        # PoS information for single lemmas        
        for fset in featsets:
            ori = fset['ori'].lower()
            tag = fset['c'][0].upper()
            lemmas = fset['ls'].split('#')
            if len(lemmas) == 1 and ori == bare(lemmas[0]):
                tagged.setdefault(ori, set()).add(tag)
        
        # need to somehow pick one analysis? (or use them all?)
        first = featsets[0]
        ori = first['ori'].lower()
        tag = first['c'][0].upper()
        lemmas = [bare(x) for x in first['ls'].split('#')]
        if len(lemmas) > 1:
            data[ori+'_'+tag] = lemmas

# Pick a PoS for each ambiguous lemma
for lemma, tags in tagged.items():
    if len(tags) == 1:
        tagged[lemma] = tags.pop()
    else:
        if 'V' in tags and (
                lemma[-2:] == 'en' or
                lemma[-3:] == 'ern' or
                lemma[-3:] == 'eln' ):
            tagged[lemma] = 'V'
        else:  # Perhaps I should go through this by hand...
            if 'A' in tags:
                tagged[lemma] = 'A'
            elif 'N' in tags:
                tagged[lemma] = 'N'
            elif 'V' in tags:
                tagged[lemma] = 'V'
            else:
                raise Exception

# Add PoS information to lemmas
new_data = {}
all_lems = set()

for string, lemmas in data.items():
    try:
        new_lemmas = [x+'_'+tagged[x] for x in lemmas]
    except KeyError:
        pass
        #print(string, lemmas)
    else:
        new_data[string] = new_lemmas
        all_lems |= set(new_lemmas)
        print(string, new_lemmas)

print(len(new_data))
print(len(all_lems))

# Save lemmas
with open(output_file, 'w') as f:
    for string, lemmas in new_data.items():
        f.write(string + '\t' + ','.join(lemmas) + '\n')