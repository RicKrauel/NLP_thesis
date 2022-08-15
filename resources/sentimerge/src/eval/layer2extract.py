import re, pickle
from collections import defaultdict, Counter

in_file  = '../../data/MLSA/layer2.phrases.majority.txt'
txt_file = '../../data/MLSA/words.txt'
pk_file  = '../../data/MLSA/words.pk'

pattern = r'\[[^\[\]]*\][+\-#0]' # Everything within a pair of square brackets, without nesting (plus the sentiment annotation) 

sentiment = defaultdict(Counter)

with open(in_file, 'r', encoding='utf8') as f:
    for line in f:
        _, text, _ = line.split('\t')
        if text[-1] not in '+-#0': raise Exception
        if text[-2] != ']': raise Exception
        if text[0] != '[': raise Exception
        text = text[1:-2]
        while True:
            match = re.search(pattern, text)
            if not match:
                break
            else:
                text = text[:match.start()] + text[match.end():]
        for token in text.split():
            token = token.lstrip('(')
            token = token.rstrip('.,?)/')
            if len(token) < 2: continue
            if token[-1] in '+-~%^':
                polarity = token[-1]
                token = token[:-1]
            else:
                polarity = None
            if token[-3:] == 's-_':
                token = token[:-3]
            if token[-2:] == '-_': # if ending in '-_'
                token = token[:-2]
            sentiment[token][polarity] += 1

senti_dict = {item:dict(counts) for item, counts in sentiment.items()}

with open(txt_file, 'w') as f:
    for item, counts in sorted(senti_dict.items()):
        f.write('{}:\t{}\n'.format(item, list(counts.items())))

with open(pk_file, 'wb') as f:
    pickle.dump(senti_dict, f)

# remove \w[+\-~%\^][^\s\]_,]