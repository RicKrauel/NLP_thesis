#*-* utf8 *-*
from collections import defaultdict
import pickle

in_file  = '../../data/MLSA/layer3.annotation.merged.xml'
txt_file = '../../data/MLSA/lemmas.txt'
pk_file  = '../../data/MLSA/lemmas.pk'

lex_pos = {'A':['ADJA', 'ADJD'], 'N':['NN'], 'V':['VVFIN', 'VVIMP', 'VVINF', 'VVIZU', 'VVPP']}
new_pos = {old:new for new, many in lex_pos.items() for old in many}

conversion = defaultdict(set)
#pos_set = defaultdict(set)

with open(in_file, 'r', encoding='utf8') as f:
    for line in f:
        if line[:8] != "\t\t\t\t\t<t ":
            continue
        props = line[8:-3].split()
        lemma = props[1].split('"')[1].lower()
        word  = props[2].split('"')[1]
        try:
            pos   = new_pos[props[3].split('"')[1]]
        except KeyError:
            continue
        entry = lemma + '_' + pos
        conversion[word].add(entry)
        #pos_set[pos].add(lemma)

easy_lemmas = {word:lemma for word, entries in conversion.items() if len(entries) == 1 for lemma in entries}
hard_lemmas = {word       for word, entries in conversion.items() if len(entries)  > 1}

with open(pk_file, 'wb') as f:
    pickle.dump((easy_lemmas, hard_lemmas), f)

with open(txt_file, 'w', encoding='utf8') as f:
    f.write("Hard lemmas:\n\n")
    for word in sorted(hard_lemmas):
        f.write("{}\t{}\n".format(word, sorted(conversion[word])))
    f.write("\nEasy lemmas:\n\n")
    for word, lemma in sorted(easy_lemmas.items()):
        f.write("{}\t{}\n".format(word, lemma))

#for x in sorted(pos_set):
#    print(x, sorted(pos_set[x])[:100])

# Conclusion on PoS:
# A: ADJA, ADJD auf jeden fall; ADV eventuell
# N: NN, vielleicht auch NE, wahrscheinlich nicht FM
# V: VVFIN, VVIMP, VVINF, VVINZU, VVPP (wahrscheinlich nicht VAFIN, VAINF, VAPP, VMFIN, VMINF - auxiliaries and modals)

### Fix multiple lemmas: 
# Discard: B, RRB-, bzw, meinen, LRB-, H, Selbst
# Correct: reich_A, groß_A, Steigerung_N, wirklich_A, Nachteil_N, frei_A, Handlung_N, Schmerz_N, Mitgefühl_N, zurückziehen_V, wunderbar_A, stark_A, begabt_A
# Both: stellt, stellen, stellten, zeichnen, bekannt, Willen, bewahren, löst, gehe, kommt, vollkommen