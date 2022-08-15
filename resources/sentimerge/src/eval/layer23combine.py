import pickle

word_file  = '../../data/MLSA/words.pk'
lemma_file = '../../data/MLSA/lemmas.pk'
out_file   = '../../data/MLSA/lemmas_tagged_tocheck.txt' # To be finalised by hand

with open(word_file, 'rb') as f:
    words = pickle.load(f)

with open(lemma_file, 'rb') as f:
    easy, hard = pickle.load(f)

with open(out_file, 'w', encoding='utf8') as f:
    for item, sentiment in sorted(words.items()):
        if item in easy and easy[item][:-2] != '&amp;lt;unknown&amp;gt;':
            print(easy[item], sentiment, file=f)
        elif item in easy or item in hard:
            print('#', item, sentiment, file=f)
        else:
            print(item)