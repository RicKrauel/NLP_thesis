from ast import literal_eval
from collections import Counter, defaultdict

old_file = '../../data/MLSA/lemmas_tagged.txt'
new_file = '../../data/MLSA/test_data.txt'

data = defaultdict(Counter)

with open(old_file, 'r', encoding='utf8') as f_in:
    for line in f_in:
        lemma, string = line.strip().split(' ', 1)
        answers = literal_eval(string)
        data[lemma].update(answers)

with open(new_file, 'w', encoding='utf8') as f_out:
    for lemma, answers in sorted(data.items()):
        if lemma[-1] == 'A':  # Convert from 'A' to 'AJ'
            lemma += 'J'
        ans_str = ''
        for pol, n in answers.items():
            if not pol:
                pol = '0'
            ans_str += pol * n
        f_out.write('{}\t{}\n'.format(lemma, ans_str))