import pickle
from scipy.stats import spearmanr


lex_file  = '../../data/sentimerge.pk'
eval_file = '../../data/MLSA/test_data.txt'

# Thresholds
pos_thr =  0.35
neg_thr = -pos_thr

with open(lex_file, 'rb') as f:
	lex, _ = pickle.load(f)

pred_cts = []
pred_dsc = []
test = []

convert = {'+':1, '-':-1, '0':0, '%':0, '^':0, '~':0}

with open(eval_file, 'r', encoding='utf8') as f:
	for line in f:
		lemma, answers = line.split()
		try:
			pol = lex[lemma]
		except KeyError:
			try:
				pol = lex[lemma[0].upper()+lemma[1:]]
				print(lemma)
			except KeyError:
				pol = 0
		if pol > pos_thr:
			dsc = 1
		elif pol < neg_thr:
			dsc = -1
		else:
			dsc = 0
		for ans in answers:
			pred_cts.append(pol)
			pred_dsc.append(dsc)
			test.append(convert[ans])

cts_r = spearmanr(pred_cts, test)
dsc_r = spearmanr(pred_dsc, test)

print("Cts:", cts_r)
print("Dsc:", dsc_r)