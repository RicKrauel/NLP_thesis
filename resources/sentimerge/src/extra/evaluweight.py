import pickle

lex_file  = '../../data/sentimerge.pk'
eval_file = '../../data/MLSA/test_data.txt'

# Thresholds
pos_thr =  0.25
neg_thr = -pos_thr
pos_aim =  0.8
neg_aim = -pos_aim
pos_norm = 1/(pos_aim - pos_thr)
neg_norm = 1/(neg_thr - neg_aim)
neu_norm = 2/(pos_thr - neg_thr)

with open(lex_file, 'rb') as f:
	lex, _ = pickle.load(f)
for lemma, pol in list(lex.items()):
	lex[lemma.split('_')[0]] = pol

# Evaluation counts
pos_true = 0
pos_fail = 0 # false positive
pos_miss = 0 # false negative
neg_true = 0
neg_fail = 0
neg_miss = 0

#N_oov = 0
#N_ambig = 0
with open(eval_file, 'r', encoding='utf8') as f:
	for line in f:
		lemma, answers = line.split()
		ans_types = set()
		try:
			pol = lex[lemma]
		except KeyError:
			try:
				pol = lex[lemma[0].upper()+lemma[1:]]
				print(lemma)
			except KeyError:
				#try:
				#	pol = lex[lemma.split('_')[0]]
				#except KeyError:
				pol = 0
				"""if '+' in answers or '-' in answers:
					N_oov += 1
					print(lemma)"""
		if pol > pos_thr:
			pred = 1
			w = (pol - pos_thr) * pos_norm
		elif pol < neg_thr:
			pred = -1
			w = (neg_thr - pol) * neg_norm
		else:
			pred = 0
			w = abs(pol) * neu_norm
		for ans in answers:
			if ans == '+':
				#ans_types.add(1)
				if pred == 1:
					pos_true += w
				else:
					pos_miss += w
					if pred == -1:
						neg_fail += w
			elif ans == '-':
				#ans_types.add(-1)
				if pred == -1:
					neg_true += w
				else:
					neg_miss += w
					if pred == 1:
						pos_fail += w
			else:
				#ans_types.add(0)
				if pred == 1:
					pos_fail += w
				elif pred == -1:
					neg_fail += w
		#if len(ans_types) > 1: N_ambig += 1; print(lemma)
#print(N_oov)
#print(N_ambig)

pos_precis = pos_true / (pos_true + pos_fail)
pos_recall = pos_true / (pos_true + pos_miss)
pos_fscore = 2 * pos_precis * pos_recall / (pos_precis + pos_recall)
neg_precis = neg_true / (neg_true + neg_fail)
neg_recall = neg_true / (neg_true + neg_miss)
neg_fscore = 2 * neg_precis * neg_recall / (neg_precis + neg_recall)

ave_fscore = (pos_fscore + neg_fscore) / 2

print(pos_precis)
print(pos_recall)
print(pos_fscore)
print(neg_precis)
print(neg_recall)
print(neg_fscore)
print()
print(ave_fscore)