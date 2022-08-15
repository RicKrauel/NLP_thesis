import pickle
from collections import Counter

with open('../../data/raw_lexicon.pk', 'rb') as f:
	data = pickle.load(f)

new = {}
cutoff = 0.1

for word, entry in data.items():
	pol = []
	for source in ['german','GermanPC','GermanSentiWS','GermanSentiSpin']:
		try:
			value = entry[source]
		except KeyError:
			continue
		if type(value) == str:
			if value[:3] == 'POS':
				value = 1
			else:
				value = -1
		elif value > cutoff:
			value = 1
		elif value < cutoff:
			value = -1
		else:
			value = 0
		pol.append(value)
	vote = Counter(pol)
	plur = max(vote.values())
	winner = [x for x in vote if vote[x] == plur]
	if len(winner) == 1:
		new[word] = winner[0]
	else:
		new[word] = pol[0]

with open('../../data/lexicon_vote.pk', 'wb') as f:
	pickle.dump(new, f)