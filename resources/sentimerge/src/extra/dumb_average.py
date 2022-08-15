import pickle

with open('../../data/raw_lexicon.pk', 'rb') as f:
	data = pickle.load(f)

new = {}

for word, entry in data.items():
	pol = []
	for value in entry.values():
		if type(value) != str:
			pol.append(value)
		else:
			if value[:3] == 'POS':
				pol.append(1)
			else:
				pol.append(-1)
	if len(pol) > 0:
		new[word] = sum(pol) / len(pol)

with open('../../data/lexicon_average.pk', 'wb') as f:
	pickle.dump(new, f)