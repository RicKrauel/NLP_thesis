import pickle

GE = 'german'
PC = 'GermanPC'
WS = 'GermanSentiWS'
SP = 'GermanSentiSpin'

with open('../../data/sentimerge.pk', 'rb') as f:
	new, _ = pickle.load(f)
with open('../../data/premerge.pk', 'rb') as f:
	raw = pickle.load(f)

for word, value in new.items():
	if word[0:2] == 'un' and word[0:5] != 'unter':
		if not word[2:] in new:
			if abs(value) > 1:
				print(word, value)
print()

for word, entry in raw.items():
	if len(entry) == 4:
		pos = 0
		neg = 0
		for source, value in entry.items():
			if value > 0:
				pos += 1
			elif value < 0:
				neg += 1
			entry[source] = float(value)
		#if pos * neg:
		#if entry[GE] * entry[PC] < 0:
		if entry[GE] * new[word] < 0:
		#if entry[GE] * new[word] < 0 and abs(new[word]) > 0.1:
			print("{}: {:.2} | {} {:.2} {:.2} {:.2}".format(word, new[word], raw[word][GE], raw[word][PC], raw[word][WS], raw[word][SP]))

sortlex = sorted([(value, word) for word, value in new.items()])
print("\nMost Negative:\n")
for v, w in sortlex[:20]:
	print("{}: {:.3}".format(w, v))
print("\nMost Positive:\n")
sortlex.reverse()
for v, w in sortlex[:20]:
	print("{}: {:.3}".format(w, v))