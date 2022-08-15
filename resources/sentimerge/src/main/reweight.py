def reweight(lexicon, average):
    """
    Given average values (ideally root mean square),
    reweights values in the lexicon
    """
    weight = {x:1/y for x,y in average.items()}
    
    for entry in lexicon.values():
    	for source, sentiment in entry.items():
    		if type(sentiment) == str:
    			continue
    		else:
    			entry[source] *= weight[source]
    
    return lexicon


def reweight_dash(lexicon, new, target='GermanPC'):
    """
    Given average values (ideally root mean square),
    assigns values to those without any
    """
    convert = {'POS':1, 'NEG':-1}
    
    for entry in lexicon.values():
        if target in entry and type(entry[target]) == str:
            pol = entry[target][0:3]
            try: val = entry[target][3]
            except IndexError: val = None
            entry[target] = convert[pol] * new[val]
    
    return lexicon