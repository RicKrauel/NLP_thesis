from copy import deepcopy
from math import sqrt

def filt(lexicon, one, two):
    """
    Filters the full lexicon to the intersection of two individual lexicons 
    """
    return {x:(y[one],y[two]) for x,y in lexicon.items() if one in y and two in y}

def stats(orig_lexicon, lex_set, verbose=True, everything=False):
    """
    Calculates the root mean square for each intersection
    """
    lexicon = deepcopy(orig_lexicon)
    # Remove 'None' entries:
    PC = 'GermanPC'
    for word, wordlex in lexicon.items():
        if PC in wordlex and type(wordlex[PC]) == str:
            wordlex.pop(PC)
            if verbose:
                print("Removed {}".format(word))
    
    lex_pairs = [(x,y) for x in lex_set for y in lex_set if x<y]
    intersect = [filt(lexicon, *x) for x in lex_pairs]
    
    avabs = dict()
    avsqu = dict()
    
    for i in range(len(lex_pairs)):
        a,b = lex_pairs[i] 
        N = len(intersect[i])
        #print("{}: {}".format(lex_pairs[i], N))
        if everything:
            avabs[a,b] = sum([abs(x[0]) for x in intersect[i].values()])/N
            avabs[b,a] = sum([abs(x[1]) for x in intersect[i].values()])/N
        avsqu[a,b] = sum([x[0]**2 for x in intersect[i].values()])/N
        avsqu[b,a] = sum([x[1]**2 for x in intersect[i].values()])/N
    
    if everything and verbose:
        print('Mean absolute values:')
        for a in lex_set:
            coeff = sum([y for x,y in avabs.items() if x[0] == a])/(len(lex_set)-1)
            print(a,coeff)
    
    rms = {}
    if verbose:
        print('Root mean square:')
    for a in lex_set:
        coeff = sqrt(sum([y for x,y in avsqu.items() if x[0] == a])/(len(lex_set)-1))
        if verbose:
            print(a,coeff)
        rms[a] = coeff
    
    ### Without intersections ###
    if everything and verbose:
        total = {x:0 for x in lex_set}
        ttsqu = {x:0 for x in lex_set}
        count = {x:0 for x in lex_set}
        
        for wordlex in lexicon.values():
            for source, sentiment in wordlex.items():
                total[source] += abs(sentiment)
                ttsqu[source] += sentiment**2
                count[source] += 1
        
        print('Without intersections:')
        print('Mean absolute values:')
        for x in lex_set:
            print(x, total[x]/count[x])
        print('Root mean square:')
        for x in lex_set:
            print(x, sqrt(ttsqu[x]/count[x]))
    
    return rms


def mean(data):
    """ Calculate the arithmetic mean of an iterable """
    return sum(data)/len(data)


def stats_dash(lexicon, other, target='GermanPC', verbose=True, everything=False):
    """
    Some lexical items have a sentiment with no value.
    This function calculates their average value in the other lexicons
    Typically: target = PC, other = (GE,WS,SP)
    """
    
    if everything:
        abs_dash = {x:[] for x in other}
        abs_zero = {x:[] for x in other}
    squ_dash = {x:[] for x in other}
    squ_zero = {x:[] for x in other}
    
    for entry in lexicon.values():
        if target in entry:
            if type(entry[target]) != str:
                continue
            elif entry[target][-1] == '0':
                for x in other:
                    if x in entry:
                        if everything:
                            abs_zero[x].append(abs(entry[x]))
                        squ_zero[x].append(entry[x]**2)
            else:
                for x in other:
                    if x in entry:
                        if everything:
                            abs_dash[x].append(abs(entry[x]))
                        squ_dash[x].append(entry[x]**2)
    
    if everything:
        mean_abs_dash = {x:mean(abs_dash[x]) for x in other}
        mean_abs_zero = {x:mean(abs_zero[x]) for x in other}
    mean_squ_dash = {x:mean(squ_dash[x]) for x in other}
    mean_squ_zero = {x:mean(squ_zero[x]) for x in other}
    
    if verbose:
        if everything:
            print('Mean absolute values:')
            print('Dash:')
            for x in other:
                print(x, mean_abs_dash[x])
            print('Average', mean(mean_abs_dash.values()))
            print('Zero:')
            for x in other:
                print(x, mean_abs_zero[x])
            print('Average', mean(mean_abs_zero.values()))
        print('Root mean square:')
        print('Dash:')
        for x in other:
            print(x, sqrt(mean_squ_dash[x]))
        print('Average', sqrt(mean(mean_squ_dash.values())))
        print('Zero:')
        for x in other:
            print(x, sqrt(mean_squ_zero[x]))
        print('Average', sqrt(mean(mean_squ_zero.values())))
    
    return {None:sqrt(mean(mean_squ_dash.values())),
             '0':sqrt(mean(mean_squ_zero.values()))}


if __name__ == '__main__':
    import pickle
    print('Loading data...')
    with open('../../data/lexicon_string.pk','rb') as f:
        old_lex = pickle.load(f)
    GE = 'german'
    PC = 'GermanPC'
    WS = 'GermanSentiWS'
    SP = 'GermanSentiSpin'
    
    stats(old_lex, (GE, PC, WS, SP), everything=True)