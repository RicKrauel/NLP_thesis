from collections import defaultdict

def lexicon_convert(in_file, verbose=True):
    """
    Converts the Perl hash to a Python dict
    """
    
    convert = {'POS':1, 'NEG':-1, 'NEUT':0, 'NEU':0}    # INT, SHI
    tagset = ['N', 'AJ', 'V', 'NE', 'XY', 'ADJA', 'AV', 'CARD', 'FM', 'PTKVZ', 'APPR', 'PIS', 'KON', 'TRUNC', 'PROAV', 'KOUS', 'PDS', 'APPRART', 'NOOJ', 'PWAV', 'PPER', 'PPOSAT', 'PWS', 'PTKNEG', 'PRF', 'KOUI', 'ITJ', 'PTKA', 'PIAT', 'PIDAT', 'ART', 'ADV']
    
    lexicon = defaultdict(dict)
    
    if verbose:
        print("Loading data...")
    
    with open(in_file, 'r', encoding='utf-8') as f:
        for i in range(9):
            f.readline()
        while True:
            try:
                word = f.readline().split('"',2)[1].lower()
            except IndexError:
                break
            while True:
                try:
                    source = f.readline().split('"',2)[1].split('::',1)[1].split('.',1)[0]
                except IndexError:
                    break
                while True:
                    try:
                        pos = f.readline().split('"',2)[1].split('::',1)[1]
                    except IndexError:
                        break
                    rankstring = f.readline().rsplit('"',2)[1]
                    if rankstring == '-':
                        rank = None
                    else:
                        rank = float(rankstring)
                    valstring = f.readline().rsplit('"',2)[1]
                    try:
                        val = convert[valstring]
                    except KeyError:
                        f.readline()
                        continue
                    if val == 0:
                        lexicon[word+'_'+pos][source] = 0
                    else:
                        if rank == None:
                            lexicon[word+'_'+pos][source] = valstring
                        elif rank == 0:
                            lexicon[word+'_'+pos][source] = valstring+'0'
                        else:
                            lexicon[word+'_'+pos][source] = rank*val
                    #print(source, word, pos, val, rank)
                    f.readline()
    if verbose:
        print('Done!')
    return lexicon

if __name__ == '__main__':
    import pickle
    lex = lexicon_convert('../../data/merged_lex_hash.pm')
    with open('../../data/raw_lexicon.pk', 'wb') as f:
        pickle.dump(lex, f)