def remove_pos(lex_pos, lex_pos_weight):
    """
    Pick the highest weighted part of speech, average if needed
    Perhaps this could be done more cleverly
    """
    pos_dict = dict()  # Record which parts of speech each item has
    for key in lex_pos:
        item, pos = key.rsplit('_', maxsplit=1)
        pos_dict.setdefault(item, set()).add(pos)
    lexicon = dict()
    weight = dict()
    for item, pos_set in pos_dict.items():
        high = 0
        new_set = set()
        for pos in pos_set:
            w = lex_pos_weight[item + '_' + pos]
            if w > high:
                high = w
                new_set = {pos}
            elif w == high:
                new_set.add(pos)
        weight[item] = high
        value = 0
        for pos in new_set:
            value += lex_pos[item + '_' + pos]
        value /= len(new_set)
        lexicon[item] = value
    return lexicon, weight

def numerify(filename, lexicon, weight, verbose=True):
    """
    Fetch sentiment scores for each compound and lemma
    File should contain data corresponding to load_mpro_nopos.py's output
    """
    numeric = []
    with open(filename, 'r') as f:
        for line in f:
            comp, rest = line.strip().split('\t')
            lemmas = rest.split(',')
            try:
                comp_sent = lexicon[comp]
                comp_weight = weight[comp]
            except KeyError:
                continue
            lemma_sent = []
            for lem in lemmas:
                try:
                    lemma_sent.append(lexicon[lem])
                except KeyError:
                    lemma_sent.append(0)
            numeric.append((comp_sent, tuple(lemma_sent), comp_weight))
            if verbose:
                print(comp, comp_sent, lemmas, lemma_sent, comp_weight)
    return numeric


if __name__ == '__main__':
    import pickle
    
    # Without SentiSpin
    lex_file = '../../data/sentimerge_nospin.pk'
    data_file = '../../data/morph/preproc_nopos.txt'
    simple_file = '../../data/morph/simple_nospin_nopos.pk'
    
    """# With SentiSpin
    lex_file = '../../data/sentimerge.pk'
    data_file = '../../data/morph/preproc_nopos.txt'
    simple_file = '../../data/morph/simple_nopos.pk'
    """
    
    # Load lexicon
    with open(lex_file, 'rb') as f:
        full_lex, full_lex_weight = pickle.load(f)
    
    # Process data
    lex, weight = remove_pos(full_lex, full_lex_weight)
    simple = numerify(data_file, lex, weight)
    
    # Save numerical data points
    with open(simple_file, 'wb') as f:
        pickle.dump(simple, f)