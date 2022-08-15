from random import random
from numpy import array, nditer
from simple_nopos import remove_pos
from latent import objective, save, load
import pickle

def no_singletons(data):
    """
    Remove non-compounds, and compounds containing singletons
    """
    from collections import Counter
    count = Counter()
    for components in data.values():
        count.update(components)
    filtered = dict()
    for item, components in data.items():
        if len(components) > 1:
            for comp in components:
                if count[comp] == 1:
                    break
            else:
                filtered[item] = components
    non_single = {x for x,y in count.items() if y>1}
    print(len(data), len(count))
    return filtered, non_single
    """
    If applied recursively, this reduces:
    from 14611 data points, with 8229 lemmas
         (4219 data points, with 2466 lemmas, just removing non-compounds)
     to   2997 data points, with 1110 lemmas
    """

def featurise(items, lexicon, dims, max_sent=1/2, max_hidden=1/16):
    """
    Build a dictionary from lexical items to vectors,
    with the first component as the sentiment,
    and remaining components as random numbers
    """
    vector = dict()
    known = set()
    for i in items:
        try:
            sentiment = lexicon[i]
            known.add(i)
        except KeyError:
            sentiment = random()*2*max_sent - max_sent
        new_vec = [sentiment] + [random()*2*max_hidden - max_hidden for _ in range(dims)]
        vector[i] = array(new_vec)
    return vector, known

def initialise(dims=3,
               lex_file = '../../data/sentimerge_nospin.pk',
               morph_file = '../../data/morph/preproc_nopos.pk',
               data_file = '../../data/morph/data.pk',
               param_name = 'out',
               sparse = True):
    """
    Initialise data structures for learning
    """
    with open(lex_file, 'rb') as f:
        full_lex, full_lex_weight = pickle.load(f)
    lex, weight = remove_pos(full_lex, full_lex_weight)
    
    with open(morph_file, 'rb') as f:
        full_morph = pickle.load(f)
    
    morph = {x:y for x,y in full_morph.items() if x in lex and len(y)>1}
    relevant = {}
    for whole, parts in morph.items():
        for atom in parts:
            relevant.setdefault(atom, []).append((lex[whole], parts))
    
    data = [(lex[whole], parts) for whole, parts in morph.items()]
    
    items = sorted(relevant.keys()) #{z for y in morph.values() for z in y})
    embed, known = featurise(items, lex, dims-1)
    
    
    A = array([[random() for _ in range(dims)] for _ in range(dims)])
    B = array([[random() for _ in range(dims)] for _ in range(dims)])
    T = array([[[random() for _ in range(dims)] for _ in range(dims)] for _ in range(dims)])
    
    A[0,0] = 0.4
    B[0,0] = 0.7
    T[0,0,0] = 0.4
    
    # Make the initialisation sparse:
    if sparse:
        for arr in (A,B,T):
            it = nditer(arr, flags=['multi_index'], op_flags=['readwrite'])
            for value in it:
                old_val = value.copy()
                old_obj = objective(data, embed, (A,B,T))
                value[...] = 0
                new_obj = objective(data, embed, (A,B,T))
                if old_obj < new_obj:
                    value[...] = old_val
                    print(it.multi_index)
        
        for atom in items:
            if atom in known:
                r = range(1,dims)
            else:
                r = range(dims)
            vec = embed[atom]
            for i in r:
                old_val = vec[i].copy()
                old_obj = objective(relevant[atom], embed, (A,B,T), reg_emb=0)
                old_obj += abs(old_val)
                vec[i] = 0
                new_obj = objective(relevant[atom], embed, (A,B,T), reg_emb=0)
                if old_obj < new_obj:
                    vec[i] = old_val
                    print(atom, i)
    
    print(A, end='\n\n\n')
    print(B, end='\n\n\n')
    print(T)
    
    with open(data_file, 'wb') as f:
        pickle.dump((data, relevant, known), f)
    save(A, B, T, embed, param_name)


def initialise_one(name, verbose=True):
    """
    Initialise data structures with no extra variables
    """
    initialise(dims=1, param_name=name, sparse=False)
    
    data, rel, known, A, B, T, embed = load(name)
    W = (A,B,T)
    
    options = [-1, -0.5, -0.2, -0.1, 0, 0.1, 0.2, 0.5, 1]
    for _ in range(5):
        for item, points in rel.items():
            if not item in known:
                old = embed[item][0]
                minscore = float('inf')
                best = None
                for sent in options:
                    embed[item][0] = sent
                    new = objective(points, embed, W, reg_emb=0) + abs(sent)
                    if new < minscore:
                        minscore = new
                        best = sent
                embed[item][0] = best
                if verbose and best != old: 
                    print(item, best)
        print('\n...\n')
    
    save(A,B,T,embed,name)


def strip_data(orig_file, new_file):
    """
    Only keep a single data point for the same sequence of parts
    """
    with open(orig_file, 'rb') as f:
        orig_data, orig_rel, known = pickle.load(f)
    new_data = []
    new_rel = {}
    score = {}
    for val, parts in orig_data:
        score.setdefault(tuple(parts), []).append(val)
    for parts, values in score.items():
        average = sum(values)/len(values)
        point = (average, list(parts))
        new_data.append(point)
        for item in parts:
            new_rel.setdefault(item, []).append(point)
    with open(new_file, 'wb') as f:
        pickle.dump((new_data, new_rel, known), f)

if __name__ == '__main__':
    strip_data('../../data/morph/data_duplicates.pk',
               '../../data/morph/data.pk')