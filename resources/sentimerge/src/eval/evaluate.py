def evaluate(lex,
             eval_file = '../../data/MLSA/test_data.txt',
             pos_thr = 0.23,
             neg_thr = None,
             show_oov = False,
             verbose = True):
    """
    Evaluates a lexicon on test data
    """
    if neg_thr == None:
        neg_thr = -pos_thr
    
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
            #ans_types = set()
            try:
                pol = lex[lemma]
            except KeyError:
                try:
                    pol = lex[lemma[0].upper()+lemma[1:]]
                    if show_oov: print(lemma)
                except KeyError:
                    #try:
                    #    pol = lex[lemma.split('_')[0]]
                    #except KeyError:
                    pol = 0
                    if show_oov: print(lemma)
                    """if '+' in answers or '-' in answers:
                        N_oov += 1
                        print(lemma)"""
            if pol > pos_thr:
                pred = 1
            elif pol < neg_thr:
                pred = -1
            else:
                pred = 0
            for ans in answers:
                if ans == '+':
                    #ans_types.add(1)
                    if pred == 1:
                        pos_true += 1
                    else:
                        pos_miss += 1
                        if pred == -1:
                            neg_fail += 1
                elif ans == '-':
                    #ans_types.add(-1)
                    if pred == -1:
                        neg_true += 1
                    else:
                        neg_miss += 1
                        if pred == 1:
                            pos_fail += 1
                else:
                    #ans_types.add(0)
                    if pred == 1:
                        pos_fail += 1
                    elif pred == -1:
                        neg_fail += 1
            #if len(ans_types) > 1: N_ambig += 1; print(lemma, ans_types)
    #print(N_oov)
    #print(N_ambig)
    
    pos_precis = pos_true / (pos_true + pos_fail)
    pos_recall = pos_true / (pos_true + pos_miss)
    pos_fscore = 2 * pos_precis * pos_recall / (pos_precis + pos_recall)
    neg_precis = neg_true / (neg_true + neg_fail)
    neg_recall = neg_true / (neg_true + neg_miss)
    neg_fscore = 2 * neg_precis * neg_recall / (neg_precis + neg_recall)
    
    ave_precis = (pos_precis + neg_precis) / 2
    ave_recall = (pos_recall + neg_recall) / 2
    ave_fscore = (pos_fscore + neg_fscore) / 2
    
    if verbose:
        print(pos_precis)
        print(pos_recall)
        print(pos_fscore)
        print(neg_precis)
        print(neg_recall)
        print(neg_fscore)
        print()
        print("& {:.3}\t& {:.3}\t& {:.3}".format(ave_precis, ave_recall, ave_fscore), end="")
    
    return (ave_precis, ave_recall, ave_fscore)


if __name__ == "__main__":
    import pickle
    from numpy import arange
    lex_file  = '../../data/sentimerge_backoff.pk'
    with open(lex_file, 'rb') as f:
        lex, weight = pickle.load(f)
    for lemma, pol in list(lex.items()):
        lex[lemma.rsplit('_', maxsplit=1)[0]] = pol
    for x in arange(0, 0.9, 0.01):
        print("{:.2f} - {:.3f} {:.3f} {:.3f}".format(x, *evaluate(lex, pos_thr=x, verbose=False)))