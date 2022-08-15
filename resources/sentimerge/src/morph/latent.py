from random import random
from numpy import array, nditer, zeros, sign
from math import copysign
from copy import copy
import pickle


def save(A, B, T, embed, name):
    """
    Save data to disk
    """
    pickle_file = '../../data/morph/'+name+'.pk'
    text_file   = '../../data/morph/'+name+'.txt'
    with open(pickle_file, 'wb') as f:
        pickle.dump((A,B,T,embed), f)
    with open(text_file, 'w') as f:
        f.write(str(A))
        f.write('\n\n')
        f.write(str(B))
        f.write('\n\n')
        f.write(str(T))
        f.write('\n\n')
        for atom in sorted(embed):
            f.write('{}: {}\n'.format(atom, embed[atom]))

def load(name):
    data_file  = '../../data/morph/data.pk'
    param_file = '../../data/morph/'+name+'.pk'
    with open(data_file, 'rb') as f:
        data, relevant, known = pickle.load(f)
    with open(param_file, 'rb') as f:
        A, B, T, embedding = pickle.load(f)
    return (data, relevant, known, A, B, T, embedding)
        


def predict(orig_vectors, mat_l, mat_r, tensor, full=False):
    """
    Predict the sentiment of a compound
    """
    vectors = copy(orig_vectors)
    right = vectors.pop()
    while vectors:
        left = vectors.pop()
        right = mat_l.dot(left) + mat_r.dot(right) + tensor.dot(left).dot(right)
    if full:
        return right
    else:
        return right[0]

def differentiate(gold, vectors, which, index, mat_l, mat_r, tensor):
    """
    Differentiate the square error, for a specific index of a specific vector
    """
    lead = predict(vectors, mat_l, mat_r, tensor)
    dims = len(vectors[0])
    pre  = vectors[:which]
    post = vectors[which+1:]
    this = zeros(dims)
    this[index] = 1
    if post:
        partial = predict(post, mat_l, mat_r, tensor, True)
        partial = predict([this, partial], mat_l, zeros((dims,dims)), tensor, True)
    else:
        partial = this
    partial = predict(pre+[partial], zeros((dims,dims)), mat_r, tensor, False)
    return 2 * (lead - gold) * partial


def objective(data, embedding, weights, reg_emb=1, reg_wei=0):
    """
    Calculate the objective function
    """
    result = 0
    for gold, parts in data:
        lead = predict([embedding[x] for x in parts], *weights)
        result += (gold - lead)**2
    if reg_emb:
        cost = 0
        for vec in embedding.values():
            for x in vec:
                cost += abs(x)
        result += cost * reg_emb
    if reg_wei:
        cost = 0
        for array in weights:
            for x in nditer(array):
                cost += abs(x)
        result += cost * reg_wei
    return result


def add_reg(val, grad, reg):
    """
    Add L1 regularisation term to gradient
    """
    if val == 0:
        if abs(grad) < reg:
            return 0
        elif grad != 0:
            return grad - copysign(reg, grad)
    elif val > 0:
        return grad + reg
    else:
        return grad - reg

def descend(val, incr):
    """
    Descend the gradient, not going past 0
    """
    if sign(incr) == sign(val) or abs(incr) < abs(val) or val == 0:
        return val + incr
    else:
        return 0

def descend_embeddings(embed, known, rel, wei, reg, learn, check=False):
    """
    Gradient descent, for embeddings
    embed - embedding dictionary
    rel - relevant data for each lexical item
    wei - weights for sentiment prediction
    reg - regularisation factor
    learn - (negative) learning rate
    """
    def pred(vecs):
        return predict(vecs, *wei)
    dims = len(wei[0])
    
    for item, points in rel.items():
        current = embed[item]
        if item in known:
            r = range(1,dims)
            increment = [None]
        else:
            r = range(dims)
            increment = []
        for i in r:
            basis_vec = zeros(dims)
            basis_vec[i] = 1
            grad_i = 0
            for gold, parts in points:
                which = parts.index(item)
                grad_i += differentiate(gold, [embed[x] for x in parts], which, i, *wei)
            grad_i = add_reg(current[i], grad_i, reg)
            increment.append(grad_i * learn)
        for i in r:
            if check:
                old_obj = objective(points, embed, wei, reg_emb=0) + abs(current[i])
            if increment[i]:
                current[i] = descend(current[i], increment[i])
                if check and objective(points, embed, wei, reg_emb=0) + abs(current[i]) > old_obj:
                    print(i)
                    print(item, embed[item])
                    print(points)
                    print(increment)

def descend_weights_numeric(cost, weights, reg, learn, step):
    """
    Gradient descent, for weights
    cost - objective function, not requiring parameters, without regularisation
    weights - their derivative will be approximated
    reg - regularisation factor
    learn - (negative) learning rate
    step - step size for derivative
    """
    derivative = []
    for arr in weights:
        der = zeros(arr.shape)
        it = nditer(arr, flags=['multi_index'], op_flags=['readwrite'])
        for value in it:
            old_val = value.copy()
            old_obj = cost()
            value[...] += step
            new_obj = cost()
            value[...] = old_val
            grad = (new_obj - old_obj)/step
            grad = add_reg(old_val, grad, reg)
            der[it.multi_index] = grad
        derivative.append(der)
    
    for n, arr in enumerate(weights):
        der = derivative[n]
        it = nditer(arr, flags=['multi_index'], op_flags=['readwrite']) 
        for value in it:
            value[...] = descend(value[...], der[it.multi_index]*learn)


def descend_indef(initial_name, out_name, reg_emb=1, reg_wei=1, learn_emb=-2**-10, learn_wei=-2**-16, step=2**-30, rep_disp=10, rep_dot=10, rep_emb=1, check=False, vec_not_mat=False):
    """
    Gradient descent, indefinitely
    """
    data, rel, known, A, B, T, embed = load(initial_name)
    if vec_not_mat:
        d = len(A)
        W = (A.ravel()[:d**2:d+1],  # Diagonals
             B.ravel()[:d**2:d+1],
             T)
    else:
        W = (A,B,T)
    def obj():
        return objective(data, embed, (A,B,T), reg_emb=reg_emb, reg_wei=reg_wei)
    def now_obj():
        return objective(data, embed, (A,B,T), reg_emb=0)
    
    print(obj())
    while True:
        for _ in range(rep_disp):
            for _ in range(rep_dot):
                descend_weights_numeric(now_obj, W, reg_wei, learn_wei, step)
                print('.', end='', flush=True)
            print()
            print(obj())
        for _ in range(rep_emb):
            descend_embeddings(embed, known, rel, W, reg_emb, learn_emb, check=check)
        print(obj())
        print('---')
        save(A, B, T, embed, out_name)


def show_top(name, N=10):
    """
    Show the items with the strongest inferred sentiment,
    and with the largest non-sentiment features
    """
    _,_, known, A,_,_, embed = load(name)
    dims = len(A)
    
    def print_extremes(things, N):
        for x in things[:N]:
            print(x)
        print('...')
        for x in things[-N:]:
            print(x)
        print()
    
    ### Top inferred sentiment ###
    
    sentiment = [(x, embed[x]) for x in embed if not x in known]
    sentiment.sort(key=lambda x:x[1][0])
    print_extremes(sentiment, N)
       
    ### Top non-sentiment ###
       
    for i in range(1, dims):
        stuff = []
        for item, vec in embed.items():
            stuff.append((item, vec))
        stuff.sort(key=lambda x:x[1][i])
        print_extremes(stuff, N)
     


if __name__ == '__main__':
    
    show_top('boot__1')
    
#     descend_indef('one__', 'one__', learn_emb=-2**-10, rep_dot=5, rep_disp=4, rep_emb=64, reg_wei=16, vec_not_mat=True)
#     descend_indef('one__', 'one__', learn_emb=-2**-10, rep_dot=5, rep_disp=4, rep_emb=64, reg_wei=1)
    
    
#     data, rel, known, A, B, T, embed = load('one__')
#         
#     print(objective(data, embed, (A,B,T), reg_wei=1))
#       
#     while True:
#         for _ in range(32):
#             descend_embeddings(embed, known, rel, (A,B,T), 1, -2**-10, check=False)
#         print(objective(data, embed, (A,B,T), reg_wei=1))
#         save(A,B,T,embed, 'one__')
    

    
#     data, rel, known, A, B, T, embed = load('boot_3')
#     _, _, _, A1, B1, T1, embed1 = load('one__')
#     
#     for item in embed:
#         embed[item][0] = embed1[item][0]
#     
#     for i in range(3):
#         for j in range(3):
#             if i != j:
#                 A[i,j] = 0
#                 B[i,j] = 0
#     
#     save(A,B,T,embed,'boot__3')
    
    
    
#     data, rel, known, A, B, T, embed = load('one_')
#     _, _, _, A1, B1, T1, embed1 = load('overnight3')
#     dims = len(A1)
#     
#     print(objective(data, embed1, (A1,B1,T1), reg_wei=16))
#     
#     for arr in (A1, B1, T1):
#         it = nditer(arr, op_flags=['readwrite'])
#         for value in it:
#             value[...] /= 2
#     
#     A1[0,0] = A[0,0]
#     B1[0,0] = B[0,0]
#     T1[0,0,0] = T[0,0,0]
#     
#     print(objective(data, embed1, (A1,B1,T1), reg_wei=16))
#     
#     for item, vec in embed1.items():
#         vec[0] = embed[item][0]
#         #for i in range(1,dims):
#         #    vec[i] /= 1
#     
#     print(objective(data, embed1, (A1,B1,T1), reg_wei=16))
#     
#     save(A1,B1,T1,embed1, 'boot3')