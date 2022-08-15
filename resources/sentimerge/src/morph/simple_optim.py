from scipy.optimize import minimize

def dp(x,y):
    """
    Dot product
    """
    result = 0
    for i in range(len(x)):
        result += x[i]*y[i]
    return result

def diff(data, regularisation=0, filter=3.54, weighting=True):
    """
    Produce an objective function
    """
    def func(weights):
        result = 0
        for whole, parts, weight in data:
            if filter and weight < filter: # Remove SentiSpin-only entries
                continue
            while len(parts) > 1:
                left, right = parts[-2:]
                vector = (left, right, left*right)
                output = dp(weights, vector)
                parts = parts[:-2] + (output,)
            error = (whole - parts[0])**2
            if weighting:
                error *= weight
            result += error
        for w in weights:
            result += regularisation * w**2
        return result
    return func


if __name__ == '__main__':
    import pickle
    with open("../../data/morph/simple_nospin_nopos.pk", 'rb') as f:
        data = pickle.load(f)
    
    objective = diff(data, filter=True)
    result = minimize(objective, (0.4, 0.7, 0.4), tol=10**-9, method='L-BFGS-B', options={'disp':True}, callback=print)
    print('weights:', result['x'])


# [ 0.11341021  0.17969634  0.06652648] no weighting, no regularisation
# [ 0.14167071  0.23797563  0.11479017] with weights, no regularisation
# [ 0.33720395  0.61887952  0.28357204] with weights, only if > 3.54, no regularisation
# [ 0.42000249  0.72799791  0.38977162] with weights, no-SP lexicon,  no regularisation