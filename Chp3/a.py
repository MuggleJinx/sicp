def iter(product, counter, n):
    if counter > n:
        return product
    else:
        return iter(product * counter, counter + 1, n)

def factorial(n):
    return iter(1, 1, n)

print(factorial(4))
