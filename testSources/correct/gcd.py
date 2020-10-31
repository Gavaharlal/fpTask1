def gcd(a, b):
    #Calculate the Greatest Common Divisor of a and b.
    #Unless b==0, the result will have the same sign as b (so that when
    #b is divided by it, the result comes out positive).
    while b:
        t = a
        a = b
        b = t % b
    return a

a = int(input())
b = int(input())
print(gcd(a, b))