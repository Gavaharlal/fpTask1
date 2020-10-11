def gcd(a, b):
    #Calculate the Greatest Common Divisor of a and b.
    #Unless b==0, the result will have the same sign as b (so that when
    #b is divided by it, the result comes out positive).
    while b:
        t = a
        a = b
        b = t % b
    return a

print("Input a: ")
a = int(input())



print("Input b: ")
b = int(input())
print()
print("GCD of a and b:")
print(gcd(a, b))