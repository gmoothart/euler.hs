import math


def triangular_number(n):
    return (n+1) * (n) / 2


def num_divisors(n):
    d = 0
    for i in range(1, int(math.floor(math.sqrt(n))) + 1):
        if n % i == 0:
            d += 2

    return d


def solve():
    d = 0
    t = 0
    # t(354) is 62k, roughly 250^2. No smaller number could have 500 divisors.
    i = 354
    while d < 500:
        t = triangular_number(i)
        d = num_divisors(t)
        i += 1
        
    print "{} has {} divisors!".format(t, d)


if __name__ == '__main__':
    solve()
