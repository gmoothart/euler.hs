


def primitive_triplets_bfs():
    # http://en.wikipedia.org/wiki/Pythagorean_triple
    # each primitive triple can be transformed using t1, t2, t3 into
    # 3 other primitive triples. We are just doing BFS over an infinite
    # tree.
    # I dunno how it works, but it works!

    def t1(a,b,c):
        return ( a - 2*b + 2*c, 2*a - b + 2*c,  2*a - 2*b + 3*c)
    def t2(a,b,c):
        return ( a + 2*b + 2*c, 2*a + b + 2*c,  2*a + 2*b + 3*c)
    def t3(a,b,c):
        return (-a + 2*b + 2*c, -2*a + b + 2*c, -2*a + 2*b + 3*c)

    q = [(3,4,5)]
    while True:
        t = q.pop()
        yield t
        q.insert(0, t1(*t))
        q.insert(0, t2(*t))
        q.insert(0, t3(*t))


def solve(target=1000):
    for (a, b, c) in primitive_triplets_bfs():
        print "### searching primitive triple {}, {}, {}".format(a,b,c)
        d, e, f = a, b, c
        k = 2
        while d + e + f < target:
            d = a*k
            e = b*k
            f = c*k
            k += 1

        if d + e + f == target:
            return (d, e, f)


if __name__ == '__main__':
    a, b,c = solve()
    print "triples: {}, {}, {}".format(a,b,c)
    print "product: {}".format(a*b*c)
