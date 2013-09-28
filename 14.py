# yeah, yeah, not Haskell. Sue me.

mem = {}


def collatz(n, acc=1):
    if n == 1:
        return acc
    if n not in mem:

        if n % 2 == 0:
            mem[n] = collatz(n/2)
        else:
            mem[n] = collatz(3*n + 1)

    return mem[n] + acc



if __name__ == '__main__':

    mem = {}
    max_chain = 0
    max_num = 0
    for n in range(1,1000000):
        chain = collatz(n)
        #print "{} has chain length {}".format(n, chain)
        if chain > max_chain:
            max_chain = chain
            max_num = n

    print "{0} has the longest chain, length {1}".format(max_num, max_chain)
