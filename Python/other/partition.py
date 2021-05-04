import sys

class Partition:
    def __init__(self, *p):
        self.__total = 0
        self.__data = []

        for item in p:
            self.__total += item
            self.__data.append(item)

        self.__normalizeData(1)

    def __normalizeData(self, new_total):
        for i, item in enumerate(self.__data):
            self.__data[i] = item / self.__total * new_total

        self.__total = new_total

    @property
    def todo(self):
        return self.__total

    @todo.setter
    def todo(self, t):
        self.__normalizeData(t)

    def __len__(self):
        len(self.__data)

    def __eq__(self, other):
        # return False # Since the only use of this fails anyway :v

        # If the totals or the lengths differ, we know they're not equal
        if(self.__total != other.__total or len(self.__data) != len(other.__data)):
            return False

        for i in range(len(self.__data)):
            if(self.__data[i] != other.__data[i]):
                return False

        return True

    def __getitem__(self, index):
        return self.__data[index]

    def __setitem__(self, index, val):
        self.__data[index] = val

        new_total = 0
        for item in self.__data:
            new_total += item

        self.__normalizeData(new_total)

    def __repr__(self):
        s = str(self)

        s += f': len = {len(self.__data)}, todo = {self.__total}'

        return s

    def __str__(self):
        s = ''

        for i, item in enumerate(self.__data):
            s += str(item)
            if(i != len(self.__data) - 1):
                s += ' '

        return s

def main():
    p = Partition(*[2, 5, 3])
    print("[%s]" % str(p).replace(' ', ', ')) # [0.2, 0.5, 0.3]

    p.todo = 100
    print("%r" % p) # 20.0 50.0 30.0: len = 3, todo = 100.0

    p = Partition(5, 6)
    print("%r" % p)

    p.todo = 11
    print(repr(p))

    q = Partition(5, 6)
    print(q)
    print(p == q)
    print("({}, {})".format(p[0], p[1]))

    p[1] = 7
    print(f"({p[0]}, {p[1]})")

if __name__ == "__main__":
    sys.exit(main())
