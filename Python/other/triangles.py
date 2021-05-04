# -*- coding: UTF-8 -*-

import sys
from math import sqrt

class triGenerator:
    """Gerador de triângulos retos."""

    SPACING = "\t"

    INSIDE = "+"
    OUTSIDE = "="

    def __init__(self, n):
        """
        Construtor. Salva a altura do triângulo.

        @param n altura do triângulo.
        """

        self.n = n

    def mirrorV(self, triangle):
        m = []

        for line in triangle:
            reversed_line = ""
            for ch in reversed(line):
                reversed_line += ch
            m.append(reversed_line)

        return m

    def mirrorH(self, triangle):
        m = []

        for line in reversed(triangle):
            m.append(line)

        return m

    def rightTriangle(self, orientation = 0):
        triangle = []

        for l in range(self.n):
            line = ""
            for c in range(self.n):
                line += self.INSIDE if c <= l else self.OUTSIDE
            triangle.append(line)

        if orientation == 0:
            pass
        elif orientation == 1:
            triangle = self.mirrorH(triangle)
        elif orientation == 2:
            triangle = self.mirrorV(triangle)
        elif orientation == 3:
            triangle = self.mirrorH(triangle)
            triangle = self.mirrorV(triangle)
        else:
            raise "Invalid orientation"

        return triangle

    def __str__(self):
        triangles = []

        for i in range(4):
            triangles.append(self.rightTriangle(i))

        s = ""
        for line in range(self.n):
            for triangle in triangles:
                s += triangle[line]
                s += self.SPACING
            s += "\n"

        return s

def main():
    print(triGenerator(int(input("Type n: "))))

if __name__ == "__main__":
    sys.exit(main())
