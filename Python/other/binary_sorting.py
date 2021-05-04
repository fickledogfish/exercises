import sys
import tkinter as tk

from random import randint

class sorter:
    @staticmethod
    def randomList(k):
        if k <= 0:
            return [9,3,4,10,100,-5,2,1,4,0,-12]

        limit = randint(10, 300)

        return [randint(-limit, limit) for _ in range(k)]

    def __init__(self, debug=False):
        pass

    def sort(self,x):
        a = []
        for element in x:
            idx = self.binarySearch(a,len(a),element)
            a.insert(idx,element)
        return a

    def binarySearch(self,arr,larr,x):
        initial_ind = 0
        final_ind = larr - 1
        pos = 0

        while initial_ind <= final_ind:
            pos = (final_ind + initial_ind)//2

            if arr[pos] == x:
                return pos
            elif arr[pos] < x:
                initial_ind = pos + 1
            else:
                final_ind = pos - 1

        return initial_ind

def main():
    s = sorter()
    lista1 = sorter.randomList(0)
    lista2 = s.sort(lista1)
    print("original list = %s" % lista1)
    print("sorted list = %s" % lista2)

    n = lista1[len(lista1)//2]
    b = s.binarySearch(lista2, len(lista2), n)
    print("pos({}) -> sorted list [{}] and found = {}".format(n,b,lista2[b]==n))

    n = 6
    b = s.binarySearch(lista2, len(lista2), n)
    print("pos({}) -> sorted list [{}] and found = {}".format(n,b, b<len(lista2) and lista2[b]==b))

class App(tk.Frame):
    def __init__(self, master):
        super().__init__(master)
        self.master = master

        self.sorter = sorter()
        self.list = self.sorter.randomList(0)

        self.pack()

        self.size_label = tk.Label(self, text = "Size: ")
        self.size_label.pack()

        self.size_entry = tk.Entry(self)
        self.size_entry.pack()

        self.list_label = tk.Label(self)
        self.updateLabel()
        self.list_label.pack()

        self.generate_button = tk.Button(self)
        self.generate_button["text"] = "Generate"
        self.generate_button["command"] = self.gen
        self.generate_button.pack()

    def gen(self):
        try:
            desired_size = int(self.size_entry.get())
        except Exception as e:
            print(e)
            self.size_entry.config(bg = "red")
            return

        self.list = self.sorter.randomList(desired_size)
        self.list = self.sorter.sort(self.list)
        self.updateLabel()
        self.size_entry.config(bg = "white")

    def updateLabel(self):
        self.list_label.configure(text = str(self.list))

def gmain():
    root = tk.Tk()
    app = App(root)
    app.mainloop()

if __name__ == "__main__":
    #sys.exit(main())
    sys.exit(gmain())
