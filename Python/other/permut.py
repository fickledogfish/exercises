def permut(lst, n):
    #print("debug " + str(n) + " " + str(lst))

    if n == 1: return [[x] for x in lst]
    if len(lst) == n: return [lst]

    ret = []
    for sublst in permut(lst[1:], n - 1):
        ret.append([lst[0]] + sublst)

    return ret + permut(lst[1:], n)

print(permut(list("abc"), 2))
print(permut([1, 2, 3, 4], 3))
