"""
The prime factors of 13195 are 5, 7, 13 and 29.

What is the largest prime factor of the number 600851475143?
 
"""

plist = [2,3]
max = 600851475143

def primelist(plist, number):
    test = True
    
    for i in str(plist):
        if number%int(i) == 0:
            test = False
        else:
            pass
            
    if test == True:
        plist.append(number)
    else:
        pass
        
    return(test)

for number in range(2, max + 1):
    p = 1
    
    for i in range(2, number + 1):
        if primelist(i, number) == True and int(max)%int(i) == 0:
            p = i
        else:
            pass

print(p)