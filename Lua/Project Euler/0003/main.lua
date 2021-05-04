--[[
The prime factors of 13195 are 5, 7, 13 and 29.

What is the largest prime factor of the number 600851475143?
]]

function Prime(N)
     P = true
     for I = 2, N - 1 do
         if N%I == 0 then
             P = false
             break
         end
     end
     return(P)
end

function Factors(N)
     for I = 2, N do
         if N%I == 0 and Prime(I) == true then
             print(I)
         end
     end
     return(K)
end

print(Factors(600851475143))

-- answer: (highest found: 6857)

-- code is making to much calculus and not giving the answer