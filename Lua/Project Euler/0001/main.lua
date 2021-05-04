--[[
If we list all the natural numbers below 10 that are multiples of 3 or 5, we get
3, 5, 6 and 9. The sum of these multiples is 23.

Find the sum of all the multiples of 3 or 5 below 1000.
]]

S = 0

for I = 1, 999 do
     if I%3 == 0 or I%5 == 0 then
         S = S + I
     end
end

print(S)

-- answer: 233168 (ok)
