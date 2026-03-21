
doubleme x= x + x

doubleUs x y = doubleme x + doubleme y 

doubleSmallNumber x = if x <= 100
                    then x*2
                    else x

doubleSmallNumber' x = (if x <= 100 then x*2 else x) + 1
