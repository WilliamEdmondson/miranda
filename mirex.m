|| Exercise 5.1
|| Write a function, using where definitions, to return the string that appears before a
|| given sublist in a string. For example, beforestring "and" "Miranda" will return the
|| string "Mir".


||multidrop :: [char]->[[char]]->[[char]]
||mydrop :: [char] -> [char]
||multidrop x y = map mydrop x y
||                where
||mydrop :: [char]-> [char] -> [char]
||mydrop x []        = []
||mydrop x (y:rest)  = []
||mydrop x (z:rest)  = z : mydrop x rest


|| 2016 3. (c)

f1 (a:b) f x = a ((x f b).f)
