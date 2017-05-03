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

||f1 (a:b) f x = a ((x f b).f)


tree ::= Tnil | Node num tree tree

extree = Node 3 (Node 2 Tnil (Node 1 Tnil Tnil)) ((Node 5 (Node 4 Tnil Tnil) Tnil))

treeinsert :: num -> tree -> tree
treeinsert n Tnil         = Node n Tnil Tnil
treeinsert n (Node x l r) = Node x (treeinsert n l) r,if (n<x)
treeinsert n (Node x l r) = Node x l (treeinsert n r),otherwise
