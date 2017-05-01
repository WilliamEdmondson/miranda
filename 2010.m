|| Q3
|| Define a list

listy * ::= Nil | Cons * (listy *)

|| listy is defined with two cases. There is the terminal case where
|| listy is 'Nil'. This allows an instance of listy to have a finite size.
|| The case 'Cons * listy *' is the self referential case which defines
|| a poly-typed element and a connection to the definition of listy.
|| This connection refers to either 'Nil' or the the self referential
|| case which defines a poly-typed element and a connection
|| to the definition of listy. This connection refers to either 'Nil' or
|| the the self referential case which defines a poly-typed element ...
|| ................. you see what I mean, no? ;)

f []           = [0]
f (1 : rest)   = 1 : (f rest)
f (2 : rest)   = 2 : (f rest)
f (x : rest)   = f rest

numchar ::= Num num | Char char

numchartree ::= Tnil | Node numchar numchartree numchartree

rooty  = Node (Num 4) lefty                        righty
lefty  = Node (Num 2) (Node (Num 1) (Tnil) (Tnil)) (Node (Num 3) (Tnil) (Tnil))
righty = Node (Num 6) Tnil                         (Node (Num 7) (Tnil) (Tnil))

printtree :: numchartree -> [char]
printtree Tnil                = "Tnil"
printtree (Node (Num  n) l r) = "(" ++ printtree l ++ ") / " ++ shownum n ++ " \\ (" ++ printtree r ++ ")"
printtree (Node (Char c) l r) = "(" ++ printtree l ++ ") / " ++    [c]    ++ " \\ (" ++ printtree r ++ ")"

insertnum :: numchar -> numchartree -> numchartree
insertnum (Char c) ls                    = error "this tree is only for Num nums"
insertnum (Num n ) Tnil                  = Tnil
insertnum (Num n ) (Node (Num x) Tnil r) = Node (Num x) (Node (Num n) (Tnil) (Tnil)) r , if n<x
insertnum (Num n ) (Node (Num x) l Tnil) = Node (Num x) l (Node (Num n) (Tnil) (Tnil)) , otherwise
insertnum (Num n ) (Node (Num x) l r)    = Node (Num x) (insertnum (Num n) l) r ,        if n<x
insertnum (Num n ) (Node (Num x) l r)    = Node (Num x) l (insertnum (Num n) r) ,        otherwise

deletenum :: numchar -> numchartree -> numchartree
deletenum (Char c) ls                    = error "this tree is only for Num nums"
deletenum (Num n ) Tnil                  = Tnil
deletenum (Num n ) (Node (Num x) l r)    = r ,                                           if n = x
deletenum (Num n ) (Node (Num x) l r)    = Node (Num x) (deletenum (Num n) l) r ,        if n<x
deletenum (Num n ) (Node (Num x) l r)    = Node (Num x) l (deletenum (Num n) r) ,        otherwise

treeheight :: numchartree -> num
treeheight Tnil         = 0
treeheight (Node x l r) = max [(1 + treeheight l),(1 + treeheight r)]

maptreenum f Tnil                = Tnil
maptreenum f (Node (Char x) l r) = error "this tree is only for Num nums"
maptreenum f (Node (Num  x) l r) = Node (Num (f x)) (maptreenum f l) (maptreenum f r)

maptree f Tnil         = Tnil
maptree f (Node x l r) = Node (Num (f (Node x l r))) (maptree f l) (maptree f r)

heightmaptree = maptree treeheight
