|| 2014 1.

cancel x y = x
swap f x y = f y x

truth * == (*->*->*)
true :: truth *
true = cancel
false :: truth *
false = (swap cancel)

not x = x false true
myand x y = x y false
myor  x y = x true y
xor x y = x (y false true) (y true false)


|| 2. (a)

pos ::= Zero | Succ pos

plus :: pos -> pos -> pos
plus x Zero     = x
plus x (Succ y) = plus (Succ x) y


times :: pos -> pos -> pos
times x Zero         = Zero
times x (Succ Zero)  = x
times x (Succ succs) = plus x (times x succs)

|| (b)

one   f x  =          f x
two   f x  =       f (f x)
three f x  =    f (f (f x))
four  f x  = f (f (f (f x)))

bplus f1 x1 f2 x2 = f1 x2

|| Q3 (a,b,c)

tree * ::= Tnil | Node * [tree *]

extree = Node "root" [Node "left" [Tnil], Node "right" [Node "level 3" [Tnil],Tnil]]

height :: (tree *)->num
height Tnil        = 0
height (Node x ls) = 1 + max(map (height) ls)

makeheighttree :: tree * -> tree num
makeheighttree Tnil        = Node 0 []
makeheighttree (Node x ls) = Node (height (Node x ls)) (map (makeheighttree) ls)

printtreeheight :: (tree [char])->[char]
printtreeheight Tnil        = "0"
printtreeheight (Node x ls) = shownum( height (Node x ls)) ++ " ( " ++ foldr (++) "" (map (++" ") (map (printtreeheight) ls)) ++ ")"

|| Q3 (d)

maptree :: (*->**)->tree *->tree **
maptree f Tnil        = Tnil
maptree f (Node x ls) = Node (f x) (map (maptree f) ls)

|| Q4 (a)
|| c1,c2,c3 :: num
|| f,g :: [num]->[num]

c1        = myinit : c2
c2        = f c3
c3        = g c1
myinit    = 0
||f []      = []
f xs      = xf [] xs
            where xf acc []     = acc
                  xf acc (y:ys) = xf (acc ++ [y+1]) ys
||g []      = []
g (x:xs)  = (x+2) : (g xs)
results  = c1


|| (b)
|| take 3 main
|| => take 3 (results)
|| => take 3 (c1)
|| => take 3 (init : c2)
|| => take 3 (init : f c3)
|| => take 3 (init : f g c1)
|| => take 3 (init : f g init : c2)
|| => take 3 (init : f g init : f c3)
|| => take 3 (init : f g init : f g c1)
|| => take 3 (init : f g init : f g init :...)
|| => take 3 (0 : f g init : f g init :...)
|| => take 3 (0 : f g 0 : f g 0 :...)
|| => take 3 (0 : f (2 : g (f g 0) :...) :...)
|| => take 3 (0 : 3 : f( g (f g 0) :...) :...) :...)
|| => take 3 (0 : 3 : f( g (f 2 :...) :...) :...) :...)
|| => take 3 (0 : 3 : f( g 3 :...) :...) :...) :...)
|| => take 3 (0 : 3 : f( 5 :... :...) :...) :...) :...)
|| => take 3 (0 : 3 : 6 :... :... :...) :...) :...) :...)
|| =>[0,3,6]

|| (c)
|| in a convoluted way we get the evaluation of something like
|| c = c which is circular and has no meaning. Miranda discovers
|| this at runtime and gives the error : BLACK HOLE

|| (d)
|| nothing as far as I can tell


|| (e)
|| f xs      = xf [] xs
||             where xf acc []     = acc
||                   xf acc (y:ys) = xf (acc ++ [y+1]) ys
||
|| => ...
|| => take 3 (0 : 3 : f( g (f g 0) :...) :...) :...)
|| => take 3 (0 : 3 : xf [] ( g (f g 0) :...) :...) :...)
|| => take 3 (0 : 3 : xf ([] ++ [(g (f g 0))+1]) ( g (f g 0) :...) :...) :...)
|| => take 3 (0 : 3 : xf ([] ++ [(g (f g 0))+1]) ( g (f g 0) :...) :...) :...)
|| => type error g (f g 0) :: [num]->[num] cannot plus one. 
