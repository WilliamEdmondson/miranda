|| 2015 paper


|| 1. (c) Stack recursion example
printdots :: num -> [char]
printdots 0 = []
printdots n = '.': printdots (n-1)

|| Accumulative recursion example
myplus :: num -> num -> num
myplus x 0 = x
myplus x y = myplus (x+1) (y-1)

|| 2. (c) Graphs
t_graph * ::= Emptygraph | Node * [t_graph *]

glist :: [t_graph char]
glist = [ Node 'A' [glist!2, glist!1],
        Node 'B' [glist!3],
        Node 'C' [glist!0, glist!3],
        Node 'D' []
        ]
graph :: t_graph char
graph = hd glist

||printgraph :: t_graph char -> [char]
||printgraph Emptygraph  = "empty"
||printgraph Node x y    = xprintgraph x y ""
||                           where
||                           xprintgraph x (hd:tl) a = a ++ "[Node " ++ x ++ printgraph hd ++ "]", if ~(member a x)
||                                                   = a ++ "Seen " ++ x ,otherwise

nil f = f (error "head of nil") (error "tail of nil") True
cons a b f = f a b False
head x = x h
         where
         h a b c = a
tail x = x t
         where t a b c = b
isnil x = x g
          where g a b c = c

|| 3. (e)

|| newmap f nil        = nil
|| newmap f (cons a b) = cons (f a) (newmap f b)

|| newmap f x = nil, if (isnil x)
||           = x g, otherwise
||             where
||             g a b c = cons (f a) (newmap f b)

operator a b = h
               where
               h c x = a c (b c x)

swap f x y = f y x

e1 a b f x = ((a f).(b f)) x

threes :: [num]->num
threes []     = 0
threes (3:xs) = 1 + threes xs
threes (x:xs) = threes xs


ch :: [num] -> (num,num,num)
ch x = xch x (0,0,0,0)
       where
       xch (0:xs) (a,b,c,d) = (a,b,c)
       xch (1:xs) (a,b,c,d) = xch xs ((a+1),b,c,(d+1))
       xch (2:xs) (a,b,c,d) = xch xs (a,(b+1),d,0), if d > c
                            = xch xs (a,(b+1),c,0), otherwise
       xch x                = error "not properly formatted"

countones :: [num] -> num
countones x = #(filter (=1) x)

c1b0 :: [num] -> num
c1b0 x = #(filter (=1) (takewhile (~=0) x))

c1c2b0 :: [num] -> (num,num,num)
c1c2b0 x =  (h 1,h 2, g x)
            where
            h a  = #(filter (=a) (takewhile (~=0) x))
            g x  = maxseq 1 (takewhile (~=0) x)

greater x y = x, if x > y
            = y, otherwise

split :: *->[*]->[[*]]
split n x = xsplit n x ([],[])
            where
            xsplit n []     (a,b) = filter (~=[]) (b:a)
            xsplit n (n:xs) (a,b) = xsplit n xs (a,n:b)
            xsplit n (x:xs) (a,b) = xsplit n xs (b:a,[])

maxseq :: num -> [num] -> num
maxseq n x = foldl (greater) 0 (map (#) (split n x))

results = [("fred",45), ("sally", 79), ("chris", 65)]
f = (map fst) . (filter ((< 50) . snd))

main = f results 
