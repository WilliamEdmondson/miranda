>

> swap f a b = f b a

> foldex = take 5 (foldr (:) [] [ n | n<-[1..] ; ((mod) n 3) = 0])
> foldex2 = hd (foldl (swap (:)) [] [1..])

>  infinite = hd ([5.. ])

> mystery op = g
>              where
>              g r = (r:) . rest
>                    where
>                    rest []    = []
>                    rest (a:x) = g (op r a) x

Fucking perms man!

My thoughts on this are to cycle through putting the first element
at the end of the list until all of those cyclic possibilities are
done.
[1,2,3] -> [[1,2,3],[2,3,1],[3,1,2]]

Then do the same for the tail of each of these lists.
[[1:[2,3],1:[3,2]],etc.]

> cycle :: [*]->[*]
> cycle []     = []
> cycle (x:xs) = reverse ( x : (reverse xs))

> cyclicperms :: [*] -> [[*]]
> cyclicperms x     = xcyclicperms x []
>                     where
>                     xcyclicperms [] acc = acc
>                     xcyclicperms x  acc = acc,                           if member acc x
>                                         = xcyclicperms (cycle x) (x:acc),otherwise

Uses the newly discovered standenv function merge to merge the two lists.

> perms :: [*] -> [[*]]
> perms x = xperms (cyclicperms x)
>           where
>           xperms []              = []
>           xperms ((x:xs):rest)   = merge (map (x:) (cyclicperms xs)) (xperms rest)


  _______________________________
 /           TYPES              /
/______________________________/

> nil :: (* -> ** -> bool -> ***) -> ***
> nil f = f (error "head of nil") (error "head of tail") True

> cons :: *->**->(*->**->bool->***)->***
> cons a b f = f a b False


> head :: ((*->**->***->*)->****)->****
> head x = x h
>          where
>          h a b c = a

> tail :: ((*->**->***->**)->****)->****
> tail x = x t
>          where
>          t a b c = b

isnil :: ((*->**->***->***)->****)->****

> isnil x = x n
>           where
>           n a b c = c


head (cons a b)
=> (cons a b) h
=> h a b False
=> a

tail (cons a b) = b
=> (cons a b) t
=> t a b False
=> b

head (tail (cons  (cons b c))) = b
=> tail (cons (cons b c))
=> tail (cons (cons b c)) h
=> cons (cons b c) h t
=> t cons b c h False
=> b

newmap f nil = nil
newmap f (cons a b) = cons (f a) (newmap f b)

 newmap f x = nil, if (isnil x)
            = x g, otherwise
              where
              g a b c = cons (f a)(newmap f b)

head (tail (newmap (+1)(cons 3 (cons 4 nil))))
=> tail (newmap (+1)(cons 3 (cons 4 nil))) h
=> newmap (+1) (cons 3 (cons 4 nil)) h t
=> cons 3 (cons 4 nil) g h t
=> g 3 (cons 4 nil) False h t
=> cons (+1 3) (newmap (+1) (cons 4 nil)) h t
=> h 4 (newmap (+1) (cons 4 nil)) False t
=> 4 t

> f1 (a:b) f x = a ((x f b). f)

> f2 [] j          z = z
> f2 f []          z = z
> f2 (x:xs) (y:ys) z = y (f2 xs ys (x y (z+1)))

> f3 []    y z = z
> f3 (a:b) y z = (a z) & (f3 b y ((y . a . y) z))

> b f x = f  (f x)

> myminus x = (-) x
