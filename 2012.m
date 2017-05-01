
|| Q2.

||rcons:: * ->([*]->**)->*->**
rcons a f b = f (a : b)

rnil:: *->*
rnil = id

myid:: *->*
myid x = x


rev items = foldr rcons rnil items []

|| 2012 Q3. Part I
suit     ::= Clubs | Hearts | Spades | Dimonds
straight ::= Two_of | Three_of | Four_of | Five_of | Six_of | Seven_of | Eight_of | Nine_of | Ten_of | Jack_of | Queen_of | King_of | Ace_of
card == (straight,suit)

deck = [(Two_of,Spades),(Three_of,Spades),(Four_of,Spades),(Five_of,Spades),
        (Six_of,Spades),(Seven_of,Spades),(Eight_of,Spades),(Nine_of,Spades),
        (Ten_of,Spades),(Jack_of,Spades),(Queen_of,Spades),(King_of,Spades),(Ace_of,Spades),
        (Two_of,Hearts),(Three_of,Hearts),(Four_of,Hearts),(Five_of,Hearts),
        (Six_of,Hearts),(Seven_of,Hearts),(Eight_of,Hearts),(Nine_of,Hearts),
        (Ten_of,Hearts),(Jack_of,Hearts),(Queen_of,Hearts),(King_of,Hearts),(Ace_of,Hearts),
        (Two_of,Clubs),(Three_of,Clubs),(Four_of,Clubs),(Five_of,Clubs),
        (Six_of,Clubs),(Seven_of,Clubs),(Eight_of,Clubs),(Nine_of,Clubs),
        (Ten_of,Clubs),(Jack_of,Clubs),(Queen_of,Clubs),(King_of,Clubs),(Ace_of,Clubs),
        (Two_of,Dimonds),(Three_of,Dimonds),(Four_of,Dimonds),(Five_of,Dimonds),
        (Six_of,Dimonds),(Seven_of,Dimonds),(Eight_of,Dimonds),(Nine_of,Dimonds),
        (Ten_of,Dimonds),(Jack_of,Dimonds),(Queen_of,Dimonds),(King_of,Dimonds),(Ace_of,Dimonds)]


shuffle [] = error "Empty List"
shuffle ls = xshuffle (take ((div) (# ls) 2) ls) [] (drop ((div) (# ls) 2) ls)
             where
             xshuffle []        x  []       = reverse x
             xshuffle (a:front) x  (b:rear) = xshuffle front (a:b:x) rear

shufflecards n x = (iterate shuffle x) ! n, if (#x) < 52
                 = error "hiding aces?"   , otherwise

|| 2012 Q3. Part II
|| Multiply 2 numbers add the digits and then if that has more than one digit
|| then add those until it is only one digit.
peter a b = xpeter (a*b)
            where
            xpeter x = foldr (+) 0 (getdigs (x))         , if #(getdigs(foldr (+) 0 (getdigs (x)))) = 1
                     = xpeter (foldr (+) 0 (getdigs (x))), otherwise

getdigs x = xgetdigs x []
            where
            xgetdigs 0 ls = ls
            xgetdigs x ls = xgetdigs ((div) x 10) (((mod) x 10):ls)


|| BEEFY List Comprehensions
||[n*n | n <-[1..100]; (mod) n 2 ~= 0]
|| get a list of all the digits of a number
listdig x = reverse (((mod) x 10) :[ (mod) ((div) x ((^) 10 n)) 10 | n <- [1..entier (log10 x)+1] ; (div) x ((^) 10 n) ~= 0 ])
