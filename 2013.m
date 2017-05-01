|| 2013

|| Q1. (a)

dll ::= Empty | ConCons num dll dll

|| define a double linked list
testdll = ConCons 1 dll1  Empty
dll1    = ConCons 2 dll2  testdll
dll2    = ConCons 3 Empty dll1

|| print the list
printdll :: dll -> [char]
printdll Empty           = "Terminal"
printdll (ConCons x n p) = printprevdll p ++ " <=> " ++ shownum x ++ " <=> " ++ printnextdll n
                           where
                           printprevdll Empty           = "Terminal"
                           printprevdll (ConCons x n p) = printprevdll p ++ " <=> " ++ shownum x
                           printnextdll Empty           = "Terminal"
                           printnextdll (ConCons x n p) = shownum x ++ " <=> " ++ printnextdll n

|| prepend the list
addtodll :: num -> dll -> dll
addtodll a (ConCons x n p)  = tying
                              where
                              tying  = ConCons a knot Empty
                              knot   = ConCons x n tying

|| general insert
insert a Empty                            = ConCons a Empty Empty
insert a (ConCons x n Empty)              = prepend a x n
                                            where
                                            prepend a x n = addtodll a (ConCons x n Empty), if x>=a
                                                          = ConCons x (insert a n) Empty  , otherwise
insert a (ConCons x Empty p)              = append
                                            where
                                            append  = ConCons a Empty knot
                                            knot    = ConCons x append p
insert a (ConCons x n (ConCons px pn pp)) = ConCons x (insert a n) (ConCons px pn pp), if a>=x
                                          = tying                                    , otherwise
                                            where
                                            tying  = ConCons a knot the
                                            the    = ConCons px tying pp
                                            knot   = ConCons x n tying

|| Q2.

||Quicksort program.
|| An unsorted list of numbers is sorted by
|| recursively subdividing the list.
qsort:: [num] -> [num]
qsort [] = []
qsort (x:xs) = (qsort left) ++ [x] ++ (qsort right)
               where
               left  = filter ( < x) xs
               right = filter (>= x) xs

|| Insertion sort program.
|| Creates new list each iteration with use of drop and take filtering.
isort:: [num]->[num]
isort ls = xisort ls []
           where
           xisort [] sorted       = sorted
           xisort (x:rest) sorted = xisort rest ((takewhile (<x) sorted) ++ [x] ++ (dropwhile (<x) sorted))

|| All sort program.
|| Generalization of isort with polytypes and an input function to
|| specify the ordering.
asc  = (>)
desc = (<)
allsort:: (*->*->bool)->[*]->[*]
allsort f ls = xallsort ls [] f
               where
               xallsort  []      sorted f = sorted
               xallsort (x:rest) sorted f = xallsort rest ((takewhile (f x) sorted) ++ [x] ++ (dropwhile (f x) sorted)) f

|| Tuple sort.
|| Partial applicaiton of allsort.
sumtup:: (num,num)->num
sumtup (p,q) = p + q

comtup:: (num,num)->(num,num)->bool
comtup a b = True,  if (sumtup a) < (sumtup b)
           = False, otherwise

sorttup = allsort comtup

|| Dice sort.
|| Partial application of allsort for algerbraic type dice.
dice ::= One | Two | Three | Four | Five | Six

dicenext :: dice -> dice
dicenext One   = Two
dicenext Two   = Three
dicenext Three = Four
dicenext Four  = Five
dicenext Five  = Six

dicegreater :: dice -> dice -> bool
dicegreater Six b  = False
dicegreater a   b  = dicenext a = b \/ dicegreater (dicenext a) b

dicesort = reverse . (allsort  dicegreater)

|| Q3. 2013

op     ::= Plus | Minus | Times | Divide
