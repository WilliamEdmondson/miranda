|| List operators ++ : # ! and --

list = ["mon","tue"]
days = list ++ ["wed","thurs"]

|| Tuple

tup = ("Yes", True, 1)

|| Guarded Equations, highest common divisor

hcf a b = hcf (a-b) b, if a>b
        = hcf a (b-a), if a<b
        = a,           if a=b

|| where clause quadratic formula, looking at determinant

quadsolve a b c = error "complex roots",    if delta<0
                = [-b/(2*a)],               if delta=0
                = [-b/(2*a) + radix/(2*a),
                   -b/(2*a) - radix/(2*a)], if delta>0
                  where
                  delta = b*b - 4*a*c
                  radix = sqrt delta

|| Pattern Matching, numerical examples factorial
|| ackerman and fibinacci

fac 0     = 1
fac (n+1) = (n+1) * fac n

ack 0 n         = n+1
ack (m+1) 0     = ack m 1
ack (m+1) (n+1) = ack m (ack (m+1) n)

fib 0 = 0
fib 1 = 1
fib (n+2) = fib (n+1) + fib n

|| Pattern Matching, list examples

xsum [] = 0
xsum (a:x) = a + xsum x

xproduct [] = 1
xproduct (a:x) = a * xproduct x

xreverse [] = []
xreverse (a:x) = xreverse x ++ [a]

|| higher order functions : output is 16 why?

answer = twice twice twice suc 0
        twice f x = f (f x)
        suc x = x + 1

|| partical application of functions

vowel = member ['a','e','i','o','u']
myDigit = member ['0','1','2','3','4','5','6','7','8','9']
month = member ["Jan","Feb","Mar","Apr","Jun","Jul","Aug","Sep",
                "Oct","Nov","Dec"]

|| foldr function

myfoldr op k []    = k
myfoldr op k (a:x) = op a (myfoldr op k x)

|| All the standard list processing functions
|| can be obtained by partially applying foldr.
|| Examples:
||
||        sum = foldr (+) 0
||        product = foldr (*) 1
||        reverse = foldr postfix []
||                  where postfix a x = x ++ [a]

|| List comprehestions
|| List iterators with the form [body | qualifiers]

mysquares = [ n*n | n <- [1..100] ]

|| example using a filter qualifier (boolean expn restricting results)

factors n = [ i | i <- [1..n div 2]; n mod i = 0 ]

|| Hoare's "Quicksort" algorithm
qsort [] = []
qsort (a:x) = qsort [ b | b <- x; b<=a ]
              ++ [a] ++
              qsort [ b | b <- x; b>a ]

|| Lazy Evaluation
|| Consequece 1 : Non Strict Functions
|| Functions where it is possible to get results although one arg
|| is undefined. Try cond True 0 (1/0)

cond True x y = x
cond False x y = y

|| Infinite Data Structures

ones = 1 : ones

myrepeat a = x
             where x = a : x

nats = [0..]

odds = [1,3..]

squares = [ n*n | n <- [0..] ]

perfects = [ n | n <- [1..]; sum(factors n) = n ]

primes = sieve [ 2.. ]
         where
         sieve (p:x) = p : sieve [ n | n <- x; n mod p > 0 ]

|| Use of infinite lists is to act as lookup
|| tables for caching the values of a function.

infib 0 = 1
    infib 1 = 1
    infib (n+2) = flist!(n+1) + flist!n
                  where
                  flist = map infib [ 0.. ]


|| User defined types



|| Tying the Knot
alternates = x
             where
             x = 0 : y
             y = 1 : x

printalt 1 = ['1']
printalt n = concat [shownum (alternates ! n), printalt (n-1)]

|| coursework
|| 1. define a graph
|| (i) an algebraic type definition for a type called graph,
|| which contains numbers stored in each node, and where each
|| node has two subgraphs.  Notice that this is called "graph"
|| rather than "tree" because one or more of the subtrees or
|| their descendents may refer back to the existing node.

graph * ::= Empty | Node num [graph *]

|| 2. make the testgraph
|| (ii) a value called testgraph of type graph which
|| is the top node (call it node1) of a cyclic graph.
|| Node1 should contain the value 23 - its left subgraph
|| should be node2 and its right subgraph should be node3.
|| Node2 should contain value 13 - its left subgraph will
|| be empty and its right subgraph should be node3.
|| Node3 should contain the value 5 - its left subgraph
|| should be node1 and its right subgraph should be node2.
||
|| Require a head node with

cyclicgraph :: [graph num]
cyclicgraph =  [Node 23 [(cyclicgraph ! 1), (cyclicgraph ! 2)],
               Node 13 [Empty, (cyclicgraph ! 2)],
               Node 5  [(cyclicgraph ! 0), (cyclicgraph ! 1)]]

|| hd of cyclicgraph

testgraph :: graph num
testgraph = hd cyclicgraph



|| 3. Print
|| (iii) a function called printgraph which takes
|| a value of type graph and returns a printable
|| representation of that graph of type [char].
|| This function must not go into an infinite loop.
|| It may assume that every value stored in a node
|| of the graph is unique (and use this knowledge
|| to prevent infinite looping).  It should
|| print "Empty" for an empty graph, and it
|| should print "Seen x" for a Node that has been
|| previously seen and already printed (and which
||  stores value x).  The function printgraph can
||  call whatever other functions it needs.  You may
|| find the built-in function shownum useful.

printgraph :: graph * -> [char]
printgraph (Node x gs) = "Node " ++ shownum  x ++ (xpg gs[x])

xpg :: [graph *]->[num]->[char]
xpg []                y = []
xpg (Empty : gs)      y = " (Empty)" ++ (xpg gs y)
xpg ((Node x ns):gs)  y = " (Seen " ++ shownum x   ++")"++ (xpg gs (x:y)) , if (member y x)
                        = " (Node " ++ shownum x ++  (xpg ns (x:y)) ++ ")" ++ (xpg gs (xseen ns [])) , otherwise

xseen :: [graph *] -> [num] -> [num]
xseen ((Node x ns):gs) n = (xseen ns (x:n)) ++ (xseen gs (x:n)), if ~(member n x)
xseen (Empty : gs) n     = (xseen gs n)
xseen ((Node x ns):gs) n = n, otherwise


 ||Output:    Node 23 (Node 13(Empty)  (Node 5 (Seen 23) (Seen 13))) (Seen 5)
 ||expected : Node 23 (Node 13 (Empty) (Node 5 (Seen 23) (Seen 13))) (Seen 5)

|| (iv) an application of the function printgraph to the value testgraph.

main = printgraph testgraph
