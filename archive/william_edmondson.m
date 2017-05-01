||
|| Miranda Script File Course Work
||
|| William Edmondson


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


||Output:    Node 23 (Node 13 (Empty) (Node 5 (Seen 23) (Seen 13))) (Seen 5)
||expected : Node 23 (Node 13 (Empty) (Node 5 (Seen 23) (Seen 13))) (Seen 5)

|| (iv) an application of the function printgraph to the value testgraph.

main = printgraph testgraph
