>

> structure * ::= Empty | Node * [structure *]

> mydata :: [structure char]
> mydata = [Node 'A' [mydata!2, mydata!1],
>           Node 'B' [mydata!3, mydata!0],
>           Node 'C' [mydata!0, mydata!3],
>           Node 'D' [mydata!4],
>           Node 'E' []]

> graph = hd (tl mydata)

> headchars :: [structure char] -> [char]
> headchars []                  = ""
> headchars ((Node c ls):rest)  = show c ++ " " ++ headchars rest

> dataflatten :: [structure char] -> [char]
> dataflatten []                = ""
> dataflatten ((Node c s):rest) = show c ++ " ( " ++ (headchars s) ++ ") " ++ (dataflatten rest)

> rabbithole :: structure char -> [char]
> rabbithole Empty       = "Emp"
> rabbithole (Node c ls) = xrabbithole ("[ " ++ show c ) ls
>                          where
>                          xrabbithole s []                 = s ++ " ]"
>                          xrabbithole s ((Node d ls):rest) = xrabbithole (xrabbithole (s ++ show d) ls) rest,if (~) (member s d)
>                                                           = s ++ " Seen ],"         ,otherwise



> combinetuple :: ([char],[char]) -> ([char],[char]) -> ([char],[char])
> combinetuple (v1,v2) (v3,v4) = (v1++v3,v2++v4)


> printgraph :: (structure char) -> [char]
> printgraph s = snd(xprintgraph [] s)
>                where
>                xprintgraph st Empty       = (st, "Empty")
>                xprintgraph st (Node n []) = ((n:st), "Node " ++ [n] ++ " empty")
>                xprintgraph st (Node n vs) = ((n:st), "Node " ++ [n] ++ " seen"), if (member st n)
>                                           = ((fst(foldfunc)++(n:st)), "Node " ++ [n] ++ " [" ++ snd(foldfunc) ++ "] "), otherwise
>                                             where
>                                             foldfunc = foldr (func) (base) (vs)
>                                             func = (combinetuple . (xprintgraph (n:st)))
>                                             base = ("","")
>
