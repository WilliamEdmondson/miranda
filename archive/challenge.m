message = "Hello World"
|| CHALLENGE
||
|| Function which takes a list of numbers and returns a three tuple
|| List contains only 012s
|| count all 1s before first 0
|| count all 2s before first 0
|| longest string of of 1s before first 0
|| this uses

|| Soln 1

challenge1 :: [num] -> (num,num,num)
challenge1 any = xchallenge any (0,0,0,0)						|| defines the function xchallenge which passes any and sets abc&d to 0

xchallenge (0:xs) (a,b,c,d) = (a,b,max[c,d]) 					|| c length of the longest sequence so far d count for current sequence
xchallenge (1:xs) (a,b,c,d) = xchallenge(xs) (a+1,b,c,d+1)  	|| a 1s accumulator
xchallenge (1:xs) (a,b,c,d) = xchallenge(xs) (a+1,b,max[c,d],d) || b 2s accumulator max[c,d] checks for a new longest sequence
xchallenge any (a,b,c,d)	= error "bad input format"			

|| Soln 2

challenge2 any = (a,b,c)
				where 
				a = #(filter(=1) segment)
				b = #(filter(=2) segment)
				c = f segment 0 0 			  					|| f is the function for the longest string of 1s, has 3 parameters 
				segment = takewhile (~=0) any				 	|| takewhile keeps items in list until condition is met
				f [] longest current 		= max[longest, current]
				f (2:xs) longest current 	= f xs (max[current, longest]) 0
				f (2:xs) longest current 	= f xs longest (current+1)
				f any longest current  		= error "bad input format"
