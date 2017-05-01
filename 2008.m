|| foldr op base list

|| Standard
mymap op []       = []
mymap op (x:rest) = (op x):(mymap op rest)

|| List Comprehensions
compmap op x = [ op n | n<-[1..#x]]

||foldmap op (x:rest) = foldr (\y ys -> (f y):ys) [] xs
||foldmap op []       = []
||foldmap op (x:rest) = foldr (filter x ) [] rest


||Q4.

set * ::= Nil | Item * (set *)

exset1 = Item 1 (Item 2 (Item 3 Nil))
exset2 = Item 2 (Item 4 (Item 8 Nil))
exset3 = Item 3 (Item 2 (Item 1 Nil))

memberset :: (set *) -> (set *) -> bool
memberset m Nil                    = False
memberset (Item m s) (Item v rest) = True, if (m=v)
                                   = memberset (Item m s) rest, otherwise

|| Just a play around function testing things when I couldn't get
|| member to work properly
has3 :: set num->bool
has3 Nil        = False
has3 (Item 3 s) = True
has3 (Item i s) = has3 s

filterset :: (*->bool)->set *->set *
filterset f Nil = Nil
filterset f (Item i s) = Item i (filterset f s) , if f i = True
                       = filterset f s          , if f i = False
                       = error "Function not TF", otherwise

nulset = Nil

addset :: set * -> set * -> set *
addset Nil s          = s
addset s Nil          = s
addset (Item i Nil) s = Item i s
addset (Item i s) r   = Item i (addset s r)

subset :: set * -> set * -> set *
subset Nil s                 = s
subset s Nil                 = s
subset (Item i s) (Item i r) = r
subset (Item i s) (Item j r) = Item j (subset (Item i s) r)

union :: set * -> set * -> set *
union = addset

interset :: set * -> set * -> set *
interset s Nil          = Nil
interset Nil s          = Nil
interset (Item i s) r   = union (filterset (=i) r) (interset s r)


showset :: set *->[*]
showset Nil        = []
showset (Item i s) = i:(showset s)
