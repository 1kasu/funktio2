-- a
newtype Count = Count {getCount :: Integer}


-- b 

newtype Sum a = Sum {getSum :: a} using the constraint Num a

-- c
newtype Product a = Product {getProduct :: a} using the constraint Num a

-- d
newtype First a = First {getFirst :: Maybe a}

-- e
newtype Last a = Last {getLast :: Maybe a}

-- f
newtype Min a = Min {getMin :: a} using the constraint Ord a

-- g
newtype Max a = Max {getMax :: a} using the constraint Ord a

-- h
data NonEmpty a = a :| [a]

-- i
newtype MonMap k a = Mon (Map k a) using the constraint Monoid a with Map being from Data.Map

-- j
newtype Predicate a = Pred (a -> Bool)

-- k
newtype Endo a = Endo {appEndo :: a -> a}

-- l
a -> b using the constraint Monoid b


-- m
(a, b) using the constraint (Monoid a, Monoid b)

-- n
IO a using the constraint Monoid a


