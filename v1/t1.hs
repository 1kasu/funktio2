import Data.Monoid hiding (Sum, Product)

-- a
newtype Count = Count {getCount :: Integer} deriving (Eq, Show)

instance Monoid Count where
    mempty = Count 0
    mappend (Count a) (Count b) = Count(a+b)
    
-- On useampi vuosi kun suoritin funktio yhden, joten en ole aivan varma miten todistukset tapahtuivat, mutta
-- alla oleva on mielestäni riittävä.
-- mempty <> Count a = Count 0 <> Count a = Count (0+a) = Count a => Toimii
-- Count a <> mempty = Count a <> Count 0 = Count (a+0) = Count a => Toimii
-- (Count a <> Count b) <> Count c = Count (a+b) <> Count c = Count ((a+b)+c) = Count (a+b+c)
-- Count a <> (Count b <> Count c)) = Count a <> Count (b+c) = Count (a+(b+c)) = Count (a+b+c) 
-- => Pätee. 


-- b 
newtype Sum a = Sum {getSum :: a} deriving (Eq, Show) --using the constraint Num a

instance Num a => Monoid (Sum a) where
    mempty = Sum 0
    mappend (Sum a) (Sum b) = Sum(a+b)
    
-- mempty <> Sum a = Sum 0 <> Sum a = Sum (0+a) = Sum a => Toimii
-- Sum a <> mempty = Sum a <> Sum 0 = Sum (a+0) = Sum a => Toimii
-- (Sum a <> Sum b) <> Sum c = Sum (a+b) <> Sum c = Sum ((a+b)+c) = Sum (a+b+c)
-- Sum a <> (Sum b <> Sum c)) = Sum a <> Sum (b+c) = Sum (a+(b+c)) = Sum (a+b+c)
-- => Pätee. 

-- c
newtype Product a = Product {getProduct :: a} deriving (Eq, Show) --using the constraint Num a

instance Num a => Monoid (Product a) where 
    mempty = Product 1
    mappend (Product a) (Product b) = Product(a*b)

-- mempty <> Product a = Product 1 <> Product a = Product (1*a) = Product a => Toimii
-- Product a <> mempty = Product a <> Product 1 = Product (a*1) = Product a => Toimii
-- (Product a <> Product b) <> Product c = Product (a*b) <> Product c = Product ((a*b)*c) = Product (a*b*c)
-- Product a <> (Product b <> Product c)) = Product a <> Product (b*c) = Product (a*(b*c)) = Product (a*b*c)
-- => Pätee.
    
    
-- d
newtype First a = First {getFirst :: Maybe a}

-- e
newtype Last a = Last {getLast :: Maybe a}

-- f
newtype Min a = Min {getMin :: a} --using the constraint Ord a

-- g
newtype Max a = Max {getMax :: a} --using the constraint Ord a

-- h
data NonEmpty a = a :| [a]

-- i
--newtype MonMap k a = Mon (Map k a) using the constraint Monoid a with Map being from Data.Map

-- j
newtype Predicate a = Pred (a -> Bool)

-- k
newtype Endo a = Endo {appEndo :: a -> a}

-- l
--a -> b using the constraint Monoid b


-- m
--(a, b) using the constraint (Monoid a, Monoid b)

-- n
--IO a using the constraint Monoid a 


