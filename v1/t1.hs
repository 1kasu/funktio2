import Data.Monoid hiding (Sum, Product, First, Last, Endo)
import qualified Data.Map as Map
import Data.Semigroup hiding (Min, Sum, Product, First, Last, Max, Endo)

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
newtype First a = First {getFirst :: Maybe a} deriving (Eq, Show)

instance Monoid (First a) where
    mempty = First Nothing
    mappend (First (Just a)) _ = First (Just a)
    mappend (First Nothing) b = b
    
-- mempty <> First a = First Nothing <> First a = First a => Toimii
-- First a <> mempty
-- haarautetaan kahteen vaihtoehtoon eli First (Just a) ja First Nothing
    -- First (Just a) <> mempty = First (Just a) <> First Nothing = First (Just a) => Toimii, jos Just a
    -- First Nothing <> mempty = First Nothing <> First Nothing = First Nothing => Toimii, jos Nothing
    -- => Toimii molemmissa(kaikissa) tapauksissa
-- (First a <> First b) <> First c ja First a <> (First b <> First c)
-- haarautetaan a = First (Just 'a)
    -- (First (Just 'a) <> First b) <> First c = First (Just 'a) <> First c = First (Just 'a)
    -- First (Just 'a) <> (First b <> First c) = First (Just 'a) <> First _ = First (Just 'a)
    -- => Toimii
--  ja a = Nothing
    -- (First Nothing <> First b) <> First c = First b <> First c
    -- First Nothing <> (First b <> First c) = (First b <> First c) = First b <> First c
    -- => Toimii
-- => Toimii

-- e
newtype Last a = Last {getLast :: Maybe a} deriving (Eq, Show)

instance Monoid (Last a) where
    mempty = Last Nothing
    mappend _ (Last (Just b)) = Last (Just b)
    mappend a (Last Nothing) = a

-- mempty <> Last a = Last Nothing <> Last a
-- haarautetaan a = Nothing
    -- Last Nothing <> Last Nothing = Last Nothing => Toimii
-- ja a = Just 'a
    -- Last Nothing <> Last (Just 'a) = Last (Just 'a) => Toimii
-- => Toimii
-- (Last a <> Last b) <> Last c ja Last a <> (Last b <> Last c)
-- Haarautetaan c = Just 'c
    -- (Last a <> Last b) <> Last (Just 'c) = _ <> Last (Just 'c) = Last (Just 'c)
    -- Last a <> (Last b <> Last (Just 'c)) = Last a <> ( _ <> Last (Just 'c)) = Last a <> Last (Just 'c) = Last (Just 'c)
    -- => Toimii
-- ja c = Nothing
    -- (Last a <> Last b) <> Last Nothing = (Last a <> Last b) = Last a <> Last b
    -- Last a <> (Last b <> Last Nothing) = Last a <> (Last b) = Last a <> Last b*c
    -- => Toimii
-- => Toimii
    

-- f
newtype Min a = Min {getMin :: a} deriving (Eq, Show) --using the constraint Ord a

instance Ord a => Semigroup (Min a) where
    (<>) (Min a) (Min b) = if (a>b) then Min b else Min a
    
-- Min a <> (Min b <> Min c) ja (Min a <> Min b) <> Min c
-- Käydään kaikki vaihtoehdot läpi
-- a < b < c
    -- Min a <> (Min b <> Min c) = Min a <> Min b = Min a
    -- (Min a <> Min b) <> Min c = Min a <> Min c = Min a
-- a < c < b
    -- Min a <> (Min b <> Min c) = Min a <> Min c = Min a
    -- (Min a <> Min b) <> Min c = Min a <> Min c = Min a
-- b < a < c
    -- Min a <> (Min b <> Min c) = Min a <> Min b = Min b
    -- (Min a <> Min b) <> Min c = Min b <> Min c = Min b
-- b < c < a
    -- Min a <> (Min b <> Min c) = Min a <> Min b = Min b
    -- (Min a <> Min b) <> Min c = Min b <> Min c = Min b
-- c < a < b
    -- Min a <> (Min b <> Min c) = Min a <> Min c = Min c
    -- (Min a <> Min b) <> Min c = Min a <> Min c = Min c
-- c < b < a
    -- Min a <> (Min b <> Min c) = Min a <> Min c = Min c
    -- (Min a <> Min b) <> Min c = Min b <> Min c = Min c
-- => Toimii kaikilla.
-- Lisäksi jos a = b
    -- Min a <> Min b = Min a eli otetaan aina yhtäsuurista vasemmanpuoleinen.
-- a < b ja b = c
    -- Min a <> (Min b <> Min c) = Min a <> Min b = Min a
    -- (Min a <> Min b) <> Min c = Min a <> Min c = Min a
-- b < a ja a = c
    -- Min a <> (Min b <> Min c) = Min a <> Min b = Min b
    -- (Min a <> Min b) <> Min c = Min b <> Min c = Min b
-- c < a ja b = a
    -- Min a <> (Min b <> Min c) = Min a <> Min c = Min c
    -- (Min a <> Min b) <> Min c = Min a <> Min c = Min c
-- a > b ja b = c
    -- Min a <> (Min b <> Min c) = Min a <> Min b = Min b
    -- (Min a <> Min b) <> Min c = Min b <> Min c = Min b
-- b > a ja a = c
    -- Min a <> (Min b <> Min c) = Min a <> Min c = Min a
    -- (Min a <> Min b) <> Min c = Min a <> Min c = Min a
-- c > a ja b = a
    -- Min a <> (Min b <> Min c) = Min a <> Min b = Min a
    -- (Min a <> Min b) <> Min c = Min a <> Min c = Min a
-- a = b = c
    -- Min a <> (Min b <> Min c) = Min a <> Min b = Min a
    -- (Min a <> Min b) <> Min c = Min a <> Min b = Min a
-- => Toimii kaikille yhdistelmille



-- g
newtype Max a = Max {getMax :: a} deriving (Eq, Show)--using the constraint Ord a

instance Ord a => Semigroup (Max a) where
    (<>) (Max a) (Max b) = if (a<b) then Max b else Max a
    
-- Max a <> (Max b <> Max c) ja (Max a <> Max b) <> Max c
-- Käydään kaikki vaihtoehdot läpi
-- a > b > c
    -- Max a <> (Max b <> Max c) = Max a <> Max b = Max a
    -- (Max a <> Max b) <> Max c = Max a <> Max c = Max a
-- a > c > b
    -- Max a <> (Max b <> Max c) = Max a <> Max c = Max a
    -- (Max a <> Max b) <> Max c = Max a <> Max c = Max a
-- b > a > c
    -- Max a <> (Max b <> Max c) = Max a <> Max b = Max b
    -- (Max a <> Max b) <> Max c = Max b <> Max c = Max b
-- b > c > a
    -- Max a <> (Max b <> Max c) = Max a <> Max b = Max b
    -- (Max a <> Max b) <> Max c = Max b <> Max c = Max b
-- c > a > b
    -- Max a <> (Max b <> Max c) = Max a <> Max c = Max c
    -- (Max a <> Max b) <> Max c = Max a <> Max c = Max c
-- c > b > a
    -- Max a <> (Max b <> Max c) = Max a <> Max c = Max c
    -- (Max a <> Max b) <> Max c = Max b <> Max c = Max c
-- => Toimii kaikilla
-- a > b ja b = c
    -- Max a <> (Max b <> Max c) = Max a <> Max b = Max a
    -- (Max a <> Max b) <> Max c = Max a <> Max c = Max a
-- b > a ja a = c
    -- Max a <> (Max b <> Max c) = Max a <> Max b = Max b
    -- (Max a <> Max b) <> Max c = Max b <> Max c = Max b
-- c > a ja b = a
    -- Max a <> (Max b <> Max c) = Max a <> Max c = Max c
    -- (Max a <> Max b) <> Max c = Max a <> Max c = Max c
-- a < b ja b = c
    -- Max a <> (Max b <> Max c) = Max a <> Max b = Max b
    -- (Max a <> Max b) <> Max c = Max b <> Max c = Max b
-- b < a ja a = c
    -- Max a <> (Max b <> Max c) = Max a <> Max c = Max a
    -- (Max a <> Max b) <> Max c = Max a <> Max c = Max a
-- c < a ja b = a
    -- Max a <> (Max b <> Max c) = Max a <> Max b = Max a
    -- (Max a <> Max b) <> Max c = Max a <> Max c = Max a
-- a = b = c
    -- Max a <> (Max b <> Max c) = Max a <> Max b = Max a
    -- (Max a <> Max b) <> Max c = Max a <> Max b = Max a
-- => Toimii kaikille yhdistelmille


-- h
data NonEmpty a = a :| [a] deriving (Eq, Show)

instance Semigroup (NonEmpty a) where
    (<>) (a :| al) (b :| bl) = a :| (concat [al,[b],bl])-- On kai parempiakin tapoja yhdistellä listoja.
    
-- ((a :| al) <> (b :| bl)) <> (c :| cl) = (a :| [al, b, bl]) <> (c :| cl) = a :| [al, b, bl, c, cl]
-- (a :| al) <> ((b :| bl) <> (c :| cl)) = (a :| al) <> (b :| [bl, c, cl]) = a :| [al, b, bl, c, cl]
-- => Toimii

-- i
newtype MonMap k a = Mon (Map.Map k a) deriving (Eq, Show)--using the constraint Monoid a with Map being from Data.Map
instance (Ord k, Monoid m) => Monoid (MonMap k m) where
    mempty = Mon (Map.empty)
    mappend (Mon x) (Mon y) = Mon (Map.unionWith mappend x y)
    
-- Mon a <> mempty = Mon a <> Mon (Map.empty) = Mon a
-- mempty <> Mon a = Mon (Map.empty) <> Mon a = Mon a
-- => Toimii
-- (Mon a <> Mon b) <> Mon c = Mon (union (a,b)) <> Mon c = Mon (union (a,b,c))
-- Mon a <> (Mon b <> Mon c) = Mon a <> Mon (union (b,c)) = Mon (union (a,b,c))
-- => Toimii kunhan unioni toimii oikealla tavalla ja käsittääkseni sen pitäisi.

-- j
newtype Predicate a = Pred (a -> Bool)

instance Monoid (Predicate f) where
    mempty = Pred (\x -> True)
    mappend (Pred f) (Pred g) = Pred (\x -> f x && g x)
    
-- mempty <> Pred f = (\x -> True) <> Pred f = Pred (\x -> True && f x) = Pred (\x -> f x) = Pred f
-- Pred f <> memtpy = Pred f <> (\x -> True) = Pred (\x -> f x && True) = Pred (\x -> f x) = Pred f
-- => Toimii
-- (Pred f <> Pred g) <> Pred h = Pred (\x -> f x && g x) <> Pred h = Pred (\y -> (\x -> f x && g x) y && h y) = Pred (\x -> f x && g x && h x)
-- Pred f <> (Pred g <> Pred h) = Pred f <> Pred (\x -> g x && h x) = Pred (\y -> f y && (\x -> g x && h x) y) = Pred (\x -> f x && g x && h x)
-- => Toimii. En ole kuitenkaan varma haettiinko Predicatella juuri tätä ratkaisua. Mielestäni toimisi myös or:lla ja mempty False


-- k
newtype Endo a = Endo {appEndo :: a -> a}

instance Monoid (Endo f) where
    mempty = Endo (\f -> f)
    mappend (Endo f) (Endo g) = Endo (f . g)
    
-- mempty <> Endo f = Endo (\g -> g) <> Endo f = Endo ((\g -> g) . f) = Endo f
-- Endo f <> mempty = Endo f <> Endo (\g -> g) = Endo ( f . (\g -> g)) = Endo f
-- (Endo f <> Endo g) <> Endo h = Endo (f . g) <> Endo h = Endo ((f . g) . h) = Endo (f . g . h)
-- Endo f <> (Endo g <> Endo h) = Endo f <> Endo (f . g) = Endo (f . (g . h)) = Endo (f . g . h)
-- => Toimii

-- l
--a -> b --using the constraint Monoid b
newtype Fun a b = Fun {getFun :: a -> b}


instance (Monoid b) => Semigroup (Fun a b) where 
    (<>) (Fun f) (Fun g) = Fun(\x -> f x Data.Monoid.<> g x)
    
-- (Fun f <> Fun g) <> Fun h = Fun (\x -> f x <m> g x) <> Fun h = Fun (\y -> (\x -> f x <m> g x) y <m> h x) = Fun (\y -> f y <m> g y <m> h y)
-- Fun f <> (Fun g <> Fun h) = Fun f <> Fun (\x -> g x <m> h x) = Fun (\y -> f y <m> (\x -> g x <m> h x) y) = Fun (\y -> f y <m> g y <m> h y)
-- => Toimii


-- m
--(a, b) using the constraint (Monoid a, Monoid b)
newtype Kiva a b = Kiva {getKiva :: (a,b)} deriving (Eq, Show)
instance (Monoid a, Monoid b) => Semigroup (Kiva a b) where
    (<>) (Kiva (a, b)) (Kiva (c, d)) = Kiva (a Data.Monoid.<> c, b Data.Monoid.<> d)
    
-- Kiva ((a,b) <> Kiva (c,d)) <> Kiva (e,f) = Kiva ((a <m> c), (b <m> d)) <> Kiva (e,f) = Kiva ((a <m> c) <m> e, (b <m> d) <m> f) = Kiva (a <m> c <m> e, b <m> d <m> f)
-- Kiva (a,b) <> (Kiva (c,d) <> Kiva (e,f)) = Kiva (a,b) <> Kiva (c <m> e, d <m> f) = Kiva (a <m> c <m> e, b <m> d <m> f)
-- => Toimii



newtype IO a = IO {getIO :: a} deriving (Eq, Show)

-- n
--IO a using the constraint Monoid a 
instance Monoid a => Monoid (Main.IO a) where
    mempty = IO mempty
    mappend (IO a) (IO b) = IO (a Data.Monoid.<> b)

-- mempty <> IO a = IO mempty <> IO a = IO (mempty <> a) = IO a
-- IO a <> mempty = IO a <> IO mempty = IO (a <> mempty) = IO a
-- => Toimii
-- IO a <> (IO b <> IO c) = IO a <> IO (b <> c) = IO (a <> (b <> c)) = IO (a <> b <> c)
-- (IO a <> IO b) <> IO c = IO (a <> b) <> IO c = IO ((a <> b) <> c) = IO (a <> b <> c)
-- => Toimii kai. En ole aivan varma oikeasta toteutuksesta.
