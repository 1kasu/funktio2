import Data.Monoid

class Functor f => ZipA f where
  zunit :: f ()
  zzip :: f a -> f b -> f (a, b)
  
--a

instance ZipA Maybe where
    zunit = Just()
    zzip Nothing _ = Nothing
    zzip (Just _) Nothing = Nothing
    zzip (Just a) (Just b) = Just (a,b)
    
--b

instance (Monoid c) => ZipA ((,) c) where
    zunit = (mempty,())
    zzip (a,b) (c,d) = ((a <> c), (b, d))--Kääntäjä sattui hyväksymään tämän...
    
--c

newtype ZipList a = Z [a] deriving (Show, Eq)

instance Functor (ZipList) where
    fmap f (Z []) = Z []
    fmap f (Z (x:ls)) = liita (Z [f x]) (fmap f (Z ls))
        where
            liita (Z a) (Z b) = Z (a ++ b)

instance ZipA ZipList where
    zunit = Z []
    zzip (Z []) _ = Z []
    zzip _ (Z []) = Z []
    zzip (Z (l:ls)) (Z (h:hs)) = yhdista (Z [(l, h)]) (zzip (Z ls) (Z hs))
        where--Lievästi sanoen huonoja virheviestejä tuli tässä vastaan.
            yhdista (Z a) (Z b) = Z (a ++ b)

tulosta (Z ls) = print ls
    