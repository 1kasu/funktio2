import Prelude hiding (Functor, fmap)

class Functor f where
    fmap :: (a -> b) -> f a -> f b

--a
instance Functor Maybe where
    fmap _ Nothing = Nothing
    fmap f (Just a) = Just (f a)

--b
instance Functor ((,) c) where
    fmap f (c, a) = (c, f a)
    
--c
newtype ZipList a = Z [a] deriving (Show, Eq)

instance Functor (ZipList) where
    fmap f (Z []) = Z []
    fmap f (Z (x:ls)) = liita (Z [f x]) (fmap f (Z ls))
        where
            liita (Z a) (Z b) = Z (a ++ b)



