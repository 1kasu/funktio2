import Prelude hiding (Applicative, (<*>))
import Data.Monoid


class Functor f => Applicative f where
    pure :: a -> f a
    (<*>) :: f (a -> b) -> f a -> f b

--a
instance Applicative Maybe where
    pure a = Just a
    (<*>) (Just f) (Just a) = Just (f a)
    (<*>) _ _ = Nothing
    
    
--b

testia = ([4], (+4))
testib = ([5], 2)

instance (Monoid c) => Applicative ((,) c) where
    pure a = (mempty, a)
    (<*>) (a,b) (c,d) = (a <> c, b d)
    --Kaipa tää toimii. Ainakin testi käytäytyy oikealla tavalla...
    
    
--c

newtype ZipList a = Z [a] deriving (Show, Eq)

instance Functor (ZipList) where
    fmap f (Z []) = Z []
    fmap f (Z (x:ls)) = liita (Z [f x]) (fmap f (Z ls))
        where
            liita (Z a) (Z b) = Z (a ++ b)

instance Applicative ZipList where
    pure a = Z [a]
    (<*>) (Z []) _ = Z []
    (<*>) _ (Z []) = Z []
    (<*>) (Z (f:fl)) (Z (s:sl)) = liita (Z ([f s])) (Z fl <*> Z sl)
        where
            liita (Z a) (Z b) = Z (a ++ b)