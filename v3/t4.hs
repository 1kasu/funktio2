import Data.Monoid
import Prelude hiding (sequence, sequence_, mapM, mapM_)

--Monad
-- >>=
-- f a -> (a -> f b) -> f b
-- Applicative
-- pure
-- a -> f a
-- <*>
-- f(a -> b) -> f a -> f b
-- Functor
-- fmap
-- (a -> b) -> f a -> f b

liita :: Monad m => m [a] -> m [a] -> m [a]
liita a b = (fmap (\x -> fmap ( x ++) b) a) >>= id

tauluta :: Monad m => m a -> m [a]
tauluta a = a >>= (\x -> pure [x])

lisaa :: Monad m => m [a] -> m a -> m [a]
lisaa a b = liita a (tauluta b)

sequence :: Monad m => [m a] -> m [a]
sequence a = foldl lisaa (pure []) a

esim1 = sequence [[43,1],[12,13]] -- = [[43,12],[43,13],[1,12],[1,13]]
esim2 = sequence [Just 3, Just 32, Just 1] -- = Just [3,32,1]. Jos yksikin on Nothing, niin ei voidaan tulkita, ettei muillakaan ole mitään väliä.

tyhjaa :: Monad m => m a -> m ()
tyhjaa a = a >>= (\x -> pure ())

sequence_ :: Monad m => [m a] -> m ()
sequence_ a = tyhjaa ( sequence a )

esim3 = sequence_ [Just 2, Just 32] -- = Just () (Tiedetään oliko jotain edes.)


mapM :: Monad m => (a -> m b) -> [a] -> m [b]
mapM f ls = sequence (fmap f ls )

esim4 = mapM (\x -> if x >= 0 then Just (x+3) else Nothing) [ 2, 455, 23] -- == Just [5,458,26]
-- Jos yksikin luku on nollaa pienempi, niin sitten meitä ei kiinnosta tulos ollenkaan vaan voidaan
-- yhtä hyvin palauttaa Nothing.

mapM_ :: Monad m => (a -> m b) -> [a] -> m ()
mapM_ f ls = tyhjaa ( mapM f ls)

esim5 = mapM_ (\x -> if x >= 0 then Just (x+3) else Nothing) [ 2, 455, 23] -- == Just () oliko lasku laskettavissa.

filterM :: Monad m => (a -> m Bool) -> [a] -> m [a]
filterM f [] = pure []
filterM f (l:ls) = liita ((\x -> x >>= (\y -> if y then pure [l] else pure [])) (f l) ) (filterM f ls)

esim6 = filterM (\x -> if x==0 then Nothing else Just True) [1,0,0] -- Nothing
-- filtteröi, mutta jos ei pystytä toteamaan totuutta (esim Nothing), niin palautetaan sen mukaisesti arvo (esim Nothing), koko listasta.
esim7 = filterM (\x -> if x==0 then Just False else Just True) [1,0,0] -- Just [1]
