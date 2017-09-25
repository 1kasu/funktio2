

--a

class Applicative m => Monad m where
    (>>=) :: m a -> (a -> m b) -> m b
    
class Applicative f => VilleMonad f where--Kopioitu luennolta
    join :: f (f a) -> f a -- Cf. concat [[a]] -> [a]

--Monad
-- >>=
-- Applicative
-- pure
-- a -> f a
-- <*>
-- f(a -> b) -> f a -> f b
-- Functor
-- fmap
-- (a -> b) -> f a -> f b
-- f a -> (a -> f b) -> f b

(>>>>=) :: VilleMonad m => m a -> (a -> m b) -> m b
(>>>>=) a f = join (pure f <*> a)--Tämä tuli kohtuu helposti
    
    
join2 :: Prelude.Monad f => f (f a) -> f a--Helpompi testata, kun on prelude
join2 a = a Prelude.>>= id--Tämä taas puolivahingossa ja olin yllättynyt, että tämä toimi. Lähinnä vain kokeilin.
    

--b
mfmap :: Prelude.Monad m => (a -> b) -> m a -> m b
mfmap f a = a Prelude.>>= (\x -> pure (f x))

--En osaa/jaksa tehdä todistuksia...