import Data.Monoid

class Applicative f => VilleMonad f where--Kopioitu luennolta
    join :: f (f a) -> f a -- Cf. concat [[a]] -> [a]

--a

instance VilleMonad Maybe where
    join (Just (Just a)) = Just a
    join _ = Nothing
    
--b

instance Monoid c => VilleMonad ((,) c) where
    join (c,(a,b)) = (c <> a, b)
    
testia = ([3,4,5,6],([0,0,0,0], "Kaali"))
--Tämä ainakin antaa samat tulokset kirjaston ja oman tekemän välillä.
    