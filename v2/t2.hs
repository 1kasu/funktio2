class Functor f where
    fmap :: (a -> b) -> f a -> f b

--a
newtype Identity a = Identity a deriving (Show, Eq)

instance Main.Functor Identity where
    fmap f (Identity i) = Identity (f i)--En ole varma, mikä tämän pointti on?
    

    
--b
newtype Compose f g a = Compose (f (g a))

tulosta (Compose a) = print a

instance (Prelude.Functor f, Prelude.Functor g) => Main.Functor (Compose f g) where
    fmap f (Compose h) = Compose (Prelude.fmap (\x -> Prelude.fmap f x) h)
    --Voisin käyttää myös Main.fmap, mutta se ei ole toteutettu kovinkaan monelle...
    
--c
newtype State s a = State (s -> (a, s))

instance Main.Functor (State s) where
    fmap f (State g) = State(\jokin ->
        let (res, jokin1) = g jokin in (f res, jokin1))--Kaytannossa suoraan luentomateriaalista
        
testi = State (\x -> (2, x))

syota :: State s a -> s -> (a, s)
syota (State f) s = f s