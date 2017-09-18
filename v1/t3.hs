import Data.Monoid

-- Toimiva ratkaisu on ihan pohjalla. Tässä näkyvät aiemmat yritykset pikkuhiljaa kehittyen.

newtype Tulos a b = Tulos {getTulos :: (a, (a -> b -> a))}


instance (Num a, Num b) => Monoid (Tulos a b) where
    mempty = Tulos (3, (\x y -> x))
    mappend (Tulos (a, f)) (Tulos (b, g)) = Tulos (a, f)
    

data Testi a b = Testi {apu :: a, f :: (a -> b -> a), g :: (a -> b)}

instance (Num a, Num b) => Monoid (Testi a b) where
    mempty = Testi {apu = 2, f = (\x y -> x), g = (\x -> 3)}
    mappend (Testi a f1 f2) (Testi b g1 g2) = Testi (f1 a (g2 b)) f1 f2  
    
data Farvo a b = Farvo {funktio :: (b -> a -> b), arvo :: Either a b}
    
data Testi2 a b = Testi2 {getTesti2 :: Maybe (Farvo a b)}


tulosta2 (Testi2 Nothing) = print "Ei ole."
tulosta2 (Testi2 (Just (Farvo _ x))) = print x

tulosta (Testi3 Nothing) = print "Ei ole."
tulosta (Testi3 (Just (Farvo2 _ (Right (Left _))))) = print "Valivaihe"
tulosta (Testi3 (Just (Farvo2 _ (Left x)))) = print (x * (-1))
tulosta (Testi3 (Just (Farvo2 _ (Right (Right x))))) = print x

instance Monoid (Testi2 a b) where
    mempty = Testi2 Nothing
    mappend (Testi2 Nothing) (Testi2 Nothing) = Testi2 Nothing
    mappend (Testi2 Nothing) b = b--oikea
    mappend (Testi2 (Just (Farvo f (Left x)))) (Testi2 Nothing) = Testi2 (Just (Farvo f (Left x)))
    mappend a (Testi2 Nothing) = a--oikea
    mappend (Testi2 (Just (Farvo f (Left _)))) (Testi2 (Just (Farvo f2 a2))) = Testi2 Nothing
    mappend (Testi2 (Just (Farvo f (Right a)))) (Testi2 (Just (Farvo f2 (Right _)))) = Testi2 Nothing
    mappend (Testi2 (Just (Farvo f (Right x)))) (Testi2 (Just (Farvo f2 (Left y)))) = Testi2 (Just (Farvo f (Right (f x y))))
    
--(Foldable f, Monoid m) => (a -> m) -> f a -> m

--foldMap f l
--mempty = b 
    
omaFoldl :: (b -> a -> b) -> b -> [a] -> b
omaFoldl f b l = case (foldMap (\x -> x) bapu) of
                    Testi2 (Just (Farvo _ (Right a))) -> a
                    _ -> b
                where
                    b1 = Testi2 (Just (Farvo f (Right b)))
                    l1 = map (\y -> Testi2 (Just (Farvo f (Left y)))) l
                    bapu = [b1] ++ l1
                    
testa = Testi3 (Just (Farvo2 (+) (Left 4)))
testb = Testi3 (Just (Farvo2 (+) (Right (Right 10))))
testc = testb <> testa
testd = testa <> testb

-- Uusi idea a ja a => b -> b

data Farvo2 a b = Farvo2 {funktio2 :: (b -> a -> b), arvo2 :: Either b (Either (b -> b) a)}
data Testi3 a b = Testi3 {getTesti3 :: Maybe (Farvo2 a b)}

--TÄMÄ TOIMII
instance Monoid (Testi3 a b) where
    mempty = Testi3 Nothing
    mappend (Testi3 Nothing) (Testi3 Nothing) = Testi3 Nothing
    mappend (Testi3 Nothing) b = b--oikea
    mappend (Testi3 (Just (Farvo2 f (Left x)))) (Testi3 Nothing) = Testi3 (Just (Farvo2 f (Left x)))
    mappend a (Testi3 Nothing) = a--oikea
    mappend (Testi3 (Just (Farvo2 f (Left b)))) (Testi3 (Just (Farvo2 _ (Right (Right a))))) = Testi3 (Just (Farvo2 f (Left (f b a))))
    mappend (Testi3 (Just (Farvo2 f (Left b)))) (Testi3 (Just (Farvo2 _ (Right (Left fb))))) = Testi3 (Just (Farvo2 f (Left (fb b))))
    mappend (Testi3 (Just (Farvo2 f (Left b)))) (Testi3 (Just (Farvo2 _ (Left b2)))) = Testi3 Nothing -- rikki, jos kaksi b:tä putkeen
    mappend (Testi3 (Just (Farvo2 f (Right (Right a))))) (Testi3 (Just (Farvo2 _ (Right (Right a2))))) = Testi3 (Just (Farvo2 f (Right (Left (\x -> f (f x a) a2)))))
    mappend (Testi3 (Just (Farvo2 f (Right (Right a))))) (Testi3 (Just (Farvo2 _ (Right (Left fb))))) = Testi3 (Just (Farvo2 f (Right (Left (\x -> fb ( f x a))))))
    mappend (Testi3 (Just (Farvo2 f (Right (Left fa))))) (Testi3 (Just (Farvo2 _ (Right (Left fb))))) = Testi3 (Just (Farvo2 f (Right (Left (fa . fb)))))
    mappend (Testi3 (Just (Farvo2 f (Right (Left fa))))) (Testi3 (Just (Farvo2 _ (Right (Right a2))))) = Testi3 (Just (Farvo2 f (Right (Left (\x -> fa (f x a2))))))
    mappend (Testi3 (Just (Farvo2 f (Right x)))) (Testi3 (Just (Farvo2 _ (Left y)))) = Testi3 Nothing

--TÄMÄ TOIMII
omaFoldl2 :: (b -> a -> b) -> b -> [a] -> b
omaFoldl2 f b l = case (foldMap (\x -> x) bapu) of
                    Testi3 (Just (Farvo2 _ (Left a))) -> a
                    _ -> b
                where
                    b1 = Testi3 (Just (Farvo2 f (Left b)))
                    l1 = map (\y -> Testi3 (Just (Farvo2 f (Right (Right y))))) l
                    bapu = [b1] ++ l1



