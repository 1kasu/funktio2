import Data.Monoid

data ExerciseLevel = Nope | HonestAttempt | Correct | Awesome deriving (Show, Eq)
data Palautus = Palautus {myohassa :: Bool, tehtavat :: [ExerciseLevel]}
data Grade = Hylatty | Yksi | Kaksi | Kolme | Nelja | Viisi | Kakku deriving (Eq, Ord, Show)
data Arvosanat = Arvosanat {getNope :: Int, getHonestAttempt :: Int, getCorrect :: Int, getAwesome :: Int} deriving (Show)

instance Monoid Arvosanat where
    mempty = Arvosanat 0 0 0 0
    mappend (Arvosanat a1 a2 a3 a4) (Arvosanat b1 b2 b3 b4) = Arvosanat (a1 + b1) (a2 + b2) (a3 + b3) (a4 + b4)

newtype Max = Max {getMax :: Grade}
newtype Min = Min {getMin :: Grade}

instance Monoid Max where
    mempty = Max Hylatty
    mappend (Max a) (Max b) = Max (max a b)

instance Monoid Grade where
    mempty = Kakku
    mappend a b = min a b
    
onkoKakku :: Arvosanat -> Max
onkoKakku (Arvosanat _ _ c a) = if a < 3
                then mempty
                else if (a - 3)*2 + c >= 3
                    then Max Kakku
                    else mempty
    
onkoViisi :: Arvosanat -> Max
onkoViisi (Arvosanat _ _ c a) = if a * 2 + c >= 6
                    then Max Viisi
                    else mempty
                    
onkoNelja :: Arvosanat -> Max
onkoNelja (Arvosanat _ h c a) = if ca >= 3 && h + ca - 3 >= 3
                    then Max Nelja
                    else mempty
                where
                    ca = a * 2 + c
                    
onkoKolme :: Arvosanat -> Max
onkoKolme (Arvosanat _ h c a) = if ca >= 3
                    then Max Kolme
                    else mempty
                where
                    ca = a * 2 + c
                    
                    
onkoKaksi :: Arvosanat -> Max
onkoKaksi (Arvosanat _ h c a) = if ca >= 2 && hca - 2 >= 1
                    then Max Kaksi
                    else mempty
                where
                    ca = a * 2 + c
                    hca = a * 2 + c + h
                    
onkoYksi :: Arvosanat -> Max
onkoYksi (Arvosanat _ h c a) = if ca >= 1 && hca - 1 >= 3
                    then Max Yksi
                    else mempty
                where
                    ca = a * 2 + c
                    hca = a * 2 + c + h
    
arvostele :: Arvosanat -> Grade
arvostele a = case onkoYksi a <> onkoKaksi a <> onkoKolme a <> onkoNelja a <> onkoViisi a <> onkoKakku a of
        Max t -> t

muutaArvosanaksi :: ExerciseLevel -> Arvosanat
muutaArvosanaksi Nope = Arvosanat 1 0 0 0
muutaArvosanaksi HonestAttempt = Arvosanat 0 1 0 0
muutaArvosanaksi Correct = Arvosanat 0 0 1 0
muutaArvosanaksi Awesome = Arvosanat 0 0 0 1
    
keraa :: [ExerciseLevel] -> Arvosanat
keraa e = foldMap muutaArvosanaksi e

laskeYhdella :: Arvosanat -> Arvosanat
laskeYhdella (Arvosanat n h c a) = if a > 0 then Arvosanat n h (c+1) (a-1)
                                            else if c > 0 then Arvosanat n (h+1) (c-1) a
                                                else if h > 0 then Arvosanat (n+1) (h-1) c a
                                                    else Arvosanat n h c a

laskeGrade :: Palautus -> Grade
laskeGrade (Palautus m e) = if m then arvostele (laskeYhdella (keraa e)) else arvostele (keraa e)


laskeKurssinGrade :: [Palautus] -> Grade
laskeKurssinGrade [] = Hylatty --Tosi laiskat saisivat muuten kakun
laskeKurssinGrade e = foldMap (\x -> laskeGrade x) e




