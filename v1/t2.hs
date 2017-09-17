import Data.Time
import Data.Monoid


-- Projektin nimi käytännössä. Voisi olla myös koodi tai muu vastaava.
newtype Project = Project {getProject :: String} deriving (Eq, Show)
-- Vastaava kuin projekti, mutta subprojekti
newtype Subproject = Subproject {getSubproject :: String} deriving (Eq, Show)
-- Aloitusaika sisältäen päivämäärän ja kellon ajan.
newtype Starttime = Starttime {getStarttime :: UTCTime} deriving (Eq, Show)
-- Lopetusaika sisältäen päivämäärän ja kellon ajan
newtype Endtime = Endtime {getEndtime :: UTCTime} deriving (Eq, Show)
-- Kommentti tekstinä.
newtype Comment = Comment {getComment :: String} deriving (Eq, Show)
-- Kulunut aika, esim. 4 tuntia
newtype KulunutAika = KulunutAika {getKulunutAika :: NominalDiffTime} deriving (Eq, Show)

instance Monoid KulunutAika where
    mempty = KulunutAika ( fromInteger 0)
    mappend (KulunutAika a) (KulunutAika b) = KulunutAika (a + b)

-- Laskee kuluneen ajan alkuajasta loppuaikaan.
laskeKulunutAika :: Starttime -> Endtime -> KulunutAika
laskeKulunutAika (Starttime s) (Endtime e) = KulunutAika (diffUTCTime e s)

-- Laskee kuluneen kuluneen ajan rivin tiedoista
laskeKulunutAikaRivista :: Tietorivi -> KulunutAika
laskeKulunutAikaRivista (Rivi _ _ s e _) = laskeKulunutAika s e

-- Säilyttää aikaisimman esiintymän.
newtype Aikaisin = Aikaisin {getAikaisin :: Maybe Starttime} deriving (Eq, Show)

instance Monoid Aikaisin where
    mempty = Aikaisin Nothing
    mappend (Aikaisin Nothing) b = b
    mappend a (Aikaisin Nothing) = a
    mappend (Aikaisin (Just (Starttime a))) (Aikaisin (Just (Starttime b))) = Aikaisin ( Just(Starttime(min a b)))

    -- Säilyttää myöhäisimmän esiintymän.
newtype Myohaisin = Myohaisin {getMyohaisin :: Maybe Endtime} deriving (Eq, Show)

instance Monoid Myohaisin where
    mempty = Myohaisin Nothing
    mappend (Myohaisin Nothing) b = b
    mappend a (Myohaisin Nothing) = a
    mappend (Myohaisin (Just (Endtime a))) (Myohaisin (Just (Endtime b))) = Myohaisin ( Just(Endtime(max a b)))


-- Sisältää rivin yhdestä tuplesta
data Tietorivi = Rivi {
            project :: Project,
            subproject :: Maybe Subproject,
            starttime :: Starttime,
            endtime :: Endtime,
            comment :: Comment
} deriving (Eq, Show)

-- Antaa aloitusajan
annaAloitusaika :: Tietorivi -> Starttime
annaAloitusaika (Rivi _ _ s _ _) = s

-- Antaa lopetusajan
annaLopetusaika :: Tietorivi -> Endtime
annaLopetusaika (Rivi _ _ _ s _) = s

annaKommentti :: Tietorivi -> Comment
annaKommentti (Rivi _ _ _ _ c) = c

-- Antaa aikaisimman projektin aloitusajan
annaAikaisinAloitusAika :: [Tietorivi] -> Aikaisin
annaAikaisinAloitusAika t = foldMap (\x -> Aikaisin (Just (annaAloitusaika x))) t --menipä nätisti. Hieman karmea tyyppi ketju, mutta muuten kyllä.

-- Antaa myohäisimmän projektin lopetusajan
annaMyohaisinLopetusAika :: [Tietorivi] -> Myohaisin
annaMyohaisinLopetusAika t = foldMap (\x -> Myohaisin (Just (annaLopetusaika x))) t

-- Antaa projektin
annaProjekti :: Tietorivi -> Project
annaProjekti (Rivi p _ _ _ _) = p

-- Filtteröi annetun projektin rivit
annaProjektilla :: Project -> [Tietorivi] -> [Tietorivi]
annaProjektilla p t = filter (\x -> (annaProjekti x) == p) t

-- Antaa projektin alun ja lopun. Voisi varmaan tehostaa yhdistämällä foldit, mutta eiköhän tämäkin jo menettele.
annaProjektinAloitusJaLopetus :: Project -> [Tietorivi] -> (Aikaisin, Myohaisin)
annaProjektinAloitusJaLopetus p t = (annaAikaisinAloitusAika tp, annaMyohaisinLopetusAika tp)
        where
            tp = annaProjektilla p t

-- Antaa kaytetyn ajan yhteensä
annaKaytettyAikaYhteensa :: [Tietorivi] -> KulunutAika
annaKaytettyAikaYhteensa t = foldMap (\x -> laskeKulunutAikaRivista x) t

-- Antaa annettuun projektiin kaytetyn ajan yhteensä
annaProjektinKaytettyAikaYhteensa :: Project -> [Tietorivi] -> KulunutAika
annaProjektinKaytettyAikaYhteensa p t = annaKaytettyAikaYhteensa tp
        where
            tp = annaProjektilla p t
            
-- Antaa rivien määrän
annaEsiintymat :: [Tietorivi] -> Sum (Integer)
annaEsiintymat t = foldMap (\_ -> Sum(1)) t

-- Antaa annetun projektin esiintymien määrän
annaProjektinEsiintymat :: Project -> [Tietorivi] -> Sum(Integer)
annaProjektinEsiintymat p t = annaEsiintymat tp
        where
            tp = annaProjektilla p t

-- Antaa keskiarvon kuluneista ajoista
annaKeskiarvoKulunutAika :: [Tietorivi] -> Maybe KulunutAika
annaKeskiarvoKulunutAika t = case n == 0 of
        True -> Nothing
        False -> Just (KulunutAika  ( y / n) ) 
        where
            KulunutAika y = annaKaytettyAikaYhteensa t
            Sum sumN = annaEsiintymat t
            n = fromInteger (sumN)
            
-- Antaa projektiin kuluneiden aikojen keskiarvot
annaProjektinKeskiarvoKulunutAika :: Project -> [Tietorivi] -> Maybe KulunutAika
annaProjektinKeskiarvoKulunutAika p t = annaKeskiarvoKulunutAika tp
        where
            tp = annaProjektilla p t
            
-- Antaa kommentit            
annaKommentit :: [Tietorivi] -> [Comment]
annaKommentit t = foldMap (\x -> [annaKommentti x]) t

-- Antaa projektin kommentit
annaProjektinKommentit :: Project -> [Tietorivi] -> [Comment]
annaProjektinKommentit p t = annaKommentit tp
        where
            tp = annaProjektilla p t
 
-- Sisältää kaiken parsitun tiedon ja lisäksi parsiessa tallensi kaikki projektit erikseen, jota hakeminen olisi helpompaa
data Tiedot = Tiedot {
                projektit :: [Project],
                tiedot :: [Tietorivi]
} deriving (Eq, Show)
      
-- Antaa kaikkien projektin kuluneet ajat
kaikkiAjatYhteensa :: Tiedot -> [(Project, KulunutAika)]
kaikkiAjatYhteensa t = keraaKaikilta t annaProjektinKaytettyAikaYhteensa
      
-- Antaa kaikkien projektien aikavälit      
kaikkiAikaValit :: Tiedot -> [(Project, (Aikaisin, Myohaisin))]
kaikkiAikaValit t = keraaKaikilta t annaProjektinAloitusJaLopetus

-- Antaa kaikkien projektien keskiarvoajat
kaikkiKeskiarvot :: Tiedot -> [(Project, Maybe KulunutAika)]
kaikkiKeskiarvot t = keraaKaikilta t annaProjektinKeskiarvoKulunutAika

-- Antaa kaikkien projektien kommentit
kaikkiKommentit :: Tiedot -> [(Project, [Comment])]
kaikkiKommentit t = keraaKaikilta t annaProjektinKommentit
        
-- Keraa kaikilta projekteilta jotakin
keraaKaikilta :: Tiedot -> (Project -> [Tietorivi] -> b) -> [(Project, b)]
keraaKaikilta (Tiedot p t) f = foldMap (\x -> [(x, f x t)]) p
      
testiTiedot = Tiedot{
        projektit = [Project ("Projekti1"), Project ("Projekti44")],
        tiedot    = testitapaus}
            
testitapaus = ([
    Rivi {
        project    = Project ("Projekti1"),
        subproject = Nothing,
        starttime  = Starttime ((read "2011-11-19 18:28:52.607875 UTC")::UTCTime),
        endtime    = Endtime ((read "2011-11-19 20:28:52.607875 UTC")::UTCTime),
        comment    = Comment "Turha projekti"
    },
    Rivi {
        project    = Project ("Projekti1"),
        subproject = Nothing,
        starttime  = Starttime ((read "2011-11-19 18:28:52.607875 UTC")::UTCTime),
        endtime    = Endtime ((read "2011-11-19 20:28:52.607875 UTC")::UTCTime),
        comment    = Comment "Turha projekti"
    },
    Rivi {
        project    = Project ("Projekti44"),
        subproject = Nothing,
        starttime  = Starttime ((read "2012-11-19 18:28:52.607875 UTC")::UTCTime),
        endtime    = Endtime ((read "2012-11-19 20:28:52.607875 UTC")::UTCTime),
        comment    = Comment "Turha projekti"
    },
    Rivi {
        project    = Project ("Projekti44"),
        subproject = Nothing,
        starttime  = Starttime ((read "2009-11-19 18:28:52.607875 UTC")::UTCTime),
        endtime    = Endtime ((read "2009-11-19 20:28:52.607875 UTC")::UTCTime),
        comment    = Comment "Turha projekti"
    },
    Rivi {
        project    = Project ("Projekti1"),
        subproject = Nothing,
        starttime  = Starttime ((read "2011-11-19 18:28:52.607875 UTC")::UTCTime),
        endtime    = Endtime ((read "2011-11-19 20:29:52.607875 UTC")::UTCTime),
        comment    = Comment "Turha projekti"
    }])