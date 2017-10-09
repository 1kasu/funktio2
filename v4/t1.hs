{-# LANGUAGE DeriveFunctor #-}


--Tätä tehtävää oli kiva tehdä ja auttoi huomattavasti oppimisessa sen jälkeen, kun keksin ladata Free:n
module SimpleEffectLanguage where
import Control.Monad.Free


data Terminal next =
  SlurpLine (String -> next) |
  BarfLine String next |
  Done
  deriving Functor

slurpLine :: Free Terminal String
slurpLine = liftF (SlurpLine id)

barfLine :: String -> Free Terminal ()
barfLine s = liftF (BarfLine s ())

terminate :: Free Terminal ()
terminate = liftF Done

echo = do
  l <- getLine
  putStrLn "----"
  putStrLn l
  echo
  
echo2 = do
  l <- slurpLine
  barfLine "----"
  barfLine l
  echo2
  
echo3 = do
  l <- slurpLine
  barfLine "----"
  barfLine l
  terminate
  
type Program = Free Terminal
  
interpretIO :: Program r -> IO ()
interpretIO (Free Done) = pure ()
interpretIO (Free (SlurpLine fnext)) = do
    x <- getLine
    interpretIO (fnext x)
interpretIO (Free (BarfLine s n)) = do
    putStrLn s
    interpretIO n
    
mockIO :: Free Terminal r -> [String] -> [String]
mockIO (Free Done) t = t
mockIO (Free (SlurpLine fnext)) t = do
    mockIO (fnext "Kala") (t ++ ["Luetaan: 'Kala'"])
mockIO (Free (BarfLine s n)) t = do
    mockIO n (t ++ (pure ("Tulostetaan: '" ++ s ++ "'")))