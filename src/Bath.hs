module Bath
    ( Bath
    , newBath
    , getTime
    ) where


data Bath e = Bath e

instance Monad Bath where
  return = Bath
  (>>=) (Bath e) f = f e

instance Functor Bath where
  fmap f (Bath e) = Bath (f e)

instance Applicative Bath where
  pure = Bath
  Bath f <*> Bath x = Bath (f x)

data Entropy

-- data RndCtx = RndCtx

newBath :: IO (Bath Entropy)
newBath = do
  entropy <- createEntropy
  return (Bath entropy)

createEntropy :: IO Entropy
createEntropy = undefined

newtype Time = Time Double
  deriving (Show)

getTime :: Bath Entropy -> Time
getTime (Bath _) = undefined
