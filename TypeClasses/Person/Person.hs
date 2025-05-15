{-# LANGUAGE DeriveGeneric #-}

module Person
  ( Person(..)
  ) where

import GHC.Generics           (Generic)
import Person.Metric          (Metric(..))
import Person.Age             ()
import Person.Gender          (Gender(..))
import Person.Ethnicity       (Ethnicity(..))
import Person.Education       (Education(..))
import Person.Personality     (Personality(..))

data Person = Person
  { name        :: String
  , age         :: Int
  , gender      :: Gender
  , ethnicity   :: Ethnicity
  , education   :: Education
  , personality :: Personality
  }
  deriving (Eq, Show, Generic)

instance Metric Person where
  similarity p q =
    let sAge  = similarity (age p)         (age q)
        sGen  = similarity (gender p)      (gender q)
        sEth  = similarity (ethnicity p)   (ethnicity q)
        sEdu  = similarity (education p)   (education q)
        sPers = similarity (personality p) (personality q)

        wAge  = 0.40
        wGen  = 0.15
        wEth  = 0.15
        wEdu  = 0.15
        wPers = 0.15
    in  wAge*sAge + wGen*sGen + wEth*sEth + wEdu*sEdu + wPers*sPers

  distance p q = 1 - similarity p q

-- | A quick test harness
main :: IO ()
main = do
  let mk n a g e d p = Person n a g e d p
      alice = mk "Alice" 30 Female Asian Bachelors Yellow
      bob   = mk "Bob"   36 Male   White    Masters  Red
      carol = mk "Carol" 28 Genderfluid Hispanic PhD Green

  putStrLn "=== Person Similarity / Distance Tests ==="
  mapM_ (printPair alice) [bob, carol]
  where
    printPair p q = do
      putStrLn $ "\nComparing " ++ name p ++ " vs. " ++ name q
      putStrLn $ "  sim = " ++ show (similarity p q)
      putStrLn $ "  dist= " ++ show (distance p q)
