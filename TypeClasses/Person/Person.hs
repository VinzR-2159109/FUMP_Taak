{-# LANGUAGE DeriveGeneric #-}

module Person.Person(Person(..)) where

import GHC.Generics           (Generic)
import Metric
import Person.Age             (Age(..))
import Person.Gender          (Gender(..))
import Person.Ethnicity       (Ethnicity(..))
import Person.Education       (Education(..))
import Person.Personality     (Personality(..))

data Person = Person
  { name        :: String
  , age         :: Age
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
