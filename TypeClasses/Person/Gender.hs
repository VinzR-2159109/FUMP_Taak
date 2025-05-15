-- https://en.wikipedia.org/wiki/Gender_identity
-- https://www23.statcan.gc.ca/imdb/p3VD.pl?CLV=0&CVD=1326716&D=1&Function=getVD&MLV=2&TVD=1326715

module Person.Gender
    ( Gender(..)
    , GenderCluster(..)
    , clusterOf
    ) where

import Metric   (Metric(..), similarity)

data Gender
    = Male
    | Female
    | TransMale
    | TransFemale
    | Agender
    | Genderfluid
    | Genderqueer
    | TwoSpirit
    | Intersex
    | OtherGender
    deriving (Eq, Show)


data GenderCluster
    = Cis        -- Male, Female
    | Trans      -- TransMale, TransFemale
    | NonBinaryC -- Agender, Genderfluid, Genderqueer, TwoSpirit, Intersex
    | OtherC     -- OtherGender
    deriving (Eq, Show)

clusterOf :: Gender -> GenderCluster
clusterOf g = case g of
    Male         -> Cis
    Female       -> Cis
    TransMale    -> Trans
    TransFemale  -> Trans
    Agender      -> NonBinaryC
    Genderfluid  -> NonBinaryC
    Genderqueer  -> NonBinaryC
    TwoSpirit    -> NonBinaryC
    Intersex     -> NonBinaryC
    OtherGender  -> OtherC

instance Metric Gender where
    similarity x y
        | x == y                      = 1.0
        | clx == cly                  = 0.8
        | clx /= Cis && cly /= Cis    = 0.6
        | otherwise                   = 0.4
        where
            clx = clusterOf x
            cly = clusterOf y
