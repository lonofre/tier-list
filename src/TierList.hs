module TierList where


data Tier = Tier { name :: !String
                 , color :: !String
                 , priority :: !Int
                 , images :: ![String]
            }

