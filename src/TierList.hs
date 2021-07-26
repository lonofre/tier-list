module TierList where
import qualified Graphics.Image as I
import Graphics.Image.Interface as Interface


data Tier = Tier { name :: !String
                 , priority :: !Int
                 , images :: ![I.Image I.VU I.RGB Double]
            }

