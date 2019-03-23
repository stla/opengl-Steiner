module InvertedCircle
  where 
import Linear

invertedCircle :: V2 Double -> Double -> V2 Double -> Double 
               -> (V2 Double, Double)
invertedCircle pole k center radius = (r *^ z2 ^+^ pole, r * r2)
  where
  r = sqrt $ abs k
  z1 = (signum k)/r *^ (center ^-^ pole)
  d1 = radius*radius/r/r - quadrance z1
  z2 = (-1/d1) *^ z1
  r2 = sqrt $ quadrance z2 + 1/d1

