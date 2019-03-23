module Chains
  (steiner)
  where 
import InvertedCircle
import Linear

type Circle = (V2 Double, Double)

type Sphere = ((Double, Double, Double), Double)

circle2sphere :: Circle -> Sphere
circle2sphere (V2 x y, r) = ((x,y,0), r)

oneCircle :: Int -> Double -> V2 Double -> Double -> Double -> Circle
oneCircle m phi center radius beta = (V2 (x - o1x) y, r)
  where
    sine = sin (pi / fromIntegral m)
    side = radius * sine
    coef = 1 / (1 + sine)
    cside = coef * side
    cradius = coef * radius
    invphi = 1/phi
    pole = (V2 (radius*invphi) 0) ^+^ center
    k = radius*radius*(1-invphi*invphi)
    o1x = 2*invphi*radius
    pti = cradius *^ V2 (cos beta) (sin beta) ^+^ center
    (V2 x y, r) = invertedCircle pole k pti cside

chain :: Int -> Double -> Double -> Bool -> Circle -> [Circle]
chain m phi shift clockwise (center, radius) =
  map (oneCircle m phi center radius) angles
  where
    shift' = if clockwise then shift else -shift
    angles = [2 * pi * f i | i <- [0 .. m - 1]]
      where
        f p = shift' + fromIntegral p / fromIntegral m

chains :: [Int] -> Double -> Double -> Bool -> [Circle]
chains n phi shift clockwise
  | null n = [(V2 0 0, 1)]
  | otherwise =
    let nclockwise = not clockwise
    in concatMap
         (chain (head n) phi shift nclockwise)
         (chains (drop 1 n) phi shift nclockwise)

steiner :: [Int] -> Double -> Double -> Bool -> [Sphere]
steiner n phi shift clockwise = 
    map circle2sphere (chains n phi shift clockwise)
