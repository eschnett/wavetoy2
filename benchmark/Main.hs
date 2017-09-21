{-# LANGUAGE FlexibleContexts #-}

-- You can benchmark your code quickly and effectively with Criterion.
-- See its website for help: <http://www.serpentine.com/criterion/>.
import Criterion.Main

import SimpleVectors
import WaveToy2



main :: IO ()
main = defaultMain
    [ bgroup "State"
      [ bench "init" $
        whnf
        (\np ->
         let skel = skeletonState (0, 1) np :: State Double (V0 Double)
             coords = coordState skel
             g = initState 0 coords
         in integralState $ energyState g)
        gridSize
      , bench "rhs" $
        whnf
        (\np ->
         let skel = skeletonState (0, 1) np :: State Double (V0 Double)
             coords = coordState skel
             g = initState 0 coords
             r = rhsState g
         in integralState $ energyState r)
        gridSize
      , bench "rk2" $
        whnf
        (\np ->
         let skel = skeletonState (0, 1) np :: State Double (V0 Double)
             coords = coordState skel
             g = initState 0 coords
             rhs x = rhsState x
             step = rk2State smallStep rhs
             g' = iterate step g !! numSteps
         in integralState $ energyState g')
        gridSize
      ]
    ]

gridSize :: Int
gridSize = 1000

numSteps :: Int
numSteps = 10

smallStep :: Double
smallStep = 1.0e-8
