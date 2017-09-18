module CellSpec (spec) where

import Test.Tasty.Hspec
import Test.Tasty.QuickCheck hiding (scale)

import Cell



spec :: Spec
spec = do
  describe "initCell" $ do
         it "is not all zero" $
            property $ \t x ->
                let c = initCell (t, x) :: Cell Double
                in u c /= 0 || rho c /= 0 || vx c /= 0
  describe "errorCell" $ do
         it "is zero when exact" $
            property $ \t x ->
                let c = initCell (t, x) :: Cell Double
                    e = errorCell (t, x) c
                in e == Cell 0 0 0
         it "is not all zero when inexact" $
            property $ \t x t' x' -> (t /= t' || x /= x') ==>
                let c = initCell (t, x) :: Cell Double
                    e = errorCell (t', x') c
                in e /= Cell 0 0 0
  describe "energyCell" $ do
         it "is zero for vacuum" $
            property $ \u0 ->
                let c = Cell u0 0 0 :: Cell Double
                    e = energyCell c
                in e == 0
         it "has the correct potential term" $
            property $ \rho0 ->
                let c = Cell 0 rho0 0 :: Cell Double
                    e = energyCell c
                in e == (1/2) * rho0 ^ 2
         it "has the correct kinetic term" $
            property $ \vx0 ->
                let c = Cell 0 0 vx0 :: Cell Double
                    e = energyCell c
                in e == (1/2) * vx0 ^ 2
  describe "rhsCell" $ do
         it "is not all zero" $
            property $ \t x t' x' dir ->
                let bx = if dir then 1 else -1
                    c = initCell (t, x) :: Cell Double
                    cx = initCell (t', x') :: Cell Double
                    r = rhsCell bx c cx
                in u r /= 0 || rho r /= 0 || vx r /= 0
