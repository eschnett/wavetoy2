module CellSpec (spec) where

import Test.Tasty.Hspec
import Test.Tasty.QuickCheck hiding (scale)

import Cell



spec :: Spec
spec = parallel $ do
         describe "initCell" specInitCell
         describe "errorCell" specErrorCell
         describe "energyCell" specEnergyCell
         describe "rhsCell"specRhsCell



specInitCell :: Spec
specInitCell = do
  it "is not all zero" $
     property $ \t x ->
         let c = initCell (t, x) :: Cell Double
         in u c /= 0 || rho c /= 0 || vx c /= 0

specErrorCell :: Spec
specErrorCell = do
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

specEnergyCell :: Spec
specEnergyCell = do
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

specRhsCell :: Spec
specRhsCell = do
  it "is not all zero for a non-zero state" $
     -- We require t/=0 so that not all (c == 0)
     property $ \t x t' x' dir -> t /= 0 ==>
         let bx = if dir then 1 else -1
             c = initCell (t, x) :: Cell Double
             cx = initCell (t', x') :: Cell Double
             r = rhsCell bx c cx
         in u r /= 0 || rho r /= 0 || vx r /= 0
