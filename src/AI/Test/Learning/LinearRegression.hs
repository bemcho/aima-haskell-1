module AI.Test.Learning.LinearRegression (runAllTests) where

import Numeric.LinearAlgebra
import Test.QuickCheck

import AI.Learning.LinearRegression
import AI.Util.Matrix
import AI.Test.Util

-- |Regressing against a column of zeros should return a zero result vector.
testRegressionAgainstZeros :: Gen Bool
testRegressionAgainstZeros = do
    m <- choose (1,10)
    n <- choose (m,100)
    x <- arbitraryGaussianMatrix (n,m) :: Gen (Matrix Double)
    let y       = constant 0 n
        b       = constant 0 m
        bSample = regress x y
    return (bSample == b)


allTests =
    [ testRegressionAgainstZeros ]

runAllTests = mapM_ quickCheck allTests
