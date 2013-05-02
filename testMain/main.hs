import TestLib.TestModule as TestLib

-- | Entry point for the test application
main = do
    putStr $ show $ TestLib.testFunction 2
    putStr "The End."
