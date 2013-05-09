import TestLib.TestModule as TestLib
import TestFFI.TestWrapper as Wrapped

-- | Entry point for the test application
main = do
    putStr $ show $ TestLib.testFunction 2
    Wrapped.hello_world
    putStr "The End."
