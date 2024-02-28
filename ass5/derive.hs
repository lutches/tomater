type DerivationStep = (String, String, String, String)

derive :: Int -> Int -> [DerivationStep]
derive i j = derive' i j []

derive' :: Int -> Int -> [DerivationStep] -> [DerivationStep]
derive' 0 0 [] = ("", "S", "", "")    : []
derive' 0 j s  = ("", "S", "Sb", "")   : wrap "" (derive' 0 (j-1) s) "b"
derive' i j s  = ("", "S", "aSb", "") : wrap "a" (derive' (i-1) j s) "b"

wrap :: String -> [DerivationStep] -> String -> [DerivationStep]
wrap a' [] b'             = []
wrap a' ((a,s,w,b):ss) b' = (a' ++ a, s, w, b ++ b') : wrap a' ss b'