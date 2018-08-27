import Data.List (sort)

main = print sol
sol = sort $ allPalindromes 7

-- All palindromes with k digits or less
allPalindromes = map (\s -> read s :: Int) . allPalindromes'
allPalindromes' k
    | odd k = oddPalindromes k ++ evenPalindromes (k-1)
    | otherwise = evenPalindromes k ++ oddPalindromes (k-1)

-- Odd length and even length palindromes
oddPalindromes k = [s ++ show d ++ reverse s | d <- [1..9], n <- [1..10^(div k 2)-1], let s = show n]
evenPalindromes k = [s ++ reverse s | n <- [1..10^(div k 2)-1], let s = show n]