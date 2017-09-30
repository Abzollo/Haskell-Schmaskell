
-- This generator is based on the sum of the triplet, where sum < maxL
main = print sol
maxL = 10^3

sol = filter (not . null . snd) . zip [2,4..maxL] . map findTriplets $ [2,4..maxL]
findTriplets s = let s2 = div s 2 in concat $ map (findTripletsAtM s2) (findMs s2)

findMs :: Int -> [Int]
findMs s2 = filter (\m -> mod s2 m == 0) [2 .. (sqrt' s2)-1]
findTripletsAtM :: Int -> Int -> [(Int, Int, Int)]
findTripletsAtM s2 m = let s_reduced = remove2s (s2 `div` m)
                           k0 = m + 1 + mod m 2  -- k0 is the smallest odd integer > m
                       in [getTriplet s2 m k | k <- [k0, k0+2 .. min (2*m-1) s_reduced],
                                               mod s_reduced k == 0, gcd k m == 1]
getTriplet :: Int -> Int -> Int -> (Int, Int, Int)
getTriplet s2 m k = (d*(m*m - n*n), 2*d*m*n, d*(m*m + n*n))
              where d = s2 `div` (k*m)
                    n = k-m
remove2s x
    | rem == 0 = remove2s x2
    | otherwise = x
    where (x2, rem) = x `divMod` 2

sqrt' = ceiling . sqrt . fromIntegral

-- Another faster generator using linear transformation, based on any condition
--main = print sol
sol' = sum . map almostT $ ppt (10^9)
    where almostT (a,_,c) = if c-2*a ` elem` [-1,1] then 2*c+2*a else 0
ppt n = concat . takeWhile (not . null) . iterate (concat . map (transformPPT n)) $ [(3,4,5)]
transformPPT n (a,b,c) = filter (\(a,b,c) -> a+b+c <= n)
                           [fixOrder (a-2*b+2*c,  2*a-b+2*c,  2*a-2*b+3*c),
                            --fixOrder (a+2*b+2*c,  2*a+b+2*c,  2*a+2*b+3*c),
                            fixOrder (-a+2*b+2*c, -2*a+b+2*c, -2*a+2*b+3*c)]
    where fixOrder (a,b,c) = if a <= b then (a,b,c) else (b,a,c)