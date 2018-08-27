import Data.Char (digitToInt)

main = print "hello"

fromBinary :: Integer -> Integer
fromBinary = toInteger . sum . zipWith (*) (map (2^) [0..]) . map digitToInt . reverse . show

toBinary 0 = "0"
toBinary n = concat . map show . reverse $ toBinary' n
             where toBinary' 0 = []
                   toBinary' n = let (q,r) = divMod n 2 in r : toBinary' q