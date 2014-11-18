-- cat code.txt | runhaskell OneTimePad.hs
{-
  [(0,"I am planning a secret mission."),(1,"He is the only person to trust."),(2,"The current plan is top secret."),(3,"When should we meet to do this?"),(4,"I think they should follow him."),(5,"This is purer than that one is."),(6,"Not one cadet is better than I.")]
-}
import qualified Data.Map as M
import qualified Data.Bits as B
import qualified Data.Char as C
import qualified Data.List as L
import qualified Numeric as N

main = interact oneTimePad  
  
oneTimePad :: String -> String
oneTimePad input =   
    let codes = splitBy (=='\n') input
        codesNumber = map toBin codes
        s = length codes
        s1 = (length (codes!!0)) `div` 2
        cs = [(x, y, (zipWith (xor) (codesNumber!!x) (codesNumber!!y)))| x <- [0..s-1], y<-[x..s-1], x /= y]

        ds = map (findSpace cs) [0..s1-1]
        --ds1 = map (\x -> map (\y -> if x == 0 && y==0 then "01001001" else ds!!x!!y) [0..s]) [0..s1-1]
        es = map (\y -> map (\x -> getLetter (x!!y)) ds) [0..6] 
        m1 = "I am planning a secret mission."
        m2 = map C.ord m1
        m3 = map (\x -> N.showHex x "") m2
        m4 = map (\x -> map C.toUpper x) m3
        m5 = foldr (\acc x -> acc ++ x) [] m4
        m6 = toBin(m5)
        cs1 = filter (\(x, y, z)-> x == 0) cs
        cs2 = map (\(x, y, z)-> (x, y, zipWith xor z m6)) cs1
        cs3 = (0, m1):map (\(x, y, z)-> (y, convertBinToString z)) cs2
        --cs1 = map (\(x, y, z)-> convertBinToString(xor z m6)) ()
        {-es1 = map (\x -> getLetter (x!!1)) ds
        es2 = map (\x -> getLetter (x!!2)) ds
        es3 = map (\x -> getLetter (x!!3)) ds
        es4 = map (\x -> getLetter (x!!4)) ds
        es5 = map (\x -> getLetter (x!!5)) ds
        es6 = map (\x -> getLetter (x!!6)) ds
        -}
        --es = '\n':es0 
        --es = map (findKey ) ds
        --es = take s (repeat (take (s1 * 8) (repeat '0')))
        {-
        n = 1
        ds = map (\(x,y,z) -> (x, y, take 8 (drop (8*n) z))) cs
        es = filter (\(x,y,z) -> take 2 z == "01") ds
        fs = getCount $ foldl (\acc (x,y,z) -> acc ++ [x, y]) [] es
        gs = filter (\(x, y) -> y >= 3) fs -}
        --test = ((codesNumber !! 0) `B.xor` (codesNumber !! 1))
    in  show cs3

convertBinToString :: String -> String
convertBinToString [] = []
convertBinToString (x1:x2:x3:x4:x5:x6:x7:x8:xs) = getLetter([x1, x2, x3, x4, x5, x6, x7, x8]):convertBinToString(xs)

getLetter :: String -> Char
getLetter "xxxxxxxx" = '#'
getLetter xs = C.chr(bin2dec(xs))

bin2dec :: String -> Int
bin2dec = foldr (\c s -> s * 2 + c) 0 . reverse . map c2i
    where c2i c = if c == '0' then 0 else 1

findSpace :: [(Int, Int, String)] -> Int -> [String] --[(Int, Int)]
findSpace cs n = let 
        ds = map (\(x,y,z) -> (x, y, take 8 (drop (8*n) z))) cs
        es = filter (\(x,y,z) -> take 2 z == "01") ds
        fs = getCount $ foldl (\acc (x,y,z) -> acc ++ [x, y]) [] es
        gs = filter (\(x, y) -> y >= 3) fs
        hs = if length gs == 0 then (take 7 (repeat "xxxxxxxx")) else 
            let codeindex = (fst $ head gs)
                is = filter (\(x,y,z) -> x == codeindex || y == codeindex) ds
                js = map (\(x,y,z) -> (x, y, (zipWith xor z "00100000"))) is
                ks = map (\(x,y,z) -> if x == codeindex then (y, z) else (x, z)) js
                ls = (codeindex, "00100000") : ks
                ms = L.sortBy  (\x y -> compare (fst x) (fst y)) ls
                ns = map (\(x, y) -> y) ms
            in ns
        in hs

splitBy :: (Char -> Bool) -> [Char] -> [[Char]]
splitBy _ [] = []
splitBy f list = first : splitBy f (dropWhile f rest) where
  (first, rest) = break f list

getCount :: [Int] -> [(Int, Int)]
getCount xxs = M.toList $ M.fromListWith (+) [(c, 1) | c <- xxs]

xor :: Char -> Char -> Char
xor x y 
   | x == y = '0'
   | otherwise = '1'

toBin :: String -> String
toBin [] = []
toBin (x:xs) = toBinLetter(x) ++ toBin(xs)

toBinLetter :: Char -> String
toBinLetter ch
    | ch == '0' = "0000"
    | ch == '1' = "0001"
    | ch == '2' = "0010"
    | ch == '3' = "0011"
    | ch == '4' = "0100"
    | ch == '5' = "0101"
    | ch == '6' = "0110"
    | ch == '7' = "0111"
    | ch == '8' = "1000"
    | ch == '9' = "1001"
    | ch == 'A' = "1010"
    | ch == 'B' = "1011"
    | ch == 'C' = "1100"
    | ch == 'D' = "1101"
    | ch == 'E' = "1110"
    | ch == 'F' = "1111"
    | otherwise     = "0000"

