-- cat ctext1.txt | runhaskell DeVigenere.hs
import qualified Data.Map as M
import qualified Data.Bits as B
import qualified Data.Char as C

main = interact deVigenere  
  
deVigenere :: String -> String  
deVigenere input =   
    let result = convertStringToHex input
        keySize = getMax caculateFreq result [1..13]
        key = map (decodeText result keySize) [1..keySize]
        message = map (\x -> C.chr (fromIntegral x)) (map (\x -> fst(x) `B.xor` snd(x)) (zip result (cycle key)))
    in  message

decodeText :: [Int] -> Int -> Int -> Int
decodeText xs keySize l = 
   let text = every keySize ((take (keySize - l) (repeat 0)) ++ xs) 
   in getMax decode text [0..255]

decode :: [Int] -> Int -> Double
decode xs k = 
       let xxs = map (\x -> x `B.xor` k) xs
           count = getCount xxs
           freq = map (\x -> (fst(x), snd(x)/size)) count
           size = fromIntegral(length xs) * 1.0
           letterFreq = (take 97 (repeat 0)) ++ map (\x -> x * 0.01) [8.167,1.492,2.782,4.253,12.702,2.228,2.015,6.094,6.966,0.153,0.772,4.025,2.406,6.749,7.507,1.929,0.095,5.987,6.327,9.056,2.758,0.978,2.360,0.150,1.974,0.074] ++ (take 5 (repeat 0))
           result = sum $ map (\x -> snd(x) * letterFreq !! fromIntegral(fst(x))) freq          
       in if (all  (\x -> x > 31 && x < 128) xxs) then result else 0.0 

getMax :: ([Int] -> Int -> Double) -> [Int] -> [Int] -> Int
getMax f ds ks = snd . maximum $ zip (map  (f ds)  ks) ks

caculateFreq :: [Int] -> Int -> Double
caculateFreq xs n = 
      let xxs = every n xs
          l = fromIntegral(length xxs) * 1.0
          cs = getCount xxs
          fs = map (\x -> snd(x)/l ) cs
      in sum $ map  (^ 2) fs

getCount :: [Int] -> [(Int, Double)]
getCount xxs = M.toList $ M.fromListWith (+) [(c, 1.0) | c <- xxs]

every :: Int -> [Int] -> [Int]
every n xs = case drop (n-1) xs of
      (y:ys) -> y : every n ys
      [] -> []

hexChar :: Char -> Int
hexChar ch
    | ch == '0' = 0
    | ch == '1' = 1
    | ch == '2' = 2
    | ch == '3' = 3
    | ch == '4' = 4
    | ch == '5' = 5
    | ch == '6' = 6
    | ch == '7' = 7
    | ch == '8' = 8
    | ch == '9' = 9
    | ch == 'A' = 10
    | ch == 'B' = 11
    | ch == 'C' = 12
    | ch == 'D' = 13
    | ch == 'E' = 14
    | ch == 'F' = 15
    | otherwise     = 0

convertStringToHex :: [Char] -> [Int]  
convertStringToHex [] = []
convertStringToHex (x:[]) = []
convertStringToHex (x:y:xs) = (hexChar(x)*16 + hexChar(y)):convertStringToHex(xs)
