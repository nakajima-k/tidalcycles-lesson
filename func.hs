import Data.Time
import Data.Char
import Data.List

-- add fractul Beat
fractBeat :: String -> Int -> String
fractBeat x 0 = ""
fractBeat x n = x ++ " ~ " ++ (fractBeat x (n-1))



fromK :: Char -> [Char]
fromK c = c : fromK (chr((ord c) + 1))
unicode = take 20902 (fromK '-')

splitEvery :: Int -> [a] -> [[a]]
splitEvery n s
    | null sx = [fx]
    | otherwise = fx : (splitEvery n sx)
    where (fx,sx) = splitAt n s


dateString :: IO [Char]
dateString = do
    zonedTm <- getZonedTime
    return $ replace '-' ' ' $ take 10 $ show zonedTm


    -- replace from to xs
replace :: (Eq a) => a -> a -> [a] -> [a]
replace _ _ [ ] = [ ]
replace from to (x:xs) |from == x = to : (replace from to xs)
                       |otherwise = x : (replace from to xs)


-- let
cylinder :: Double -> Double -> Double
cylinder r h =
  let sideArea = 2 * pi * r * h
      topArea = pi * r ^ 2
  in  sideArea + 2 * topArea

max' :: (Ord a) => a -> a -> a
max' a b
 | a <= b  = b
 | otherwise = a

myCompare :: (Ord a) => a -> a -> Ordering
a `myCompare` b
 | a== b  = EQ
 | a <= b  = LT
 | otherwise = GT

-- bmi where
bmiTell :: Double -> Double -> String
bmiTell weight height
 | bmi <= skinny = "You're underweight, you emo, you!"
 | bmi <= normal = "You re supposedly normal Pffft, I bet you're ugly!"
 | bmi <= fat = "you're fat! Lose some weight,fatty"
 | otherwise = "You're a whale, congratylations!"
 where bmi = weight / height ^ 2
       skinny = 18.5
       normal = 25.0
       fat = 30.0

niceGreeting :: String
niceGreeting = "Hello! So very nice to see you."
badGreeting :: String
badGreeting = "Oh! Pffft. It's you."
greet :: String -> String
greet "Juan" = niceGreeting ++ " Juan!"
greet "Fernando" = niceGreeting ++ " Fernando!"
greet name = badGreeting ++ " " ++ name

initials :: String -> String -> String
initials firstname lastname = [f] ++ ". " ++ [l] ++ "."
 where (f:_) = firstname
       (l:_) = lastname

calcBmis :: [(Double, Double)] -> [Double]
calcBmis xs = [bmi w h | (w,h) <- xs]
 where bmi weight height = weight / height ^ 2
