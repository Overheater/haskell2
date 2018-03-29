module Lists where

-- prints the first n numbers starting with 1
  countingNumbers :: Int -> [Int]
  countingNumbers n = [ x | x <- [1..n], (mod x 1) == 0] 
--prints the first n even numbers
  evenNumbers :: Int -> [Int]
  evenNumbers n = 
        let
          a = doublen n
        in
            [ x | x <- [2..a], (mod x 2) == 0]

--prints the first n prime numbers            
  primeNumbers:: Int -> [Integer]
  primeNumbers n = take n [i | i <- [2..],isPrime i]
--merges two sorted lists together to make one
  merge:: [Int]->[Int]->[Int]
  merge xs [] = xs
  merge [] ys = ys
  merge (x:xs) ys
    | x <= head ys = x : (merge xs ys)
    | otherwise = head ys : (merge (x:xs) (tail ys))
-- "wraps" the first n values of a list to the back
  wrap:: Int -> [Int] -> [Int]
  wrap n xs =
        let
          ys = take n xs
          zs = drop n xs
        in
          zs++ys
--checks if the number given is a prime number, helper function made for the primeNumbers function
  isPrime:: Integer -> Bool
  isPrime n = 
      let 
          --sqrt wont allow an int, so you have to convert it beforehand
          a = fromIntegral n
      in
      ([]==[y | y<-[2..floor(sqrt a)], mod n y ==0])
--couldn't find an elegant solution for doubling in the even numbers function, so I made this
--helper function
  doublen :: Int -> Int
  doublen x=x*2
  
