import Data.List

type Matrix = [[Int]]

-- Task 1
isSameSize :: Matrix -> Matrix -> Bool
isSameSize [] [] = True
isSameSize [] _ = False
isSameSize _ [] = False
isSameSize m1 m2 = sum [1 | r1 <- m1] == sum [1 | r2 <- m2] && sum [1 | r1 <- head m1] == sum [1 | r2 <- head m2]

-- Task 2
magicSquare :: Matrix -> Maybe Int
magicSquare [] = Nothing
magicSquare matrix
 | length matrix /= length (head matrix) = Nothing -- Checks if matrix is n*n
 | sum [sum [1 | y <- x, y == 1] | x <- matrix] /= 1 = Nothing -- Checks if matrix contains 1
 | sum [1 | x <- group $ concat matrix, length x == 1] /= length matrix * length matrix = Nothing -- Checks if elements are different
 | all (== returnSum) [sum r | r <- matrix] && all (== returnSum) [sum x | x <- transpose matrix] && --Checks row and col sum
 sum [x | (x, y) <- zip (concat matrix) [1,2..], (y + length (head matrix)) `mod` (length (head matrix) + 1)  == 0] == returnSum && --Checks diagonal
 sum [x | (x, y) <- zip (concat revmatrix) [1,2..], (y + length (head matrix)) `mod` (length (head matrix) + 1) == 0] == returnSum = (Just returnSum) -- Checks rev_diag
 | otherwise = Nothing
 where 
  returnSum = sum (head matrix)
  revmatrix = [reverse x | x <- matrix]

-- Task 3
solveMatrix = [[2,9,4], [7,5,3], [6,1,8]] -- x8 transform / only 1 solve for 3x3 / sum must equal to 15
minimalCost :: Matrix -> Maybe Int
minimalCost [] = Nothing
minimalCost matrix
 | length (take 4 (head matrix)) /= 3 = Nothing
 | length (take 4 matrix) /= 3 = Nothing
 -- | length (head matrix) > 3 || length (head matrix) < 3 = Nothing
 | length matrix /= 3 = Nothing
 | magicSquare matrix /= Nothing = (Just 0)
 | otherwise = (Just (minimum [sum [abs (x - y) | (x, y) <- zip (concat mtrx) (concat matrix)] | mtrx <- ls_x8]))
 where
 sm = solveMatrix
 tsm = transpose solveMatrix
 rt :: Matrix -> Int -> Matrix -- Recursive function to transform the matrix
 rt m 0 = m
 rt m n = rt (map reverse (transpose m)) (n-1)
 ls_x8 = [rt sm 3, rt sm 2, rt sm 1, solveMatrix, rt tsm 3, rt tsm 2, rt tsm 1, transpose solveMatrix]
