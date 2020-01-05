module Matrix where

initialMatrix = [
    [1, 2, 3],
    [4, 5, 6],
    [7, 8, 9]
  ]

getDimensions matrix = [length matrix, length $ head matrix]

transposeMatrix matrix = 
  let [m2, n2] = getDimensions matrix in
    map (\i -> map (\j -> matrix !! j !! i) (take n2 (iterate (1+) 0))) (take m2 (iterate (1+) 0))

reflectMatrixHorizontally matrix = 
  let [m2, n2] = getDimensions matrix in
    map (\i -> let i2 = m2 - i - 1 in
      map (\j -> matrix !! i2 !! j) (take n2 (iterate (1+) 0))) (take m2 (iterate (1+) 0))

rotateMatrixCounterClockwise = reflectMatrixHorizontally . transposeMatrix