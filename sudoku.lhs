Sudoku Solver

S Rank, May-June 2005

TODO: generalise "pairs" rule for triples, 4s, etc? 
``the general rule is "when N cells have only N candidates between them,
 those N candidates may be eliminated from the other cells in the unit"''
This is removeExclusives, but it's very expensive!
TODO: try to solve without removeExclusives, but 
      call it in later if it's needed?

TOOD: "X-wing" rule: in 
  9? * * * 9?
  X  X X X X
  X  X X X X
  9? * * * 9?
where 9? indicate possible 9s, * indicates 9s are impossible, and X indicates
other info, we can remove 9s from columns 1 and 5 due to info from
top and bottom rows.

TODO: possibly use guesswork?  "If this box is a 1, can the puzzle be solved?"


TODO: see other rules at
http://www.simes.clara.co.uk/programs/sudokutechniques.htm



3. Reducing Candidates Part 1
Sudoku program uses this technique

Sometimes, when you examine a block, you can determine that a 
certain number must be in a specific row or column, even though 
you cannot determine exactly which cell in that row or column. More »

4. Reducing Candidates Part 2
Sudoku program uses this technique

If a number appears as candidates for two cells 
in two different blocks, but both cells are in the same
 column or row, it is possible to remove that number as a
 candidate for other cells in that column or row. More »


6. Reducing Candidates Part 4
Sudoku program uses this technique

If two cells in the same row, column or block have only the 
same two candidates, then those candidates can be removed from 
other cells in that row, column or block. This technique can 
also be extended to cover more that two cells. More »

7. X-Wing
Sudoku program uses this technique

This is another method of reducing the candidates when two 
rows have the same candidate only in the same two columns. More »

8. Swordfish
Sudoku program does not (yet?) use this technique

This is a more general form of the X-Wing. More »
9. Nishio
Sudoku program does not (yet?) use this technique

This is limited form of trial and error. It asks what are 
the effects of putting a particular number into a particular cell? More »



TODO: count iterations?
TODO: nicer input capabilities

> import List(nub, (\\), transpose)


Representation of a grid cell: either solved with a particular value
or a list of possibilities.

TODO: better show function for grids

> data Cell = Solved Int | 
>             Possibles [Int] deriving (Eq)

> instance Show Cell where
>     show (Solved n) = show n
>     show (Possibles ps) = show ps

Is it a valid solution?

> isValidSolution :: [[Cell]] -> Bool
> isValidSolution cells =
>     ((map length cells) == [9,9,9,9,9,9,9,9,9]) &&
>     (and $ map validSet cells) &&
>     (and $ map validSet $ transpose cells) &&
>     (and $ map validSet $ gridToFromSquares cells)
>         where validSet :: [Cell] -> Bool
>               validSet cells = (allSolved cells) && (eachOnce cells)
>               allSolved :: [Cell] -> Bool
>               allSolved cells = length (getSolved cells) == 9
>               eachOnce :: [Cell] -> Bool
>               eachOnce cells = (((getSolved cells) \\ [1..9]) == []) && 
>                                (([1..9] \\ (getSolved cells)) == [])


If only one possibility exists, set the cell to Solved

> isSolved :: Cell -> Cell
> isSolved (Possibles ([x])) = Solved x
> isSolved (Possibles xs) = Possibles xs
> isSolved solved = solved


Give a list of all numbers which have already been solved for

> getSolved :: [Cell] -> [Int]
> getSolved [] = []
> getSolved ((Solved x):xs) = x:getSolved xs
> getSolved (_:xs) = getSolved xs


If there's only one place for a digit, we've solved for it.
FIXME: this is a bit ugly!

> doRowDigit ::  Int -> [Cell] -> [Cell]
> doRowDigit n cells
>  | length (filter (==n) (possList cells)) == 1 = putIn n cells
>  | otherwise = cells
>     where putIn :: Int -> [Cell] -> [Cell]
>           putIn _ [] = []
>           putIn n ((Solved x):cells) = (Solved x):putIn n cells
>           putIn n ((Possibles xs):cells)
>                     | n `elem` xs = (Solved n):cells
>                     | otherwise = (Possibles xs):putIn n cells
>           possList :: [Cell] -> [Int]
>           possList ((Solved n):cells) = n:possList cells
>           possList ((Possibles ps):cells) = ps++possList cells
>           possList [] = []


Look at a set of cells, and remove Solved numbers from Possibles lists

> removeSolved :: [Cell] -> [Cell]
> removeSolved cells
>     = map (removePoss (getSolved cells)) cells
>       where removePoss :: [Int] -> Cell -> Cell
>             removePoss ns (Possibles ps) = Possibles (ps \\ ns)
>             removePoss _ solved = solved


Convert a list of rows to (or from) a list of squares.
This function is its own inverse.

> gridToFromSquares :: [[Cell]] -> [[Cell]]
> gridToFromSquares [] = []
> gridToFromSquares cells = 
>     (take 3 $ (doFirstThree cells):gridToFromSquares (map (drop 3) cells)) ++
>      (gridToFromSquares (drop 3 cells))
>         where
>         doFirstThree =  concat . take 3 . map (take 3) 


"Pairs rule": if we have two squares with exactly [a,b], then 
neither a nor b can appear in any other squares.
FIXME: this can be generalised to three squares with exactly [a,b,c]

> pairsRule :: [Cell] -> [Cell]
> pairsRule cells = removePairs (getPairs cells) cells
>     where removePairs :: [[Int]] -> [Cell] -> [Cell]
>           removePairs [] cells = cells
>           removePairs _ [] = []
>           removePairs (p:ps) cells = removePair p (removePairs ps cells)
>           removePair :: [Int] -> [Cell] -> [Cell]
>           removePair _ [] = []
>           removePair pair (c@(Possibles ps):cells)
>               | ps == pair = c:removePair pair cells
>               | otherwise = (Possibles (ps \\ pair)): removePair pair cells
>           removePair pair (c:cells) = c:removePair pair cells

> getPairs :: [Cell] -> [[Int]]
> getPairs = getTwos . getPossPairs
>     where getTwos :: (Eq a) => [a] -> [a]
>           getTwos [] = []
>           getTwos (x:xs)
>               | length (filter (==x) xs) == 1 = x:(getTwos (filter (/= x) xs))
>               | otherwise = getTwos (filter (/= x) xs)

> getPossPairs :: [Cell] -> [[Int]]
> getPossPairs [] = []
> getPossPairs ((Solved n):cells) = getPossPairs cells
> getPossPairs ((Possibles [a,b]):cells) = [a,b]:getPossPairs cells
> getPossPairs (_:cells) = getPossPairs cells


Generalisation of pairs rule: if a set of n cells have only 
n candidates between them,  those N candidates may be eliminated 
from the other cells in the unit

example: 2 cells have only [1,2] between them, and three have only [3,4,5]
between them

> nRuleEg :: [Cell]
> nRuleEg = [Possibles [1,2], Possibles [1,2], 
>            Possibles [3,4], Possibles [3,5], Possibles [4,5],
>            Possibles [1..9],Possibles [1..9],Possibles [1..9],
>            Possibles [1..9]]


> getPossibles :: [Cell] -> [[Int]]
> getPossibles [] = []
> getPossibles ((Solved _):cells) = getPossibles cells
> getPossibles ((Possibles ps):cells) = ps:getPossibles cells


> possiblePowers :: [Cell] -> [[[Int]]]
> possiblePowers = filter (\x -> (length x >= 2) && (length x < 9)) .
>                  powerset . getPossibles

> potentials =  filter (\(a,b) -> length a == b ) .
>               map (\x -> (x, length $ nub $ concat x)) . possiblePowers 

> exclusiveSets :: [Cell] -> [[Int]]
> exclusiveSets =  map nub . map concat . map fst . potentials

> removeExclusives :: [Cell] -> [Cell]
> removeExclusives cells = map (removeExSets exSets) cells
>     where exSets = exclusiveSets cells
>           removeExSets :: [[Int]] -> Cell -> Cell
>           removeExSets [] cell = cell
>           removeExSets _ (Solved n) = Solved n
>           removeExSets (ns:nss) (Possibles ps) 
>               | ps \\ ns == [] = removeExSets nss $ Possibles ps
>               | otherwise = removeExSets nss $ Possibles (ps \\ ns)

Mark P Jones' powerset
http://www.haskell.org/pipermail/haskell-cafe/2003-June/004484.html

> powerset       :: [a] -> [[a]]
> powerset []     = [[]]
> powerset (x:xs) = xss /\/ map (x:) xss
>     where xss = powerset xs

> (/\/)        :: [a] -> [a] -> [a]
> []     /\/ ys = ys
> (x:xs) /\/ ys = x : (ys /\/ xs)



Execute all the `cheap' rules on a set of cells
(which may be a row, column, or 'square').  `Cheap' means
not `removeExclusives', which is expensive!

> tryAllRules :: [Cell] -> [Cell]
> tryAllRules = map isSolved . pairsRule . checkAllDigits . removeSolved

> expensiveRules :: [[Cell]] -> [[Cell]]
> expensiveRules = map removeExclusives

> tryExpensiveRules :: [[Cell]] -> [[Cell]]
> tryExpensiveRules = 
>     gridToFromSquares . expensiveRules  . gridToFromSquares .
>     transpose . expensiveRules . transpose . expensiveRules

Try all `cheap' rules on all structures (rows, columns, squares)

> doAllRules :: [[Cell]] -> [[Cell]]
> doAllRules =
>     gridToFromSquares . map tryAllRules . gridToFromSquares .
>     transpose . map tryAllRules . transpose . map tryAllRules

> checkAllDigits :: [Cell] -> [Cell]
> checkAllDigits = checkAllDigits' 9
>     where checkAllDigits' :: Int -> [Cell] -> [Cell]
>           checkAllDigits' 0 = id
>           checkAllDigits' n = checkAllDigits' (n-1) . doRowDigit n


Iterate until solved or impossible to get any further

> tryToSolve :: [[Cell]] -> [[Cell]]
> tryToSolve = tryToSolveCheaply --tryHarder

> tryToSolveCheaply :: [[Cell]] -> [[Cell]]
> tryToSolveCheaply grid
>     | nextStep == grid = grid
>     | otherwise = tryToSolveCheaply nextStep
>     where nextStep = doAllRules grid



Iterate using all rules (including the expensive ones!)
FIXME: uncomment??

> tryHarder :: [[Cell]] -> [[Cell]]
> tryHarder grid
>     | isValidSolution cheapResult = cheapResult
>--     | grid == tryTryAgain = grid
>     | otherwise = tryAgain cheapResult
>     where cheapResult = tryToSolveCheaply grid
>           tryAgain :: [[Cell]] -> [[Cell]]
>           tryAgain puzzle
>               | nextStep == puzzle = puzzle
>               | otherwise = tryHarder puzzle
>               where nextStep = tryExpensiveRules puzzle
>           tryTryAgain = tryAgain cheapResult

Test functions

> tryIt :: [[Cell]]
> tryIt = tryToSolve examplePuzzle

> allTests :: Bool
> allTests = test_squaresToGrid && test_pairsRule && test_egPuzzle &&
>            test_tryIt && test_gridToFromSquares && test_hardPuzzle &&
>            test_maybePuzzle

> test_squaresToGrid :: Bool
> test_squaresToGrid = (gridToFromSquares $ gridToFromSquares examplePuzzle) == 
>                      examplePuzzle

> countSolved :: [[Cell]] -> Int
> countSolved = sum . map countSolvedRow
>     where countSolvedRow :: [Cell] -> Int
>           countSolvedRow [] = 0
>           countSolvedRow ((Solved _):cells) = 1 + countSolvedRow cells
>           countSolvedRow (_:cells) = countSolvedRow cells

           
> examplePuzzle :: [[Cell]]
> examplePuzzle = 
>     [[unk, Solved 6, unk, Solved 1, unk, Solved 4, unk, Solved 5, unk],
>      [unk, unk, Solved 8, Solved 3, unk, Solved 5, Solved 6, unk, unk],
>      [Solved 2, unk,unk,unk,unk,unk,unk,unk,Solved 1],
>      [Solved 8,unk,unk,Solved 4,unk, Solved 7, unk, unk, Solved 6],
>      [unk,unk,Solved 6,unk,unk,unk,Solved 3,unk,unk],
>      [Solved 7,unk,unk,Solved 9,unk,Solved 1,unk,unk,Solved 4],
>      [Solved 5,unk,unk,unk,unk,unk,unk,unk,Solved 2],
>      [unk,unk,Solved 7,Solved 2,unk, Solved 6,Solved 9,unk,unk],
>      [unk,Solved 4,unk,Solved 5,unk, Solved 8,unk,Solved 7,unk]
>     ]
>     where unk = Possibles [1..9]

> examplePuzzleAnswer :: [[Cell]]
> examplePuzzleAnswer = 
>     [[Solved 9,Solved 6,Solved 3,Solved 1,Solved 7,Solved 4,Solved 2,Solved 5,Solved 8],
>      [Solved 1,Solved 7,Solved 8,Solved 3,Solved 2,Solved 5,Solved 6,Solved 4,Solved 9],
>      [Solved 2,Solved 5,Solved 4,Solved 6,Solved 8,Solved 9,Solved 7,Solved 3,Solved 1],
>      [Solved 8,Solved 2,Solved 1,Solved 4,Solved 3,Solved 7,Solved 5,Solved 9,Solved 6],
>      [Solved 4,Solved 9,Solved 6,Solved 8,Solved 5,Solved 2,Solved 3,Solved 1,Solved 7],
>      [Solved 7,Solved 3,Solved 5,Solved 9,Solved 6,Solved 1,Solved 8,Solved 2,Solved 4],
>      [Solved 5,Solved 8,Solved 9,Solved 7,Solved 1,Solved 3,Solved 4,Solved 6,Solved 2],
>      [Solved 3,Solved 1,Solved 7,Solved 2,Solved 4,Solved 6,Solved 9,Solved 8,Solved 5],
>      [Solved 6,Solved 4,Solved 2,Solved 5,Solved 9,Solved 8,Solved 1,Solved 7,Solved 3]]


> test_egPuzzle :: Bool
> test_egPuzzle = (length examplePuzzle == 9) &&
>                 ((filter (/=9) $ map length examplePuzzle) == [])

> test_tryIt :: Bool
> test_tryIt = (tryIt) == examplePuzzleAnswer

> egRow :: [Cell]
> egRow = [Solved 9, Solved 6, Solved 3, Solved 1, Solved 7, 
>          Solved 4, Solved 2, Solved 5, unk]
>     where unk = Possibles [1..9]

> test_gridToFromSquares :: Bool
> test_gridToFromSquares =
>     ((gridToFromSquares examplePuzzle) == realGtoSEg) &&
>     ((gridToFromSquares $ gridToFromSquares examplePuzzle) == examplePuzzle) &&
>     ((gridToFromSquares $ gridToFromSquares hardPuzzle) == hardPuzzle)

> test_pairsRule = 
>     (pairsRule [Possibles [1, 2], Possibles [1, 2], 
>                 Possibles [1, 2, 3], Possibles [3,4]]) == 
>     [Possibles [1, 2], Possibles [1, 2], 
>            Possibles [3], Possibles [3,4]]

> realGtoSEg = [[unk,Solved 6,unk,unk, unk, Solved 8,Solved 2, unk,unk],
>               [Solved 1, unk, Solved 4,Solved 3, unk, Solved 5,unk,unk,unk],
>               [unk, Solved 5, unk,Solved 6, unk, unk,unk,unk,Solved 1],
>               [Solved 8,unk,unk,unk,unk,Solved 6,Solved 7,unk,unk ],
>               [Solved 4,unk, Solved 7,unk,unk,unk,Solved 9,unk,Solved 1],
>               [unk, unk, Solved 6,Solved 3,unk,unk,unk,unk,Solved 4],
>               [Solved 5,unk,unk,unk,unk,Solved 7,unk,Solved 4,unk],
>               [unk,unk,unk,Solved 2,unk, Solved 6,Solved 5,unk, Solved 8],
>               [unk,unk,Solved 2,Solved 9,unk,unk,unk,Solved 7,unk]
>              ]
>     where unk = Possibles [1..9]

From the Guardian, 27th May 2005: "Number 17, Difficult".  Ha!

> hardPuzzle = [[unk,unk,unk,unk,Solved 5,unk,unk,unk, Solved 3],
>               [unk,Solved 1,unk,unk,unk,unk,Solved 9,unk,unk],
>               [unk,unk,Solved 2,unk,unk,Solved 6,unk,unk,unk],
>               [Solved 8,unk,unk,unk,unk,Solved 7,unk,Solved 5,unk],
>               [unk,Solved 6,unk,unk,unk,unk,unk,Solved 1,unk],
>               [unk,Solved 9,unk,Solved 3,unk,unk,unk,unk,Solved 4],
>               [unk,unk,unk,Solved 1,unk,unk,Solved 8,unk,unk],
>               [unk,unk,Solved 7,unk,unk,unk,unk,Solved 2,unk],
>               [Solved 4,unk,unk,unk,Solved 9,unk,unk,unk,unk]]
>     where unk = Possibles [1..9]

> test_hardPuzzle :: Bool
> test_hardPuzzle = isValidSolution $ tryToSolve hardPuzzle


Guardian number 20 `easy'

> graun20 :: [[Cell]]
> graun20 = puzzleFromDigits [[0,8,0,4,0,5,0,0,0],
>                             [4,0,1,0,0,0,6,0,0],
>                             [0,6,0,0,1,0,0,4,0],
>                             [7,0,0,2,0,4,0,0,8],
>                             [0,0,3,0,5,0,7,0,0],
>                             [9,0,0,6,0,8,0,0,4],
>                             [0,7,0,0,9,0,0,3,0],
>                             [0,0,8,0,0,0,2,0,5],
>                             [0,0,0,3,0,1,0,6,0]]

> maybePuzzle :: [[Cell]]
> maybePuzzle = puzzleFromDigits [[9,0,2,0,0,0,8,0,7],
>                                 [0,5,4,0,0,0,2,3,0],
>                                 [0,0,0,7,0,9,0,0,0],
>                                 [3,0,5,8,0,2,7,0,1],
>                                 [0,0,0,0,0,0,0,0,0],
>                                 [6,0,1,5,0,7,4,0,2],
>                                 [0,0,0,3,0,8,0,0,0],
>                                 [0,7,8,0,0,0,1,5,0],
>                                 [2,0,9,0,0,0,6,0,8]]

> test_maybePuzzle :: Bool
> test_maybePuzzle = isValidSolution $ tryToSolve maybePuzzle

"Hard" from the times

> timesHard :: [[Cell]]
> timesHard = puzzleFromDigits [[5,0,0,0,0,2,6,0,0],
>                               [0,7,8,0,0,6,0,2,0],
>                               [0,2,0,0,0,0,9,0,3],
>                               [0,0,0,6,0,0,8,0,0],
>                               [4,0,0,0,0,0,0,0,1],
>                               [0,0,7,0,0,4,0,0,0],
>                               [3,0,2,0,0,0,0,5,0],
>                               [0,9,0,5,0,0,1,7,0],
>                               [0,0,1,8,0,0,0,0,6]]

Create a puzzle from a set of digits.  0 indicates "unknown", anything
else indicates "solved" with that digit.
FIXME: check for things outside the range [0..9]?

> puzzleFromDigits :: [[Int]] -> [[Cell]]
> puzzleFromDigits = map (map digToCell)
>     where digToCell :: Int -> Cell
>           digToCell 0 = Possibles [1..9]
>           digToCell n = Solved n

Two hard puzzles from sudoku.com:

> desperatePuzzle :: [[Cell]]
> desperatePuzzle = puzzleFromDigits [[0, 3, 0, 2, 0, 0, 7, 8, 1],
>                                     [0, 0, 8, 0, 0, 0, 3, 0, 4],
>                                     [0, 7, 4, 0, 3, 0, 9, 6, 0],
>                                     [0, 0, 0, 1, 8, 0, 6, 0, 7],
>                                     [0, 1, 7, 6, 0, 9, 0, 3, 8],
>                                     [8, 0, 0, 3, 7, 5, 1, 4, 9],
>                                     [0, 4, 1, 0, 6, 0, 8, 9, 0],
>                                     [7, 0, 0, 0, 0, 0, 4, 1, 0],
>                                     [9, 8, 0, 4, 1, 3, 0, 7, 0]]

> vhardPuzzle :: [[Cell]]
> vhardPuzzle =  puzzleFromDigits [[0,4,2,0,3,0,0,0,0],
>                                  [0,0,5,0,0,0,0,0,0],
>                                  [0,0,0,9,0,0,0,0,8],
>                                  [1,0,0,8,0,0,0,0,0],
>                                  [0,0,3,0,4,0,2,0,0],
>                                  [0,0,0,0,0,7,0,0,6],
>                                  [7,0,0,0,0,6,0,0,0],
>                                  [0,0,0,0,0,0,4,6,0],
>                                  [0,0,0,0,2,0,5,9,0]]

> toryGraphPuzzle :: [[Cell]]
> toryGraphPuzzle =
>     puzzleFromDigits [[0,1,2,5,0,4,8,7,0],
>                       [0,0,0,0,0,0,0,0,0],
>                       [7,5,0,0,0,6,0,2,3],
>                       [0,0,4,1,0,8,7,0,0],
>                       [0,2,0,0,5,0,0,4,0],
>                       [0,0,3,4,0,9,5,0,0],
>                       [4,8,0,0,0,0,0,1,7],
>                       [0,0,0,0,0,0,0,0,0],
>                       [0,3,5,7,0,1,6,9,0]]

> notherPuzzle = 
>     puzzleFromDigits [[0, 0, 0, 0, 0, 3, 0, 6, 0],
>                       [0, 0, 0, 0, 0, 0, 0, 1, 0],
>                       [0, 9, 7, 5, 0, 0, 0, 8, 0],
>                       [0, 0, 0, 0, 9, 0, 2, 0, 0],
>                       [0, 0, 8, 0, 7, 0, 4, 0, 0],
>                       [0, 0, 3, 0, 6, 0, 0, 0, 0],
>                       [0, 1, 0, 0, 0, 2, 8, 9, 0],
>                       [0, 4, 0, 0, 0, 0, 0, 0, 0],
>                       [0, 5, 0, 1, 0, 0, 0, 0, 0]]

> toughPuzzle = puzzleFromDigits [[0,0,0,0,0,2,9,0,0],
>                                 [2,0,4,0,5,0,0,6,0],
>                                 [0,0,7,4,0,0,0,0,3],
>                                 [0,4,0,0,6,0,0,9,0],
>                                 [0,0,0,1,0,5,0,0,0],
>                                 [0,3,0,0,4,0,0,5,0],
>                                 [1,0,0,0,0,6,8,0,0],
>                                 [0,8,0,0,1,0,7,0,6],
>                                 [0,0,6,8,0,0,0,0,0]]



Unsolved (so far) puzzles:

> unsolvedPuzzles :: [[[Cell]]]
> unsolvedPuzzles = [desperatePuzzle, vhardPuzzle, 
>                    toryGraphPuzzle, notherPuzzle, toughPuzzle]

> unsolvedBest :: [Int]
> unsolvedBest = [44,21,39,30,33]

> countUnsolved :: [Int]
> countUnsolved = map (countSolved . tryToSolve) unsolvedPuzzles