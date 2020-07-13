# Puzzle Filling

Detials can be found in `Project2.pdf`

## Execution

1. Cd to `Version1`.
2. Open compiling environment using `swipl`.
3. Compile target file using `[p2].`.
4. call `puzzle_solution` function such as `Puzzle = [['#',h,'#'],[_,_,_],['#',_,'#']], WordList = [[h,a,t], [b,a,g]], puzzle_solution(Puzzle, WordList).`

============

1. Cd to `Version2` (this version is more powerful, contains I/O operations and preprocessing functions).
2. Open compiling environment using `swipl`.
3. Compile target file using `[p2].`.
4. Call the `main` function to solve puzzle, such as `main('samples/puzzle1', 'samples/words1', 'samples/filled1').`