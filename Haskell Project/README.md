# Game of Battleship

Detials can be found in `Project1.pdf`

## Execution

1. Cd to `Ver 1.0` or `Ver 2.0`.
2. Compile using  `ghc -O2 --make Main`.
3. Run `Main.exe` with 3 targets as parameters, for example `Main A1 D4 G2` would search for the target ["A1", "D4", "G2"]. It will then use the `Main` model to guess the target; the output will look something like:
```
Your guess 1:  ["A1","D3","H2"]
My answer:  (1,0,2)
Your guess 2:  ["A1","C2","G1"]
My answer:  (1,0,2)
Your guess 3:  ["A1","C3","G3"]
My answer:  (1,0,2)
Your guess 4:  ["A1","C4","G2"]
My answer:  (3,0,0)
Your guess 5:  ["A1","D4","G2"]
My answer:  (3,0,0)
You got it in 5 guesses!
```

## updates in Ver2.0

- change the defination of `Location`

- optimization of `nextGuess` function