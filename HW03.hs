module HW03 where

data Expression =
    Var String                   -- Variable
  | Val Int                      -- Integer literal
  | Op Expression Bop Expression -- Operation
  deriving (Show, Eq)

-- Binary (2-input) operators
data Bop =
    Plus
  | Minus
  | Times
  | Divide
  | Gt
  | Ge
  | Lt
  | Le
  | Eql
  deriving (Show, Eq)

data Statement =
    Assign   String     Expression
  | Incr     String
  | If       Expression Statement  Statement
  | While    Expression Statement
  | For      Statement  Expression Statement Statement
  | Sequence Statement  Statement
  | Skip
  deriving (Show, Eq)

type State = String -> Int

-- Exercise 1 -----------------------------------------
-- let st’ = extend st "A" 5
-- in st’ "A" == 5

extend :: State -> String -> Int -> State
extend st s i =
    \lookup ->
         if lookup == s
         then i
         else st lookup

extendCurry :: State -> String -> Int -> State
extendCurry st s i lookup =
         if s == lookup
         then i
         else st lookup

empty :: State
empty _ = 0

-- Exercise 2 -----------------------------------------
-- evalE empty (Op (Val 1) Eql (Val 2)) == 0
-- evalE empty (Val 5) == 5
-- return the evaluation of the expression
evalE :: State -> Expression -> Int
evalE st e =
    case e of
      (Var s) -> st s
      (Val i) -> i
      (Op e1 bop e2) ->
          case bop of
            Plus -> (+) (evalE st (e1)) (evalE st (e2))
            Minus -> (-) (evalE st (e1)) (evalE st (e2))
            Times -> (*) (evalE st (e1)) (evalE st (e2))
            Divide -> (div) (evalE st (e1)) (evalE st (e2))
            Gt -> boolFunc (>) (evalE st e1) (evalE st e2)
            Ge -> boolFunc (>=) (evalE st e1) (evalE st e2)
            Lt -> boolFunc (<) (evalE st e1) (evalE st e2)
            Le -> boolFunc (<=) (evalE st e1) (evalE st e2)
            Eql -> boolFunc (==) (evalE st e1) (evalE st e2)
        where
          boolFunc :: (Int -> Int -> Bool) -> Int -> Int -> Int
          boolFunc f x y =
              if x `f` y then 1 else 0
          -- combo :: (Int -> Int -> Bool) -> Int

-- Exercise 3 -----------------------------------------

data DietStatement = DAssign String Expression
                   | DIf Expression DietStatement DietStatement
                   | DWhile Expression DietStatement
                   | DSequence DietStatement DietStatement
                   | DSkip
                     deriving (Show, Eq)
-- For (Assign "A" (Val 0)) (Op (Var "A") Lt (Var "N")) (Incr "A")
desugar :: Statement -> DietStatement
desugar st =
    case st of
      Assign x exp ->
          DAssign x exp
      Incr x ->
          DAssign x (Op (Var x) Plus (Val 1))
      If exp st1 st2 ->
          DIf exp (desugar st1) (desugar st2)
      While exp st' ->
          DWhile exp (desugar st')
      Sequence st1 st2 ->
          DSequence (desugar st1) (desugar st2)
      For init condition increment body ->
          DSequence
            (desugar init)
            (DWhile condition
              (DSequence
                (desugar body)
                (desugar increment)
              )
            )
      Skip -> DSkip

-- Exercise 4 -----------------------------------------

-- let s = evalSimple empty (DAssign "A" (Val 10))
-- in s "A" == 10
evalSimple :: State -> DietStatement -> State
evalSimple x dst =
    case dst of
      DAssign str exp -> extend x str (evalE x exp)
      DIf exp dst1 dst2 -> if (evalE x exp) == 1
                           then evalSimple x dst1
                           else evalSimple x dst2
     -- DWhile exp dst' ->
     -- DSequence dst1 dst2 ->
     -- DSkip -> empty -- ??


run :: State -> Statement -> State
run = undefined

-- Programs -------------------------------------------

slist :: [Statement] -> Statement
slist [] = Skip
slist l  = foldr1 Sequence l

{- Calculate the factorial of the input

   for (Out := 1; In > 0; In := In - 1) {
     Out := In * Out
   }
-}
factorial :: Statement
factorial = For (Assign "Out" (Val 1))
                (Op (Var "In") Gt (Val 0))
                (Assign "In" (Op (Var "In") Minus (Val 1)))
                (Assign "Out" (Op (Var "In") Times (Var "Out")))


{- Calculate the floor of the square root of the input

   B := 0;
   while (A >= B * B) {
     B++
   };
   B := B - 1
-}
squareRoot :: Statement
squareRoot = slist [ Assign "B" (Val 0)
                   , While (Op (Var "A") Ge (Op (Var "B") Times (Var "B")))
                       (Incr "B")
                   , Assign "B" (Op (Var "B") Minus (Val 1))
                   ]

{- Calculate the nth Fibonacci number

   F0 := 1;
   F1 := 1;
   if (In == 0) {
     Out := F0
   } else {
     if (In == 1) {
       Out := F1
     } else {
       for (C := 2; C <= In; C++) {
         T  := F0 + F1;
         F0 := F1;
         F1 := T;
         Out := T
       }
     }
   }
-}
fibonacci :: Statement
fibonacci = slist [ Assign "F0" (Val 1)
                  , Assign "F1" (Val 1)
                  , If (Op (Var "In") Eql (Val 0))
                       (Assign "Out" (Var "F0"))
                       (If (Op (Var "In") Eql (Val 1))
                           (Assign "Out" (Var "F1"))
                           (For (Assign "C" (Val 2))
                                (Op (Var "C") Le (Var "In"))
                                (Incr "C")
                                (slist
                                 [ Assign "T" (Op (Var "F0") Plus (Var "F1"))
                                 , Assign "F0" (Var "F1")
                                 , Assign "F1" (Var "T")
                                 , Assign "Out" (Var "T")
                                 ])
                           )
                       )
                  ]
