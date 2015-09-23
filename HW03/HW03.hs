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

extend :: State -> String -> Int -> State
extend s k v = (\k' -> if k == k' then v else s k')

empty :: State
empty = const 0

-- Exercise 2 -----------------------------------------

evalE :: State -> Expression -> Int
evalE s (Var var)            = s var
evalE _ (Val i)              = i
evalE s (Op lexpr bop rexpr) = evalBop bop (evalE s lexpr) (evalE s rexpr)

evalBop :: Bop -> Int -> Int -> Int
evalBop Plus x y   = x + y
evalBop Minus x y  = x - y
evalBop Times x y  = x * y
evalBop Divide x y = x `div` y
evalBop Gt x y     = if x > y then 1 else 0
evalBop Ge x y     = if x >= y then 1 else 0
evalBop Lt x y     = if x < y then 1 else 0
evalBop Le x y     = if x <= y then 1 else 0
evalBop Eql x y    = if x == y then 1 else 0

-- Exercise 3 -----------------------------------------

data DietStatement = DAssign String Expression
                   | DIf Expression DietStatement DietStatement
                   | DWhile Expression DietStatement
                   | DSequence DietStatement DietStatement
                   | DSkip
                     deriving (Show, Eq)

desugar :: Statement -> DietStatement
desugar (Assign s e)            = DAssign s e
desugar (Incr s)                = DAssign s (Op (Var s) Plus (Val 1))
desugar (If e t f)              = DIf e (desugar t) (desugar f)
desugar (While expr stmt)       = DWhile expr (desugar stmt)
desugar (For ini test upd body) =
    DSequence
        (desugar ini)
        (DWhile
            test
            (DSequence (desugar body) (desugar upd)))
desugar (Sequence fst snd)      = DSequence (desugar fst) (desugar snd)
desugar Skip                    = DSkip

-- Exercise 4 -----------------------------------------

evalSimple :: State -> DietStatement -> State
evalSimple s (DAssign name expr) = extend s name (evalE s expr)
evalSimple s (DIf test iftrue iffalse) = if (evalE s test) /= 0
    then evalSimple s iftrue
    else evalSimple s iffalse
evalSimple s (DWhile expr stmt) = if (evalE s expr) /= 0
    then evalSimple s (DSequence stmt (DWhile expr stmt))
    else s
evalSimple s (DSequence stmt1 stmt2) = evalSimple (evalSimple s stmt1) stmt2

run :: State -> Statement -> State
run s stmt = evalSimple s (desugar stmt)

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
