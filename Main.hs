{-Sat solver for a class. Doesn't work for nested Conjunctions when reduced to distributive normal form
 -}


module Main where


main :: IO()
main = eachLine convertToLogic

eachLine :: (String -> String) -> IO()
eachLine _ = interact (unlines . map convertToLogic . lines)

--data type with separate constructors for different cases
--Statement to hold some form of predicate with 2 subvalues and an operator
--LogicChar to hold a single character as a fundamental building Block
--Negate to serve as a negated form of one of the other types
data Logic = Statement Logic Logic String | LogicChar String | Negate Logic deriving Eq

-- legacy show code left in just in case, not used in compiled mode
instance Show Logic where
    show(Statement l r op) = "(" ++ show l ++ " "  ++ show op ++  " " ++ show r ++ " " ++ ")"
    show(LogicChar l) = show l
    show(Negate l ) = show l

--Function to recursively print Logic data types
printLogic :: Logic -> String
printLogic(Statement l r op) = "(" ++ printLogic l ++ " "  ++  op ++  " " ++ printLogic r  ++ ")"
printLogic(LogicChar l) = l
printLogic(Negate l) = "~" ++ printLogic l



--Convenience functions to treat haskell lists like a stack
push :: a ->  [a] -> [a]
push x xs=  x:xs

pop :: [a] -> ( a, [a])
pop [] = pop []
pop (x : xs) =  ( x,xs)



--pops 2 elements to satisfy removal of do notation
pop2 :: [Logic] ->  (Logic, Logic, [Logic])
pop2(x : (y : xs)) =  (y, x , xs)
pop2 _ = ( (LogicChar "FAILED POP2"), (LogicChar "FAILED POP2"), [(LogicChar "FAILED POP2")])


makeAndPushStatement:: (Logic, Logic, [Logic])-> String -> [Logic]
makeAndPushStatement (l, r, stack) operator = push (Statement l r operator) stack

--Special function just for negate since it only pops once
negateIsSpecial:: (Logic, [Logic]) -> [Logic]
negateIsSpecial(l, stack) = push(Negate l) stack



--Function to map inpunts from stdin to my Logic datatypes, uses a stack to keep track of operands to be used
convertToLogicImpl :: [Logic] -> Char  ->  [Logic]
convertToLogicImpl operands 'A'  =  makeAndPushStatement (pop2 operands) "v"
convertToLogicImpl operands 'C' = makeAndPushStatement (pop2 operands) "=>"
convertToLogicImpl operands 'D'  = makeAndPushStatement (pop2 operands) "|" 
convertToLogicImpl operands 'E'  = makeAndPushStatement (pop2 operands)"<=>" 
convertToLogicImpl operands 'J' = makeAndPushStatement (pop2 operands) "+" 
convertToLogicImpl operands 'K'  = makeAndPushStatement (pop2 operands) "&" 
convertToLogicImpl operands 'N' = negateIsSpecial (pop operands)
convertToLogicImpl operands x  = push (LogicChar [x]) operands

--function to call my conversion function via foldl and grab the first (and only) element of the resultant stack
convertToLogic:: [Char] -> String
convertToLogic input = satSolved (distributiveNormalForm(negationNormalForm (head (foldl convertToLogicImpl [] input))))


--function to determine if the function is solvable and concat that answer with the pretty printed logic structure
satSolved:: Logic -> String
satSolved (Statement l r op) =  if isSatisfiable (Statement l r op) then "true " ++ printLogic (Statement l r op) else "false " ++ printLogic (Statement l r op)
satSolved (Negate x) = "true " ++ printLogic x
satSolved (LogicChar l) = "true " ++ l



--conversion from stock to NNF for my own sanity's sake to not have to refer back
{- OPERATORS
 a <=> B = (A -> B) n (B -> A)
 a -> B = ~ A v b
 a + b = (a n ~B) v(b n ~a) 
 a | b =  `(a n b)
 ~(A v B) = ~A n ~B
 ~(A n B) = ~A v ~B
 ~~ A = A
 
-}


--function to grab core logic from a negative statement
getLogicFromNegate :: Logic -> Logic
getLogicFromNegate(Negate l) = l
getLogicFromNegate x = x


--Function to apply de morgan's laws to a negative statement
negationDeMorgan :: Logic -> Logic
negationDeMorgan(Statement l r "v") = 
    Statement (Negate l) (Negate r) "&"
negationDeMorgan(Statement l r "&") = 
    Statement (Negate l) (Negate r)"v"
negationDeMorgan x = x
    

--Function to apply NNF to a logic predicate
negationNormalFormImpl :: Logic -> Logic
negationNormalFormImpl(Statement l r "<=>") = 
    Statement (Statement l r "=>") (Statement r l "=>") "&"
negationNormalFormImpl(Statement l r "=>") = 
    Statement (Negate l) r "v"
negationNormalFormImpl(Statement l r "+") = 
    Statement (Statement l (Negate r) "&") (Statement (Negate l) r "&") "v"
negationNormalFormImpl(Statement l r "|") = 
    Statement (Negate l) (Negate r) "&"

negationNormalFormImpl(Negate l) = negationDeMorgan l    

negationNormalFormImpl x = x



--Grabs the left element from a statement
getLogicLeft:: Logic  -> Logic
getLogicLeft (Statement l _ _) = l
getLogicLeft x = x
--Ditto for the right
getLogicRight :: Logic -> Logic
getLogicRight (Statement _ r _) = r
getLogicRight x = x
--Ditto for the operator
getLogicOp :: Logic -> String
getLogicOp (Statement _ _ op) = op
getLogicOp _ = "NOT A STATEMENT"

--Tells me if the data is an isntance of the LogicChar subtype
isChar :: Logic -> Bool
isChar (LogicChar _) = True
isChar _ = False



--Created to make one of the following lines more manageable
nnfs :: Logic-> Logic-> String -> Logic
nnfs l r op = negationNormalFormImpl (Statement l r op)

--Function to recursively apply NNF down a Logic tree
negationNormalForm :: Logic -> Logic
negationNormalForm (Statement l r op) = Statement (negationNormalForm (getLogicLeft (nnfs l r op))) (negationNormalForm (getLogicRight (nnfs l r op))) (getLogicOp (nnfs l r op))
negationNormalForm (Negate l )=
    if isChar l
        then  Negate l
    else
        negationNormalForm (negationNormalFormImpl (Negate l) )
negationNormalForm x = x

isStatement:: Logic -> Bool
isStatement (Statement _ _ _) = True
isStatement _ = False

getLogicTuple :: Logic -> (Logic, Logic)
getLogicTuple (Statement l r _) = (l, r)
getLogicTuple x = (x, x)



--Function to make sure the right side of a Logic element gets converted properly
distributiveNormalFormRight ::Logic -> Logic
distributiveNormalFormRight(Statement l r "&") = 
    
    if isStatement r && getLogicOp r == "v"
        then
            Statement (distributiveNormalForm (Statement (getLogicLeft r) l "&")) (distributiveNormalForm(Statement (getLogicRight r) l "&")) "v"
    else 
        Statement (distributiveNormalForm r) (distributiveNormalForm l) "&"
distributiveNormalFormRight x = x

--Function to convert to DNF by ensuring the left side satisfies requirements then ensuring the right side also satisfies
distributiveNormalForm :: Logic -> Logic
distributiveNormalForm (Statement l r "&") = 
    if isStatement l && getLogicOp l == "v"
        then 
            Statement (distributiveNormalForm (Statement (getLogicLeft l) r "&")) (distributiveNormalForm(Statement (getLogicRight l) r "&")) "v"
    else 
        if isStatement r && getLogicOp r == "v"
            then  
                Statement (distributiveNormalForm(Statement  l (getLogicLeft r  )"&")) (distributiveNormalForm(Statement  l  (getLogicRight r) "&")) "v"
        else
            distributiveNormalFormRight( Statement (distributiveNormalForm l) (distributiveNormalForm r) "&")
distributiveNormalForm x = x


--gets the LogicChar from a negated Logic Char
getCharFromNegatedLogicChar :: Logic -> String
getCharFromNegatedLogicChar (LogicChar l ) = l
getCharFromNegatedLogicChar _ = "NOT A NEGATED LOGIC CHAR"


--Sat solves a specific statement. Since the only ways to fail are if P and ~p are in the same predicate, that's all we test for here
satStatement :: (Logic, Logic) -> Bool
satStatement (Negate l, LogicChar x ) = getCharFromNegatedLogicChar l /= x
satStatement (LogicChar x, Negate l) = getCharFromNegatedLogicChar l /= x
satStatement _ = True


--recursively solves predicates bubbling up falses until they meet a true at an OR
isSatisfiable :: Logic -> Bool
isSatisfiable (Statement l r "v") = isSatisfiable l || isSatisfiable r
isSatisfiable (Statement l r "&") = 
    if not (isStatement l) && not (isStatement r)
        then
            satStatement (l, r) 
    else isSatisfiable l && isSatisfiable r
isSatisfiable _ = True

