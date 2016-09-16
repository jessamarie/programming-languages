import PA1HelperDebug
-- Same thing, but when printed Lexp's are easier to copy and paste into Haskell
-- import PA1HelperDebug

-- Haskell representation of lambda expression
-- In Lambda Lexp Lexp, the first Lexp should always be Atom String
-- data Lexp = Atom String | Lambda Lexp Lexp | Apply Lexp  Lexp 

-- Given a filename and function for reducing lambda expressions,
-- reduce all valid lambda expressions in the file and output results.
-- runProgram :: String -> (Lexp -> Lexp) -> IO()

-- This is the identity function for the Lexp datatype, which is
-- used to illustrate pattern matching with the datatype. "_" was
-- used since I did not need to use bound variable. For your code,
-- however, you can replace "_" with an actual variable name so you
-- can use the bound variable. The "@" allows you to retain a variable
-- that represents the entire structure, while pattern matching on
-- components of the structure.
id' :: Lexp -> Lexp
id' v@(Atom _) = v
id' lexp@(Lambda (Atom _) _) = lexp
id' lexp@(Apply _ _) = lexp 

-------------------------------- BEGIN MY CODE ---------------------------
-- QUESTIONS:

-- TODO: Alpha-Renaming --
-- HANDLE: (Atom atom), (Lambda (Atom x) exp)

alphaRename :: Lexp -> Lexp
alphaRename v@(Atom atom) = v
alphaRename (Lambda (Atom a) exp) = let exp' = alphaRename(exp)
                                    in (Lambda(Atom a) exp')
alphaRename lexp@(Apply exp1 exp2) = let exp1' = alphaRename(exp1) 
                                         exp2' = alphaRename(exp2) 
                                     in (Apply exp1' exp2') 

atoms = [Atom "a", Atom "c", Atom "d", Atom "f", Atom "g", Atom "h"]
getNewAtom:: Lexp -> Lexp
getNewAtom v@(Atom _) = v
getNewAtom lexp@(Lambda(Atom x) (Atom y))
         | x == y = (Lambda(Atom "l") (Atom "l"))
         | otherwise = (Lambda(Atom "l") (Atom y))

-- Beta-reduction in Applicative Order --
-- This function simplifies the expression
betaReduce:: Lexp -> Lexp
betaReduce v@(Atom atom) = v
betaReduce lexp@(Apply (Lambda with@(Atom _) exp1) exp2) = betaSub exp1 with exp2
betaReduce lexp@(Apply exp1 exp2) = let exp1' = betaReduce(exp1) 
                                        exp2' = betaReduce(exp2)
                                    in (Apply exp1' exp2')
betaReduce lexp@(Lambda exp1 exp2) = let exp1' = betaReduce(exp1) 
                                         exp2' = betaReduce(exp2)
                                     in (Lambda exp1' exp2')

-- betaSub -- 
-- input: In this order: the expression to be substituted, the variable argument, the new expression
betaSub:: Lexp -> Lexp -> Lexp -> Lexp
betaSub v@(Atom _) with exp = if v == with then exp else v
betaSub (Apply exp1 exp2) with exp = let exp1' = betaSub exp1 with exp
                                         exp2' = betaSub exp2 with exp
                                        in (Apply exp1' exp2')
betaSub (Lambda exp1 exp2) with exp = let exp1' = betaSub exp1 with exp
                                          exp2' = betaSub exp2 with exp
                                         in (Lambda exp1' exp2') 

reducers:: Lexp -> Lexp 
reducers lexp = alphaRename lexp --etaConvert(betaReduce lexp)

-- TODO: Eta-Conversion --
-- \v.(E v) if v == v AND v is not in E -> E, else \v.(E v) 

etaConvert:: Lexp -> Lexp
etaConvert v@(Atom _) = v
etaConvert lexp@(Lambda v1@(Atom _) exp@(Apply exp1 v2@(Atom _)))
        | v1 == v2 = etaSub lexp v1 -- eta reduce here
        | otherwise = lexp
etaConvert lexp = lexp 

etaSub:: Lexp -> Lexp -> Lexp
etaSub lexp@(Apply left@(Atom _) right@(Atom _)) atom
        | left == atom || right == atom = atom
        | otherwise = lexp
etaSub e@(Atom _) v 
        | e == v = v
        | otherwise = e
etaSub lexp@(Lambda v1@(Atom _) exp@(Apply exp1 v2)) v
        | (etaSub exp1 v) ==  exp1 = exp1
        | otherwise = lexp
etaSub (Apply exp1 exp2) v = let exp1' = etaSub exp1 v 
                                 exp2' = etaSub exp2 v
                             in (Apply exp1' exp2')
                                

-- Possible need for isFree, isBound, some helper functions

-------------------------- END MY CODE ---------------------------------------


-- Entry point of program
main = do
    putStrLn "Please enter a filename containing lambda expressions:"
    fileName <- getLine
 
    -- id' simply returns its input, so runProgram will result
    -- in printing each lambda expression twice.

    -- TODO: change id' to sth like (alphaRename betaReduce etaConvert)	
    runProgram fileName reducers 
  







