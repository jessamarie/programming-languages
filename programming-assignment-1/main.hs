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
alphaRename lexp@(Apply (Lambda (Atom x) (Atom y)) (Atom z)) 
	| x == y && x == z = (Apply (Lambda a a) (Atom z)) 
	| x == z = (Apply (Lambda a (Atom y)) (Atom z)) 
	| otherwise = lexp
	where a = getAtom lexp
--  alphaRename lexp@(Lambda (Atom x) exp) = lexp 
--alphaRename lexp@(Lambda (Atom x) exp) = lexp = let exp' = rename lexp show lexp 
-- NEEDS WORK: How to actually rename the variable?

atoms = [Atom "a", Atom "c", Atom "d", Atom "f", Atom "g", Atom "h"]
getAtom:: Lexp -> Lexp
getAtom lexp = (Atom "j")


-- TODO: Beta-reduction in Applicative Order --
-- HANDLE :: (Atom x), (Lambda (Atom x) expression)
--           (Apply exp_x exp_y), (Lambda (atom x) var)
 
betaReduce:: Lexp -> Lexp
betaReduce v@(Atom atom) = v
betaReduce lexp@(Apply exp1@(Lambda (Atom x) (Atom y)) exp2)
	| x == y = exp2
	| otherwise = (Atom y)
-- betaReduce lexp@(Apply exp1@(Lambda (Atom x) exp) exp2) = exp' = betaReduce exp
betaReduce lexp@(Lambda (Atom atom) exp) = Atom atom  
betaReduce lexp@(Apply exp1 exp2) = exp2

--betaReduce lexp@(Lambda (Atom atom) exp) = (Lambda (Atom atom) (betaReduce exp)) 
--betaReduce lexp = lexp
--betaReduce lexp@(Apply exp1 exp2) = (Apply (betaReduce exp1) (betaReduce exp2))

reducers:: Lexp -> Lexp 
reducers lexp = alphaRename lexp

-- TODO: Eta-Conversion --
-- \v.(E v) if v == v AND isBound(v) -> E, else \v.(E v) 

--etaConvert:: Lexp -> Lexp
--etaConvert v@(Atom atom) = v
--etaConvert lexp@(Lambda (Atom x) exp) = lexp -- eta reduce here 

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
  







