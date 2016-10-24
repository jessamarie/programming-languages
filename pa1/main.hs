import PA1Helper
import Data.List
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

-- Alpha-Renaming ---------------------------------------------------------------
-- The following functions replace the bound variables of all lambda expressions
-- This avoids the need to check for free variables, by just renaming all bound ones
-- with new variables


-- alpha prepares an expression to be sent to alpha
alpha:: Lexp -> [Lexp] -> Lexp
alpha v@(Atom _) atomList = v
alpha (Apply exp1 exp2) atomList = let atomList' = atomList \\ (getExpAtoms exp1) -- Make sure we don't catch any free vars while renaming
                                       exp1' = alpha' exp1 atomList
                                       atomList'' = atomList \\ (getExpAtoms exp1') -- Make sure we don't reuse the same variables
                                       exp2' = alpha' exp2 atomList''
                                   in (Apply exp1' exp2')
alpha lexp@(Lambda v@(Atom _) exp) atomList = (Lambda v (alpha' exp atomList')) 
                                              where atomList' = atomList \\ (getExpAtoms exp)


-- alpha' recursively runs through an expression, removing the head of the new atoms list to prevent it from being used again.
alpha':: Lexp -> [Lexp] -> Lexp
alpha' v@(Atom _) atomList = v
alpha' lexp@(Lambda v@(Atom _) exp) atomList = let expAtoms = getExpAtoms lexp
                                                   newAtom  = head atomList
                                                   atomList' = tail atomList
                                                   exp'' = rename v newAtom (alpha' exp atomList') 
                                               in (Lambda newAtom exp'')
alpha' (Apply exp1 exp2) atomList = let exp1' = alpha' exp1 atomList
                                        atomList' = atomList \\ (getExpAtoms exp1') 
                                        exp2' = alpha' exp2 atomList'
                                    in (Apply exp1' exp2')

-- rename returns a new expression with the properly bound variable names changed
rename:: Lexp -> Lexp -> Lexp -> Lexp
rename bound@(Atom _) new free@(Atom _)
      | bound == free = new
      | otherwise = free
rename bound@(Atom _) v1 (Lambda v2@(Atom _) e) = let exp' = (rename bound v1 e)
                                                  in (Lambda v2 exp')
rename bound@(Atom _) v1 exp@(Apply exp1 exp2) = let exp1' = (rename bound v1 exp1)
                                                     exp2' = (rename bound v1 exp2)
                                                 in (Apply exp1' exp2')

-- This function helps get all the atoms in the current expression
getExpAtoms :: Lexp -> [Lexp]
getExpAtoms s@(Atom _)        = [s] 
getExpAtoms (Lambda v exp1)   = [v] ++ (getExpAtoms exp1)
getExpAtoms (Apply exp1 exp2) = (getExpAtoms exp1) ++ (getExpAtoms exp2)


-- Beta-reduction in Applicative Order --------------------------------------------------------
-- These functions simplify a fully alpha-renamed beta expression to the simplest form possible

-- beta gives the base case, recursive case, and the worker case.
-- the worker case sends an expression of the form (Lambda with@(Atom _) exp) to beta'
beta:: Lexp -> Lexp
beta v@(Atom _) = v
beta (Lambda with@(Atom _) exp) = (Lambda with (beta exp))
beta (Apply exp1 exp2) = let exp1' = beta exp1
                             exp2' = beta exp2
                         in case exp1' of (Lambda with@(Atom _) exp) -> beta (beta' exp with exp2')
                                          _ -> (Apply (eta (beta exp1')) (eta(beta exp2')))

-- beta' substitutes the second expression into the proper bound variable of the first
-- function, if applicable
beta':: Lexp -> Lexp -> Lexp -> Lexp
beta' v@(Atom _) with exp = if v == with then exp else v
beta' (Apply exp1 exp2) with exp = let exp1' = beta' exp1 with exp
                                       exp2' = beta' exp2 with exp
                                        in (Apply exp1' exp2')
beta' (Lambda exp1 exp2) with exp = let exp1' = beta' exp1 with exp
                                        exp2' = beta' exp2 with exp
                                         in (Lambda exp1' exp2') 


-- TODO: Eta-Conversion --------------------------------------------------------------------   
-- \v.(E v) if v == v AND v is not in E -> E, else \v.(E v) 

-- eta recurses on the expression takes care of the following cases:
--    if v == v, we send E and v to eta' for further testing, 
--    else return the same expression, since nothing can be done with it
eta:: Lexp -> Lexp 
eta v@(Atom _) = v --base
eta lexp@(Lambda v1@(Atom _) (Apply exp v2)) -- worker
        | v1 == v2 = eta' lexp v1 
        | otherwise = lexp 
eta lexp@(Lambda (Atom a) exp) = lexp --prune
eta lexp@(Apply exp1 exp2) = (Apply (eta exp1) (eta exp2)) -- recursive

-- eta' detects if there is any instance of v that is free in E, and returns E if there is none, otherwise it returns the same expression
eta':: Lexp -> Lexp -> Lexp
eta' v@(Atom _) bound 
        | bound == v = bound -- don't change it v is free in E
        | otherwise = v
eta' lexp@(Lambda v1@(Atom _) exp@(Apply exp1 v2)) bound
        | (eta' exp1 bound) ==  exp1 = exp1 -- if we get back the same expression we met the conditions
        | otherwise = lexp
eta' (Apply exp1 exp2) bound = let exp1' = beta (eta' exp1 bound) 
                                   exp2' = beta (eta' exp2 bound)
                             in (Apply exp1' exp2')


-- reducers is the function that begins the reduction process
reducers:: Lexp -> Lexp 
reducers lexp = eta (beta (alpha lexp atomList))
                where atomList = [Atom "a", Atom "c", Atom "d", Atom "e", Atom "f", Atom "g", Atom "h", Atom "e", Atom "i", Atom "j",Atom "k", Atom "j"]             
                                

-------------------------- END MY CODE ---------------------------------------


-- Entry point of program
main = do
    putStrLn "Please enter a filename containing lambda expressions:"
    fileName <- getLine
 
    -- id' simply returns its input, so runProgram will result
    -- in printing each lambda expression twice.

    -- TODO: change id' to sth like (alpha' betaReduce etaConvert)	
    runProgram fileName reducers 
  







