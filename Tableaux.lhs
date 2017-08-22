\section{Analytic Tableaux Implementation}

The following section and module document the implementation of the Method of Analytic Tableaux. This is done through the application of Haskell functions to elements of type Form.

\begin{code}
module Tableaux where
import Form
\end{code}

The following "Form Tree" type represents the tree structure that is given by applying the decomposition rules (section 2) to a form. We will need to define some function for taking Form to FTree.

\begin{code}
data FTree = Pair FTree FTree | Split FTree FTree | Leaf (Char,Bool)
\end{code}


We will now begin introducing functions. We will start from the highest level of abstraction, and proceed to introduce functions as they are needed.

Our first function is simple. It evaluates the validity of a Form. We know that we need to compute an FTree given our input, which we will then evaluate to test for validity. We feed our input Form 'f' to some function genFTree, and feed the output to some function evalTree.

\begin{code}
evalForm :: Form -> Bool
evalForm f = (evalTree (genFTree f) [])
\end{code}

Don't be confused by the empty list applied to evalTree; it is a recursive parameter for the function and will be clarified shortly.\\

We now present genFTree, taking Form to FTree. genFTree works inductively over the structure of Form. There is a case for every possible occurance of a binary operator, as well as one for each operator when paired with a NOT.
Each recursive case corresponds to a rule from figure 2.1. Recall splits ($\lor$) and pairs ($\land$) as discussed previously.

\begin{code}
genFTree :: Form -> FTree
genFTree (P v) = Leaf (v, True)
genFTree (Not f) = case f of
  Not f'	-> genFTree f'
  P v	-> Leaf (v, False)
  Bin op f1 f2 -> case op of
    And -> Split (genFTree (Not f1)) (genFTree (Not f2))
    Or 	-> Pair (genFTree (Not f1)) (genFTree (Not f2))
    Implies -> Pair (genFTree f1) (genFTree (Not f2))
    Equiv -> Split 
             (Pair (genFTree (Not f1)) (genFTree f2)) 
             (Pair (genFTree f1) (genFTree (Not f2)))
genFTree (Bin op f1 f2) = case op of
  And -> Pair (genFTree f1) (genFTree f2)
  Or -> Split (genFTree f1) (genFTree f2)
  Implies -> Split (genFTree (Not f1)) (genFTree f2)
  Equiv -> Split
           (Pair (genFTree f1) (genFTree f2))
           (Pair (genFTree (Not f1)) (genFTree (Not f2)))
\end{code}

The following Branch datatype represents the return type of the function $t$ mentioned in section 2. Note that it is a list of (Char,Bool). Each pair holds a propositional symbol with its evaluation as computed by genFTree.
It takes a list of Branches to properly describe an entire FTree, as a Branch is equivalent to one subtree.

\begin{code}
type Branch = [(Char,Bool)]
\end{code}

The upcoming (dense and girthy) function, evalTree, was what we conceptualized in evalForm when we said there would be some function to evaluate the validity of some Form based on its FTree. The function takes an FTree and returns a Bool (denoting validity).

This function is defined inductively over the structure of FTree. It works by adding propositional variables from the FTree into a Branch as it encounters them. This Branch is our second parameter (recall the empty list in evalForm). 

Once the FTree has been followed all the way to its leaves, some function evalBranch is called to evaluate the built up Branch, which is where the return type Bool comes from. evalBranch, once we write it, will correspond to checking a Branch ($t$ output) for contradictions.

When evalTree encounters a Pair ($\land$), it puts the two children into the same Branch. If it encounters a Split ($\lor$), it calls evalTree twice, signifying two seperate Branches. The one exception to this simple scheme lies wherein a Pair is encountered in which both of it's children are Pairs. When this happens we OR four evalTree instances, corresponding to all the possible combinations of children; if a contradiction is found between any of them then a contradiction is found in the original Pair.

\begin{code}
evalTree :: FTree -> Branch -> Bool
evalTree (Pair f t) b
  | (isLeaf f) && (isLeaf t) = evalBranch ((peelLeaf f):(peelLeaf t):b)	
  | (isLeaf f) = evalTree t ((peelLeaf f):b)	
  | (isLeaf t) = evalTree f ((peelLeaf t):b)	
  | (isPair f) && (isPair t) = (evalTree (Pair (digLeft f) (digLeft t)) b) || 
                               (evalTree (Pair (digLeft f) (digRight t)) b) || 
                               (evalTree (Pair (digRight f) (digLeft t)) b) || 
                               (evalTree (Pair (digRight f) (digRight t)) b)	
  | (isSplit f) && (isSplit t) = (evalTree (Pair (digLeft f) (digLeft t)) b) && 
                                 (evalTree (Pair (digLeft f) (digRight t)) b) &&
                                 (evalTree (Pair (digRight f) (digLeft t)) b) &&
                                 (evalTree (Pair (digRight f) (digRight t)) b)	
  | (isPair f) = (evalTree (Pair f (digLeft t)) b) && 
                 (evalTree (Pair f (digRight t)) b)	
  | otherwise = (evalTree (Pair t (digLeft f)) b) && 
                (evalTree (Pair t (digRight f)) b)
evalTree (Split f t) b	
  | (isLeaf f) && (isLeaf t) = evalBranch ((peelLeaf f):b) && 
                               evalBranch ((peelLeaf t):b)	
  | (isLeaf f) = (evalTree t b) && (evalBranch ((peelLeaf f):b))
  | (isLeaf t) = (evalTree f b) && (evalBranch ((peelLeaf t):b))
  | otherwise = (evalTree f b) && (evalTree t b)
evalTree (Leaf (c, n)) b = evalBranch ((c, n):b)
\end{code}

The functions digRight and digLeft are very simple and will be declared shortly. They return either the right or left child (respectively) of an FTree. 

peelLeaf is simply used to access the (Char, Bool) once we reach it in the FTree.\\

And at last, we have reached the final piece of the puzzle. The upcoming function evalBranch, which is called from evalTree for each Branch it generates, takes as input a Branch, and returns a Bool signifying if it found a contradiction.

Because True is returned when a contradiction is found ($p$ and $\neg p$ existing in the same Branch, for any p,) The original function evalForm returns TRUE if the NEGATION of the original formula is valid. This may seem counterintuitive, but changing these return values so deep in the problem results in a cluttering of evalTree's logic. If we wish, we can make adjustments at a higher level.

Stated formally,
\begin{center}
($\forall$ f: Form (evalForm (f) = True $\iff$ isValid($\neg$f))) 
\end{center}

\begin{code}
evalBranch :: Branch -> Bool
evalBranch [] = False
evalBranch (x:xs) = case lookup (fst x) xs of
  Nothing -> evalBranch xs
  Just b -> if b == (snd x)
            then evalBranch xs
            else True       
\end{code}

The following are definitions for each of the auxilliary functions used in evalTree.
The dig functions as well as peelLeaf are defined for every instance of FTree simply for completion's sake, a dig will never be called for a Leaf and peelLeaf will never be called for a Split or a Pair (see evalTree for clarification).

\begin{code}
digLeft :: FTree -> FTree
digLeft (Split f t) = f
digLeft (Pair f t) = f
digLeft (Leaf l) = Leaf l

digRight :: FTree -> FTree
digRight (Split f t) = t
digRight (Pair f t) = t
digRight (Leaf l) = Leaf l

peelLeaf :: FTree -> (Char, Bool)
peelLeaf (Leaf l) = l

isPair :: FTree -> Bool
isPair (Pair _ _) = True
isPair _ = False

isSplit :: FTree -> Bool
isSplit (Split _ _) = True
isSplit _ = False

isLeaf :: FTree -> Bool
isLeaf (Leaf _) = True
isLeaf _ = False
\end{code}

\subsection{Interfacing with Tableaux}

By this point we have constructed enough to (rather ruggedly) determine the validity of propositional forumlas.
To do so, load the module in GHCI (":l Tableaux.lhs") and call evalForm for any of the previously defined Form elements (again, feel free to build your own using the Form constructors).

\subsection{Extending to Satisfiability}

The Tableaux method can be used to derive satisfiability as well as validity. The way this can be accomplished is rather straightforward. If every branch contains a contradiction, the negation of the formula is valid. If no branches contain a contradiction, the formula itself is satisfiable.

The first function we define in this section resolves the previously mentioned counterintuition of evalForm.
It is very simple, and should be self explanatory.

\begin{code}
isValid :: Form -> Bool
isValid f = evalForm (Not f)
\end{code}

This next function, when fed a formula, will return True if it is satisfiable. By extension, a return value of False indicates that the formula is a contradiction.

\begin{code}
isSat :: Form -> Bool
isSat f = evalTreeSat (genFTree f) []
\end{code}

Note that this function is identical to our original evalForm, with the exception that evalTree has been replaced by some new function 'evalTreeSat'. This function will be very close to evalTree, with a few differences.
We will also need to make adjustments to evalBranch.

There are two differences between evalTreeSat and evalTree. The first is that we now use evalBranchSat (defined below) instead of evalBranch. The second is that in the instance where we have a Pair followed by two Pairs we now combine all the cases with AND instead of OR. The reason for this is that in evalTree a contradiction in any of the subcases was enough to satisfy the requirements, but in evalTreeSat EVERY subcase must be free of contradictions.

\begin{code}
evalTreeSat :: FTree -> Branch -> Bool
evalTreeSat (Pair f t) b
  | (isLeaf f) && (isLeaf t) = evalBranchSat ((peelLeaf f):(peelLeaf t):b)
  | (isLeaf f) = evalTreeSat t ((peelLeaf f):b)	
  | (isLeaf t) = evalTreeSat f ((peelLeaf t):b)	
  | (isPair f) && (isPair t) = (evalTreeSat (Pair (digLeft f) (digLeft t)) b) &&
                               (evalTreeSat (Pair (digLeft f) (digRight t)) b) && 
                               (evalTreeSat (Pair (digRight f) (digLeft t)) b) && 
                               (evalTreeSat (Pair (digRight f) (digRight t)) b)	
  | (isSplit f) && (isSplit t) = (evalTreeSat (Pair (digLeft f) (digLeft t)) b) && 
                                 (evalTreeSat (Pair (digLeft f) (digRight t)) b) && 
                                 (evalTreeSat (Pair (digRight f) (digLeft t)) b) && 
                                 (evalTreeSat (Pair (digRight f) (digRight t)) b)
  | (isPair f) = (evalTreeSat (Pair f (digLeft t)) b) && (evalTreeSat (Pair f (digRight t)) b)	
  | otherwise = (evalTreeSat (Pair t (digLeft f)) b) && (evalTreeSat (Pair t (digRight f)) b)
evalTreeSat (Split f t) b
	| (isLeaf f) && (isLeaf t) = evalBranchSat ((peelLeaf f):b) && evalBranchSat ((peelLeaf t):b)
        | (isLeaf f) = (evalTreeSat t b) && (evalBranchSat ((peelLeaf f):b))
	| (isLeaf t) = (evalTreeSat f b) && (evalBranchSat ((peelLeaf t):b))
	| otherwise = (evalTreeSat f b) && (evalTreeSat t b)
evalTreeSat (Leaf (c, n)) b = evalBranchSat ((c, n):b)
\end{code}

Lastly, you will find that the only difference in our new implementation of evalBranch is the replacement of False for True on the empty list pattern and the replacement of True for False when a contradiction is found.
This will return True if there are no contradictions in the Branch.
Paired with our changes to evalTreeSat, isSat can now determine whether a formula is satisfiable.

\begin{code}
evalBranchSat :: Branch -> Bool
evalBranchSat [] = True
evalBranchSat (x:xs) = case lookup (fst x) xs of
  Nothing -> evalBranchSat xs
  Just b -> if b == (snd x)
            then evalBranchSat xs
            else False    
\end{code}

We can now determine whether a propositional formula is valid, satisfiable, or invalid.\\\\

Enjoy.
