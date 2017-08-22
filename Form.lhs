\setlength\parindent{0pt}

\section{Form Data Type}

The following section and corresponding module details a data type for expressing propositional formulae. Much of this section is borrowed from or inspired by Dr. Wolfram Kahl's 'Expr' module. 

\begin{code}
module Form where
\end{code}

First we build a free type representing the possible binary operators over propositional formulae. An equivalence operator, 'Equiv', is included for usability's sake, and is not required.

\begin{code}
data BinOp = Implies | Or | And | Equiv
\end{code}

A formula is then one of the following: a propositional symbol (denoted by a single Char), the negation of a formula, or the application of a binary operator applied to two subformula.

\begin{code}
data Form = P Char
          | Not Form  
          | Bin BinOp Form Form
\end{code}

\subsection{Interfacing with Form}

We can represent the following formulas using our type:
\begin{center}
$\neg p \land p$\\
$(p \land q) \Rightarrow q$\\
$\neg (A \Rightarrow B) \Leftrightarrow (\neg B \Rightarrow \neg A)$\\
$\neg ((A \Rightarrow B) \lor (B \Rightarrow A))$
\end{center}
They are, in order, as follows:

\begin{code}
f1, f2, f3, f4  :: Form
f1 = Bin And (Not (P 'p')) (P 'p')
f2 = Bin Implies (Bin And (P 'p') (P 'q')) (P 'q')
f3 = Not (Bin Equiv (Bin Implies (P 'A') (P 'B')) (Bin Implies (Not (P 'B')) (Not (P 'A'))))
f4 = Not (Bin Or (Bin Implies (P 'A') (P 'B')) (Bin Implies (P 'B') (P 'A')))
\end{code}

Note that f1, f3, and f4 are tautologies, and f2 is the same $F$ mentioned in the previous section.\\

The following is a function which returns the string representation of the offered formula.

\begin{code}
showForm :: Form -> ShowS
showForm (P p) = (p :)
showForm (Not f) = ('~' :) . ('(' :) . showForm f . (')' :)
showForm (Bin op f1 f2)  = ('(' :) . showForm f1 . showOp op . showForm f2 . (')' :)
 
showOp :: BinOp -> ShowS
showOp Implies = (" => " ++)
showOp Or = (" | " ++)
showOp And = (" & " ++)
showOp Equiv = (" == " ++)
\end{code}

ShowS is used for efficiencies sake (a simple recursive concatination yields quadratic complexity). Because ShowS is itself a function from String to String, we provide showF which calls showForm with an empty list, yielding the appropriate String.

\begin{code}
showF :: Form -> String
showF f = showForm f []
\end{code}

You can test this module by first loading it in GHCI (":l Form.lhs") and then calling showF on one of f1, f2, f3, f4.

Additionally, you can define your own propositions using the constructor rules of Form ("let f = Not (Bin Or ...)", etc).
