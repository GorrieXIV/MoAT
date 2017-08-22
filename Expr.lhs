\section{Simple Arithmetic Expressions}

\begin{code}
module Haskell1.Expr where
\end{code}

A type synonym for readability:
\begin{code}
type Var = String
\end{code}

Instead of the single |Bin| constructor below,
we could have four constructors for the following four binary operators,
and code dealing with these would have large common pieces.
We avoid this duplication by encoding the different binary operators as elements
of a separate datatype:
\begin{code}
data BinOp = Add | Subtract | Mult | Div
\end{code}
An expression is then either a variable, or an integer literal,
or an application of a binary operator two two subexpressions:
\begin{code}
data Expr
  = V Var
  | IntLit Integer
  | Bin BinOp Expr Expr
\end{code}

The expressions ``$5 + 7 * x$'' and ``$5 * (7 + x)$'' are then represented as follows:
\begin{code}
e1, e2  :: Expr
e1 = Bin Add (IntLit 5) (Bin Mult (IntLit 7) (V "x"))
e2 = Bin Mult (IntLit 5) (Bin Add (IntLit 7) (V "x"))
\end{code}

Substititions can be represented as lookup lists:
\begin{code}
type Subst = [(Var, Expr)]

subst :: Subst -> Expr -> Expr
subst = _
\end{code}

Variable assignments can also be represented as lookup lists:
\begin{code}
type Env = [(Var, Integer)]
\end{code}

Expression evaluation can be undefined for different reasons ---
using the following type, we can only throw |error|s in those cases:
\begin{code}
eval0 :: Env -> Expr -> Integer
eval0 env (V v)           = _
eval0 env (IntLit k)      = _
eval0 env (Bin op e1 e2)  = _
\end{code}

We can produce more or less user-friendly and detailed error messages,
and delegating the decision whether to crash to the caller by using for
example the following type:
\begin{code}
eval :: Env -> Expr -> Either String Integer
eval env e = _
\end{code}

\begin{code}
bigExpr :: Int -> Expr
bigExpr 0 = IntLit 42
bigExpr n = Bin Mult e e
  where
    e = bigExpr (n - 1)
\end{code}

It is relatively easy to produce \emph{some} string representation from expressions:
\begin{code}
showExpr :: Expr -> String
showExpr (V v)           = v
showExpr (IntLit k)      = show k
showExpr (Bin op e1 e2)  = parens (showExpr e1 ++ (showOp op ++ showExpr e2))
  where
    parens s = '(' : s ++ ")"

showOp :: BinOp -> String
showOp Add       = "+"
showOp Mult      = "*"
showOp Subtract  = "-"
showOp Div       = "/"
\end{code}
Due to the fact that |(++)| has linear time complexity
in the list length of its first argument, |showExpr| as defined above
has (in general) quadratic complexity in the size of its argument expression,
calculated as the number of constructors.

This can be seen to some extent by timing evaluation of expressions like |length . showExpr $ bigExpr 20|.

The way to avoid this is to switch from generating |String|s
to generate functions of type |ShowS|,
which is understood as the subtype of |String -> String|
that is the range of |(++)| considered as a function from |String|
to |String -> String|,
and, together with that,
also switch from |(++) :: String -> String -> String|
with its linear complexity in its first argument
to |(.) :: ShowS -> ShowS -> ShowS|,
which has constant complexity in both of its arguments.

%{{{ showsOp, showsExpr
\begin{code}
showsOp :: BinOp -> ShowS
showsOp Add       = ('+' :)
showsOp Mult      = ('*' :)
showsOp Subtract  = ('-' :)
showsOp Div       = ('/' :)
\end{code}

\begin{code}
showsExpr :: Expr -> ShowS
showsExpr (V v)           = (v ++)
showsExpr (IntLit k)      = shows k
showsExpr (Bin op e1 e2)  = ('(' :) . showsExpr e1 . showsOp op . showsExpr e2 . (')' :)
\end{code}
%}}}

%{{{ instance Show Expr
The professional way to deal with this is
by appropriately defining the |Show| method |showsPrec|,
which takes an additional precedence argument,
via which generation of superfluous parentheses can be avoided.
\begin{code}
instance Show Expr where
  showsPrec p (V v) = (v ++)
  showsPrec p (IntLit k) = shows k
  showsPrec p (Bin op e1 e2) = (if p > p0 then parens else id) $
      showsPrec p' e1 . showsOp op . showsPrec p'' e2
    where
      parens f =  ('(' :) . f . (')' :)
      p' = _
      p'' = _
      p0 = _
\end{code}
%}}}

\begin{code}
\end{code}

%{{{ EMACS lv
% Local Variables:
% folded-file: t
% fold-internal-margins: 0
% eval: (fold-set-marks "%{{{ " "%}}}")
% eval: (fold-whole-buffer)
% end:
%}}}
