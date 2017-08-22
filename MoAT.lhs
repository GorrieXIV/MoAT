\documentclass[11pt]{article}
\usepackage[hmargin=20mm,vmargin=20mm]{geometry}
\usepackage[round]{natbib}
\usepackage{verbatim}
\usepackage{graphicx}
\newenvironment{code}{\footnotesize\verbatim}{\endverbatim\normalsize}

%include polycode.fmt

% Better for literate programs:
\setlength\parindent{24pt}
\parskip=0.5ex

\usepackage{hyperref} % put this always immediatedly before \begin{document}!
\begin{document}

\begin{center}
\Large{Method of Analytic Tableaux In Haskell}\\
\normalsize {Robert Gorrie -- McMaster University -- \today}\\
\end{center}
\hline
\pagenumbering{arabic}

\section{Introduction}
The following document outlines in detail a Haskell implementation of the Method of Analytic Tableaux. The method in question is a technique and a proof procedure used for checking the validity of a propositional formula. Truthfully, the procedure is a general one, and applies to many different types of logics. The following implementation is, however, only defined over basic propositional logic.

This document is organized as follows. In the first section we offer a brief summary of the Method of Analytic Tableaux; for readers who already understand the procedure, we advise against skipping this section as some of the terminology used will be integral to forthcoming sections. In the following section we introduce our datatypes and functions for defining, building, and interfacing with propositional formulas. Lastly, in the remaining section we build our implementation of the Tableaux method from the ground up, while introducing a new collection interconnected functions.\\\\
This document assumes a beginner level of Haskell comprehension.

\section{Tableaux Method in Brief}

The method is rather simple, and we will attempt to make this as quick and painless as possible.

Abstractedly, the procedure works by decomposing a given formula using a set of rules. These decompositions produce a tree structure in which each leaf is a propositional symbol with an assignment of TRUE or FALSE.

Direct your attention to the figure immediately below. Each operator either splits the tree ($\lor$) or pairs child elements together ($\land$) and is affected by whether a NOT ($\neg$) is applied to the whole of the operation.
\begin{center}
\includegraphics[width=100mm]{TabRules.png}
\end{center}
We will now turn to the following example which will (hopefully) clarify the procedure. In it we consider the propositional formula $(p \land q) \Rightarrow q$ which is a clear tautology.
\begin{center}
\includegraphics[width=100mm]{diagram.png}
\end{center}
We first apply the 'Implies' rule, splitting the tree, then on the left side we apply the 'Not And' rule, splitting the tree again.\\

Let us denote a function $t$ which takes a single formula as a parameter and outputs a list for every subtree.\\
\begin{center}
Let $F = (p \land q) \Rightarrow q$\\
Taking $t (F)$ we get [$\neg p$], [$\neg q$], and [$q$]\\
\end{center}
The final step of the procedure is then to check the subtrees for contradictions. Because we only have splits ($\lor$) and no pairs ($\land$) we are left with the subtrees [$\neg p$], [$\neg q$], and [$q$], each subtree is free of contradictions. If we started with $\neg$(\_ $\Rightarrow$ \_) (instead of (\_ $\Rightarrow$ \_)), we would have resulted with [$p$,$q$,$\neg q$] as our only subtree, which houses a contradiction.\\
If ALL of the produced subtrees possess a contradiction, then the NEGATION of the original formula is valid. Noting this we can confirm that $\neg F$ is valid.

%include Form.lhs
%include Tableaux.lhs

\end{document}
