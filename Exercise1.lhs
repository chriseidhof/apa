\documentclass[a4wide,12pt]{article} 
\usepackage{a4wide}
\usepackage{times}
\usepackage{fancyvrb}
\usepackage{url}
\usepackage{enumerate}
\usepackage{palatino}
%include polycode.fmt 
%options ghci
%format  True = "something"

\usepackage{amsmath,amsthm,amssymb}

\def\eq{\;\; =  \;\;}
\def\N{\mathbb{N}}
\def\pset#1{\mathcal{P}(#1)}

\def\Stmt  {\mathbf{Stmt}}
\def\Lab   {\mathbf{Lab}}
\def\Blocks{\mathbf{Blocks}}
\def\Var   {\mathbf{Var}}


\def\skip {\texttt{skip}\     }
\def\while{\texttt{while}\    }
\def\do   {\texttt{do}\       }
\def\ifc  {\texttt{if}\       }
\def\then {\texttt{then}\     }
\def\elsec {\texttt{else}\     }
\def\print{\texttt{print}\    }
\def\cont {\texttt{continue}\ }
\def\break{\texttt{break}\    }

\def\haskell{\textsc{Haskell}}

\begin{document}
\author{Chris Eidhof, Rui S. Barbosa}
\title{Data Flow Assignment I \\ Automatic Program Analysis}

\maketitle

\section{Part 1}

Our analysis for Strongly Live Variables is almost the same as the Live
Variables analysis. We now only present the changes made. We
change our $gen$ function to take an extra argument $l$ of type $L = \pset{\Var_\star}$ which
corresponds to the set of strongly live variables at the exit of the corresponding block.
This is needed because the strong liveliness of a variable before
being used in an assignment block
depends on whether the assigned variable is strongly live at the exit of that block.

\begin{align*}
kill_{SLV} & \; : \; \Blocks_\star \to \pset{\Var_\star} \\
kill_{SLV}([x:=a]^\ell) &  \eq \{x\} \\
kill_{SLV}([\skip]^\ell) & \eq \emptyset\\
kill_{SLV}([b]^\ell) &  \eq \emptyset \\
\\
gen_{SLV} & \; : \; \Blocks_\star \times \pset{\Var_\star} \to \pset{\Var_\star} \\
gen_{SLV}([x:=a]^\ell,l) & \eq
         \begin{cases}
          FV(a) & \text{if $x \in l$} \\
          \emptyset & \text{otherwise}
         \end{cases} \\
gen_{SLV}([\skip]^\ell,l) & \eq \emptyset\\
gen_{SLV}([b]^\ell,l) & \eq FV(b)
\end{align*}

This means our $f_\ell$ also has to change:

\[ f_\ell(l) \eq (l \setminus kill([B]^\ell)) \cup gen([B]^\ell, l) \;\; \text{where} \;\; [B]^\ell
\in blocks(S_\star)
\]

Moreover, in our analysis, $\iota$ will not be necessarily empty. Instead, it will be a chosen set of
variables of interest.
By now, there are no means for a program
to communicate its results besides the inspection of some variables when execution halts: those variables
are what we call the variables of interest at the end of the program. Clearly, 
those varialbes need to be considered live at the exit of all final blocks, so that we can always inspect their values. 
When $\iota$ is empty, the only way to have intermediate
non-empty sets of strongly live variables will be by using them in conditional expressions (which control the
flow of the program). In that case, we are not interested in any output from the program.


\section{Part 2}

DESCRIBE THE HASKELL MODULE(S)

To demonstrate the Strong Live Variable Analysis, we will use our \haskell program
to perform chaotic iteration on the following simple example program

(is it possible to evaluate an haskell expr that gives tex 
and use that tex or do we need to use the format directive? 
i will look on the documentation and ask andres)
% \begin{program}
% & [r := 1]^{1}; \\
% & [a := r * r]^{2}; \\
% & \texttt{while }[y > 1]^{3}; \\
% & ~\;\;[r := r * x]^{4}; \\
% & ~\;\;[y := y - 1]^{5}
% \end{program}

% $ [r := 1]^{1};$ \\
% $ [a := r * r]^{2};$ \\
% $ \texttt{while }[y > 1]^{3};$ \\
% $ ~\;\;[r := r * x]^{4};$ \\
% $ ~\;\;[y := y - 1]^{5}$



\section{Part 3}

We start by adding the new labelled constructs to the abstract syntax of the language:
\begin{align*}
 Stmt ::=  \; &  \ldots                                               \\
 ~    |\;\; & [\print a]^\ell                                      \\
 ~    |\;\; & [x_1,\ldots,x_n := a_1,\ldots,a_n]^\ell , n \in \N   \\
 ~    |\;\; & [\cont]^\ell                                         \\
 ~    |\;\; & [\break]^\ell                                      
\end{align*}

We also extend the $blocks$ function accordingly in a pretty straightforward manner. The definition
of the $labels$ function given in the book stays valid.
\begin{align*}
 blocks([\print a]^\ell)                         & \eq  \{[\print a]^\ell\}                         \\ 
 blocks([x_1,\ldots,x_n := a_1,\ldots,a_n]^\ell) & \eq  \{[x_1,\ldots,x_n := a_1,\ldots,a_n]^\ell\} \\
 blocks([\cont]^\ell)                            & \eq  \{[\cont]^\ell\}                            \\ 
 blocks([\break]^\ell)                           & \eq  \{[\break]^\ell\}
\end{align*}


Then, for each of those constructs, we describe its semantics as well as the modification that need to be done
to the monotone framework, particularly the functions defining the flow control
($init$,$final$ and $flow$) as well as the $gen_{SLV}$ and $kill_{SLV}$ functions
which define our analysis. We also give some example
programs in order to demonstrate the resulting analysis.

\subsection{Print}

Informally, the semantics of this construct will be to write the value of arithmetic expression $a$ to an output 
stream. If defining the formal semantics, we could add an extra parameter to the state corresponding to the
list of values \emph{so far} printed. 

This new construct has no effect on the flow control of the program. Hence, the related functions are
defined just as for the other simple statements (assignments and $\skip$).
\begin{align*}
init([\print a]^\ell)  & =  \ell     \\
final([\print a]^\ell) & =  \{\ell\} \\
flow([\print a]^\ell)  & =  \emptyset
\end{align*}

From the Strongly Live Variable's Analysis point of view, the most import thing is
the added constraint that all the variables used in expression $a$ need to be
considered (strongly) live when entering the $[\print a]^\ell$ block. Hence, the $gen$ and $kill$ are extended in this way:
\begin{align*}
kill_{SLV}([\print a]^\ell) &  \eq \emptyset \\
\\
gen_{SLV}([\print a]^\ell,l) & \eq FV(a)
\end{align*}

Note that the $\print$ construct provides the program with a new capability of conveying
results to the outside. Thus, we are not required to consider $\iota \neq \emptyset$ from now on.
We simply need to print the so called variables of interest at the end of our program.
For example, adding $[\print r]$ or $[\print y]$ at the end of program \ref{exprog1}, 
we get the exact same results we got before.

EXAMPLE PROgrAM and results WIth PRINT

\subsection{Simultaneous Assignements}

The meaning of multiple assignments ($[v_1,\ldots,v_n := a_1,\ldots,a_n]^\ell$) is
pretty straightforward: all expressions in the right hand side are evaluated and
then each of them is assigned to the corresponding variable on the left hand side, in left to right order.

As in the previous case, the flow related functions are easily extended.
\begin{align*}
init ([v_1,\ldots,v_n = a_1,\ldots,a_n]^\ell)  & \eq \ell     \\ 
final([v_1,\ldots,v_n = a_1,\ldots,a_n]^\ell)  & \eq \{\ell\} \\ 
flow ([v_1,\ldots,v_n = a_1,\ldots,a_n]^\ell)  & \eq \emptyset 
\end{align*}

Clearly, as with simple assignements, the $kill$
function gives the variables which are assigned to in $\ell$,
meaning that those variables are not live before the block.

On the other hand, the
$gen$ function needs to give all the variables required to be strongly live before the block. 
As a first guess, we could include all the
free variables used in expressions assigned to variables which are strongly live after the block, closely following the 
single assignement case. However, this solution is not optimal in the case where the $v_i$ are not pairwise distinct.
For example, considering the block $[x,y,x := a,b,c]^\ell$) with $x$ and $y$ strongly live after it, the variables in expression
$a$ need not be considered strongly live before the block, as the attribution $x:=a$ will be immediately overwritten
by $x:=c$ (recall that attributions are done in left to right order).

Therefore, we extend the $kill$ and $gen$ functions in the following manner:
\begin{align*}
kill_{SLV}([v_1,\ldots,v_n := a_1,\ldots,a_n]^\ell) &  \eq \bigcup\{\{v_i\} | 1 \leq i\leq n\} \\
\\
gen_{SLV}([v_1,\ldots,v_n := a_1,\ldots,a_n]^\ell,l) & \eq \bigcup\{FV(a_i) | v_i \in l \wedge \forall j : j<i\,.\, v_j \neq v_i\}
\end{align*}

To demonstrate the use of this construct, let us consider a slightly different version of
\ref{exprog1}, where some assignments have been grouped togheter. Also, some new (dummy)
assignments were added to illustrate the situation when multiple assignments to the same
variable occur.

EXAMPLE PROGRAM

\subsection{Break and Continue}
The meaning of this constructs inside while loops is just what we are used to:
a $\break$ statement cause the program to jump immediatly off the loop whilst
a $\cont$ statement \emph{restarts} the while loop execution (including testing the condition). 
If found outside a while loop, these constructs cause the program to halt in a error state.

The formal semantics could be given in a way ressembling exception handling in Java (where while loops take
the r\^{o}le of catch), as we briefly explain. These constructs
would cause execution to halt in a new kind of state
enclosing the information of the \emph{then current} state along with an annotation of the reason
for halting ($\break$ or $\cont$). 
We would then need several rules to deal with those halting states. 
Firstly, we would need to propagate the halting states unchanged through consecutive statements:
sequencing ($;$) with another statement would ignore that second statement and enclosing
if-then-else constructs would also propagate the halting state (for this second case, the current
rule suffices). Finally, the way to deal with $\while$ should also change. A possibility
would be to change the $[wh_1]$ rule so that it takes a look similar to that of $[seq_1]$.
Then, if the body statement results in a $\cont$-annotated state, the annotation would be dropped
and the while loop reinitiated. If the state was $\break$-annotated, the annotation would also be
dropped but the while loop would be considered finished.

It is worth noticing that if a computation ends in an annotated state and there is
no enclosing while loop, the program itself as a whole would end in that kind of state.
We will consider that to be a error terminating program. Our framework will not consider
the top-level $\break$ and $\cont$ statements to be part of the final statements of
a program. That means that we do not care about the values of the variables of interest
if the program terminates in that way. This will do no harm because these errors could be easily
avoided and a program with those top level construct could be discarded as invalid.
On the other hand, this assumption will facilitate the definition of
the flow related functions: for example, when considering that a $\cont$ statement is not
a final statement of $S_1$, then the definition of $flow(S_1;S_2)$ will not consider the flow
from the $\cont$ statement to $init(S_1)$, just like one would expect. We then need to account
for the existence of these constructs in the definition of the flow information functions
for $\while$ statements (as well as the $\break$ and $\cont$, of course).
Those definitions now read:
\begin{align*}
init ([\cont]^\ell) &\eq \ell      & init ([\break]^\ell) &\eq \ell      \\         
final([\cont]^\ell) &\eq \emptyset & final([\break]^\ell) &\eq \emptyset \\      
flow ([\cont]^\ell) &\eq \emptyset & flow ([\break]^\ell) &\eq \emptyset       
\end{align*}
\begin{eqnarray*}
init (\while [b]^\ell \do S) &\eq& \ell \\
final(\while [b]^\ell \do S) &\eq& \ell \cup breaksOf(S) \\
flow (\while [b]^\ell \do S) &\eq& \{(l,init(S))\} \cup flow(S)              \\
                            &   & \cup\; \{(l',l) | l' \in final(S)\}        \\
                            &   & \cup\; \{(l',l) | l' \in continuesOf(S)\}
\end{eqnarray*}
where $continuesOf$ and $breaksOf$ functions are auxiliary functions which map a given program statement
to the labels of its top level (not nested in a $\while$) $\cont$ and $\break$ statements, respectively:

\begin{align*}
continuesOf & \; : \;  \Stmt \to \pset{\Lab} \\
continuesOf([x:=a]^\ell)                          & \eq \emptyset                              \\
continuesOf([\skip]^\ell)                         & \eq \emptyset                              \\
continuesOf([x_1,\ldots,x_:=a_1,\ldots,a_n]^\ell) & \eq \emptyset                              \\
continuesOf([\print a]^\ell)                      & \eq \emptyset                              \\
continuesOf([\cont]^\ell)                         & \eq \{\ell\}                               \\
continuesOf([\break]^\ell)                        & \eq \emptyset                              \\
continuesOf(S_1;S_2)                              & \eq continuesOf(S_1) \cup continuesOf(S_2) \\
continuesOf(\ifc [b]^\ell \then S_1 \elsec S_2)   & \eq continuesOf(S_1) \cup continuesOf(S_2) \\
continuesOf(\while [b]^\ell \do S)                & \eq \emptyset                              \\
\\
breaksOf & \; : \;  \Stmt \to \pset{\Lab} \\
breaksOf([x:=a]^\ell)                          & \eq \emptyset                              \\
breaksOf([\skip]^\ell)                         & \eq \emptyset                              \\
breaksOf([x_1,\ldots,x_:=a_1,\ldots,a_n]^\ell) & \eq \emptyset                              \\
breaksOf([\print a]^\ell)                      & \eq \emptyset                              \\
breaksOf([\cont]^\ell)                         & \eq \emptyset                              \\
breaksOf([\break]^\ell)                        & \eq \{\ell\}                               \\ 
breaksOf(S_1;S_2)                              & \eq breaksOf(S_1) \cup breaksOf(S_2)       \\
breaksOf(\ifc [b]^\ell \then S_1 \elsec S_2)   & \eq breaksOf(S_1) \cup breaksOf(S_2)        \\
breaksOf(\while [b]^\ell \do S)                & \eq \emptyset                              
\end{align*}

We presented above the changes made on the definition of the flow of the program.
These definitions can be used in all monotone frameworks. As for what concerns specifically
the Strongly Live Variable analysis, these constructs are easy to handle. Actually, they
are just like $\skip$ as they do not change the state of the data whatsoever (in the
semantics informally presented above, the state was freezen with an annotation and then de-annotated
when execution restarts at the right program point. Thus, we have simply:
\begin{align*}
kill_{SLV}([\cont]^\ell)   \eq  kill_{SLV}([\break]^\ell)    & \eq  \emptyset \\
\\                                
gen_{SLV} ([\cont]^\ell,l) \eq  gen_{SLV} ([\break]^\ell,l) & \eq  \emptyset
\end{align*}


%THERE IS ALSO A PROBLEM HERE
%(until END these lines are not final)
%
%because when we had a program with
%$[\cont]^{\ell1};[x:=2]^{\ell2}$
%(where this were in a while loop) we also considered the flow edge $(\ell1,\ell2)$ which should not 
%really be there. To avoid passing an extra parameter (a context) to the flow function the solution
%could probably be one of the the following:
%
%1.
%we define the flow of a while loop, we catch continues and breaks removing and adding the appropriate edges.
%\[flow(\texttt{while}[b]^\ell \texttt{do} S) = flow(S) \cup \{(l,init(S))\} \cup \{(l',l) | l' \in final(S) \cup continues(S)\}) \setminus \{(s,l')| ((s \in continues(S)\cup breaks(S)) and l' \in Blocks(S)\}\]
%
%2. we consider $final([continue]^\ell) = \emptyset$ instead of $l$. In this case, however, the semantics outside a loop would be different: a program would halt immediately.
%\[flow(\texttt{while}[b]^\ell \texttt{do} S) = flow(S) \cup \{(l,init(S))\} \cup \{(l',l) | l' \in final(S) \cup continues(S)\}\]
%

EXAMPLE

\end{document}


