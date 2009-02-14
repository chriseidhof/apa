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

\def\skip{\texttt{skip}}
\def\print{\texttt{print}}
\def\cont{\texttt{continue}}
\def\break{\texttt{break}}


\def\eq{\;\; =  \;\;}

\usepackage{amsmath, amsthm, amssymb}
\def\N{\mathbb{N}}


\begin{document}
\author{Chris Eidhof, Rui S. Barbosa}
\title{Assignment 1}

\maketitle

\section{Part 1}

% TODO ask Jurriaan if he wants the bottom part of table 2.4 as well, or just
% the $f_\ell$

Our analysis for Strongly Live Variables is almost the same as the Live
Variables analysis. We now only present the changes made. We
change our $gen$ function to take an extra argument $l$ of type $L$ which
corresponds to the set of strongly live variables at the exit of the block.
This is needed because variable's strong liveliness before being used in an assignment block
depends on whether the assignmed variable is strongly live at the exit of that block.

$kill$ and $gen$ functions
\begin{align*}
kill_{SLV}([x:=a]^\ell) &  \eq \{x\} \\
kill_{SLV}([\skip]^\ell) & \eq \emptyset\\
kill_{SLV}([b]^\ell) &  \eq \emptyset \\
\\
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
flow of the program). In that case, we are not be interested in any output from the program.


\section{Part 2}



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
 Stmt ::=& \ldots | \\
 ~       & [\print a]^\ell |\\
 ~       & [x_1,\ldots,x_n := a_1,\ldots,a_n]^\ell , n \in \N |\\
 ~       & [\cont]^\ell |\\
 ~       & [\break]^\ell
\end{align*}

Then, for each of those constructs, we describe its semantics as well as the modification that need to be done
to the monotone framework. We also give some example programs in order to demonstrate them.

\subsection{Print}

Informally, the semantics of this construct will be to write the value of arithmetic expression a to an output 
stream. If defining the formal semantics, we could add an extra parameter to the state corresponding to the
list of values \emph{so far} printed. From the Strongly Live Variable's Analysis point of view, the most import thing is
the added constraint that all the variables used in expression $a$ need to be
considered (strongly) live when entering the $[\print a]^\ell$ block. Hence, the $gen$ and $kill$ are extended in this way:

\begin{align*}
kill_{SLV}([\print a]^\ell) &  \eq \emptyset \\
\\
gen_{SLV}([\print a]^\ell,l) & \eq FV(a)
\end{align*}

Note that the new construct provides the program with a new capability of outputing
results. Thus, we are not required to consider $\iota \neq \emptyset$ from now on.

\subsection{Simultaneous Assignements}

%The meaning of multiple assignments is pretty straightforward. Each 

THERE IS A PROBLEM HERE WITH OUR SOLUTION
(until END this is not final)

we should change something. in the case x,y,x := a,b,c the FV(a) should not be generated because attribution is done left to right.

i guess the solution should be 
\[gen([v_1,\ldots,v_n := a_1,\ldots,a_n]^\ell,l) = \bigcup\{FV(a_i) | v_i \in l \wedge \forall j<i. v_i \neq v_j\}\]

rather than the one we had:

\[gen([v_1,\ldots,v_n := a_1,\ldots,a_n]^\ell,l) = \bigcup\{FV(a_i) | v_i \in l\}\]

END

\subsection{Break and Continue}
The meaning of this constructs inside while loops is just what we are used to: $\break$ jumps immediatly off the loop whilst
$\cont$ \emph{restarts} the while loop execution (including the test). 

THERE IS ALSO A PROBLEM HERE
(until END these lines are not final)

because when we had a program with
$[\cont]^{\ell1};[x:=2]^{\ell2}$
(where this were in a while loop) we also considered the flow edge $(\ell1,\ell2)$ which should not 
really be there. To avoid passing an extra parameter (a context) to the flow function the solution
could probably be one of the the following:

1.
we define the flow of a while loop, we catch continues and breaks removing and adding the appropriate edges.
\[flow(\texttt{while}[b]^\ell \texttt{do} S) = flow(S) \cup \{(l,init(S))\} \cup \{(l',l) | l' \in final(S) \cup continues(S)\}) \setminus \{(s,l')| ((s \in continues(S)\cup breaks(S)) and l' \in Blocks(S)\}\]

2. we consider $final([continue]^\ell) = \emptyset$ instead of $l$. In this case, however, the semantics outside a loop would be different: a program would halt immediately.
\[flow(\texttt{while}[b]^\ell \texttt{do} S) = flow(S) \cup \{(l,init(S))\} \cup \{(l',l) | l' \in final(S) \cup continues(S)\}\]

\end{document}


