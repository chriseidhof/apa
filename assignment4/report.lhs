\documentclass[a4wide,12pt]{article}
%include polycode.fmt 
\usepackage{a4wide}
\usepackage{times}
\usepackage{fancyvrb}
\usepackage{url}
\usepackage{enumerate}
\usepackage{palatino}
\usepackage{rotating}
 
%\usepackage{prooftree}

\usepackage{amsmath, amsthm, amssymb}

\theoremstyle{definition}
\newtheorem{defi}{Definition}
\newtheorem{example}{Example}
\newtheorem*{conj}{Conjecture}
\newtheorem*{prob}{Problem}
\newtheorem*{question}{Question}
\theoremstyle{plain} 
\newtheorem{theo}{Theorem}
\newtheorem{prop}[theo]{Proposition}
\newtheorem{lemma}[theo]{Lemma}
\newtheorem{cor}[theo]{Corollary}
\newtheorem*{theo*}{Theorem}
\newtheorem*{prop*}{Proposition}
\newtheorem*{lemma*}{Lemma}
\newtheorem*{cor*}{Corollary}
\theoremstyle{remark}
\newtheorem*{remark}{Remark}
\newtheorem*{notation}{Notation}
\def\qed{\begin{flushright} $\Box$ \end{flushright}}

\newenvironment{prf}
                {\vspace{-2mm} \noindent {\bf Proof.}}
                {\par \nopagebreak \qed }
\newenvironment{namedprf}[1]
                {\noindent {\bf Proof (#1).}}
                {\par \nopagebreak \qed }


\def\logequiv{\Leftrightarrow}

\allowdisplaybreaks[0]
 
\def\eq{\;\; = \;\;}
\def\N{\mathbb{N}}
\def\Z{\mathbb{Z}}
\def\compose{\circ}
\def\Zext{\Z^\top_\bot}
 
\def\pset#1{\mathcal{P}(#1)}
\def\A#1{\mathcal{A}[\hspace{-1pt}[#1]\hspace{-1pt}]}
 
\def\const#1{\mathopen{\langle}#1\mathclose{\rangle}} % <a,b,...z>
\def\pair#1{\const{#1}}
 
\def\Stmt {\mathbf{Stmt}}
\def\Lab {\mathbf{Lab}}
\def\Labstar {\mathbf{Lab_\star}}
\def\Blocks{\mathbf{Blocks}}
\def\Var {\mathbf{Var}}
\def\Varstar {\mathbf{Var_\star}}
 
 
\def\skip {\texttt{skip}\ }
\def\whilel{\texttt{while}\ }
\def\dol {\texttt{do}\ }
\def\ifl {\texttt{if}\ }
\def\thenl {\texttt{then}\ }
\def\elsel {\texttt{else}\ }
\def\printl{\texttt{print}\ }
\def\contl {\texttt{continue}\ }
\def\breakl{\texttt{break}\ }
 
\def\haskell{\textsc{Haskell}}
\def\starto{\overset{\star}{\to}}

\newcounter{Progenvcount}
\setcounter{Progenvcount}{0}
\newenvironment{progenv}
                {\refstepcounter{Progenvcount} \nopagebreak
                 \bigskip\hrule\nopagebreak\medskip\noindent
                 {\bf Program \arabic{Progenvcount}.} \nopagebreak\vspace{0.3cm} \nopagebreak \\ \nopagebreak
                  \nopagebreak
                 $\begin{array}{ll}}
                {\end{array}$ \bigskip\hrule\bigskip\bigskip }


%\def\programold#1{\fbox{\begin{minipage}{0.5\textwidth}\protect{$\begin{array}{ll} #1 \end{array}$}\end{minipage}}}

\def\restabR#1#2[#3]{
\begin{table}
{\scriptsize
\caption{#1}\label{#3}
\begin{center}\begin{sideways}\input{#2}\end{sideways}\end{center}
}
\end{table}}

\def\restabRtiny#1#2[#3]{
\begin{table}
{\tiny
\caption{#1}\label{#3}
\begin{center}\begin{sideways}\input{#2}\end{sideways}\end{center}
}
\end{table}}


\def\restab#1#2[#3]{
\begin{table}
\caption{#1}\label{#3}
\begin{center}\input{#2}\end{center}
\end{table}}


\def\program#1[#2]{\begin{progenv}\label{#2}\input{#1}\end{progenv}}

\def\Tiny{\fontsize{3pt}{3pt}\selectfont}
\def\hs#1{\texttt{#1}}
 
\begin{document}
\author{Chris Eidhof, Rui S. Barbosa}
\title{Abstract Interpretation Assignment \\ Automatic Program Analysis}
 
\maketitle

\section{Galois Connections}

We start out by giving a pair of functions $\alpha, \gamma$ that transform
$\pset{\Z}$ into $\Zext$ and the other way around:

\begin{eqnarray*}
\alpha & \emptyset & = \bot \\
\alpha & \{n\} & = n \\
\alpha & otherwise & = \top \\
& & \\
\gamma & \bot & = \emptyset \\
\gamma & n & = \{n\} \\
\gamma & \top & = \Z 
\end{eqnarray*}

TODO chris: step by step.
TODO rui/chris: did it preserve galois insertion?

If we take $\alpha \compose \gamma$ we can verify for all values in $\Zext$ that
this is equal to the identity function. For $\gamma \compose \alpha$ we have
that in the case of $\emptyset$ and a singleton set that it is the identity
function, but when applying $\gamma \compose \alpha$ to a non-empty set with more
than one element we get $\Z$ as a result. Thus, this is clearly not the identity
function, but the relation $gamma \compose \alpha \sqsubseteq id$ holds, which
makes $start = (\pset{\Z}, \alpha, \gamma, \Zext)$ a Galois insertion.

We can now apply a number of transformations to end up in our final result. These
transformations preserve the Galois insertion and thus the Galois connection.
Firstly, we will transform $start$ using the total function space combinator,
yielding the Galois insertion $(\Varstar \to \pset{\Z}, \alpha^1, \gamma^1,
\Varstar \to \Zext)$. Next, we use the Galois Connection from slide 13 in
lecture 13, which gives us $(\pset{\Varstar \to \Z}, \alpha^2, \gamma^2,
\Varstar \to \pset{\Z})$. If we then apply composition we get 
$(\pset{\Varstar \to \Z}, \alpha^3, \gamma^3, \Varstar \to \Zext)$. Now we only
have apply the total function space combinator once more to get to our final
result, $(\pset{\Labstar \to \Varstar \to \Z}, \alpha^4, \gamma^4, \Labstar \to \Varstar \to \Zext)$

\section{II}
ii: Together
\section{III}
iii: Rui

\section{Mini Project 4.1}
iv: Chris, correction by Rui.

We assume the functions |isSuffix| and |sameHead| on lists (as described in the
book). Now we can compare two lists and return the set $\pset{\{H,S\}}$.
\[cmpParts(x,y) = \{S \mid isSuffix(x,y)\} \cup \{H \mid sameHead(x,y) \]

To compute the range, we first compute the difference (with $diff : (V \prod V)
\to \Z$) and then apply the $range$ function as found in the book.

\begin{eqnarray*}
diff & (x,y) & = length(x)-length(y) \\
cmpRange & & = range \compose diff
\end{eqnarray*}

Now we can us the two $cmp$-functions to get our $compare$-function:

\[compare (x, y) = (cmpRange (x,y), cmpRange (x,y))\].

We can lift the function $compare$ to sets, yielding our $\alpha$ function:

TODO: do we need $forall$-quantification here?

\[\alpha\;A = \{compare\;v \mid v \in A\}\]

We can then define $\gamma$ by inverting $compare$:

\[\gamma\;B = \{(x,y) \mid compare\;(x,y) = b \land b \in B\}\]

For a Galois insertion, it is required that $\alpha \compose \gamma$ is the
identity function. We show this by equational reasoning:
\begin{eqnarray*}
       & (\alpha \compose \gamma)\; B \\
\equiv & (\{compare\;v \mid v \in \{(x,y) \land compare\;(x,y) = b \mid b \in
B\}\})\; B  \\
\equiv & (\{compare\;(x,y) \mid compare\;(x,y) = b \land b \in B\})\; B \\
\equiv & (\{b \mid b \in B\})\; B \\
\equiv & B \\
\end{eqnarray*}

(TODO: is non-surjectivity a problem for $\alpha \compose \gamma$?). 

We also need to show that $\gamma \compose \alpha \sqsupseteq id$. If we expand
the definitions of $\gamma$ we get the following equation:

\[(\gamma \compose \alpha)\; A = \{(x,y) \mid \underline{compare\;(x,y) = z} \land z \in \alpha(A)\}\]

It can easily be seen from the definition of $compare$ that it is a
non-injective function: there are inputs have the same output.  This means that the
pairs $(x,y)$ in the underlined $compare$ function are a superset of $A$. Thus,
the result is a superset of $A$ which gives us $\gamma \compose \alpha \sqsupseteq id$.
(TODO: better explanation).

Now we can define our Galois insertion:

\[\pset{V \times V}, \alpha, \gamma, \pset{\mathbf{LR}}\]

Because a powersets ordered by inclusion is a complete lattice, and we have
shown the necessary properties of $\alpha \compose \gamma$ and $\gamma \compose
\alpha$ we can conclude that this is a valid Galois insertion.

TODO: do we need to prove that $\alpha$ and $\gamma$ are monotone?

\section{Widening Operators}
v: Chris

\section{Types}
vi: Rui

\end{document}  

