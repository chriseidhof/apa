\documentclass[a4wide,12pt]{article}
%include polycode.fmt 
\usepackage{a4wide}
\usepackage{times}
\usepackage{fancyvrb}
\usepackage{url}
\usepackage{enumerate}
\usepackage{palatino}
\usepackage{rotating}
\usepackage{cp0708t}
 
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
\def\comp{\circ}
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

If we take $\alpha \compose \gamma$ we can verify for all values in $\Zext$ that
this is equal to the identity function. For $\gamma \compose \alpha$ we have
that in the case of $\emptyset$ and a singleton set that it is the identity
function, but when applying $\gamma \compose \alpha$ to a non-empty set with more
than one element we get $\Z$ as a result. Thus, this is clearly not the identity
function, but the relation $gamma \compose \alpha \sqsubseteq id$ holds, which
makes the Galois insertion

\[ start = (\pset{\Z}, \alpha, \gamma, \Zext)\]

We can now apply a number of transformations to end up in our final result. These
transformations preserve the Galois insertion and thus the Galois connection.
Firstly, we will transform $start$ using the total function space combinator,
yielding the next Galois insertion:

\[ (\Varstar \to \pset{\Z}, \alpha^1, \gamma^1, \Varstar \to \Zext) \]. 

Next, we use the Galois Connection combinator from slide 13 in
lecture 13, which gives us the Galois \textbf{connection} 

\[(\pset{\Varstar \to \Z}, \alpha^2, \gamma^2. \Varstar \to \pset{\Z})\] 

From here on it is not a Galois insertion anymore). If we then apply composition we get 

\[(\pset{\Varstar \to \Z}, \alpha^3, \gamma^3, \Varstar \to \Zext)\]

Now we only
have apply the total function space combinator once more to get to our final
result, the Galois connection 

\[(\pset{\Labstar \to \Varstar \to \Z}, \alpha^4, \gamma^4, \Labstar \to \Varstar \to \Zext)\]

\section{II}
We reuse the $\alpha$ and $\beta$ from the previous exercise. Now we have again the
Galois insertion $start$. If we transform using the total function space
combinator, we end up with 

\[(\Varstar \to \pset{\Z}, \alpha^1, \gamma^1, \Varstar \to \Zext)\]

We again use the above combinator from lecture 13 together with composition to
get:

\[(\pset{\Varstar \to \Z}, \alpha^2, \gamma^2, \Varstar \to \Zext)\]

Now we apply the total function space combinator again, yielding:

\[(\Labstar \to (\pset{\Varstar \to \Z}), \alpha^3, \gamma^3, \Labstar \to (\Varstar \to \Zext))\]

Finally, we merge the labels in $\alpha_4$, and we ignore them in $\gamma_4$:

\begin{eqnarray*}
\alpha_4 & f & = \alpha_3 \compose \bigsqcup \{f(l) \mid l \in \Labstar\} \\
\gamma_4 & f & = \gamma_3 \compose \lambda x . f
\end{eqnarray*}

We can show that $\alpha_4 \compose \gamma_4 = id$:

\begin{eqnarray*}
       & (\alpha_4 \compose \gamma_4)\; f \\
       & \text{TODO} \\
\equiv & f
\end{eqnarray*}

$\gamma \compose \alpha \sqsubseteq id$ is straightforward to see (TODO)

Which gives us the final Galois connection:

\[(\Labstar \to (\pset{\Varstar \to \Z}), \alpha^4, \gamma^4, \Varstar \to \Zext)\]

\section{Exercise 4.17}

In this exercise, let $(L_1,\alpha_1,\gamma_1,M_1)$ and 
    $(L_2,\alpha_2,\gamma_2,M_2)$ be Galois insertions.

For the first part, let us define $\alpha$ and $\gamma$ as follows:
\begin{eqnarray*}
\alpha(l_1, l_2) & = & (\alpha_1(l_1), \alpha_2(l_2)) \\
\gamma(m_1, m_2) & = & (\gamma_1(m_1), \gamma_2(m_2))
\end{eqnarray*}

This corresponds to the independent attribute method presented in section 4.1.1 in 
the book. There we can find a proof that
$(L_1 \times L_2,\alpha,\gamma,M_1 \times M_2)$ is a Galois
connection. We now need to show that it is a Galois insertion, that is to say
that $\alpha \comp \gamma = id$. 

As usual, given two functions $f:A \to B$ and $g: C \to D$, the product of
the two functions $f \times g : A \times C \to B \times D$ is
defined as
$(f \times g) (a,c)  = (f(a),g(b))$.
Note that $\times$ is a bifunctor.

Thus, we can rewrite the definitions above as
\begin{eqnarray*}
\alpha & = & \alpha_1 \times \alpha_2 \\
\gamma & = & \gamma_1 \times \gamma_2
\end{eqnarray*}

We use this notation in the proof of the desired property, presented below.
\begin{calculation}
\alpha \comp \gamma 
\just={definition of $\alpha$ and $\gamma$}
(\alpha_1 \times \alpha_2) \comp (\gamma_1 \times \gamma_2)
\just={$\times$ is a bifunctor (preserves composition)}
(\alpha_1 \times \gamma_1) \comp (\alpha_2 \times \gamma_2)
\just={$(L_1,\alpha_1,\gamma_1,M_1)$ and $(L_2,alpha_2,\gamma_2,M_2)$ are Galois insertions}
id \times id
\just={$\times$ is a bifunctor (preserves identity)}
id
\end{calculation}

Let us now turn our attention to the second construction:
\begin{eqnarray*}
\alpha(f) & = & \alpha_2 \compose f \compose \gamma_1 \\
\gamma(g) & = & \gamma_2 \compose g \compose \alpha_1
\end{eqnarray*}

This corresponds to the monotone function space combinator also presented in the
book, where it is proved that
$(L_1 \to L_2,\alpha,\gamma,M_1 \to M_2)$ is a Galois
connection. We now prove that it is also a Galois insertion.

\begin{calculation}
(\alpha \comp \gamma)(g)
\just={definition of $\gamma$}
\alpha(\gamma_2 \comp g \comp \alpha_1)
\just={definition of $\alpha$}
\alpha_2 \comp (\gamma_2 \comp g \comp \alpha_1) \comp \gamma_1
\just={associativity of $\comp$}
(\alpha_2 \comp \gamma_2) \comp g \comp (\alpha_1 \comp \gamma_1)
\just={$(L_1,\alpha_1,\gamma_1,M_1)$ and $(L_2,alpha_2,\gamma_2,M_2)$ are Galois insertions}
id \comp g \comp id
\just={$id$ is unit for $\comp$}
g
\end{calculation}


\section{Mini Project 4.1}
iv: Chris, correction by Rui.

We assume the functions |isSuffix| and |sameHead| on lists (as described in the
book). Now we can compare two lists and return the set $\pset{\{H,S\}}$.
\[cmpParts(x,y) = \{S \mid isSuffix(x,y)\} \cup \{H \mid sameHead(x,y) \]

To compute the range, we first compute the difference (with $diff : (V \times V)
\to \Z$) and then apply the $range$ function as found in the book.

\begin{eqnarray*}
diff & (x,y) & = length(x)-length(y) \\
cmpRange & & = range \compose diff
\end{eqnarray*}

Now we can us the two $cmp$-functions to get our $compare$-function:

\[compare (x, y) = (cmpRange (x,y), cmpRange (x,y))\].

We can lift the function $compare$ to sets, yielding our $\alpha$ function:

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

We also need to show that $\gamma \compose \alpha \sqsupseteq id$. If we expand
the definitions of $\gamma$ we get the following equation:

\[(\gamma \compose \alpha)\; A = \{(x,y) \mid \underline{compare\;(x,y) = z} \land z \in \alpha(A)\}\]

It can easily be seen from the definition of $compare$ that it is a
non-injective function: there are inputs have the same output.  This means that the
pairs $(x,y)$ in the underlined $compare$ function are a superset of $A$. Thus,
the result is a superset of $A$ which means $\gamma \compose \alpha \sqsupseteq id$.

Now we can define our Galois insertion:

\[(\pset{V \times V}, \alpha, \gamma, \pset{\mathbf{LR}})\]

Because a powersets ordered by inclusion is a complete lattice, and we have
shown the necessary properties of $\alpha \compose \gamma$ and $\gamma \compose
\alpha$ we can conclude that this is a valid Galois insertion.

TODO: do we need to prove that $\alpha$ and $\gamma$ are monotone?

\section{Widening Operators}
We need to show that $\nabla$ is a widening operator. This means that the
following two statements should hold:

\begin{itemize}
\item $\nabla$ is an upper-bound operator, i.e. $l_1 \sqsubseteq (l_1 \nabla
l_2) \sqsupseteq l_2$ for all $l_1, l_2 \in L$.
\item for all ascending chains $(l_n)_n$ the ascending chain $(l_n^\nabla)_n$
eventually stabilises.
\end{itemize}

Here, $\nabla$ is defined as:

\begin{eqnarray*}
\bot \nabla X & = & X \\
X   \nabla \bot & = & X \\
\lbrack i_1,j_1 \rbrack  \nabla \lbrack i_2, j_2\rbrack  & = & \lbrack\text{if } i_2 < i_1 \text{ then } -\infty \text { else } i_1, \text{if } j_2 > j_1 \text{ then } \infty \text{ else } j_1\rbrack 
\end{eqnarray*}

\subsection*{Upper-boundness}

We need to show that $l_1 \sqsubseteq (l_1 \nabla l_2) \sqsupseteq l_2$ for all
$l_1, l_2 \in L$. If either $l_1$ or $l_2$ is $\bot$ then it is straightforward
to see that this holds by looking at the definiton of $\nabla$. We now show this for
the cases where both $l_1$ and $l_2$ are not $\bot$.

First, we show that $l_1 \sqsubseteq (l_1 \nabla l_2)$ by using the definition of $\nabla$:

\begin{eqnarray*}
& l_1 & \sqsubseteq (l_1 \nabla l_2) \\
\equiv & \lbrack i_1,j_1 \rbrack & \sqsubseteq (\lbrack i_1,j_1 \rbrack  \nabla \lbrack i_2, j_2\rbrack) \\
\equiv & \lbrack i_1,j_1 \rbrack & \sqsubseteq \lbrack\text{if } i_2 < i_1 \text{ then } -\infty \text { else } i_1, \text{if } j_2 > j_1 \text{ then } \infty \text{ else } j_1\rbrack
\end{eqnarray*}

By definition of $inf$, $sup$ and the partial order on \textbf{Interval} we have to show that

\[       ((\text{if } i_2 < i_1 \text{ then } -\infty \text{ else } i_1) \leq i_1)
   \land (j_1 \leq (\text{if } j_2 > j_1 \text{ then }  \infty \text{ else }
   j_1))
\]

Both parts of the conjunction clearly hold, regardless of the condition. Now we
also need to show that $(l_1 \nabla l_2) \sqsupseteq l_2$, or equivalently, $l_2 \sqsubseteq (l_1 \nabla l_2)$

We will use similar reasoning as above:

\begin{eqnarray*}
& l_2 & \sqsubseteq (l_1 \nabla l_2) \\
\equiv & \lbrack i_2,j_2 \rbrack & \sqsubseteq (\lbrack i_1,j_1 \rbrack  \nabla \lbrack i_2, j_2\rbrack) \\
\equiv & \lbrack i_2,j_2 \rbrack & \sqsubseteq \lbrack\text{if } i_2 < i_1 \text{ then } -\infty \text { else } i_1, \text{if } j_2 > j_1 \text{ then } \infty \text{ else } j_1\rbrack
\end{eqnarray*}

Which leaves us with the need to prove the following:

\[       ((\text{if } i_2 < i_1 \text{ then } -\infty \text{ else } i_1) \leq i_2)
   \land (j_2 \leq (\text{if } j_2 > j_1 \text{ then }  \infty \text{ else }
   j_1))
\]

And again, regardless of the conditions of the \textbf{if}-statement, both conjuncts
will hold. We can now conclude that $l_1 \sqsubseteq (l_1 \nabla l_2)
\sqsupseteq l_2$.

\subsection*{Ascending Chain}


We have to proof that for all ascending chains $(int_n)_n$ the ascending chain $(int_n^\nabla)_n$
eventually stabilises. As in the book, we will show this by contradiction.
Suppose the chain does not stabilize. Then one of the following properties will
hold:

\begin{eqnarray*}
(\forall n : \text{inf}(int^\nabla_n) > -\infty) & \land &
\text{inf}(\bigsqcup_n int^\nabla_n) = -\infty \\
(\forall n : \text{sup}(int^\nabla_n) < \infty) & \land & \text{sup}(\bigsqcup_n int^\nabla_n) =
\infty
\end{eqnarray*}

Without loss of generality we can assume the second property holds. Hence there
must exist an infinite sequence $n_1 < n_2 < \ldots$ such that 

\[\forall i : \infty > \text{sup}(int^\nabla_{n_i + 1}) > \text{sup}(int^\nabla_{n_i}) \]

However, $\text{sup}(int^\nabla_{n_i + 1})$ is defined as
$\text{sup}(int^\nabla_{n_i} \nabla int_{n_{i+1}})$, which is equivalent to 
\[ \text{if } \text{sup}(int_{n_{i+1}}) > \text{sup}(int^\nabla_{n_i}) \text{
  then } \infty \text{ else } \text{sup}(int^\nabla_{n_i}) \]

It can easily be seen that $\text{sup}(int^\nabla_{n_i + 1})$ is
equal to either $\infty$ or $\text{sup}(int^\nabla_{n_i})$, so we have a contradiction.
This proves our original statement that $(int_n^\nabla)_n$ eventually
stabilizes.

\section{Types}
vi: Rui

\end{document}  

