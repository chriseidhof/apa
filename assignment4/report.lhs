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

\allowdisplaybreaks[3]
 
\def\eq{\;\; = \;\;}
\def\N{\mathbb{N}}
\def\Z{\mathbb{Z}}
\def\T{\mathbb{T}}
\def\compose{\circ}
\def\comp{\circ}
\def\Zext{\Z^\top_\bot}
 
\def\pset#1{\mathcal{P}(#1)}
\def\A#1{\mathcal{A}[\hspace{-1pt}[#1]\hspace{-1pt}]}
\def\typeof#1#2{\mathcal{T}_{#1}[\hspace{-1pt}[#2]\hspace{-1pt}]}
\def\typeofT{\mathcal{T}}
 


\def\const#1{\mathopen{\langle}#1\mathclose{\rangle}} % <a,b,...z>
\def\pair#1{\const{#1}}
 
\def\Stmt {\mathbf{Stmt}}
\def\Lab {\mathbf{Lab}}
\def\Labstar {\mathbf{Lab_\star}}
\def\Blocks{\mathbf{Blocks}}
\def\Var {\mathbf{Var}}
\def\Varstar {\mathbf{Var_\star}}
\def\Ctxt {\mathbf{Ctxt}}
\def\AExp {\mathbf{AExp}}


 
\def\While{\textsc{While}\ } 
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

\section{Galois Connection: Constant Propagation}

We start out by giving a pair of functions $\alpha$ and  $\gamma$ that transform
$\pset{\Z}$ into $\Zext$ and the other way around:

\begin{eqnarray*}
\alpha :& \pset{\Z}& \to \Zext \\
\alpha & \emptyset & = \bot \\
\alpha & \{n\}     & = n \\
\alpha & X         & = \top \;\;\text{if } ||X||\geq 2 \\
& & \\
\gamma :& \Zext &\to \pset{\Z} \\
\gamma & \bot & = \emptyset \\
\gamma & n    & = \{n\} \\
\gamma & \top & = \Z 
\end{eqnarray*}

We can easily verify that $\alpha \compose \gamma$ is the identity function: just
consider all the cases in $\Zext$.
For $\gamma \compose \alpha$, 
in the case the argument is $\emptyset$ or a singleton set, this acts as the identity
function. But when applying $\gamma \compose \alpha$ to a non-empty set with more
than one element we get $\Z$ as a result. Anyway, 
$\gamma \compose \alpha \sqsubseteq id$ holds.
This means that 
\[ start = (\pset{\Z}, \alpha, \gamma, \Zext)\]
is a Galois insertion.

We can now apply a number of combinators to end up with our final Galois connection.

Firstly, we will transform $start$ using the total function space combinator,
yielding the next Galois connection (which is still also an insertion):
\[ (\Varstar \to \pset{\Z}, \alpha_1, \gamma_1, \Varstar \to \Zext) \]
where $\alpha_1(f) = \alpha \compose f$ and $\gamma_1(g) = \gamma \compose g$.

Next, we reuse the Galois connection from slide 13 in
lecture 13:
\[(\pset{\Varstar \to \Z}, \alpha_2, \gamma_2. \Varstar \to \pset{\Z})\] 
(Note that this is not a Galois insertion anymore).

If we now compose the last two connections, we get 
\[(\pset{\Varstar \to \Z}, \alpha_3, \gamma_3, \Varstar \to \Zext)\]
where $\alpha_3 = \alpha_1 \compose \alpha_2$ and
$\gamma_3 = \gamma_2 \compose \gamma_1$.

Now we only
have apply the total function space combinator once more to get to our final
result, the Galois connection 
\[(\Labstar \to \pset{\Varstar \to \Z}, \alpha_4, \gamma_4, \Labstar \to \Varstar \to \Zext)\]

We note that this connection is not a Galois insertion. The case where it fails has
to do with undefinedness. Let us fix a label: basically we are considering 
the Galois connection $(\alpha_3,\gamma_3)$.
In the abstract lattice, we can have $\bot$
associated to a variable. This situation can only arise from a $\emptyset$ in
the concrete lattice. In that case,
$\alpha(\emptyset)$ yields a function associating $\bot$
to every variable. Thus a function associating $\bot$ just to some variables
never arises as a result of $\alpha$. So, taking such a function $f$, we have
$(\alpha_3 \compose \gamma_3)(f) \neq f$.

We shall now illustrate the Galois connection with an example.
Let us consider two labels and three variables and the function of type
$\Labstar \to \pset{\Varstar \to \Z}$ given by the table below:
\begin{eqnarray*}
1 & \{[x \mapsto 1, y \mapsto 2, z \mapsto 4],[x \mapsto 1, y \mapsto 2, x\mapsto 5]\} \\
2 & \{[x \mapsto 1, y \mapsto 3, z \mapsto 4]\}
\end{eqnarray*}
Applying the abstraction function $\alpha_4$, we obtain the function described in the table below 
\begin{eqnarray*}
1 & [x \mapsto 1, y \mapsto 2, z \mapsto \top] \\
2 & [x \mapsto 1, y \mapsto 3, z \mapsto 4]
\end{eqnarray*}
Note that in the second label, all the variables are attributed a (constant) value
because there was only one possible function. As for the first label, only the variable $z$
gets mapped to $\top$ because it is the only one that does not have a constant
value for all possible functions in that label. 

Applying now $\gamma$ to this function, we get a function that, to each
label, associates the set of functions
that could have resulted in this. In this case, we have
\begin{eqnarray*}
1 & \{[x\mapsto1,y\mapsto 3,z\mapsto 4]\} \\
2 & \{[x\mapsto 1,y\mapsto 2, z\mapsto k] \mid k \in \Z\}
\end{eqnarray*}

\section{Galois Connection: Constant Variable}
%We reuse the $\alpha$ and $\beta$ from the previous exercise. Now we have again the
%Galois insertion $start$. If we transform using the total function space
%combinator, we end up with 
%
%\[(\Varstar \to \pset{\Z}, \alpha^1, \gamma^1, \Varstar \to \Zext)\]
%
%We again use the above combinator from lecture 13 together with composition to
%get:
%
%\[(\pset{\Varstar \to \Z}, \alpha^2, \gamma^2, \Varstar \to \Zext)\]
%
%Now we apply the total function space combinator again, yielding:
%
%\[(\Labstar \to (\pset{\Varstar \to \Z}), \alpha^3, \gamma^3, \Labstar \to (\Varstar \to \Zext))\]
%
We reuse the Galois connection constructed in the previous exercise.
\[(\Labstar \to \pset{\Varstar \to \Z}, \alpha_4, \gamma_4, \Labstar \to (\Varstar \to \Zext))\]

Basically, we now forget the separation of information per label. We construct
a new Galois connection
\[(\Labstar \to \pset{\Varstar \to \Z}, \alpha_5, \gamma_5, \Varstar \to \Zext)\]

Finally, we merge the information for all labels in $\alpha_5$,
and replicate the same (safe) information to every label in $\gamma_5$:

\begin{align*}
\alpha_5\, (f)  =& \bigsqcup \{\alpha_4(f)(l) \mid l \in \Labstar\} \\
\gamma_5\, (g)  =& \gamma_4  (\lambda \ell . g)
\end{align*}

The join operation above is done in the lattice $\Varstar \to \Zext$.
It corresponds to do the join (in $\Zext$) in a point wise manner.


%We can show that $\alpha_4 \compose \gamma_4 = id$:
%
%\begin{eqnarray*}
%       & (\alpha_4 \compose \gamma_4)\; f \\
%       & \text{TODO} \\
%\equiv & f
%\end{eqnarray*}
%
%$\gamma \compose \alpha \sqsubseteq id$ is straightforward to see (TODO)
%
%Which gives us the final Galois connection:
%
%\[(\Labstar \to (\pset{\Varstar \to \Z}), \alpha^4, \gamma^4, \Varstar \to \Zext)\]

Considering again the same example, the result of applying $\alpha_5$ is now the function
\[[x\mapsto 1, y\mapsto \top, z \mapsto \top]\]
In this case, $y$ is mapped to $\top$ because it has a different (yet constant) value
in each program label. So, only $x$ is mapped to a constant as it is the only variable
maintaining a constant value throughout a program.

Applying $\gamma_5$ back, we loose all the specific information for each label (all get
poisoned).
\begin{eqnarray*}
1 & \{[x\mapsto 1,y\mapsto k_1, z\mapsto k_2] \mid k_1,k_2 \in \Z\} \\
2 & \{[x\mapsto 1,y\mapsto k_3, z\mapsto k_4] \mid k_3,k_4 \in \Z\}
\end{eqnarray*}

\section{Galois Insertions: Product and Function (Exercise 4.17)}

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


\section{Galois Insertion for Lists (Mini Project 4.1)}
We will make use of the following general construction.
Given a function $f: A \to B$, we define the images
and inverse images of subsets of $A$ and $B$, respectively, as
\begin{align*}
f^{\rightarrow} : \pset{A} &\to \pset{B} \\
f^{\rightarrow}(X) &= \{f (x) \mid x \in X\} \\
~\\
f^{\leftarrow} : \pset{B} &\to \pset{A} \\
f^{\leftarrow}(Y) &= \{x  \mid f (x) \in Y \} \\
\end{align*} 

We now show that these two functions form a Galois connection
\[(\pset{A},f^{\rightarrow},f^\leftarrow,\pset{B})\]
between the power sets ordered by inclusion.
It is straightforward to show that they are monotone. As for the other conditions, we have:
\begin{calculation}
(f^\rightarrow \compose f^\leftarrow) (Y) 
\just\equiv{definition of $f^\leftarrow$}
f^\rightarrow \{x  \mid f (x) \in Y\}
\just\equiv{definition of $f^\rightarrow$}
\{f(k) \mid k \in \{x  \mid f (x) \in Y\}\}
\just\equiv{definition of set comprehension}
\{f(k) \mid k \in A ,  f (k) \in Y\}
\just\subseteq{set comprehension condition}
Y
\end{calculation}
\begin{calculation}
(f^\leftarrow \compose f^\rightarrow) (X) 
\just\equiv{definition of $f^\rightarrow$}
f^\leftarrow \{f(x)  \mid x \in X\}
\just\equiv{definition of $f^\leftarrow$}
\{s \mid f(s) \in \{f(x)  \mid x \in X\}\}
\just\equiv{definition of set comprehension}
\{s \mid \exists x \in X . f(s) = f(x)\}
\just\supseteq{set comprehension condition is trivially satisfied for $s \in X$}
X
\end{calculation}

Moreover, note that if $f$ is surjective (onto), then
the last step in the first derivation is an equality and we
so we have a Galois insertion. By the way, if the function is injective (one-to-one),
then we would have equality in the second condition.

The aim of this exercise is to construct a Galois insertion of this kind:
\[(\pset{V \times V}, \alpha, \gamma, \pset{\mathbf{LR}})\]
We will do this by defining a function
\[compare : V \times V \to \mathbf{LR}\]
and then lift it to power sets using the construction above. 
To guarantee that we end up with a Galois insertion, we just need to show that $compare$ is
surjective.

We use the predicates |sameHead|
and |isSuffix| on
pairs of lists, that say whether two
lists share the same head and whether one is a suffix of the other.
We assume that this predicates are defined as described in the
book.



Now we can compare two lists and return a subset of $\{H,S\}$.
\begin{align*}
cmpParts &: V \times V  \to \pset{\{S,H\}} \\
cmpParts(x,y) &= \{S \mid isSuffix(x,y)\} \cup \{H \mid sameHead(x,y)\}\\ 
\end{align*}

To compute the range, we first compute the difference
and then apply the $range$ function as found in the book.
\begin{align*}
diff &: V \times V \to \Z \\
diff  (x,y) & = length(x)-length(y) \\
~\\
cmpRange &: V \times V \to \mathbf{Range} \\
cmpRange  & = range \compose diff
\end{align*}

Now we can use the two $cmp$-functions to get our $compare$-function:
\begin{align*}
compare &: V \times V \to \pset{\{S,H\}} \times \mathbf{Range}\\
compare  &= \pair{cmpParts, cmpRange}
\end{align*}

Using the construction described in the beginning of the
exercise, we now have a Galois connection. We now just need to show that
compare is surjective to guarantee that we have an insertion.
For that, we need to show that every value in the codomain may be reached through the
function. That amounts to find a pair of list that results
in each possible combination of $S$, $H$ and $\mathbf{Range}$.
Let us first consider the case when we have $\emptyset$ in the first component of the resulting pair.
This means the lists do not have the same head and are not a suffix of one another.
For that, we just need to choose a pair of lists with no shared elements and it is obvious
that we can make the difference between the length of such lists to be any integer
we want (so we have all the values for $\mathbf{Range}$). For the case we have $\{H\}$, we
just consider the same kind of pairs of lists
with an added common element (different from all the others) as head. If the lists
have at least one more element each (and still requiring that the set of elements
of the two lists is disjunct, except for the head), we guarantee that we do not have
a common suffix and we still can of course make the difference between the lengths
to be any integer we want. The case $\{S\}$ is also easy: just take one of the lists to
be empty. They certainly will not have the same head and the empty list will be a suffix
of the other one. Once again, the difference can be made anything we want (positive if the second is empty or negative if the first is, zero if both are). The last case is when we have
$\{H,S\}$. We can choose one of the lists to have only one element and the other one to
have that same element as first and last element. Then both lists share the head and
one is a length-one suffix of the other. We can still choose any range: take the first
list to have one element for negative values, the second for positive and both for
zero difference.
So, the function $compare$ is definitively surjective.

We can then conclude that
\[(\pset{V \times V}, compare^\rightarrow, compare^\leftarrow, \pset{\pset{\{H,S\}} \times \mathbf{Range}})\]
is a Galois insertion.

%We can lift the function $compare$ to sets, yielding our $\alpha$ function:
%
%\[\alpha\;A = \{compare\;v \mid v \in A\}\]
%
%We can then define $\gamma$ by inverting $compare$:
%
%\[\gamma\;B = \{(x,y) \mid compare\;(x,y) = b \land b \in B\}\]
%
%
%For a Galois insertion, it is required that $\alpha \compose \gamma$ is the
%identity function. We show this by equational reasoning:
%\begin{eqnarray*}
%       & (\alpha \compose \gamma)\; B \\
%\equiv & (\{compare\;v \mid v \in \{(x,y) \land compare\;(x,y) = b \mid b \in
%B\}\})\; B  \\
%\equiv & (\{compare\;(x,y) \mid compare\;(x,y) = b \land b \in B\})\; B \\
%\equiv & (\{b \mid b \in B\})\; B \\
%\equiv & B \\
%\end{eqnarray*}
%
%
%We also need to show that $\gamma \compose \alpha \sqsupseteq id$. If we expand
%the definitions of $\gamma$ we get the following equation:
%
%\[(\gamma \compose \alpha)\; A = \{(x,y) \mid \underline{compare\;(x,y) = z} \land z \in \alpha(A)\}\]
%
%It can easily be seen from the definition of $compare$ that it is a
%non-injective function: there are inputs have the same output.  This means that the
%pairs $(x,y)$ in the underlined $compare$ function are a superset of $A$. Thus,
%the result is a superset of $A$ which means $\gamma \compose \alpha \sqsupseteq id$.
%
%Now we can define our Galois insertion:
%
%\[(\pset{V \times V}, \alpha, \gamma, \pset{\mathbf{LR}})\]
%
%Because a powersets ordered by inclusion is a complete lattice, and we have
%shown the necessary properties of $\alpha \compose \gamma$ and $\gamma \compose
%\alpha$ we can conclude that this is a valid Galois insertion.
%
%TODO: do we need to prove that $\alpha$ and $\gamma$ are monotone?

\section{Widening Operator for Intervals}
We need to show that $\nabla$ is a widening operator. This means that the
following two statements should hold:

\begin{itemize}
\item $\nabla$ is an upper-bound operator, i.e. $l_1 \sqcup l_2 \sqsubseteq l_1 \nabla
l_2$ for all $l_1, l_2 \in L$.
\item for all ascending chains $(l_n)_n$, the ascending chain $(l_n^\nabla)_n$
eventually stabilises.
\end{itemize}

Here, $\nabla$ is defined as:

\begin{eqnarray*}
\bot \nabla X & = & X \\
X   \nabla \bot & = & X \\
\lbrack i_1,j_1 \rbrack  \nabla \lbrack i_2, j_2\rbrack  & = & \lbrack\text{if } i_2 < i_1 \text{ then } -\infty \text { else } i_1, \text{if } j_2 > j_1 \text{ then } \infty \text{ else } j_1\rbrack 
\end{eqnarray*}

\subsection*{Upper-boundness}

For the first condition, we simply need
to show that $l_1 \sqsubseteq l_1 \nabla l_2 \land l_2 \sqsubseteq l_1 \nabla l_2$ for all
$l_1, l_2 \in L$.

If either $l_1$ or $l_2$ is $\bot$ then it is straightforward
to see that this holds by looking at the definition of $\nabla$. We now show this for
the cases where both $l_1$ and $l_2$ are not $\bot$.

First, we show that $l_1 \sqsubseteq l_1 \nabla l_2$:

\begin{calculation}
l_1  \sqsubseteq l_1 \nabla l_2 
\just\equiv{$l_1 = [i_1,j_1]$, $l_2 = [i_2,j_2]$}
\lbrack i_1,j_1 \rbrack  \sqsubseteq \lbrack i_1,j_1 \rbrack  \nabla \lbrack i_2, j_2\rbrack 
\just\equiv{definition of $\nabla$}
\lbrack i_1,j_1 \rbrack \sqsubseteq \lbrack\text{if } i_2 < i_1 \text{ then } -\infty \text { else } i_1, \text{if } j_2 > j_1 \text{ then } \infty \text{ else } j_1\rbrack
\just\equiv{definition of $\sqsubseteq$}
(\text{if } i_2 < i_1 \text{ then } -\infty \text{ else } i_1) \leq i_1
\;   \land \; j_1 \leq (\text{if } j_2 > j_1 \text{ then }  \infty \text{ else }
   j_1)
\just\equiv{$-\infty \leq i_1 \land i_1 \leq i_1$(left) and $j_1 \leq \infty \land j_1 \leq j_1$(right)}
true \land true
\just\equiv{boolean arithmetic}
true
\end{calculation}

Now, we show that $l_2 \sqsubseteq (l_1 \nabla l_2)$ in a similar fashion:
\begin{calculation}
l_2  \sqsubseteq l_1 \nabla l_2 
\just\equiv{$l_1 = [i_1,j_1]$, $l_2 = [i_2,j_2]$}
\lbrack i_2,j_2 \rbrack  \sqsubseteq \lbrack i_1,j_1 \rbrack  \nabla \lbrack i_2, j_2\rbrack 
\just\equiv{definition of $\nabla$}
\lbrack i_2,j_2 \rbrack \sqsubseteq \lbrack\text{if } i_2 < i_1 \text{ then } -\infty \text { else } i_1, \text{if } j_2 > j_1 \text{ then } \infty \text{ else } j_1\rbrack
\just\equiv{definition of $\sqsubseteq$}
\begin{cases}
(\text{if } i_2 < i_1 \text{ then } -\infty \text{ else } i_1) \leq i_2 \\
j_2 \leq (\text{if } j_2 > j_1 \text{ then }  \infty \text{ else } j_1)
\end{cases}
\just\equiv{translating conditional to implication}
\begin{cases}
(i_2 < i_1) \implies -\infty \leq i_2 \; &\land \; \neg(i_2 < i_1) \implies i_1 \leq i_2 \\
(j_2 > j_1) \implies j_2 \leq  \infty \; &\land \; \neg(j_2 > j_1) \implies j_2 \leq j_1)
\end{cases}
\just\equiv{negation of $<$ is $\geq$}
\begin{cases}
(i_2 < i_1) \implies -\infty \leq i_2 \; &\land \; i_1 \leq i_2 \implies i_1 \leq i_2 \\
(j_2 > j_1) \implies j_2 \leq  \infty \; &\land \; j_2 \leq j_1 \implies j_2 \leq j_1)
\end{cases}
\just\equiv{$a \leq \infty$ and $p \implies p$}
true
\end{calculation}

We can now conclude that $l_1 \sqsubseteq (l_1 \nabla l_2)
\sqsupseteq l_2$, which is equivalent to $l_1 \sqcup l2 \sqsubseteq l_1 \nabla l_2$.

\subsection*{Finite Ascending Chain}


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

Without loss of generality we can assume the second property holds (The other case is dual).
This means that the upper bound of the interval keeps increasing without ever reaching infinity.
That means that we can choose an infinite sequence $n_1 < n_2 < \ldots$ such that 
\begin{equation}\label{sups}
\forall i : \text{sup}(int^\nabla_{n_i}) < \text{sup}(int^\nabla_{n_i + 1}) < \infty 
\end{equation}
(This is done by choosing the $n_i$'s where the upper bound of the interval changes)

However, $\text{sup}(int^\nabla_{n_i + 1})$ is defined as
$\text{sup}(int^\nabla_{n_i} \nabla int_{n_{i+1}})$, which is equal equal to
\[ \text{if } \text{sup}(int_{n_{i+1}}) > \text{sup}(int^\nabla_{n_i}) \text{
  then } \infty \text{ else } \text{sup}(int^\nabla_{n_i}) \]

It can easily be seen that $\text{sup}(int^\nabla_{n_i + 1})$ is
equal to either $\infty$ or $\text{sup}(int^\nabla_{n_i})$, contradicting
relation \ref{sups}.

This proves our original statement that $(int_n^\nabla)_n$ eventually
stabilizes.

\section{Soft Typing}
For this exercise, we consider the \While language extended with
types. We consider an infinite (countable) set of types 
$\T = \{t_k \mid k \in \N\}$. This type system is completely flat,
meaning that no two types are unifiable. However, operators can be overloaded
(ad-hoc polymorphism). We allow this because otherwise we would never get ascending chains when performing the analysis.

The language considered is dynamically typed, meaning that a variable
may store values of several different types during its lifetime.
The analysis we develop is intended to record which types a variable might
have at each point of the program.
We need to account for possible bad uses of a variable. For example, if the operator
$+$ is applied to a variable storing a boolean, the resulting expression can not be typed.
We will overcome this partiality by saying that this expression has type $\bot$.
We will write $\T_\bot$ for $\T \cup \{\bot\}$.

More formally, each constant $c$ accepted in the language
is given a type $\tau_c \in \T$ and each (binary) 
operator $op$ has a typing function $\tau_op$ which given the types of the two arguments
$\tau_{op}^1$ and $\tau_{op}^2$, gives the type of the result.
which might also be $\bot$
meaning that the operator is not defined for such input types.
(We could easily consider operators
with other arities in a similar fashion). 

At each program point, we then have a context $\Gamma : \Var_\star \to \pset{\T_\bot}$ 
which associates to each variable in the program the types it might have at that point.
We will write $\Ctxt$ for $\Var_\star \to \pset{\T_\bot}$.
Given such a context, we can always compute the possible types of an expression $a$.
The following function describes how we do so.
\begin{align*}
\typeofT : \Ctxt &\to \AExp \to \pset{\T_\bot} \\
\typeof{\Gamma}{x} &= \Gamma (x) \\
\typeof{\Gamma}{c} &= \{\tau_c\} \\
\typeof{\Gamma}{a_1\, op\, a_2} &= 
     \{\tau_{op} (x,y) \mid x \in  \typeof{\Gamma}{a_1}, y \in \typeof{\Gamma}{a_2}\} \\
\end{align*}

$\Ctxt$ is the lattice in our monotone framework (with the
obvious point wise set union as the join operation).
We now give the transfer
functions.
\begin{align*}
f_\ell(\Gamma) = \begin{cases}
\Gamma & \text{if}\; [skip]^\ell \in \Blocks_\star \\
\Gamma[x\mapsto \typeof{\Gamma}{a}] & \text{if}\; [x:=a]^\ell \in \Blocks_\star \\
\Gamma & \text{if}\; [b]^\ell \in \Blocks_\star \\
\end{cases}
\end{align*}
The value of $\iota$ determines an initial mapping of possible types for each variable.


We can easily notice that the lattice $Ctxt$ considered does not satisfy ACC. 
This lattice is a map from a finite set to another lattice ($\pset{\T_\bot}$).
So we just need to explain why the latter might have infinite ascending chains.
This happens because it is a power set of an infinite set. We can always consider the
ascending chain:
\[\emptyset \subseteq \{t_1\}  \subseteq \{t_1,t_2\} \subseteq \{t_1,t_2,t_3\} 
 \subseteq \ldots \subseteq \{t_k \mid k \leq n\} \subseteq \ldots\]   

To overcome this problem, we will make use of widening. We will consider two
possible widening operators.
The first approach is to restrict
the cardinal of the sets of types associated with each variable.
The rationale for this is that, giving warnings related to multiple types of a variable,
it makes no real difference to know that this variable might have 
five different types or any type: we will not list the conflicting types if there
are many of them. This widening operator is now defined (actually, the definition
is parametrized by the number $k$ of the number of possible types
until which detailed information
is likely to be needed):
\begin{align*}
A \nabla_k B = \begin{cases}
A \cup B & \text{if}\, ||A \cup B|| \leq k \\
\T_\bot  & \text{otherwise}
\end{cases}
\end{align*}

We now consider an alternative widening operator. In this case, we will
not restrict the size of the resulting sets but only the 
types that appear there. Before starting the analysis, we
collect all the types
that
explicitly show up in the program.
The goal is to construct a finite set of such types.
However, as an operator might have an infinite set of possible resulting types (and also argument).
Our approach is to identify all of these possible outcomes of an operator.
We then collect 
a finite set of subsets of $\T_\bot$ instead of simply a finite set of types.
The set we collect is:
\begin{eqnarray*}
\T_\star =&  ~   & \{ \{\tau_c\} \mid c  \in \mathbf{Consts_\star} \}\\ 
            & \cup& \{ \mathbf{cod}(\tau_{op})    \mid op \in \mathbf{Operators_\star}\}\\ 
            & \cup& \{ \mathbf{dom_{\pi_1}}(\tau{op})  \mid op \in \mathbf{Operators_\star}\} \\
            & \cup& \{ \mathbf{dom_{\pi_2}}(\tau{op})  \mid op \in \mathbf{Operators_\star}\}\\
            & \cup& \{\T_\bot\}\\
\end{eqnarray*}
where $\mathbf{Consts_\star}$ and $\mathbf{Operators_\star}$ are respectively the constants and operators that appear in the program and
$\mathbf{dom_{\pi_1}}(f)$ stands for the
domain of the first component of the pair $f$ gets as an argument.

The widening operator now presented is a-symmetrical:
we try to preserve the types in the
first argument (even if they are not in $\T_\star$). 
%\begin{align*}
%A \nabla B = \begin{cases}
%A  & \text{if}\, B \subseteq A \\
%A \cup B & \text{if}\, A \cup B \subseteq \T_\star \\
%\T_\bot  & \text{otherwise}
%\end{cases}
%\end{align*}
\begin{align*}
A \nabla B = \begin{cases}
A  & \text{if}\, B \subseteq A \\
\bigcap\{X \in \T_\star \mid A \cup B \subseteq X\}  & \text{otherwise}
\end{cases}
\end{align*}

It can be  shown that this is a widening operator: the finiteness of $\T_\star$ and
thus of its closure by intersections implies that there is a finite number of outcomes of
$A \nabla B$ when $A \subseteq B$ and thus that an ascending chain ``folded'' by $nabla$
eventually stabilizes.

This alternative could also be used to produce warnings.
The idea is that only the information in each point encodes what is known from the uses
of operators and constants in the program. In a certain sense, the sets of types we associate
with each variable may be compared to class constraints, whilst the sets in $\T_\star$
compare to type classes.
So, this alternative might be more well-suited in some cases where operators
really give different outcome types but only a restricted set (even if infinite) of those. 
For example, if an operator which is used in a loop may give $t_0, t_2, t_4, \ldots$ as a result, after a few iterations we might loose all information with the former widening operator, while, using this one, we retain a set which is not $\T_\bot$ (the top element of our lattice). 

Even so, the first alternative is still easier to understand. So, it might be that those values would be more useful. A combination of the two operators could also be considered.

\end{document}  

