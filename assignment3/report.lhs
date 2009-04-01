\documentclass[a4wide,12pt]{article}
%include polycode.fmt 
\usepackage{a4wide}
\usepackage{times}
\usepackage{fancyvrb}
\usepackage{url}
\usepackage{enumerate}
\usepackage{palatino}
\usepackage{rotating}
 
\usepackage{prooftree}

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
 
\def\pset#1{\mathcal{P}(#1)}
\def\A#1{\mathcal{A}[\hspace{-1pt}[#1]\hspace{-1pt}]}
 
\def\const#1{\mathopen{\langle}#1\mathclose{\rangle}} % <a,b,...z>
\def\pair#1{\const{#1}}
 
\def\Expr{\mathbf{Expr}}
\def\Lab {\mathbf{Lab}}
\def\Blocks{\mathbf{Blocks}}
\def\Var {\mathbf{Var}}
\def\Type {\mathbf{Type}}
\def\AType{\mathbf{AType}}
\def\Annot{\mathbf{Annot}}
\def\ATypeScheme{\mathbf{ATypeScheme}}

 
 
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

\def\sqleq{\sqsubseteq}

\def\htau{\hat{\tau}}
\def\hsigma{\hat{\sigma}}
\def\HGamma{\hat{\Gamma}}
\def\judge#1#2#3{#1 \vdash #2 : #3\;\;}

\def\annot#1{\mid#1\mid}
\def\baset#1{\lfloor#1\rfloor}

\def\program#1[#2]{\begin{progenv}\label{#2}\input{#1}\end{progenv}}

\def\Tiny{\fontsize{3pt}{3pt}\selectfont}
\def\hs#1{\texttt{#1}}

\begin{document}
\author{Chris Eidhof, Rui S. Barbosa}
\title{Type and Effect Systems Assignment \\ Automatic Program Analysis}
 
\maketitle


TODO: subeffecting definition seems to be a problem: see explanation when we thought we had subtyping.
   just top level comparison can lead to inconsistencies, right? ...

\section{Monovariant Binding Time Analysis}

\subsection{Types and Annotations}
As in the book, we have the base type system $\Type$ where $\tau \in \Type$ is given by:
\begin{align*}
\tau := bool \mid int \mid \tau_1 \to \tau_2
\end{align*}

We  now add annotations to our types. An annotation can be either $S$ (for static) or $D$ (for Dynamic). 
We need such an annotation
at all levels in a type (not just on functions). We designate the set of annotations by $\Annot$ and
the set of annotated types by
$\AType$. $\htau \in \AType$ and $\varphi \in \Annot$ are given by:
\begin{align*}
\htau  \; := \; & bool^\varphi \mid int^\varphi \mid \htau_1 \overset{\varphi}{\to} \htau_2 \\
& \\
\varphi \; := \; & S \mid D\\
\end{align*}

$\Annot$ is endowed with a lattice structure where $S$ is bottom and  $D$ is top. We write $\sqleq$ for
the associated order relation. 

Given an annotated type $\htau$, $\annot{\htau}$ stands for its topmost annotation
and $\baset{\htau}$ stands for its underlying (not annotated) type. That is:
\begin{align*}
 \annot{.} : \AType &\to \Annot\\
 \annot{bool^\varphi} & = \varphi \\
 \annot{int^\varphi} & = \varphi \\
 \annot{\htau_1 \overset{\varphi}{\to} \htau_2} & = \varphi \\
\\
\baset{.}  : \AType &\to \Type\\
\baset{bool^\varphi} & = bool \\
\baset{int^\varphi} & = int \\
\baset{\htau_1 \overset{\varphi}{\to} \htau_2} & = \baset{\htau_1} \to \baset{\htau_2} \\
\end{align*}


Some annotated types are not allowed, so we need to introduce a well-formedness
constraint to restrict them. This constraint will express that a dynamic function
has to map a dynamic argument to a dynamic result. This implies that
it can neither receive static arguments nor produce static results. The latter is actually
an impossible situation, whilst the former will be considered again in the other versions
(dynamic functions will be able to accept static arguments, even though they still expect dynamic arguments,
thus the constraint will be kept).
We will just be concerned to give a definition of well-formedness at the top-level as
induction on the type rules then guarantees that only
well-formed types are ever constructed.
\[
wff(\htau_1 \overset{\varphi}{\to} \htau_2) 
= \varphi \sqleq \annot{\htau_1}
 \land   \varphi \sqleq \annot{\htau_2}
\]

\subsection{Type Rules}

We now present the type rules for monomorphic binding-time analysis.

The judgments are of the form
\[\judge{\HGamma}{e}{\htau}\]
where $\HGamma: \Var_\star \to \AType$ is the context, assigning types to each program variable, 
$e \in \Expr$ is an expression in the \textsc{Fun} language and $\htau \in \AType$ is an annotated type.
The meaning is that we can assign the annotated type $\htau$ to expression $e$ under the context $\HGamma$. 


The rules are presented below. All the rules 
have a counterpart in the underlying type system rules presented in the book.
Most are just a lifting of their counterparts to deal with annotated types.
Some specificities show up in the rules
$\lbrack fn \rbrack$,
$\lbrack fun \rbrack$,
$\lbrack con \rbrack$ and
$\lbrack if \rbrack$. 
The first two were already discussed in the last subsection and refer to 
the well-formedness constraint there defined.
In what concerns the $\lbrack con \rbrack$ rule, we can note that we can annotate constants 
as being either static or dynamic. This is necessary so that this type system
is a conservative extension of the underlying type system in the sense that 
we can give an annotated type to each valid term. The algorithm will always try to give the
optimal annotation ($S$) to a constant but it might be forced to decide that it is $D$. 
This will no longer be an issue when we allow for subeffecting: then the annotation will be weakened
only when used. Regarding the $\lbrack if \rbrack$ rule, we note that a conditional expression
needs to be tagged as dynamic if the condition is itself dynamic (because in that case,
the value of the expression cannot be known statically). On the other hand, if the condition
can be known statically then it does not impose a restriction on the expression annotation: it
just needs to be the same as the annotations in the branches.


\begin{eqnarray*}
\lbrack con \rbrack\;\; &
\begin{prooftree}
\justifies
\judge{\HGamma}{c}{\tau_c^\varphi}
\end{prooftree}\\
& & \\
\lbrack var\rbrack\;\; &
\begin{prooftree}
\HGamma(x)=\htau
\justifies
\judge{\HGamma}{x}{\htau}
\end{prooftree}\\
& & \\
\lbrack fn \rbrack\;\; &
\begin{prooftree}
\judge{\HGamma[x\mapsto\htau_1]}{e}{\htau_2}
\justifies
\judge{\HGamma}{fn\; x \Rightarrow e}{\htau_1 \overset{\varphi}{\to} \htau_2}
\end{prooftree}\;\; \text{if}\;\;
wff(\htau_1 \overset{\varphi}{\to} \htau_2)\\
& & \\
\lbrack fun \rbrack\;\; &
\begin{prooftree}
\judge{\HGamma[f \mapsto \htau_1 \overset{\varphi}{\to} \htau_2][x\mapsto\htau_1]}{e}{\htau_2}
\justifies
\judge{\HGamma}{fun\; x \Rightarrow e}{\htau_1 \overset{\varphi}{\to} \htau_2}
\end{prooftree}\;\; \text{if}\;\;
wff(\htau_1 \overset{\varphi}{\to} \htau_2)\\
& & \\
\lbrack app \rbrack\;\; &
\begin{prooftree}
\judge{\HGamma}{e_1}{\htau_1 \overset{\varphi}{\to} \htau_2}\;  \judge{\HGamma}{e_2}{\htau_1}
\justifies
\judge{\HGamma}{e_1 \; e_2}{\htau_2}
\end{prooftree}\\
& & \\
\lbrack if \rbrack\;\; &
\begin{prooftree}
\judge{\HGamma}{e_0}{bool^{\varphi_0}}
\judge{\HGamma}{e_1}{\htau}
\judge{\HGamma}{e_2}{\htau}
\justifies
\judge{\HGamma}{if\;e_0\;then\;e_1\;else\;e_2}{\htau}
\end{prooftree}\;\;\text{if}\;\;
\varphi_0 \sqleq \annot{\htau}\\
& & \\
\lbrack let \rbrack\;\; &
\begin{prooftree}
\judge{\HGamma}{e_1}{\htau_1}
\judge{\HGamma[x \mapsto \htau_1]}{e_2}{\htau_2}
\justifies
\judge{\HGamma}{let\; x=e_1\; in\; e_2}{\htau_2}
\end{prooftree}\\
& & \\
\lbrack op \rbrack\;\; &
\begin{prooftree}
\judge{\HGamma}{e_1}{\tau_{op_1}^\varphi}
\judge{\HGamma}{e_2}{\tau_{op_2}^\varphi}
\justifies
\judge{\HGamma}{e_1\;op\;e_2}{\tau_{op}^\varphi}
\end{prooftree}\\
& & \\
%\lbrack main \rbrack\;\; &
%\begin{prooftree}
%\judge{\HGamma}{e}{\htau}
%\justifies
%\judge{\HGamma}{main = e}{\htau}
%\end{prooftree}\\ \text{if}
%& & \\
\end{eqnarray*}

\subsection{Algorithm}

Our implementation of the inferencing algorithm in Haskell can be found in the accompanying files.
It is a moderate extension of Algorithm W for simply typed $\lambda$-calculus
and the code closely follows the book's specification style.
It generates fresh annotation variables everywhere and collects constraints
with the restricted form
\[\beta \sqleq \beta'\]
where $\beta$ and $\beta'$ are annotation variables.
After this first phase, every subexpression is given a type annotated
with annotation variables. Then, in the constraint solving phase,
the annotation variables get replaced by $S$ or $D$.
The simple form of constraints and the simple structure of the lattice itself make it easy to solve them
and get the ``best'' possible annotations (according to the type rules).

When using the algorithm to analyze programs, the user will need to supply an
annotation for each free variable (the arguments to |main|). The algorithm will then add
those to the constraints and will also add that the program itself needs to be dynamic.
This will become clearer in the examples below.

\subsection{Examples}

Our algorithm has the following type:
\begin{spec}
runW :: [(Var, Annot)] -> Expr () -> Expr (Type Annot)
\end{spec}

The examples in the assignment translate to the following Haskell-code:
\begin{spec}
ex01, ex02, ex03 :: Expr ()
ex01  =  (fn 'x' (fn 'y' $ 'x') <@> i 2) <@> i 3
ex02  =  let_ 'i' (fn 'x' 'x') $ let_ 'y' ('i' <@> i 2) ('i' <@> i 3)
ex03  =  ((fn 'f' (fn 'x' $ 'f' <@> 'x')) <@> (fn 'y' 'y')) <@> i 42
\end{spec}

When we then run the examples we get the expected results (the annotation is
always the first part of the constructor, see also \texttt{Types.hs}):

\begin{verbatim}
res01  = App D (App (S->D)^S 
                    (Fn (D->(S->D)^S)^S 'x' (Fn (S->D)^S 'y' (Var D 'x'))) 
                    (CInt D 2)
               ) 
               (CInt S 3)
res02  = Let D 'i' (Fn (D->D)^S 'x' (Var D 'x')) 
                   (Let D 'y' (App D (Var (D->D)^S 'i') (CInt D 2)) 
                              (App D (Var (D->D)^S 'i') (CInt D 3)))
res03  = App D (App (D->D)^S (Fn ((D->D)^S->(D->D)^S)^S 'f' 
                                 (Fn (D->D)^S 'x' (App D 
                                                       (Var (D->D)^S 'f') 
                                                       (Var D 'x')
                                                  )
                                 )
                             ) 
                             (Fn (D->D)^S 'y' (Var D 'y'))) 
               (CInt D 42)
\end{verbatim}

To illustrate the initial context, consider the program $ex01'$:
\begin{spec}
ex01'  = (fn 'x' (fn 'y' $ 'x') <@> ('q' +: i 1)) <@> ('r' +: i 1)
st     = runW [('r',S),('q', S)] ex01'
dyn    = runW [('r',D),('q', D)] ex01'
\end{spec}

When we take |r| and |q| static we get the following result:
\begin{verbatim}
App D (App (S->D)^S (Fn (D->(S->D)^S)^S 'x' (Fn (S->D)^S 'y' (Var D 'x'))) 
                    (Op D (Var D 'q') Plus (CInt D 1))) 
      (Op S (Var S 'r') Plus (CInt S 1))

\end{verbatim}

If we now take both |r| and |q| to be dynamic we see that this will also change
the functions.

\begin{verbatim}
App D (App (D->D)^S (Fn (D->(D->D)^S)^S 'x' (Fn (D->D)^S 'y' (Var D 'x'))) 
                    (Op D (Var D 'q') Plus (CInt D 1))) 
      (Op D (Var D 'r') Plus (CInt D 1))
\end{verbatim}

\section{Polyvariant Analysis}

\subsection{Types and Annotations}

Instead of considering just types, we allow for type and annotation variables.
We also need to consider
top level quantification
over these in let bindings. 

So, we extend $\AType$ and $\Annot$ as follows:
\begin{eqnarray*}
\htau & := \ldots  \mid \alpha \\ 
\\
\varphi & := \ldots  \mid \beta \\
\end{eqnarray*}
We define annotated type schemes $\hsigma \in \ATypeScheme$ given by
\[\hsigma = \forall (\zeta_1 \ldots \zeta_n). \htau\]
where the $\zeta_i$ are type variables or annotation variables and $\htau \in \AType$.

Note however that we only allow our top level
programs to have annotated types with no free variables, so polymorphism will only show up when using $\texttt{let}$.

\subsection{Type Rules}

We could extend our rule system with the rules
$\lbrack gen \rbrack$ and $\lbrack ins \rbrack$
from the book which
express generalization (abstraction over type and annotation variables) and
instantiation, respectively.
The problem with these is that they are 
non-syntax directed.
We will instead include those concepts into  syntax-directed rules.

The judgements will be of the form
\[\judge{\HGamma}{e}{\htau}\]
where $\htau$ are annotated types (possibly including variables)
and $\HGamma$ now maps variables
to $\ATypeScheme$.

We introduce generalization in the $\lbrack let \rbrack$ rule (generalizing types
and putting them in the context for typing the expression) and
instantiation in the $\lbrack var \rbrack$ rule. In these way, quantification
over type variables will be restricted to let bindings.
Below, we present the rules that need to change in relation to the monovariant version.
\begin{eqnarray*}
\lbrack var \rbrack\;\; &
\begin{prooftree}
\HGamma(x)= \forall(\zeta_1,\ldots,\zeta_n). \htau
\justifies
\judge{\HGamma}{x}{\theta\; \htau}
\end{prooftree} \;\;\text{if}\;\; dom(\theta) \subseteq \{\zeta_1, \ldots, \zeta_n\}
& \\
\lbrack let \rbrack\;\; &
\begin{prooftree}
\judge{\HGamma}{e_1}{\htau_1}
\judge{\HGamma[x \mapsto \forall (\zeta_1, \ldots, \zeta_n). \htau_1]}{e_2}{\htau_2}
\justifies
\judge{\HGamma}{let\; x=e_1\; in\; e_2}{\htau_2}
\end{prooftree}\;\; \text{if}\;\; \zeta_1, \ldots, \zeta_n \notin freeVar(\HGamma) \\
& & \\
\end{eqnarray*}

\section{Subeffecting}

\subsection{Types and Annotations}

We will now consider subeffecting, a form of weakening annotations. The underlying
idea is that, when we expect a dynamic argument, it does no harm to pass
a static one. So, we will be able to loose the information that something is static, weakening it to dynamic.

For that, we need a weakening relation on types. We will not consider a structural shap-conformant
subtyping relation
but will restrict ourselves to look into the topmost annotation and weaken that. 
There is obviously a loss of expressive power, but on the other hand it is much easier to implement
this in practise. 

We will then say that an annotated type $\tau$ can be weakened to
another $\htau'$  ($\htau \preceq \htau'$) if $\annot{\htau} \sqleq \annot{htau'}$
and they have the same underlying type and the same annotations
in all deeper levels. So the only thing we are allowed to do is weakening the top level annotation.
This is not enough yet: we also need to perform some sanity checks to guarantee
that our types are well-formed. The actual definition is given below. 
\begin{align*}
\htau \leq \htau' &\overset{.}{=} \annot{\htau} \sqleq \annot{\htau'} \land samebelow(\htau,\htau') \\
samebelow(\htau,\htau') &\overset{.}{=} 
(\exists \varphi,\varphi'.\;\; \htau = int^{\varphi} \land \htau' = int^{\varphi'})
\\ &\lor 
(\exists \varphi,\varphi'.\;\; \htau = bool^{\varphi} \land \htau' = bool^{\varphi'})
\\ &\lor
(\exists \varphi, \varphi',\htau_1,\htau_2.\;\;
\htau = \htau_1 \overset{\varphi}{\to} \htau_2 \land 
\htau' = \htau_1 \overset{\varphi'}{\to} \htau_2 \land
wff(\htau_1 \overset{\varphi}{\to} \htau_2) \land
wff(\htau_1 \overset{\varphi'}{\to} \htau_2)
) \\
\end{align*}
Note that, in the case for functions above, the annotated types of the arguments (resp. results) need
to be exactly the same.

\subsection{Type Rules}

A simple rule for subeffecting would be
\begin{eqnarray*}
\lbrack sub \rbrack\;\; &
\begin{prooftree}
\judge{\HGamma}{e}{\htau}
\justifies
\judge{\HGamma}{e}{\htau'}
\end{prooftree}\;\;\text{if}\;\; \htau \preceq \htau'\\
& & \\
\end{eqnarray*}

Again, we have the problem that this is not syntax-directed.
In order to incorporate this into the syntax-directed type rules we change the
rules $\lbrack app \rbrack$, $\lbrack if \rbrack$ and $\lbrack op \rbrack$. We are now also allowed to give
static annotations to constants:  they can always be weakened when we need to use them.
The type rules for the subeffecting analysis are presented below.

\begin{eqnarray*}
\lbrack con \rbrack\;\; &
\begin{prooftree}
\justifies
\judge{\HGamma}{c}{\tau_c^S}
\end{prooftree}\\
& & \\
\lbrack var\rbrack\;\; &
\begin{prooftree}
\HGamma(x)=\htau
\justifies
\judge{\HGamma}{x}{\htau}
\end{prooftree}\\
& & \\
\lbrack fn \rbrack\;\; &
\begin{prooftree}
\judge{\HGamma[x\mapsto\htau_1]}{e}{\htau_2}
\justifies
\judge{\HGamma}{fn\; x \Rightarrow e}{\htau_1 \overset{\varphi}{\to} \htau_2}
\end{prooftree}\;\; \text{if}\;\;
wff(\htau_1 \overset{\varphi}{\to} \htau_2)\\
& & \\
\lbrack fun \rbrack\;\; &
\begin{prooftree}
\judge{\HGamma[f \mapsto \htau_1 \overset{\varphi}{\to} \htau_2][x\mapsto\htau_1]}{e}{\htau_2}
\justifies
\judge{\HGamma}{fun\; x \Rightarrow e}{\htau_1 \overset{\varphi}{\to} \htau_2} 
\end{prooftree}\;\; \text{if}\;\;
wff(\htau_1 \overset{\varphi}{\to} \htau_2)\\
& & \\
\lbrack app \rbrack\;\; &
\begin{prooftree}
\judge{\HGamma}{e_1}{\htau_1 \overset{\varphi}{\to} \htau_2}\; \judge{\HGamma}{e_2}{\htau_1'}
\justifies
\judge{\HGamma}{e_1 \; e_2}{\htau_2}
\end{prooftree}\;\;\text{if}\;\;
\htau_1' \preceq \htau_1
\\
& & \\
\lbrack if \rbrack\;\; &
\begin{prooftree}
\judge{\HGamma}{e_0}{bool^{\varphi_0}}
\judge{\HGamma}{e_1}{\htau_1}
\judge{\HGamma}{e_2}{\htau_2}
\justifies
\judge{\HGamma}{if\;e_0\;then\;e_1\;else\;e_2}{\htau}
\end{prooftree}\;\;\text{if}\;\;
\varphi_0 \sqleq \annot{\htau}       \land
\htau_1 \preceq \htau \land
\htau_2 \preceq \htau
\\
& & \\
\lbrack let \rbrack\;\; &
\begin{prooftree}
\judge{\HGamma}{e_1}{\htau_1}
\judge{\HGamma[x \mapsto \htau_1]}{e_2}{\htau_2}
\justifies
\judge{\HGamma}{let\; x=e_1\; in\; e_2}{\htau_2}
\end{prooftree}\\
& & \\
\lbrack op \rbrack\;\; &
\begin{prooftree}
\judge{\HGamma}{e_1}{\tau_{op_1}^{\varphi_1}}
\judge{\HGamma}{e_2}{\tau_{op_2}^{\varphi_2}}
\justifies
\judge{\HGamma}{e_1\;op\;e_2}{\tau_{op}^\varphi}
\end{prooftree}\;\;\text{if}\;\;
\tau_{op_1}^{\varphi_1} \preceq \tau_{op}^\varphi \land
\tau_{op_2}^{\varphi_2} \preceq \tau_{op}^\varphi 
\\
& & \\
\end{eqnarray*}

In the last rule, the condition could have been written more simply as $\varphi_1 \sqleq \varphi \land \varphi_2 \sqleq \varphi$.
We chose not to write in that way so that it is explicit in the type
rules where subeffecting is being applied and  where the conditions are just meant to guarantee well-formedness.

\section{Examples}

We have one example that shows the difference between all three analyses.

\begin{align*}
\texttt{let } & id = \texttt{fn } x \overset{a \overset{a}{\to} a}{\Rightarrow} x^{a} \\
\texttt{in } & (\;\;( \\
             & \;\;\;\;\;\;\;(\texttt{fn } x \overset{a \overset{a}{\to} (a \overset{a}{\to} a)}{\Rightarrow} (\texttt{fn } y
             \overset{a \overset{a}{\to} a}{\Rightarrow} x^{a})) \\
             & \;\;\;\;\;\;\;(\;id^{a \overset{a}{\to} a} \; (z^{a} + 1^{a})^{a}\;)^{a}  \\
             & \;\;\;\;\;)^{a} \\
             & \;\;\;\;(\;id^{a \overset{a}{\to} a} \; (5^{a} + 3^{a})^{a}\;)^{a}   \\
             & )^{a}
\end{align*}

We now give the annotations when doing monovariant analysis:

\begin{align*}
\texttt{let } & id = \texttt{fn } x \overset{D \overset{S}{\to} D}{\Rightarrow} x^{D} \\
\texttt{in } & (\;\;( \\
             & \;\;\;\;\;\;\;(\texttt{fn } x \overset{D \overset{S}{\to} (D \overset{S}{\to} D)}{\Rightarrow} (\texttt{fn } y
             \overset{D \overset{S}{\to} D}{\Rightarrow} x^{D})) \\
             & \;\;\;\;\;\;\;(\;id^{D \overset{S}{\to} D} \; (z^{D} + 1^{D})^{D}\;)^{D}  \\
             & \;\;\;\;\;)^{D \to D} \\
             & \;\;\;\;(\;id^{D \overset{S}{\to} D} \; (5^{D} + 3^{D})^{D}\;)^{D}   \\
             & )^{D}
\end{align*}

We now give the annotations when doing polyvariant analysis. The nice thing here
is that the last argument is $S$.

\begin{align*}
\texttt{let } & id = \texttt{fn } x \overset{\alpha \overset{S}{\to} \alpha}{\Rightarrow} x^{\alpha} \\
\texttt{in } & (\;\;( \\
             & \;\;\;\;\;\;\;(\texttt{fn } x \overset{D \overset{S}{\to} (S \overset{S}{\to} D)}{\Rightarrow} (\texttt{fn } y
             \overset{S \overset{S}{\to} D}{\Rightarrow} x^{D})) \\
             & \;\;\;\;\;\;\;(\;id^{D \overset{S}{\to} D} \; (z^{D} + 1^{D})^{D}\;)^{D}  \\
             & \;\;\;\;\;)^{S \to D} \\
             & \;\;\;\;(\;id^{S \overset{S}{\to} S} \; (5^{S} + 3^{S})^{S}\;)^{S}   \\
             & )^{D}
\end{align*}

We now give the annotations when doing sub-effect analysis:

\begin{align*}
\texttt{let } & id = \texttt{fn } x \overset{D \overset{S}{\to} D}{\Rightarrow} x^{D} \\
\texttt{in } & (\;\;( \\
             & \;\;\;\;\;\;\;(\texttt{fn } x \overset{D \overset{S}{\to} (D \overset{S}{\to} D)}{\Rightarrow} (\texttt{fn } y
             \overset{D \overset{S}{\to} D}{\Rightarrow} x^{D})) \\
             & \;\;\;\;\;\;\;(\;id^{D \overset{S}{\to} D} \; (z^{D} + 1^{S})^{D}\;)^{D}  \\
             & \;\;\;\;\;)^{D \to D} \\
             & \;\;\;\;(\;id^{D \overset{S}{\to} D} \; (5^{S} + 3^{S})^{S}\;)^{D}   \\
             & )^{D}
\end{align*}

\end{document}  

