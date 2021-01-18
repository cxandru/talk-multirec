%include talk.fmt

%if False
\begin{code}
{-# LANGUAGE GADTSyntax #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}


module Uni where
import Prelude hiding ( length )
import Data.Bool ( bool )
import Data.Kind (Type)
import Pres ((.>))
\end{code}
%endif

% for \eval{..}
%options ghci -XGADTSyntax -XDeriveFunctor -XInstanceSigs -XTypeApplications -XLambdaCase -XScopedTypeVariables

\section{Single Recursion}

\subsection{Motivation}

\begin{frame}
\begin{code}
length :: [a] -> Int
length = \case
{-"\alert<2->{"-}[]{-"}"-} -> 0
{-"\alert<2->{"-}(_:xs){-"}"-} -> 1 + {-"\alert<2->{"-}length xs{-"}"-}
\end{code}
 % still unsure if `go` makes this more or less understandable here
\begin{code}
filter :: (a -> Bool) -> [a] -> [a]
filter p = go where
  {-"\alert<2->{"-}go []{-"}"-} = []
  {-"\alert<2->{"-}go (x:xs){-"}"-} = if p x then [x] else [] ++ {-"\alert<2->{"-}go xs{-"}"-}
\end{code}
\onslide<3->
\begin{itemize}
\item List Design pattern?
\item Design Patterns are a poor man's abstraction
\item Recognize common structure \& find correct abstract notion
\end{itemize}
\end{frame}
\forestset{
  ADT/.style={
    for tree={
      font={\fontsize{9.5pt}{9.5pt}\ttfamily},
      % https://www.dickimaw-books.com/latex/novices/html/fontstyle.html
    }
  },
}

\begin{frame}
  \begin{columns}[t]
    \begin{column}{0.2\textwidth}
      List\\
      \begin{forest}
        ADT,
        baseline=(current bounding box.north)
          [:
          [1]
          [:
          [2]
          [:
          [3]
          [:
          [4]
          [{[]}]
          ]
          ]
          ]
          ]
        \end{forest}
    \end{column}
    \begin{column}{0.8\textwidth}
      Traversals\\
      \begin{columns}[t]
        \begin{column}{0.4\textwidth}
          \texttt{length}
          \begin{forest}
            ADT,
            baseline=(current bounding box.north)
            [|\_ n -> 1+n|
            [1]
            [|\_ n -> 1+n|
            [2]
            [|\_ n -> 1+n|
            [3]
            [|\_ n -> 1+n|
            [4]
            [~~~0~~~~~]
            ]
            ]
            ]
            ]
          \end{forest}
        \end{column}
        \begin{column}{0.4\textwidth}
          |filter p|\\
          % (++).(\x -> (bool [] [x] (p x)))
          \begin{forest}
            ADT,
            baseline=(current bounding box.north)
            [g p
            [1]
            [g p
            [2]
            [g p
            [3]
            [g p
            [4]
            [{[]}]
            ]
            ]
            ]
            ]
          \end{forest}\\
          \fontsize{9.5pt}{9.5pt}
\begin{code}
g p x xs =
  (if p x then [x] else [])
  ++ xs
\end{code}
        \end{column}
      \end{columns}
    \end{column}
  \end{columns}
\end{frame}
%List Example
\begin{frame}[t]
  \frametitle{Example Evaluation of |filter even|}
\begin{spec}
g p x xs =
  (if p x then [x] else []) ++ xs
\end{spec}
  \only<1>{\begin{forest}
    ADT,
    baseline=(current bounding box.north)
    [g even
    [1]
    [g even
    [2]
    [g even
    [3]
    [g even
    [4]
    [{[]}]
    ]
    ]
    ]
    ]
  \end{forest}}
  \only<2>{\begin{forest}
    ADT,
    baseline=(current bounding box.north)
    [g even
    [1]
    [g even
    [2]
    [g even
    [3]
    [{4:[]}]
    ]
    ]
    ]
  \end{forest}}
\only<3>{\begin{forest}
    ADT,
    baseline=(current bounding box.north)
    [g even
    [1]
    [g even
    [2]
    [{4:[]}]
    ]
    ]
  \end{forest}}
\only<4>{\begin{forest}
    ADT,
    baseline=(current bounding box.north)
    [g even
    [1]
    [{2:4:[]}]
    ]
  \end{forest}}
\only<5>{\begin{forest}
    ADT,
    baseline=(current bounding box.north)
    [{2:4:[]}]
  \end{forest}}
\end{frame}
\begin{frame}
  \frametitle{Insight}
 Even though the functions were defined recursively, their behaviour can be understood non-recursively as simply replacing the two constructors for |[a]| by functions of the same arity.
\end{frame}
\begin{frame}
\only<1>{
\begin{spec}
data List a = Nil | Cons a (List a)
\end{spec}
}
\only<2->{
  GADT Syntax:
\begin{code}
data List a where
  Nil :: List a
  Cons :: a -> (List a) -> (List a)
\end{code}
\onslide<3->{
\begin{code}
list :: b -> (a -> b -> b) -> List a -> b
list {-"\alert<4->{"-}nil cons{-"}"-} = fold where
  {-"\alert<4->{"-}fold Nil{-"}"-} = nil
  {-"\alert<4->{"-}fold (x `Cons` xs){-"}"-} = x `cons` {-"\alert<4->{"-}fold xs{-"}"-}

length' = list 0 (\_ n -> 1+n)
filter' p = list []
  (\x xs -> (if p x then [x] else []) ++ xs)
\end{code}
}
}
\end{frame}
\begin{frame}
  \frametitle{Now for Expressions}
\begin{code}
data Expr where
  Lit :: Int -> Expr
  Plus :: Expr -> Expr -> Expr

expr :: (Int -> b) -> (b -> b -> b) -> Expr -> b
expr lit plus = fold where
  fold (Lit i) = lit i
  fold (l `Plus` r) = (fold l) `plus` (fold r)
\end{code}
\end{frame}
\begin{frame}
  \begin{itemize}
  \item We want to define a polytypic \enquote{fold}, subsuming \texttt{list}, \texttt{expr}, which encapsulates the whole \enquote{replace constructors with functions} pattern
  \item We need a deeper understanding of what the datatypes we are working with \emph{are}
  \item \(\leadsto\) Introduce a little \sout{Anarchy}Category Theory
  \end{itemize}
\end{frame}
\subsection{Category Theory}
\begin{frame}
  \frametitle{Category}
  A category \(\mathcal{C}\) consists of collections \(\mathcal{C}_0\) of objects and \(\mathcal{C}_1\) of morphisms (or arrows) between them, with the following structure:
  \begin{itemize}
  \item For every two arrows \(f: A\to B\), \(g: B\to C\), there is a composite arrow \(f;g:A\to C\)
  \item For every object \(A:\mathcal{C}_0\) there is an identity morphism \(1_A: A\to A\)
  \item Such that the following hold:
    \begin{itemize}
    \item Composition is associative, that is: \(f;(g;h) = (f;g);h\).
    \item Composition satisfies unit laws: For every \(f:A\to B.\ id_A;f = f,\ f;id_B=f\).
    \end{itemize}
  \end{itemize}
\end{frame}
\begin{frame}
  \frametitle{Isomorphisms}
  Given a category \(C\) and two objects \(A,B : \mathcal{C}_0\), we say \(A\) and \(B\) are isomorphic via \(f:A\to B\), if there exists a \(g:B\to A\) which is both a left- and right-inverse:
  \[
    \begin{array}{ccc}
      \begin{tikzcd}[column sep=small, ampersand replacement=\&]
        \& B \arrow[dr, "g"] \& \\
        A \arrow[ur,"f"] \arrow[rr, "1_A"] \&
        \& A
      \end{tikzcd}
      &
        \begin{tikzcd}[column sep=small, ampersand replacement=\&]
          \& A \arrow[dr, "f"] \& \\
          B \arrow[ur,"g"] \arrow[rr, "1_B"] \&
          \& B
        \end{tikzcd}
      &
        \begin{tikzcd}[column sep=small, ampersand replacement=\&]
          A\arrow[loop left, "1_A"] \arrow[rr, bend left, "f"] \& \&
          B \arrow[ll, bend left, "g"] \arrow[loop right, "1_B"]
        \end{tikzcd}
    \end{array}
  \]
\end{frame}

\begin{frame}
  \frametitle{Functors}
  Let \(\mathcal{C}, \mathcal{D}\) be Categories. A \emph{Functor} \(F\) is a pair of maps \((F_0,F_1)\), on the objects and morphisms of the category respectively, such that commuting diagrams are preserved, e.g.:
  \[\begin{array}{ccc}
      \begin{tikzcd}[column sep=small, ampersand replacement=\&]
        \& B \arrow[dr, "g"] \& \\
        A \arrow[ur,"f"] \arrow[rr, "h"] \&
        \& C
      \end{tikzcd}
    &
      \stackrel{F}{\Rightarrow}
    &
    \begin{tikzcd}[column sep=small, ampersand replacement=\&]
      \& F_0B \arrow[dr, "F_1g"] \& \\
      F_0A \arrow[ur,"F_1f"] \arrow[rr, "F_1h"] \&
      \& F_0C
    \end{tikzcd}
  \end{array}\]
In particular, this means identities \& composition are preserved.  \pause
We will often only write out the definition of a functor on objects.  \pause
When \(\mathcal{C}=\mathcal{D}\), we say \(F\) is an \emph{Endofunctor}.
\end{frame}
\begin{frame}
  \frametitle{Building the Functor kit}
  \begin{itemize}
  \item Identity (\(IX := X\)) is a functor.
  \item Constant-to-\(A\) (\(K_AX := A\)), for \(A:C_0\), is a functor.
  \end{itemize}
  \vspace{2ex}
  Categories can have products (\(\times_{\mathcal{C}}\)) and/or coproducts (\(+_{\mathcal{C}}\)). Think of coproducts as indexed unions in our case. Then if \(F,G\) are functors so are:
  \vspace{1.5ex}
  \begin{itemize}
  \item \((F\times G) X := FX \times GX\)
  \item \((F + G) X := FX + GX\)
  \end{itemize}
\end{frame}
\newcommand{\Alg}[3]{\ensuremath{{#1}{#2}\xrightarrow{{#3}} {#1}}}
\begin{frame}
  \frametitle{Algebra}
  Let \(F:\mathcal{C}\to\mathcal{C}\) be an endofunctor \(A: \mathcal{C}\), \(φ: FA\to A\). Then \(FA \xrightarrow{φ} A\) (or \((A,φ)\)) is an \emph{Algebra}, and \(A\) its \emph{Carrier}.
\vspace{1ex}
\begin{columns}
\begin{column}{0.2\textwidth}
Algebra\\
\begin{tikzcd}
    FA \arrow[d, "φ"]\\
    A
\end{tikzcd}\\
\end{column}
\begin{column}{0.3\textwidth}
Algebra-Hom: $(A,φ)\stackrel{f}{\to} (B,\psi)$ \\
\begin{tikzcd}[ampersand replacement=\&]
   FA \arrow[d, "φ"] \arrow[r,"Ff"]
      \& FB \arrow[d, "ψ"] \\
    A \arrow[r, "f"]
      \& B
\end{tikzcd}
\end{column}
\begin{column}{0.3\textwidth}
Initial Algebra: $(A,α)$ s.t. \(∀(B,ψ).\) \\
\begin{tikzcd}[ampersand replacement=\&]
    FA \arrow[d, "α"]\arrow[r, dashed, "Fh"]
      \& FB \arrow[d, "ψ"]\\
    A \arrow[r, dashed, "h"]
      \& B
\end{tikzcd}
\end{column}
\end{columns}
\end{frame}

\newbox\final

\savebox{\final}{
  \begin{tikzcd}[ampersand replacement=\&]
    FA \arrow[d, "α"]\arrow[r, dashed, "Fh"]\arrow[rr, bend left, "F1_A"]
    \& F(FA) \arrow[d, "Fα"] \arrow[r, "Fα"]\& FX\arrow[d,"α"]\\
    A \arrow[r, dashed, "h"] \arrow[rr, bend right=26, "1_A"']
    \& FA \arrow[r, "α"]\& X
  \end{tikzcd}
}
% https://tex.stackexchange.com/questions/265884/print-size-of-box-to-latex-output
% https://tex.stackexchange.com/questions/83672/beamer-vertically-center-picture-inside-overlayarea
%\typeout{Box wdth:\the\wd\final Box hght: \the\ht\final}

\begin{frame}
  \frametitle{Lambek's Lemma}
  If \(F\) has an initial algebra \((A,α)\), then \(A\) is isomorphic to \(FA\) via \(α\).
  Proof (We show only \(h;α = id_A\)): Consider the algebra \((FA,Fα)\):\\
  \parbox[t][\the\wd\final][c]{\the\ht\final}{
    \only<1>{
      \begin{tikzcd}[ampersand replacement=\&]
        FA \arrow[d, "α"]\\
        A
      \end{tikzcd}
    }
    \only<2>{
      \begin{tikzcd}[ampersand replacement=\&]
        FA \arrow[d, "α"]\arrow[r, dashed, "Fh"]
        \& F(FA) \arrow[d, "Fα"]\\
        A \arrow[r, dashed, "h"]
        \& FA
      \end{tikzcd}
    }
    \only<3>{
      \begin{tikzcd}[ampersand replacement=\&]
        FA \arrow[d, "α"]\arrow[r, dashed, "Fh"]
        \& F(FA) \arrow[d, "Fα"]\arrow[r, "Fα"]\& FX\arrow[d,"α"]\\
        A \arrow[r, dashed, "h"]
        \& FA \arrow[r, "α"]\& X
      \end{tikzcd}
    }
    \only<4>{
      \begin{tikzcd}[ampersand replacement=\&]
        FA \arrow[d, "α"]\arrow[r, dashed, "Fh"]\arrow[rr, bend left, "F1_A"]
        \& F(FA) \arrow[d, "Fα"] \arrow[r, "Fα"]\& FX\arrow[d,"α"]\\
        A \arrow[r, dashed, "h"] \arrow[rr, bend right=26, "1_A"']
        \& FA \arrow[r, "α"]\& X
      \end{tikzcd}
    }
  }
% Wish the below worked *sigh*:
% \begin{tikzcd}[ampersand replacement=\&]
%   FA \arrow[d, "α"]\arrow[r, dashed, "Fh"]\onslide<3->{\arrow[rr, bend left, "F1_A"]}
%   \& F(FA) \arrow[d, "Fα"] \onslide<2->{\arrow[r, "Fα"]\& FX\arrow[d,"α"]}\\
%   A \arrow[r, dashed, "h"] \onslide<3->{\arrow[rr, bend right, "1_A"]}
%   \& FA \onslide<2->{\arrow[r, "α"]\& X}
% \end{tikzcd}
\end{frame}

\begin{frame}
  \frametitle{Fixed Point}
  The carrier \(A\) of the initial algebra \((A,α)\) of a functor \(F\) is a least fixed point of \(F\). Least, that is, in that there is a morphism from it to any other algebra, by initiality.\\
  \pause
  \begin{tikzcd}[ampersand replacement=\&]
    FA \Commutes[\circlearrowleft]{rd}\arrow[d,shift left=.75ex, "α"] \arrow[r, dashed, "Fh"]
    \& FB \arrow[d, "ψ"]\\
    A \arrow[r, dashed, "h"] \arrow[u,shift left=.75ex,"α^{-1}"]
    \& B
  \end{tikzcd}
  We get a recursive definition for h: \(h=α^{-1};Fh;ψ\)
\end{frame}
\begin{frame}
  We can phrase the business logic of the previously seen functions as such (using the transformation \(A^B\times A^C\sim A^{B+C}\)):
\begin{code}
type Algebra f a = f a -> a
listBL :: b -> (a -> b -> b) -> Algebra (ListF a) b
listBL nil cons = \case
  NilF -> nil
  x `ConsF` b -> x `cons` b
\end{code}
\end{frame}
\begin{frame}
\begin{code}
cata :: Functor f => Algebra f b -> (Fix f) -> b
cata ψ = unFix .> fmap (cata ψ) .> ψ
\end{code}
\end{frame}
\begin{frame}
  \frametitle{Structural Functors}
\begin{code}
data ListF c x = NilF | ConsF c x deriving Functor
\end{code}
\end{frame}

\begin{frame}
\frametitle{As Program}
\begin{code}
newtype Fix (f :: * -> *) :: * where
  In :: f (Fix f) -> Fix f
unFix :: Fix f -> f (Fix f)
unFix (In f) = f
\end{code}
\end{frame}

%%% Local Variables:
%%% TeX-master: "Pres"
%%% End: