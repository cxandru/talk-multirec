%include lhs2TeX.fmt

%if False
\begin{code}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE PatternSynonyms #-}


module Poly where
import Pres ((.>))
import Data.Functor.Const (Const(..))
\end{code}
%endif

% for \eval{..}
%options ghci -XGADTSyntax -XDeriveFunctor -XInstanceSigs -XTypeApplications -XLambdaCase -XTypeFamilies -XKindSignatures -XScopedTypeVariables -XStandaloneKindSignatures -XTypeOperators -XPolyKinds -XDataKinds -XRankNTypes -XExplicitForAll -XGADTs

\section{Mutual Recursion}

\subsection{Motivation}

\begin{frame}
  \frametitle{A simple AST}
  Consider the datatype
\begin{code}
data Expr = Lit Int | Var Char | Plus Expr Expr |
  LetIn Decl Expr
data Decl = Bind Char Expr | Seq Decl Decl
\end{code}
Consider an example expression
\begin{code}
e1 :: Expr
e1 = LetIn
  (('x' `Bind` (Lit 3))
  `Seq`
   ('y' `Bind` (Lit 4))
  )  (Var 'x') `Plus` (Var 'y')
\end{code}
\end{frame}
\begin{frame}
  \frametitle{Mutual Recursion}
  \begin{itemize}
  \item |Expr|, |Decl| are a mutually recursive data family. So it is unclear how we should factor out the correct \enquote{structural functor}.
  \item As we did with fixed points, we will take inspiration from how such situations can be handled on the value level.\pause
  \end{itemize}
\begin{code}
foo = let
  even = \case 0 -> True; n -> odd (n-1)
  odd = \case 0 -> False; n -> even (n-1)
  in even 42
\end{code}\pause
\begin{itemize}
\item What if the language provides only a singly recursive \texttt{let}?
\item Use a tupling trick:
\end{itemize}
\begin{code}
bar = let (even, odd) = (
            \case 0 -> True; n -> odd (n-1),
            \case 0 -> False; n -> even (n-1)
            )
  in even 42
\end{code}
\end{frame}

\subsection{Theory}
\begin{frame}
  \frametitle{Product category}
  For two categories \(\mathcal{C}\) and \(\mathcal{D}\), the \emph{product category} \(C\times D\) consists of the following:
  \begin{itemize}
  \item as objects pairs \((C,D)\), \(C:\mathcal{C}_0\), \(D:\mathcal{D}_0\)
  \item as morphisms pairs \((f:X\to Y, g:A\to B)\), \(f:\mathcal{C}_1\), \(g:\mathcal{D}_1\), such that composition is defined componentwise
  \end{itemize}
\end{frame}

\subsection{Implementation}
\begin{frame}
  \frametitle{Functors}
  In Haskell |Functor| represents functors in \texttt{Hask}, so we cannot use it for functors in the product category \texttt{Hask}\(\times\)\texttt{Hask}. We can write some pseudocode though, for how our structural functor ought to look:
\begin{spec}
data ExprF (e,d) = (
   Lit Int | Var Char | Plus e e | LetIn d e
 , Bind Char e | Seq d d
                   )
\end{spec}
What we need is another way to represent tuples. A preliminary observation is that an \(n\)-tuple \(A^n\) can be seen as a function from the finite set of cardinality \(n\) to \(A\): \(n\to A\).
\end{frame}

\begin{frame}[t]
  \frametitle{Remodeling}
\only<1>{The functor for our example family has kind |(2 -> *) -> (2 -> *)|. First we should define this kind |2|.

  We can define define an Enum with two accessors for our expr/decl family, and use it as a kind (lifting its constructors to singleton types), using the GHC extension \texttt{DataKinds}
}
\only<2->{
\begin{code}
data Tag = E | D
data family ASTF1 :: (Tag -> *) -> (Tag -> *)
data instance ASTF1 c E =
  Lit1 Int | Var1 Char | Plus1 (c E) (c E) |
  LetIn1 (c D) (c E)
data instance ASTF1 c D =
  Bind1 Char (c E) | Seq1 (c D) (c D)
\end{code}
\onslide<3->{
  \alt<4>{The two instances define the the component of the structural functor\footnote{Actually, for technical reasons you need to write a GADT. I used type family syntax here since I believe it makes it clearer what is going on} for |Expr| and |Decl| respectively. The tags on the rhs are used to access the components of |c|. If | c ≅ (e,d)|, then |c E = e|, |c D = d|.}
  {The type of |ASTF1| can be read as \enquote{given a table of types to be used in the recursive positions of Expr and Decl, respectively, return a table, where at tag E you find the configured Expr type, at D Decl}.}
}
}
\end{frame}
\begin{frame}
  \frametitle{IFunctor}
  We will need a new typeclass, |IFunctor| (standing for \emph{indexed functor}), for functors of the discussed shape. What is the type of morphisms in |(k -> *)|?
  We saw that in the product category, morphisms were of the form \((f,g)\). Generalizing, we are looking at a collection of maps of the form:\\
\begin{tikzcd}[column sep=small, ampersand replacement=\&]
  A_1 \arrow[d, "f_1"] \& A_2 \arrow[d, "f_2"] \& \cdots \& A_k\arrow[d, "f_k"]\\
  A'_1 \& A'_2 \& \cdots \& A'_k
\end{tikzcd}
\begin{code}
class IFunctor (f :: (k -> *) -> (k -> *)) where
  iFmap :: (forall (i :: k). r i -> r' i) -> f r ix -> f r' ix
\end{code}
\end{frame}
\begin{frame}
  \frametitle{Fixpoint \& Algebra}
  Nothing very fancy happens here. Both result in a family and are defined pointwise via |ix ::k|.
\begin{code}
type IFix :: ((k -> *) -> (k -> *)) -> (k -> *)
newtype IFix f (ix :: k) =
  IIn (f (IFix f) ix)

iUnFix :: IFix f ix -> f (IFix f) ix
iUnFix (IIn f) = f

type Algebra f r ix = f r ix -> r ix
\end{code}
\end{frame}

\begin{frame}
  \frametitle{Cata}
\begin{code}
iCata :: forall k (f :: (k -> *) -> (k -> *))
  (r :: (k -> *))
  (ix :: k). IFunctor f  =>
  (forall (i :: k). Algebra f r i) -> (IFix f ix) -> r ix
iCata ψ = iUnFix .> iFmap (iCata ψ) .> ψ
\end{code}
\end{frame}

\subsection{Worked Example}
\begin{frame}
  \frametitle{Demo!}
\end{frame}
%if False
\begin{code}
type ASTF2 :: (Tag -> *) -> (Tag -> *)
data ASTF2 c :: (Tag -> *) where
  Lit2 :: Int -> ASTF2 c E
  Var2 :: Char -> ASTF2 c E
  Plus2 :: c E -> c E -> ASTF2 c E
  LetIn2 :: c D -> c E -> ASTF2 c E
  Bind2 :: Char -> c E -> ASTF2 c D
  Seq2 :: c D -> c D -> ASTF2 c D

instance IFunctor ASTF2 where
  iFmap f = \case
    Lit2 i -> Lit2 i
    Var2 v -> Var2 v
    Plus2 l r -> Plus2 (f @E l) (f @E r)
    LetIn2 d e -> LetIn2 (f @D d) (f @E e)
    Bind2 c e -> Bind2 c (f @E e)
    Seq2 l r -> Seq2 (f @D l) (f @D r)

prettyPrint :: forall (ix :: Tag). Algebra ASTF2 (Const String) ix
prettyPrint = Const . \case
  Lit2 i -> show i
  Var2 v -> [v]
  Plus2 (Const l) (Const r) -> l ++ " + " ++ r
  LetIn2 (Const d) (Const e) -> "let\n" ++ d ++ "in " ++ e

  Bind2 c (Const e) -> c:" := " ++ e ++ "\n"
  Seq2 (Const l) (Const r) -> l ++ r

pattern Lit' i = (IIn (Lit2 i))
pattern Var' v = (IIn (Var2 v))
pattern Plus' l r = (IIn (Plus2 l r))
pattern LetIn' d e = (IIn (LetIn2 d e))
pattern Bind' c e = (IIn (Bind2 c e))
pattern Seq' l r = (IIn (Seq2 l r))

--e2 :: IFix (ASTF2)
e2 = LetIn'
  (('x' `Bind'` (Lit' 3))
  `Seq'`
   ('y' `Bind'` (Lit' 4))
  )  (Var' 'x') `Plus'` (Var' 'y')
\end{code}
%endif

%%% Local Variables:
%%% TeX-master: "Pres"
%%% End:
