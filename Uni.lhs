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

\begin{frame}
\begin{code}
length :: [a] -> Int
length = \case
{-"\alert<2->{"-}[]{-"}"-} -> 0
{-"\alert<2->{"-}(x:xs){-"}"-} -> 1 + {-"\alert<2->{"-}length xs{-"}"-}
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
  \begin{columns}
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
      \begin{columns}
        \begin{column}{0.4\textwidth}
          \texttt{length}
          \begin{forest}
            ADT,
            baseline=(current bounding box.north)
            [|\_-> (1+)|
            [1]
            [|\_-> (1+)|
            [2]
            [|\_-> (1+)|
            [3]
            [|\_-> (1+)|
            [4]
            [0]
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
  bool [] [x] (p x) ++ xs
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
  bool [] [x] (p x) ++ xs
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
\only<1>{
\begin{spec}
data List a = Nil | Cons a (List a)
data BooL = TT | FF
\end{spec}
}
\only<2>{
  GADT Syntax:}
\only<2->{
\begin{code}
data List a where
  Nil :: List a
  Cons :: a -> (List a) -> (List a)
data BooL where
  FF :: BooL
  TT :: BooL
\end{code}}
\only<3->{
\begin{code}
list :: b -> (a -> b -> b) -> List a -> b
list {-"\alert<4->{"-}nil cons{-"}"-} = fold where
  {-"\alert<4->{"-}fold Nil{-"}"-} = nil
  {-"\alert<4->{"-}fold (x `Cons` xs){-"}"-} = x `cons` {-"\alert<4->{"-}fold xs{-"}"-}

booL :: b -> b -> BooL -> b
booL tt ff = fold where
  fold FF = ff
  fold TT = tt
\end{code}
}
\end{frame}
\begin{frame}
  \frametitle{Structural Functors}
\begin{code}
data ListF c x = NilF | ConsF c x --deriving Functor
data BooLF x = TTF | FFF deriving Functor

instance Functor (ListF c) where
  fmap :: (a -> b) -> (((ListF c) a) -> ((ListF c) b))
  fmap f = \case
    NilF -> NilF
    ConsF c a -> ConsF c {-"\alert<2->{"-}(f a){-"}"-}
\end{code}

If you are unfamiliar with functors: In our case regarding them as a computational context with holes suffices.
\end{frame}
\begin{frame}
  \begin{itemize}
  \item The structural functors encode where the recursion should happen
  \item Relation to original datatype not yet wholly clear
  \item \(\leadsto\) Introduce a little Category Theory
  \end{itemize}
\end{frame}

\section{Category Theory}

\begin{frame}
  \frametitle{Endofunctors}
  Let \(\mathcal{C}\) be a Category. An \emph{Endofunctor} is a pair of maps, on the objects and morphisms of the category respectively:
  \(F_0:\mathcal{C}_0\to \mathcal{C}_0\), \(F_1:\mathcal{C}_1\to \mathcal{C}_1\)
  Such that:
  \begin{columns}
    \begin{column}{0.5\textwidth}
      \begin{itemize}
      \item \(F_1(h:A\to B) : F_0A\to F_0B\)
      \item \(F_1(id_A) = id_{F_0A}\)
      \item \(F_1(h\circ g)=(F_1h)\circ (F_1g)\)
      \end{itemize}
    \end{column}
    \begin{column}{0.5\textwidth}
      \begin{itemize}
      \item |fmap @f (h :: a -> b) ::| |f a -> f b|
      \item |fmap @f (id @a)| = |id @(f a)|
      \item |fmap @f (h . g)| = |fmap @f h . fmap @f g|
      \end{itemize}
    \end{column}
  \end{columns}
  \vspace{2ex}
  The category we are regarding is \textsl{Hask}, where objects are Haskell types, and morphisms are functions between them.
  This is usually interpreted as a \emph{CPO} (complete partial order). We will mention what notions are specic to \emph{CPO}s.
\end{frame}
\begin{frame}{Algebras}
  Let \(F:\mathcal{C}\to\mathcal{C}\) be an endofunctor \(A_0\in \mathcal{C}_0\), \(\phi: A\to FA\). Then \(A \xrightarrow{\phi} FA\) is an \emph{Algebra}, and \(A\) its \emph{Carrier}.
  We can phrase the business logic of the previously seen functions as such (using the transformation \(A^B\times A^C\sim A^{B+C}\)):
\begin{code}
type Algebra f a = f a -> a
boolBL :: b -> b -> Algebra BooLF b
boolBL tt ff = \case
  TTF -> tt
  FFF -> ff
listBL :: b -> (a -> b -> b) -> Algebra (ListF a) b
listBL nil cons = \case
  NilF -> nil
  x `ConsF` b -> x `cons` b
\end{code}
\end{frame}
\begin{frame}
  To conclude this exercise, we want to generalize the function for recursively applying the business logic (So a function subsuming |list| and |booL| seen earlier):
  \eval{:t list} \eval{:t booL}
We define a type family to associate the structural functors with their types.
\begin{code}
type family CI (f :: * -> *) :: *

type instance CI (ListF a) = List a
type instance CI (BooLF) = BooL
\end{code}
Then our that function would have type \eval{:t cata}. To get to its definition, we must interleave some more Cat.Th.
\end{frame}
\begin{frame}
\begin{code}
cata :: UnwrapCI f => Algebra f b -> (CI f) -> b
cata ψ = unwrap .> fmap (cata ψ) .> ψ
\end{code}
\end{frame}

\begin{frame}
\vspace{2em}
\begin{columns}
\begin{column}{0.2\textwidth}
Algebra\\
\begin{tikzcd}
    FA \arrow[d, "\phi"]\\
    A
\end{tikzcd}\\
\end{column}
\begin{column}{0.3\textwidth}
Algebra-Hom: $(A,\phi)\to (B,\psi)$ \\
\begin{tikzcd}[ampersand replacement=\&]
    FA \Commutes[\circlearrowleft]{rd}\arrow[d, "\phi"] \arrow[r,"Ff"]
      \& FB \arrow[d, "\psi"]\\
    A \arrow[r, "f"]
      \& B
\end{tikzcd}
\end{column}
\begin{column}{0.3\textwidth}
Initial Algebra: $(A,κ)$ \\
\begin{tikzcd}[ampersand replacement=\&]
    FA \Commutes[\circlearrowleft]{rd}\arrow[d,shift left=.75ex, "κ"] \arrow[r, dashed, "Fh"]
      \& FB \arrow[d, "\psi"]\\
    A \arrow[r, dashed, "h"] \arrow[u,shift left=.75ex,"κ^{-1}"]
      \& B
\end{tikzcd}
\end{column}
\end{columns}
{\footnotesize Legend: \tikz[outer xsep=0pt,inner xsep=0pt]{\draw[->,dashed] (0,0) -- (0.4,0);}: \(\exists!\),
  \begin{tikzcd}[cramped, sep=small, ampersand replacement=\&]
    {} \arrow[d,left,"f'"'] \arrow[r,"f"]\Commutes[\circlearrowleft]{rd} \& {}\arrow[d,"g"]\\
    {} \arrow[r,"g'"'] \& {}\\
  \end{tikzcd} : \(f;g = f';g'\) }\\
Initiality requirement: \(h=κ^{-1};Fh;\psi\)\\
\(h\) has exactly the type we want for |cata ψ|, when |CI F| = |A| (CI stands for \enquote{Carrier Initial}).\\
The initiality requirement gives us a function definition, we just still need \(κ^{-1}\).
\end{frame}
\begin{frame}
\begin{code}
class Functor f => UnwrapCI f where
  unwrap :: (CI f) -> f (CI f)
instance UnwrapCI BooLF where
  unwrap :: BooL -> BooLF BooL
  unwrap = \case
    TT -> TTF
    FF -> FFF
instance UnwrapCI (ListF a) where
  unwrap :: List a -> ListF a (List a)
  unwrap = \case
    Nil -> NilF
    x `Cons` xs -> x `ConsF` xs
\end{code}
\end{frame}
\begin{frame}[fragile]
\frametitle{As Program}
\begin{code}
newtype Fix f = In { out :: f (Fix f) }
\end{code}
\end{frame}

%%% Local Variables:
%%% TeX-master: "Pres"
%%% End: