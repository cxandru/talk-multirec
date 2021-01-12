%include talk.fmt

%if False
\begin{code}
{-# LANGUAGE GADTSyntax #-}
module Uni where
import Prelude hiding ( length )
import Data.Bool ( bool )
\end{code}
%endif

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
\begin{frame}{Algebras}
\(F:\mathcal{C}\to\mathcal{C}, A,B,A_0\in \mathcal{C}_0\)\\
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
    FA \arrow[d, "\phi"] \arrow[r,"Ff"]
      \& FB \arrow[d, "\psi"]\\
    A \arrow[r, "f"]
      \& B
\end{tikzcd}
\end{column}
\begin{column}{0.3\textwidth}
Initial Algebra: $(A,\kappa)$ \\
\begin{tikzcd}[ampersand replacement=\&]
    FA \arrow[d,shift left=.75ex, "\kappa"] \arrow[r, dashed, "Fh"]
      \& FB \arrow[d, "\psi"]\\
    A \arrow[r, dashed, "h"] \arrow[u,shift left=.75ex,"\kappa^{-1}"]
      \& B
\end{tikzcd}
\end{column}
\end{columns}
\vspace{2em}
Initiality requirement: \(h=\kappa^{-1};Fh;\psi\)
\end{frame}
\begin{frame}[fragile]
\frametitle{As Program}
\begin{code}
newtype Fix f = In { out :: f (Fix f) }
\end{code}
\begin{code}
type Algebra f c = f c -> c
\end{code}
\begin{code}
cata :: Functor f => Algebra f a -> Fix f -> a
cata alg = alg . fmap (cata alg) . out
\end{code}
\end{frame}

%%% Local Variables:
%%% TeX-master: "Pres"
%%% End: