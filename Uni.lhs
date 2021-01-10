%include talk.fmt

\begin{frame}
\begin{code}
{-# LANGUAGE GADTSyntax #-}
data List a = Nil | Cons a (List a)
data List a where
  Nil :: List a
  Cons :: a -> (List a) -> (List a)
\end{code}
\begin{code}
main :: IO ()
main = do
  case f of
    Nothing -> bar
    Just g -> baz
  return foo;
\end{code}
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