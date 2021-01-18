\documentclass{beamer}
\mode<presentation>{
  \usetheme{Luebeck}
  \usecolortheme{whale}
  \setbeamertemplate{navigation symbols}{}
  \useoutertheme{infolines}
}

\usepackage[english]{babel}
\usepackage{csquotes}
\usepackage[mathletters]{ucs}
\usepackage[utf8x]{inputenc}
\usepackage{amsmath}
\usepackage{amssymb}
\usepackage{latexsym}
%\usepackage{biblatex}[backend=biber]
%\addbibresource{mybib.bib}
%\usepackage{fancyvrb}
\usepackage{forest}
\usepackage{tikz}
\usetikzlibrary{cd}
\usepackage[normalem]{ulem} %strikethrough

% Inspirations:
% https://onthebalcony.wordpress.com/2008/05/16/haskell-lhs2tex-latex-beamer/
% https://github.com/sergv/kievfprog-2017-november
% use %if False %endif lhs2tex directives to ignore

%include talk.fmt
%include lhs2TeX.sty

\title[Structured Traversals for (M)RADTs]{Structured Traversals for (Multiply) Recursive Algebraic Datatypes}
\author{G. Cassian Alexandru}
\date{\today}
\begin{document}

\AtBeginSection[]
{
  \begin{frame}<beamer>{Structure}
    \tableofcontents[currentsection]
  \end{frame}
}

%https://tex.stackexchange.com/questions/119543/how-can-i-get-symbols-to-appear-in-the-middle-of-commutative-diagrams-using-tikz
\tikzset{commutative diagrams/.cd,
mysymbol/.style={start anchor=center,end anchor=center,draw=none}
}
\newcommand\Commutes[2][?]{%
  \arrow[mysymbol]{#2}[description]{#1}}

%if False
\begin{code}
module Pres where
(.>) :: (a -> b) -> (b -> c) -> a -> c
(.>) = flip (.)
\end{code}
%endif

\begin{frame}
  \titlepage
  {\scriptsize \hspace{1cm}Presentation generated from \texttt{.lhs} sources using \texttt{lhs2TeX}}
\end{frame}

\begin{frame}
  \frametitle{Context \& Conventions}
  \begin{itemize}
  \item Language: Haskell, with numerous language extensions
  \item Syntactic (e.g. \texttt{LambdaCase})
  \item Clarifying (e.g. \texttt{TypeApplications, InstanceSigs})
  \item Limited Dependent programming (e.g. \texttt{DataKinds}), for multiple recursion
  \end{itemize}
  \pause
\begin{spec}
{-# LANGUAGE LambdaCase #-}
foo :: [a] -> b
foo = \case
  [] -> …
  (x:xs) -> …
\end{spec}
\pause
\begin{itemize}
\item Composition in diagrammatic order: \(f;g\) reads \enquote{\(f\), then \(g\)}
\item Haskell: |f .> g|
\end{itemize}
\end{frame}

\include{Uni}
\include{Poly}
\end{document}
