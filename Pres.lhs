\documentclass{beamer}
\usetheme{Luebeck}
\usecolortheme{whale}
\usepackage[english]{babel}
\usepackage{csquotes}
%\usepackage{biblatex}[backend=biber]
%\addbibresource{mybib.bib}
%\usepackage{fancyvrb}
\usepackage{forest}
\usepackage{tikz}
\usetikzlibrary{cd}

% Inspirations:
% https://onthebalcony.wordpress.com/2008/05/16/haskell-lhs2tex-latex-beamer/
% https://github.com/sergv/kievfprog-2017-november
% use %if False %endif lhs2tex directives to ignore

%include talk.fmt
%include lhs2TeX.sty

\title[Indexed Recursive Traversals]{Structured Traversals for (Multiply) Recursive Algebraic Datatypes}
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
\end{code}
%endif

\begin{frame}
  \titlepage
  {\scriptsize \hspace{1cm}Presentation generated from \texttt{.lhs} sources using \texttt{lhs2TeX}}
\end{frame}

\include{Uni}
\include{Poly}
\end{document}
