{ pkgs ? import (fetchTarball https://github.com/NixOS/nixpkgs/archive/f677051b8dc0b5e2a9348941c99eea8c4b0ff28f.tar.gz) {} }:

with pkgs;

let
    texEnv = (texlive.combine {
      inherit (texlive)
      collection-latex
      pgf
      tikz-cd
      textpos
      forest
      #undeclared forest deps:
      pgfopts
      etoolbox
      environ
      #undeclared environ deps:
      trimspaces
      #
      l3packages #xparse
      inlinedef
      #
      beamer
      csquotes
      babel
      ucs
      pdftex
      latexmk
      #dependencies of lhs2TeX
      stmaryrd
      amsmath
      cm-mf-extra-bold
      metafont
      #
      ulem
      ;
    });
in
pkgs.mkShell {
  buildInputs = [
      texEnv
      haskellPackages.lhs2tex
      ghc
      haskell-language-server
      gnumake
  ];
}
