{ pkgs ? import (fetchTarball https://github.com/NixOS/nixpkgs/archive/6f0c00907bbd.tar.gz) {} }:

with pkgs;

let
    texEnv = (texlive.combine {
      inherit (texlive)
        collection-latex
        pgf
        tikz-cd
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
        ;
  });
in
pkgs.mkShell {
  buildInputs = [
      texEnv
      haskellPackages.lhs2tex
      ghc
      gnumake
  ];
}
