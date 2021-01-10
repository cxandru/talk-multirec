{ pkgs ? import (fetchTarball https://github.com/NixOS/nixpkgs/archive/6f0c00907bbd.tar.gz) {} }:

with pkgs;

pkgs.mkShell {
  buildInputs = [
    (texlive.combine {
      inherit (texlive)
        collection-latex
        pgf
        tikz-cd
        beamer
        csquotes
        babel
        pdftex
        latexmk
        #dependencies of lhs2TeX
        stmaryrd
        amsmath
        cm-mf-extra-bold
        metafont
      ;
    })
    
    haskellPackages.lhs2tex

    ghc
    # (pkgs.ghcWithPackages (
    #   hpkgs: with hpkgs; [
        
    # ]))
    gnumake
    
  ];
}
