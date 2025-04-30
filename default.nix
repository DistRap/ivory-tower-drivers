{ nixpkgs ? import <nixpkgs> {}, compiler ? "default" }:
let
  itnSrc = nixpkgs.fetchFromGitHub {
    owner = "HaskellEmbedded";
    repo = "ivory-tower-nix";
    rev = "d190cabc990ededb1d4a8ce6e55d593d9ea8c942";
    sha256 = "0qr5wwmc6x3n8nryrkqv7zq0w13acvnj4kr5z5diwikhnp46ifhr";
  };

  itn = import itnSrc { inherit compiler; };

  src = itn.pkgs.nix-gitignore.gitignoreSource [] ./.;
in
  itn // {
    shell = itn.mkShell itn.ivorypkgs.ivory-tower-drivers;
  }
