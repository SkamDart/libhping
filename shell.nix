let nixos = fetchTarball { 
  url = "https://releases.nixos.org/nixos/20.09/nixos-20.09.3505.12d9950bf47/nixexprs.tar.xz";
  sha256 = "0fsl8bsdb8i536pfs4wrp0826h5l84xqlwx32sbz66jg4ykqp9lr";
}; in
{ compiler ? "ghc8102"
, pkgs ? import nixos { config = {}; }
}:
with pkgs;
let
  deps = h: with h; [];
  ghc = haskell.packages.${compiler}.ghcWithPackages deps;
  easy-hls-src = fetchFromGitHub {
    owner  = "jkachmar";
    repo   = "easy-hls-nix";
    rev    = "0cc4e5893a3e1de3456e3c91bc8dfdebad249dc1";
    sha256 = "nu3HCXSie7yfMhj2h7wCtsEYTrzrBiVE7kdFg0SsV8o=";
  };
  easy-hls = callPackage easy-hls-src {
    ghcVersions = [ "8.10.2" ];
  };
in
stdenv.mkDerivation {
    name = "libhping";
    buildInputs = [ easy-hls ghc cabal-install liboping ];
    shellHook = ''
      eval $(grep export ${ghc}/bin/ghc)
    '';
}
