{
  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/haskell-updates";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = inputs:
    inputs.flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = import inputs.nixpkgs { inherit system; };
        # the name of the cabal package and command line
        name = "ndbom";

        haskellExtend = hpFinal: hpPrev: {
          ${name} =
            hpPrev.callCabal2nix name inputs.self { };
        };
        hsPkgs = pkgs.haskellPackages.extend haskellExtend;
      in {
        packages.default = pkgs.haskell.lib.justStaticExecutables hsPkgs.${name};

        devShell = hsPkgs.shellFor {
          packages = p: [ p.${name} ];
          buildInputs = [ pkgs.cabal-install pkgs.ghcid ];
        };
      });
}
