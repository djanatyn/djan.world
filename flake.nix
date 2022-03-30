{
  inputs = { nixpkgs.url = "nixpkgs"; };

  description = "source code for https://djan.world/";

  outputs = { self, nixpkgs }:
    let
      supportedSystems = [ "x86_64-linux" ];
      forAllSystems = f:
        nixpkgs.lib.genAttrs supportedSystems (system: f system);
    in {
      packages = forAllSystems (system:
        let pkgs = import nixpkgs { inherit system; };
        in {
          djan-world = pkgs.stdenv.mkDerivation {
            name = "djan-world";
            version = "0.1.0";
            src = ./src;

            buildInputs = with pkgs.haskell; [
              (packages.ghc902.ghcWithPackages (haskellPackages:
                with haskellPackages; [
                  blaze-html
                  typed-process
                  relude
                  dhall
                ]))
              pkgs.dhall
              pkgs.cabal-install
              pkgs.wkhtmltopdf
            ];

            buildPhase = ''
              # run source to
              cabal run djan-world
            '';

            installPhase = ''
              # TODO: replace with website
              mkdir -p $out && touch $out/index.html
            '';
          };
        });
    };
}
