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
            src = ./.;

            buildInputs = with pkgs.haskell; [
              (packages.ghc902.ghcWithPackages (haskellPackages:
                with haskellPackages; [
                  blaze-html
                  typed-process
                  (pkgs.haskellPackages.callHackageDirect {
                    pkg = "relude";
                    ver = "1.0.0.1";
                    sha256 =
                      "sha256-tpv6QvG8jhAO8tvfehPuyqJBQNosDaS/83cwMkYQl5g=";
                  } { })
                  (pkgs.haskellPackages.callHackageDirect {
                    pkg = "dhall";
                    ver = "1.41.1";
                    sha256 =
                      "sha256-M2pp0ht/KW3U9947unlQXX0T890uq1MVxkpkOkL2LKI=";
                  } { })
                ]))
              pkgs.dhall # dhall cli
            ];

            buildPhase = ''
              runhaskell $src/app/Main.hs
            '';

            installPhase = ''
              # TODO: replace with website
              mkdir -p $out && touch $out/index.html
            '';
          };
        });
    };
}
