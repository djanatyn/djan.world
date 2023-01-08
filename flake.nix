{
  inputs = { nixpkgs.url = "nixpkgs"; };

  description = "source code for https://djan.world/";

  outputs = { self, nixpkgs }:
    let
      supportedSystems = [ "x86_64-linux" ];
      forAllSystems = f:
        nixpkgs.lib.genAttrs supportedSystems (system: f system);
    in {
      devShells = forAllSystems (system:
        let pkgs = import nixpkgs { inherit system; };
        in {
          djan-world = pkgs.mkShell {
            buildInputs = with pkgs; [
              haskellPackages.ghc # ghc 9.0.2
              zlib.dev
            ];
          };
        });
      packages = forAllSystems (system:
        let pkgs = import nixpkgs { inherit system; };
        in rec {
          djan-world = pkgs.haskellPackages.mkDerivation {
            pname = "djan-world";
            version = "1.0";
            src = ./.;
            license = null;
            buildDepends = with pkgs.haskellPackages; [
              blaze-html
              blaze-svg
              typed-process
              tasty
              tasty-hunit
              tasty-golden
              directory
              co-log-core
              (callHackageDirect {
                pkg = "relude";
                ver = "1.0.0.1";
                sha256 = "sha256-tpv6QvG8jhAO8tvfehPuyqJBQNosDaS/83cwMkYQl5g=";
              } { })
              pkgs.zlib
            ];
          };

          djan-world-site = pkgs.stdenv.mkDerivation {
            name = "djan-world";
            version = "0.1.0";
            src = ./.;

            buildInputs = with pkgs; [ djan-world ];

            buildPhase = ''
              # TODO: implement site generator
              djan-world > index.html
            '';

            installPhase = ''
              # TODO: copy directory of site
              mkdir -p $out && install -D index.html $out/index.html
            '';
          };
        });
    };
}
