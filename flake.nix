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
        in rec {
          djan-world = pkgs.haskellPackages.mkDerivation {
            pname = "djan-world";
            version = "1.0";
            src = ./.;
            license = null;
            buildDepends = with pkgs.haskellPackages; [
              blaze-html
              typed-process
              tasty
              tasty-hunit
              co-log-core
              (callHackageDirect {
                pkg = "relude";
                ver = "1.0.0.1";
                sha256 = "sha256-tpv6QvG8jhAO8tvfehPuyqJBQNosDaS/83cwMkYQl5g=";
              } { })
              (callHackageDirect {
                pkg = "dhall";
                ver = "1.41.1";
                sha256 = "sha256-M2pp0ht/KW3U9947unlQXX0T890uq1MVxkpkOkL2LKI=";
              } { })
              pkgs.zlib
            ];
          };

          djan-world-site = pkgs.stdenv.mkDerivation {
            name = "djan-world";
            version = "0.1.0";
            src = ./.;

            buildInputs = with pkgs; [ djan-world dhall ];

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
