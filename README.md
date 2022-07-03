# djan.world

this is a flake for generating [my website](https://djan.world)!

* `djan-world` is a haskell package. it generates an html site as output.
* `djan-world-site` is the output of running `djan-world`.

```
❯ nix flake show github:djanatyn/djan.world
github:djanatyn/djan.world/06fc80e6039df48e7ad5beb1d21df33c2c908b17
└───packages
    └───x86_64-linux
        ├───djan-world: package 'djan-world-1.0'
        └───djan-world-site: package 'djan-world'
```

## tools

* [`nix`](https://nixos.wiki/wiki/Flakes) (flakes) for reproducibility and packaging
* [`ghc 9.0.2`](https://www.haskell.org/ghc/download_ghc_9_0_2.html)
* [`blaze-html`](https://hackage.haskell.org/package/blaze-html) for generating html
* [`relude`](https://hackage.haskell.org/package/relude) for comfort
* [`typed-process`](https://hackage.haskell.org/package/typed-process) for scripting
* [`co-log-core`](https://hackage.haskell.org/package/co-log-core) for logging
