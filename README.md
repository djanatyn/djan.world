# djan.world

this is a flake for generating [my website](https://djan.world)!

* `djan-world` is a haskell package. it takes `dhall` site configuration + content as input, and generates an html site as output.
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
* [`dhall`](https://dhall-lang.org/) for type-checked, structured content + config
* [`ghc 9.0.2`](https://www.haskell.org/ghc/download_ghc_9_0_2.html)
* [`blaze-html`](https://hackage.haskell.org/package/blaze-html) for generating html
* [`relude`](https://hackage.haskell.org/package/relude) for comfort
* [`typed-process`](https://hackage.haskell.org/package/typed-process) for scripting

## design choices

### why use dhall for content?

first of all, dhall is neat! but also, it provides:

* a quick way to get a hash of all content for my blog (`dhall hash < index.dhall`)
* a quick way to compare content between revisions: (`dhall diff < index.dhall`)
* dhall performs type-checking for partial post metadata, showing errors ahead of time
* easy to derive parsers in haskell (`derive FromDhall`)
* wonderful tooling, formatter, and linting support (`dhall help`)
* simple conversion between formats (`dhall-to-yaml`, `dhall-to-json`)

### pull dates from git revision history

all content that's published to the site is tracked with git.

for published content, i want to expose:
* the last modified date of published content, and
* the history of revisions to a piece of content

it's possible to use `git log` to get the history of a file over time:
```
❯ git log --pretty='format:%h %an %s [%ar (%at)]' --follow flake.nix
ca288b6 Jonathan Strickland add zlib to dependencies for flake [16 hours ago (1648686638)]
1144aab Jonathan Strickland using with [17 hours ago (1648683740)]
06fc80e Jonathan Strickland separate into two packages [18 hours ago (1648681589)]
86af897 Jonathan Strickland include zlib [21 hours ago (1648671110)]
0b41fe9 Jonathan Strickland update dhall + relude packages [21 hours ago (1648670783)]
ce6fca7 Jonathan Strickland override relude [25 hours ago (1648656249)]
860efbe Jonathan Strickland initial commit [26 hours ago (1648652884)]
```

because all content is indexed with dhall, we can get this information for each post:
```sh
for file in $(dhall-to-json < index.dhall | jq '.[] | .filename' -r)
do 
    echo "file: ${file}"
    git log --pretty='format:%h %an %s [%ar (%at)]' --follow "${file}"
done 
```

in haskell, we can use `typed-process` to fetch (and later parse) this information for each post.
