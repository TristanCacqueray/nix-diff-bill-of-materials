# ndbom: nix diff bill of materials

ndbom collects the sources of a given installable to compute
the source diff between two versions.

The goal is to show the list of every LOC added after running
`nix flake update`.

## Usage

Here are a few demos:

- A nixpkgs package:

```ShellSession
$ ndbom list nixpkgs/nixos-23.11#mold
mold-2.3.3: /nix/store/mgnc0zrfgzxmpbgzbjri6ma2855jiqp4-source <https://github.com/rui314/mold/archive/v2.3.3.tar.gz>
...
mimalloc-2.1.2: /nix/store/c58kvgdm0ykzkq8318f7by64isbbpyjd-source <https://github.com/microsoft/mimalloc/archive/v2.1.2.tar.gz>
```

- A rust application built with crane:

```ShellSession
$ ndbom list github:logjuicer/logjuicer
logjuicer-cli-deps-0.9.6: /nix/store/5dlpmvlj8nmcvygk346fmsw7s1i0arcf-source <unknown>
...
sprs-0.11.1: /nix/store/cly8f0899l67k60wcv3ck8jvaww1r2b6-sprs-0.11.1 <https://crates.io/api/v1/crates/sprs/0.11.1/download>
web-sys-0.3.64: /nix/store/fk4kp403rblhpxs4jbkjs6p8iaz8hnfw-web-sys-0.3.64 <https://crates.io/api/v1/crates/web-sys/0.3.64/download>
syn-2.0.28: /nix/store/y2n8w1wrkaccf43hj03i0fqhb0bm1vzm-syn-2.0.28 <https://crates.io/api/v1/crates/syn/2.0.28/download>
tokio-stream-0.1.14: /nix/store/sb8vdsbprwc4ssfgk4flhmwngnrlxndn-tokio-stream-0.1.14 <https://crates.io/api/v1/crates/tokio-stream/0.1.14/download>
```

> Note that the flake source input does not have an url (marked unknown) because it is provided by nix

- A haskell application built with nixpkgs:

```ShellSession
$ ndbom list github:Gabriel439/nix-diff
nix-diff-1.0.19: /nix/store/l3jrgrjkibips68aqz6ic87w93ywnira-fkrsjynjqpr8ch1dy5lqs7af3025kjba-source <unknown>
...
unliftio-core-0.2.0.1: /nix/store/0ydddavs1dqvbbk7xncy1rbhqvlwbfcx-unliftio-core-0.2.0.1.tar.gz <mirror://hackage/unliftio-core-0.2.0.1.tar.gz>
witherable-0.4.2: /nix/store/vkbllbpcamkghfvpdkdjb441aicc3zp8-witherable-0.4.2.tar.gz <mirror://hackage/witherable-0.4.2.tar.gz>
base16-bytestring-1.0.2.0: /nix/store/8b8f6aiqazxwgrdznnzii2zjvjmzldnz-base16-bytestring-1.0.2.0.tar.gz <mirror://hackage/base16-bytestring-1.0.2.0.tar.gz>
uniplate-1.6.13: /nix/store/ka5s06hbpzrad83wn0q9ivc77imhq6mm-uniplate-1.6.13.tar.gz <mirror://hackage/uniplate-1.6.13.tar.gz>
patience-0.3: /nix/store/97wn7k8k562av53ryhwzsj830rwisn9r-patience-0.3.tar.gz <mirror://hackage/patience-0.3.tar.gz>
```

- An emacs with packages:

```ShellSession
$ ndbom list github:podenv/nano-agenda.nix
emacs-nox-29.1: /nix/store/g8xgf2qvc3mqkkygg2vmjpfhlbh6fy5w-source <https://git.savannah.gnu.org/cgit/emacs.git/snapshot/emacs-29.1.tar.gz>
...
org-ql-20231106.2230: /nix/store/jyxb8nmayf26y20knpac7j63yypc3xla-source <https://github.com/alphapapa/org-ql/archive/4f62ba3bd6d639b021ee9f159357b2a80d7a2f92.tar.gz>
nano-agenda-0.3: /nix/store/r8pyzy4xqah4nfhxjwd305wd8qvs5b8s-nano-agenda-0.3.tar <https://elpa.gnu.org/packages/nano-agenda-0.3.tar https://elpa.gnu.org/packages/nano-agenda-0.3.tar.lz>
magit-20231112.914: /nix/store/9lkz0l7q8835dph0cdj5iszphk6gpkfg-source <https://github.com/magit/magit/archive/f4ff817cb2a48f0f7887050c3be469c03a059567.tar.gz>
magit-section-20231014.1405: /nix/store/zd6m2827fb95xf2msls2cdcq3as8vv4g-source <https://github.com/magit/magit/archive/22c99839c9fad89461412f153a290779cf3af82c.tar.gz>
```

- A dev environment:

```ShellSession
$ ndbom list github:logjuicer/logjuicer#devShell.x86_64-linux
...
rust-analyzer-2023-10-02: /nix/store/x0c64h1rhdxlzz894jv60qq01fdjjmkm-source <https://github.com/rust-lang/rust-analyzer/archive/2023-10-02.tar.gz>
capnproto-1.0.1: /nix/store/mspd99q7gw6igc3rf6y3hkkas7b14jkw-source <https://github.com/capnproto/capnproto/archive/v1.0.1.tar.gz>
cargo-watch-8.4.1: /nix/store/wsla3x0rznrxfslm0qhw4bnndwkllwll-source <https://github.com/watchexec/cargo-watch/archive/v8.4.1.tar.gz>
wasm-pack-0.12.1: /nix/store/w5ndkqw62yl2q4bvdp4nhk59vxyd6s64-source <https://github.com/rustwasm/wasm-pack/archive/refs/tags/v0.12.1.tar.gz>
```

## Install

Install `ndbom` by running: `nix profile install github:TristanCacqueray/nix-diff-bill-of-materials`

## Roadmap

Implement the diff command to compare two derivations:
When two inputs have the same name, perform a diff on the sources.

## Notes

There got to be a better way to do this, that's just an investigation.
