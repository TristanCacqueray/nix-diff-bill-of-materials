# ndbom: nix diff bill of materials

ndbom collects the sources of a given installable to compute
the source diff between two versions.

The goal is to show the list of every LOC added after running
`nix flake update`.

## Usage

Here are a few demos:

- Compare two versions:

```ShellSession
$ nix flake update
$ ndbom diff github:TristanCacqueray/nix-diff-bill-of-materials#devShell.x86_64-linux .#devShell.x86_64-linux
[+] Modified ansi-terminal: 0.11.5 -> 1.0.2
diff -rup ansi-terminal-0.11.5/CHANGELOG.md ansi-terminal-1.0.2/CHANGELOG.md
--- ansi-terminal-0.11.5/CHANGELOG.md	                                  2023-03-20 16:27:58.000000000 -0400
+++ ansi-terminal-1.0.2/CHANGELOG.md	                                  2024-01-13 12:23:51.000000000 -0500
@@ -1,6 +1,41 @@
 Changes
 =======

+Version 1.0.2
+-------------
+
+* On Windows, fix linker error about a duplicate symbol `castUINTPtrToPtr`.
+
```

- List a nixpkgs package sources:

```ShellSession
$ ndbom list nixpkgs/nixos-23.11#mold
mold 2.3.3 <https://github.com/rui314/mold/archive/v2.3.3.tar.gz>
  tbb 2021.8.0 <https://github.com/oneapi-src/oneTBB/archive/v2021.8.0.tar.gz>
  mimalloc 2.1.2 <https://github.com/microsoft/mimalloc/archive/v2.1.2.tar.gz>
```

- List a rust application sources built with crane:

```ShellSession
$ ndbom list github:logjuicer/logjuicer
logjuicer-cli 0.9.6 <unknown>
  logjuicer-cli-deps 0.9.6 <unknown>
    sqlx-macros 0.7.2 <https://crates.io/api/v1/crates/sqlx-macros/0.7.2/download>
    sprs-0.11.1: /nix/store/cly8f0899l67k60wcv3ck8jvaww1r2b6-sprs-0.11.1 <https://crates.io/api/v1/crates/sprs/0.11.1/download>
    web-sys-0.3.64: /nix/store/fk4kp403rblhpxs4jbkjs6p8iaz8hnfw-web-sys-0.3.64 <https://crates.io/api/v1/crates/web-sys/0.3.64/download>
    tokio-stream-0.1.14: /nix/store/sb8vdsbprwc4ssfgk4flhmwngnrlxndn-tokio-stream-0.1.14 <https://crates.io/api/v1/crates/tokio-stream/0.1.14/download>
```

> Note that some sources are unknown, that's because they are provided by the flake self.

- List a haskell application sources built with nixpkgs:

```ShellSession
$ ndbom list github:Gabriel439/nix-diff
nix-diff 1.0.19 <unknown>
  nix-derivation 1.1.2 <mirror://hackage/nix-derivation-1.1.2.tar.gz>
    QuickCheck 2.14.2 <mirror://hackage/QuickCheck-2.14.2.tar.gz>
      splitmix 0.1.0.4 <mirror://hackage/splitmix-0.1.0.4.tar.gz>
        hscolour 1.24.4 <mirror://hackage/hscolour-1.24.4.tar.gz>
      random 1.2.1.1 <mirror://hackage/random-1.2.1.1.tar.gz>
    attoparsec 0.14.4 <mirror://hackage/attoparsec-0.14.4.tar.gz>
```

- List an emacs with packages:

```ShellSession
$ ndbom list github:podenv/nano-agenda.nix
markdown-mode 20231028.853 <https://github.com/jrblevin/markdown-mode/archive/b1a862f0165b7bafe0f874738a55be1b1720dd7d.tar.gz>
org-ql 20231106.2230 <https://github.com/alphapapa/org-ql/archive/4f62ba3bd6d639b021ee9f159357b2a80d7a2f92.tar.gz>
  s 20220902.1511 <https://github.com/magnars/s.el/archive/b4b8c03fcef316a27f75633fe4bb990aeff6e705.tar.gz>
  transient 20231112.923 <https://github.com/magit/transient/archive/3cd1de1695084df089cc90cff89b32dfd6ca5a0a.tar.gz>
    compat 29.1.4.4 <https://elpa.gnu.org/packages/compat-29.1.4.4.tar https://elpa.gnu.org/packages/compat-29.1.4.4.tar.lz>
magit 20231112.914 <https://github.com/magit/magit/archive/f4ff817cb2a48f0f7887050c3be469c03a059567.tar.gz>
  magit-section 20231014.1405 <https://github.com/magit/magit/archive/22c99839c9fad89461412f153a290779cf3af82c.tar.gz>
```

- List a dev environment:

```ShellSession
$ ndbom list github:logjuicer/logjuicer#devShell.x86_64-linux
trunk 0.17.2 <https://github.com/thedodd/trunk/archive/v0.17.2.tar.gz>
  cargo-auditable 0.6.1 <https://github.com/rust-secure-code/cargo-auditable/archive/v0.6.1.tar.gz>
rust-analyzer 2023-10-02 <https://github.com/rust-lang/rust-analyzer/archive/2023-10-02.tar.gz>
capnproto 1.0.1 <https://github.com/capnproto/capnproto/archive/v1.0.1.tar.gz>
cargo-watch 8.4.1 <https://github.com/watchexec/cargo-watch/archive/v8.4.1.tar.gz>
wasm-pack 0.12.1 <https://github.com/rustwasm/wasm-pack/archive/refs/tags/v0.12.1.tar.gz>
```

## Install

Install `ndbom` by running: `nix profile install github:TristanCacqueray/nix-diff-bill-of-materials`


## Roadmap

- [x] Show source diff
- [ ] Support custom exclude/include list


## Notes

Matching sources with their package name is surprisingly complicated, and this implementation may not work for every installables.
If you know a better way to do this, please let me know!
