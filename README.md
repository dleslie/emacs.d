emacs.d
=======

A collection of my emacs settings, tuned for Emacs 24.3 and higher.

Setup
=====

This is Linux-focused, but wherever possible this configuration also works with Windows. I use MSYS2 alongside of the official distributions of the various supported languages.

* Link this repository to ~/.emacs.d
* Quick Ubuntu apt package requirements:

```
sudo apt-get install gnutls-bin golang cargo rustc clojure1.6 sbcl chicken-bin guile-2.0 racket ruby nodejs nodejs-legacy npm opam haskell-platform rust-gdb rust-doc build-essential gcc gdb g++ git make libtool cmake-curses-gui autoproject automake autoconf clang libclang-dev
```

## (Optional, Windows) MSYS2 dependencies

Run from a MingW64 shell.

```
pacman -Sy
pacman -S mingw-w64-cross-toolchain base-devel mingw64/mingw-w64-x86_64-cmake global
```

## (Optional) Install extra stuff to use particular languages and features

### C++

At least once, use `M-x` to execute `irony-install-server`

### Rust

Install from the official distribution. Set RUST_SRC_PATH appropriately.

```
cargo install racer
```

### Go

Install Go from golang.org

```
go get -u github.com/alecthomas/gometalinter
go get -u github.com/nsf/gocode
gometalinter --install --update
```

### Clojure

```
sudo apt-get install clojure1.4
```

### LISP

```
sudo apt-get install sbcl
```

### Scheme

```
sudo apt-get install chicken-bin guile-2.0 racket
```

Then configure Chicken to support Geiser:

```
chicken-install -s apropos chicken-doc
cd `csi -p '(chicken-home)'` && curl http://3e8.org/pub/chicken-doc/chicken-doc-repo.tgz | sudo tar zx
```

### Python

This is rather straightforward:

```
sudo apt install python-pip python3-pip
```

Then launch emacs and ```M-x elpy-config```

### Ruby

For Windows, Pry and Pry-Doc are shipped with the latest Ruby releases and need not be installed seperately.

```
sudo apt-get install ruby
gem install pry pry-doc
```

### Node

```
sudo apt-get install nodejs nodejs-legacy npm
npm install -g tern
```

### O'Caml

Windows users are SOL. Use a VM, I suppose.

```
sudo apt-get install opam
opam install merlin
```

### Haskell

You should be using stack, if you can; but if you must use Ubuntu's repositories:


```
sudo apt-get install haskell-platform
cabal update
cabal install ghc-mod
```

If you're using stack,

```
stack install ghc-mod
```


## Notes

* The first time you launch it will require an active internet connection to fetch all the necessary packages

* Feel free to configure obvious things (like where to find tern) in `configuration.el`

* Check the "Keys" section for predefined custom keys

* There's a fair amount of undocumented behaviour. Check out the `init.d` folder and have a poke around.

Making it Launch Quickly
========================

You'll want to launch Emacs at least once as a regular application before using it in daemon mode, in order to answer any questions that arise in initial setup.

Run Emacs as a server:

```
emacs --daemon
```

Connect Emacs clients as desired:

```
emacsclient -c
```

