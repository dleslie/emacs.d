emacs.d
=======

A collection of my emacs settings, tuned for Emacs 24.3 and higher.

Setup
=====

This is Linux-focused, but wherever possible this configuration also works with Windows. I use MSYS2 alongside of the official distributions of the various supported languages.

* Link this repository to ~/.emacs.d

## (Optional) Install extra stuff to use particular languages and features

### C++

At least once, use `M-x` to execute `irony-install-server`

### Rust

Install from the official distribution. Do the following:

```
cargo install racer
rustup component add rust-src
```

### Go

Install Go from golang.org

```
go get -u github.com/alecthomas/gometalinter
go get -u github.com/nsf/gocode
gometalinter --install --update
```

### Scheme

Install Chicken, Guile, or Racket.

For Chicken support in Geiser:

```
chicken-install apropos chicken-doc
cd `csi -p '(chicken-home)'` && curl http://3e8.org/pub/chicken-doc/chicken-doc-repo.tgz | tar zx
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
gem install pry pry-doc
```

### Node

```
npm install -g tern
```

### O'Caml

Windows users are SOL. Use a VM, I suppose.

```
opam install merlin
```

### Haskell

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

