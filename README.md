emacs.d
=======

A collection of my emacs settings.

Setup
=====

* Link this repository to ~/.emacs.d

## (Optional) Install extra stuff to use particular languages and features

### C++

Install clang.

At least once, use `M-x` to execute `irony-install-server`

### C#

At least once, use `M-x` to execute `omnisharp-install-server`

### Rust

Install from the official distribution. 

### Go

Install Go from golang.org

### Scheme

Install Chicken, Guile, or Racket.

For Chicken support in Geiser:

```
chicken-install apropos chicken-doc
cd `csi -p '(chicken-home)'` && curl http://3e8.org/pub/chicken-doc/chicken-doc-repo.tgz | tar zx
```

### Python

Install Python and Pip.

At least once, use `M-x` to execute `elpy-config`

### Ruby

Install the official Ruby distribution.

### Node

Install Node somehow.

### Haskell

Install the Haskell Stack.

## Notes

* The first time you launch it will require an active internet connection to fetch all the necessary packages

* First time launch will be a while; get a coffee

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

