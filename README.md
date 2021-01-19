emacs.d
=======

A collection of my Emacs settings.

Setup
=====

For the most part, simply clone the git repo to `~/.emacs.d` and launch Emacs!

Some programming languages require extra tools, for the full suite I'd go with:
* `git`
* `ccls`
* `clang-format`
* `python2` and `python3`
* `npm`
* `rustup`
* `rust-analyzer`
* `ag`
* `rvm`
* `dotnet-core`
* `chicken`
* `sbcl`, `cmucl`, `ecl` or `armcl`
* `gdb`, `make`, `gcc`, etc that you'll find in `base-devel` or `build-essential` or similar

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

Or (preferred) launch an Emacs client in a login shell and have it create a server if necessary:

```
bash --login -c 'emacsclient -c -a ""'
```

Windows
=======

I recommend using [Scoop](https://scoop.sh/) to install packages.

And I create a shortcut to `Emacsclientw` similar to:
```
emacsclientw.exe -c -a ""
```

Using .projectile to Create a Project Root
==========================================

The following is an example project file for a simple Makefile project, using Projectile:

```
((nil . ((projectile-project-name . "001")
	 (projectile-enable-caching . nil)
	 (projectile-compilation-command . "make debug")
	 (compile-command . "make debug"))))
```

