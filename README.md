emacs.d
=======

A collection of my Emacs settings.

Setup
=====

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

Using .projectile to Create a Project Root
==========================================

The following is an example project file for a simple Makefile project, using Projectile:

```
((nil . ((projectile-project-name . "001")
	 (projectile-enable-caching . nil)
	 (projectile-compilation-command . "make debug")
	 (compile-command . "make debug"))))
```

