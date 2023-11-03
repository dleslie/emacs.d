emacs.d
=======

A collection of my Emacs settings.

Setup
=====

For the most part, simply clone the git repo to `~/.emacs.d` and launch Emacs!

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

I create a shortcut to `Emacsclientw` similar to:
```
emacsclientw.exe -c -a ""
```
