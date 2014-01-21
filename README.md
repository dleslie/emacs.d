emacs.d
=======

A collection of my emacs settings, tuned for Emacs 24.2 and higher.

Setup
=====

* Link this repository to ~/.emacs.d
* Add the following to ~/.emacs

```
(defun post-init-hook ()
  (load "~/.emacs.d/post-init.el"))

(add-hook 'after-init-hook 'post-init-hook)
```

* Jedi Mode requires additional configuration, but is so worth it:

```
pip install jedi
```

* C++ auto-complete support requires clang and is bound to C-return.
JIT auto-complete with clang is a tad too slow to be bearable on my netbook.

* Scheme is currently configured to use my chicken-scheme mode, and so requires Chicken Scheme 4.8.0 or higher installed. If you use another Scheme then I highly recommend Geiser.

Clang Completion Notes
======================

You may need to perform some additional configuration to get clang to auto-complete appropriately. To do so, modify the following variable in post-init.el:

```lisp
(setq ac-clang-flags '("-I/usr/local/include"
                 "-I/usr/include/c++/4.7/"
                 "-I/usr/include/x86_64-linux-gnu/c++/4.7/"
                 "-I/usr/include"
                 "-I/usr/include/x86_64-linux-gnu"
                 "-I/usr/include/clang/3.2/include"
                 "-std=c++11"
                 "-pthread"
                 "-D__GCC_HAVE_SYNC_COMPARE_AND_SWAP_1"
                 "-D__GCC_HAVE_SYNC_COMPARE_AND_SWAP_2"
                 "-D__GCC_HAVE_SYNC_COMPARE_AND_SWAP_4"
                 "-D__GCC_HAVE_SYNC_COMPARE_AND_SWAP_8"))
```

Generally, if clang fails to complete then there's something wrong with your code or a missing ac-clang-flags entry, you can check the \*clang-error\* buffer for more information.

Additional Package Requirements
===============================

At the moment, these are the extra elpa packages I have installed, some of which will be required to make this emacs configuration work:

```
elpa/auto-complete-20131118.1433/
elpa/cl-lib-0.3/
elpa/concurrent-20130914.536/
elpa/ctable-20131202.2114/
elpa/deferred-20130930.607/
elpa/epc-20130803.2228/
elpa/gh-20131110.1521/
elpa/gist-20131109.2155/
elpa/git-commit-mode-20131120.2324/
elpa/git-rebase-mode-20131005.1730/
elpa/jedi-20130714.1228/
elpa/logito-20120225.2155/
elpa/magit-20131122.1539/
elpa/magit-filenotify-20131018.1053/
elpa/magit-gh-pulls-20130405.1828/
elpa/magit-svn-20131117.2059/
elpa/nyan-mode-20120710.1922/
elpa/paredit-20130722.1324/
elpa/paredit-everywhere-20131010.1118/
elpa/parenface-20091203.1917/
elpa/pcache-20120408.1206/
elpa/popup-20130708.2245/
elpa/rainbow-delimiters-20131015.1304/
elpa/rainbow-mode-0.9/
elpa/seclusion-mode-1.1.1/
```
