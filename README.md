emacs.d
=======

A collection of my emacs settings, tuned for Emacs 24.2 and higher.

Setup
=====

* Link this repository to ~/.emacs.d
* Add the following to ~/.emacs

```
(load-file (concat user-emacs-directory "/cedet/cedet-devel-load.el"))
(load-file (concat user-emacs-directory "/cedet/contrib/cedet-contrib-load.el"))

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

Additional Package Requirements
===============================

At the moment, these are the extra elpa packages I have installed, some of which will be required to make this emacs configuration work:

```
async-20140913.2050
auto-complete-20140414.2324
auto-complete-exuberant-ctags-20140320.24
cl-lib-0.5
concurrent-20140609.1940
ctable-20140304.1659
dash-20140811.523
deferred-20140816.2205
epc-20140609.2234
epl-20140823.609
f-20140828.716
function-args-20140918.423
ggtags-20140812.2036
gh-20140706.1506
ghc-20140915.2022
gist-20140706.1503
git-commit-mode-20140831.1359
git-rebase-mode-20140605.520
jedi-20140321.1323
logito-20120225.2155
magit-20140908.1424
magit-filenotify-20140807.340
magit-gh-pulls-20140919.1201
magit-svn-20140807.909
nyan-mode-20140801.1329
paredit-20140128.1248
parenface-20140322.1823
pcache-20131201.1159
pkg-info-20140610.630
popup-20140815.629
python-environment-20140321.1116
qsimpleq-theme-0.1.3
rainbow-delimiters-20140917.323
rainbow-mode-0.10
s-20140910.334
seclusion-mode-20121118.1553
sublime-themes-20140920.500
xmlgen-20130219.219
zenburn-theme-20140811.754
```
