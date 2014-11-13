emacs.d
=======

A collection of my emacs settings, tuned for Emacs 24.3 and higher.

Setup
=====

Everything will break if you don't do the following:

* Link this repository to ~/.emacs.d
* Install all the external tools and programming languages supported:

```
sudo apt-get install global python python-pip sbcl ruby chicken-bin nodejs nodejs-legacy npm

sudo pip install jedi epc virtualenv

sudo gem install pry pry-doc

sudo npm install tern

```

* The first time you launch it will require an active internet connection to fetch all the necessary packages

* Feel free to configure obvious things (like where to find tern) in `init.el`

* Check the "Keys" section for predefined custom keys

Making it Launch Quickly
========================

Run Emacs as a server:

```
emacs --daemon
```

Connect Emacs clients as desired:

```
emacsclient -c
```

