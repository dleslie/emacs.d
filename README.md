emacs.d
=======

A collection of my emacs settings, tuned for Emacs 24.3 and higher.

Setup
=====

This is Linux-focused, but wherever possible this configuration also works with Windows. I use MSYS2 alongside of the official distributions of the various supported languages.

* Link this repository to ~/.emacs.d
* Install gnutls and certifi, so we can use TLS properly:

```
sudo apt-get install gnutls-bin
python -m pip install --user certifi
```

## (Optional, Windows) MSYS2 dependencies

```
pacman -Sy
pacman -S mingw-w64-cross-toolchain base-devel mingw64/mingw-w64-x86_64-cmake global
```

A Google Drive repository of tools resides here:
https://drive.google.com/folderview?id=0B868nNRNhuc2bFRhSmVXbXIwcXM&usp=sharing

## (Optional) Install extra stuff to use particular languages and features

### C/C++

```
sudo apt-get install global
```

### w3m

```
sudo apt-get install  w3m w3m-img
```

###Clojure

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
cd `csi -p '(chicken-home)'`
curl http://3e8.org/pub/chicken-doc/chicken-doc-repo.tgz | sudo tar zx
```

### Python

```
sudo apt-get install python python-pip 
python -m pip install jedi epc virtualenv
```

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

```
sudo apt-get install haskell-platform
cabal update
cabal install ghc-mod
```

## Notes

* The first time you launch it will require an active internet connection to fetch all the necessary packages

* Feel free to configure obvious things (like where to find tern) in `configuration.el`

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

Email Configuration
===================

mu4e is a mail client that requires a little bit of extra legwork to get running.

Install offlineimap and html2text:

```
sudo apt-get install offlineimap html2text
```

Build and install mu4e:
https://github.com/djcb/mu

Create ~/.offlineimaprc

```
[general]
accounts = Gmail
maxsyncaccounts = 3

[Account Gmail]
localrepository = Local
remoterepository = Remote

[Repository Local]
type = Maildir
localfolders = ~/Maildir

[Repository Remote]
type = IMAP
remotehost = imap.gmail.com
remoteuser = YOURNAME@gmail.com
remotepass = SUPERSECRETPASSWORDOFDOOOOOOOOM
ssl = yes
maxconnections = 1
realdelete = no
sslcacertfile = /etc/ssl/certs/ca-certificates.crt
folderfilter = lambda folder: folder not in ['Trash', 'Starred', 'Important', 'Drafts', 'Spam']
```

And finally, create ~/.authinfo

```
machine imap.gmail.com login YOURNAME@gmail.com port 993 password SUPERSECRETPASSWORDOFDOOOOOOOOM
machine smtp.gmail.com login YOURNAME@gmail.com port 587 password SUPERSECRETPASSWORDOFDOOOOOOOOM
```

