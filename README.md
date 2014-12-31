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

pip install jedi epc virtualenv

gem install pry pry-doc

npm install tern

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

Advanced Email Setup
====================

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

