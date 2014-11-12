emacs.d
=======

A collection of my emacs settings, tuned for Emacs 24.3 and higher.

Setup
=====

Everything will break if you don't do the following:

* Link this repository to ~/.emacs.d
* Install all the external tools:

```
sudo apt-get install global

sudo pip install jedi epc virtualenv

sudo gem install pry pry-doc
```

* The first time you launch it will require an active internet connection to fetch all the necessary packages
