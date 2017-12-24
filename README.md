emacs.d
=======

A collection of my Emacs settings.

For the most part it is configuration-minimal. If it can locate the package manager for a programming language it will use it to install dependencies, under the assumption that superuser permissions are _not_ required to do so.

When dependencies are missing it is logged in the `*Messages*` buffer; so if something isn't working, look there first. I recommend restarting once or twice after the first install in order to avoid all the Emacs package-install logs.

Setup
=====

* Link this repository to ~/.emacs.d

## Language-specific external requirements

### C++

Install clang somehow.

At least once, use `M-x` to execute `irony-install-server`

### C#

At least once, use `M-x` to execute `omnisharp-install-server`

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

### Rust

Install from the official distribution. 

### Go

Install Go from golang.org

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

Using .dir-locals.el for C/C++ Completion Configuration
=======================================================

Because I work on a fair amount of C/C++ projects in my hobby time I've developed a few hacks to get per-project include paths working seamlessly with the various tools I use with Emacs. The following is an example `.dir-locals.el` that will include all `build/{}/include` paths, and the project root; as well as configure the C/C++ standards appropriately.

```
((nil . ((tab-width . 2)
				 (eval .
							 (progn
								 (let* ((build-root (file-name-as-directory (f-join (dir-locals-dir) "build")))
												(targets (delete ".." (delete "." (directory-files build-root))))
												(target-dirs (mapcar (lambda (d) (file-name-as-directory (f-join build-root d))) targets)))
									 (dolist (d target-dirs)
										 (let ((include-dir (file-name-as-directory (f-join d "include"))))
											 (semantic-add-system-include include-dir)
											 (add-c-flycheck-path include-dir))))
								 (let ((include-dir (f-join (dir-locals-dir) "dl")))
									 (add-c-flycheck-path include-dir)
									 (semantic-add-system-include include-dir))))))
 (scheme-mode . ((indent-tabs-mode . nil)))
 (c-mode . ((indent-tabs-mode . nil)
						(eval . (progn
											(add-c-flycheck-arg "-std=c99")))))
 (c++-mode . ((indent-tabs-mode . nil)
							(eval . (progn
												(add-c-flycheck-arg "-std=c++14"))))))
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

