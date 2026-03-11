---
title: "Getting Started"
linkTitle: "Getting Started"
date: 2026-03-09
description: >
  An easy way to start with Lisp-Stat
---

It's never been easy for a developer to get started in Common Lisp. Emacs, though a powerful editor, isn't considered an IDE by modern standards.  Setting up a compiler, quicklisp, slime, swank, and then learning an entirely new programming paradigm has scared off many would-be entrants to the Common Lisp community.

Given the size of the Common Lisp community this is understandable. Making the new user experience smooth and frictionless as possible is _hard work_.  It's the kind of work that no one volunteers for; it's the kind of work you have to be _paid_ for.

Still, it's a pre-requisite for new users, so I've created [ls-dev-image](https://github.com/Lisp-Stat/ls-dev-image), a 'batteries included' OCI image for Lisp-Stat or just plain Common Lisp development.  Assuming you have an OCI (e.g Docker) runtime installed.  Here's how to get started.

## Shell
```sh
docker run --rm -it --user vscode -w /home/vscode ghcr.io/lisp-stat/ls-dev:latest bash
```

Now you're in the container with a `bash` shell and can configure the machine as you like.


## Common Lisp REPL

If you want a Common Lisp REPL:

```sh
docker run --rm -it --user vscode -w /home/vscode ghcr.io/lisp-stat/ls-dev:latest ls-repl
```

and you should see:
```
Linedit version 0.17.6, smart mode, ESC-h for help.
CL-USER(1):
```

You could also have gotten here from the shell with the `sbcl` command. This is a bare common lisp image, with nothing loaded. Now you load lisp-stat from the Quicklisp repositories with:

```
(ql:quickload :lisp-stat)
... lot's of compilation output ...
[package ls-user]
(:LISP-STAT)
CL-USER(2):
```

and to start working with Lisp-Stat:

```lisp
(in-package :ls-user
```

This REPL has been configured with a few packages to make it easier to work with.

* [linedit](https://github.com/sharplispers/linedit), provides customizable line-editing.  You can use emacs key bindings to edit the REPL commands.
* [Acl-repl](https://www.sbcl.org/manual/#sb_002daclrepl) is a SBCL extension that gives you command history (via up/down arrows) and some short command codes such as `:cs` for `compile-system`, `:ts` for `test-system`, etc.

Generally you won't be doing development with the REPL, you'll be doing it in emacs.


## Emacs/Slime


Now you can use Common Lisp with emacs, quicklisp and slime. From the shell you can type 'emacs' followed by 'M-x slime' and start hacking Common Lisp.

<img width="1306" height="762" alt="image" src="https://github.com/user-attachments/assets/1abaeea4-1bee-494b-9dd4-c590c9a66f5c" />


## Lisp-Stat
The upstream repositories are often out of date.  To get the latest and to keep them synced you can use the `init.sh` script with `ls-init.sh --mode experimenter`.  Experimenter will download the repos so you can try out the source code, but you won't be able to push your changes or make pull requests.  For that you want to be a 'contributor': `ls-init.sh --mode contributor`.

Now when you start `ls-repl` you should see the lisp-stat REPL prompt, indicating you're in the `LS-USER` package:

```sh
ls-init.sh --mode experimenter
# ... output from repo checkouts, linking, configuration
```

```sh
ls-repl
```
lot's of recompilation because you're now using local source repos and then:

```
...
To load "ls-server":
  Load 1 ASDF system:
    ls-server
; Loading "ls-server"
.....

Linedit version 0.17.6, smart mode, ESC-h for help.
LS-USER(1):
```

From here `emacs` and `M-x slime` (from the shell) will load Lisp-Stat.

## LS-Server

A [ls-server](https://github.com/Lisp-Stat/ls-server) is also configured to start automatically on port 20202.  If you open your browser and point it to https://localhost:20202 (or other port, depending on your OCI container configuration) you'll see the web interface for displaying plots and viewing/editing data-frames.

<img width="1038" height="704" alt="image" src="https://github.com/user-attachments/assets/98c0e707-b259-48e4-93c1-1ba7f62e3f09" />

You can also run this OCI image in GitHub codespaces.

## Staying up-to-date

You will want to run `docker pull` occasionally to get the latest `ls-dev-image`.  This should stabilize in a month or so and after that it won't need to be updated frequently.  To keep lisp-stat source in sync, run `ls-init.sh --refresh`.  If you want help with `ls-init.sh`, run `ls-init.sh --help`.

Contributions and [bug reports](https://github.com/Lisp-Stat/ls-dev-image/issues) are welcome and encouraged.
