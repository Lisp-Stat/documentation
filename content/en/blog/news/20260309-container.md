---
title: "Getting Started"
linkTitle: "Getting Started"
date: 2026-03-09
description: >
  An easy way to start with Lisp-Stat
---

It's never been easy for a developer to get started in Common Lisp. Emacs, though a powerful editor, isn't considered an IDE by modern standards.  Setting up a compiler, quicklisp, slime, swank, and then learning an entirely new programming paradigm has scared off many would-be entrants to the Common Lisp community.

Given the size of the Common Lisp community this is understandable. Making the new user experience smooth and frictionless as possible is _hard work_.  It's the kind of work that no one volunteers for; it's the kind of work you have to be _paid_ for.

Still, it's a pre-requisite for new users, so I've created [ls-dev-image](https://github.com/Lisp-Stat/ls-dev-image), a 'batteries included' OCI image for Lisp-Stat or just plain Common Lisp development.  Assuming you have an OCI (e.g Docker) runtime installed, you can get started with:

```sh
docker run --rm -it --user vscode -w /home/vscode ghcr.io/lisp-stat/ls-dev:latest bash
```

Now install lisp-stat with `ls-init.sh --mode experimenter` and from the shell you can type 'emacs' followed by 'M-x slime' and start hacking Common Lisp.

<img width="1306" height="762" alt="image" src="https://github.com/user-attachments/assets/1abaeea4-1bee-494b-9dd4-c590c9a66f5c" />


This OCI image is configured with emacs, slime, quicklisp, lisp-stat and a few sample data sets and plots.  A [ls-server](https://github.com/Lisp-Stat/ls-server) is also configured to start automatically on port 20202.  If you open your browser and point it to https://localhost:20202 (or other port, depending on your OCI container configuration) you'll see the web interface for displaying plots and viewing/editing data-frames.  It also has a 'refresh' script so that you can stay synced with the upstream lisp-stat, which is being updated frequently.

<img width="1038" height="704" alt="image" src="https://github.com/user-attachments/assets/98c0e707-b259-48e4-93c1-1ba7f62e3f09" />

You can also run this image in GitHub codespaces.

I'd like to point out that you can use this for _any_ Common Lisp development, not just Lisp-Stat.  Contributions and [bug reports](https://github.com/Lisp-Stat/ls-dev-image/issues) are welcome and encouraged.