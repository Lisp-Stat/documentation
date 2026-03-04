---
title: "Installation"
date: 2022-06-18
weight: 1
description: >
  Installing and configuring Lisp-Stat
---

## Notebook

{{< figure src="https://mybinder.org/badge_logo.svg" alt="Binder" link="https://mybinder.org/v2/gh/Lisp-Stat/IPS9/HEAD?urlpath=%2Fdoc%2Ftree%2Findex.ipynb" >}}

The easiest way to get started is with the link above which will open a preconfigured
notebook on mybinder.org.

Users new to lisp should also consider going through the Lisp-Stat
[basic tutorial](/docs/tutorials/basics), which guides you
step-by-step through the basics of working with Lisp as a statistics
practitioner.

## OCI/Docker

You can also run a pre-built OCI image.  This is a minimal Docker file:

```
FROM ghcr.io/lisp-stat/cl-jupyter:latest
```

Our images are based on [Jupyter Docker Stacks](https://jupyter-docker-stacks.readthedocs.io/en/latest/using/running.html) and all of their
documentation is applicable to the `cl-jupyter` image.

For a quickstart:

```shell
docker run -it -p 8888:8888 ghcr.io/lisp-stat/cl-jupyter:latest

# Entered start.sh with args: jupyter lab

# ...

#     To access the server, open this file in a browser:
#         file:///home/jovyan/.local/share/jupyter/runtime/jpserver-7-open.html
#     Or copy and paste one of these URLs:
#         http://eca4aa01751c:8888/lab?token=d4ac9278f5f5388e88097a3a8ebbe9401be206cfa0b83099
#         http://127.0.0.1:8888/lab?token=d4ac9278f5f5388e88097a3a8ebbe9401be206cfa0b83099
```

This command pulls the latest `cl-jupyter` image from ghcr.io if it is not already present on the local host. It then starts a container running a Jupyter Server with the JupyterLab frontend and exposes the server on host port 8888. The server logs appear in the terminal and include a URL to the server.

## Initialization file

You can put customisations to your environment in either your
implementation's init file, or in a personal init file and load it
from the implementation's init file.  For example, I keep my
customisations in `#P"~/ls-init.lisp"` and load it from SBCL's init
file `~/.sbclrc` in a Lisp-Stat initialisation section like this:

```lisp
;;; Lisp-Stat
(asdf:load-system :lisp-stat)
(load #P"~/ls-init.lisp")
```

Settings in your personal lisp-stat init file override the system defaults.

Here's an example `ls-init.lisp` file that loads some common R data sets:

```lisp
(defparameter *default-datasets*
  '("tooth-growth" "plant-growth" "usarrests" "iris" "mtcars")
  "Data sets loaded as part of personal Lisp-Stat initialisation.
  Available in every session.")

(map nil #'(lambda (x)
	     (format t "Loading ~A~%" x)
	     (data x))
	     *default-datasets*)
```

With this init file, you can immediately access the data sets in the
`*default-datasets*` list defined above, e.g.:

```lisp
(head iris)
;;   X2 SEPAL-LENGTH SEPAL-WIDTH PETAL-LENGTH PETAL-WIDTH SPECIES
;; 0  1          5.1         3.5          1.4         0.2 setosa
;; 1  2          4.9         3.0          1.4         0.2 setosa
;; 2  3          4.7         3.2          1.3         0.2 setosa
;; 3  4          4.6         3.1          1.5         0.2 setosa
;; 4  5          5.0         3.6          1.4         0.2 setosa
;; 5  6          5.4         3.9          1.7         0.4 setosa
```

## Emacs / Hemlock

We assume an experienced user will have their own Emacs and lisp
implementation and will want to install according to their own tastes
and setup. The repo links you need are below, or you can install with
`quicklisp`.

### Prerequisites

All that is needed is an ANSI Common Lisp implementation.  Development
is done with SBCL.  Other platforms _should_ work, but will
not have been tested, nor can we offer support (maintaining & testing
on multiple implementations requires more resources than the project
has available).  Note that CCL is not in good health, and there are a
few numerical bugs that remain unfixed.  A shame, as we really liked
CCL.

You may want to consider [emacs-vega-view](https://github.com/applied-science/emacs-vega-view)
for viewing plots from within emacs.

### Installation

The easiest way to install Lisp-Stat is via
[Quicklisp](https://www.quicklisp.org/beta/), a library manager for
Common Lisp.  It works with your existing Common Lisp implementation to
download, install, and load any of over 1,500 libraries with a few
simple commands.

Quicklisp is like a package manager in Linux.  It can load packages
from the local file system, or download them if required.  If you have
quicklisp installed, you can use:

```lisp
(ql:quickload :lisp-stat)
```

Quicklisp is good at managing the project dependency retrieval, but
most of the time we use ASDF because of its REPL integration.  You only
have to use Quicklisp once to get the dependencies, then use ASDF for
day-to-day work.

You can install additional Lisp-Stat modules in the same way.  For example to install the `CEPHES` module:

```lisp
(ql:quickload :cephes)
```

### Loading

Once you have obtained Lisp-Stat via Quicklisp, you can load in one of two ways:

- ASDF
- Quicklisp

#### Loading with ASDF

```lisp
(asdf:load-system :lisp-stat)
```

If you are using emacs, you can use the [slime
shortcuts](https://slime.common-lisp.dev/doc/html/Shortcuts.html) to
load systems by typing `,` and then `load-system` in the mini-buffer.
This is what the Lisp-Stat developers use most often, the shortcuts
are a helpful part of the workflow.

#### Loading with Quicklisp

To load with Quicklisp:
```lisp
(ql:quickload :lisp-stat)
```

Quicklisp uses the same ASDF command as above to load Lisp-Stat.

### Updating Lisp-Stat

When a new release is announced, you can update via Quicklisp like so:

```lisp
(ql:update-dist "lisp-stat")
```

### Documentation

You can install the info manuals into the emacs help system and this
allows searching and browsing from within the editing environment.  To
do this, use the
[install-info](https://www.gnu.org/software/texinfo/manual/texinfo/html_node/Invoking-install_002dinfo.html)
command.  As an example, on my MS Windows 10 machine, with MSYS2/emacs
installation:

```shell
install-info --add-once select.info /c/msys64/mingw64/share/info/dir
```

installs the `select` manual at the top level of the info tree.  You
can also install the common lisp hyperspec and browse documentation
for the base Common Lisp system.  This really is the best way to use
documentation whilst programming Common Lisp and Lisp-Stat.  See the
[emacs external
documentation](https://www.emacswiki.org/emacs/ExternalDocumentation)
and "[How do I install a piece of Texinfo
documentation?](https://www.gnu.org/software/emacs/manual/html_node/efaq/Installing-Texinfo-documentation.html)"
for more information on installing help files in emacs.

See [getting help](/docs/getting-started/getting-help/) for
information on how to access Info documentation as you code.  This is
the mechanism used by Lisp-Stat developers because you don't have to
leave the emacs editor to look up function documentation in a browser.


### Try it out

Load Lisp-Stat:
```lisp
(asdf:load-system :lisp-stat)
```

Change to the Lisp-Stat user package:
```lisp
(in-package :ls-user)
```

Load some data:

```lisp
(data :sg-weather)
```

Find the sample mean and median:

```lisp
(mean sg-weather:precipitation) ;=> .0714
(median sg-weather:max-temps)   ;=> 31.55
```

## Next steps

{{< cardpane >}}
  {{% card header="**Get Started**" %}}
  [Get Help](/docs/getting-started/getting-help)<br/>
  [Plot in 5 minutes](/docs/getting-started/)
  {{% /card %}}
  {{% card header="**Examples**" %}}
  [Analytics](/docs/examples/statistics)<br/>
  [Plotting](/docs/examples/plotting)
  {{% /card %}}
  {{% card header="**R Users**" %}}
  [Basic tutorial](/docs/tutorials/basics)<br/>
  [Data Frame](/docs/tutorials/data-frame/)
  {{% /card %}}
{{< /cardpane >}}


