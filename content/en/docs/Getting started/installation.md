---
title: "Installation"
date: 2022-06-18
weight: 1
description: >
  Installing and configuring Lisp-Stat
---

## New to Lisp

If you are a Lisp newbie and want to get started as fast as possible,
then Portacle is your best option.  Portacle is a multi-platform IDE
for Common Lisp that includes Emacs, SBCL, Git, Quicklisp, all
configured and ready to use.

<div class="mx-auto">
	<a class="btn btn-lg btn-primary me-3 mb-4" href="https://portacle.github.io/">
		Download Portacle<i class="fas fa-arrow-alt-circle-right ms-2"></i>
	</a>
</div>

Users new to lisp should also consider going through the Lisp-Stat
[basic tutorial](/docs/tutorials/basics), which guides you
step-by-step through the basics of working with Lisp as a statistics
practitioner.

If you currently use emacs for other languages, you can [configure
emacs for Common Lisp](https://github.com/susam/emacs4cl).

## Experienced with Lisp

We assume an experienced user will have their own Emacs and lisp
implementation and will want to install according to their own tastes
and setup. The repo links you need are below, or you can install with
`clpm` or `quicklisp`.

## Prerequisites

All that is needed is an ANSI Common Lisp implementation.  Development
is done with Genera and SBCL.  Other platforms _should_ work, but will
not have been tested, nor can we offer support (maintaining & testing
on multiple implementations requires more resources than the project
has available).  Note that CCL is not in good health, and there are a
few numerical bugs that remain unfixed.  A shame, as we really liked
CCL.

## Installation

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

You can install additional Lisp-Stat modules in the same way.  For example to install the `SQLDF` module:

```lisp
(ql:quickload :sqldf)
```

## Loading

Once you have obtained Lisp-Stat via Quicklisp, you can load in one of two ways:

- ASDF
- Quicklisp

### Loading with ASDF

```lisp
(asdf:load-system :lisp-stat)
```

If you are using emacs, you can use the [slime
shortcuts](https://slime.common-lisp.dev/doc/html/Shortcuts.html) to
load systems by typing `,` and then `load-system` in the mini-buffer.
This is what the Lisp-Stat developers use most often, the shortcuts
are a helpful part of the workflow.

### Loading with Quicklisp

To load with Quicklisp:
```lisp
(ql:quickload :lisp-stat)
```

Quicklisp uses the same ASDF command as above to load Lisp-Stat.

## Updating Lisp-Stat

When a new release is announced, you can update via Quicklisp like so:

```lisp
(ql:update-dist "lisp-stat")
```



<!--
### ASDF
If you want to modify Lisp-Stat you'll need to retrieve the
files from github and place them in a directory that is known to
ASDF. This long shell command will checkout all the required
systems:

```shell
cd ~/quicklisp/local-projects && \
git clone https://github.com/Lisp-Stat/data-frame.git && \
git clone https://github.com/Lisp-Stat/dfio.git && \
git clone https://github.com/Lisp-Stat/special-functions.git && \
git clone https://github.com/Lisp-Stat/numerical-utilities.git && \
git clone https://github.com/Lisp-Stat/array-operations.git && \
git clone https://github.com/Lisp-Stat/documentation.git && \
git clone https://github.com/Lisp-Stat/distributions.git && \
git clone https://github.com/Lisp-Stat/plot.git && \
git clone https://github.com/Lisp-Stat/select.git && \
git clone https://github.com/Lisp-Stat/cephes.cl.git && \
git clone https://github.com/Symbolics/alexandria-plus && \
git clone https://github.com/Lisp-Stat/statistics.git && \
git clone https://github.com/Lisp-Stat/lisp-stat.git
```

The above assumes you are using the default installation directories
(~/common-lisp/).  Adjust accordingly if you have changed this. If
this is the first time running Lisp-Stat, use Quicklisp to get the
dependencies:

```lisp
(ql:quickload :lisp-stat)
```

From now on you can load it with:

```lisp
(asdf:load-system :lisp-stat)
```

If ASDF claims it can't find the required systems (this might happen
the first time around), reset the system configuration with:

```lisp
(asfd:clear-source-registry)
```

and try again.
-->


## IDEs {#ides}
There are a couple of IDE's to consider:

### Emacs {#emacs}

Emacs, with the [slime](https://common-lisp.net/project/slime/)
package is the most tested IDE and the one the authors use.  If you
are using one of the starter lisp packages mentioned in the [getting
started](/docs/getting-started/installation) section, this will have
been installed for you. Otherwise, slime/swank is available in
quicklisp and clpm.

### Jupyter Lab {#jupyter-lab}

[Jupyter Lab](http://jupyter.org/) and
[common-lisp-jupyter](https://github.com/yitzchak/common-lisp-jupyter)
provide an environment similar to RStudio for working with data and
performing analysis.  The [Lisp-Stat analytics
examples](/docs/examples/statistics) use Jupyter Lab to illustrate
worked examples based on the book, *Introduction to the Practice of
Statistics*.

### Visual Studio Code

This is a very popular IDE, with improving support for Common Lisp.
If you already use this editor, it is worth investigating to see if
the Lisp support is sufficient for you to perform an analysis.


## Documentation

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

## Try it out

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


