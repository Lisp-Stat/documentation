---
title: "Installation"
date: 2021-04-27
weight: 1
description: >
  Automated and manual installation
---

## New to Lisp

If you are a Lisp newbie and want to get started as fast as possible,
then Portacle is probably your best option. Portacle is a
multi-platform IDE for Common Lisp that includes Emacs, SBCL, Git,
Quicklisp, all configured and ready to use.

<div class="mx-auto">
	<a class="btn btn-lg btn-primary mr-3 mb-4" href="https://portacle.github.io/">
		Download Portacle<i class="fas fa-arrow-alt-circle-right ml-2"></i>
	</a>
</div>

If you are an existing emacs user, you can [configure emacs for Common
Lisp](https://github.com/susam/emacs4cl).

Users new to lisp should also consider going through the [basic
tutorial](/docs/tutorials/basics), which guides you step-by-step
through the basics of working with Lisp as a statistics practitioner.

## Experienced with Lisp

We assume an experienced user will have their own Emacs and lisp
implementation and will want to install according to their own tastes
and setup. The repo links you need are below, or you can install with
quicklisp.

## Prerequisites

All that is needed is an ANSI Common Lisp implementation. Development
is done with CCL and SBCL. Other platforms _should_ work, but will not
have been tested.

## Installation

### Automated install
The easiest way to install Lisp-Stat is with Quicklisp:

```lisp
(ql:quickload :lisp-stat)
```

### Manual install
If you want to modify Lisp-Stat you'll need to retrieve the
files from github and place them in a directory that is known to
quicklisp. This long shell command will checkout all the required
systems:

```shell
cd ~/quicklisp/local-projects && \
git clone https://github.com/Lisp-Stat/data-frame.git && \
git clone https://github.com/Lisp-Stat/dfio.git && \
git clone https://github.com/Lisp-Stat/special-functions.git && \
git clone https://github.com/Lisp-Stat/numerical-utilities.git && \
git clone https://github.com/Lisp-Stat/documentation.git && \
git clone https://github.com/Lisp-Stat/plot.git && \
git clone https://github.com/Lisp-Stat/select.git && \
git clone https://github.com/Lisp-Stat/lisp-stat.git
```

The above assumes you have the default installation directories. Adjust
accordingly if you have changed this. If Quicklisp claims it cannot
find the systems, try this at the REPL:

```lisp
(ql:register-local-projects)
```

### Documentation

Lisp-Stat reference manuals are generated with the
[declt](https://github.com/didierverna/declt) system. This produces
high quality PDFs, markdown, HTML and Info output.  The API reference
manuals are available in HTML in the [reference](/docs/reference)
section of this website; PDF and Info files that can be download from
the individual systems `docs/` directory.

You can install the info manuals into the emacs help system and this
allows searching and browsing from within the editing environment.  To
do this, use the
[install-info](https://www.gnu.org/software/texinfo/manual/texinfo/html_node/Invoking-install_002dinfo.html)
command.  As an example, on my MS Windows 10 machine, with MSYS2/emacs
installation:

```shell
install-info --add-once select.info /c/msys64/mingw64/share/info/dir
```

installs the `select` manual into a Lisp-Stat node at the top level of
the info tree.

### Initialization file

You can put customisations to your environment in the user
initialisation file, `#P"~/ls-init.lisp"`. It is loaded after the
internal lisp-stat initialisation file, and settings here override the
defaults.

Here's an example `ls-init.lisp` file that loads some common R data sets.

```lisp
;; Define common data sets
(ql:quickload :lisp-stat/rdata)
(in-package #:ls-user)
(define-data-frame mtcars
    (read-csv (rdata:rdata 'rdata:datasets 'rdata:mtcars)))
(define-data-frame iris
    (read-csv (rdata:rdata 'rdata:datasets 'rdata:iris)))
(define-data-frame tooth-growth
    (read-csv (rdata:rdata 'rdata:datasets 'rdata:toothgrowth)))
(define-data-frame plant-growth
    (read-csv (rdata:rdata 'rdata:datasets 'rdata:plantgrowth)))
(define-data-frame us-arrests
    (read-csv (rdata:rdata 'rdata:datasets 'rdata:usarrests)))
```

With this init file, you can immediately access the data sets, e.g.:

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
(ql:quickload :lisp-stat)
```

Change to the Lisp-Stat user package:
```lisp
(in-package :ls-user)
```

Load some data:

```lisp
(load #P"LS:DATASETS;CAR-PRICES")
```

Find the sample mean and median:

```lisp
(mean car-prices)
(median car-prices)
```

## Next steps

{{< cardpane >}}
  {{< card header="**Get Started**" >}}
  [Load & plot](/docs/getting-started/)<br/>
  [Data Frame](/docs/getting-started/data-frame/)
  {{< /card >}}
  {{< card header="**Examples**" >}}
  [Analytics](/docs/examples/notebooks)<br/>
  [Plotting](/docs/examples/plotting)
  {{< /card >}}
  {{< card header="**R Users**" >}}
  [Basic tutorial](/docs/tutorials/basics)
  {{< /card >}}
{{< /cardpane >}}


