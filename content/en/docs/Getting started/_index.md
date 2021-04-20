---
title: "Getting Started"
linkTitle: "Getting Started"
weight: 2
description: >
  Installation, setup and a simple sample analysis
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

## Experienced with Lisp

We assume an experienced user will have their own Emacs and lisp
implementation and will want to install according to their own tastes
and setup. The repo links you need are below.

## Prerequisites

All that is needed is an ANSI Common Lisp implementation. Development
is done with CCL and SBCL. Other platforms _should_ work, but will not
have been tested.

## Installation

The easiest way to install Lisp-Stat is with Quicklisp:

```lisp
(ql:quickload :lisp-stat)
```

### Manual Install
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

### Installing documentation

Lisp-Stat reference manuals are generated with the
[declt](https://github.com/didierverna/declt) system. This produces
high quality PDFs, markdown (soon), HTML and Info output.  The manuals
are available in HTML, <!-- in the [reference](/docs/reference)
section of this website;--> PDF and Info files that can be download
from the individual systems `docs/` directory.

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
(mean 'car-prices)
(median 'car-prices)
```


