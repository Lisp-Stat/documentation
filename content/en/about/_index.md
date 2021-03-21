---
title: About Lisp-Stat
linkTitle: About
menu:
  main:
    weight: 10
layout: docs
---

{{% blocks/cover title="About Lisp-Stat" height="auto" %}}

Lisp-Stat is an environment for statistical computing, conceptually
similar to R, that is also suitable for front-line production
deployments.  It grew out of a desire to have an environment for
rapidly prototyping analytical and A.I. solutions, and move directly to
production environments with minimal friction.  Typically there is a
gap between the modeling environment, perhaps R or Python, and the
delivery environment. Lisp-Stat closes that gap.

{{% /blocks/cover %}}

{{% blocks/section type="section" color="primary" %}}
## Why Lisp?
We had a few requirements when evaluating options.  Specifically the
system had to:

- Work well in the kind of exploratory environment conducive to analytics and AI
- Be robust enough to work in an enterprise-level production environment
- Be available under a license without source code restrictions

Common Lisp was the only framework that met all these
requirements.  And, it is not the first time lisp was used in a
statistical setting.  [XLISP-STAT](https://en.wikipedia.org/wiki/XLispStat),
our spiritual predecessor, was a contemporary of R in the early days of
development.  Wikipedia says about it: "XLispStat was historically
influential in the field of statistical visualization" and its author,
Luke Tierney, was a member of the original R core team.

{{% /blocks/section %}}


{{% blocks/section type="section" color="white" %}}

## What does Lisp-Stat do?

Lisp-Stat provides support for vectorized mathematical operations, and
a comprehensive set of statistical methods that are implemented using
the latest numerical algorithms. In addition, Common Lisp provides a
dynamic programming environment
([REPL](https://en.wikipedia.org/wiki/Read%E2%80%93eval%E2%80%93print_loop)),
an excellent object-oriented facility
([CLOS](https://en.wikipedia.org/wiki/Common_Lisp_Object_System))
and meta-object protocol
([MOP](https://en.wikipedia.org/wiki/Metaobject#Metaobject_protocol)).

Lisp-Stat is fully functional today, and most of the XLISP-STAT
libraries can be ported with the aid of a compatibility package
(XLS). This gives Lisp-Stat a leg up on ecosystem development. Though
not as complete as CRAN, there is enough here to get useful work done.

### Data-Frame

Lisp-Stat includes a column-oriented data-frame. I/O methods
are provided for delimited data files and JSON formats. Data may be
loaded from the network or local disk.

### Notebooks

[JupyterLab](http://jupyterlab.io/), along with
[common-lisp-jupyter](https://github.com/yitzchak/common-lisp-jupyter/)
are used to provide notebook style environments for reproducible
research.

### Visualization

Plotting is done with Vega-Lite. There are a few ways to plot; see the
documentation and tutorials for example. Plotly is not yet supported,
and we would welcome someone picking up that ball and running with it.

### XLisp-Stat Compatibility

The XLS package implements many of the XLisp-Stat functions using
Lisp-Stat equivalents. This package is working internally, but not yet
released. If you need this, please raise an issue on github.

### Git Integration

Git workflows are used both in development and in projects.


{{% /blocks/section %}}

{{% blocks/section type="section" color="primary" %}}

## What's next for Lisp-Stat?

Lisp-Stat is an open source project and we welcome patches and
contributions to improve Lisp-Stat. Both code and documentation help,
and documenting the systems is an excellent way to learn the ins and
outs of a statistical system whilst it is small enough to be
managable.  We hope to continue to make improvements to the system
along with the Lisp-Stat community.

Visit the [github repository](https://github.com/lisp-stat/) to
see what we're currently working on. If there is something you would like
to see in Lisp-Stat, please create an issue yourself - or assign
yourself an issue if you would like to fix or add something. See our
[contribution guidelines](/docs/contributing/) for more
information.

{{% /blocks/section %}}




