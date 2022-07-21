---
title: About Lisp-Stat
linkTitle: About
menu:
  main:
    weight: 10
layout: docs
---

{{% blocks/cover title="About Lisp-Stat" height="auto" %}}

Lisp-Stat is an environment for statistical computing


{{% /blocks/cover %}}

{{% blocks/section type="section" color="primary" %}}

Lisp-Stat is conceptually similar to R and will be familiar to most
people from that ecosystem.  It is suitable for both exploratory data
analysis as well as front-line production deployments.  Common Lisp is
currently used at Google in several high-availability, high-volume
transactional systems.

## Why Lisp?
We had a few requirements when evaluating options.  Specifically the
system had to:

- Work well in the kind of exploratory environment conducive to analytics and AI
- Be robust enough to work in an enterprise production environment
- Be available under a license without source code restrictions

Common Lisp was the only framework that met all these requirements.

Probably the most important reasons though are given in the paper by
Ross Ihaka, one of the originators of the R language, [Lisp as a Base
for a Statistical Computing
System](https://www.stat.auckland.ac.nz/~ihaka/downloads/Compstat-2008.pdf)
about the deficiencies in R and the inability to compile to
machine code (among other issues). The same is true of Python. In that
paper he argues for Lisp as a replacement for R.

{{% /blocks/section %}}


{{% blocks/section type="section" color="white" %}}

## What does Lisp-Stat do?

Lisp-Stat provides support for vectorized mathematical operations and
a comprehensive set of statistical methods that are implemented using
the latest numerical algorithms.  In addition, Common Lisp provides a
dynamic programming environment
([REPL](https://en.wikipedia.org/wiki/Read%E2%80%93eval%E2%80%93print_loop)),
an excellent object-oriented facility
([CLOS](https://en.wikipedia.org/wiki/Common_Lisp_Object_System))
and meta-object protocol
([MOP](https://en.wikipedia.org/wiki/Metaobject#Metaobject_protocol)).

Lisp-Stat is functional today and in daily use on several projects. It has an [archive of libraries](https://github.com/Lisp-Stat/xls-archive) from XLISP-STAT that can be used with the aid of a compatibility package ([XLS-compat](https://github.com/Lisp-Stat/XLS-compat)).  The archive includes packages for linear models, KNN, advanced statistics, temporal/spatial reasoning and a Lisp version of the [NSWC Library of Mathematics Subroutines](https://ntrl.ntis.gov/NTRL/dashboard/searchResults/titleDetail/ADA261511.xhtml). It also includes the [Cephes mathmatical library](https://github.com/Lisp-Stat/cephes.cl) for accurate statistical distribution calculations.

For more on what Lisp-Stat can do, see [getting started](/docs/getting-started/) and the [overview](/docs/overview).

{{% /blocks/section %}}


{{% blocks/section type="section" color="primary" %}}
## What's next for Lisp-Stat?

Lisp-Stat is an open source project and we welcome patches and
contributions.  Both code and documentation contributions help, and
documenting the code, writing tutorials, etc., is an excellent way to
learn the ins and outs of a statistical system.  We hope to continue
to make improvements to the system along with the Lisp-Stat community.

Visit the [github repository](https://github.com/lisp-stat/) to
see what we're currently working on. If there is something you would like
to see in Lisp-Stat, please create an issue yourself - or assign
yourself an issue if you would like to fix or add something.  See our
[contribution guidelines](/docs/contributing/) for more
information.

{{% /blocks/section %}}




