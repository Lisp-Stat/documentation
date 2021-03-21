
---
title: "First Release"
linkTitle: "Release 1.0"
date: 2021-03-28
author: Steve Nunez
description: >
  Lisp-Stat 1.0
---

Lisp-Stat is an environment for statistical computing, conceptually
similar to R, that is also suitable for front-line production
deployments.  It grew out of a desire to have an environment for
rapidly prototyping analytical and A.I. solutions, and move directly to
production environments with minimal friction.  Typically there is a
gap between the modeling environment, perhaps R or Python, and the
delivery environment. Lisp-Stat closes that gap.


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


## What does Lisp-Stat do?

Lisp-Stat provides support for vectorized mathematical operations, and
a comprehensive set of statistical methods that are implemented using
the latest numerical algorithms.  In addition, Common Lisp provides a
dynamic programming environmen

([REPL](https://en.wikipedia.org/wiki/Read%E2%80%93eval%E2%80%93print_loop)),
an excellent object-oriented facility
([CLOS](https://en.wikipedia.org/wiki/Common_Lisp_Object_System))
and meta-object protocol
([MOP](https://en.wikipedia.org/wiki/Metaobject#Metaobject_protocol)).

Lisp-Stat is fully functional today, and most of the XLISP-STAT
libraries can be ported with the aid of a compatibility package
(XLS).  This gives Lisp-Stat a leg up on ecosystem development.
