---
title: "Linear Algebra"
author: ["Steven Nunez"]
date: 2022-01-11
weight: 5
description: >
  Linear Algebra for Common Lisp
---

## Overview {#Overview}

LLA works with matrices, that is arrays of rank 2, with all numerical values.  Categorical variables could be integer coded if you need to.


## Basic Usage

`lla` requires a BLAS and LAPACK shared library. These may be available via
your operating systems package manager, or you can download [OpenBLAS](https://github.com/xianyi/OpenBLAS), which includes precompiled binaries for MS Windows.

{{< alert title="Note" >}}LLA relies on
[CFFI](https://common-lisp.net/project/cffi/manual/cffi-manual.html)
to locate the BLAS & LAPPACK shared library. In most cases, this means CFFI
will use the system default search paths. If you encounter errors in
loading the library, consult the CFFI documentation. For MS Windows,
the certain way to successfully load the DLL is to ensure that the
library is on the `PATH`.{{</alert >}}

You can also configure the path by setting the `cl-user::*lla-configuration*` variable like so:
```lisp
(defvar *lla-configuration*
  '(:libraries ("s:/src/lla/lib/libopenblas.dll")))
```
Use the location specific to your system.

To load `lla`:

```lisp
(asdf:load-system :lla)
(use-package 'lla) ;access to the symbols
```

## Examples

To make working with matrices easier, we're going to use the matrix-shorthand library.  Load it like so:

```lisp
(asdf:load-system :num-utils)
(use-package :num-utils.matrix-shorthand)
```

### Matrix Multiplication
`mm` is the matrix multiplication function.  It is generic and can operate on both regular arrays and 'wrapped' array types, e.g. hermitian or triangular.  In this example we'll multiple an array by a vector.  `mx` is the short-hand way of defining a matrix, and `vec` for vector.


```lisp
(let ((a (mx 'lla-double
           (1 2)
           (3 4)
           (5 6)))
      (b2 (vec 'lla-double 1 2)))
  (mm a b2))

; #(5.0d0 11.0d0 17.0d0)
```
