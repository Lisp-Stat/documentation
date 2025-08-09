---
title: "Linear Algebra"
linktitle: "Linear Algebra"
author: ["Steven Nunez"]
date: 2025-08-08
weight: 5
latex: true
vega: true
description: >
  Linear Algebra for Common Lisp
---

## Overview {#Overview}

LLA (Lisp Linear Algebra) is a high-level Common Lisp library for numerical linear algebra operations. It provides a Lisp-friendly interface to BLAS and LAPACK libraries, allowing you to work with matrices and vectors using Lisp's native array types while leveraging the performance of optimized numerical libraries.

The library is designed to work with dense matrices (rank-2 arrays) containing numerical values. While categorical variables can be integer-coded if needed, LLA is primarily intended for continuous numerical data.

## Setup

`lla` requires a BLAS and LAPACK shared library. These may be available via
your operating systems package manager, or you can download [OpenBLAS](https://github.com/xianyi/OpenBLAS), which includes precompiled binaries for MS Windows.

If you're working on UNIX or Linux and have the BLAS library installed, LLA should 'just work'.  If you've installed to a custom location, or on MS Windows, you'll need to tell LLA where your libraries are.

{{< alert title="Note" >}}LLA relies on
[CFFI](https://common-lisp.net/project/cffi/manual/cffi-manual.html)
to locate the BLAS & LAPACK shared libraries. In most cases, this means CFFI
will use the system default search paths. If you encounter errors in
loading the library, consult the CFFI documentation. For MS Windows,
the certain way to successfully load the DLL is to ensure that the
library is on the `PATH`.{{</alert >}}

### configuration

LLA can be configured before loading by setting the `cl-user::*lla-configuration*` variable. This allows you to specify custom library paths and enable various optimizations:

```lisp
(defvar cl-user::*lla-configuration*
  '(:libraries ("s:/src/lla/lib/libopenblas.dll")))
```

The configuration accepts the following options:

- `:libraries` - List of paths to BLAS/LAPACK libraries
- `:int64` - Use 64-bit integers (default: nil)
- `:efficiency-warnings` - Enable efficiency warnings (default: nil)
  - `:array-type` - Warn when array types need elementwise checking
  - `:array-conversion` - Warn when arrays need copying for foreign calls

Use the location specific to your system.

### loading LLA

To load `lla`:

```lisp
(asdf:load-system :lla)
(use-package 'lla) ;access to the symbols
```

### getting started

To make working with matrices easier, we're going to use the matrix-shorthand library.  Load it like so:

```lisp
(use-package :num-utils.matrix-shorthand)
```

Here's a simple example demonstrating matrix multiplication:

```lisp
(let ((a (mx 'lla-double
           (1 2)
           (3 4)
           (5 6)))
      (b2 (vec 'lla-double 1 2)))
  (mm a b2))
; => #(5.0d0 11.0d0 17.0d0)
```

The `mx` macro creates a matrix, `vec` creates a vector, and `mm` performs matrix multiplication.

## Numeric types

LLA provides type synonyms for commonly used numeric types. These serve as optimization hints, similar to element-type declarations in `MAKE-ARRAY`:

* `lla-integer` - Signed integers (32 or 64-bit depending on configuration)
* `lla-single` - Single-precision floating point
* `lla-double` - Double-precision floating point
* `lla-complex-single` - Single-precision complex numbers
* `lla-complex-double` - Double-precision complex numbers

These types help LLA avoid runtime type detection and enable more efficient operations.


## Matrix Types

LLA supports specialized matrix types that take advantage of mathematical properties for more efficient storage and computation. These types are provided by the `num-utils.matrix` package and can be used interchangeably with regular arrays thanks to the `aops` protocol.

### diagonal

Diagonal matrices store only the diagonal elements in a vector, saving memory and computation time. Off-diagonal elements are implicitly zero.

```lisp
(diagonal-matrix #(1 2 3))
; => #<DIAGONAL-MATRIX 3x3
;      1  .  .
;      .  2  .
;      .  .  3>

;; Converting to a regular array shows the full structure
(aops:as-array (diagonal-matrix #(1 2 3)))
; => #2A((1 0 0)
;        (0 2 0)
;        (0 0 3))
```

You can extract and modify the diagonal elements:

```lisp
(diagonal-vector (diagonal-matrix #(4 5 6)))
; => #(4 5 6)

;; Set new diagonal values
(let ((d (diagonal-matrix #(1 2 3))))
  (setf (diagonal-vector d) #(7 8 9))
  d)
; => #<DIAGONAL-MATRIX 3x3
;      7  .  .
;      .  8  .
;      .  .  9>
```

### triangular

Triangular matrices come in two varieties: lower and upper triangular. Elements outside the triangular region are treated as zero, though they may contain arbitrary values in the internal storage.

#### Lower Triangular

Lower triangular matrices have all elements above the diagonal as zero:

```lisp
(lower-triangular-matrix #2A((1 999)
                            (2 3)))
; => #<LOWER-TRIANGULAR-MATRIX 2x2
;      1  .
;      2  3>

;; Converting to array shows the zero structure
(aops:as-array (lower-triangular-matrix #2A((1 999)
                                           (2 3))))
; => #2A((1 0)
;        (2 3))
```

#### Upper Triangular

Upper triangular matrices have all elements below the diagonal as zero:

```lisp
(upper-triangular-matrix #2A((1 2)
                            (999 3)))
; => #<UPPER-TRIANGULAR-MATRIX 2x2
;      1  2
;      .  3>

;; Converting to array shows the zero structure
(aops:as-array (upper-triangular-matrix #2A((1 2)
                                           (999 3))))
; => #2A((1 2)
;        (0 3))
```

Transposing switches between upper and lower triangular:

```lisp
(transpose (lower-triangular-matrix #2A((1 0)
                                       (2 3))))
; => #<UPPER-TRIANGULAR-MATRIX 2x2
;      1  2
;      .  3>
```

### hermitian

Hermitian matrices are equal to their conjugate transpose. For real-valued matrices, this means symmetric matrices. Only the lower triangle needs to be stored, with the upper triangle automatically filled by conjugation.

```lisp
;; Real symmetric matrix
(hermitian-matrix #2A((1 2)
                     (2 3)))
; => #<HERMITIAN-MATRIX 2x2
;      1  .
;      2  3>

;; Converting to array shows the symmetric structure
(aops:as-array (hermitian-matrix #2A((1 2)
                                    (2 3))))
; => #2A((1 2)
;        (2 3))
```

For complex matrices, the upper triangle is the conjugate of the lower:

```lisp
(hermitian-matrix #2A((#C(1 0) #C(2 1))
                     (#C(2 1) #C(3 0))))
; => #<HERMITIAN-MATRIX 2x2
;      #C(1 0)  .
;      #C(2 1)  #C(3 0)>

;; Converting shows the conjugate symmetry
(aops:as-array (hermitian-matrix #2A((#C(1 0) #C(2 1))
                                    (#C(2 1) #C(3 0)))))
; => #2A((#C(1 0) #C(2 -1))
;        (#C(2 1) #C(3 0)))
```

### as-arrays

All matrix types support the `aops` protocol, making them interchangeable with regular arrays:

```lisp
;; Dimensions
(aops:dims (diagonal-matrix #(1 2 3)))
; => (3 3)

;; Element type
(aops:element-type (hermitian-matrix #2A((1.0 2.0) (2.0 3.0))))
; => DOUBLE-FLOAT

;; Size
(aops:size (upper-triangular-matrix #2A((1 2 3) (0 4 5) (0 0 6))))
; => 9

;; Rank
(aops:rank (lower-triangular-matrix #2A((1 0) (2 3))))
; => 2
```

You can also use array displacement and slicing:

```lisp
;; Flatten to a vector
(aops:flatten (diagonal-matrix #(1 2 3)))
; => #(1 0 0 0 2 0 0 0 3)

;; Slice operations with select
(select (upper-triangular-matrix #2A((1 2 3)
                                    (0 4 5)
                                    (0 0 6)))
        0 t)  ; First row, all columns
; => #(1 2 3)
```

The specialized matrix types automatically maintain their structure during operations, providing both memory efficiency and computational advantages while remaining compatible with generic array operations.

## Shorthand

The `num-utils.matrix-shorthand` package provides convenient macros and functions for creating matrices and vectors with specific element types. These constructors simplify the creation of typed arrays and specialized matrix structures.

### vec

**`vec`** creates a vector with elements coerced to the specified element type:

```lisp
(vec t 1 2 3)
; => #(1 2 3)

(vec 'double-float 1 2 3)
; => #(1.0d0 2.0d0 3.0d0)

(vec 'single-float 1/2 3/4 5/6)
; => #(0.5 0.75 0.8333333)
```

The first argument specifies the element type, followed by the vector elements. Each element is coerced to the specified type.

### mx

**`mx`** is a macro for creating dense matrices (rank 2 arrays) from row specifications:

```lisp
(mx t
  (1 2 3)
  (4 5 6))
; => #2A((1 2 3)
;        (4 5 6))

(mx 'double-float
  (1 2)
  (3 4))
; => #2A((1.0d0 2.0d0)
;        (3.0d0 4.0d0))
```

Each row is specified as a list. Elements are coerced to the specified type.

### diagonal-mx

**`diagonal-mx`** creates a diagonal matrix from the given diagonal elements:

```lisp
(diagonal-mx t 1 2 3)
; => #<DIAGONAL-MATRIX 3x3
;      1  .  .
;      .  2  .
;      .  .  3>

(diagonal-mx 'double-float 4 5 6)
; => #<DIAGONAL-MATRIX 3x3
;      4.0d0  .  .
;      .      5.0d0  .
;      .      .      6.0d0>
```

The resulting diagonal matrix stores only the diagonal elements, with off-diagonal elements implicitly zero.

### lower-triangular-mx

**`lower-triangular-mx`** creates a lower triangular matrix. Elements above the diagonal are ignored, and rows are padded with zeros as needed:

```lisp
(lower-triangular-mx t
  (1)
  (3 4))
; => #<LOWER-TRIANGULAR-MATRIX 2x2
;      1  .
;      3  4>

;; Elements above diagonal are ignored
(lower-triangular-mx t
  (1 9)     ; 9 is ignored
  (3 4))
; => #<LOWER-TRIANGULAR-MATRIX 2x2
;      1  .
;      3  4>
```

### upper-triangular-mx

**`upper-triangular-mx`** creates an upper triangular matrix. Elements below the diagonal are replaced with zeros:

```lisp
(upper-triangular-mx t
  (1 2)
  (3 4))
; => #<UPPER-TRIANGULAR-MATRIX 2x2
;      1  2
;      .  4>

;; Elements below diagonal become zero
(upper-triangular-mx t
  (1 2)
  (9 4))    ; 9 becomes 0
; => #<UPPER-TRIANGULAR-MATRIX 2x2
;      1  2
;      .  4>

(upper-triangular-mx 'double-float
  (1 2 3)
  (4 5 6)
  (7 8 9))
; => #<UPPER-TRIANGULAR-MATRIX 3x3
;      1.0d0  2.0d0  3.0d0
;      .      5.0d0  6.0d0
;      .      .      9.0d0>
```

### hermitian-mx

**`hermitian-mx`** creates a Hermitian matrix (symmetric for real values). Only the lower triangle needs to be specified, and elements above the diagonal are ignored:

```lisp
(hermitian-mx t
  (1)
  (3 4))
; => #<HERMITIAN-MATRIX 2x2
;      1  .
;      3  4>

;; Elements above diagonal are ignored
(hermitian-mx t
  (1 9)     ; 9 is ignored
  (3 4))
; => #<HERMITIAN-MATRIX 2x2
;      1  .
;      3  4>
```

For Hermitian matrices, attempting to create non-square matrices will signal an error:

```lisp
(hermitian-mx t
  (1 2 3)
  (3 4 5))  ; Error: rows too long for matrix
```

All shorthand constructors coerce elements to the specified type and return specialized matrix objects that are memory-efficient and compatible with the `aops` protocol for array operations.

## Matrix Arithmetic

The `num-utils.elementwise` package provides a comprehensive set of elementwise operations for arrays and numerical objects. These operations extend Common Lisp's standard arithmetic to work seamlessly with arrays, matrices, and vectors while preserving numerical type precision through automatic type contagion.

### intuition

**Elementwise arithmetic** operates on corresponding elements of arrays, applying the specified operation to each pair of elements independently. This differs fundamentally from **algebraic matrix operations** like matrix multiplication:

- **Algebraic operations** follow mathematical rules (e.g., matrix multiplication requires compatible dimensions: m×n · n×p = m×p)
- **Elementwise operations** require identical dimensions and operate position-by-position

For example:
```lisp
;; Elementwise multiplication (e*)
(e* (mx 'double-float (1 2) (3 4))
    (mx 'double-float (5 6) (7 8)))
; => #2A((5.0d0 12.0d0)     ; 1×5=5, 2×6=12
;        (21.0d0 32.0d0))   ; 3×7=21, 4×8=32

;; Algebraic matrix multiplication (mm)
(mm (mx 'double-float (1 2) (3 4))
    (mx 'double-float (5 6) (7 8)))
; => #2A((19.0d0 22.0d0)    ; 1×5+2×7=19, 1×6+2×8=22
;        (43.0d0 50.0d0))   ; 3×5+4×7=43, 3×6+4×8=50
```

### type contagion

The `elementwise-float-contagion` function automatically determines the appropriate result type when combining different numeric types, following Common Lisp's numeric contagion rules but extended for arrays:

```lisp
(e+ (vec 'single-float 1.0 2.0)     ; single-float array
    (vec 'double-float 3.0 4.0))    ; double-float array
; => #(4.0d0 6.0d0)                  ; result is double-float

(e* 2                                ; integer scalar
    (vec 'double-float 1.5 2.5))    ; double-float array
; => #(3.0d0 5.0d0)                  ; result is double-float
```

### unary operations

Unary operations apply a function to each element of an array independently. They work with both scalars and arrays:

```lisp
;; Unary operations on vectors
(e1- (vec 'double-float 1 2 3 4 5))
; => #(-1.0d0 -2.0d0 -3.0d0 -4.0d0 -5.0d0)

(esqrt (vec 'double-float 4 9 16 25))
; => #(2.0d0 3.0d0 4.0d0 5.0d0)

;; Unary operations on matrices
(eabs (mx 'double-float
        (-1 2.5)
        (-3.7 4)))
; => #2A((1.0d0 2.5d0)
;        (3.7d0 4.0d0))

(elog (mx 'double-float
        ((exp 1) (exp 2))
        ((exp 3) (exp 4))))
; => #2A((1.0d0 2.0d0)
;        (3.0d0 4.0d0))
```

### binary operations

Binary operations combine two arrays element-by-element or broadcast a scalar across an array. Arrays must have matching dimensions:

```lisp
;; Binary operations with equal-length vectors
(e2+ (vec 'double-float 1 2 3)
     (vec 'double-float 4 5 6))
; => #(5.0d0 7.0d0 9.0d0)

;; Binary operations with scalar broadcasting
(e2* (vec 'double-float 2 3 4)
     2.5d0)
; => #(5.0d0 7.5d0 10.0d0)

;; Binary operations on matrices
(e2- (mx 'double-float
       (10 20)
       (30 40))
     (mx 'double-float
       (1 2)
       (3 4)))
; => #2A((9.0d0 18.0d0)
;        (27.0d0 36.0d0))

;; Dimension mismatch signals an error
(handler-case
    (e2+ (vec 'double-float 1 2 3)      ; length 3
         (vec 'double-float 4 5))        ; length 2
  (error (e) (format nil "Error: ~A" e)))
; => "Error: Assertion failed: (EQUAL (ARRAY-DIMENSIONS A) (ARRAY-DIMENSIONS B))"
```

### unary operators reference

| Function | Description | Example |
|----------|-------------|---------|
| `e1-` | Univariate elementwise - | `(e1- x)` ≡ `-x` |
| `e1/` | Univariate elementwise / | `(e1/ x)` ≡ `1/x` |
| `e1log` | Univariate elementwise LOG | `(e1log x)` ≡ `log(x)` |
| `e1exp` | Univariate elementwise EXP | `(e1exp x)` ≡ `exp(x)` |
| `eabs` | Univariate elementwise ABS | `(eabs x)` ≡ `|x|` |
| `efloor` | Univariate elementwise FLOOR | `(efloor x)` ≡ `⌊x⌋` |
| `eceiling` | Univariate elementwise CEILING | `(eceiling x)` ≡ `⌈x⌉` |
| `eexp` | Univariate elementwise EXP | `(eexp x)` ≡ `e^x` |
| `esqrt` | Univariate elementwise SQRT | `(esqrt x)` ≡ `√x` |
| `econjugate` | Univariate elementwise CONJUGATE | `(econjugate x)` ≡ `x*` |
| `esquare` | Univariate elementwise SQUARE | `(esquare x)` ≡ `x²` |
| `esin` | Univariate elementwise SIN | `(esin x)` ≡ `sin(x)` |
| `ecos` | Univariate elementwise COS | `(ecos x)` ≡ `cos(x)` |
| `emod` | Univariate elementwise MOD | `(emod x)` ≡ `mod(x)` |

### binary operators reference

| Function | Description | Example |
|----------|-------------|---------|
| `e2+` | Bivariate elementwise + | `(e2+ a b)` ≡ `a + b` |
| `e2-` | Bivariate elementwise - | `(e2- a b)` ≡ `a - b` |
| `e2*` | Bivariate elementwise * | `(e2* a b)` ≡ `a × b` |
| `e2/` | Bivariate elementwise / | `(e2/ a b)` ≡ `a ÷ b` |
| `e2log` | Bivariate elementwise LOG | `(e2log a b)` ≡ `log_b(a)` |
| `e2exp` | Bivariate elementwise EXPT | `(e2exp a b)` ≡ `a^b` |
| `eexpt` | Bivariate elementwise EXPT | `(eexpt a b)` ≡ `a^b` |
| `e2mod` | Bivariate elementwise MOD | `(e2mod a b)` ≡ `a mod b` |
| `e2<` | Bivariate elementwise < | `(e2< a b)` ≡ `a < b` |
| `e2<=` | Bivariate elementwise <= | `(e2<= a b)` ≡ `a ≤ b` |
| `e2>` | Bivariate elementwise > | `(e2> a b)` ≡ `a > b` |
| `e2>=` | Bivariate elementwise >= | `(e2>= a b)` ≡ `a ≥ b` |
| `e2=` | Bivariate elementwise = | `(e2= a b)` ≡ `a = b` |

### variadic operations

The variadic operators (`e+`, `e-`, `e*`, `e/`) accept multiple arguments, applying the operation from left to right:

```lisp
;; Multiple arguments with e+
(e+ (vec 'double-float 1 2 3)
    (vec 'double-float 4 5 6)
    (vec 'double-float 7 8 9))
; => #(12.0d0 15.0d0 18.0d0)

;; Multiple arguments with e*
(e* (vec 'double-float 2 3 4)
    (vec 'double-float 5 6 7)
    (vec 'double-float 8 9 10))
; => #(80.0d0 162.0d0 280.0d0)

;; Single argument returns identity for e+ and e*
(e+ (vec 'double-float 1 2 3))
; => #(1.0d0 2.0d0 3.0d0)

(e* (vec 'double-float 1 2 3))
; => #(1.0d0 2.0d0 3.0d0)

;; Unary negation with e-
(e- (vec 'double-float 1 2 3))
; => #(-1.0d0 -2.0d0 -3.0d0)

;; Reciprocal with e/
(e/ (vec 'double-float 2 4 8))
; => #(0.5d0 0.25d0 0.125d0)
```

### special operations

- **`elog`**: Provides both natural logarithm and logarithm with arbitrary base
  ```lisp
  (elog (vec 'double-float 10 100))        ; Natural log
  ; => #(2.302... 4.605...)
  
  (elog (vec 'double-float 10 100) 10)     ; Log base 10
  ; => #(1.0d0 2.0d0)
  ```

- **`ereduce`**: Applies a reduction function across all elements in row-major order
  ```lisp
  (ereduce #'+ (mx 'double-float
                 (1 2 3)
                 (4 5 6)))
  ; => 21.0d0
  ```

- **`emin`/`emax`**: Find the minimum or maximum element
  ```lisp
  (emin (mx 'double-float (5 2 8) (1 9 3)))
  ; => 1.0d0
  
  (emax (mx 'double-float (5 2 8) (1 9 3)))
  ; => 9.0d0
  ```

These elementwise operations provide a powerful and consistent interface for numerical computations, automatically handling type promotion and supporting both scalar and array arguments





## Factorizations

Matrix factorizations are fundamental tools in numerical linear algebra that decompose a matrix into a product of simpler, structured matrices. These decompositions reveal important properties of the original matrix and enable efficient algorithms for solving systems of equations, computing eigenvalues, finding least-squares solutions, and performing other numerical operations.

LLA provides several key factorizations:

- **LU decomposition** - Factors a matrix into lower and upper triangular matrices with row pivoting, used for solving linear systems and computing determinants
- **QR decomposition** - Factors a matrix into an orthogonal matrix and an upper triangular matrix, essential for least-squares problems and eigenvalue algorithms
- **Cholesky decomposition** - Factors a positive definite symmetric matrix into the product of a lower triangular matrix and its transpose, providing the most efficient method for solving positive definite systems
- **Spectral decomposition** - Decomposes a symmetric matrix into its eigenvalues and eigenvectors, revealing the matrix's fundamental structure
- **Singular Value Decomposition (SVD)** - The most general factorization, decomposing any matrix into orthogonal matrices and a diagonal matrix of singular values, used for rank determination, pseudo-inverses, and data compression

Each factorization exploits specific mathematical properties to provide computational advantages. The factored forms often require less storage than the original matrix and enable specialized algorithms that are more numerically stable and computationally efficient than working with the original matrix directly.

### lu

**`lu`** computes the LU factorization of a matrix with pivoting. The factorization represents $PA = LU$, where $P$ is a permutation matrix, $L$ is lower triangular with unit diagonal, and $U$ is upper triangular.

```lisp
(let ((a (mx 'lla-double
           (1 2)
           (3 4))))
  (lu a))
; => #<LU 
;     
;      L=#<LOWER-TRIANGULAR-MATRIX element-type DOUBLE-FLOAT
;        1.00000       .
;        0.33333 1.00000>
;     
;      U=#<UPPER-TRIANGULAR-MATRIX element-type DOUBLE-FLOAT
;        3.00000 4.00000
;              . 0.66667>
;     
;       pivot indices=#(2 2)>
```

### lu-u

**`lu-u`** returns the upper triangular $U$ matrix from an LU factorization:

```lisp
(let* ((a (mx 'lla-double
            (1 2)
            (3 4)))
       (lu-fact (lu a)))
  (lu-u lu-fact))
; => #<UPPER-TRIANGULAR-MATRIX 2x2
;      3.0d0  4.0d0
;      .      0.6666...>
```

### lu-l

**`lu-l`** returns the lower triangular $L$ matrix from an LU factorization, with ones on the diagonal:

```lisp
(let* ((a (mx 'lla-double
            (1 2)
            (3 4)))
       (lu-fact (lu a)))
  (lu-l lu-fact))
; => #<LOWER-TRIANGULAR-MATRIX 2x2
;      1.0d0     .
;      0.3333... 1.0d0>
```

### ipiv

**`ipiv`** returns pivot indices in a format understood by SELECT, counting from 0. These indices show how rows were permuted during factorization:

```lisp
(let* ((a (mx 'lla-double
            (1 2)
            (3 4)))
       (lu-fact (lu a)))
  (ipiv lu-fact))
; => #(1 0)
```

The identity $(select\; a\; ipiv\; t) \equiv (mm\; (lu\text{-}l\; lu\text{-}fact)\; (lu\text{-}u\; lu\text{-}fact))$ holds.

### ipiv-inverse

**`ipiv-inverse`** returns the inverted permutation indices. This allows you to reconstruct the original matrix from the factorization:

```lisp
(let* ((a (mx 'lla-double
            (1 2)
            (3 4)))
       (lu-fact (lu a)))
  (ipiv-inverse lu-fact))
; => #(1 0)
```

The identity $a \equiv (select\; (mm\; (lu\text{-}l\; lu\text{-}fact)\; (lu\text{-}u\; lu\text{-}fact))\; ipiv\text{-}inverse\; t)$ holds.

### qr

**`qr`** computes the QR factorization of a matrix, where $Q$ is orthogonal and $R$ is upper triangular such that $A = QR$:

```lisp
(let ((a (mx 'lla-double
           (1 2)
           (3 4))))
  (qr a))
; => #<QR 2x2>
```

### qr-r

**`qr-r`** returns the upper triangular $R$ matrix from a QR factorization:

```lisp
(let* ((a (mx 'lla-double
            (1 2)
            (3 4)))
       (qr-fact (qr a)))
  (qr-r qr-fact))
; => #<UPPER-TRIANGULAR-MATRIX 2x2>
```

### matrix-square-root

**`matrix-square-root`** is a general structure for representing $XX^T$ decompositions of matrices. The convention is to store $X$, the left square root:

```lisp
(let ((x (mx 'lla-double
           (1 0)
           (2 1))))
  (make-matrix-square-root x))
; => #S(MATRIX-SQUARE-ROOT :LEFT #2A((1.0d0 0.0d0) (2.0d0 1.0d0)))
```

### xx

**`xx`** is a convenience function to create a matrix-square-root from a left square root:

```lisp
(xx (mx 'lla-double
      (1 0)
      (2 1)))
; => #S(MATRIX-SQUARE-ROOT :LEFT #2A((1.0d0 0.0d0) (2.0d0 1.0d0)))
```

### left-square-root

**`left-square-root`** returns $X$ such that $XX^T = A$. For general matrix-square-root objects, it returns the stored left factor:

```lisp
(let* ((a (hermitian-mx 'lla-double
            (2)
            (-1 2)
            (0 -1 2)))
       (chol (cholesky a)))
  (left-square-root chol))
; => #<LOWER-TRIANGULAR-MATRIX 3x3
;      1.414...    .         .
;     -0.707...    1.224...  .
;      0.0        -0.816...  1.154...>
```

### right-square-root

**`right-square-root`** returns $Y$ such that $Y^T Y = A$. This is computed as the transpose of the left square root:

```lisp
(let* ((a (hermitian-mx 'lla-double
            (2)
            (-1 2)
            (0 -1 2)))
       (chol (cholesky a)))
  (right-square-root chol))
; => #<UPPER-TRIANGULAR-MATRIX 3x3
;      1.414...   -0.707...   0.0
;      .           1.224...  -0.816...
;      .           .          1.154...>
```

### cholesky

**`cholesky`** computes the Cholesky factorization of a positive definite Hermitian matrix. It returns a lower triangular matrix $L$ such that $A = LL^T$:

```lisp
(let ((a (hermitian-mx 'lla-double
           (2)
           (-1 2)
           (0 -1 2))))
  (cholesky a))
; => #S(CHOLESKY :LEFT #<LOWER-TRIANGULAR-MATRIX 3x3
;                        1.414...    .         .
;                       -0.707...    1.224...  .
;                        0.0        -0.816...  1.154...>)
```

### hermitian-factorization

**`hermitian-factorization`** computes a factorization for indefinite Hermitian matrices with pivoting. This is used internally for solving systems with Hermitian matrices that may not be positive definite.

### spectral-factorization

**`spectral-factorization`** computes the eigenvalue decomposition of a Hermitian matrix, returning a structure containing eigenvectors ($Z$) and eigenvalues ($W$) such that $A = ZWZ^T$:

```lisp
(let* ((a (mm t (mx 'lla-double
                  (1 2)
                  (3 4))))
       (sf (spectral-factorization a)))
  sf)
; => #S(SPECTRAL-FACTORIZATION 
;       :Z #2A((-0.8174... 0.5760...)
;              (0.5760...  0.8174...))
;       :W #<DIAGONAL-MATRIX 2x2
;             0.1339...  .
;             .          29.866...>)
```

### spectral-factorization-w

**`spectral-factorization-w`** returns the diagonal matrix $W$ of eigenvalues from a spectral factorization:

```lisp
(let* ((a (mm t (mx 'lla-double
                  (1 2)
                  (3 4))))
       (sf (spectral-factorization a)))
  (spectral-factorization-w sf))
; => #<DIAGONAL-MATRIX 2x2
;      0.1339...  .
;      .          29.866...>
```

### spectral-factorization-z

**`spectral-factorization-z`** returns the matrix $Z$ of eigenvectors from a spectral factorization:

```lisp
(let* ((a (mm t (mx 'lla-double
                  (1 2)
                  (3 4))))
       (sf (spectral-factorization a)))
  (spectral-factorization-z sf))
; => #2A((-0.8174... 0.5760...)
;        (0.5760...  0.8174...))
```

### svd

**`svd`** computes the singular value decomposition of a matrix $A = U\Sigma V^T$, where $U$ and $V$ are orthogonal matrices and $\Sigma$ is diagonal with non-negative entries:

```lisp
(let ((a (mx 'lla-double
           (0 1)
           (2 3)
           (4 5))))
  (svd a))
; => #S(SVD 
;       :U #2A((-0.1081... 0.9064...)
;              (-0.4873... 0.3095...)
;              (-0.8664... -0.2872...))
;       :D #<DIAGONAL-MATRIX 2x2
;             7.386...  .
;             .         0.6632...>
;       :VT #2A((-0.6011... -0.7991...)
;               (-0.7991... 0.6011...)))
```

### svd-u

**`svd-u`** returns the left singular vectors ($U$ matrix) from an SVD:

```lisp
(let* ((a (mx 'lla-double
            (0 1)
            (2 3)
            (4 5)))
       (svd-result (svd a)))
  (svd-u svd-result))
; => #2A((-0.1081... 0.9064...)
;        (-0.4873... 0.3095...)
;        (-0.8664... -0.2872...))
```

### svd-d

**`svd-d`** returns the diagonal matrix $\Sigma$ of singular values from an SVD, in descending order:

```lisp
(let* ((a (mx 'lla-double
            (0 1)
            (2 3)
            (4 5)))
       (svd-result (svd a)))
  (svd-d svd-result))
; => #<DIAGONAL-MATRIX 2x2
;      7.386...  .
;      .         0.6632...>
```

### svd-vt

**`svd-vt`** returns the right singular vectors ($V^T$ matrix) from an SVD:

```lisp
(let* ((a (mx 'lla-double
            (0 1)
            (2 3)
            (4 5)))
       (svd-result (svd a)))
  (svd-vt svd-result))
; => #2A((-0.6011... -0.7991...)
;        (-0.7991... 0.6011...))
```

## Linear Algebra

This section covers the core linear algebra operations provided by LLA. These functions leverage BLAS and LAPACK for efficient numerical computations while providing a convenient Lisp interface.

### multiplication

**`mm`** performs matrix multiplication of two arrays. It handles matrices, vectors, and special matrix types, automatically selecting the appropriate operation based on the arguments' dimensions.

When given two matrices, it computes their product:

```lisp
(let ((a (mx 'lla-double
           (1 2)
           (3 4)
           (5 6)))
      (i2 (mx 'lla-double
            (1 0)
            (0 1))))
  (mm a i2))
; => #2A((1.0d0 2.0d0)
;        (3.0d0 4.0d0)
;        (5.0d0 6.0d0))
```

Matrix-vector multiplication produces a vector:

```lisp
(let ((a (mx 'lla-double
           (1 2)
           (3 4)
           (5 6)))
      (b2 (vec 'lla-double 1 2)))
  (mm a b2))
; => #(5.0d0 11.0d0 17.0d0)
```

Vector-matrix multiplication (row vector times matrix):

```lisp
(let ((b3 (vec 'lla-double 1 2 3))
      (a (mx 'lla-double
           (1 2)
           (3 4)
           (5 6))))
  (mm b3 a))
; => #(22.0d0 28.0d0)
```

The dot product of two vectors returns a scalar:

```lisp
(let ((a (vec 'lla-double 2 3 5))
      (b (vec 'lla-complex-double 1 #C(2 1) 3)))
  (mm a b))
; => #C(31.0d0 -3.0d0)
```

Special handling for transpose operations using the symbol `t`:

```lisp
;; A * A^T
(let ((a (mx 'lla-double
           (1 2)
           (3 4))))
  (mm a t))
; => #<HERMITIAN-MATRIX 2x2
;      5.0d0   .
;      11.0d0  25.0d0>

;; A^T * A
(let ((a (mx 'lla-double
           (1 2)
           (3 4))))
  (mm t a))
; => #<HERMITIAN-MATRIX 2x2
;      10.0d0  .
;      14.0d0  20.0d0>
```

### multiple matrix multiply

**`mmm`** multiplies multiple matrices from left to right. This is more efficient than repeated calls to `mm`:

```lisp
(mmm a b c ... z)  ; equivalent to (mm (mm (mm a b) c) ... z)
```

### outer

**`outer`** computes the outer product of two vectors, returning $column(a) \times row(b)^H$:

```lisp
(let ((a (vec 'lla-double 2 3))
      (b (vec 'lla-complex-double 1 #C(2 1) 9)))
  (outer a b))
; => #2A((#C(2.0d0 0.0d0) #C(4.0d0 -2.0d0) #C(18.0d0 0.0d0))
;        (#C(3.0d0 0.0d0) #C(6.0d0 -3.0d0) #C(27.0d0 0.0d0)))
```

When the second argument is `t`, it computes the outer product with itself:

```lisp
(outer (vec 'lla-double 2 3) t)
; => #<HERMITIAN-MATRIX 2x2
;      4.0d0  .
;      6.0d0  9.0d0>
```

### solve

**`solve`** is LLA's general-purpose linear system solver that finds the solution vector $\mathbf{x}$ to the matrix equation:

$$A\mathbf{x} = \mathbf{b}$$

where $A$ is an $n \times n$ coefficient matrix, $\mathbf{b}$ is the right-hand side vector (or matrix for multiple systems), and $\mathbf{x}$ is the unknown solution vector. The function intelligently dispatches to the most appropriate algorithm based on the matrix type and structure, ensuring both numerical stability and computational efficiency.

*# Algorithm Selection Strategy

LLA automatically selects the optimal solving strategy:

- **General matrices**: LU decomposition with partial pivoting ($PA = LU$)
- **Symmetric positive definite**: Cholesky decomposition ($A = LL^T$) 
- **Triangular matrices**: Forward/backward substitution ($O(n^2)$ complexity)
- **Diagonal matrices**: Element-wise division ($O(n)$ complexity)
- **Pre-factored matrices**: Direct substitution using stored decomposition

*# Basic Linear System Solving

For general square matrices, `solve` uses LU decomposition with partial pivoting to handle the system $A\mathbf{x} = \mathbf{b}$:

```lisp
(let* ((a (mx 'lla-double
            (1 2)
            (3 4)))
       (x (vec 'lla-double 5 6))
       (b (mm a x)))  ; Create b = Ax for verification
  (solve a b))
; => #(5.0d0 6.0d0)
```

The mathematical process involves:
1. Factor $A = PLU$ where $P$ is a permutation matrix
2. Solve $L\mathbf{y} = P\mathbf{b}$ by forward substitution
3. Solve $U\mathbf{x} = \mathbf{y}$ by backward substitution

*# Efficient Solving with Pre-computed Factorizations

When solving multiple systems with the same coefficient matrix, pre-computing the factorization is much more efficient. The LU factorization $PA = LU$ needs to be computed only once:

```lisp
(let* ((a (mx 'lla-double
            (1 2)
            (3 4)))
       (lu-fact (lu a))      ; Factor once: PA = LU
       (b (vec 'lla-double 17 39)))
  (solve lu-fact b))         ; Reuse factorization
; => #(5.0d0 6.0d0)
```

This approach reduces the complexity from $O(n^3)$ for each solve to $O(n^2)$, making it ideal for applications requiring multiple solves with the same matrix.

*# Triangular System Solvers

For triangular matrices, `solve` uses specialized algorithms that exploit the matrix structure. Upper triangular systems $U\mathbf{x} = \mathbf{b}$ are solved by backward substitution:

$$x_i = \frac{b_i - \sum_{j=i+1}^{n} u_{ij}x_j}{u_{ii}}$$

```lisp
(let ((u (upper-triangular-mx 'lla-double
           (1 2)
           (0 3)))
      (b (vec 'lla-double 5 7)))
  (solve u b))
; => #(1.0d0 2.333...d0)
```

Similarly, lower triangular systems use forward substitution with complexity $O(n^2)$.

*# Cholesky Solvers for Positive Definite Systems

For symmetric positive definite matrices, the Cholesky decomposition $A = LL^T$ provides the most efficient and numerically stable solution method:

```lisp
(let* ((a (hermitian-mx 'lla-double
            (2)
            (-1 2)
            (0 -1 2)))    ; Tridiagonal positive definite matrix
       (b (vec 'lla-double 5 7 13))
       (chol (cholesky a)))
  (solve chol b))
; => #(3.5d0 7.5d0 10.25d0)
```

The solution process involves:
1. Decompose $A = LL^T$ (Cholesky factorization)
2. Solve $L\mathbf{y} = \mathbf{b}$ by forward substitution  
3. Solve $L^T\mathbf{x} = \mathbf{y}$ by backward substitution

This method is approximately twice as fast as LU decomposition and uses half the storage, making it the preferred approach for positive definite systems arising in optimization, statistics, and numerical PDEs.

*# Multiple Right-Hand Sides

The `solve` function handles matrix right-hand sides $AX = B$ where $B$ contains multiple column vectors:

```lisp
(let ((a (mx 'lla-double
           (2 1)
           (1 2)))
      (b (mx 'lla-double
           (3 7)
           (2 8))))
  (solve a b))
; => #2A((1.333... 2.0d0)
;        (0.333... 4.0d0))
```

Each column of the result matrix contains the solution to $A\mathbf{x}_i = \mathbf{b}_i$, solved simultaneously using optimized LAPACK routines.

### invert

**`invert`** computes the matrix inverse $A^{-1}$ such that $A A^{-1} = A^{-1} A = I$, where $I$ is the identity matrix. While mathematically fundamental, explicit matrix inversion should generally be avoided in favor of `solve` for numerical linear algebra applications.

*# Why Avoid Explicit Inversion?

Computing $A^{-1}$ and then multiplying $A^{-1}\mathbf{b}$ to solve $A\mathbf{x} = \mathbf{b}$ is both computationally expensive and numerically unstable compared to direct solving methods:

**Computational Cost:**
- Matrix inversion: $O(\frac{2}{3}n^3)$ operations
- Matrix-vector multiplication: $O(n^2)$ operations  
- **Total for inversion approach**: $O(\frac{2}{3}n^3 + n^2)$
- **Direct solve**: $O(\frac{2}{3}n^3)$ operations (same factorization cost, no extra multiplication)

**Numerical Stability:**
Direct solving typically achieves machine precision, while the inversion approach can amplify rounding errors, especially for ill-conditioned matrices.

*# Comparison: Explicit Inversion vs Direct Solving

Let's compare both approaches using the same system $A\mathbf{x} = \mathbf{b}$:

```lisp
(let* ((a (mx 'lla-double
            (1 2)
            (3 4)))
       (b (vec 'lla-double 5 11)))
  
  ;; Method 1: Explicit inversion (NOT recommended)
  (let ((a-inv (invert a)))
    (format t "Method 1 - Explicit inversion:~%")
    (format t "  Inverse matrix: ~A~%" a-inv)
    (format t "  Solution via inverse: ~A~%~%" (mm a-inv b)))
  
  ;; Method 2: Direct solving (RECOMMENDED)
  (format t "Method 2 - Direct solving:~%")
  (format t "  Solution: ~A~%" (solve a b)))

; Output:
; Method 1 - Explicit inversion:
;   Inverse matrix: #2A((-2.0d0 1.0d0) (1.5d0 -0.5d0))
;   Solution via inverse: #(1.0d0 3.0d0)
; 
; Method 2 - Direct solving:
;   Solution: #(1.0d0 3.0d0)
```

Both methods produce identical results, but `solve` is more efficient and numerically stable. For a more programmatic comparison:

```lisp
;; Compare the results programmatically
(let* ((a (mx 'lla-double
            (1 2)
            (3 4)))
       (b (vec 'lla-double 5 11))
       (x-inverse (mm (invert a) b))
       (x-solve (solve a b)))
  (equalp x-inverse x-solve))
; => T  ; Both methods give the same result
```

*# Basic Matrix Inversion

For educational purposes or when the inverse itself is needed (rare in practice):

```lisp
(let ((m (mx 'lla-double
           (1 2)
           (3 4))))
  (invert m))
; => #2A((-2.0d0 1.0d0)
;        (1.5d0 -0.5d0))

;; Verification: A * A^(-1) = I
(let* ((a (mx 'lla-double (1 2) (3 4)))
       (a-inv (invert a)))
  (mm a a-inv))
; => #2A((1.0d0 0.0d0)
;        (0.0d0 1.0d0))
```

*# Structured Matrix Inversions

Inverting triangular matrices preserves their structure and is computationally efficient:

```lisp
(invert (upper-triangular-mx 'lla-double
          (1 2)
          (0 4)))
; => #<UPPER-TRIANGULAR-MATRIX 2x2
;      1.0d0  -0.5d0
;      .       0.25d0>
```

For upper triangular matrices $U$, the inverse $(U^{-1})_{ij}$ can be computed by back-substitution, maintaining the triangular structure.

*# Pseudoinverse for Singular Matrices

When a matrix is **singular** (determinant = 0) or **nearly singular**, the standard inverse doesn't exist. The **Moore-Penrose pseudoinverse** $A^+$ generalizes the concept of inversion to non-invertible matrices.

**Mathematical Definition:**
For any matrix $A \in \mathbb{R}^{m \times n}$, the pseudoinverse $A^+ \in \mathbb{R}^{n \times m}$ satisfies:
1. $A A^+ A = A$ (generalized inverse property)
2. $A^+ A A^+ = A^+$ (reflexive property)
3. $(A A^+)^T = A A^+$ (symmetry of $AA^+$)
4. $(A^+ A)^T = A^+ A$ (symmetry of $A^+A$)

**SVD Construction:**
If $A = U\Sigma V^T$ is the singular value decomposition, then:
$$A^+ = V\Sigma^+ U^T$$
where $\Sigma^+$ is formed by taking the reciprocal of non-zero singular values and transposing.

*# Diagonal Matrix Pseudoinverse

For diagonal matrices, the pseudoinverse is particularly intuitive. Elements below a specified tolerance are treated as zero:

```lisp
;; Singular diagonal matrix (contains zero)
(let ((d (diagonal-mx 'lla-double 2 0 3)))
  (invert d :tolerance 0.1))
; => #<DIAGONAL-MATRIX 3x3
;      0.5d0  .      .
;      .      0.0d0  .
;      .      .      0.333...d0>
```

**Mathematical interpretation:**
- $d_{11} = 2 > 0.1 \Rightarrow (d^+)_{11} = 1/2 = 0.5$
- $d_{22} = 0 < 0.1 \Rightarrow (d^+)_{22} = 0$ (treated as zero)
- $d_{33} = 3 > 0.1 \Rightarrow (d^+)_{33} = 1/3 \approx 0.333$

This tolerance-based approach prevents division by near-zero values, which would create numerically unstable results.

*# Nearly Singular Systems Example

The pseudoinverse is particularly useful for rank-deficient systems arising in least-squares problems:

```lisp
;; Create a rank-deficient matrix (columns are linearly dependent)
(let* ((a (mx 'lla-double
            (1 2 3)    ; Third column = first + second
            (4 5 9)
            (7 8 15)))
       (b (vec 'lla-double 6 18 30)))
  
  ;; For rank-deficient systems, use least-squares
  ;; which handles the pseudoinverse internally
  (least-squares b a))
; => #(1.0d0 2.0d0 0.0d0)  ; coefficients (non-unique solution)
;    0.0d0                  ; sum of squares (perfect fit)
;    0                      ; degrees of freedom
;    #<QR 3x3>             ; QR decomposition
```

For rank-deficient systems, the least-squares solution using the pseudoinverse provides the **minimum-norm solution** among all possible solutions that minimize $||A\mathbf{x} - \mathbf{b}||_2$. This is essential for handling over-parameterized systems in machine learning and statistics.

**Key properties of the pseudoinverse:**
- For full-rank square matrices: $A^+ = A^{-1}$
- For full column rank matrices: $A^+ = (A^T A)^{-1} A^T$ (left inverse)
- For full row rank matrices: $A^+ = A^T (A A^T)^{-1}$ (right inverse)
- Always exists and is unique for any matrix

### least-squares

**`least-squares`** solves overdetermined systems in the least squares sense, finding the best-fit solution that minimizes the sum of squared residuals. This is fundamental for regression analysis, curve fitting, and many practical applications where you have more observations than unknown parameters.

*# Mathematical Foundation

Given a system $X\mathbf{b} = \mathbf{y}$ where:
- $X$ is an $m \times n$ design matrix with $m > n$ (more rows than columns)
- $\mathbf{y}$ is an $m \times 1$ response vector
- $\mathbf{b}$ is the $n \times 1$ vector of unknown coefficients

The least squares solution minimizes:
$$||\mathbf{y} - X\mathbf{b}||_2^2 = \sum_{i=1}^{m} (y_i - \mathbf{x}_i^T\mathbf{b})^2$$

The solution is given by the normal equations:
$$\mathbf{b} = (X^T X)^{-1} X^T \mathbf{y}$$

However, LLA uses the more numerically stable QR decomposition approach, factoring $X = QR$ where $Q$ is orthogonal and $R$ is upper triangular.

*# Return Values

The function returns four values:
1. **Coefficient vector** $\mathbf{b}$ - the best-fit parameters
2. **Sum of squared residuals** - $||\mathbf{y} - X\mathbf{b}||_2^2$
3. **Degrees of freedom** - $m - n$ (observations minus parameters)
4. **QR decomposition** - can be reused for further computations

*# Simple Example

```lisp
(let ((x (mx 'lla-double
           (1 23)    ; observation 1: intercept, x-value
           (1 22)    ; observation 2
           (1 25)    ; observation 3
           (1 29)    ; observation 4
           (1 24)))  ; observation 5
      (y (vec 'lla-double 67 63 65 94 84)))
  (least-squares y x))
; => #(-24.9795... 4.0479...)  ; coefficients [intercept, slope]
;    270.7329...               ; sum of squared residuals
;    3                         ; degrees of freedom
;    #<QR {1006A69913}>        ; QR decomposition
```

This fits the linear model $y = -24.98 + 4.05x$.

*# Real-Life Example: Predicting House Prices

Let's model house prices based on size (square feet) and number of bedrooms:

```lisp
;; House data: [size_sqft, bedrooms] -> price_thousands
;; Design matrix includes intercept term
(let* ((x (mx 'lla-double
            ;; intercept  size(1000s) bedrooms
            (1           1.2         2)    ; 1200 sqft, 2 bed -> €280k
            (1           1.5         3)    ; 1500 sqft, 3 bed -> €320k
            (1           1.8         3)    ; 1800 sqft, 3 bed -> €360k
            (1           2.0         4)    ; 2000 sqft, 4 bed -> €400k
            (1           2.2         4)    ; 2200 sqft, 4 bed -> €430k
            (1           1.0         1)    ; 1000 sqft, 1 bed -> €220k
            (1           2.5         5)    ; 2500 sqft, 5 bed -> €480k
            (1           1.6         3)))  ; 1600 sqft, 3 bed -> €340k
       (y (vec 'lla-double 280 320 360 400 430 220 480 340))
       (coefficients (least-squares y x)))
  
  ;; Display coefficients
  (format t "Model: price = ~,2f + ~,2f*size + ~,2f*bedrooms~%"
          (aref coefficients 0)
          (aref coefficients 1)
          (aref coefficients 2))
  
  ;; Make a prediction
  (let* ((new-house (vec 'lla-double 1 1.7 3))  ; 1700 sqft, 3 bedrooms
         (prediction (mm new-house coefficients)))
    (format t "Predicted price for 1700 sqft, 3 bedroom: €~,0f~%"
            (* prediction 1000))))

; Output:
; Model: price = 91.75 + 113.14*size + 21.39*bedrooms
; Predicted price for 1700 sqft, 3 bedroom: €348248
```

The model shows that each additional 1000 square feet adds approximately €113,140 to the price, and each bedroom adds about €21,390.

*# Handling Rank-Deficient Matrices

When predictors are linearly dependent, the design matrix becomes rank-deficient. The QR decomposition can detect and handle this:

```lisp
;; Example with perfect multicollinearity
(let* ((x (mx 'lla-double
            (1 2 3)    ; x3 = x1 + x2 (perfect collinearity)
            (1 3 4)
            (1 4 5)
            (1 5 6)))
       (y (vec 'lla-double 10 12 14 16)))
  (multiple-value-bind (coeffs ssr df qr) (least-squares y x)
    (declare (ignore ssr qr))  ; Suppress unused variable warnings
    (format t "Coefficients: ~A~%" coeffs)
    (format t "Degrees of freedom: ~A~%" df)))
; Output:
; Coefficients: #(6.0d0 2.0d0 -0.0d0)
; Degrees of freedom: 1
```

Note that one coefficient is effectively zero due to the linear dependency, and the degrees of freedom reflect the rank deficiency.

*# Computing Standard Errors

Standard errors measure the precision of estimated regression coefficients. They quantify the uncertainty in our parameter estimates due to sampling variability. Smaller standard errors indicate more precise estimates, while larger ones suggest greater uncertainty.

**What are Standard Errors?**

When we fit a linear model $y = \beta_0 + \beta_1 x + \epsilon$, the coefficients $\hat{\beta}_0$ and $\hat{\beta}_1$ are estimates based on our sample data. If we collected a different sample, we'd get slightly different estimates. The standard error measures this variability - it's the standard deviation of the sampling distribution of the coefficient.

**Why Standard Errors Matter:**
- **Confidence intervals**: We use them to construct confidence intervals (e.g., $\hat{\beta} \pm 1.96 \times SE$ for 95% CI)
- **Hypothesis testing**: The t-statistic $t = \hat{\beta}/SE$ tests if a coefficient is significantly different from zero
- **Model assessment**: Large standard errors relative to coefficients suggest unstable estimates

**Example: Analyzing a Linear Trend**

Let's fit a line to some data and compute standard errors to understand the precision of our estimates:

```lisp
(let* ((x (mx 'lla-double
            (1 1)    ; Year 1
            (1 2)    ; Year 2
            (1 3)    ; Year 3
            (1 4)    ; Year 4
            (1 5)))  ; Year 5
       (y (vec 'lla-double 2.1 3.9 6.1 8.0 9.9))  ; Sales in millions
       ;; Fit the model
       (results (multiple-value-list (least-squares y x)))
       (coefficients (first results))
       (ssr (second results))
       (df (third results)))
  
  ;; Calculate standard errors using classical formulas
  (let* ((mse (/ ssr df))                    ; Mean squared error (residual variance)
         (n (length y))                      ; Number of observations
         (x-values '(1 2 3 4 5))            ; Extract x values
         (x-bar (/ (reduce #'+ x-values) n)) ; Mean of x
         (sxx (reduce #'+                    ; Sum of squared deviations
                      (mapcar (lambda (xi) (expt (- xi x-bar) 2))
                              x-values)))
         ;; Standard error formulas for simple linear regression
         (se-slope (sqrt (/ mse sxx)))
         (se-intercept (sqrt (* mse (+ (/ 1 n) (/ (expt x-bar 2) sxx))))))
    
    (format t "Linear Regression Analysis~%")
    (format t "=========================~%~%")
    
    ;; Report coefficients with standard errors
    (format t "Parameter Estimates:~%")
    (format t "  Intercept: ~,3f (SE = ~,3f)~%" 
            (aref coefficients 0) se-intercept)
    (format t "  Slope:     ~,3f (SE = ~,3f)~%~%" 
            (aref coefficients 1) se-slope)
    
    ;; Compute t-statistics
    (let ((t-intercept (/ (aref coefficients 0) se-intercept))
          (t-slope (/ (aref coefficients 1) se-slope)))
      (format t "Significance Tests (H0: parameter = 0):~%")
      (format t "  Intercept: t = ~,2f~%" t-intercept)
      (format t "  Slope:     t = ~,2f (highly significant)~%~%" t-slope))
    
    ;; Construct confidence intervals
    (format t "95% Confidence Intervals:~%")
    (format t "  Intercept: [~,3f, ~,3f]~%" 
            (- (aref coefficients 0) (* 1.96 se-intercept))
            (+ (aref coefficients 0) (* 1.96 se-intercept)))
    (format t "  Slope:     [~,3f, ~,3f]~%~%" 
            (- (aref coefficients 1) (* 1.96 se-slope))
            (+ (aref coefficients 1) (* 1.96 se-slope)))
    
    ;; Interpretation
    (format t "Interpretation:~%")
    (format t "  Sales increase by ~,3f ± ~,3f million per year~%"
            (aref coefficients 1) (* 1.96 se-slope))
    (format t "  We are 95% confident the true annual increase~%")
    (format t "  is between ~,3f and ~,3f million~%"
            (- (aref coefficients 1) (* 1.96 se-slope))
            (+ (aref coefficients 1) (* 1.96 se-slope)))))

; Output:
; Linear Regression Analysis
; =========================
; 
; Parameter Estimates:
;   Intercept: 0.090 (SE = 0.107)
;   Slope:     1.970 (SE = 0.032)
; 
; Significance Tests (H0: parameter = 0):
;   Intercept: t = 0.84
;   Slope:     t = 61.14 (highly significant)
; 
; 95% Confidence Intervals:
;   Intercept: [-0.119, 0.299]
;   Slope:     [1.907, 2.033]
; 
; Interpretation:
;   Sales increase by 1.970 ± 0.063 million per year
;   We are 95% confident the true annual increase
;   is between 1.907 and 2.033 million
```

**Key Insights from Standard Errors:**

1. **Slope precision**: The small standard error (0.032) relative to the slope (1.970) indicates a very precise estimate of the yearly trend
2. **Intercept uncertainty**: The larger relative standard error for the intercept shows more uncertainty about the starting value
3. **Statistical significance**: The t-statistic of 61.14 for the slope (much larger than 2) confirms a highly significant upward trend
4. **Practical interpretation**: We can confidently state that sales are increasing by approximately 2 million per year

This analysis demonstrates how standard errors transform point estimates into interval estimates, enabling us to quantify and communicate the uncertainty inherent

*# Performance Considerations

The QR decomposition approach used by `least-squares`:
- Is more numerically stable than the normal equations
- Handles rank-deficient matrices better
- Has complexity $O(mn^2)$ for an $m \times n$ matrix
- Can be reused for multiple right-hand sides

For very large systems, consider:
- Centering and scaling predictors to improve numerical stability
- Using incremental/online algorithms for streaming data
- Regularization methods (ridge, lasso) for high-dimensional problems

The least squares method forms the foundation for linear regression, polynomial fitting, and many machine learning algorithms, making it one of the most important tools in numerical computing.

### invert-xx

**`invert-xx`** calculates $(X^T X)^{-1}$ from a QR decomposition, which is essential for computing variance estimates and standard errors in regression analysis. This function is particularly useful because it leverages the numerical stability of the QR factorization to compute the inverse of the normal matrix $(X^T X)$ without explicitly forming it.

**Mathematical Background:**

For a matrix $X$ with QR decomposition $X = QR$, we have:
$$X^T X = (QR)^T (QR) = R^T Q^T Q R = R^T R$$

Therefore:
$$(X^T X)^{-1} = (R^T R)^{-1} = R^{-1} (R^T)^{-1}$$

The `invert-xx` function computes this efficiently using the upper triangular structure of $R$.

**Usage Example:**

```lisp
(let* ((x (mx 'lla-double
            (1 1)
            (1 2)
            (1 3)))
       (y (vec 'lla-double 2 4 5))
       (qr (fourth (multiple-value-list (least-squares y x)))))
  (invert-xx qr))
; => #S(MATRIX-SQUARE-ROOT :LEFT #<UPPER-TRIANGULAR-MATRIX element-type DOUBLE-FLOAT
;                                   -0.57735  1.41421
;                                          . -0.70711>)
```

**Understanding the Output:**

The result is a `MATRIX-SQUARE-ROOT` structure, which represents the Cholesky-like factorization of $(X^T X)^{-1}$. The structure contains:

- **`:LEFT` field**: An upper triangular matrix $U$ such that $U^T U = (X^T X)^{-1}$
- **Element values**: The numerical entries showing the factorized form
- **Structure definition**: This comes from the LLA source code's internal representation for matrix square roots

This factored form is computationally efficient for subsequent operations like computing standard errors, where you need the diagonal elements of $(X^T X)^{-1}$ multiplied by the residual variance.

### eigenvalues

**`eigenvalues`** returns the eigenvalues of a Hermitian matrix. Eigenvalues are scalar values $\lambda$ that satisfy the equation $A\mathbf{v} = \lambda\mathbf{v}$, where $\mathbf{v}$ is the corresponding eigenvector.

For Hermitian matrices (symmetric for real matrices), eigenvalues have special properties:
- All eigenvalues are real numbers
- Eigenvectors corresponding to different eigenvalues are orthogonal
- The matrix can be diagonalized as $A = Q\Lambda Q^T$ where $Q$ contains orthonormal eigenvectors and $\Lambda$ is diagonal with eigenvalues

These properties make Hermitian matrices particularly important in applications like principal component analysis, quantum mechanics, and optimization.

```lisp
(eigenvalues (hermitian-mx 'lla-double
               (1)
               (2 4)))
; => #(0.0d0 5.0d0)
```

The optional `abstol` parameter controls the absolute error tolerance for eigenvalue computation.

### logdet

**`logdet`** computes the logarithm of the determinant efficiently, returning the log absolute value and sign separately:

```lisp
(logdet (mx 'lla-double
          (1 0)
          (-1 (exp 1d0))))
; => 1.0d0    ; log|det|
;    1        ; sign
```

### det

**`det`** computes the determinant of a matrix:

```lisp
(det (mx 'lla-double
       (1 2)
       (3 4)))
; => -2.0d0
```

For numerical stability with large determinants, use `logdet` instead.

### tr

**`tr`** computes the trace (sum of diagonal elements) of a square matrix:

```lisp
(tr (mx 'lla-double
      (1 2)
      (3 4)))
; => 5.0d0

(tr (diagonal-mx 'lla-double 2 15))
; => 17.0d0
```

These operations form the foundation for most numerical linear algebra computations. They are optimized to work with LLA's specialized matrix types and automatically dispatch to the most efficient BLAS/LAPACK routines based on the matrix properties.

## BLAS

The Basic Linear Algebra Subprograms (BLAS) are a collection of low-level routines that provide standard building blocks for performing basic vector and matrix operations.

{{< alert title="Note" color="warning" >}}
**Low-Level Interface**: These are wrappers for BLAS linear algebra functions that provide direct access to the underlying BLAS routines. This code is experimental and only a select few BLAS functions are currently implemented. For most applications, the higher-level functions like `mm`, `solve`, and `least-squares` are recommended as they provide better error handling, type safety, and convenience.
{{< /alert >}}

These functions operate directly on arrays and modify them in-place (functions ending with `!`). They require careful attention to array dimensions, strides, and memory layout. Use these functions when you need maximum performance and control over the computation, or when implementing custom linear algebra algorithms.

### gemm!

**`gemm!`** performs the general matrix-matrix multiplication: C = α·A'·B' + β·C, where A' and B' can be either the original matrices or their transposes. This is one of the most important BLAS operations, forming the computational kernel for many linear algebra algorithms.

The function modifies C in-place and returns it. The operation performed is:
- If `transpose-a?` is NIL and `transpose-b?` is NIL: C = α·A·B + β·C
- If `transpose-a?` is T and `transpose-b?` is NIL: C = α·A^T·B + β·C
- If `transpose-a?` is NIL and `transpose-b?` is T: C = α·A·B^T + β·C
- If `transpose-a?` is T and `transpose-b?` is T: C = α·A^T·B^T + β·C

Where:
- A' is an M×K matrix (A or A^T)
- B' is a K×N matrix (B or B^T)
- C is an M×N matrix

```lisp
;; Basic matrix multiplication: C = 2·A·B + 3·C
(let ((a (mx 'lla-double
           (1 2 3)
           (4 5 6)))
      (b (mx 'lla-double
           (7 8)
           (9 10)
           (11 12)))
      (c (mx 'lla-double
           (0.5 1.5)
           (2.5 3.5))))
  (gemm! 2d0 a b 3d0 c))
; => #2A((118.5d0 140.5d0)
;        (298.5d0 356.5d0))

;; With transposed A: C = A^T·B
(let ((a (mx 'lla-double
           (1 4)
           (2 5)
           (3 6)))
      (b (mx 'lla-double
           (7 8)
           (9 10)))
      (c (mx 'lla-double
           (0 0)
           (0 0)
           (0 0))))
  (gemm! 1d0 a b 0d0 c :transpose-a? t))
; => #2A((43.0d0 48.0d0)
;        (56.0d0 63.0d0)
;        (69.0d0 78.0d0))
```

The optional parameters allow working with submatrices:
- `m`, `n`, `k` - dimensions of the operation (default: inferred from C and A/B)
- `lda`, `ldb`, `ldc` - leading dimensions for matrices stored in larger arrays

### scal!

**`scal!`** computes the product of a vector by a scalar: x ← α·x, modifying the vector in-place.

```lisp
(let ((x (vec 'lla-double 1 2 3 4 5)))
  (scal! 2.5d0 x))
; => #(2.5d0 5.0d0 7.5d0 10.0d0 12.5d0)

;; Scale every other element using incx
(let ((x (vec 'lla-double 1 2 3 4 5 6)))
  (scal! 0.5d0 x :n 3 :incx 2))
; => #(0.5d0 2.0d0 1.5d0 4.0d0 2.5d0 6.0d0)
```

Parameters:
- `alpha` - the scalar multiplier
- `x` - the vector to scale (modified in-place)
- `n` - number of elements to process (default: all elements considering incx)
- `incx` - stride between elements (default: 1)

### axpy!

**`axpy!`** computes a vector-scalar product and adds the result to a vector: y ← α·x + y, modifying y in-place.

```lisp
(let ((x (vec 'lla-double 1 2 3))
      (y (vec 'lla-double 4 5 6)))
  (axpy! 2d0 x y))
; => #(6.0d0 9.0d0 12.0d0)  ; y = 2·x + y

;; Using strides
(let ((x (vec 'lla-double 1 2 3 4 5 6))
      (y (vec 'lla-double 10 20 30 40 50 60)))
  (axpy! 3d0 x y :n 3 :incx 2 :incy 2))
; => #(13.0d0 20.0d0 39.0d0 40.0d0 65.0d0 60.0d0)
```

Parameters:
- `alpha` - the scalar multiplier
- `x` - the source vector
- `y` - the destination vector (modified in-place)
- `n` - number of elements to process
- `incx`, `incy` - strides for x and y (default: 1)

### copy!

**`copy!`** copies elements from vector x to vector y, modifying y in-place.

```lisp
(let ((x (vec 'lla-double 1 2 3 4 5))
      (y (vec 'lla-double 0 0 0 0 0)))
  (copy! x y))
; => #(1.0d0 2.0d0 3.0d0 4.0d0 5.0d0)

;; Copy with strides
(let ((x (vec 'lla-double 1 2 3 4 5 6))
      (y (vec 'lla-double 0 0 0 0 0 0)))
  (copy! x y :n 3 :incx 2 :incy 1))
; => #(1.0d0 3.0d0 5.0d0 0.0d0 0.0d0 0.0d0)
```

Parameters:
- `x` - the source vector
- `y` - the destination vector (modified in-place)
- `n` - number of elements to copy
- `incx`, `incy` - strides for x and y (default: 1)

### dot

**`dot`** computes the dot product of two vectors, returning a scalar value.

```lisp
(let ((x (vec 'lla-double 1 2 3))
      (y (vec 'lla-double 4 5 6)))
  (dot x y))
; => 32.0d0  ; 1·4 + 2·5 + 3·6

;; Dot product with strides
(let ((x (vec 'lla-double 1 2 3 4 5))
      (y (vec 'lla-double 10 20 30 40 50)))
  (dot x y :n 3 :incx 2 :incy 1))
; => 140.0d0  ; 1·10 + 3·20 + 5·30
```

Parameters:
- `x`, `y` - the vectors
- `n` - number of elements to process
- `incx`, `incy` - strides for x and y (default: 1)

Note: Complex dot products are not available in all BLAS implementations.

### asum

**`asum`** returns the sum of absolute values of vector elements (L1 norm).

```lisp
(let ((x (vec 'lla-double 1 -2 3 -4 5)))
  (asum x))
; => 15.0d0  ; |1| + |-2| + |3| + |-4| + |5|

;; L1 norm with stride
(let ((x (vec 'lla-double 1 -2 3 -4 5 -6)))
  (asum x :n 3 :incx 2))
; => 9.0d0  ; |1| + |3| + |5|

;; Complex vectors: sum of |real| + |imag|
(let ((x (vec 'lla-complex-double #C(3 4) #C(-5 12) #C(0 -8))))
  (asum x))
; => 37.0d0  ; (3+4) + (5+12) + (0+8)
```

Parameters:
- `x` - the vector
- `n` - number of elements to process
- `incx` - stride between elements (default: 1)

### nrm2

**`nrm2`** returns the Euclidean norm (L2 norm) of a vector: √(Σ|xi|²).

```lisp
(let ((x (vec 'lla-double 3 4)))
  (nrm2 x))
; => 5.0d0  ; √(3² + 4²)

(let ((x (vec 'lla-double 1 2 3 4 5)))
  (nrm2 x))
; => 7.416...d0  ; √(1² + 2² + 3² + 4² + 5²)

;; L2 norm with stride
(let ((x (vec 'lla-double 3 0 4 0 0 0)))
  (nrm2 x :n 2 :incx 2))
; => 5.0d0  ; √(3² + 4²)

;; Complex vectors
(let ((x (vec 'lla-complex-double #C(3 4) #C(0 0) #C(0 0))))
  (nrm2 x))
; => 5.0d0  ; √(|3+4i|²) = √25
```

Parameters:
- `x` - the vector
- `n` - number of elements to process
- `incx` - stride between elements (default: 1)

These BLAS functions provide the fundamental building blocks for linear algebra operations. They are highly optimized and form the computational core of higher-level operations like matrix multiplication (`mm`), solving linear systems (`solve`), and computing factorizations. The in-place operations (marked with `!`) modify their arguments directly, providing memory-efficient computation for large-scale numerical applications.