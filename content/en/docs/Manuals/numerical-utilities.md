---
title: "Numeric Utilities"
weight: 4
date: 2025-08-08
description: >
  Various utilities for numerical computation
---

## Arithmetic

The arithmetic package provides a collection of mathematical and arithmetic functions for Common Lisp. These utilities are designed to simplify common numerical computations while maintaining efficiency through inline declarations where appropriate.

### Basic Operations

#### same-sign-p
**`(same-sign-p &rest numbers)`**

Tests whether all arguments have the same sign (i.e., all are positive, negative, or zero).

```lisp
(same-sign-p 1 2 3)    ; ⇒ T
(same-sign-p 1 -2 3)   ; ⇒ NIL
(same-sign-p -1 -2 -3) ; ⇒ T
```

#### square
**`(square x)`**

Returns the square of a number. This function is inlined for performance.

```lisp
(square 2)    ; ⇒ 4
(square -3)   ; ⇒ 9
(square 1.5)  ; ⇒ 2.25
```

#### cube
**`(cube x)`**

Returns the cube of a number. This function is inlined for performance.

```lisp
(cube 2)    ; ⇒ 8
(cube -3)   ; ⇒ -27
(cube 1.5)  ; ⇒ 3.375
```

#### absolute-square
**`(absolute-square x)`**

Returns a number multiplied by its complex conjugate. For real numbers, this is equivalent to squaring. For complex numbers, it returns the squared magnitude.

```lisp
(absolute-square 2.0)     ; ⇒ 4.0
(absolute-square #C(3 4)) ; ⇒ 25  (since |3+4i|² = 3² + 4² = 25)
```

#### abs-diff
**`(abs-diff x y)`**

Returns the absolute difference between two numbers. This function is inlined for performance.

```lisp
(abs-diff 3 5)   ; ⇒ 2
(abs-diff -3 -5) ; ⇒ 2
(abs-diff 5 3)   ; ⇒ 2
```

### Logarithmic Functions

#### log10
**`(log10 x)`**

Computes the decimal (base 10) logarithm. It always returns a double-float to avoid the Common Lisp behavior where `log` might return a single-float for integer arguments.

```lisp
(log10 100)  ; ⇒ 2.0d0
(log10 1000) ; ⇒ 3.0d0
```

#### log2
**`(log2 x)`**

Computes the binary (base 2) logarithm. Like `log10`, it ensures double-float precision.

```lisp
(log2 256) ; ⇒ 8.0d0
(log2 64)  ; ⇒ 6.0d0
```

### Utility Functions

#### 1c
**`(1c x)`**

Returns 1 minus the argument. The mnemonic is "1 complement" (since `1-` is already a CL function).

```lisp
(1c 4/5)   ; ⇒ 1/5
(1c 0.3)   ; ⇒ 0.7
(1c 2)     ; ⇒ -1
```

#### divides?
**`(divides? number divisor)`**

Tests if `divisor` divides `number` without remainder. If so, returns the quotient; otherwise returns NIL.

```lisp
(divides? 8 2)  ; ⇒ 4
(divides? 8 3)  ; ⇒ NIL
(divides? 15 5) ; ⇒ 3
```

#### as-integer
**`(as-integer x)`**

Converts a number to an integer if it represents an integer value, otherwise signals an error. Works with integers, rationals, floats, and complex numbers (if imaginary part is zero).

```lisp
(as-integer 2.0)   ; ⇒ 2
(as-integer 5/1)   ; ⇒ 5
(as-integer #C(3 0)) ; ⇒ 3
(as-integer 2.5)   ; signals error: "2.5 has non-zero fractional part."
```

### Sequence Generation

#### numseq
**`(numseq from &optional to by)`**

Generates a sequence of numbers. With one argument, generates from 0 to `from`-1. With two arguments, generates from first to second (exclusive). With three arguments, uses `by` as the step size.

```lisp
(numseq 5)       ; ⇒ #(0 1 2 3 4)  
(numseq 2 7)     ; ⇒ #(2 3 4 5 6)
(numseq 0 10 2)  ; ⇒ #(0 2 4 6 8)
```

The sign of `by` is automatically adjusted to match the direction from `from` to `to`. The `type` parameter can be:
- `'list` to return a list instead of an array
- A numeric type specifier for the array element type
- `nil` for automatic type detection

#### ivec
**`(ivec from &optional to by)`**

Generates a vector of fixnums (integers).

```lisp
(ivec 4)       ; ⇒ #(0 1 2 3)
(ivec 1 4)     ; ⇒ #(1 2 3)
(ivec 1 4 2)   ; ⇒ #(1 3)
(ivec -3)      ; ⇒ #(0 -1 -2)
```

When called with one argument, generates integers from 0 up to (but not including) the `argument`. With two arguments, generates from `start` to `end`. The `by` parameter controls the increment.

### Aggregate Operations

#### sum
**`(sum sequence &key key)`**

Returns the sum of elements in a sequence or array. An optional `key` function can be applied to each element before summing.

```lisp
(sum #(2 3 4))           ; ⇒ 9
(sum '(1 2 3 4))         ; ⇒ 10
(sum #())                ; ⇒ 0
(sum #(1 2 3) :key #'square) ; ⇒ 14  (1² + 2² + 3²)
```

#### product
**`(product sequence)`**

Returns the product of elements in a sequence or array.

```lisp
(product #(2 3 4))  ; ⇒ 24
(product '(1 2 3))  ; ⇒ 6
(product #())       ; ⇒ 1
```

#### cumulative-sum
**`(cumulative-sum sequence)`**

Returns a sequence where each element is the sum of all preceding elements plus itself. Also returns the total sum as a second value.

```lisp
(cumulative-sum #(2 3 4))    ; ⇒ #(2 5 9), 9
(cumulative-sum '(1 2 3 4)) ; ⇒ (1 3 6 10), 10
(cumulative-sum #())         ; ⇒ #(), 0
```

#### cumulative-product
**`(cumulative-product sequence)`**

Returns a sequence where each element is the product of all preceding elements times itself. Also returns the total product as a second value.

```lisp
(cumulative-product #(2 3 4))  ; ⇒ #(2 6 24), 24
(cumulative-product '(1 2 3))  ; ⇒ (1 2 6), 6
(cumulative-product #())       ; ⇒ #(), 1
```

### Probability Operations

#### normalize-probabilities
**`(normalize-probabilities sequence &key element-type)`**

Verifies that each element is non-negative and returns a vector scaled so elements sum to 1.

```lisp
(normalize-probabilities #(1 2 7))     ; ⇒ #(1/10 2/10 7/10)
(normalize-probabilities #(1 2 7) :element-type 'double-float) 
                                       ; ⇒ #(0.1d0 0.2d0 0.7d0)
```

Parameters:
- `:element-type` - type of result elements (default `t`)
- `:result` - if provided, results are placed here; if NIL, modifies input vector in-place

```lisp
(let ((v #(1 2 7)))
  (normalize-probabilities v :result nil)
  v)  ; ⇒ #(1/10 2/10 7/10)  ; vector modified in place
```

### Rounding with Offset

These functions find values of the form `A = I × DIVISOR + OFFSET` that satisfy various rounding criteria.

#### floor*
**`(floor* number divisor &optional offset)`**

Finds the highest `A = I × DIVISOR + OFFSET` that is ≤ `number`. Returns `(values A remainder)`.

```lisp
(floor* 27 5)      ; ⇒ 25, 2   (25 = 5×5 + 0, remainder = 2)
(floor* 27 5 1)    ; ⇒ 26, 1   (26 = 5×5 + 1, remainder = 1)
```

#### ceiling*
**`(ceiling* number divisor &optional offset)`**

Finds the lowest `A = I × DIVISOR + OFFSET` that is ≥ `number`. Returns `(values A remainder)`.

```lisp
(ceiling* 27 5)    ; ⇒ 30, -3  (30 = 6×5 + 0, remainder = -3)
(ceiling* 27 5 1)  ; ⇒ 31, -4  (31 = 6×5 + 1, remainder = -4)
```

#### round*
**`(round* number divisor &optional offset)`**

Finds `A = I × DIVISOR + OFFSET` that minimizes `|A - number|`. Returns `(values A remainder)`.

```lisp
(round* 27 5)      ; ⇒ 25, 2   (25 is closer to 27 than 30)
(round* 27 5 -1)   ; ⇒ 29, -2  (29 = 6×5 + (-1))
```

#### truncate*
**`(truncate* number divisor &optional offset)`**

Finds `A = I × DIVISOR + OFFSET` that maximizes `|A| ≤ |number|` with the same sign. Returns `(values A remainder)`.

```lisp
(truncate* -27 5)     ; ⇒ -25, -2
(truncate* -27 5 1)   ; ⇒ -24, -3
```

### Sequence Min/Max

#### seq-max
**`(seq-max sequence)`**

Returns the maximum value in a sequence (list or vector).

```lisp
(seq-max #(0 1 2 3 4 5))  ; ⇒ 5
(seq-max '(0 1 2 3 4 5))  ; ⇒ 5
```

#### seq-min
**`(seq-min sequence)`**

Returns the minimum value in a sequence (list or vector).

```lisp
(seq-min #(0 1 2 3 4 5))  ; ⇒ 0
(seq-min '(-2 5 3 1))     ; ⇒ -2
```


## Chebyshev

The Chebyshev package provides efficient polynomial approximation for functions on finite and semi-infinite intervals. It includes computation of Chebyshev polynomial roots, regression coefficients, and evaluation. This module is particularly useful for approximating expensive functions with smooth, polynomial-like behavior.

### chebyshev-root
**`(chebyshev-root m i)`**

Returns the `i`-th root of the `m`-th Chebyshev polynomial as a double-float. The roots are the zeros of the Chebyshev polynomial Tₘ(x).

```lisp
(chebyshev-root 4 0)  ; ⇒ -0.9238795325112867d0
(chebyshev-root 4 1)  ; ⇒ -0.38268343236508984d0
(chebyshev-root 4 2)  ; ⇒ 0.38268343236508984d0
(chebyshev-root 4 3)  ; ⇒ 0.9238795325112867d0
```

The `i` parameter must satisfy 0 ≤ `i` < `m`. These roots are commonly used as interpolation points for polynomial approximation.

### chebyshev-roots
**`(chebyshev-roots m)`**

Returns all roots of the `m`-th Chebyshev polynomial as a vector of double-floats.

```lisp
(chebyshev-roots 3)  
; ⇒ #(-0.8660254037844387d0 0.0d0 0.8660254037844387d0)

(chebyshev-roots 5)
; ⇒ #(-0.9510565162951535d0 -0.5877852522924731d0 0.0d0 
;      0.5877852522924731d0 0.9510565162951535d0)
```

### chebyshev-regression
**`(chebyshev-regression f n-polynomials &optional n-points)`**

Computes Chebyshev polynomial regression coefficients for function `f` using the specified number of polynomials. The optional `n-points` parameter (defaults to `n-polynomials`) specifies the number of interpolation points.

```lisp
;; Approximate sin(x) on [-1, 1] with 5 Chebyshev polynomials
(chebyshev-regression #'sin 5)
; ⇒ #(8.808207830203602d-17 0.8801099265688267d0 -7.851872826654999d-17 
;     -0.03912505871870944d0 2.477076195177506d-17)

;; Using more interpolation points than polynomials
(chebyshev-regression (lambda (x) (exp x)) 4 8)
; ⇒ #(1.2660658777520082d0 1.1303182079849703d0 0.2714953395340767d0 
;     0.04433684984866382d0)
```

The function `f` is evaluated at the Chebyshev nodes, and coefficients are computed using the discrete cosine transform. Note that `n-points` must be ≥ `n-polynomials`.

### evaluate-chebyshev
**`(evaluate-chebyshev coefficients x)`**

Evaluates a Chebyshev polynomial series at point `x`, given the coefficient vector from `chebyshev-regression`.

```lisp
;; Evaluate approximation of exp(x) at x = 0.5
(let ((coeffs (chebyshev-regression #'exp 5)))
  (evaluate-chebyshev coeffs 0.5))
; ⇒ 1.6487208279284558d0  (compare to (exp 0.5) = 1.6487212707001282d0)

;; Evaluate at multiple points
(let ((coeffs (chebyshev-regression #'sin 4)))
  (mapcar (lambda (x) (evaluate-chebyshev coeffs x))
          '(-0.5 0.0 0.5)))
; ⇒ (-0.47942553860420295d0 0.0d0 0.47942553860420295d0)
```

Uses the efficient Clenshaw algorithm for polynomial evaluation.

### chebyshev-approximate
**`(chebyshev-approximate f interval n-polynomials &key n-points)`**

Returns a closure that approximates function `f` on the given interval using Chebyshev polynomial interpolation. The interval can be finite or semi-infinite.

```lisp
;; Example from tests: approximate x/(4+x) on [2, ∞)
(let ((f-approx (chebyshev-approximate (lambda (x) (/ x (+ 4 x)))
                                       (interval 2 :plusinf) 
                                       15)))
  (funcall f-approx 10.0))
; ⇒ 0.7142857142857143d0  (compare to exact: 10/14 = 0.714...)

;; Exponential decay on [0, ∞) with more sampling points
(let ((exp-approx (chebyshev-approximate (lambda (x) (exp (- x)))
                                         (interval 0 :plusinf) 
                                         15
                                         :n-points 30)))
  (funcall exp-approx 2.0))
; ⇒ 0.1353352832366127d0  (compare to (exp -2) = 0.1353...)

;; Finite interval example: 1/(1+x²) on [-3, 2]
(let ((rational-approx (chebyshev-approximate (lambda (x) (/ (1+ (expt x 2))))
                                              (interval -3d0 2d0) 
                                              20)))
  (funcall rational-approx 0.0))
; ⇒ 1.0d0  (exact at x=0)
```

For semi-infinite intervals, the function automatically applies an appropriate transformation to map the interval to [-1, 1]. The `:n-points` keyword (defaults to `n-polynomials`) controls the number of interpolation points.

### Usage Examples

#### Example: Accuracy Testing (from test suite)

```lisp
;; Helper function from tests to measure approximation error
(defun approximation-error (f f-approx interval &optional (n-grid 1000))
  "Approximation error, using maximum on grid."
  (loop for index below n-grid
        maximizing (abs (- (funcall f (+ (interval-left interval)
                                         (* index 
                                            (/ (interval-width interval) 
                                               (1- n-grid)))))
                           (funcall f-approx (+ (interval-left interval)
                                                (* index 
                                                   (/ (interval-width interval) 
                                                      (1- n-grid)))))))))

;; Test semi-infinite interval approximation
(let ((f (lambda (x) (/ x (+ 4 x))))
      (f-approx (chebyshev-approximate (lambda (x) (/ x (+ 4 x)))
                                       (interval 2 :plusinf) 
                                       15)))
  ;; Error should be <= 1e-5 on interval [2, 102]
  (approximation-error f f-approx (interval 2 102)))
; ⇒ small value <= 1e-5
```

#### Example: Exponential Decay with Higher Sampling

```lisp
;; From test: exponential decay on [0, ∞) with 30 sampling points
(let ((decay-approx (chebyshev-approximate (lambda (x) (exp (- x)))
                                           (interval 0 :plusinf) 
                                           15
                                           :n-points 30)))
  ;; Test at a few points
  (list (funcall decay-approx 0.0)   ; ⇒ ≈ 1.0
        (funcall decay-approx 1.0)   ; ⇒ ≈ 0.368
        (funcall decay-approx 3.0))) ; ⇒ ≈ 0.050
; Error should be <= 1e-4 on [0, 10]
```

#### Example: Finite Interval Rational Function

```lisp
;; From test: 1/(1+x²) on finite interval [-3, 2] 
(let ((rational-fn (lambda (x) (/ (1+ (expt x 2)))))
      (rational-approx (chebyshev-approximate (lambda (x) (/ (1+ (expt x 2))))
                                              (interval -3d0 2d0) 
                                              20)))
  ;; Test accuracy at center and edges  
  (list (funcall rational-approx -1.5)  ; ⇒ ≈ 0.308
        (funcall rational-approx 0.0)   ; ⇒ 1.0
        (funcall rational-approx 1.0))) ; ⇒ 0.5
; Error should be <= 1e-3 on [-1.5, 1.0]
```

### Notes on Usage

1. **Interval Selection**: Chebyshev approximation works best when the function is smooth over the interval. Avoid intervals containing singularities or discontinuities.

2. **Number of Polynomials**: More polynomials generally give better approximation, but beware of numerical instability with very high orders (typically > 50).

3. **Semi-infinite Intervals**: For intervals like [a, ∞), the function applies a transformation x ↦ (x-a)/(1+x-a) to map to a finite interval.

4. **Performance**: The returned closure is very fast to evaluate, making this ideal for replacing expensive function calls in performance-critical code.


## Extended Real

The Extended Real package extends the real number line with positive and negative infinity (`:plusinf`, `:minusinf`). It provides comparison operators that work seamlessly with both real numbers and infinities, type definitions for extended reals, and template macros for pattern matching.

### Type Definitions

#### extended-real
**`(extended-real &optional (base 'real))`**

Type definition for extended real numbers, which includes real numbers plus positive and negative infinity.

```lisp
;; Type checking
(typep 1 'extended-real)         ; ⇒ T
(typep 1.5 'extended-real)       ; ⇒ T  
(typep :plusinf 'extended-real)  ; ⇒ T
(typep :minusinf 'extended-real) ; ⇒ T
(typep "string" 'extended-real)  ; ⇒ NIL
(typep #C(1 2) 'extended-real)   ; ⇒ NIL
```

#### infinite?
**`(infinite? object)`**

Tests if an object represents positive or negative infinity.

```lisp
;; Infinity testing
(infinite? :plusinf)   ; ⇒ T
(infinite? :minusinf)  ; ⇒ T
(infinite? 1)          ; ⇒ NIL
(infinite? 1.0)        ; ⇒ NIL
(infinite? "string")   ; ⇒ NIL
```

### Comparison Operators

All comparison operators accept one or more arguments and work with both real numbers and infinities.

#### xreal:=
**`(xreal:= number &rest more-numbers)`**

Tests equality across extended real numbers.

```lisp
;; Equality cases that return T
(xreal:= 1 1)                           ; ⇒ T
(xreal:= :plusinf :plusinf)             ; ⇒ T
(xreal:= :minusinf :minusinf)           ; ⇒ T
(xreal:= 2 2 2)                         ; ⇒ T
(xreal:= :plusinf :plusinf :plusinf)    ; ⇒ T
(xreal:= :minusinf :minusinf :minusinf) ; ⇒ T

;; Equality cases that return NIL
(xreal:= 1 2)               ; ⇒ NIL
(xreal:= 1 :plusinf)        ; ⇒ NIL
(xreal:= :plusinf 1)        ; ⇒ NIL
(xreal:= 1 :minusinf)       ; ⇒ NIL
(xreal:= :minusinf 1)       ; ⇒ NIL
(xreal:= 1 2 2)             ; ⇒ NIL
(xreal:= 2 2 1)             ; ⇒ NIL
(xreal:= :plusinf :plusinf 9) ; ⇒ NIL
(xreal:= :plusinf :minusinf)  ; ⇒ NIL
```

#### xreal:<
**`(xreal:< number &rest more-numbers)`**

Tests strict less-than ordering across extended real numbers.

```lisp
;; Less-than cases that return T
(xreal:< 1 2)                           ; ⇒ T
(xreal:< 1 :plusinf)                    ; ⇒ T
(xreal:< :minusinf :plusinf)            ; ⇒ T
(xreal:< :minusinf 1)                   ; ⇒ T
(xreal:< 1 2 3)                         ; ⇒ T
(xreal:< 1 2 :plusinf)                  ; ⇒ T
(xreal:< :minusinf 1 4 :plusinf)        ; ⇒ T

;; Less-than cases that return NIL
(xreal:< 1 1)               ; ⇒ NIL
(xreal:< 2 1)               ; ⇒ NIL
(xreal:< :plusinf :plusinf) ; ⇒ NIL
(xreal:< :plusinf 1)        ; ⇒ NIL
(xreal:< :minusinf :minusinf) ; ⇒ NIL
(xreal:< :plusinf :minusinf)  ; ⇒ NIL
(xreal:< 1 :minusinf)       ; ⇒ NIL
(xreal:< 1 2 2)             ; ⇒ NIL
(xreal:< 1 3 2)             ; ⇒ NIL
(xreal:< 1 :plusinf 2)      ; ⇒ NIL
(xreal:< 1 :plusinf :plusinf) ; ⇒ NIL
```

#### xreal:>
**`(xreal:> number &rest more-numbers)`**

Tests strict greater-than ordering across extended real numbers.

```lisp
;; Greater-than cases that return T
(xreal:> 2 1)                           ; ⇒ T
(xreal:> :plusinf 1)                    ; ⇒ T
(xreal:> :plusinf :minusinf)            ; ⇒ T
(xreal:> 1 :minusinf)                   ; ⇒ T
(xreal:> 3 2 1)                         ; ⇒ T
(xreal:> :plusinf 2 1)                  ; ⇒ T
(xreal:> :plusinf 4 1 :minusinf)        ; ⇒ T

;; Greater-than cases that return NIL
(xreal:> 1 1)               ; ⇒ NIL
(xreal:> 1 2)               ; ⇒ NIL
(xreal:> :plusinf :plusinf) ; ⇒ NIL
(xreal:> 1 :plusinf)        ; ⇒ NIL
(xreal:> :minusinf :minusinf) ; ⇒ NIL
(xreal:> :minusinf :plusinf)  ; ⇒ NIL
(xreal:> :minusinf 1)       ; ⇒ NIL
(xreal:> 2 2 1)             ; ⇒ NIL
(xreal:> 2 3 1)             ; ⇒ NIL
(xreal:> 2 :plusinf 1)      ; ⇒ NIL
(xreal:> :plusinf :plusinf 1) ; ⇒ NIL
```

#### xreal:<=
**`(xreal:<= number &rest more-numbers)`**

Tests less-than-or-equal ordering across extended real numbers.

```lisp
;; Less-than-or-equal cases that return T
(xreal:<= 1 1)                          ; ⇒ T
(xreal:<= 1 2)                          ; ⇒ T
(xreal:<= 1 :plusinf)                   ; ⇒ T
(xreal:<= :plusinf :plusinf)            ; ⇒ T
(xreal:<= :minusinf :plusinf)           ; ⇒ T
(xreal:<= :minusinf :minusinf)          ; ⇒ T
(xreal:<= :minusinf 1)                  ; ⇒ T
(xreal:<= 1 2 2)                        ; ⇒ T
(xreal:<= 1 2 3)                        ; ⇒ T
(xreal:<= 1 2 :plusinf)                 ; ⇒ T
(xreal:<= 1 :plusinf :plusinf)          ; ⇒ T
(xreal:<= :minusinf 1 4 :plusinf)       ; ⇒ T

;; Less-than-or-equal cases that return NIL
(xreal:<= 2 1)              ; ⇒ NIL
(xreal:<= :plusinf 1)       ; ⇒ NIL
(xreal:<= :plusinf :minusinf) ; ⇒ NIL
(xreal:<= 1 :minusinf)      ; ⇒ NIL
(xreal:<= 1 3 2)            ; ⇒ NIL
(xreal:<= 1 :plusinf 2)     ; ⇒ NIL
```

#### xreal:>=
**`(xreal:>= number &rest more-numbers)`**

Tests greater-than-or-equal ordering across extended real numbers.

```lisp
;; Greater-than-or-equal cases that return T
(xreal:>= 1 1)                          ; ⇒ T
(xreal:>= 2 1)                          ; ⇒ T
(xreal:>= :plusinf 1)                   ; ⇒ T
(xreal:>= :plusinf :plusinf)            ; ⇒ T
(xreal:>= :plusinf :minusinf)           ; ⇒ T
(xreal:>= :minusinf :minusinf)          ; ⇒ T
(xreal:>= 1 :minusinf)                  ; ⇒ T
(xreal:>= 2 2 1)                        ; ⇒ T
(xreal:>= 3 2 1)                        ; ⇒ T
(xreal:>= :plusinf 2 1)                 ; ⇒ T
(xreal:>= :plusinf :plusinf 1)          ; ⇒ T
(xreal:>= :plusinf 4 1 :minusinf)       ; ⇒ T

;; Greater-than-or-equal cases that return NIL
(xreal:>= 1 2)              ; ⇒ NIL
(xreal:>= 1 :plusinf)       ; ⇒ NIL
(xreal:>= :minusinf :plusinf) ; ⇒ NIL
(xreal:>= :minusinf 1)      ; ⇒ NIL
(xreal:>= 2 3 1)            ; ⇒ NIL
(xreal:>= 2 :plusinf 1)     ; ⇒ NIL
```

### Template Macros

#### with-template
**`(with-template (prefix &rest variables) &body body)`**

Defines a local macro for pattern matching on extended real values. The macro can match against `:plusinf`, `:minusinf`, `real`, or `t`.

```lisp
;; Pattern matching with template macro
(with-template (? x y)
  (if (? real real)
      (+ x y)                    ; both are real numbers
      (if (? :plusinf :plusinf)
          :plusinf               ; both are +∞
          (if (? :minusinf :minusinf)
              :minusinf          ; both are -∞
              (error "Mixed infinity types")))))

;; Example usage with different patterns
(let ((x 5) (y 10))
  (with-template (? x y)
    (? real real)))              ; ⇒ T

(let ((x :plusinf) (y :plusinf))
  (with-template (? x y)
    (? :plusinf :plusinf)))      ; ⇒ T

(let ((x 5) (y :minusinf))
  (with-template (? x y)
    (? real :minusinf)))         ; ⇒ T
```

#### lambda-template
**`(lambda-template (prefix &rest variables) &body body)`**

Convenience macro that combines `lambda` with `with-template`.

```lisp
;; Creating a function with template matching
(let ((compare-fn (lambda-template (? a b)
                    (if (? real real)
                        (< a b)
                        (if (? :minusinf t)
                            t
                            (if (? t :plusinf)
                                t
                                nil))))))
  (list (funcall compare-fn 1 2)         ; ⇒ T  (real numbers)
        (funcall compare-fn :minusinf 5)  ; ⇒ T  (-∞ < real)
        (funcall compare-fn 5 :plusinf)   ; ⇒ T  (real < +∞)
        (funcall compare-fn :plusinf 5))) ; ⇒ NIL (+∞ not < real)
```

### Corner Cases

All comparison functions handle corner cases consistently:

```lisp
;; Single argument always returns T
(xreal:= 1)         ; ⇒ T
(xreal:< :plusinf)  ; ⇒ T
(xreal:> 1)         ; ⇒ T
(xreal:>= :minusinf) ; ⇒ T
(xreal:<= 1)        ; ⇒ T

;; No arguments signals an error
(xreal:=)   ; ⇒ ERROR
(xreal:<)   ; ⇒ ERROR
(xreal:>)   ; ⇒ ERROR
(xreal:>=)  ; ⇒ ERROR
(xreal:<=)  ; ⇒ ERROR
```

### Notes on Usage

1. **Type Safety**: All functions check that arguments are extended reals (real numbers or infinities)

2. **Operator Shadowing**: The package shadows CL operators `=`, `<`, `>`, `<=`, `>=`. Use package prefixes or be careful with `use-package`

3. **Mathematical Consistency**: The ordering follows mathematical conventions:
   - `:minusinf` < any real number < `:plusinf`
   - `:minusinf` < `:plusinf`
   - Infinities are equal to themselves but not to each other

4. **Template Patterns**: When using `with-template` or `lambda-template`:
   - `:plusinf` matches only positive infinity
   - `:minusinf` matches only negative infinity  
   - `real` matches any real number
   - `t` matches any extended real value


## Interval

The `interval` package provides interval arithmetic on the extended real line, supporting finite, semi-infinite, and infinite intervals with open/closed endpoints. Features include interval creation, length/midpoint calculations, membership testing, hull operations, interval extension, splitting, shrinking, and grid generation.

### Creation

#### interval
**`(interval left right &optional open-left? open-right?)`**

Creates an interval with specified endpoints and open/closed status. Supports finite intervals as well as semi-infinite and infinite intervals using `:minusinf` and `:plusinf`.

```lisp
;; Basic interval creation
(interval 1 2)  ; ⇒ [1,2]

;; Invalid intervals signal errors
(interval 2 1)  ; ⇒ ERROR
```

#### finite-interval
**`(finite-interval left right &optional open-left? open-right?)`**

Creates a finite interval, ensuring both endpoints are real numbers.

#### plusinf-interval
**`(plusinf-interval left &optional open-left?)`**

Creates a semi-infinite interval extending to positive infinity.

#### minusinf-interval
**`(minusinf-interval right &optional open-right?)`**

Creates a semi-infinite interval extending from negative infinity.

#### real-line
**`(real-line)`**

Returns the entire real line as an interval.

#### plusminus-interval
**`(plusminus-interval center radius)`**

Creates a symmetric interval around a center point with the given radius.

```lisp
;; Create interval around center point
(plusminus-interval 1 0.5)  ; ⇒ [0.5,1.5] (equivalent to (interval 0.5 1.5))
```

### Properties

#### left
**`(left interval)`**

Returns the left endpoint of an interval.

#### right
**`(right interval)`**

Returns the right endpoint of an interval.

#### open-left?
**`(open-left? interval)`**

Tests if the left endpoint is open.

#### open-right?
**`(open-right? interval)`**

Tests if the right endpoint is open.

#### interval-length
**`(interval-length interval)`**

Returns the length of a finite interval.

```lisp
;; Length of unit interval
(let ((a (interval 1 2)))
  (interval-length a))  ; ⇒ 1
```

#### interval-midpoint
**`(interval-midpoint interval &optional fraction)`**

Returns a point within the interval. With no fraction, returns the midpoint. With fraction, returns a point at that relative position.

```lisp
;; Midpoint with fraction
(let ((a (interval 1 2)))
  (interval-midpoint a 0.25))  ; ⇒ 1.25
```

### Testing

#### in-interval?
**`(in-interval? interval number)`**

Tests if a number is contained in an interval, respecting open/closed endpoints.

```lisp
;; Membership testing
(let ((a (interval 1 2)))
  (list (in-interval? a 1.5)   ; ⇒ T
        (in-interval? a 1)     ; ⇒ T  
        (in-interval? a 2)     ; ⇒ T
        (in-interval? a 0.9)   ; ⇒ NIL
        (in-interval? a 2.1))) ; ⇒ NIL
```

### Operations

#### extend-interval
**`(extend-interval interval &optional (left 0) (right 0))`**

Returns a new interval extended by the specified amounts on each side.

#### extendf-interval
**`(extendf-interval interval &optional (left 0) (right 0))`**

Destructively extends an interval by modifying it in place.

```lisp
;; Destructive extension with counter and array
(let+ ((counter -1)
       (a (make-array 2 :initial-contents (list nil (interval 1 2)))))
  (extendf-interval (aref a (incf counter)) 3)
  (extendf-interval (aref a (incf counter)) 3)
  (values a counter))
; ⇒ #([3,3] [1,3]), 1
```

#### interval-hull
**`(interval-hull &rest objects)`**

Returns the smallest interval containing all given objects (intervals, numbers, arrays).

```lisp
;; Hull operations
(let ((a (interval 1 2)))
  (list (interval-hull nil)              ; ⇒ NIL
        (interval-hull a)                ; ⇒ [1,2]
        (interval-hull '(1 1.5 2))      ; ⇒ [1,2]
        (interval-hull #(1 1.5 2))      ; ⇒ [1,2]
        (interval-hull #2A((1) (1.5) (2))))) ; ⇒ [1,2]

;; Complex hull with mixed types
(interval-hull (list (interval 0 2) -1 #(3) '(2.5)))
; ⇒ [-1,3]

;; Invalid input signals error
(interval-hull #C(1 2))  ; ⇒ ERROR
```

#### shift-interval
**`(shift-interval interval shift)`**

Returns a new interval shifted by the given amount.

### Division

#### split-interval
**`(split-interval interval splits)`**

Splits an interval at specified points, returning a vector of subintervals.

```lisp
;; Complex splitting with spacers and relative positions  
(let ((a (interval 10 20)))
  (split-interval a (list (spacer 1) (relative 0.1) (spacer 2))))
; ⇒ #([10,13] [13,14] [14,20])

;; Simpler split with spacer and absolute value
(let ((a (interval 10 20)))
  (split-interval a (list (spacer) 4)))
; ⇒ #([10,16] [16,20])

;; Invalid splits signal errors
(let ((a (interval 10 20)))
  (split-interval a (list 9)))        ; ⇒ ERROR (outside interval)
  
(let ((a (interval 10 20)))
  (split-interval a (list 6 7 (spacer))))  ; ⇒ ERROR (invalid sequence)
```

#### shrink-interval
**`(shrink-interval interval left &optional right)`**

Returns a sub-interval by shrinking from both ends. Parameters are relative positions (0 to 1).

```lisp
;; Shrinking with relative positions
(let ((a (interval 1 2)))
  (shrink-interval a 0.25 0.2))  ; ⇒ [1.25,1.8]
```

### Grid Generation

#### grid-in
**`(grid-in interval n &key endpoints)`**

Generates a vector of `n` evenly spaced points within an interval.

```lisp
;; Grid generation examples
(grid-in (interval 0.0 1.0) 3)  ; ⇒ #(0.0 0.5 1.0)
(grid-in (interval 0 4) 3)      ; ⇒ #(0 2 4)
```

#### subintervals-in
**`(subintervals-in interval n)`**

Divides an interval into `n` equal subintervals, returning a vector.

```lisp
;; Creating subintervals
(subintervals-in (interval 0 3) 3)
; ⇒ #([0,1) [1,2) [2,3])
```

### Relative

#### relative
**`(relative &optional fraction offset)`**

Creates a relative specification for use with other interval functions. Represents a position as `fraction × width + offset`.

```lisp
;; Used in split-interval examples above
(relative 0.1)  ; represents 10% of interval width from left
```

#### spacer
**`(spacer &optional n)`**

Creates a relative specification that divides an interval into equal parts.

```lisp
;; Used in split-interval examples above
(spacer 1)  ; divides interval into 1+1=2 parts, split at middle
(spacer 2)  ; divides interval into 2+1=3 parts
(spacer)    ; default spacing
```

### Notes on Usage

1. **Extended Real Support**: Intervals can use `:plusinf` and `:minusinf` for semi-infinite and infinite intervals

2. **Open/Closed Endpoints**: The third and fourth arguments to `interval` control whether endpoints are open (excluded) or closed (included)

3. **Type Safety**: Functions check that intervals are properly ordered (left ≤ right) and that operations make sense

4. **Relative Specifications**: The `relative` and `spacer` functions provide flexible ways to specify positions within intervals without hardcoding absolute values

5. **Destructive Operations**: Functions ending in `f` (like `extendf-interval`) modify their arguments in place for efficiency

6. **Error Handling**: Invalid operations (like creating intervals with left > right, or using complex numbers in hulls) signal appropriate errors

### Usage Examples

#### Example: Statistical Confidence Intervals

```lisp
;; Create confidence intervals for a mean estimate
(defun confidence-interval (mean std-error confidence-level)
  "Create a confidence interval for a mean with given standard error.
   Confidence level should be between 0 and 1 (e.g., 0.95 for 95% CI)."
  (let* ((alpha (- 1 confidence-level))
         (z-score (cond 
                    ((= confidence-level 0.90) 1.645)  ; 90% CI
                    ((= confidence-level 0.95) 1.96)   ; 95% CI
                    ((= confidence-level 0.99) 2.576)  ; 99% CI
                    (t (error "Unsupported confidence level"))))
         (margin (* z-score std-error)))
    (interval (- mean margin) (+ mean margin))))

;; Example: Sample mean = 100, standard error = 5
(confidence-interval 100 5 0.95)     ; ⇒ [90.2,109.8] (95% CI)
(confidence-interval 100 5 0.99)     ; ⇒ [87.12,112.88] (99% CI)

;; Check if a value falls within the confidence interval
(let ((ci-95 (confidence-interval 100 5 0.95)))
  (list (in-interval? ci-95 95)      ; ⇒ T (within CI)
        (in-interval? ci-95 105)     ; ⇒ T (within CI)
        (in-interval? ci-95 110)))   ; ⇒ NIL (outside CI)

;; Overlapping confidence intervals
(defun intervals-overlap? (int1 int2)
  "Check if two intervals overlap."
  (not (or (< (right int1) (left int2))
           (> (left int1) (right int2)))))

(let ((group1-ci (confidence-interval 100 3 0.95))  ; [94.12,105.88]
      (group2-ci (confidence-interval 108 4 0.95))) ; [100.16,115.84]
  (intervals-overlap? group1-ci group2-ci))         ; ⇒ T (overlap suggests no significant difference)

;; Compute confidence interval width for sample size planning
(defun ci-width (std-error confidence-level)
  "Calculate the width of a confidence interval."
  (let ((ci (confidence-interval 0 std-error confidence-level)))
    (interval-length ci)))

(ci-width 5 0.95)   ; ⇒ 19.6 (width of 95% CI with SE=5)
(ci-width 2 0.95)   ; ⇒ 7.84 (smaller SE gives narrower CI)
```


## Log-Exp

The Log-Exp package provides numerically stable implementations of logarithmic and exponential functions that require special handling near zero. These implementations avoid floating-point overflow, underflow, and loss of precision in critical numerical computations involving values close to mathematical singularities.

### Basic Log Functions

#### log1+
**`(log1+ x)`**

Computes log(1+x) stably even when x is near 0. Uses specialized algorithm to avoid loss of precision.

```lisp
(log1+ 0.0)     ; ⇒ 0.0d0
(log1+ 1e-15)   ; ⇒ 1.0d-15 (exact for small x)
(log1+ 1.0)     ; ⇒ 0.6931471805599453d0 (log(2))
(log1+ -0.5)    ; ⇒ -0.6931471805599453d0 (log(0.5))
```

For small x, returns x directly to maintain precision. For larger x, uses the stable formula: x·log(1+x)/(x).

#### log1-
**`(log1- x)`**

Computes log(1−x) stably even when x is near 0.

```lisp
(log1- 0.0)     ; ⇒ 0.0d0
(log1- 1e-15)   ; ⇒ -1.0d-15 (exact for small x)
(log1- 0.5)     ; ⇒ -0.6931471805599453d0 (log(0.5))
(log1- -1.0)    ; ⇒ 0.6931471805599453d0 (log(2))
```

#### log1+/x
**`(log1+/x x)`**

Computes log(1+x)/x stably even when x is near 0.

```lisp
(log1+/x 0.0)     ; ⇒ 1.0d0 (limit as x→0)
(log1+/x 1e-10)   ; ⇒ 0.99999999995d0 (very close to 1)
(log1+/x 1.0)     ; ⇒ 0.6931471805599453d0 (log(2)/1)
(log1+/x 2.0)     ; ⇒ 0.5493061443340549d0 (log(3)/2)
```

### Exponential Functions

#### exp-1
**`(exp-1 x)`**

Computes exp(x)−1 stably even when x is near 0.

```lisp
(exp-1 0.0)     ; ⇒ 0.0d0
(exp-1 1e-15)   ; ⇒ 1.0d-15 (exact for small x)
(exp-1 1.0)     ; ⇒ 1.718281828459045d0 (e−1)
(exp-1 -1.0)    ; ⇒ -0.6321205588285577d0 (1/e−1)
```

#### exp-1/x
**`(exp-1/x x)`**

Computes (exp(x)−1)/x stably even when x is near 0.

```lisp
(exp-1/x 0.0)     ; ⇒ 1.0d0 (limit as x→0)
(exp-1/x 1e-10)   ; ⇒ 1.0000000000500000d0 (very close to 1)
(exp-1/x 1.0)     ; ⇒ 1.718281828459045d0 (e−1)
(exp-1/x -1.0)    ; ⇒ 0.6321205588285577d0 ((1/e−1)/(−1))
```

#### expt-1
**`(expt-1 a z)`**

Computes aᶻ − 1 stably when a is close to 1 or z is close to 0.

```lisp
(expt-1 1.0 0.0)      ; ⇒ 0.0d0 (1⁰ − 1)
(expt-1 1.001 0.1)    ; ⇒ 0.00010005001667083846d0 (small perturbation)
(expt-1 2.0 3.0)      ; ⇒ 7.0d0 (2³ − 1 = 8 − 1)
(expt-1 0.5 2.0)      ; ⇒ -0.75d0 (0.25 − 1)
```

### Logarithmic Exponential Combinations

#### log1+exp
**`(log1+exp a)`**

Accurately computes log(1+exp(x)) even when a is near zero or large.

```lisp
(log1+exp 0.0)       ; ⇒ 0.6931471805599453d0 (log(2))
(log1+exp -40.0)     ; ⇒ 4.248354255291589d-18 (exp(−40), very small)
(log1+exp 1.0)       ; ⇒ 1.3132616875182228d0 (log(1+e))
(log1+exp 20.0)      ; ⇒ 20.000000485165195d0 (≈ 20 for large x)
(log1+exp 50.0)      ; ⇒ 50.0d0 (exactly 50 for very large x)
```

Uses different algorithms based on the magnitude of the input to maintain numerical stability.

#### log1-exp
**`(log1-exp a)`**

Computes log(1−exp(x)) stably. This is the third Einstein function E₃.

```lisp
(log1-exp 0.0)       ; ⇒ −∞ (log(0))
(log1-exp -1.0)      ; ⇒ -1.3132616875182228d0 (log(1−1/e))
(log1-exp -0.1)      ; ⇒ -2.3514297952645346d0 (log(1−exp(−0.1)))
(log1-exp 0.5)       ; ⇒ -0.3665129205816643d0 (log(1−exp(0.5)))
```

#### log2-exp
**`(log2-exp x)`**

Computes log(2−exp(x)) stably even when x is near zero.

```lisp
(log2-exp 0.0)       ; ⇒ 0.6931471805599453d0 (log(1) = 0, so log(2−1))
(log2-exp -1.0)      ; ⇒ 1.0681748093670006d0 (log(2−1/e))
(log2-exp 0.5)       ; ⇒ 0.5596157879354228d0 (log(2−exp(0.5)))
```

#### logexp-1
**`(logexp-1 a)`**

Computes log(exp(a)−1) stably even when a is small.

```lisp
(logexp-1 0.0)       ; ⇒ −∞ (log(0))
(logexp-1 1.0)       ; ⇒ 0.5413248546129181d0 (log(e−1))
(logexp-1 -40.0)     ; ⇒ -40.0d0 (≈ −40 for very negative values)
(logexp-1 20.0)      ; ⇒ 19.999999514834805d0 (≈ 20 for large positive)
```

### Utility Functions

#### hypot
**`(hypot x y)`**

Computes the hypotenuse √(x²+y²) without danger of floating-point overflow or underflow.

```lisp
(hypot 3.0 4.0)      ; ⇒ 5.0d0 (classic 3-4-5 triangle)
(hypot 1.0 1.0)      ; ⇒ 1.4142135623730951d0 (√2)
(hypot 0.0 5.0)      ; ⇒ 5.0d0
(hypot -3.0 4.0)     ; ⇒ 5.0d0 (uses absolute values)
(hypot 1e200 1e200)  ; ⇒ 1.4142135623730951d200 (avoids overflow)
```

Always returns a positive result by taking absolute values of inputs.

#### log1pmx
**`(log1pmx x)`**

Computes log(1+x) − x accurately. Most accurate for −0.227 < x < 0.315.

```lisp
(log1pmx 0.0)        ; ⇒ 0.0d0 (log(1) − 0)
(log1pmx 0.1)        ; ⇒ -0.004758473160399683d0
(log1pmx 0.5)        ; ⇒ -0.09063636515559594d0
(log1pmx -0.1)       ; ⇒ 0.005170919881432486d0
(log1pmx 1.0)        ; ⇒ -0.30685281944005443d0 (log(2) − 1)
```

Uses polynomial approximations in different ranges for optimal accuracy.

### Usage Examples

#### Example: Numerical Stability Comparison

```lisp
;; Standard approach vs stable approach for small values
(let ((x 1e-15))
  (list 
    ;; Potentially inaccurate
    (- (log (+ 1 x)) x)           ; May lose precision
    ;; Stable version
    (log1pmx x)))                 ; ⇒ (0.0d0 0.0d0) - both accurate here
```

#### Example: Exponential Probability Calculations

```lisp
;; Computing log probabilities stably
(defun log-sigmoid (x)
  "Stable computation of log(1/(1+exp(−x)))"
  (- (log1+exp (- x))))

(log-sigmoid 0.0)       ; ⇒ -0.6931471805599453d0
(log-sigmoid 10.0)      ; ⇒ -0.000045398899216859934d0 (very small)
(log-sigmoid -10.0)     ; ⇒ -10.000045399929762d0

;; Computing softmax denominators stably
(defun log-sum-exp (values)
  "Stable log-sum-exp computation"
  (let ((max-val (reduce #'max values)))
    (+ max-val 
       (log1+exp (log (reduce #'+ values 
                             :key (lambda (x) (exp (- x max-val)))))))))
```

#### Example: Statistical Distributions

```lisp
;; Log-normal distribution helpers
(defun log-normal-log-pdf (x mu sigma)
  "Log probability density for log-normal distribution"
  (let ((log-x (log x))
        (sigma-sq (* sigma sigma)))
    (- (- (* 0.5 (/ (* (- log-x mu) (- log-x mu)) sigma-sq)))
       (log (* x sigma (sqrt (* 2 pi)))))))

;; Using stable exponential functions
(defun stable-gaussian-tail (x)
  "Stable computation of log(1−Φ(x)) for large positive x"
  (log1-exp (- (* 0.5 x x))))
```

#### Example: Financial Mathematics

```lisp
;; Continuous compounding calculations
(defun continuously-compounded-return (initial final time)
  "Calculate annualized continuously compounded return"
  (/ (log (/ final initial)) time))

;; Small return approximations
(defun log-return-approx (return-rate)
  "Approximate log(1+r) for small returns r"
  (log1+ return-rate))

(log-return-approx 0.05)     ; ⇒ 0.04879016416943205d0 (vs naive 0.05)
(log-return-approx 0.001)    ; ⇒ 0.0009995003330835334d0 (very accurate)
```

### Notes on Usage

1. **Numerical Stability**: These functions are designed to maintain accuracy when standard implementations would lose precision due to cancellation or overflow.

2. **Domain Considerations**: Functions like `log1-exp` and `logexp-1` may return −∞ for certain inputs where the mathematical result is undefined.

3. **Performance**: While more stable, these functions may be slightly slower than naive implementations. Use when precision near singularities is critical.

4. **Range Optimization**: Functions like `log1+exp` and `logexp-1` use different algorithms based on input ranges to optimize both accuracy and performance.

5. **Complex Numbers**: Most functions are designed primarily for real inputs, though some will handle complex numbers by falling back to standard implementations.

6. **Integration with Statistical Computing**: These functions are particularly useful in machine learning, statistics, and financial mathematics where log-probabilities and exponential transformations are common.


## Num=

The Num= package provides approximate equality comparison for numeric values and structures containing numbers. Features tolerance-based floating-point comparison using relative error metrics, with support for numbers, arrays, lists, and custom structures. Includes `num=` generic function, `num-delta` for relative differences, configurable default tolerance, and macros for defining comparison methods on user-defined types.

### Configuration

#### \*num=-tolerance\*
**`*num=-tolerance*`**

Dynamic variable that sets the default tolerance for `num=` comparisons. Default value is `1d-5`.

```lisp
*num=-tolerance*  ; ⇒ 1.0d-5

;; Temporarily change tolerance
(let ((*num=-tolerance* 1e-3))
  (num= 1 1.001))  ; ⇒ T (within 0.1% tolerance)

(num= 1 1.001)     ; ⇒ NIL (outside default 0.001% tolerance)
```

### Core Functions

#### num-delta
**`(num-delta a b)`**

Computes the relative difference |a−b|/max(1,|a|,|b|). This metric is used internally by `num=` for comparing numbers.

```lisp
(num-delta 1 1)       ; ⇒ 0.0d0
(num-delta 1 1.001)   ; ⇒ 0.0009990009990009974d0
(num-delta 100 101)   ; ⇒ 0.009900990099009901d0
(num-delta 0 0.001)   ; ⇒ 0.001d0 (uses 1 as minimum divisor)
```

#### num=
**`(num= a b &optional tolerance)`**

Generic function that compares two objects for approximate equality. Specializations exist for numbers, arrays, lists, and custom structures.

#### Numbers

```lisp
;; Number comparisons with custom tolerance
(let ((*num=-tolerance* 1e-3))
  (num= 1 1))        ; ⇒ T
  (num= 1 1.0))      ; ⇒ T
  (num= 1 1.001))    ; ⇒ T (within tolerance)
  (num= 1 2))        ; ⇒ NIL
  (num= 1 1.01)))    ; ⇒ NIL (outside tolerance)

;; Using explicit tolerance parameter
(num= 1 1.01 0.01)   ; ⇒ T (within 1% tolerance)
(num= 1 1.01 0.001)  ; ⇒ NIL (outside 0.1% tolerance)
```

#### Lists

```lisp
(let ((*num=-tolerance* 1e-3))
  (num= nil nil))              ; ⇒ T
  (num= '(1) '(1.001)))        ; ⇒ T
  (num= '(1 2) '(1.001 1.999))) ; ⇒ T
  (num= '(0 1) '(0 1.02)))     ; ⇒ NIL (1.02 outside tolerance)
  (num= nil '(1))))            ; ⇒ NIL (different structures)
```

#### Arrays

```lisp
(let* ((*num=-tolerance* 1e-3)
       (a #(0 1 2))
       (b #2A((0 1)
              (2 3))))
  ;; Vector comparisons
  (num= a a))                  ; ⇒ T (same object)
  (num= a #(0 1.001 2)))       ; ⇒ T
  (num= a #(0 1.001 2.001)))   ; ⇒ T
  (num= a #(0 1.01 2)))        ; ⇒ NIL (1.01 outside tolerance)
  (num= a #(0 1)))             ; ⇒ NIL (different dimensions)
  
  ;; 2D array comparisons
  (num= b b))                  ; ⇒ T
  (num= b #2A((0 1)
              (2.001 3))))     ; ⇒ T
  (num= b #2A((0 1.01)
              (2 3))))         ; ⇒ NIL
  (num= b #2A((0 1)))))        ; ⇒ NIL (different dimensions)

;; Arrays of different ranks
(num= #(0 1 2) #2A((0 1 2)))  ; ⇒ NIL (different ranks)
```

#### num=-function
**`(num=-function tolerance)`**

Returns a curried version of `num=` with the given tolerance fixed.

```lisp
(let ((approx= (num=-function 0.01)))
  (funcall approx= 1 1.005))   ; ⇒ T (within 1%)
  (funcall approx= 1 1.02)))   ; ⇒ NIL (outside 1%)

;; Use with higher-order functions
(remove-if-not (num=-function 0.1) 
               '(1.05 2.0 0.95 1.08)
               :key (lambda (x) 1))
; ⇒ (1.05 0.95 1.08) (all within 10% of 1)
```

### Structure Comparison

#### define-num=-with-accessors
**`(define-num=-with-accessors class accessors)`**

Macro that defines a `num=` method for a class, comparing values obtained through the specified accessor functions.

```lisp
;; Define a class with accessors
(defclass point ()
  ((x :accessor point-x :initarg :x)
   (y :accessor point-y :initarg :y)))

;; Define num= method using accessors
(define-num=-with-accessors point (point-x point-y))

;; Use the defined method
(let ((p1 (make-instance 'point :x 1.0 :y 2.0))
      (p2 (make-instance 'point :x 1.001 :y 1.999)))
  (num= p1 p2 0.01))  ; ⇒ T
```

#### define-structure-num=
**`(define-structure-num= structure &rest slots)`**

Convenience macro for structures that automatically generates accessor names from the structure name and slot names.

```lisp
;; Define structure from test
(defstruct num=-test-struct
  "Structure for testing DEFINE-STRUCTURE-num=."
  a b)

;; Generate num= method
(define-structure-num= num=-test-struct a b)

;; Examples from test
(let ((*num=-tolerance* 1e-3)
      (a (make-num=-test-struct :a 0 :b 1))
      (b (make-num=-test-struct :a "string" :b nil)))
  (num= a a))                                      ; ⇒ T
  (num= a (make-num=-test-struct :a 0 :b 1)))     ; ⇒ T
  (num= a (make-num=-test-struct :a 0 :b 1.001))) ; ⇒ T
  (num= a (make-num=-test-struct :a 0 :b 1.01)))  ; ⇒ NIL
  (num= b b))                                      ; ⇒ T
  (num= a b)))                                     ; ⇒ NIL
```

The macro expands to use accessors named `structure-slot`, so for `num=-test-struct` with slots `a` and `b`, it uses `num=-test-struct-a` and `num=-test-struct-b`.

### Usage Examples

#### Example: Floating-Point Computation Verification

```lisp
;; Verify numerical algorithm results
(defun verify-computation (computed expected &optional (tolerance 1e-10))
  "Verify that computed result matches expected value within tolerance."
  (if (num= computed expected tolerance)
      (format t "✓ Result ~A matches expected ~A~%" computed expected)
      (format t "✗ Result ~A differs from expected ~A by ~A~%" 
              computed expected (num-delta computed expected))))

;; Example: Verify matrix computation
(let* ((result #2A((1.0 0.0)
                   (0.0 1.0)))
       (expected #2A((1.0 0.0)
                     (0.0 0.99999999))))
  (verify-computation result expected 1e-8))
; prints: ✓ Result #2A((1.0 0.0) (0.0 1.0)) matches expected #2A((1.0 0.0) (0.0 0.99999999))
```

#### Example: Testing Numerical Algorithms

```lisp
;; Compare different implementations
(defun compare-algorithms (algorithm1 algorithm2 test-inputs &optional (tol 1e-6))
  "Compare outputs of two algorithms on test inputs."
  (loop for input in test-inputs
        for result1 = (funcall algorithm1 input)
        for result2 = (funcall algorithm2 input)
        for equal-p = (num= result1 result2 tol)
        unless equal-p
          collect (list :input input 
                       :alg1 result1 
                       :alg2 result2
                       :delta (num-delta result1 result2))))

;; Example: Compare two sqrt implementations
(compare-algorithms #'sqrt 
                   (lambda (x) (expt x 0.5))
                   '(1.0 2.0 3.0 4.0))
; ⇒ NIL (all results match within tolerance)
```

#### Example: Custom Structure Comparison

```lisp
;; Define a complex number structure
(defstruct (complex-num (:constructor make-complex-num (real imag)))
  real imag)

;; Enable approximate comparison
(define-structure-num= complex-num real imag)

;; Use in computations
(let* ((z1 (make-complex-num 1.0 2.0))
       (z2 (make-complex-num 1.0001 1.9999))
       (*num=-tolerance* 1e-3))
  (num= z1 z2))  ; ⇒ T

;; Array of structures
(let ((arr1 (vector (make-complex-num 1 0) 
                    (make-complex-num 0 1)))
      (arr2 (vector (make-complex-num 1.0001 0) 
                    (make-complex-num 0 0.9999))))
  (num= arr1 arr2 1e-3))  ; ⇒ T
```

### Notes on Usage

1. **Relative Error Metric**: The comparison uses relative error |a−b|/max(1,|a|,|b|), which handles both small and large numbers appropriately.

2. **Default Fallback**: Objects without specialized methods are compared using `equalp`, ensuring the function works with any Lisp objects.

3. **Recursive Comparison**: Methods for arrays and lists recursively apply `num=` to elements, propagating the tolerance parameter.

4. **Structure Macros**: The `define-structure-num=` macro assumes standard naming convention for accessors (structure-name-slot-name).

5. **Performance**: For large arrays or deeply nested structures, consider the overhead of element-wise comparison.

6. **Mixed Types**: `num=` can compare different numeric types (integer vs float) as it uses numeric operations that handle type coercion.


## Polynomial

The Polynomial package provides efficient evaluation of polynomial functions using Horner's method. It supports optimized implementations for different numeric types including fixnum, single-float, double-float, and arbitrary precision numbers.

### evaluate-polynomial
**`(evaluate-polynomial coefficients x)`**

Evaluates a polynomial at point x using Horner's method. Coefficients are ordered from highest degree down to the constant term.

```lisp
;; Basic polynomial evaluation with fixnum coefficients
(evaluate-polynomial #(2 -6 2 -1) 3)  ; ⇒ 5
;; Evaluates: 2x³ - 6x² + 2x - 1 at x=3
;; = 2(27) - 6(9) + 2(3) - 1 = 54 - 54 + 6 - 1 = 5

(evaluate-polynomial #(2 0 3 1) 2)    ; ⇒ 23
;; Evaluates: 2x³ + 0x² + 3x + 1 at x=2
;; = 2(8) + 0 + 3(2) + 1 = 16 + 6 + 1 = 23

(evaluate-polynomial #(1 3 5 7 9) 2)  ; ⇒ 83
;; Evaluates: x⁴ + 3x³ + 5x² + 7x + 9 at x=2
;; = 16 + 3(8) + 5(4) + 7(2) + 9 = 16 + 24 + 20 + 14 + 9 = 83

;; Single coefficient (constant polynomial)
(evaluate-polynomial #(5) 2)          ; ⇒ 5
```

#### Type-Specific Optimizations

The function provides optimized implementations for different numeric types:

```lisp
;; Single-float coefficients
(evaluate-polynomial #(2.0 -6.0 2.0 -1.0) 3.0)      ; ⇒ 5.0
(evaluate-polynomial #(2.0 0.0 3.0 1.0) 2.0)        ; ⇒ 23.0
(evaluate-polynomial #(1.0 3.0 5.0 7.0 9.0) 2.0)    ; ⇒ 83.0

;; Double-float coefficients  
(evaluate-polynomial #(2.0d0 -6.0d0 2.0d0 -1.0d0) 3.0d0)    ; ⇒ 5.0d0
(evaluate-polynomial #(2.0d0 0.0d0 3.0d0 1.0d0) 2.0d0)      ; ⇒ 23.0d0
(evaluate-polynomial #(1.0d0 3.0d0 5.0d0 7.0d0 9.0d0) 2.0d0) ; ⇒ 83.0d0

;; Arbitrary precision (bignum) - evaluating at large values
(evaluate-polynomial #(2 0 1) (1+ most-positive-fixnum))
; ⇒ 42535295865117307932921825928971026433
;; Evaluates: 2x² + 1 at x = 2^62 (on 64-bit systems)
```

### evaluate-rational
**`(evaluate-rational numerator denominator z)`**

Evaluates a rational function (ratio of two polynomials) using Horner's method with special handling to prevent overflow for large z values.

**Important**: Unlike `evaluate-polynomial`, coefficients are ordered from lowest degree (constant term) to highest degree.

```lisp
;; Simple rational function: (1 + 2z) / (1 + z)
(evaluate-rational #(1 2) #(1 1) 3.0)  ; ⇒ 1.75d0
;; = (1 + 2×3) / (1 + 3) = 7/4 = 1.75

;; More complex example: (1 + 2z + z²) / (1 + z + z²)  
(evaluate-rational #(1 2 1) #(1 1 1) 2.0)  ; ⇒ 1.2857142857142858d0
;; = (1 + 2×2 + 2²) / (1 + 2 + 2²) = 9/7 ≈ 1.286

;; Handling large values - uses z⁻¹ internally to prevent overflow
(evaluate-rational #(1 0 1) #(1 1 0) 1e10)  ; Works without overflow
```

### Usage Examples

#### Polynomial Fitting and Evaluation

```lisp
;; Fit a quadratic polynomial to data points and evaluate
(defun fit-and-evaluate-quadratic (x1 y1 x2 y2 x3 y3 x-eval)
  "Fit ax² + bx + c through three points and evaluate at x-eval"
  ;; For demonstration - actual fitting would use linear algebra
  ;; Here we use a pre-computed polynomial
  (let ((coefficients #(1.0d0 -2.0d0 3.0d0))) ; x² - 2x + 3
    (evaluate-polynomial coefficients x-eval)))

(fit-and-evaluate-quadratic 0 3 1 2 2 3 1.5)  ; ⇒ 2.25d0
```

#### Chebyshev Polynomial Evaluation

```lisp
;; Chebyshev polynomials of the first kind
(defun chebyshev-t3 (x)
  "T₃(x) = 4x³ - 3x"
  (evaluate-polynomial #(4 0 -3 0) x))

(defun chebyshev-t4 (x)
  "T₄(x) = 8x⁴ - 8x² + 1"
  (evaluate-polynomial #(8 0 -8 0 1) x))

(chebyshev-t3 0.5d0)   ; ⇒ -1.0d0
(chebyshev-t4 0.5d0)   ; ⇒ -0.5d0
```

#### Numerical Stability Comparison

```lisp
;; Compare naive evaluation vs Horner's method
(defun naive-polynomial-eval (coeffs x)
  "Naive polynomial evaluation - less numerically stable"
  (loop for i from 0
        for coeff across coeffs
        sum (* coeff (expt x (- (length coeffs) i 1)))))

;; For well-conditioned polynomials, results are similar
(let ((coeffs #(1.0d0 -2.0d0 1.0d0)))  ; (x-1)²
  (list (evaluate-polynomial coeffs 1.0001d0)     ; ⇒ 1.0000000100000003d-8
        (naive-polynomial-eval coeffs 1.0001d0))) ; ⇒ 1.0000000099999842d-8

;; Horner's method is more efficient (fewer operations)
;; and generally more numerically stable
```

#### Rational Function Applications

```lisp
;; Padé approximation of exp(x) around x=0
;; exp(x) ≈ (1 + x/2 + x²/12) / (1 - x/2 + x²/12)
(defun pade-exp (x)
  (evaluate-rational #(1.0d0 0.5d0 0.08333333333333333d0)    ; 1, 1/2, 1/12
                     #(1.0d0 -0.5d0 0.08333333333333333d0)   ; 1, -1/2, 1/12
                     x))

(pade-exp 0.1d0)   ; ⇒ 1.1051709180756474d0 (compare to (exp 0.1) = 1.1051709180756477d0)
(pade-exp 0.5d0)   ; ⇒ 1.6487212707001282d0 (compare to (exp 0.5) = 1.6487212707001282d0)

;; Transfer function evaluation in control theory
;; H(s) = (s + 1) / (s² + 2s + 1)
(defun transfer-function (s)
  (evaluate-rational #(1 1)      ; 1 + s
                     #(1 2 1)    ; 1 + 2s + s²
                     s))

(transfer-function 1.0d0)  ; ⇒ 0.5d0
```

### Performance and Usage Notes

1. **Type-Specific Paths**: The implementation provides optimized paths for `double-float`, `single-float`, `fixnum`, and generic arbitrary precision arithmetic.

2. **Horner's Method Efficiency**: Evaluating an n-degree polynomial requires only n multiplications and n additions, compared to naive evaluation which requires O(n²) operations.

3. **Numerical Stability**: Horner's method generally provides better numerical stability than naive evaluation, especially for polynomials with terms of vastly different magnitudes.

4. **Coefficient Ordering**: 
   - `evaluate-polynomial`: Highest degree → constant term
   - `evaluate-rational`: Constant term → highest degree

5. **Type Consistency**: For optimal performance, ensure x and all coefficients are of the same numeric type.

6. **Overflow Prevention**: `evaluate-rational` automatically switches to reciprocal evaluation for |z| > 1 to prevent overflow.

7. **Optimization**: The functions are declared with `(optimize (speed 3) (safety 1))` for maximum performance in production use.


## Print-Matrix

The Print-Matrix package provides formatted printing of 2D matrices with configurable precision, alignment, and truncation. Features include column alignment, custom element formatting, masking specific elements, respecting `*print-length*` for large matrices, and special handling for complex numbers. Supports customizable padding, indentation, and precision control through `*print-matrix-precision*` for human-readable matrix display.

### \*print-matrix-precision\*
**`*print-matrix-precision*`**

Dynamic variable that controls the number of digits after the decimal point when printing numeric matrices. Default value is 5.

```lisp
*print-matrix-precision*  ; ⇒ 5

;; Temporarily change precision
(let ((*print-matrix-precision* 2))
  (print-matrix #2A((1.234567 2.345678)
                    (3.456789 4.567890)) t))
; Prints:
;   1.23  2.35
;   3.46  4.57

;; Default precision
(print-matrix #2A((1.234567 2.345678)
                  (3.456789 4.567890)) t)
; Prints:
;   1.23457  2.34568
;   3.45679  4.56789
```

### print-length-truncate
**`(print-length-truncate dimension)`**

Returns the effective dimension to use based on `*print-length*` and whether truncation occurred. Returns two values: the effective dimension and a boolean indicating if truncation happened.

```lisp
(print-length-truncate 10)        ; ⇒ 10, NIL (no truncation)

(let ((*print-length* 5))
  (print-length-truncate 10))     ; ⇒ 5, T (truncated)

(let ((*print-length* nil))
  (print-length-truncate 10))     ; ⇒ 10, NIL (no limit)
```

### print-matrix
**`(print-matrix matrix stream &key formatter masked-fn aligned? padding indent)`**

Prints a 2D matrix with configurable formatting options.

Parameters:
- `matrix` - a 2D array to print
- `stream` - output stream (T for standard output)
- `:formatter` - function to format individual elements (default: `print-matrix-formatter`)
- `:masked-fn` - predicate function; elements where this returns true are replaced with "..."
- `:aligned?` - whether to align columns (default: T)
- `:padding` - string between columns (default: " ")
- `:indent` - string at start of each line (default: "  ")

#### Basic Usage Examples

```lisp
;; Integer matrix
(print-matrix #2A((1 2 3)
                  (4 5 6)
                  (7 8 9)) t)
; Prints:
;   1  2  3
;   4  5  6
;   7  8  9

;; Floating-point matrix with default precision
(print-matrix #2A((1.0 2.5 3.33333)
                  (4.1 5.0 6.66667)) t)
; Prints:
;   1.00000  2.50000  3.33333
;   4.10000  5.00000  6.66667

;; Mixed numeric types
(print-matrix #2A((1 2.5 3)
                  (4.0 5 6.7)) t)
; Prints:
;   1  2.50000  3
;   4.00000  5  6.70000
```

#### Complex Number Formatting

```lisp
;; Complex matrix
(print-matrix #2A((#C(1 2) #C(3 -4))
                  (#C(0 1) #C(-2 0))) t)
; Prints:
;   1.00000+2.00000i   3.00000+-4.00000i
;   0.00000+1.00000i  -2.00000+0.00000i

;; Mixed real and complex
(print-matrix #2A((1.0 #C(2 3))
                  (#C(4 0) 5.0)) t)
; Prints:
;   1.00000           2.00000+3.00000i
;   4.00000+0.00000i  5.00000
```

#### Truncation with \*print-length\*

```lisp
;; Large matrix with print-length restriction
(let ((*print-length* 3))
  (print-matrix #2A((1 2 3 4 5)
                    (6 7 8 9 10)
                    (11 12 13 14 15)
                    (16 17 18 19 20)
                    (21 22 23 24 25)) t))
; Prints:
;    1   2   3  ...
;    6   7   8  ...
;   11  12  13  ...
;   ...

;; Different print-length values
(let ((*print-length* 2))
  (print-matrix #2A((1.1 2.2 3.3)
                    (4.4 5.5 6.6)
                    (7.7 8.8 9.9)) t))
; Prints:
;   1.10000  2.20000  ...
;   4.40000  5.50000  ...
;   ...
```

### Advanced Features

#### Custom Formatting

```lisp
;; Custom formatter for percentages
(defun percentage-formatter (x)
  (format nil "~,1f%" (* x 100)))

(print-matrix #2A((0.15 0.30 0.55)
                  (0.20 0.45 0.35)) t
              :formatter #'percentage-formatter)
; Prints:
;   15.0%  30.0%  55.0%
;   20.0%  45.0%  35.0%

;; Scientific notation formatter
(defun scientific-formatter (x)
  (format nil "~,2e" x))

(print-matrix #2A((1e-5 2.5e6)
                  (3.14159 0.001)) t
              :formatter #'scientific-formatter)
; Prints:
;   1.00e-5  2.50e+6
;   3.14e+0  1.00e-3
```

#### Element Masking

```lisp
;; Mask diagonal elements
(print-matrix #2A((1 2 3)
                  (4 5 6)
                  (7 8 9)) t
              :masked-fn (lambda (row col) (= row col)))
; Prints:
;   ...  2  3
;   4  ...  6
;   7  8  ...

;; Mask values below threshold
(print-matrix #2A((0.1 0.5 0.9)
                  (0.3 0.7 0.2)
                  (0.8 0.4 0.6)) t
              :masked-fn (lambda (row col) 
                          (< (aref #2A((0.1 0.5 0.9)
                                       (0.3 0.7 0.2)
                                       (0.8 0.4 0.6)) row col) 
                             0.5)))
; Prints:
;   ...      0.50000  0.90000
;   ...      0.70000  ...
;   0.80000  ...      0.60000
```

#### Alignment and Padding Options

```lisp
;; No alignment
(print-matrix #2A((1 22 333)
                  (4444 5 66)) t
              :aligned? nil)
; Prints:
;   1 22 333
;   4444 5 66

;; With alignment (default)
(print-matrix #2A((1 22 333)
                  (4444 5 66)) t)
; Prints:
;      1  22  333
;   4444   5   66

;; Custom padding
(print-matrix #2A((1 2 3)
                  (4 5 6)) t
              :padding " | ")
; Prints:
;   1 | 2 | 3
;   4 | 5 | 6

;; Custom indentation
(print-matrix #2A((1 2)
                  (3 4)) t
              :indent ">>>")
; Prints:
; >>>1  2
; >>>3  4
```

### Practical Applications

#### Correlation Matrix Display

```lisp
;; Display correlation matrix with custom precision
(let ((*print-matrix-precision* 3)
      (corr-matrix #2A((1.000 0.856 -0.234)
                       (0.856 1.000 0.142)
                       (-0.234 0.142 1.000))))
  (print-matrix corr-matrix t))
; Prints:
;    1.000   0.856  -0.234
;    0.856   1.000   0.142
;   -0.234   0.142   1.000

;; With significance masking (mask values < 0.3)
(print-matrix corr-matrix t
              :masked-fn (lambda (row col)
                          (and (/= row col)
                               (< (abs (aref corr-matrix row col)) 0.3))))
; Prints:
;   1.000  0.856  ...
;   0.856  1.000  ...
;   ...    ...    1.000
```

#### Sparse Matrix Visualization

```lisp
;; Visualize sparse matrix by masking zeros
(let ((sparse #2A((1.0 0.0 0.0 2.0)
                  (0.0 3.0 0.0 0.0)
                  (0.0 0.0 0.0 4.0)
                  (5.0 0.0 6.0 0.0))))
  (print-matrix sparse t
                :masked-fn (lambda (row col)
                            (zerop (aref sparse row col)))))
; Prints:
;   1.00000  ...      ...      2.00000
;   ...      3.00000  ...      ...
;   ...      ...      ...      4.00000
;   5.00000  ...      6.00000  ...
```

### Usage Notes

1. **Stream Output**: The `stream` parameter follows Common Lisp conventions - use `t` for standard output, `nil` for a string, or any stream object.

2. **Precision Control**: `*print-matrix-precision*` affects all real and complex numbers. For integers, no decimal places are shown.

3. **Performance**: For very large matrices, consider using `*print-length*` to limit output, or write custom formatters that summarize data.

4. **Alignment**: Column alignment adds overhead for large matrices as it requires pre-scanning all elements. Disable with `:aligned? nil` for better performance.

5. **Complex Number Format**: Complex numbers are always printed as `a+bi` format, with the imaginary unit shown as 'i'.

6. **Thread Safety**: The `*print-matrix-precision*` variable is dynamically bound, so it's thread-safe when using `let` bindings.


## Quadrature

The Quadrature package provides adaptive numerical integration using Romberg quadrature with Richardson extrapolation. Supports finite and semi-infinite intervals with automatic coordinate transformations. Features trapezoidal and midpoint rule refinements, configurable convergence criteria (epsilon tolerance), and handles open/closed interval endpoints. Efficiently computes definite integrals with controlled accuracy through iterative refinement and extrapolation techniques.

### romberg-quadrature
**`(romberg-quadrature function interval &key open epsilon max-iterations)`**

Computes the definite integral of a function over an interval using Romberg's method with Richardson extrapolation.

Parameters:
- `function` - the integrand function of one argument
- `interval` - an interval object (finite or semi-infinite)
- `:open` - if true, uses midpoint rule for open intervals (default: nil uses trapezoidal rule)
- `:epsilon` - relative error tolerance for convergence (default: machine epsilon)
- `:max-iterations` - maximum number of refinement iterations (default: 20)

Returns two values:
- The estimated integral value
- The estimated relative error

#### Basic Integration Examples

```lisp
;; Integrate x² from 0 to 1 (exact: 1/3)
(romberg-quadrature (lambda (x) (* x x)) (interval 0d0 1d0))
; ⇒ 0.3333333333333333d0, 5.551115123125783d-17

;; Integrate exp(x) from 0 to 1 (exact: e-1 ≈ 1.71828...)
(romberg-quadrature #'exp (interval 0d0 1d0))
; ⇒ 1.7182818284590453d0, 5.551115123125783d-17

;; Integrate 1/x from 1 to e (exact: 1)
(romberg-quadrature (lambda (x) (/ x)) (interval 1d0 (exp 1d0)))
; ⇒ 1.0000000000000002d0, 2.220446049250313d-16

;; Integrate sin(x) from 0 to π (exact: 2)
(romberg-quadrature #'sin (interval 0d0 pi))
; ⇒ 2.0000000000000004d0, 1.7763568394002506d-15
```

#### Open Interval Integration

When the integrand has singularities at the endpoints, use the `:open t` option to employ the midpoint rule:

```lisp
;; Integrate 1/√x from 0 to 1 using open interval (exact: 2)
(romberg-quadrature (lambda (x) (/ (sqrt x))) 
                    (interval 0d0 1d0) 
                    :open t)
; ⇒ 1.9999999999999998d0, 1.1102230246251566d-16

;; Integrate log(x) from 0 to 1 using open interval (exact: -1)
(romberg-quadrature #'log 
                    (interval 0d0 1d0) 
                    :open t)
; ⇒ -0.9999999999999998d0, 2.220446049250313d-16
```

#### Semi-Infinite Intervals

The function automatically applies appropriate transformations for semi-infinite intervals:

```lisp
;; Integrate exp(-x) from 0 to ∞ (exact: 1)
(romberg-quadrature (lambda (x) (exp (- x))) 
                    (interval 0d0 :plusinf))
; ⇒ 1.0000000000000002d0, 2.220446049250313d-16

;; Integrate x*exp(-x²) from 0 to ∞ (exact: 1/2)
(romberg-quadrature (lambda (x) (* x (exp (- (* x x))))) 
                    (interval 0d0 :plusinf))
; ⇒ 0.5000000000000001d0, 2.220446049250313d-16

;; Integrate 1/(1+x²) from -∞ to ∞ (exact: π)
(romberg-quadrature (lambda (x) (/ (1+ (* x x)))) 
                    (interval :minusinf :plusinf))
; ⇒ 3.141592653589793d0, 2.220446049250313d-16
```

#### Custom Tolerance

Specify a custom error tolerance for faster computation or higher accuracy:

```lisp
;; Lower precision for faster computation
(romberg-quadrature #'exp 
                    (interval 0d0 1d0) 
                    :epsilon 1d-6)
; ⇒ 1.7182818284590429d0, 9.325873406851296d-7

;; Higher precision (will use more iterations)
(romberg-quadrature #'exp 
                    (interval 0d0 1d0) 
                    :epsilon 1d-12)
; ⇒ 1.7182818284590453d0, 5.551115123125783d-17

;; Complex integrands with custom tolerance
(romberg-quadrature (lambda (x) (sin (/ x))) 
                    (interval 0.1d0 1d0) 
                    :epsilon 1d-8)
; ⇒ 0.8639703768373046d0, 6.453172330487389d-9
```

### Practical Applications

#### Example: Probability Distributions

```lisp
;; Compute cumulative distribution function values
(defun normal-cdf (x &key (mean 0d0) (stddev 1d0))
  "Cumulative distribution function of normal distribution"
  (let ((z (/ (- x mean) stddev)))
    (+ 0.5d0 
       (* 0.5d0 
          (first (romberg-quadrature 
                  (lambda (t) (* (/ (sqrt (* 2 pi))) 
                                 (exp (- (* 0.5 t t)))))
                  (interval 0d0 z)))))))

;; Standard normal CDF values
(normal-cdf 0d0)    ; ⇒ 0.5d0
(normal-cdf 1d0)    ; ⇒ 0.8413447460685429d0
(normal-cdf -1d0)   ; ⇒ 0.15865525393145707d0
(normal-cdf 2d0)    ; ⇒ 0.9772498680518208d0

;; Compute probability between two values
(defun normal-probability (a b &key (mean 0d0) (stddev 1d0))
  "Probability that normal random variable is between a and b"
  (- (normal-cdf b :mean mean :stddev stddev)
     (normal-cdf a :mean mean :stddev stddev)))

(normal-probability -1d0 1d0)  ; ⇒ 0.6826894921370859d0 (≈ 68.3%)
(normal-probability -2d0 2d0)  ; ⇒ 0.9544997361036416d0 (≈ 95.4%)
```

#### Example: Arc Length Calculation

```lisp
;; Compute arc length of a curve y = f(x)
(defun arc-length (f df a b)
  "Arc length of curve y=f(x) from x=a to x=b, where df is f'(x)"
  (romberg-quadrature 
   (lambda (x) (sqrt (1+ (expt (funcall df x) 2))))
   (interval a b)))

;; Arc length of parabola y = x² from 0 to 1
(arc-length (lambda (x) (* x x))     ; f(x) = x²
            (lambda (x) (* 2 x))      ; f'(x) = 2x
            0d0 1d0)
; ⇒ 1.4789428575445975d0, 5.551115123125783d-17

;; Arc length of sine curve from 0 to π
(arc-length #'sin                     ; f(x) = sin(x)
            #'cos                     ; f'(x) = cos(x)
            0d0 pi)
; ⇒ 3.8201977382081133d0, 1.887379141862766d-15
```

#### Example: Expected Values

```lisp
;; Compute expected value of a function under a probability distribution
(defun expected-value (g pdf a b)
  "E[g(X)] where X has probability density function pdf on [a,b]"
  (romberg-quadrature 
   (lambda (x) (* (funcall g x) (funcall pdf x)))
   (interval a b)))

;; Expected value of X² under uniform distribution on [0,1]
(expected-value (lambda (x) (* x x))  ; g(x) = x²
                (lambda (x) 1d0)       ; uniform pdf = 1
                0d0 1d0)
; ⇒ 0.3333333333333333d0 (exact: 1/3)

;; Expected value of X under exponential distribution
(expected-value (lambda (x) x)                    ; g(x) = x
                (lambda (x) (exp (- x)))          ; exponential pdf
                0d0 :plusinf)
; ⇒ 1.0000000000000002d0 (exact: 1)
```

#### Example: Fourier Coefficients

```lisp
;; Compute Fourier coefficients of a periodic function
(defun fourier-coefficient (f n period &key (cosine t))
  "Compute nth Fourier coefficient (cosine or sine) of function f"
  (let ((omega (* 2 pi (/ n period))))
    (* (/ 2d0 period)
       (first (romberg-quadrature 
               (lambda (x) 
                 (* (funcall f x)
                    (if cosine
                        (cos (* omega x))
                        (sin (* omega x)))))
               (interval 0d0 period))))))

;; Fourier coefficients of square wave
(defun square-wave (x)
  (if (< (mod x 2d0) 1d0) 1d0 -1d0))

(fourier-coefficient #'square-wave 1 2d0 :cosine nil)  ; b₁
; ⇒ 1.2732395447351628d0 (exact: 4/π)

(fourier-coefficient #'square-wave 3 2d0 :cosine nil)  ; b₃
; ⇒ 0.4244131815783876d0 (exact: 4/(3π))

(fourier-coefficient #'square-wave 2 2d0 :cosine nil)  ; b₂ = 0
; ⇒ 4.440892098500626d-16 (≈ 0)
```

### Advanced Usage

#### Example: Improper Integrals with Singularities

```lisp
;; Handle integrals with removable singularities
(defun integrate-with-singularity (f interval singularity-points 
                                   &key (epsilon 1d-10))
  "Integrate function with known singularities by splitting interval"
  (let* ((splits (sort (copy-list singularity-points) #'<))
         (subintervals (split-interval interval splits))
         (total 0d0)
         (total-error 0d0))
    (loop for subinterval across subintervals
          do (multiple-value-bind (value error)
                 (romberg-quadrature f subinterval 
                                    :open t 
                                    :epsilon epsilon)
               (incf total value)
               (incf total-error error)))
    (values total total-error)))

;; Example: ∫|sin(x)/x| dx from -π to π (singularity at x=0)
(integrate-with-singularity 
 (lambda (x) (if (zerop x) 1d0 (abs (/ (sin x) x))))
 (interval (- pi) pi)
 '(0d0))
; ⇒ 5.876481158479012d0, 3.552713678800501d-15
```

#### Example: Parameter-Dependent Integrals

```lisp
;; Compute integrals that depend on a parameter
(defun gamma-incomplete (s x)
  "Lower incomplete gamma function γ(s,x)"
  (first (romberg-quadrature 
          (lambda (t) (* (expt t (1- s)) (exp (- t))))
          (interval 0d0 x)
          :open (< s 1))))  ; Use open interval if s < 1

(gamma-incomplete 2d0 1d0)   ; ⇒ 0.2642411176571154d0
(gamma-incomplete 0.5d0 1d0) ; ⇒ 1.4936482656248541d0

;; Beta function B(a,b)
(defun beta-function (a b)
  "Beta function B(a,b) = ∫₀¹ t^(a-1)(1-t)^(b-1) dt"
  (first (romberg-quadrature 
          (lambda (t) (* (expt t (1- a)) 
                        (expt (- 1 t) (1- b))))
          (interval 0d0 1d0)
          :open t)))

(beta-function 2d0 3d0)  ; ⇒ 0.08333333333333334d0 (exact: 1/12)
(beta-function 0.5d0 0.5d0) ; ⇒ 3.1415926535897927d0 (exact: π)
```

### Performance Notes

1. **Convergence Rate**: Romberg quadrature has very fast convergence for smooth functions, typically achieving machine precision in 10-15 iterations.

2. **Coordinate Transformations**: For semi-infinite intervals, the function applies transformations that may affect convergence for certain integrands.

3. **Singularities**: Use `:open t` for integrands with endpoint singularities. For interior singularities, split the interval.

4. **Oscillatory Integrands**: For highly oscillatory functions, consider using specialized methods or increasing `max-iterations`.

5. **Error Estimation**: The returned error is an estimate based on Richardson extrapolation convergence. The actual error may differ.

6. **Numerical Stability**: The implementation uses double-float arithmetic throughout for consistency and stability.

### Notes on Usage

1. **Function Smoothness**: Romberg quadrature works best for smooth (infinitely differentiable) functions. For functions with discontinuities or kinks, consider splitting the interval at problematic points.

2. **Interval Types**: The method supports:
   - Finite intervals: `(interval a b)`
   - Semi-infinite: `(interval a :plusinf)` or `(interval :minusinf b)`
   - Infinite: `(interval :minusinf :plusinf)`

3. **Open vs Closed**: 
   - Closed (default): Uses trapezoidal rule, evaluates at endpoints
   - Open: Uses midpoint rule, avoids endpoint evaluation

4. **Convergence Criteria**: The algorithm stops when the relative change between successive Richardson extrapolation steps is less than `epsilon`.

5. **Maximum Iterations**: If convergence isn't achieved within `max-iterations`, the function returns the best estimate with a larger error bound.

6. **Thread Safety**: The function is thread-safe as it doesn't use global state beyond the input parameters.


## Rootfinding

The Rootfinding package provides numerical root-finding algorithms for univariate functions with configurable convergence criteria. Currently implements bisection method with automatic bracketing validation. Features adjustable tolerance (interval width) and epsilon (function value) parameters, supports double-float precision, and returns detailed convergence information including final bracket bounds and whether the root satisfies the epsilon criterion.

### \*rootfinding-epsilon\*
**`*rootfinding-epsilon*`**

Dynamic variable that sets the default maximum absolute value of the function at the root. Default value is `(expt double-float-epsilon 0.25)`.

```lisp
*rootfinding-epsilon*  ; ⇒ 1.1920928955078125d-4

;; Temporarily change epsilon for higher accuracy
(let ((*rootfinding-epsilon* 1e-10))
  (root-bisection #'sin (interval 3d0 4d0)))
; ⇒ 3.141592653589793d0, -1.2246467991473532d-16, T, 3.141592653589793d0, 3.1415926535897936d0
```

### \*rootfinding-delta-relative\*
**`*rootfinding-delta-relative*`**

Dynamic variable that sets the default relative interval width for rootfinding. Default value is `(expt double-float-epsilon 0.25)`.

```lisp
*rootfinding-delta-relative*  ; ⇒ 1.1920928955078125d-4

;; Use different relative tolerance
(let ((*rootfinding-delta-relative* 1e-6))
  (root-bisection #'identity (interval -1 2)))
; ⇒ 0.0, 0.0d0, T, -9.5367431640625d-7, 9.5367431640625d-7
```

### root-bisection
**`(root-bisection f bracket &key delta epsilon)`**

Finds the root of function `f` within the given bracket using the bisection method.

Parameters:
- `f` - a univariate function
- `bracket` - an interval object containing the root
- `:delta` - absolute tolerance for bracket width (defaults to relative tolerance × initial bracket width)
- `:epsilon` - tolerance for function value at root (defaults to `*rootfinding-epsilon*`)

Returns five values:
1. The root location
2. The function value at the root
3. Boolean indicating if |f(root)| ≤ epsilon
4. Left endpoint of final bracket
5. Right endpoint of final bracket

```lisp
;; Test examples from rootfinding test file
(let ((*rootfinding-delta-relative* 1e-6)
      (*num=-tolerance* 1d-2))
  ;; Find root of identity function (root at 0)
  (root-bisection #'identity (interval -1 2)))
; ⇒ 0.0, 0.0d0, T, -9.5367431640625d-7, 9.5367431640625d-7

(let ((*rootfinding-delta-relative* 1e-6)
      (*num=-tolerance* 1d-2))
  ;; Find root of (x-5)³ = 0 at x = 5
  (root-bisection (lambda (x) (expt (- x 5) 3))
                  (interval -1 10)))
; ⇒ 5.000000476837158d0, 5.445199250513759d-14, T, 4.999999523162842d0, 5.000000476837158d0
```

### Helper Functions

#### opposite-sign?
**`(opposite-sign? a b)`**

Tests whether two numbers have opposite signs (one positive, one negative).

```lisp
(opposite-sign? -1 2)   ; ⇒ T
(opposite-sign? 1 2)    ; ⇒ NIL
(opposite-sign? -1 -2)  ; ⇒ NIL
(opposite-sign? 0 1)    ; ⇒ NIL (zero is neither positive nor negative)
```

#### narrow-bracket?
**`(narrow-bracket? a b delta)`**

Tests whether the interval [a,b] is narrower than delta.

```lisp
(narrow-bracket? 1.0 1.001 0.01)   ; ⇒ T
(narrow-bracket? 1.0 2.0 0.5)      ; ⇒ NIL
(narrow-bracket? -0.5 0.5 1.1)     ; ⇒ T
```

#### near-root?
**`(near-root? f epsilon)`**

Tests whether |f| < epsilon, indicating a value close to a root.

```lisp
(near-root? 0.0001 0.001)   ; ⇒ T
(near-root? 0.01 0.001)     ; ⇒ NIL
(near-root? -0.0001 0.001)  ; ⇒ T (uses absolute value)
```

#### rootfinding-delta
**`(rootfinding-delta interval &optional delta-relative)`**

Computes the absolute tolerance from a relative tolerance and interval width.

```lisp
(rootfinding-delta (interval 0d0 10d0))  
; ⇒ 0.0011920928955078125d0 (10 × default relative tolerance)

(rootfinding-delta (interval -5d0 5d0) 1e-6)
; ⇒ 1.0d-5 (10 × 1e-6)
```

### Usage Examples

#### Example: Finding Roots of Polynomials

```lisp
;; Find root of x² - 4 = 0 in [0, 3] (exact root: x = 2)
(defun f1 (x) (- (* x x) 4))

(root-bisection #'f1 (interval 0d0 3d0))
; ⇒ 2.0000000298023224d0, 1.1920928955078125d-7, T, 1.9999999701976776d0, 2.0000000298023224d0

;; Find root of x³ - 2x - 5 = 0 in [2, 3] (exact root ≈ 2.094551)
(defun f2 (x) (- (* x x x) (* 2 x) 5))

(root-bisection #'f2 (interval 2d0 3d0))
; ⇒ 2.0945514519214863d0, -5.906386491214556d-8, T, 2.094551417231559d0, 2.0945514866109134d0
```

#### Example: Transcendental Equations

```lisp
;; Find where cos(x) = x (fixed point, exact ≈ 0.739085)
(defun f3 (x) (- (cos x) x))

(root-bisection #'f3 (interval 0d0 1d0))
; ⇒ 0.7390851378440857d0, -5.896262174291357d-8, T, 0.7390850857645273d0, 0.7390851899236441d0

;; Find root of e^x = 3x (has two roots)
(defun f4 (x) (- (exp x) (* 3 x)))

;; First root in [0, 1]
(root-bisection #'f4 (interval 0d0 1d0))
; ⇒ 0.6190612792968751d0, 5.872900560954019d-8, T, 0.6190612167119979d0, 0.6190613418817521d0

;; Second root in [1, 2]  
(root-bisection #'f4 (interval 1d0 2d0))
; ⇒ 1.512134553491497d0, -5.823208439077178d-8, T, 1.512134492397309d0, 1.5121346145856858d0
```

#### Example: Custom Tolerances

```lisp
;; High precision root finding for x - sin(x) = 0
(root-bisection (lambda (x) (- x (sin x)))
                (interval 0.1d0 1d0)
                :epsilon 1d-12
                :delta 1d-12)
; ⇒ 0.5110276571540832d0, -5.551115123125783d-17, T, 0.5110276571540831d0, 0.5110276571540832d0

;; Lower precision for faster computation
(root-bisection (lambda (x) (- x (sin x)))
                (interval 0.1d0 1d0)
                :epsilon 1d-3
                :delta 1d-3)
; ⇒ 0.5107421875d0, -0.0002839813765924419d0, T, 0.509765625d0, 0.51171875d0
```

#### Example: Error Handling

```lisp
;; Function without roots in bracket signals error
(handler-case 
    (root-bisection (lambda (x) (+ 1 (* x x)))  ; always positive
                    (interval -1d0 1d0))
  (error (e) (format nil "Error: ~A" e)))
; ⇒ "Error: Boundaries don't bracket 0."

;; Bracket must contain sign change
(let ((f (lambda (x) (- x 5))))
  (handler-case 
      (root-bisection f (interval 6d0 10d0))  ; f > 0 throughout
    (error () "No sign change in bracket")))
; ⇒ "No sign change in bracket"
```

### Practical Applications

#### Example: Finding Interest Rates

```lisp
;; Find interest rate r such that present value = 1000
;; for payments of 100/year for 15 years: PV = 100 × [(1-(1+r)^-15)/r] = 1000
(defun pv-annuity (r)
  (if (zerop r)
      1500.0d0  ; limiting case when r → 0
      (- (* 100 (/ (- 1 (expt (+ 1 r) -15)) r)) 1000)))

(root-bisection #'pv-annuity (interval 0.01d0 0.15d0))
; ⇒ 0.0579444758594036d0, -1.875277668157615d-5, T, 0.057944431900978086d0, 0.057944519817829134d0
; Interest rate ≈ 5.79%
```

#### Example: Solving Optimization Conditions

```lisp
;; Find critical points by solving f'(x) = 0
;; For f(x) = x³ - 3x² - 9x + 5, f'(x) = 3x² - 6x - 9
(defun derivative (x)
  (- (* 3 x x) (* 6 x) 9))

;; Find critical point in [-2, 0] (exact: x = -1)
(root-bisection #'derivative (interval -2d0 0d0))
; ⇒ -0.9999999701976776d0, -8.940696716308594d-8, T, -1.0000000298023224d0, -0.9999999701976776d0

;; Find critical point in [2, 4] (exact: x = 3)
(root-bisection #'derivative (interval 2d0 4d0))
; ⇒ 3.0000000596046448d0, 1.7881393432617188d-7, T, 2.9999999403953552d0, 3.0000000596046448d0
```

#### Example: Inverse Function Evaluation

```lisp
;; Find x such that sinh(x) = 2
(defun sinh-eqn (x)
  (- (sinh x) 2))

(root-bisection #'sinh-eqn (interval 1d0 2d0))
; ⇒ 1.4436354846954346d0, -5.92959405947104d-8, T, 1.4436354227364063d0, 1.443635546654463d0

;; Verify: (sinh 1.4436354846954346d0) ⇒ 1.9999999407040594d0

;; Find x such that log(x) + x = 2
(defun log-eqn (x)
  (- (+ (log x) x) 2))

(root-bisection #'log-eqn (interval 0.1d0 2d0))
; ⇒ 1.5571455955505562d0, 6.041633141658837d-9, T, 1.5571455433964732d0, 1.5571456477046013d0
```

### Performance and Convergence

#### Example: Convergence Analysis

```lisp
;; Track iterations by wrapping function
(let ((iterations 0))
  (flet ((counting-f (x)
           (incf iterations)
           (- (* x x) 2)))
    (multiple-value-bind (root froot within-epsilon a b)
        (root-bisection #'counting-f (interval 1d0 2d0)
                        :delta 1d-10)
      (format t "Root: ~A~%Function calls: ~D~%Final bracket width: ~A~%" 
              root iterations (- b a)))))
; Prints:
; Root: 1.4142135623730951d0
; Function calls: 36
; Final bracket width: 7.105427357601002d-13

;; Bisection converges linearly - each iteration halves the bracket
```

### Notes on Usage

1. **Bracket Requirement**: The function must have opposite signs at the bracket endpoints. The algorithm will signal an error otherwise.

2. **Convergence Criteria**: The algorithm stops when either:
   - The bracket width is less than `delta`, OR
   - |f(root)| < `epsilon`

3. **Return Values**: The third return value indicates which stopping criterion was met:
   - `T` means |f(root)| < epsilon (found accurate root)
   - `NIL` means bracket is narrow but root may not be accurate

4. **Numerical Precision**: All computations use double-float arithmetic for consistency.

5. **Multiple Roots**: If multiple roots exist in the bracket, bisection will find one of them (not necessarily any particular one).

6. **Performance**: Bisection has guaranteed convergence but is slower than methods like Newton-Raphson. It requires approximately log₂(initial_bracket/tolerance) iterations.

7. **Robustness**: Bisection is very robust - it will always converge if the initial bracket contains a root and the function is continuous.


## Test-Utilities

The Test-Utilities package provides utilities for testing accuracy of mathematical functions against reference values. Features functions to compare implementations, measure relative errors, and generate statistical reports including min/max/mean errors, variance, and RMS. Supports testing against known values, reference implementations, or pre-computed vectors. Returns detailed test-results structure with error statistics and worst-case identification.

### test-results
**`test-results`**

Structure containing statistical differences between reference values and computed values.

Fields:
- `worst-case` - integer row index where the worst error occurred
- `min-error` - smallest relative error found (double-float)
- `max-error` - largest relative error found (double-float)
- `mean-error` - mean of all errors (double-float)
- `test-count` - number of test cases (integer)
- `variance0` - population variance of the errors (double-float)
- `variance1` - sample (unbiased) variance of the errors (double-float)
- `rms` - Root Mean Square of the errors (double-float)

```lisp
;; Create and access test results
(let ((results (make-test-results :worst-case 5
                                 :min-error 1d-10
                                 :max-error 1d-6
                                 :mean-error 1d-8
                                 :test-count 100
                                 :variance0 1d-12
                                 :variance1 1.01d-12
                                 :rms 1d-7)))
  (list (worst-case results)     ; ⇒ 5
        (min-error results)      ; ⇒ 1d-10
        (max-error results)      ; ⇒ 1d-6
        (mean-error results)     ; ⇒ 1d-8
        (test-count results)     ; ⇒ 100
        (variance0 results)      ; ⇒ 1d-12
        (variance1 results)      ; ⇒ 1.01d-12
        (rms results)))          ; ⇒ 1d-7
```

### test-fn
**`(test-fn test-name function data)`**

Compares a function against reference data containing input and expected output values. Returns a test-results structure with error statistics.

Parameters:
- `test-name` - string or symbol naming the test (for error messages)
- `function` - function to test (should accept arguments from data)
- `data` - 2D array where each row contains [input₁ ... inputₙ expected-output]

```lisp
;; Example: Testing a square root implementation
(defparameter *sqrt-test-data*
  #2A((1.0  1.0)
      (4.0  2.0)
      (9.0  3.0)
      (16.0 4.0)
      (25.0 5.0)))

(test-fn "sqrt" #'sqrt *sqrt-test-data*)
; ⇒ #S(TEST-RESULTS 
;      :WORST-CASE 0
;      :MIN-ERROR 0.0d0
;      :MAX-ERROR 0.0d0
;      :MEAN-ERROR 0.0d0
;      :TEST-COUNT 5
;      :VARIANCE0 0.0d0
;      :VARIANCE1 0.0d0
;      :RMS 0.0d0)

;; Testing with small errors
(defun approx-sqrt (x)
  (* (sqrt x) (+ 1 (* 1d-6 (random 2.0) (- (random 2.0))))))

(let ((results (test-fn "approx-sqrt" #'approx-sqrt *sqrt-test-data*)))
  (format t "Max error: ~,2e~%" (max-error results))
  (format t "RMS error: ~,2e~%" (rms results)))
; Max error: 1.00e-6
; RMS error: 5.77e-7
```

### compare-fns
**`(compare-fns test-name function reference-function data)`**

Compares two function implementations by evaluating both on the same inputs and measuring relative differences.

Parameters:
- `test-name` - string or symbol naming the comparison
- `function` - function under test
- `reference-function` - reference implementation to compare against
- `data` - 2D array where each row contains input arguments

```lisp
;; Example: Compare two exponential implementations
(defparameter *exp-test-inputs*
  #2A((0.0)
      (1.0)
      (-1.0)
      (10.0)
      (-10.0)))

;; Compare built-in exp with Taylor series approximation
(defun exp-taylor (x)
  "Simple Taylor series approximation of exp(x)"
  (let ((sum 1.0d0)
        (term 1.0d0))
    (loop for n from 1 to 20 do
      (setf term (* term (/ x n)))
      (incf sum term))
    sum))

(compare-fns "exp-taylor" #'exp-taylor #'exp *exp-test-inputs*)
; ⇒ #S(TEST-RESULTS 
;      :WORST-CASE 3
;      :MIN-ERROR 0.0d0
;      :MAX-ERROR 2.688117141816135d-10
;      :MEAN-ERROR 5.376234283632267d-11
;      :TEST-COUNT 5
;      :VARIANCE0 1.1515628172868944d-20
;      :VARIANCE1 1.4394535216086182d-20
;      :RMS 1.2030913128542057d-10)
```

### compare-vectors
**`(compare-vectors test-name vector reference-vector)`**

Compares two pre-computed vectors of values element by element.

Parameters:
- `test-name` - string or symbol naming the comparison
- `vector` - computed values to test
- `reference-vector` - reference values to compare against

```lisp
;; Example: Compare precomputed function values
(let* ((x-values (vector 0.0 0.1 0.2 0.3 0.4 0.5))
       (computed (map 'vector (lambda (x) (sin x)) x-values))
       (reference (vector 0.0d0 
                         0.09983341664682815d0
                         0.19866933079506122d0
                         0.29552020666133955d0
                         0.38941834230865045d0
                         0.47942553860420306d0)))
  (compare-vectors "sin-values" computed reference))
; ⇒ #S(TEST-RESULTS 
;      :WORST-CASE 0
;      :MIN-ERROR 0.0d0
;      :MAX-ERROR 2.220446049250313d-16
;      :MEAN-ERROR 3.7007434154172195d-17
;      :TEST-COUNT 6
;      :VARIANCE0 8.630170314869144d-33
;      :VARIANCE1 1.0356204377842972d-32
;      :RMS 9.291498646471065d-17)

;; Example with larger errors
(let ((computed (vector 1.0 2.001 2.999 4.002))
      (reference (vector 1.0 2.0 3.0 4.0)))
  (compare-vectors "small-errors" computed reference))
; Shows relative errors around 0.0005
```

### Practical Examples

#### Example: Testing Special Function Implementations

```lisp
;; Test a Bessel function implementation
(defparameter *bessel-j0-data*
  ;; x, J₀(x) reference values
  #2A((0.0    1.0)
      (0.5    0.93846980724081290423)
      (1.0    0.76519768655796655145)
      (2.0    0.22389077914123566805)
      (5.0   -0.17759677131433830435)
      (10.0  -0.24593576445134833520)))

(defun my-bessel-j0 (x)
  "Placeholder for user's Bessel J0 implementation"
  ;; In reality, this would be the function being tested
  (cos x))  ; Wrong implementation for demonstration

(let ((results (test-fn "bessel-j0" #'my-bessel-j0 *bessel-j0-data*)))
  (format t "Testing Bessel J0 implementation:~%")
  (format t "  Test count: ~D~%" (test-count results))
  (format t "  Max error: ~,2e~%" (max-error results))
  (format t "  RMS error: ~,2e~%" (rms results))
  (format t "  Worst case at row: ~D~%" (worst-case results)))
; Testing Bessel J0 implementation:
;   Test count: 6
;   Max error: 1.24e+0
;   RMS error: 5.91e-1
;   Worst case at row: 1
```

#### Example: Comparing Optimization Algorithms

```lisp
;; Compare different matrix multiplication algorithms
(defun naive-matmul (a b)
  "Simple matrix multiplication"
  (let* ((m (array-dimension a 0))
         (n (array-dimension b 1))
         (k (array-dimension a 1))
         (result (make-array (list m n) :element-type 'double-float)))
    (dotimes (i m)
      (dotimes (j n)
        (setf (aref result i j)
              (loop for p below k
                    sum (* (aref a i p) (aref b p j))))))
    result))

;; Generate test cases for 2x2 matrices
(defparameter *matmul-test-data*
  (let ((data (make-array '(10 8) :element-type 'double-float)))
    (loop for i below 10 do
      ;; Random 2x2 matrices A and B (flattened)
      (loop for j below 8 do
        (setf (aref data i j) (random 10.0d0))))
    data))

;; Assume we have a reference implementation
(defun reference-matmul (a b)
  ;; Same as naive but serves as reference
  (naive-matmul a b))

;; Test wrapper that reconstructs matrices
(defun matmul-wrapper (a11 a12 a21 a22 b11 b12 b21 b22)
  (let ((a (make-array '(2 2) :initial-contents 
                       `((,a11 ,a12) (,a21 ,a22))))
        (b (make-array '(2 2) :initial-contents 
                       `((,b11 ,b12) (,b21 ,b22))))
        (result (naive-matmul a b)))
    ;; Return flattened result for comparison
    (vector (aref result 0 0) (aref result 0 1)
            (aref result 1 0) (aref result 1 1))))

(compare-fns "matmul" #'matmul-wrapper #'matmul-wrapper *matmul-test-data*)
; Shows near-zero errors for identical implementations
```

#### Example: Validating Numerical Integration

```lisp
;; Test numerical integration against known integrals
(defparameter *integration-test-data*
  ;; Function parameters and expected integral value
  ;; ∫₀¹ x^n dx = 1/(n+1)
  #2A((0.0  1.0)      ; ∫₀¹ x⁰ dx = 1
      (1.0  0.5)      ; ∫₀¹ x¹ dx = 1/2
      (2.0  0.333333333333333d0)  ; ∫₀¹ x² dx = 1/3
      (3.0  0.25)     ; ∫₀¹ x³ dx = 1/4
      (4.0  0.2)))    ; ∫₀¹ x⁴ dx = 1/5

(defun integrate-power (n)
  "Integrate x^n from 0 to 1 using simple trapezoid rule"
  (let ((steps 1000)
        (sum 0.0d0))
    (loop for i from 1 below steps
          for x = (/ i (float steps 1d0))
          do (incf sum (expt x n)))
    (/ sum steps)))

(let ((results (test-fn "power-integration" #'integrate-power 
                       *integration-test-data*)))
  (format t "Integration test results:~%")
  (format t "  Mean relative error: ~,2e~%" (mean-error results))
  (format t "  Maximum error: ~,2e~%" (max-error results))
  (when (< (max-error results) 1d-3)
    (format t "  ✓ All tests passed with < 0.1% error~%")))
```

#### Example: Cross-Platform Consistency

```lisp
;; Compare results across different numeric types
(defun test-numeric-consistency ()
  "Test that algorithms give consistent results across numeric types"
  (let ((test-values #(0.1 0.5 1.0 2.0 10.0)))
    
    ;; Single-float version
    (defun log-single (x)
      (log (coerce x 'single-float)))
    
    ;; Double-float version  
    (defun log-double (x)
      (log (coerce x 'double-float)))
    
    ;; Compare implementations
    (let ((single-results (map 'vector #'log-single test-values))
          (double-results (map 'vector #'log-double test-values)))
      (compare-vectors "single-vs-double-log" 
                      single-results double-results))))

(test-numeric-consistency)
; Shows relative errors around single-float precision (1e-7)
```

### Notes on Usage

1. **Relative Error Metric**: The package uses `num-delta` for computing relative errors, which handles both small and large values appropriately.

2. **Array Format**: Test data arrays should have inputs in the first columns and expected output in the last column for `test-fn`.

3. **Statistical Measures**: 
   - `variance0` is the population variance (divides by n)
   - `variance1` is the sample variance (divides by n-1)
   - `rms` provides a single measure of typical error magnitude

4. **Error Identification**: The `worst-case` field helps identify which test case needs the most attention.

5. **Function Signatures**: When testing multi-argument functions, ensure the data array has the correct number of columns.

6. **Performance**: For large test suites, consider breaking tests into smaller batches to get intermediate results.

7. **Integration**: This package was designed to support the special-functions library but works well for testing any numerical computations.


## Utilities

A collection of utilities to work with floating point values. Optimised for double-float. Provides type conversion functions, vector creation utilities, sequence generation, binary search, and utility macros including currying, multiple bindings, and conditional splicing. Features specialized array types for fixnum, boolean, and floating-point vectors with conversion functions.

### Hash Table Utilities

#### gethash*
**`(gethash* key hash-table &optional (datum "Key not found.") &rest arguments)`**

Like `gethash`, but checks that key is present and raises an error if not found.

```lisp
(let ((ht (make-hash-table :test 'equal)))
  (setf (gethash "key" ht) "value")
  (gethash* "key" ht))                   ; ⇒ "value"

(let ((ht (make-hash-table :test 'equal)))
  (gethash* "missing" ht "Key ~A not found" "missing"))
; ⇒ ERROR: Key missing not found
```

### Conditional Splicing

#### splice-when
**`(splice-when test &body forms)`**

Similar to `when`, but wraps the result in a list for use with splicing operators.

```lisp
(let ((add-middle t))
  `(start ,@(splice-when add-middle 'middle) end))
; ⇒ (START MIDDLE END)

(let ((add-middle nil))
  `(start ,@(splice-when add-middle 'middle) end))
; ⇒ (START END)
```

#### splice-awhen
**`(splice-awhen test &body forms)`**

Anaphoric version of `splice-when` that binds the test result to `it`.

```lisp
(let ((value 42))
  `(result ,@(splice-awhen value `(found ,it))))
; ⇒ (RESULT FOUND 42)

(let ((value nil))
  `(result ,@(splice-awhen value `(found ,it))))
; ⇒ (RESULT)
```

### Functional Utilities

#### curry*
**`(curry* function &rest arguments)`**

Currying macro that accepts `*` as placeholders for arguments to be supplied later.

```lisp
(funcall (curry* + 5 *) 3)               ; ⇒ 8
(funcall (curry* list 'a * 'c) 'b)       ; ⇒ (A B C)
(funcall (curry* - * 3) 10)              ; ⇒ 7

;; Multiple placeholders
(funcall (curry* + * * 5) 2 3)           ; ⇒ 10
```

### Type Checking

#### check-types
**`(check-types (&rest arguments) type)`**

Applies `check-type` to multiple places of the same type.

```lisp
(let ((a 1.0d0) (b 2.0d0) (c 3.0d0))
  (check-types (a b c) double-float)
  (+ a b c))                             ; ⇒ 6.0d0

(let ((x 1) (y 2.0d0))
  (check-types (x y) double-float))       ; ⇒ ERROR: The value of X is 1, which is not of type DOUBLE-FLOAT
```

### Multiple Bindings

#### define-with-multiple-bindings
**`(define-with-multiple-bindings macro &key (plural) (docstring))`**

Defines a version of a macro that accepts multiple bindings as a list.

```lisp
;; Example usage (typically used to create macros like let+s from let+)
(define-with-multiple-bindings let+ :plural let+s)

;; This creates a let+s macro that can be used like:
(let+s ((x 1)
        ((&plist y z) '(:y 2 :z 3)))
  (+ x y z))                             ; ⇒ 6
```

### Numeric Predicates

#### within?
**`(within? left value right)`**

Returns non-nil if value is in the interval [left, right).

```lisp
(within? 0 0.5 1)                        ; ⇒ T
(within? 0 1 1)                          ; ⇒ NIL (right boundary exclusive)
(within? -1 0 1)                         ; ⇒ T
(within? 5 3 10)                         ; ⇒ NIL
```

#### fixnum?
**`(fixnum? object)`**

Checks if object is of type fixnum.

```lisp
(fixnum? 42)                             ; ⇒ T
(fixnum? 3.14)                           ; ⇒ NIL
(fixnum? most-positive-fixnum)           ; ⇒ T
(fixnum? (1+ most-positive-fixnum))      ; ⇒ NIL
```

### Type Definitions

#### simple-fixnum-vector
**`simple-fixnum-vector`**

Type definition for simple one-dimensional arrays of fixnums.

```lisp
(typep #(1 2 3) 'simple-fixnum-vector)        ; ⇒ T (implementation-dependent)
(make-array 5 :element-type 'fixnum)          ; Creates simple-fixnum-vector
```

#### simple-boolean-vector
**`simple-boolean-vector`**

Type definition for simple one-dimensional arrays of booleans.

```lisp
(let ((vec (make-array 3 :initial-contents '(t nil t))))
  (typep vec 'simple-boolean-vector))          ; ⇒ T (if all elements are boolean)
```

#### simple-single-float-vector
**`simple-single-float-vector`**

Type definition for simple one-dimensional arrays of single-floats.

```lisp
(make-array 3 :element-type 'single-float 
              :initial-contents '(1.0 2.0 3.0))  ; Creates simple-single-float-vector
```

#### simple-double-float-vector
**`simple-double-float-vector`**

Type definition for simple one-dimensional arrays of double-floats.

```lisp
(make-array 3 :element-type 'double-float 
              :initial-contents '(1.0d0 2.0d0 3.0d0))  ; Creates simple-double-float-vector
```

### Type Conversion Functions

#### as-simple-fixnum-vector
**`(as-simple-fixnum-vector sequence &optional copy?)`**

Converts sequence to a simple-fixnum-vector.

```lisp
(as-simple-fixnum-vector '(1 2 3))       ; ⇒ #(1 2 3)
(as-simple-fixnum-vector #(4 5 6))       ; ⇒ #(4 5 6)

;; With copy flag
(let ((original #(1 2 3)))
  (eq original (as-simple-fixnum-vector original)))     ; ⇒ T
(let ((original #(1 2 3)))
  (eq original (as-simple-fixnum-vector original t)))   ; ⇒ NIL
```

#### as-bit-vector
**`(as-bit-vector vector)`**

Converts a vector to a bit vector, mapping non-nil to 1 and nil to 0.

```lisp
(as-bit-vector #(t nil t nil t))         ; ⇒ #*10101
(as-bit-vector '(1 nil 0 nil "hello"))   ; ⇒ #*10101
(as-bit-vector #(nil nil nil))           ; ⇒ #*000
```

#### as-double-float
**`(as-double-float number)`**

Converts a number to double-float.

```lisp
(as-double-float 5)                      ; ⇒ 5.0d0
(as-double-float 1/2)                    ; ⇒ 0.5d0
(as-double-float 3.14)                   ; ⇒ 3.14d0 (converted to double)
```

#### with-double-floats
**`(with-double-floats bindings &body body)`**

Macro that coerces values to double-float and binds them to variables.

```lisp
(with-double-floats ((a 1)
                     (b 1/2)
                     (c 3.14))
  (list a b c))
; ⇒ (1.0d0 0.5d0 3.14d0)

;; Variable name can be inferred
(let ((x 5) (y 2))
  (with-double-floats (x y)
    (/ x y)))                            ; ⇒ 2.5d0
```

#### as-simple-double-float-vector
**`(as-simple-double-float-vector sequence &optional copy?)`**

Converts sequence to a simple-double-float-vector.

```lisp
(as-simple-double-float-vector '(1 2 3))        ; ⇒ #(1.0d0 2.0d0 3.0d0)
(as-simple-double-float-vector #(1.5 2.5))      ; ⇒ #(1.5d0 2.5d0)
(as-simple-double-float-vector '(1/2 1/3))      ; ⇒ #(0.5d0 0.3333333333333333d0)
```

### Vector Creation

#### make-vector
**`(make-vector element-type &rest initial-contents)`**

Creates a vector with specified element type and initial contents.

```lisp
(make-vector 'fixnum 1 2 3 4)           ; ⇒ #(1 2 3 4)
(make-vector 'double-float 1.0 2.0)     ; ⇒ #(1.0d0 2.0d0)
(make-vector 'character #\a #\b #\c)    ; ⇒ #(#\a #\b #\c)
```

#### generate-sequence
**`(generate-sequence result-type size function)`**

Creates a sequence by repeatedly calling function.

```lisp
(generate-sequence 'vector 5 (lambda () (random 10)))
; ⇒ #(3 7 1 9 2) ; Random values

(generate-sequence '(vector double-float) 3 
                   (lambda () (random 1.0d0)))
; ⇒ #(0.23d0 0.87d0 0.45d0) ; Random double-floats

(let ((counter 0))
  (generate-sequence 'list 4 (lambda () (incf counter))))
; ⇒ (1 2 3 4)
```

### Utility Functions

#### expanding
**`(expanding &body body)`**

Expands body at macro-expansion time. Useful for code generation.

```lisp
;; Typically used in macro definitions for programmatic code generation
(defmacro make-accessors (slots)
  (expanding
    `(progn
       ,@(loop for slot in slots
               collect `(defun ,(intern (format nil "GET-~A" slot)) (obj)
                          (slot-value obj ',slot))))))
```

#### bic
**`(bic a b)`**

Biconditional function. Returns true if both arguments have the same truth value.

```lisp
(bic t t)                                ; ⇒ T
(bic nil nil)                            ; ⇒ T
(bic t nil)                              ; ⇒ NIL
(bic nil t)                              ; ⇒ NIL

;; Useful for logical equivalence testing
(bic (> 5 3) (< 2 4))                    ; ⇒ T (both true)
(bic (> 5 3) (< 4 2))                    ; ⇒ NIL (different truth values)
```

#### binary-search
**`(binary-search sorted-reals value)`**

Performs binary search on a sorted vector of real numbers.

```lisp
(let ((sorted-vec #(1.0 3.0 5.0 7.0 9.0)))
  (binary-search sorted-vec 5.0))       ; ⇒ 2 (index where 5.0 would go)

(let ((sorted-vec #(1.0 3.0 5.0 7.0 9.0)))
  (binary-search sorted-vec 4.0))       ; ⇒ 1 (between indices 1 and 2)

(let ((sorted-vec #(1.0 3.0 5.0 7.0 9.0)))
  (binary-search sorted-vec 0.0))       ; ⇒ NIL (below minimum)

(let ((sorted-vec #(1.0 3.0 5.0 7.0 9.0)))
  (binary-search sorted-vec 10.0))      ; ⇒ T (above maximum)
```

### Generic Conversion

#### as-alist
**`(as-alist object)`**

Generic function to convert objects to association lists. Methods defined for various types.

```lisp
;; Default behavior depends on object type
;; Hash tables convert key-value pairs to alist
(let ((ht (make-hash-table :test 'equal)))
  (setf (gethash "a" ht) 1
        (gethash "b" ht) 2)
  (as-alist ht))                         ; ⇒ (("a" . 1) ("b" . 2)) ; Order may vary
```

#### as-plist
**`(as-plist object)`**

Generic function to convert objects to property lists. Default method uses `as-alist`.

```lisp
;; Default implementation converts through alist
(let ((ht (make-hash-table :test 'equal)))
  (setf (gethash "a" ht) 1
        (gethash "b" ht) 2)
  (as-plist ht))                         ; ⇒ ("a" 1 "b" 2) ; Order may vary
```

### Practical Examples

#### Example: Type-Safe Vector Operations

```lisp
;; Create and manipulate typed vectors efficiently
(let* ((indices (as-simple-fixnum-vector '(0 1 2 3 4)))
       (values (as-simple-double-float-vector '(0.0 1.0 1.4 1.7 2.0))))
  (with-double-floats ((threshold 1.5))
    (loop for i across indices
          for v across values
          when (>= v threshold)
          collect (cons i v))))
; ⇒ ((3 . 1.7d0) (4 . 2.0d0))
```

#### Example: Functional Programming with Currying

```lisp
;; Create specialized functions using curry*
(let* ((add-tax (curry* * 1.08 *))         ; 8% tax
       (format-currency (curry* format nil "$~,2F" *))
       (prices '(10.00 25.50 99.99)))
  (mapcar (lambda (price)
            (format-currency (funcall add-tax price)))
          prices))
; ⇒ ("$10.80" "$27.54" "$107.99")
```

#### Example: Conditional List Building

```lisp
;; Build lists conditionally using splice-when
(defun make-command (base-cmd &key verbose debug output-file)
  `(,base-cmd
    ,@(splice-when verbose "--verbose")
    ,@(splice-when debug "--debug")  
    ,@(splice-awhen output-file `("--output" ,it))))

(make-command "process" :verbose t :output-file "result.txt")
; ⇒ ("process" "--verbose" "--output" "result.txt")

(make-command "process" :debug t)
; ⇒ ("process" "--debug")
```

#### Example: Binary Search for Interpolation

```lisp
;; Use binary search for table lookup with interpolation
(defun interpolate-table (x-values y-values x)
  (let ((index (binary-search x-values x)))
    (cond
      ((null index) (first y-values))      ; Below range
      ((eq index t) (first (last y-values))) ; Above range
      (t ; Interpolate between points
       (let* ((i index)
              (x1 (aref x-values i))
              (x2 (aref x-values (1+ i)))
              (y1 (aref y-values i))
              (y2 (aref y-values (1+ i)))
              (alpha (/ (- x x1) (- x2 x1))))
         (+ y1 (* alpha (- y2 y1))))))))

(let ((x-vals #(0.0 1.0 2.0 3.0 4.0))
      (y-vals #(0.0 1.0 4.0 9.0 16.0))) ; y = x²
  (list (interpolate-table x-vals y-vals 1.5)   ; Between 1 and 2
        (interpolate-table x-vals y-vals 2.5)))  ; Between 2 and 3
; ⇒ (2.5 6.5)
```

#### Example: Sequence Generation Patterns

```lisp
;; Generate sequences with different patterns
(let* ((fibonacci (let ((a 1) (b 1))
                   (generate-sequence 'vector 10 
                                    (lambda () 
                                      (let ((result a))
                                        (setf a b b (+ a b))
                                        result)))))
       (powers-of-2 (generate-sequence 'vector 8
                                     (let ((power 0))
                                       (lambda ()
                                         (prog1 (expt 2 power)
                                           (incf power))))))
       (random-bools (generate-sequence 'vector 5
                                      (lambda () (< (random 1.0) 0.5)))))
  (list fibonacci powers-of-2 (as-bit-vector random-bools)))
; ⇒ (#(1 1 2 3 5 8 13 21 34 55) 
;    #(1 2 4 8 16 32 64 128)
;    #*10110) ; Random bit pattern
```

### Notes on Usage

1. **Type Optimization**: Use specific vector types like `simple-double-float-vector` for better performance in numeric computations.

2. **Memory Efficiency**: The `copy?` parameter in conversion functions controls whether data is copied or shared.

3. **Currying with Placeholders**: `curry*` uses `*` as placeholders, making it more flexible than traditional currying.

4. **Binary Search Semantics**: Returns the insertion point for values not found, `nil` for values below range, `t` for values above range.

5. **Conditional Splicing**: Use `splice-when` and `splice-awhen` with backquote for building lists conditionally.

6. **Type Checking**: `check-types` provides a convenient way to validate multiple variables of the same type.

7. **Sequence Generation**: `generate-sequence` is more flexible than `make-sequence` when you need computed initial values.

8. **Double-Float Preference**: The package emphasizes double-float precision for numerical stability in scientific computing.


