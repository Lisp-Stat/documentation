---
title: "Statistics"
author: ["Steven Nunez"]
date: 2026-03-14
weight: 8
description: >
  Batch and streaming statistical functions
---

## Overview

The statistics system is organised in three layers.

`stat-generics` defines the shared generic functions — `mean`, `variance`,
`standard-deviation`, `quantile`, `quantiles`, `median`, `mode`, and `modes` —
together with the weight type hierarchy.  Neither package contains standalone
implementations; they provide the protocol that other implementation packages
specialise.

`batch-statistics` provides methods on those generics specialised on fully
realised Common Lisp sequences, together with additional descriptive functions
that have no natural streaming equivalent.

`streaming-statistics` provides accumulator objects and methods for computing
statistics in a single pass.  Observations are incorporated one at a time with
`add`; independent accumulators can be merged exactly with `pool`.  This makes
the streaming package appropriate for large data sets, live data feeds, and
parallel or distributed computation.

Because both implementation packages specialise the same generic functions,
`mean`, `variance`, `standard-deviation`, `quantile`, and `quantiles` dispatch
correctly whether you pass a plain sequence or a streaming accumulator.

---

## Weight types

Weights in this system carry *statistical meaning*, not just numeric values.
Rather than a bare vector of numbers, a weight object declares how those numbers
should be interpreted, which in turn determines the correct bias correction for
variance and standard deviation.  This design follows the convention established
by Julia's StatsBase library.

Three weight classes are provided, all inheriting from the abstract base class
`stat-generics:weights`.  Each class encodes a different statistical scenario
and contributes a different *correction denominator* — the divisor used in the
bias-corrected weighted variance formula.  Everything else in the variance
calculation is identical across weight types; only this one quantity varies.

### weights

**`weights`** is the abstract base class for all weight types.  It stores the
weight vector and a cached sum.  Do not instantiate `weights` directly; use one
of the three concrete subclasses below.

Two readers are available on all weight objects:

| Reader | Returns |
|--------|---------|
| `weight-values` | The weight vector (coerced from whatever sequence was supplied at construction) |
| `weight-sum` | Cached Σwᵢ, computed once at construction time |

### frequency-weights

**`frequency-weights`** represents integer count weights, appropriate when each
observation stands for a number of identical repeated measurements — for
example, when a data set has been compressed by recording a value and the number
of times it was observed.

```lisp
;; Observation 1.0 seen once, 2.0 seen three times, 3.0 seen once
(make-instance 'stat-generics:frequency-weights :values #(1 3 1))

;; Convenience constructor
(fweights #(1 3 1))
```

Bias correction denominator: Σw − 1.  This is the direct weighted analogue of
the familiar n − 1 Bessel correction.

### analytic-weights

**`analytic-weights`** represents reliability or precision weights, typically
used when each observation has a known measurement variance and is weighted by
its reciprocal (inverse-variance weighting).

```lisp
(aweights #(0.5 1.0 2.0))
```

Bias correction denominator: Σw − Σ(wᵢ²)/Σw, the V₁ − V₂/V₁ formula where
V₁ = Σwᵢ and V₂ = Σwᵢ².

### probability-weights

**`probability-weights`** represents sampling inclusion probabilities, as used
in survey statistics when observations are drawn with unequal probability from a
population.

```lisp
(pweights #(0.2 0.5 0.3))
```

Bias correction denominator: Σw · (n − 1)/n, the weighted analogue of the
n/(n − 1) Bessel correction.

### Choosing a weight type

| Situation | Weight type |
|-----------|-------------|
| Each row represents *k* identical observations | `frequency-weights` |
| Each observation has a known variance; weights are 1/σᵢ² | `analytic-weights` |
| Data collected by probability sampling with known inclusion probabilities | `probability-weights` |

The convenience constructors `fweights`, `aweights`, and `pweights` are the
idiomatic shorthand for the three types:

```lisp
(fweights #(1 2 1))   ; frequency-weights
(aweights #(1 2 1))   ; analytic-weights
(pweights #(1 2 1))   ; probability-weights
```

The statistical functions that accept a `:weights` keyword do not inspect the
numeric values to infer the type; the type must be declared at construction
time.  Passing the wrong type yields a numerically valid but statistically
incorrect result without any error.

---

## Batch

`batch-statistics` provides methods on the `stat-generics` generic functions
specialised on `sequence`, so both lists and vectors are accepted without a
separate code path.  It also exports several descriptive functions that have
no natural generic home.

### mean

**`mean`** computes the arithmetic mean of a sequence.

Lambda list: `(seq &key weights)`

```lisp
(mean #(2 4 6 8))    ; => 5
(mean '(1 2 3 4 5))  ; => 3
```

When `:weights` is a `weights` object of matching length, the weighted mean
Σ(wᵢ·xᵢ) / Σwᵢ is returned:

```lisp
(mean #(10 20 30)
                    :weights (fweights #(1 2 1))) ; => 20
```

### variance

**`variance`** computes the variance of a sequence.

Lambda list: `(seq &key weights corrected mean)`

With `:corrected T` (the default) the sample variance normalised by n − 1 is
returned.  With `:corrected NIL` the population variance normalised by n is
returned.  A precomputed `:mean` avoids a second pass through the data.

```lisp
(variance #(2 4 4 4 5 5 7 9))                ; => 4
(variance #(2 4 4 4 5 5 7 9) :corrected nil) ; => 7/2
```

When `:weights` is supplied, the correction denominator is determined
automatically by the weight type — see [Weight types](#weight-types) above.
This means the same call produces the appropriate result whether the weights are
frequency counts, reliability weights, or sampling probabilities.

```lisp
;; Frequency weights: denominator is Σw - 1 = 3
(variance #(10 20 30)
                        :weights (fweights #(1 2 1)))

;; Analytic weights: denominator is Σw - Σ(w²)/Σw
(variance #(10 20 30)
                        :weights (aweights #(1 2 1)))
```

### standard-deviation

**`standard-deviation`** computes the standard deviation of a sequence.
It accepts the same keyword arguments as `variance` and returns
`(sqrt (variance seq ...))`.

Lambda list: `(seq &key weights corrected mean)`

```lisp
(standard-deviation #(2 4 4 4 5 5 7 9)) ; => 2
```

### sd

**`sd`** is a convenience alias for `standard-deviation`, defined in
`batch-statistics`.

Lambda list: `(object &rest keys)`

All keyword arguments are forwarded to `standard-deviation`.

```lisp
(sd #(2 4 4 4 5 5 7 9)) ; => 2
```

### median

**`median`** returns the median value of a sequence.

Lambda list: `(seq)`

The median is the 0.5 quantile computed using the 0.5-correction empirical
quantile method; see `quantile` for the definition.

```lisp
(median #(1 2 3 4 5)) ; => 3
(median #(1 2 3 4))   ; => 5/2
```

### quantile

**`quantile`** returns the empirical quantile of a sequence at probability `q`,
using a 0.5 correction with linear interpolation.

Lambda list: `(seq q &key weights)`

`q` must lie in \[0, 1\]; values outside this range signal an error.

The 0.5-correction formula places the probability mass of the *i*-th smallest
value (1-indexed) at (i − 0.5)/n.  Values below the first mass point return the
minimum; values above the last mass point return the maximum.  Between mass
points the result is linearly interpolated between the two adjacent sorted
values.

```lisp
(quantile #(1 2 3 4 5) 0.5)  ; => 3
(quantile #(1 2 3 4 5) 0.25) ; => 2
```

Weighted quantiles on plain sequences are not yet implemented.  Use a
`sorted-reals` accumulator from `streaming-statistics` when weighted quantile
estimation is needed — see [weighted-quantiles](#weighted-quantiles).

### quantiles

**`quantiles`** computes multiple quantiles in a single sorted pass, returning
a vector of results.

Lambda list: `(seq qs &key weights)`

`qs` is a sequence of probabilities in \[0, 1\].  Sorting once and mapping over
all of `qs` is more efficient than calling `quantile` repeatedly on large data
sets.

```lisp
(quantiles #(1 2 3 4 5) #(0.25 0.5 0.75))
; => #(2 3 4)
```

### mode

**`mode`** returns the most frequent element of a sequence.  When there are
ties it returns the first mode in order of appearance.

Lambda list: `(sequence)`

```lisp
(mode #(1 2 3 2 1 2))       ; => 2
(mode '(:a :b :a :c :b :a)) ; => :A
```

### modes

**`modes`** returns a list of *all* most-frequent elements of a sequence, in
order of first appearance.

Lambda list: `(sequence)`

```lisp
(modes #(1 2 3 2 1))  ; => (1 2)
(modes '(:a :b :c))   ; => (:A :B :C)
```

### weighted-mean

**`weighted-mean`** computes Σ(wᵢ·xᵢ) / Σwᵢ directly from a vector and a
`weights` object.

Lambda list: `(vec w)`

The length of the weight vector must equal the length of `vec`; a mismatch
signals an error.  This function is the internal implementation called by the
`mean` method when weights are supplied; it is exported for callers that already
hold a `weights` object and want to bypass generic dispatch.

```lisp
(weighted-mean #(10 20 30)
                                (fweights #(1 2 1))) ; => 20
```

### weighted-variance

**`weighted-variance`** computes the weighted variance of a vector.

Lambda list: `(vec w &key corrected mean)`

With `:corrected T` (the default) the correction denominator is determined by
the type of `w` — see [Weight types](#weight-types).  With `:corrected NIL` the
result is normalised by Σwᵢ regardless of weight type, giving the population
weighted variance.  A precomputed `:mean` avoids an extra pass.

```lisp
;; Analytic weights: denominator is Σw - Σ(w²)/Σw
(weighted-variance #(10 20 30)
                                    (aweights #(1 2 1)))

;; Population (uncorrected) version
(weighted-variance #(10 20 30)
                                    (fweights #(1 2 1))
                                    :corrected nil)
```

### interquartile-range

**`interquartile-range`** (alias **`iqr`**) returns Q75 − Q25 of `x`.

Lambda list: `(x)`

The interquartile range is a robust measure of spread that is insensitive to
outliers.  `x` is sorted once and both quantiles are extracted in that single
pass.

```lisp
(interquartile-range #(1 2 3 4 5 6 7 8 9 10)) ; => 5
(iqr                 #(1 2 3 4 5 6 7 8 9 10)) ; => 5
```

### fivenum

**`fivenum`** computes a five-number summary of `x`, returning a vector
`#(min q25 median q75 max)`.

Lambda list: `(x &key tukey)`

With `:tukey NIL` (the default) the five values are the 0th, 25th, 50th, 75th,
and 100th empirical quantiles using the 0.5-correction method.

With `:tukey T` the classic Tukey hinges are used instead.  The sorted data is
split at the median into a lower half (indices 0 to ⌊(n+1)/2⌋ − 1) and an
upper half (indices ⌊n/2⌋ to n − 1).  The median of each half becomes the lower
and upper hinge respectively.  The two methods agree asymptotically but can
differ noticeably on small samples.

```lisp
(fivenum #(1 2 3 4 5 6 7 8 9 10))
; => #(1 25/8 11/2 71/8 10)

(fivenum #(1 2 3 4 5 6 7 8 9 10) :tukey t)
; => #(1 3 11/2 8 10)
```

### scale

**`scale`** standardises `x`, returning a new vector of the form
(xᵢ − center) / scale.  It is modelled on R's `scale()`.

Lambda list: `(x &key center scale)`

`:center` defaults to the mean of `x`; `:scale` defaults to the standard
deviation of `x`.  Pass `nil` to suppress centering or scaling independently.
When both are `nil`, `x` is returned unchanged.

```lisp
(scale #(2 4 4 4 5 5 7 9))
; => #(-1.5 -0.5 -0.5 -0.5 0.0 0.0 1.0 2.0)

;; Centre only, no scaling
(scale #(1 2 3) :scale nil)
; => #(-1 0 1)

;; Scale only, no centering
(scale #(1 2 3) :center nil)
; => #(0.408... 0.816... 1.224...)
```

---

## Streaming

`streaming-statistics` provides accumulator objects that compute statistics
in a single pass over data.  Three accumulator types are provided.

| Type | Purpose |
|------|---------|
| `central-sample-moments` | Mean, variance, and higher central moments |
| `sorted-reals` | Quantiles via lazy sort-on-demand |
| `sparse-counter` | Frequency tables and cross-tabulations |

### Accumulator protocol

All accumulator types share a common protocol: `tally`, `add`, and `pool`.

#### tally

**`tally`** returns the total weight of elements incorporated into an
accumulator.  For unweighted data this equals the element count.

Lambda list: `(accumulator)`

```lisp
(let ((acc (central-sample-moments nil)))
  (add acc 1)
  (add acc 2)
  (add acc 3)
  (tally acc)) ; => 3
```

#### add

**`add`** incorporates a new observation into an accumulator and returns the
observation.  `nil` is silently ignored unless a specialised method decides
otherwise.  An optional `:weight` keyword (default 1) is supported by
`central-sample-moments` and `sparse-counter`; weights must be non-negative.

Lambda list: `(accumulator object &key weight)`

```lisp
(let ((acc (central-sample-moments nil)))
  (add acc 5.0)
  (add acc 7.0 :weight 2)
  (mean acc)) ; => 19/3
```

#### pool

**`pool`** merges two or more accumulators into a single combined accumulator.
Pooling is exact: the result is identical to having observed all elements in a
single accumulator from the start, making `pool` suitable for combining results
from parallel workers.

Lambda list: `(accumulator &rest more-accumulators)`

When two `central-sample-moments` accumulators of different degrees are pooled,
the result is downgraded to the information available in both.

```lisp
(let ((a1 (central-sample-moments #(1 2 3)))
      (a2 (central-sample-moments #(4 5 6))))
  (mean (pool a1 a2))) ; => 7/2
```

#### Conditions

| Condition | Signalled when |
|-----------|----------------|
| `empty-accumulator` | A statistics function is called on an accumulator with no elements |
| `not-enough-elements-in-accumulator` | Fewer elements are present than the function requires (e.g. `variance` with n = 1) |
| `information-not-collected-in-accumulator` | A higher-order moment is requested but the accumulator's degree is too low |

### \*central-sample-moments-default-degree\*

**`*central-sample-moments-default-degree*`** is a variable controlling the
default degree used when constructing a `central-sample-moments` accumulator
without an explicit `:degree` argument.  Its initial value is 4, meaning all
four central moments are tracked by default.

Set this variable to a lower value in performance-sensitive code that only
needs the mean or variance and does not want to pay the overhead of tracking
higher moments.

```lisp
;; Collect only mean and variance by default
(setf *central-sample-moments-default-degree* 2)
```

### central-sample-moments

**`central-sample-moments`** is both a struct type and a generic function that
constructs one.  It tracks a running weighted mean and optionally the second
through fourth central moments using a numerically stable single-pass algorithm
(Bennett et al., 2009; West, 1979).

Lambda list: `(object &key degree weights)`

`degree` controls which moments are collected:

| Degree | Moments tracked | Functions enabled |
|--------|-----------------|-------------------|
| 1 | Mean | `mean` |
| 2 | Mean, S₂ | `mean`, `variance`, `standard-deviation`, `central-m2` |
| 3 | Mean, S₂, S₃ | All degree-2 functions, plus `central-m3`, `skewness` |
| 4 | Mean, S₂, S₃, S₄ | All degree-3 functions, plus `central-m4`, `kurtosis` |

The default degree is controlled by
`*central-sample-moments-default-degree*`, which is initially 4.

`object` may be `nil` (produces an empty accumulator ready to receive
observations via `add`), an existing `central-sample-moments` (returned
unchanged, subject to the degree constraint), or any sequence (all elements are
incorporated immediately).

```lisp
;; Build from a sequence
(let ((acc (central-sample-moments #(2 4 4 4 5 5 7 9))))
  (mean acc)      ; => 5
  (variance acc)) ; => 4

;; Build empty and accumulate one observation at a time
(let ((acc (central-sample-moments nil :degree 2)))
  (add acc 3.0)
  (add acc 5.0)
  (mean acc)) ; => 4.0

;; Weighted sequence
(central-sample-moments #(1 2 3) :weights #(1 2 1))
```

### central-sample-moments-degree

**`central-sample-moments-degree`** returns the degree (1–4) of an existing
accumulator, reflecting the highest moment being tracked.

Lambda list: `(central-sample-moments)`

```lisp
(central-sample-moments-degree
  (central-sample-moments nil :degree 3)) ; => 3
```

### mean

**`mean`** returns the mean of the accumulated elements.  Requires degree ≥ 1.

Lambda list: `(accumulator)`

Signals `empty-accumulator` if no elements have been added.

```lisp
(mean
  (central-sample-moments #(1 2 3 4 5))) ; => 3
```

### variance

**`variance`** returns the sample variance of the accumulated elements,
normalised by (total-weight − 1).  Requires degree ≥ 2 and at least two
observations.

Lambda list: `(accumulator)`

Signals `not-enough-elements-in-accumulator` with n = 1 and
`information-not-collected-in-accumulator` if the accumulator was built with
degree 1.

```lisp
(variance
  (central-sample-moments #(2 4 4 4 5 5 7 9))) ; => 4
```

### standard-deviation

**`standard-deviation`** returns `(sqrt (variance accumulator))`.  Requires
degree ≥ 2 and at least two observations.

Lambda list: `(accumulator)`

```lisp
(standard-deviation
  (central-sample-moments #(2 4 4 4 5 5 7 9))) ; => 2
```

### central-m2

**`central-m2`** returns the second central moment, normalised by total weight
(the population, not bias-corrected, estimate).  Requires degree ≥ 2.

Lambda list: `(object &key weights)`

`object` may be a `central-sample-moments` accumulator or any sequence;
sequences are converted to an accumulator on demand.  Note the distinction from
`variance`: `central-m2` divides by Σw whereas `variance` divides by Σw − 1.

```lisp
(central-m2 #(2 4 4 4 5 5 7 9)) ; => 7/2
```

### central-m3

**`central-m3`** returns the third central moment, normalised by total weight.
Requires degree ≥ 3.

Lambda list: `(object &key weights)`

`object` may be a `central-sample-moments` accumulator or any sequence.  A
result of zero indicates a symmetric distribution.

```lisp
(central-m3 #(1 2 3 4 5)) ; => 0.0
```

### central-m4

**`central-m4`** returns the fourth central moment, normalised by total weight.
Requires degree ≥ 4.

Lambda list: `(object &key weights)`

`object` may be a `central-sample-moments` accumulator or any sequence.

```lisp
(central-m4 #(1 2 3 4 5)) ; => 2.0
```

### skewness

**`skewness`** returns the skewness, defined as m₃ / m₂^(3/2), where m₂ and m₃
are the second and third central moments.  Requires degree ≥ 3.

Lambda list: `(object &key weights)`

A value of zero indicates a symmetric distribution.  Positive values indicate
right-skew (a longer right tail); negative values indicate left-skew.  `object`
may be a `central-sample-moments` accumulator or any sequence.

```lisp
(skewness #(1 2 3 4 5)) ; => 0.0
```

### kurtosis

**`kurtosis`** returns the kurtosis, defined as m₄ / m₂².  Requires degree ≥ 4.

Lambda list: `(object &key weights)`

This is the *raw* (non-excess) kurtosis; a normal distribution has kurtosis 3.
Distributions with heavier tails than the normal have kurtosis greater than 3;
lighter-tailed distributions have kurtosis less than 3.  To obtain excess
kurtosis, subtract 3 from the result.  `object` may be a
`central-sample-moments` accumulator or any sequence.

```lisp
(kurtosis #(1 2 3 4 5)) ; => 17/10
```

### sorted-reals

**`sorted-reals`** is an accumulator for quantile computation.  Elements are
accumulated with `add` and sorted lazily: the sort is deferred until a quantile
is actually requested, so adding many observations in succession is cheap.  Once
sorted, the internal vector is reused until new unordered elements arrive.

Construct with `make-instance` or convert an existing sequence with
`ensure-sorted-reals`.

```lisp
(let ((sr (make-instance 'sorted-reals)))
  (add sr 3.0)
  (add sr 1.0)
  (add sr 2.0)
  (quantile  sr 0.5)           ; => 2.0
  (quantiles sr #(0.25 0.75))) ; => #(1.5 2.5)
```

The printed representation provides a quick five-number summary:

```
#<SORTED-REALS min: 1.0, q25: 1.5, q50: 2.0, q75: 2.5, max: 3.0>
```

### sorted-reals-elements

**`sorted-reals-elements`** returns the contents of a `sorted-reals`
accumulator as a sorted vector, merging any pending unordered elements first.
Subsequent calls reuse the cached vector until new elements are added.

Lambda list: `(sorted-reals)`

```lisp
(sorted-reals-elements sr) ; => #(1.0 2.0 3.0)
```

### empirical-quantile

**`empirical-quantile`** computes the empirical quantile of a pre-sorted vector
at probability `q`, using the same 0.5-correction and linear interpolation as
the batch `quantile` method.

Lambda list: `(sorted-vector q)`

The caller is responsible for ensuring `sorted-vector` is in ascending order;
this is not checked at runtime.  `q` must lie in \[0, 1\].

```lisp
(empirical-quantile #(1 2 3 4 5) 0.5)  ; => 3
(empirical-quantile #(1 2 3 4 5) 0.25) ; => 2
```

### empirical-quantile-probabilities

**`empirical-quantile-probabilities`** returns a vector of `n` probability
values whose corresponding empirical quantiles exactly recover the original
sorted sample.  These are the canonical plotting positions for an empirical CDF.

Lambda list: `(n)`

The *i*-th probability (0-indexed) is (2i + 1) / (2n), placing equal-width
probability mass around each observation.

```lisp
(empirical-quantile-probabilities 4)
; => #(1/8 3/8 5/8 7/8)
```

### quantile

**`quantile`** returns the quantile of a `sorted-reals` accumulator at
probability `q`.

Lambda list: `(accumulator q &key weights)`

When `:weights` is supplied, weighted quantile estimation is used; see
`weighted-quantiles` for the algorithm.

```lisp
(quantile sr 0.5) ; => 2.0
```

### quantiles

**`quantiles`** returns a vector of quantiles of a `sorted-reals` accumulator
at the probabilities in `qs`, sorting the accumulated elements at most once
regardless of the number of quantiles requested.

Lambda list: `(accumulator qs &key weights)`

```lisp
(quantiles sr #(0 0.5 1)) ; => #(1.0 2.0 3.0)
```

### ensure-sorted-reals

**`ensure-sorted-reals`** coerces `object` to a `sorted-reals` accumulator,
returning it unchanged if it is already one.  Accepts arrays (which are
flattened with `aops:flatten` before sorting) and lists.

Lambda list: `(object)`

```lisp
(ensure-sorted-reals #(3 1 2))
; => #<SORTED-REALS min: 1, q50: 2, max: 3>
```

### ensure-sorted-vector

**`ensure-sorted-vector`** returns the contents of `object` as a sorted vector,
constructing a `sorted-reals` internally if necessary.

Lambda list: `(object)`

```lisp
(ensure-sorted-vector '(3 1 4 1 5 9))
; => #(1 1 3 4 5 9)
```

### weighted-quantiles

**`weighted-quantiles`** computes quantiles for weighted observations using a
0.5 correction.

Lambda list: `(values weights qs)`

`values` and `weights` are sequences of equal length.  `qs` is a sequence of
probabilities in \[0, 1\].  The function sorts `values` and `weights` together
by value, constructs a cumulative probability table by placing each weight's
mass at its midpoint within the cumulative sum, and interpolates linearly
between bracket points.

```lisp
(weighted-quantiles #(1 2 3)
                                         #(1 2 1)
                                         #(0.25 0.5 0.75))
; => #(1.5 2.0 2.5)
```

Note that `weighted-quantiles` accepts a raw weight vector, not a `weights`
object.  The weight type distinction (and its associated variance correction)
does not apply to quantile estimation.

### sparse-counter

**`sparse-counter`** is the struct type for frequency-counting accumulators.
It wraps a hash table that maps each observed object to its accumulated weight.
Instances are created with `make-sparse-counter` and populated with `add`;
the type itself is exported so that callers can specialise methods on it or
use `typep` for dispatch.

```lisp
(typep (make-sparse-counter)
       'sparse-counter) ; => T
```

### make-sparse-counter

**`make-sparse-counter`** creates an empty `sparse-counter` accumulator backed
by a hash table.

Lambda list: `(&key test)`

`test` is the equality predicate for the hash table; it defaults to `#'equal`.
Use `#'eq` for symbols or keywords; use `#'equalp` for case-insensitive string
comparison.

```lisp
(let ((sc (make-sparse-counter)))
  (add sc :a)
  (add sc :b)
  (add sc :a)
  (sparse-counter-count sc :a)) ; => 2
```

### sparse-counter-count

**`sparse-counter-count`** returns the accumulated count (or total weight) for
`object` in `sparse-counter`.  Returns 0 if `object` has never been added.

Lambda list: `(sparse-counter object)`

```lisp
(sparse-counter-count sc :a) ; => 2
(sparse-counter-count sc :z) ; => 0
```

### sparse-counter-table

**`sparse-counter-table`** returns the underlying hash table of a
`sparse-counter`, mapping each observed object to its accumulated weight.
Mutating the returned table directly is not recommended.

Lambda list: `(sparse-counter)`

### tabulate

**`tabulate`** tabulates the elements of a sequence, returning a
`sparse-counter` whose counts reflect element frequencies.

Lambda list: `(sequence &key test)`

`test` defaults to `#'equalp`.

```lisp
(tabulate '(:a :b :a :c :a :b))
; => #<SPARSE-COUNTER tally: 6, varieties: 3
;      :A  3  (50.0%)
;      :B  2  (33.3%)
;      :C  1  (16.7%)>
```

### cross-tabulate

**`cross-tabulate`** cross-tabulates two sequences of equal length, returning a
`sparse-counter` whose keys are `(element-from-sequence-1 . element-from-sequence-2)`
cons cells.

Lambda list: `(sequence1 sequence2 &key test)`

`test` defaults to `#'equalp`.  The two sequences must be the same length; a
mismatch signals an error.

```lisp
(cross-tabulate '(:m :f :m) '(:y :y :n))
; => #<SPARSE-COUNTER tally: 3, varieties: 3
;      (:M . :Y)  1  (33.3%)
;      (:F . :Y)  1  (33.3%)
;      (:M . :N)  1  (33.3%)>
```
