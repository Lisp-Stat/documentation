---
title: "Select"
linkTitle: "Selecting subsets"
author: ["Steven Nunez"]
date: 2021-03-07
weight: 4
description: >
  Selecting subsets of data
---

## Overview {#Overview}

Select provides:

1.  An API for taking slices (elements selected by the Cartesian
    product of vectors of subscripts for each axis) of array-like
    objects.  The most important function is `select`. Unless you want
    to define additional methods for `select`, this is pretty much
    all you need from this library.  See the [API reference](https://lisp-stat.github.io/select/) for
    additional details.
2.  An extensible DSL for selecting a subset of valid
    subscripts.  This is useful if, for example, you want to resolve
    column names in a data frame in your implementation of select.
3.  A set of utility functions for traversing selections in
    array-like objects.

It combines the functionality of dplyr's _slice_ and _select_ methods.

## Basic Usage {#Using}

The most frequently used form is:

<a id="code-snippet--simple-select-example"></a>
```lisp
(select object selection1 selection2 ...)
```

where each `selection` specifies a set of subscripts along the
corresponding axis.  The selection specifications are found below.


## Selection Specifiers {#selection-specifiers}


### Selecting Single Values {#selecting-single-values}

A non-negative integer selects the corresponding index, while a
negative integer selects an index counting backwards from the last
index.  For example:

<a id="code-snippet--example-select-single-value"></a>
```lisp
(select #(0 1 2 3) 1)                  ; => 1
(select #(0 1 2 3) -2)                 ; => 2
```

These are called _singleton_ slices.  Each singleton slice drops the
dimension: vectors become atoms, matrices become vectors, etc.


### Selecting Ranges {#selecting-ranges}

`(range start end)` selects subscripts _i_ where start <= i < end.
When end is `nil`, the last index is included (cf. subseq).  Each
boundary is resolved according to the other rules, if applicable, so
you can use negative integers:

<a id="code-snippet--example-select-range"></a>
```lisp
(select #(0 1 2 3) (range 1 3))         ; => #(1 2)
(select #(0 1 2 3) (range 1 -1))        ; => #(1 2)
```


### Selecting All Subscripts {#selecting-all-subscripts}

_t_ selects all subscripts:

<a id="code-snippet--example-select-all"></a>
```lisp
(select #2A((0 1 2)
	        (3 4 5))
	 t 1)                           ; => #(1 4)
```


### Selecting w/ Sequences {#selecting-w-sequences}

Sequences can be used to make specific selections from the object.  For example:

<a id="code-snippet--example-select-with-sequence"></a>
```lisp
(select #(0 1 2 3 4 5 6 7 8 9)
	(vector (range 1 3) 6 (range -2 -1))) ; => #(1 2 3 6 8 9)

(select #(0 1 2) '(2 2 1 0 0))                ; => #(2 2 1 0 0)
```


### Bit Vectors as Mask {#bit-vectors-as-mask}

Bit vectors can be used to select elements of arrays and sequences
as well:

<a id="code-snippet--example-select-bitmask"></a>
```lisp
(select #(0 1 2 3 4) #*00110)          ; => #(2 3)
```


## Extensions {#extensions}

The previous section describes the core functionality. The semantics
can be extended.  The extensions in this section are provided by the
library and prove useful in practice. Their implementation provide
good examples of extending the library.

`including` is convenient if you want the selection to include the
end of the range:

<a id="code-snippet--example-select-including"></a>
```lisp
(select #(0 1 2 3) (including 1 2))
				    ; => #(1 2), cf. (select ... (range 1 3))
```

`nodrop` is useful if you do not want to drop dimensions:

<a id="code-snippet--example-select-including"></a>
```lisp
(select #(0 1 2 3) (nodrop 2))
			; => #(2), cf. (select ... (range 2 3))
```

All of these are trivial to implement. If there is something you are
missing, you can easily extend `select`.  Pull request are
welcome.

`(ref)` is a version of `(select)` that always returns a single
element, so it can only be used with singleton slices.


## Select Semantics {#select-semantics}

Arguments of `select`, except the first one, are meant to be
resolved using `canonical-representation`, in the `select-dev`
package. If you want to extend `select`, you should define methods
for `canonical-representation`. See the source code for the best
examples. Below is a simple example that extends the semantics with
ordinal numbers.

<a id="code-snippet--example-select-including"></a>
```lisp
(defmacro define-ordinal-selection (number)
  (check-type number (integer 0))
  `(defmethod select-dev:canonical-representation
       ((axis integer) (select (eql ',(intern (format nil \"~:@@(~:r~)\" number)))))
     (assert (< ,number axis))
     (select-dev:canonical-singleton ,number)))

(define-ordinal-selection 1)
(define-ordinal-selection 2)
(define-ordinal-selection 3)

(select #(0 1 2 3 4 5) (range 'first 'third)) ; => #(1 2)
```

Note the following:

-   The value returned by `canonical-representation` needs to be
    constructed using `canonical-singleton`, `canonical-range`, or
    `canonical-sequence`. You should not use the internal
    representation directly as it is subject to change.
-   You can assume that `axis` is an integer; this is the
    default. An object may define a more complex mapping (such as, for
    example, named rows & columns), but unless a method specialized to
    that is found, `canonical-representation` will just query its
    dimension (with `axis-dimension`) and try to find a method
    that works on integers.
-   You need to make sure that the subscript is valid, hence the
    assertion.
