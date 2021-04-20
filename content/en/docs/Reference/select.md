---
title: "The Select Reference Manual"
linkTitle: "Select"
weight: 3
date: 2021-03-07
description: >
  System version 1.2.
---

# Systems {#Systems}

The main system appears first, followed by any subsystem dependency.

## `select` {#The-select-system}

select

System, select

**Author**

:   Steve Nunez

**Home Page**

:   <https://lisp-stat.github.io/select/>

**Source Control**

:   `(:git "git://github.com/symbolics/select")`

**Bug Tracker**

:   <https://github.com/Lisp-Stat/select/issues/>

**License**

:   MS-PL

**Description**

:   DSL for array slices.

**Long Description**

:   Select is a facility for selecting portions of sequences or arrays.
    It provides:

    An API for taking slices (elements selected by the Cartesian product
    of vectors of subscripts for each axis) of array-like objects. The
    most important function is 'select'. Unless you want to define
    additional methods for 'select', this is pretty much all you need
    from this library. See the documentation at
    https://symbolics.github.io/select/ for a tutorial.

    An extensible DSL for selecting a subset of valid subscripts. This
    is useful if, for example, you want to resolve column names in a
    data frame in your implementation of slice.

    A set of utility functions for traversing slices in array-like
    objects.

**Version**

:   1.2

**Dependencies**

:   -   `alexandria`

    -   `anaphora`

    -   `let-plus`

**Source**

:   [`select.asd`](#go-to-the-select_2024asd-file) (file)

**Directory**

:   [ignore](file://s:/src/select/)

**Components**

:   -   [`package.lisp`](#go-to-the-select_002fpackage_2024lisp-file)
        (file)

    -   [`select-dev.lisp`](#go-to-the-select_002fselect_002ddev_2024lisp-file)
        (file)

    -   [`select.lisp`](#go-to-the-select_002fselect_2024lisp-file)
        (file)

# Files {#Files}

Files are sorted by type and then listed depth-first from the systems
components trees.

## Lisp {#Lisp-files}

### `select.asd` {#The-select_2024asd-file}

select.asd

Lisp File,

select.asd

File, Lisp,

select.asd

**Location**

:   [ignore](file://s:/src/select/select.asd)

**Systems**

:   [`select`](#go-to-the-select-system) (system)

### `select/package.lisp` {#The-select_002fpackage_2024lisp-file}

select/package.lisp

Lisp File,

select/package.lisp

File, Lisp,

select/package.lisp

**Parent**

:   [`select`](#go-to-the-select-system) (system)

**Location**

:   [ignore](file://s:/src/select/package.lisp)

**Packages**

:   -   [`select-dev`](#go-to-the-SELECT_002dDEV-package)

    -   [`select`](#go-to-the-SELECT-package)

### `select/select-dev.lisp` {#The-select_002fselect_002ddev_2024lisp-file}

select/select-dev.lisp

Lisp File,

select/select-dev.lisp

File, Lisp,

select/select-dev.lisp

**Dependency**

:   [`package.lisp`](#go-to-the-select_002fpackage_2024lisp-file) (file)

**Parent**

:   [`select`](#go-to-the-select-system) (system)

**Location**

:   [ignore](file://s:/src/select/select-dev.lisp)

**Exported Definitions**

:   -   [`all-singleton-representations?`](#go-to-the-SELECT_002dDEV_2236_2236ALL_002dSINGLETON_002dREPRESENTATIONS_003f-function)
        (function)

    -   [`axis-dimension`](#go-to-the-SELECT_002dDEV_2236_2236AXIS_002dDIMENSION-generic-function)
        (generic function)

    -   [`canonical-range`](#go-to-the-SELECT_002dDEV_2236_2236CANONICAL_002dRANGE-function)
        (function)

    -   [`canonical-range`](#go-to-the-SELECT_002dDEV_2236_2236CANONICAL_002dRANGE-structure)
        (structure)

    -   [`canonical-representation`](#go-to-the-SELECT_002dDEV_2236_2236CANONICAL_002dREPRESENTATION-generic-function)
        (generic function)

    -   [`canonical-representation`](#go-to-the-SELECT_002dDEV_2236_2236CANONICAL_002dREPRESENTATION-COMMON_002dLISP_2236_2236T-COMMON_002dLISP_2236_2236T-method)
        (method)

    -   [`canonical-representation`](#go-to-the-SELECT_002dDEV_2236_2236CANONICAL_002dREPRESENTATION-COMMON_002dLISP_2236_2236T-SELECT_002dDEV_2236_2236CANONICAL_002dRANGE-method)
        (method)

    -   [`canonical-representation`](#go-to-the-SELECT_002dDEV_2236_2236CANONICAL_002dREPRESENTATION-COMMON_002dLISP_2236_2236T-SELECT_002dDEV_2236_2236CANONICAL_002dSEQUENCE-method)
        (method)

    -   [`canonical-representation`](#go-to-the-SELECT_002dDEV_2236_2236CANONICAL_002dREPRESENTATION-COMMON_002dLISP_2236_2236INTEGER-COMMON_002dLISP_2236_2236NULL-method)
        (method)

    -   [`canonical-representation`](#go-to-the-SELECT_002dDEV_2236_2236CANONICAL_002dREPRESENTATION-COMMON_002dLISP_2236_2236INTEGER-COMMON_002dLISP_2236_2236INTEGER-method)
        (method)

    -   [`canonical-representation`](#go-to-the-SELECT_002dDEV_2236_2236CANONICAL_002dREPRESENTATION-COMMON_002dLISP_2236_2236T-COMMON_002dLISP_2236_2236SEQUENCE-method)
        (method)

    -   [`canonical-representation`](#go-to-the-SELECT_002dDEV_2236_2236CANONICAL_002dREPRESENTATION-COMMON_002dLISP_2236_2236INTEGER-_2768eql-COMMON_002dLISP_2236_2236T_2769-method)
        (method)

    -   [`canonical-representation`](#go-to-the-SELECT_002dDEV_2236_2236CANONICAL_002dREPRESENTATION-COMMON_002dLISP_2236_2236T-COMMON_002dLISP_2236_2236BIT_002dVECTOR-method)
        (method)

    -   [`canonical-representations`](#go-to-the-SELECT_002dDEV_2236_2236CANONICAL_002dREPRESENTATIONS-function)
        (function)

    -   [`canonical-sequence`](#go-to-the-SELECT_002dDEV_2236_2236CANONICAL_002dSEQUENCE-function)
        (function)

    -   [`canonical-sequence`](#go-to-the-SELECT_002dDEV_2236_2236CANONICAL_002dSEQUENCE-structure)
        (structure)

    -   [`canonical-singleton`](#go-to-the-SELECT_002dDEV_2236_2236CANONICAL_002dSINGLETON-function)
        (function)

    -   [`column-major-setup`](#go-to-the-SELECT_002dDEV_2236_2236COLUMN_002dMAJOR_002dSETUP-function)
        (function)

    -   [`representation-dimensions`](#go-to-the-SELECT_002dDEV_2236_2236REPRESENTATION_002dDIMENSIONS-function)
        (function)

    -   [`row-major-setup`](#go-to-the-SELECT_002dDEV_2236_2236ROW_002dMAJOR_002dSETUP-function)
        (function)

    -   [`select-reserved-symbol?`](#go-to-the-SELECT_002dDEV_2236_2236SELECT_002dRESERVED_002dSYMBOL_003f-function)
        (function)

    -   [`singleton-representation?`](#go-to-the-SELECT_002dDEV_2236_2236SINGLETON_002dREPRESENTATION_003f-function)
        (function)

    -   [`traverse-representations`](#go-to-the-SELECT_002dDEV_2236_2236TRAVERSE_002dREPRESENTATIONS-macro)
        (macro)

**Internal Definitions**

:   -   [`canonical-range-end`](#go-to-the-SELECT_002dDEV_2236_2236CANONICAL_002dRANGE_002dEND-function)
        (function)

    -   [`(setf canonical-range-end)`](#go-to-the-SELECT_002dDEV_2236_2236_2768SETF-CANONICAL_002dRANGE_002dEND_2769-function)
        (function)

    -   [`canonical-range-p`](#go-to-the-SELECT_002dDEV_2236_2236CANONICAL_002dRANGE_002dP-function)
        (function)

    -   [`canonical-range-start`](#go-to-the-SELECT_002dDEV_2236_2236CANONICAL_002dRANGE_002dSTART-function)
        (function)

    -   [`(setf canonical-range-start)`](#go-to-the-SELECT_002dDEV_2236_2236_2768SETF-CANONICAL_002dRANGE_002dSTART_2769-function)
        (function)

    -   [`canonical-sequence-p`](#go-to-the-SELECT_002dDEV_2236_2236CANONICAL_002dSEQUENCE_002dP-function)
        (function)

    -   [`canonical-sequence-vector`](#go-to-the-SELECT_002dDEV_2236_2236CANONICAL_002dSEQUENCE_002dVECTOR-function)
        (function)

    -   [`(setf canonical-sequence-vector)`](#go-to-the-SELECT_002dDEV_2236_2236_2768SETF-CANONICAL_002dSEQUENCE_002dVECTOR_2769-function)
        (function)

    -   [`copy-canonical-range`](#go-to-the-SELECT_002dDEV_2236_2236COPY_002dCANONICAL_002dRANGE-function)
        (function)

    -   [`copy-canonical-sequence`](#go-to-the-SELECT_002dDEV_2236_2236COPY_002dCANONICAL_002dSEQUENCE-function)
        (function)

    -   [`make-canonical-range`](#go-to-the-SELECT_002dDEV_2236_2236MAKE_002dCANONICAL_002dRANGE-function)
        (function)

    -   [`make-canonical-sequence`](#go-to-the-SELECT_002dDEV_2236_2236MAKE_002dCANONICAL_002dSEQUENCE-function)
        (function)

    -   [`representation-dimension`](#go-to-the-SELECT_002dDEV_2236_2236REPRESENTATION_002dDIMENSION-function)
        (function)

    -   [`representation-initial-value`](#go-to-the-SELECT_002dDEV_2236_2236REPRESENTATION_002dINITIAL_002dVALUE-function)
        (function)

    -   [`representation-iterator`](#go-to-the-SELECT_002dDEV_2236_2236REPRESENTATION_002dITERATOR-function)
        (function)

### `select/select.lisp` {#The-select_002fselect_2024lisp-file}

select/select.lisp

Lisp File,

select/select.lisp

File, Lisp,

select/select.lisp

**Dependency**

:   [`select-dev.lisp`](#go-to-the-select_002fselect_002ddev_2024lisp-file)
    (file)

**Parent**

:   [`select`](#go-to-the-select-system) (system)

**Location**

:   [ignore](file://s:/src/select/select.lisp)

**Exported Definitions**

:   -   [`canonical-representation`](#go-to-the-SELECT_002dDEV_2236_2236CANONICAL_002dREPRESENTATION-COMMON_002dLISP_2236_2236T-SELECT_2236_2236NODROP-method)
        (method)

    -   [`canonical-representation`](#go-to-the-SELECT_002dDEV_2236_2236CANONICAL_002dREPRESENTATION-COMMON_002dLISP_2236_2236T-SELECT_2236_2236RANGE-method)
        (method)

    -   [`canonical-representation`](#go-to-the-SELECT_002dDEV_2236_2236CANONICAL_002dREPRESENTATION-COMMON_002dLISP_2236_2236T-SELECT_2236_2236INCLUDING-method)
        (method)

    -   [`head`](#go-to-the-SELECT_2236_2236HEAD-function) (function)

    -   [`including`](#go-to-the-SELECT_2236_2236INCLUDING-function)
        (function)

    -   [`including`](#go-to-the-SELECT_2236_2236INCLUDING-structure)
        (structure)

    -   [`mask`](#go-to-the-SELECT_2236_2236MASK-generic-function)
        (generic function)

    -   [`mask`](#go-to-the-SELECT_2236_2236MASK-COMMON_002dLISP_2236_2236SEQUENCE-COMMON_002dLISP_2236_2236T-method)
        (method)

    -   [`nodrop`](#go-to-the-SELECT_2236_2236NODROP-function)
        (function)

    -   [`nodrop`](#go-to-the-SELECT_2236_2236NODROP-structure)
        (structure)

    -   [`range`](#go-to-the-SELECT_2236_2236RANGE-function) (function)

    -   [`range`](#go-to-the-SELECT_2236_2236RANGE-structure)
        (structure)

    -   [`ref`](#go-to-the-SELECT_2236_2236REF-generic-function)
        (generic function)

    -   [`ref`](#go-to-the-SELECT_2236_2236REF-COMMON_002dLISP_2236_2236ARRAY-method)
        (method)

    -   [`(setf ref)`](#go-to-the-SELECT_2236_2236_2768SETF-REF_2769-COMMON_002dLISP_2236_2236T-COMMON_002dLISP_2236_2236ARRAY-method)
        (method)

    -   [`(setf ref)`](#go-to-the-SELECT_2236_2236_2768SETF-REF_2769-generic-function)
        (generic function)

    -   [`select`](#go-to-the-SELECT_2236_2236SELECT-generic-function)
        (generic function)

    -   [`select`](#go-to-the-SELECT_2236_2236SELECT-COMMON_002dLISP_2236_2236LIST-method)
        (method)

    -   [`select`](#go-to-the-SELECT_2236_2236SELECT-COMMON_002dLISP_2236_2236ARRAY-method)
        (method)

    -   [`(setf select)`](#go-to-the-SELECT_2236_2236_2768SETF-SELECT_2769-COMMON_002dLISP_2236_2236T-COMMON_002dLISP_2236_2236ARRAY-method)
        (method)

    -   [`(setf select)`](#go-to-the-SELECT_2236_2236_2768SETF-SELECT_2769-generic-function)
        (generic function)

    -   [`tail`](#go-to-the-SELECT_2236_2236TAIL-function) (function)

    -   [`which`](#go-to-the-SELECT_2236_2236WHICH-generic-function)
        (generic function)

    -   [`which`](#go-to-the-SELECT_2236_2236WHICH-COMMON_002dLISP_2236_2236SEQUENCE-method)
        (method)

**Internal Definitions**

:   -   [`copy-including`](#go-to-the-SELECT_2236_2236COPY_002dINCLUDING-function)
        (function)

    -   [`copy-nodrop`](#go-to-the-SELECT_2236_2236COPY_002dNODROP-function)
        (function)

    -   [`copy-range`](#go-to-the-SELECT_2236_2236COPY_002dRANGE-function)
        (function)

    -   [`including-end`](#go-to-the-SELECT_2236_2236INCLUDING_002dEND-function)
        (function)

    -   [`(setf including-end)`](#go-to-the-SELECT_2236_2236_2768SETF-INCLUDING_002dEND_2769-function)
        (function)

    -   [`including-p`](#go-to-the-SELECT_2236_2236INCLUDING_002dP-function)
        (function)

    -   [`including-start`](#go-to-the-SELECT_2236_2236INCLUDING_002dSTART-function)
        (function)

    -   [`(setf including-start)`](#go-to-the-SELECT_2236_2236_2768SETF-INCLUDING_002dSTART_2769-function)
        (function)

    -   [`make-including`](#go-to-the-SELECT_2236_2236MAKE_002dINCLUDING-function)
        (function)

    -   [`make-nodrop`](#go-to-the-SELECT_2236_2236MAKE_002dNODROP-function)
        (function)

    -   [`make-range`](#go-to-the-SELECT_2236_2236MAKE_002dRANGE-function)
        (function)

    -   [`nodrop-index`](#go-to-the-SELECT_2236_2236NODROP_002dINDEX-function)
        (function)

    -   [`(setf nodrop-index)`](#go-to-the-SELECT_2236_2236_2768SETF-NODROP_002dINDEX_2769-function)
        (function)

    -   [`nodrop-p`](#go-to-the-SELECT_2236_2236NODROP_002dP-function)
        (function)

    -   [`range-end`](#go-to-the-SELECT_2236_2236RANGE_002dEND-function)
        (function)

    -   [`(setf range-end)`](#go-to-the-SELECT_2236_2236_2768SETF-RANGE_002dEND_2769-function)
        (function)

    -   [`range-p`](#go-to-the-SELECT_2236_2236RANGE_002dP-function)
        (function)

    -   [`range-start`](#go-to-the-SELECT_2236_2236RANGE_002dSTART-function)
        (function)

    -   [`(setf range-start)`](#go-to-the-SELECT_2236_2236_2768SETF-RANGE_002dSTART_2769-function)
        (function)

# Packages {#Packages}

Packages are listed by definition order.

## `select-dev` {#The-select_002ddev-package}

select-dev

Package, select-dev

SELECT-DEV is used to implement SELECT operations on data structures
other than arrays.

**Source**

:   [`package.lisp`](#go-to-the-select_002fpackage_2024lisp-file) (file)

**Use List**

:   -   `let-plus`

    -   `anaphora`

    -   `alexandria`

    -   `common-lisp`

**Used By List**

:   -   `data-frame`

    -   [`select`](#go-to-the-SELECT-package)

**Exported Definitions**

:   -   [`all-singleton-representations?`](#go-to-the-SELECT_002dDEV_2236_2236ALL_002dSINGLETON_002dREPRESENTATIONS_003f-function)
        (function)

    -   [`axis-dimension`](#go-to-the-SELECT_002dDEV_2236_2236AXIS_002dDIMENSION-generic-function)
        (generic function)

    -   [`axis-dimension`](#go-to-the-SELECT_002dDEV_2236_2236AXIS_002dDIMENSION-DATA_002dFRAME_2236_2236ORDERED_002dKEYS-method)
        (method)

    -   [`canonical-range`](#go-to-the-SELECT_002dDEV_2236_2236CANONICAL_002dRANGE-function)
        (function)

    -   [`canonical-range`](#go-to-the-SELECT_002dDEV_2236_2236CANONICAL_002dRANGE-structure)
        (structure)

    -   [`canonical-representation`](#go-to-the-SELECT_002dDEV_2236_2236CANONICAL_002dREPRESENTATION-generic-function)
        (generic function)

    -   [`canonical-representation`](#go-to-the-SELECT_002dDEV_2236_2236CANONICAL_002dREPRESENTATION-DATA_002dFRAME_2236_2236ORDERED_002dKEYS-COMMON_002dLISP_2236_2236SYMBOL-method)
        (method)

    -   [`canonical-representation`](#go-to-the-SELECT_002dDEV_2236_2236CANONICAL_002dREPRESENTATION-COMMON_002dLISP_2236_2236T-SELECT_2236_2236NODROP-method)
        (method)

    -   [`canonical-representation`](#go-to-the-SELECT_002dDEV_2236_2236CANONICAL_002dREPRESENTATION-COMMON_002dLISP_2236_2236T-SELECT_2236_2236RANGE-method)
        (method)

    -   [`canonical-representation`](#go-to-the-SELECT_002dDEV_2236_2236CANONICAL_002dREPRESENTATION-COMMON_002dLISP_2236_2236T-SELECT_2236_2236INCLUDING-method)
        (method)

    -   [`canonical-representation`](#go-to-the-SELECT_002dDEV_2236_2236CANONICAL_002dREPRESENTATION-COMMON_002dLISP_2236_2236T-COMMON_002dLISP_2236_2236T-method)
        (method)

    -   [`canonical-representation`](#go-to-the-SELECT_002dDEV_2236_2236CANONICAL_002dREPRESENTATION-COMMON_002dLISP_2236_2236T-SELECT_002dDEV_2236_2236CANONICAL_002dRANGE-method)
        (method)

    -   [`canonical-representation`](#go-to-the-SELECT_002dDEV_2236_2236CANONICAL_002dREPRESENTATION-COMMON_002dLISP_2236_2236T-SELECT_002dDEV_2236_2236CANONICAL_002dSEQUENCE-method)
        (method)

    -   [`canonical-representation`](#go-to-the-SELECT_002dDEV_2236_2236CANONICAL_002dREPRESENTATION-COMMON_002dLISP_2236_2236INTEGER-COMMON_002dLISP_2236_2236NULL-method)
        (method)

    -   [`canonical-representation`](#go-to-the-SELECT_002dDEV_2236_2236CANONICAL_002dREPRESENTATION-COMMON_002dLISP_2236_2236INTEGER-COMMON_002dLISP_2236_2236INTEGER-method)
        (method)

    -   [`canonical-representation`](#go-to-the-SELECT_002dDEV_2236_2236CANONICAL_002dREPRESENTATION-COMMON_002dLISP_2236_2236T-COMMON_002dLISP_2236_2236SEQUENCE-method)
        (method)

    -   [`canonical-representation`](#go-to-the-SELECT_002dDEV_2236_2236CANONICAL_002dREPRESENTATION-COMMON_002dLISP_2236_2236INTEGER-_2768eql-COMMON_002dLISP_2236_2236T_2769-method)
        (method)

    -   [`canonical-representation`](#go-to-the-SELECT_002dDEV_2236_2236CANONICAL_002dREPRESENTATION-COMMON_002dLISP_2236_2236T-COMMON_002dLISP_2236_2236BIT_002dVECTOR-method)
        (method)

    -   [`canonical-representations`](#go-to-the-SELECT_002dDEV_2236_2236CANONICAL_002dREPRESENTATIONS-function)
        (function)

    -   [`canonical-sequence`](#go-to-the-SELECT_002dDEV_2236_2236CANONICAL_002dSEQUENCE-function)
        (function)

    -   [`canonical-sequence`](#go-to-the-SELECT_002dDEV_2236_2236CANONICAL_002dSEQUENCE-structure)
        (structure)

    -   [`canonical-singleton`](#go-to-the-SELECT_002dDEV_2236_2236CANONICAL_002dSINGLETON-function)
        (function)

    -   [`column-major-setup`](#go-to-the-SELECT_002dDEV_2236_2236COLUMN_002dMAJOR_002dSETUP-function)
        (function)

    -   [`representation-dimensions`](#go-to-the-SELECT_002dDEV_2236_2236REPRESENTATION_002dDIMENSIONS-function)
        (function)

    -   [`row-major-setup`](#go-to-the-SELECT_002dDEV_2236_2236ROW_002dMAJOR_002dSETUP-function)
        (function)

    -   [`select-reserved-symbol?`](#go-to-the-SELECT_002dDEV_2236_2236SELECT_002dRESERVED_002dSYMBOL_003f-function)
        (function)

    -   [`singleton-representation?`](#go-to-the-SELECT_002dDEV_2236_2236SINGLETON_002dREPRESENTATION_003f-function)
        (function)

    -   [`traverse-representations`](#go-to-the-SELECT_002dDEV_2236_2236TRAVERSE_002dREPRESENTATIONS-macro)
        (macro)

**Internal Definitions**

:   -   [`canonical-range-end`](#go-to-the-SELECT_002dDEV_2236_2236CANONICAL_002dRANGE_002dEND-function)
        (function)

    -   [`(setf canonical-range-end)`](#go-to-the-SELECT_002dDEV_2236_2236_2768SETF-CANONICAL_002dRANGE_002dEND_2769-function)
        (function)

    -   [`canonical-range-p`](#go-to-the-SELECT_002dDEV_2236_2236CANONICAL_002dRANGE_002dP-function)
        (function)

    -   [`canonical-range-start`](#go-to-the-SELECT_002dDEV_2236_2236CANONICAL_002dRANGE_002dSTART-function)
        (function)

    -   [`(setf canonical-range-start)`](#go-to-the-SELECT_002dDEV_2236_2236_2768SETF-CANONICAL_002dRANGE_002dSTART_2769-function)
        (function)

    -   [`canonical-sequence-p`](#go-to-the-SELECT_002dDEV_2236_2236CANONICAL_002dSEQUENCE_002dP-function)
        (function)

    -   [`canonical-sequence-vector`](#go-to-the-SELECT_002dDEV_2236_2236CANONICAL_002dSEQUENCE_002dVECTOR-function)
        (function)

    -   [`(setf canonical-sequence-vector)`](#go-to-the-SELECT_002dDEV_2236_2236_2768SETF-CANONICAL_002dSEQUENCE_002dVECTOR_2769-function)
        (function)

    -   [`copy-canonical-range`](#go-to-the-SELECT_002dDEV_2236_2236COPY_002dCANONICAL_002dRANGE-function)
        (function)

    -   [`copy-canonical-sequence`](#go-to-the-SELECT_002dDEV_2236_2236COPY_002dCANONICAL_002dSEQUENCE-function)
        (function)

    -   [`make-canonical-range`](#go-to-the-SELECT_002dDEV_2236_2236MAKE_002dCANONICAL_002dRANGE-function)
        (function)

    -   [`make-canonical-sequence`](#go-to-the-SELECT_002dDEV_2236_2236MAKE_002dCANONICAL_002dSEQUENCE-function)
        (function)

    -   [`representation-dimension`](#go-to-the-SELECT_002dDEV_2236_2236REPRESENTATION_002dDIMENSION-function)
        (function)

    -   [`representation-initial-value`](#go-to-the-SELECT_002dDEV_2236_2236REPRESENTATION_002dINITIAL_002dVALUE-function)
        (function)

    -   [`representation-iterator`](#go-to-the-SELECT_002dDEV_2236_2236REPRESENTATION_002dITERATOR-function)
        (function)

## `select` {#The-select-package}

select

Package, select

SELECT is a facility for selecting portions of sequences or arrays.

**Source**

:   [`package.lisp`](#go-to-the-select_002fpackage_2024lisp-file) (file)

**Nickname**

:   `slct`

**Use List**

:   -   `let-plus`

    -   [`select-dev`](#go-to-the-SELECT_002dDEV-package)

    -   `anaphora`

    -   `alexandria`

    -   `common-lisp`

**Used By List**

:   -   `vglt`

    -   `lisp-stat`

    -   `data-frame`

    -   `num-utils.matrix`

**Exported Definitions**

:   -   [`head`](#go-to-the-SELECT_2236_2236HEAD-function) (function)

    -   [`including`](#go-to-the-SELECT_2236_2236INCLUDING-function)
        (function)

    -   [`including`](#go-to-the-SELECT_2236_2236INCLUDING-structure)
        (structure)

    -   [`mask`](#go-to-the-SELECT_2236_2236MASK-generic-function)
        (generic function)

    -   [`mask`](#go-to-the-SELECT_2236_2236MASK-COMMON_002dLISP_2236_2236SEQUENCE-COMMON_002dLISP_2236_2236T-method)
        (method)

    -   [`nodrop`](#go-to-the-SELECT_2236_2236NODROP-function)
        (function)

    -   [`nodrop`](#go-to-the-SELECT_2236_2236NODROP-structure)
        (structure)

    -   [`range`](#go-to-the-SELECT_2236_2236RANGE-function) (function)

    -   [`range`](#go-to-the-SELECT_2236_2236RANGE-structure)
        (structure)

    -   [`ref`](#go-to-the-SELECT_2236_2236REF-generic-function)
        (generic function)

    -   [`ref`](#go-to-the-SELECT_2236_2236REF-COMMON_002dLISP_2236_2236ARRAY-method)
        (method)

    -   [`(setf ref)`](#go-to-the-SELECT_2236_2236_2768SETF-REF_2769-COMMON_002dLISP_2236_2236T-COMMON_002dLISP_2236_2236ARRAY-method)
        (method)

    -   [`(setf ref)`](#go-to-the-SELECT_2236_2236_2768SETF-REF_2769-generic-function)
        (generic function)

    -   [`select`](#go-to-the-SELECT_2236_2236SELECT-generic-function)
        (generic function)

    -   [`select`](#go-to-the-SELECT_2236_2236SELECT-DATA_002dFRAME_2236_2236DATA_002dFRAME-method)
        (method)

    -   [`select`](#go-to-the-SELECT_2236_2236SELECT-DATA_002dFRAME_2236_2236DATA_002dVECTOR-method)
        (method)

    -   [`select`](#go-to-the-SELECT_2236_2236SELECT-DATA_002dFRAME_2236_2236ORDERED_002dKEYS-method)
        (method)

    -   [`select`](#go-to-the-SELECT_2236_2236SELECT-NUM_002dUTILS_2024MATRIX_2236_2236HERMITIAN_002dMATRIX-method)
        (method)

    -   [`select`](#go-to-the-SELECT_2236_2236SELECT-NUM_002dUTILS_2024MATRIX_2236_2236UPPER_002dTRIANGULAR_002dMATRIX-method)
        (method)

    -   [`select`](#go-to-the-SELECT_2236_2236SELECT-NUM_002dUTILS_2024MATRIX_2236_2236LOWER_002dTRIANGULAR_002dMATRIX-method)
        (method)

    -   [`select`](#go-to-the-SELECT_2236_2236SELECT-COMMON_002dLISP_2236_2236LIST-method)
        (method)

    -   [`select`](#go-to-the-SELECT_2236_2236SELECT-COMMON_002dLISP_2236_2236ARRAY-method)
        (method)

    -   [`(setf select)`](#go-to-the-SELECT_2236_2236_2768SETF-SELECT_2769-COMMON_002dLISP_2236_2236T-COMMON_002dLISP_2236_2236ARRAY-method)
        (method)

    -   [`(setf select)`](#go-to-the-SELECT_2236_2236_2768SETF-SELECT_2769-generic-function)
        (generic function)

    -   [`tail`](#go-to-the-SELECT_2236_2236TAIL-function) (function)

    -   [`which`](#go-to-the-SELECT_2236_2236WHICH-generic-function)
        (generic function)

    -   [`which`](#go-to-the-SELECT_2236_2236WHICH-COMMON_002dLISP_2236_2236SEQUENCE-method)
        (method)

**Internal Definitions**

:   -   [`copy-including`](#go-to-the-SELECT_2236_2236COPY_002dINCLUDING-function)
        (function)

    -   [`copy-nodrop`](#go-to-the-SELECT_2236_2236COPY_002dNODROP-function)
        (function)

    -   [`copy-range`](#go-to-the-SELECT_2236_2236COPY_002dRANGE-function)
        (function)

    -   [`including-end`](#go-to-the-SELECT_2236_2236INCLUDING_002dEND-function)
        (function)

    -   [`(setf including-end)`](#go-to-the-SELECT_2236_2236_2768SETF-INCLUDING_002dEND_2769-function)
        (function)

    -   [`including-p`](#go-to-the-SELECT_2236_2236INCLUDING_002dP-function)
        (function)

    -   [`including-start`](#go-to-the-SELECT_2236_2236INCLUDING_002dSTART-function)
        (function)

    -   [`(setf including-start)`](#go-to-the-SELECT_2236_2236_2768SETF-INCLUDING_002dSTART_2769-function)
        (function)

    -   [`make-including`](#go-to-the-SELECT_2236_2236MAKE_002dINCLUDING-function)
        (function)

    -   [`make-nodrop`](#go-to-the-SELECT_2236_2236MAKE_002dNODROP-function)
        (function)

    -   [`make-range`](#go-to-the-SELECT_2236_2236MAKE_002dRANGE-function)
        (function)

    -   [`nodrop-index`](#go-to-the-SELECT_2236_2236NODROP_002dINDEX-function)
        (function)

    -   [`(setf nodrop-index)`](#go-to-the-SELECT_2236_2236_2768SETF-NODROP_002dINDEX_2769-function)
        (function)

    -   [`nodrop-p`](#go-to-the-SELECT_2236_2236NODROP_002dP-function)
        (function)

    -   [`range-end`](#go-to-the-SELECT_2236_2236RANGE_002dEND-function)
        (function)

    -   [`(setf range-end)`](#go-to-the-SELECT_2236_2236_2768SETF-RANGE_002dEND_2769-function)
        (function)

    -   [`range-p`](#go-to-the-SELECT_2236_2236RANGE_002dP-function)
        (function)

    -   [`range-start`](#go-to-the-SELECT_2236_2236RANGE_002dSTART-function)
        (function)

    -   [`(setf range-start)`](#go-to-the-SELECT_2236_2236_2768SETF-RANGE_002dSTART_2769-function)
        (function)

# Definitions {#Definitions}

Definitions are sorted by export status, category, package, and then by
lexicographic order.

## Exported definitions {#Exported-definitions}

### Macros {#Exported-macros}

traverse-representations

Macro

:

traverse-representations

(

SUBSCRIPTS

REPRESENTATIONS

&key

INDEX

SETUP

)

&body

BODY

> Macro, traverse-representations
>
> Loops over all possible subscripts in REPRESENTAITONS, making them
> available in SUBSCRIPTS during the execution of BODY. The iterator is
> constructed using the function SETUP (see for example
> ROW-MAJOR-SETUP). When INDEX is given, a variable with that name is
> provided, containing an index that counts iterations.
>
> **Package**
>
> :   [`select-dev`](#go-to-the-SELECT_002dDEV-package)
>
> **Source**
>
> :   [`select-dev.lisp`](#go-to-the-select_002fselect_002ddev_2024lisp-file)
>     (file)

### Functions {#Exported-functions}

all-singleton-representations?

Function

:

all-singleton-representations?

REPRESENTATIONS

> Function, all-singleton-representations?
>
> Test if all canonical representations are singletons.
>
> **Package**
>
> :   [`select-dev`](#go-to-the-SELECT_002dDEV-package)
>
> **Source**
>
> :   [`select-dev.lisp`](#go-to-the-select_002fselect_002ddev_2024lisp-file)
>     (file)

canonical-range

Function

:

canonical-range

START

END

> Function, canonical-range
>
> Canonical representation of a contiguous set of array indices from
> START (inclusive) to END (exclusive).
>
> **Package**
>
> :   [`select-dev`](#go-to-the-SELECT_002dDEV-package)
>
> **Source**
>
> :   [`select-dev.lisp`](#go-to-the-select_002fselect_002ddev_2024lisp-file)
>     (file)

canonical-representations

Function

:

canonical-representations

AXES

SELECTIONS

> Function, canonical-representations
>
> Return the canonical representations of SELECTIONS given the
> corresponding AXES, checking for matching length.
>
> **Package**
>
> :   [`select-dev`](#go-to-the-SELECT_002dDEV-package)
>
> **Source**
>
> :   [`select-dev.lisp`](#go-to-the-select_002fselect_002ddev_2024lisp-file)
>     (file)

canonical-sequence

Function

:

canonical-sequence

SEQUENCE

> Function, canonical-sequence
>
> Canonical representation of array indexes from canonical-sequence
> SEQUENCE.
>
> May share structure. Vectors of the upgraded type of (SIMPLE-ARRAY
> ARRAY-INDEX (\*)) are preferred for efficiency, otherwise they are
> coerced.
>
> **Package**
>
> :   [`select-dev`](#go-to-the-SELECT_002dDEV-package)
>
> **Source**
>
> :   [`select-dev.lisp`](#go-to-the-select_002fselect_002ddev_2024lisp-file)
>     (file)

canonical-singleton

Function

:

canonical-singleton

INDEX

> Function, canonical-singleton
>
> Canonical representation of a singleton index (a nonnegative integer,
> which is a valid array index).
>
> **Package**
>
> :   [`select-dev`](#go-to-the-SELECT_002dDEV-package)
>
> **Source**
>
> :   [`select-dev.lisp`](#go-to-the-select_002fselect_002ddev_2024lisp-file)
>     (file)

column-major-setup

Function

:

column-major-setup

REPRESENTATIONS

TERMINATOR

> Function, column-major-setup
>
> Return SUBSCRIPTS (a list) and ITERATOR (a closure, no arguments) that
> increments the contents of SUBSCRIPTS in column-major order.
> TERMINATOR is called when all subscripts have been visited.
>
> **Package**
>
> :   [`select-dev`](#go-to-the-SELECT_002dDEV-package)
>
> **Source**
>
> :   [`select-dev.lisp`](#go-to-the-select_002fselect_002ddev_2024lisp-file)
>     (file)

head

Function

:

head

COUNT

> Function, head
>
> First COUNT indexes.
>
> **Package**
>
> :   [`select`](#go-to-the-SELECT-package)
>
> **Source**
>
> :   [`select.lisp`](#go-to-the-select_002fselect_2024lisp-file) (file)

including

Function

:

including

START

END

> Function, including
>
> Range, including both ends.
>
> **Package**
>
> :   [`select`](#go-to-the-SELECT-package)
>
> **Source**
>
> :   [`select.lisp`](#go-to-the-select_002fselect_2024lisp-file) (file)

nodrop

Function

:

nodrop

INDEX

> Function, nodrop
>
> Select a single index, but do not drop a dimension.
>
> **Package**
>
> :   [`select`](#go-to-the-SELECT-package)
>
> **Source**
>
> :   [`select.lisp`](#go-to-the-select_002fselect_2024lisp-file) (file)

range

Function

:

range

START

END

> Function, range
>
> Range, including START, excluding END.
>
> **Package**
>
> :   [`select`](#go-to-the-SELECT-package)
>
> **Source**
>
> :   [`select.lisp`](#go-to-the-select_002fselect_2024lisp-file) (file)

representation-dimensions

Function

:

representation-dimensions

REPRESENTATIONS

> Function, representation-dimensions
>
> Return a list for the dimensions of canonical representations,
> dropping singletons.
>
> **Package**
>
> :   [`select-dev`](#go-to-the-SELECT_002dDEV-package)
>
> **Source**
>
> :   [`select-dev.lisp`](#go-to-the-select_002fselect_002ddev_2024lisp-file)
>     (file)

row-major-setup

Function

:

row-major-setup

REPRESENTATIONS

TERMINATOR

> Function, row-major-setup
>
> Return SUBSCRIPTS (a list) and ITERATOR (a closure, no arguments) that
> increments the contents of SUBSCRIPTS in row-major order. TERMINATOR
> is called when all subscripts have been visited.
>
> **Package**
>
> :   [`select-dev`](#go-to-the-SELECT_002dDEV-package)
>
> **Source**
>
> :   [`select-dev.lisp`](#go-to-the-select_002fselect_002ddev_2024lisp-file)
>     (file)

select-reserved-symbol?

Function

:

select-reserved-symbol?

SYMBOL

> Function, select-reserved-symbol?
>
> Test if SYMBOL has special semantics for SELECTION.
>
> **Package**
>
> :   [`select-dev`](#go-to-the-SELECT_002dDEV-package)
>
> **Source**
>
> :   [`select-dev.lisp`](#go-to-the-select_002fselect_002ddev_2024lisp-file)
>     (file)

singleton-representation?

Function

:

singleton-representation?

REPRESENTATION

> Function, singleton-representation?
>
> Test if a canonical REPRESENTATION is a singleton.
>
> **Package**
>
> :   [`select-dev`](#go-to-the-SELECT_002dDEV-package)
>
> **Source**
>
> :   [`select-dev.lisp`](#go-to-the-select_002fselect_002ddev_2024lisp-file)
>     (file)

tail

Function

:

tail

COUNT

> Function, tail
>
> Last COUNT indexes.
>
> **Package**
>
> :   [`select`](#go-to-the-SELECT-package)
>
> **Source**
>
> :   [`select.lisp`](#go-to-the-select_002fselect_2024lisp-file) (file)

### Generic functions {#Exported-generic-functions}

axis-dimension

Generic Function

:

axis-dimension

AXIS

> Generic Function, axis-dimension
>
> Return the dimension of axis. Needs to be defined for non-integer
> axes.
>
> **Package**
>
> :   [`select-dev`](#go-to-the-SELECT_002dDEV-package)
>
> **Source**
>
> :   [`select-dev.lisp`](#go-to-the-select_002fselect_002ddev_2024lisp-file)
>     (file)
>
> **Methods**
> :   axis-dimension
>     Method
>     :
>     axis-dimension
>     (
>     AXIS
>     ordered-keys
>     )
>     > Method, axis-dimension
>     >
>     > **Source**
>     >
>     > :   [ignore](file://s:/src/data-frame/data-frame.lisp)

canonical-representation

Generic Function

:

canonical-representation

AXIS

SELECTION

> Generic Function, canonical-representation
>
> Canonical representation of SELECTION, given information in AXIS. The
> default methods use dimensions as AXIS.
>
> Each selection needs to be resolved into a canonical representation,
> which is either a singleton, a range, or a sequence of subscripts.
> They should only be constructed with the corresponding
> CANONICAL-SINGLETION, CANONICAL-RANGE and CANONICAL-SEQUENCE
> functions.
>
> \@c(CANONICAL-REPRESENTATION) needs to ensure that the represented
> subscripts are valid for the axis.
>
> Unless a specialized method is found, the dimension of the axis is
> queried with AXIS-DIMENSION and resolution is attempted using the
> latter. Methods that resolve symbols should test them with
> SELECT-RESERVED-SYMBOL? and use CALL-NEXT-METHOD.
>
> **Package**
>
> :   [`select-dev`](#go-to-the-SELECT_002dDEV-package)
>
> **Source**
>
> :   [`select-dev.lisp`](#go-to-the-select_002fselect_002ddev_2024lisp-file)
>     (file)
>
> **Methods**
> :   canonical-representation
>     Method
>     :
>     canonical-representation
>     (
>     AXIS
>     ordered-keys
>     ) (
>     SLICE
>     symbol
>     )
>     > Method, canonical-representation
>     >
>     > **Source**
>     >
>     > :   [ignore](file://s:/src/data-frame/data-frame.lisp)
>
>     canonical-representation
>     Method
>     :
>     canonical-representation
>     AXIS
>     (
>     SELECTION
>     nodrop
>     )
>     > Method, canonical-representation
>     > The canonical representation for NODROP.
>     >
>     > **Source**
>     >
>     > :   [`select.lisp`](#go-to-the-select_002fselect_2024lisp-file)
>     >     (file)
>
>     canonical-representation
>     Method
>     :
>     canonical-representation
>     AXIS
>     (
>     SELECTION
>     range
>     )
>     > Method, canonical-representation
>     > The canonical representation for RANGE.
>     >
>     > **Source**
>     >
>     > :   [`select.lisp`](#go-to-the-select_002fselect_2024lisp-file)
>     >     (file)
>
>     canonical-representation
>     Method
>     :
>     canonical-representation
>     AXIS
>     (
>     SELECTION
>     including
>     )
>     > Method, canonical-representation
>     > The canonical representation for INCLUDING.
>     >
>     > **Source**
>     >
>     > :   [`select.lisp`](#go-to-the-select_002fselect_2024lisp-file)
>     >     (file)
>
>     canonical-representation
>     Method
>     :
>     canonical-representation
>     AXIS
>     SELECTION
>     > Method, canonical-representation
>
>     canonical-representation
>     Method
>     :
>     canonical-representation
>     AXIS
>     (
>     CANONICAL-RANGE
>     canonical-range
>     )
>     > Method, canonical-representation
>
>     canonical-representation
>     Method
>     :
>     canonical-representation
>     AXIS
>     (
>     CANONICAL-SEQUENCE
>     canonical-sequence
>     )
>     > Method, canonical-representation
>
>     canonical-representation
>     Method
>     :
>     canonical-representation
>     (
>     AXIS
>     integer
>     ) (
>     SLICE
>     null
>     )
>     > Method, canonical-representation
>
>     canonical-representation
>     Method
>     :
>     canonical-representation
>     (
>     AXIS
>     integer
>     ) (
>     SELECTION
>     integer
>     )
>     > Method, canonical-representation
>
>     canonical-representation
>     Method
>     :
>     canonical-representation
>     AXIS
>     (
>     SELECTION
>     sequence
>     )
>     > Method, canonical-representation
>
>     canonical-representation
>     Method
>     :
>     canonical-representation
>     (
>     AXIS
>     integer
>     ) (
>     SELECTION
>     (eql t)
>     )
>     > Method, canonical-representation
>
>     canonical-representation
>     Method
>     :
>     canonical-representation
>     AXIS
>     (
>     SELECTION
>     bit-vector
>     )
>     > Method, canonical-representation

mask

Generic Function

:

mask

SEQUENCE

PREDICATE

> Generic Function, mask
>
> Map sequence into a simple-bit-vector, using 1 when PREDICATE yields
> true, 0 otherwise.
>
> **Package**
>
> :   [`select`](#go-to-the-SELECT-package)
>
> **Source**
>
> :   [`select.lisp`](#go-to-the-select_002fselect_2024lisp-file) (file)
>
> **Methods**
> :   mask
>     Method
>     :
>     mask
>     (
>     SEQUENCE
>     sequence
>     )
>     PREDICATE
>     > Method, mask

ref

Generic Function

:

ref

OBJECT

&rest

SUBSCRIPTS

> Generic Function, ref
>
> Return the element of OBJECT specified by SUBSCRIPTS.
>
> **Package**
>
> :   [`select`](#go-to-the-SELECT-package)
>
> **Source**
>
> :   [`select.lisp`](#go-to-the-select_002fselect_2024lisp-file) (file)
>
> **Writer**
>
> :   [`(setf ref)`](#go-to-the-SELECT_2236_2236_2768SETF-REF_2769-generic-function)
>     (generic function)
>
> **Methods**
> :   ref
>     Method
>     :
>     ref
>     (
>     ARRAY
>     array
>     )
>     &rest
>     SUBSCRIPTS
>     > Method, ref

(setf ref)

Generic Function

:

(setf ref)

VALUE

OBJECT

&rest

SUBSCRIPTS

> Generic Function, (setf ref)
>
> Stores VALUE into the place specified by SUBSCRIPTS.
>
> **Package**
>
> :   [`select`](#go-to-the-SELECT-package)
>
> **Source**
>
> :   [`select.lisp`](#go-to-the-select_002fselect_2024lisp-file) (file)
>
> **Reader**
>
> :   [`ref`](#go-to-the-SELECT_2236_2236REF-generic-function) (generic
>     function)
>
> **Methods**
> :   (setf ref)
>     Method
>     :
>     (setf ref)
>     VALUE
>     (
>     ARRAY
>     array
>     )
>     &rest
>     SUBSCRIPTS
>     > Method, (setf ref)

select

Generic Function

:

select

OBJECT

&rest

SELECTIONS

> Generic Function, select
>
> Return the slices of OBJECT specified by SELECTIONS.
>
> **Package**
>
> :   [`select`](#go-to-the-SELECT-package)
>
> **Source**
>
> :   [`select.lisp`](#go-to-the-select_002fselect_2024lisp-file) (file)
>
> **Writer**
>
> :   [`(setf select)`](#go-to-the-SELECT_2236_2236_2768SETF-SELECT_2769-generic-function)
>     (generic function)
>
> **Methods**
> :   select
>     Method
>     :
>     select
>     (
>     DATA-FRAME
>     data-frame
>     )
>     &rest
>     SLICES
>     > Method, select
>     >
>     > **Source**
>     >
>     > :   [ignore](file://s:/src/data-frame/data-frame.lisp)
>
>     select
>     Method
>     :
>     select
>     (
>     DATA-VECTOR
>     data-vector
>     )
>     &rest
>     SLICES
>     > Method, select
>     >
>     > **Source**
>     >
>     > :   [ignore](file://s:/src/data-frame/data-frame.lisp)
>
>     select
>     Method
>     :
>     select
>     (
>     ORDERED-KEYS
>     ordered-keys
>     )
>     &rest
>     SELECTIONS
>     > Method, select
>     >
>     > **Source**
>     >
>     > :   [ignore](file://s:/src/data-frame/data-frame.lisp)
>
>     select
>     Method
>     :
>     select
>     (
>     MATRIX0
>     hermitian-matrix
>     )
>     &rest
>     SLICES
>     > Method, select
>     >
>     > **Source**
>     >
>     > :   [ignore](file://s:/src/num-utils/src/matrix.lisp)
>
>     select
>     Method
>     :
>     select
>     (
>     MATRIX0
>     upper-triangular-matrix
>     )
>     &rest
>     SLICES
>     > Method, select
>     >
>     > **Source**
>     >
>     > :   [ignore](file://s:/src/num-utils/src/matrix.lisp)
>
>     select
>     Method
>     :
>     select
>     (
>     MATRIX0
>     lower-triangular-matrix
>     )
>     &rest
>     SLICES
>     > Method, select
>     >
>     > **Source**
>     >
>     > :   [ignore](file://s:/src/num-utils/src/matrix.lisp)
>
>     select
>     Method
>     :
>     select
>     (
>     LST
>     list
>     )
>     &rest
>     SELECTIONS
>     > Method, select
>     > Select from LST the subscripts or range specified in SELECTIONS.
>     > SELECTIONS must be a VECTOR, LIST or RANGE.
>
>     select
>     Method
>     :
>     select
>     (
>     ARRAY
>     array
>     )
>     &rest
>     SELECTIONS
>     > Method, select
>     > Return the SELECTIONS in the given ARRAY.

(setf select)

Generic Function

:

(setf select)

VALUE

OBJECT

&rest

SELECTIONS

> Generic Function, (setf select)
>
> Stores VALUES into the locations given by SELECTIONS.
>
> **Package**
>
> :   [`select`](#go-to-the-SELECT-package)
>
> **Source**
>
> :   [`select.lisp`](#go-to-the-select_002fselect_2024lisp-file) (file)
>
> **Reader**
>
> :   [`select`](#go-to-the-SELECT_2236_2236SELECT-generic-function)
>     (generic function)
>
> **Methods**
> :   (setf select)
>     Method
>     :
>     (setf select)
>     VALUE
>     (
>     ARRAY
>     array
>     )
>     &rest
>     SELECTIONS
>     > Method, (setf select)

which

Generic Function

:

which

SEQUENCE

&key

PREDICATE

> Generic Function, which
>
> Return an index of the positions in SEQUENCE which satisfy PREDICATE.
> Defaults to return non-NIL indices.
>
> **Package**
>
> :   [`select`](#go-to-the-SELECT-package)
>
> **Source**
>
> :   [`select.lisp`](#go-to-the-select_002fselect_2024lisp-file) (file)
>
> **Methods**
> :   which
>     Method
>     :
>     which
>     (
>     SEQUENCE
>     sequence
>     )
>     &key
>     PREDICATE
>     > Method, which

### Structures {#Exported-structures}

canonical-range

Structure

:

canonical-range

()

> Structure, canonical-range
>
> Canonical representation of a contiguous set of array indices from
> START (inclusive) to END (exclusive).
>
> **Package**
>
> :   [`select-dev`](#go-to-the-SELECT_002dDEV-package)
>
> **Source**
>
> :   [`select-dev.lisp`](#go-to-the-select_002fselect_002ddev_2024lisp-file)
>     (file)
>
> **Direct superclasses**
>
> :   `structure-object` (structure)
>
> **Direct methods**
>
> :   [`canonical-representation`](#go-to-the-SELECT_002dDEV_2236_2236CANONICAL_002dREPRESENTATION-COMMON_002dLISP_2236_2236T-SELECT_002dDEV_2236_2236CANONICAL_002dRANGE-method)
>     (method)
>
> **Direct slots**
> :   start
>     Slot
>     :
>     start
>     > Slot, start
>     >
>     > **Type**
>     >
>     > :   `alexandria:array-index`
>     >
>     > **Readers**
>     >
>     > :   [`canonical-range-start`](#go-to-the-SELECT_002dDEV_2236_2236CANONICAL_002dRANGE_002dSTART-function)
>     >     (function)
>     >
>     > **Writers**
>     >
>     > :   [`(setf canonical-range-start)`](#go-to-the-SELECT_002dDEV_2236_2236_2768SETF-CANONICAL_002dRANGE_002dSTART_2769-function)
>     >     (function)
>
>     end
>     Slot
>     :
>     end
>     > Slot, end
>     >
>     > **Type**
>     >
>     > :   `alexandria:array-index`
>     >
>     > **Readers**
>     >
>     > :   [`canonical-range-end`](#go-to-the-SELECT_002dDEV_2236_2236CANONICAL_002dRANGE_002dEND-function)
>     >     (function)
>     >
>     > **Writers**
>     >
>     > :   [`(setf canonical-range-end)`](#go-to-the-SELECT_002dDEV_2236_2236_2768SETF-CANONICAL_002dRANGE_002dEND_2769-function)
>     >     (function)

canonical-sequence

Structure

:

canonical-sequence

()

> Structure, canonical-sequence
>
> Canonical representation of a sequence of array indexes.
>
> **Package**
>
> :   [`select-dev`](#go-to-the-SELECT_002dDEV-package)
>
> **Source**
>
> :   [`select-dev.lisp`](#go-to-the-select_002fselect_002ddev_2024lisp-file)
>     (file)
>
> **Direct superclasses**
>
> :   `structure-object` (structure)
>
> **Direct methods**
>
> :   [`canonical-representation`](#go-to-the-SELECT_002dDEV_2236_2236CANONICAL_002dREPRESENTATION-COMMON_002dLISP_2236_2236T-SELECT_002dDEV_2236_2236CANONICAL_002dSEQUENCE-method)
>     (method)
>
> **Direct slots**
> :   vector
>     Slot
>     :
>     vector
>     > Slot, vector
>     >
>     > **Type**
>     >
>     > :   `(simple-array alexandria:array-index (*))`
>     >
>     > **Readers**
>     >
>     > :   [`canonical-sequence-vector`](#go-to-the-SELECT_002dDEV_2236_2236CANONICAL_002dSEQUENCE_002dVECTOR-function)
>     >     (function)
>     >
>     > **Writers**
>     >
>     > :   [`(setf canonical-sequence-vector)`](#go-to-the-SELECT_002dDEV_2236_2236_2768SETF-CANONICAL_002dSEQUENCE_002dVECTOR_2769-function)
>     >     (function)

including

Structure

:

including

()

> Structure, including
>
> Range, including both ends.
>
> **Package**
>
> :   [`select`](#go-to-the-SELECT-package)
>
> **Source**
>
> :   [`select.lisp`](#go-to-the-select_002fselect_2024lisp-file) (file)
>
> **Direct superclasses**
>
> :   `structure-object` (structure)
>
> **Direct methods**
>
> :   [`canonical-representation`](#go-to-the-SELECT_002dDEV_2236_2236CANONICAL_002dREPRESENTATION-COMMON_002dLISP_2236_2236T-SELECT_2236_2236INCLUDING-method)
>     (method)
>
> **Direct slots**
> :   start
>     Slot
>     :
>     start
>     > Slot, start
>     >
>     > **Readers**
>     >
>     > :   [`including-start`](#go-to-the-SELECT_2236_2236INCLUDING_002dSTART-function)
>     >     (function)
>     >
>     > **Writers**
>     >
>     > :   [`(setf including-start)`](#go-to-the-SELECT_2236_2236_2768SETF-INCLUDING_002dSTART_2769-function)
>     >     (function)
>
>     end
>     Slot
>     :
>     end
>     > Slot, end
>     >
>     > **Readers**
>     >
>     > :   [`including-end`](#go-to-the-SELECT_2236_2236INCLUDING_002dEND-function)
>     >     (function)
>     >
>     > **Writers**
>     >
>     > :   [`(setf including-end)`](#go-to-the-SELECT_2236_2236_2768SETF-INCLUDING_002dEND_2769-function)
>     >     (function)

nodrop

Structure

:

nodrop

()

> Structure, nodrop
>
> Select a single index, but don't drop a dimension.
>
> **Package**
>
> :   [`select`](#go-to-the-SELECT-package)
>
> **Source**
>
> :   [`select.lisp`](#go-to-the-select_002fselect_2024lisp-file) (file)
>
> **Direct superclasses**
>
> :   `structure-object` (structure)
>
> **Direct methods**
>
> :   [`canonical-representation`](#go-to-the-SELECT_002dDEV_2236_2236CANONICAL_002dREPRESENTATION-COMMON_002dLISP_2236_2236T-SELECT_2236_2236NODROP-method)
>     (method)
>
> **Direct slots**
> :   index
>     Slot
>     :
>     index
>     > Slot, index
>     >
>     > **Readers**
>     >
>     > :   [`nodrop-index`](#go-to-the-SELECT_2236_2236NODROP_002dINDEX-function)
>     >     (function)
>     >
>     > **Writers**
>     >
>     > :   [`(setf nodrop-index)`](#go-to-the-SELECT_2236_2236_2768SETF-NODROP_002dINDEX_2769-function)
>     >     (function)

range

Structure

:

range

()

> Structure, range
>
> Range, including start, excluding end.
>
> **Package**
>
> :   [`select`](#go-to-the-SELECT-package)
>
> **Source**
>
> :   [`select.lisp`](#go-to-the-select_002fselect_2024lisp-file) (file)
>
> **Direct superclasses**
>
> :   `structure-object` (structure)
>
> **Direct methods**
>
> :   [`canonical-representation`](#go-to-the-SELECT_002dDEV_2236_2236CANONICAL_002dREPRESENTATION-COMMON_002dLISP_2236_2236T-SELECT_2236_2236RANGE-method)
>     (method)
>
> **Direct slots**
> :   start
>     Slot
>     :
>     start
>     > Slot, start
>     >
>     > **Readers**
>     >
>     > :   [`range-start`](#go-to-the-SELECT_2236_2236RANGE_002dSTART-function)
>     >     (function)
>     >
>     > **Writers**
>     >
>     > :   [`(setf range-start)`](#go-to-the-SELECT_2236_2236_2768SETF-RANGE_002dSTART_2769-function)
>     >     (function)
>
>     end
>     Slot
>     :
>     end
>     > Slot, end
>     >
>     > **Readers**
>     >
>     > :   [`range-end`](#go-to-the-SELECT_2236_2236RANGE_002dEND-function)
>     >     (function)
>     >
>     > **Writers**
>     >
>     > :   [`(setf range-end)`](#go-to-the-SELECT_2236_2236_2768SETF-RANGE_002dEND_2769-function)
>     >     (function)

## Internal definitions {#Internal-definitions}

### Functions {#Internal-functions}

canonical-range-end

Function

:

canonical-range-end

INSTANCE

Function, canonical-range-end

(setf canonical-range-end)

Function

:

(setf canonical-range-end)

VALUE

INSTANCE

> Function, (setf canonical-range-end)
>
> **Package**
>
> :   [`select-dev`](#go-to-the-SELECT_002dDEV-package)
>
> **Source**
>
> :   [`select-dev.lisp`](#go-to-the-select_002fselect_002ddev_2024lisp-file)
>     (file)

canonical-range-p

Function

:

canonical-range-p

OBJECT

> Function, canonical-range-p
>
> **Package**
>
> :   [`select-dev`](#go-to-the-SELECT_002dDEV-package)
>
> **Source**
>
> :   [`select-dev.lisp`](#go-to-the-select_002fselect_002ddev_2024lisp-file)
>     (file)

canonical-range-start

Function

:

canonical-range-start

INSTANCE

Function, canonical-range-start

(setf canonical-range-start)

Function

:

(setf canonical-range-start)

VALUE

INSTANCE

> Function, (setf canonical-range-start)
>
> **Package**
>
> :   [`select-dev`](#go-to-the-SELECT_002dDEV-package)
>
> **Source**
>
> :   [`select-dev.lisp`](#go-to-the-select_002fselect_002ddev_2024lisp-file)
>     (file)

canonical-sequence-p

Function

:

canonical-sequence-p

OBJECT

> Function, canonical-sequence-p
>
> **Package**
>
> :   [`select-dev`](#go-to-the-SELECT_002dDEV-package)
>
> **Source**
>
> :   [`select-dev.lisp`](#go-to-the-select_002fselect_002ddev_2024lisp-file)
>     (file)

canonical-sequence-vector

Function

:

canonical-sequence-vector

INSTANCE

Function, canonical-sequence-vector

(setf canonical-sequence-vector)

Function

:

(setf canonical-sequence-vector)

VALUE

INSTANCE

> Function, (setf canonical-sequence-vector)
>
> **Package**
>
> :   [`select-dev`](#go-to-the-SELECT_002dDEV-package)
>
> **Source**
>
> :   [`select-dev.lisp`](#go-to-the-select_002fselect_002ddev_2024lisp-file)
>     (file)

copy-canonical-range

Function

:

copy-canonical-range

INSTANCE

> Function, copy-canonical-range
>
> **Package**
>
> :   [`select-dev`](#go-to-the-SELECT_002dDEV-package)
>
> **Source**
>
> :   [`select-dev.lisp`](#go-to-the-select_002fselect_002ddev_2024lisp-file)
>     (file)

copy-canonical-sequence

Function

:

copy-canonical-sequence

INSTANCE

> Function, copy-canonical-sequence
>
> **Package**
>
> :   [`select-dev`](#go-to-the-SELECT_002dDEV-package)
>
> **Source**
>
> :   [`select-dev.lisp`](#go-to-the-select_002fselect_002ddev_2024lisp-file)
>     (file)

copy-including

Function

:

copy-including

INSTANCE

> Function, copy-including
>
> **Package**
>
> :   [`select`](#go-to-the-SELECT-package)
>
> **Source**
>
> :   [`select.lisp`](#go-to-the-select_002fselect_2024lisp-file) (file)

copy-nodrop

Function

:

copy-nodrop

INSTANCE

> Function, copy-nodrop
>
> **Package**
>
> :   [`select`](#go-to-the-SELECT-package)
>
> **Source**
>
> :   [`select.lisp`](#go-to-the-select_002fselect_2024lisp-file) (file)

copy-range

Function

:

copy-range

INSTANCE

> Function, copy-range
>
> **Package**
>
> :   [`select`](#go-to-the-SELECT-package)
>
> **Source**
>
> :   [`select.lisp`](#go-to-the-select_002fselect_2024lisp-file) (file)

including-end

Function

:

including-end

INSTANCE

Function, including-end

(setf including-end)

Function

:

(setf including-end)

VALUE

INSTANCE

> Function, (setf including-end)
>
> **Package**
>
> :   [`select`](#go-to-the-SELECT-package)
>
> **Source**
>
> :   [`select.lisp`](#go-to-the-select_002fselect_2024lisp-file) (file)

including-p

Function

:

including-p

OBJECT

> Function, including-p
>
> **Package**
>
> :   [`select`](#go-to-the-SELECT-package)
>
> **Source**
>
> :   [`select.lisp`](#go-to-the-select_002fselect_2024lisp-file) (file)

including-start

Function

:

including-start

INSTANCE

Function, including-start

(setf including-start)

Function

:

(setf including-start)

VALUE

INSTANCE

> Function, (setf including-start)
>
> **Package**
>
> :   [`select`](#go-to-the-SELECT-package)
>
> **Source**
>
> :   [`select.lisp`](#go-to-the-select_002fselect_2024lisp-file) (file)

make-canonical-range

Function

:

make-canonical-range

&key

(

START

START

) (

END

END

)

> Function, make-canonical-range
>
> **Package**
>
> :   [`select-dev`](#go-to-the-SELECT_002dDEV-package)
>
> **Source**
>
> :   [`select-dev.lisp`](#go-to-the-select_002fselect_002ddev_2024lisp-file)
>     (file)

make-canonical-sequence

Function

:

make-canonical-sequence

&key

(

VECTOR

VECTOR

)

> Function, make-canonical-sequence
>
> **Package**
>
> :   [`select-dev`](#go-to-the-SELECT_002dDEV-package)
>
> **Source**
>
> :   [`select-dev.lisp`](#go-to-the-select_002fselect_002ddev_2024lisp-file)
>     (file)

make-including

Function

:

make-including

&key

(

START

START

) (

END

END

)

> Function, make-including
>
> **Package**
>
> :   [`select`](#go-to-the-SELECT-package)
>
> **Source**
>
> :   [`select.lisp`](#go-to-the-select_002fselect_2024lisp-file) (file)

make-nodrop

Function

:

make-nodrop

&key

(

INDEX

INDEX

)

> Function, make-nodrop
>
> **Package**
>
> :   [`select`](#go-to-the-SELECT-package)
>
> **Source**
>
> :   [`select.lisp`](#go-to-the-select_002fselect_2024lisp-file) (file)

make-range

Function

:

make-range

&key

(

START

START

) (

END

END

)

> Function, make-range
>
> **Package**
>
> :   [`select`](#go-to-the-SELECT-package)
>
> **Source**
>
> :   [`select.lisp`](#go-to-the-select_002fselect_2024lisp-file) (file)

nodrop-index

Function

:

nodrop-index

INSTANCE

Function, nodrop-index

(setf nodrop-index)

Function

:

(setf nodrop-index)

VALUE

INSTANCE

> Function, (setf nodrop-index)
>
> **Package**
>
> :   [`select`](#go-to-the-SELECT-package)
>
> **Source**
>
> :   [`select.lisp`](#go-to-the-select_002fselect_2024lisp-file) (file)

nodrop-p

Function

:

nodrop-p

OBJECT

> Function, nodrop-p
>
> **Package**
>
> :   [`select`](#go-to-the-SELECT-package)
>
> **Source**
>
> :   [`select.lisp`](#go-to-the-select_002fselect_2024lisp-file) (file)

range-end

Function

:

range-end

INSTANCE

Function, range-end

(setf range-end)

Function

:

(setf range-end)

VALUE

INSTANCE

> Function, (setf range-end)
>
> **Package**
>
> :   [`select`](#go-to-the-SELECT-package)
>
> **Source**
>
> :   [`select.lisp`](#go-to-the-select_002fselect_2024lisp-file) (file)

range-p

Function

:

range-p

OBJECT

> Function, range-p
>
> **Package**
>
> :   [`select`](#go-to-the-SELECT-package)
>
> **Source**
>
> :   [`select.lisp`](#go-to-the-select_002fselect_2024lisp-file) (file)

range-start

Function

:

range-start

INSTANCE

Function, range-start

(setf range-start)

Function

:

(setf range-start)

VALUE

INSTANCE

> Function, (setf range-start)
>
> **Package**
>
> :   [`select`](#go-to-the-SELECT-package)
>
> **Source**
>
> :   [`select.lisp`](#go-to-the-select_002fselect_2024lisp-file) (file)

representation-dimension

Function

:

representation-dimension

REPRESENTATION

> Function, representation-dimension
>
> Return the dimension of a canonical-representation, or NIL for
> singleton selections (they are dropped).
>
> **Package**
>
> :   [`select-dev`](#go-to-the-SELECT_002dDEV-package)
>
> **Source**
>
> :   [`select-dev.lisp`](#go-to-the-select_002fselect_002ddev_2024lisp-file)
>     (file)

representation-initial-value

Function

:

representation-initial-value

REPRESENTATION

> Function, representation-initial-value
>
> Initial value for iteration.
>
> **Package**
>
> :   [`select-dev`](#go-to-the-SELECT_002dDEV-package)
>
> **Source**
>
> :   [`select-dev.lisp`](#go-to-the-select_002fselect_002ddev_2024lisp-file)
>     (file)

representation-iterator

Function

:

representation-iterator

REPRESENTATION

CARRY

CONS

> Function, representation-iterator
>
> Return a closure that sets the car of CONS to the next value each time
> it is called, resetting and calling CARRY when it reaches the end of
> its range.
>
> **Package**
>
> :   [`select-dev`](#go-to-the-SELECT_002dDEV-package)
>
> **Source**
>
> :   [`select-dev.lisp`](#go-to-the-select_002fselect_002ddev_2024lisp-file)
>     (file)

# Indexes {#Indexes}

## Concepts {#Concept-index}

## Functions {#Function-index}

## Variables {#Variable-index}

## Data types {#Data-type-index}
