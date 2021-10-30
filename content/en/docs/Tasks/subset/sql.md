---
title: "SQLDF"
linkTitle: "SQL"
author: ["Steven Nunez"]
date: 2021-03-07
weight: 5
description: >
  Selecting subsets of data using SQL
---

## Overview {#Overview}

`sqldf` is a library for querying data in a lisp `data-frame` using
SQL, optimised for memory consumption.  Any query that can be done in
SQL can also be done in the API, but since SQL is widely known, many
developers find it more convenient to use SQL.

To use SQL to query a data frame, the developer uses the `sqldf`
function, using the data frame name (converted to SQL identifier
format) in place of the table name.  `sqldf` will automatically create
an in-memory SQLite database, copy the contents of the data frame to
it, perform the query, return the results as a new data frame and
delete the database.  We have tested this with data frames of 350K
rows and there is no noticeable difference in performance compared to
API based queries.

See the [cl-sqlite](https://common-lisp.net/project/cl-sqlite/)
documentation for additional functionality provided by the SQLite
library. You can create databases, employ multiple persistent
connections, use prepared statements, etc. with the underlying
library. `sqldf` is a thin layer for moving data to/from
`data-frames`.


## Basic Usage

`sqldf` requires the sqlite shared library from the [SQLite
project](https://sqlite.org/index.html). It may also be available via
your operating systems package manager.

{{< alert title="Note" >}}SQLDF relies on
[CFFI](https://common-lisp.net/project/cffi/manual/cffi-manual.html)
to locate the SQLite shared library. In most cases, this means CFFI
will use the system default search paths. If you encounter errors in
loading the library, consult the CFFI documentation. For MS Windows,
the certain way to successfully load the DLL is to ensure that the
library is on the `PATH`, _regardless of whether you install via MSYS
or natively_.  {{</alert >}}

To load `sqldf`:

```lisp
(asdf:load-system :sqldf)
(use-package 'sqldf) ;access to the symbols
```

## Examples

These examples use the R data sets that are loaded using the [example
ls-init
file](/docs/getting-started/installation/#initialization-file). If
your init file doesn't do this, go now and load the example datasets
in the REPL. Mostly these examples are intended to demonstrate
commonly used queries for users who are new to SQL. If you already
know SQL, you can skip this section.

{{< alert title="Note" >}}As always when working with lisp-stat,
ensure you are in the LS-USER package {{</alert >}}

### Ordering & Limiting

This example shows how to limit the number of rows output by the
query. It also illustrates changing the column name to meet SQL
identifier requirements.  In particular the data frame data set has
`sepal.length` for a column name, which is converted to `sepal-length`
for the data frame, and we query it with `sepal_length`.

First, let's see how big the `iris` data set is:

```lisp
LS-USER> iris
#<DATA-FRAME (150 observations of 6 variables)>
```

and look at the first few rows:

```lisp
(head iris)
;;   X7 SEPAL-LENGTH SEPAL-WIDTH PETAL-LENGTH PETAL-WIDTH SPECIES
;; 0  1          5.1         3.5          1.4         0.2 setosa
;; 1  2          4.9         3.0          1.4         0.2 setosa
;; 2  3          4.7         3.2          1.3         0.2 setosa
;; 3  4          4.6         3.1          1.5         0.2 setosa
;; 4  5          5.0         3.6          1.4         0.2 setosa
;; 5  6          5.4         3.9          1.7         0.4 setosa
```

`X7` is the row name/number from the data set. Since it was not assigned a
column name in the data set, `lisp-stat` gives it a random name upon
import (X1, X2, X3, ...).

Now use `sqldf` for a query:

```lisp
(pprint
  (sqldf "select * from iris order by sepal_length desc limit 3"))

;;    X7 SEPAL-LENGTH SEPAL-WIDTH PETAL-LENGTH PETAL-WIDTH SPECIES
;; 0 132          7.9         3.8          6.4         2.0 virginica
;; 1 118          7.7         3.8          6.7         2.2 virginica
;; 2 119          7.7         2.6          6.9         2.3 virginica
```

### Averaging & Grouping

Grouping is often useful during the exploratory phase of data
analysis. Here's how to do it with `sqldf`:

```lisp
(pprint
  (sqldf "select species, avg(sepal_length) from iris group by species"))

;;   SPECIES    AVG(SEPAL-LENGTH)
;; 0 setosa                5.0060
;; 1 versicolor            5.9360
;; 2 virginica             6.5880
```

### Nested Select

For each species, show the two rows with the largest sepal lengths:

```lisp
(pprint
  (sqldf "select * from iris i
	      where x7 in
		  (select x7 from iris where species = i.species order by sepal_length desc limit 2) order by i.species, i.sepal_length desc"))

;;    X7 SEPAL-LENGTH SEPAL-WIDTH PETAL-LENGTH PETAL-WIDTH SPECIES
;; 0  15          5.8         4.0          1.2         0.2 setosa
;; 1  16          5.7         4.4          1.5         0.4 setosa
;; 2  51          7.0         3.2          4.7         1.4 versicolor
;; 3  53          6.9         3.1          4.9         1.5 versicolor
;; 4 132          7.9         3.8          6.4         2.0 virginica
;; 5 118          7.7         3.8          6.7         2.2 virginica
```

Recall the note above about X7 being the row id. This may be different
depending on how many other data frames with an unnamed column have
been imported.

## SQLite access

`sqldf` needs to read and write data frames to the data base, and
these functions are exported for general use.

### Write a data frame
`create-df-table` and `write-table` can be used to write a data frame
to a database. Each take a connection to a database, which may be file
or memory based, a table name and a data frame. Multiple data frames,
with different table names, may be written to a single SQLite file
this way.

### Read a data frame

`read-table` will read a database table into a data frame and update
the column names to be lisp like by converting "." and "\_" to
"-". Note that the CSV reading tools of SQLite (for example,
[DB-Browser for SQLite](https://sqlitebrowser.org/) are _much_ faster
than the lisp libraries, sometimes 15x faster.  This means that often
the quickest way to load a data-frame from CSV data is to first read it
into a SQLite database, and then load the database table into a data
frame.  In practice, SQLite turn out to be a convenient file format
for storing data frames.

## Roadmap

SQLDF is currently written using an apparently abandoned library,
[cl-sqlite](https://github.com/TeMPOraL/cl-sqlite).  Pull requests
from 2012 have been made with no response from the author, and the
SQLite C API has improved considerably in the 12 years since
the 'cl-sqlite` FFI was last updated.

We choose CL-SQLite because, at the time of writing, it was the only
SQLite library with a commercially acceptable license. Since then
[CLSQL](https://www.cliki.net/CLSQL) has migrated to a BSD license and
is a better option for new development. Not only does it support
[CommonSQL](http://www.lispworks.com/documentation/sql-tutorial/), the
de-facto SQL query syntax for Common Lisp, it also supports several
additional databases.

Version 2 of SQLDF will use CLSQL, possibly including some of the
[CSV](https://www.sqlite.org/csv.html) and other extensions available
in SQLite.  Benchmarks show that SQLite's CSV import is about 15x
faster than [cl-csv](https://github.com/AccelerationNet/cl-csv), and a
FFI wrapper of SQLite's CSV importer would be a good addition to
Lisp-Stat.

### Joins

Joins on tables are not implemented in SQLDF, though there is no
technical reason they could not be. This will be done as part of the
CLSQL conversion and involves more advanced SQL
parsing. [SXQL](https://github.com/fukamachi/sxql) is worth
investigating as a SQL parser.
