---
title: "SQL"
date: 2021-05-12
weight: 4
draft: true
description: >
  Creating data frames from relational databases
---

SQLDF has a single exported function `sqldf`, which you use to query a data frame.  In the background, `sqldf` constructs a database table in memory (for SQLite backends), runs the query, returns a new data frame and deletes the table.  This allows you to use ANSI SQL to filter the data frame.


See:
https://cran.r-project.org/doc/manuals/r-devel/R-data.html#Relational-databases - good intro on why to use SQL
https://dept.stat.lsa.umich.edu/~jerrick/courses/stat701/notes/sql.html

