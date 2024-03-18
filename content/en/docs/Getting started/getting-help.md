---
title: "Getting Help"
weight: 3
date: 2022-06-18
description: >
  Ways to get help with Lisp-Stat
---

There are several ways to get help with Lisp-Stat and your statistical
analysis.  This section describes way to get help with your data
objects, with Lisp-Stat commands to process them, and with Common
Lisp.

## Search
We use the [algolia search engine](https://www.algolia.com/) to index
the site.  This search engine is specialised to work well with
documentation websites like this one.  If you're looking for something
and can't find it in the navigation panes, use the search box:

{{< figure src="/images/search-box.png" >}}

## Apropos

If you're not quite sure what you're looking for, you can use the
`apropos` command.  You can do this either from the REPL or emacs.
Here are two examples:

```lisp
LS-USER> (apropos "remove-if")
SB-SEQUENCE:REMOVE-IF (fbound)
SB-SEQUENCE:REMOVE-IF-NOT (fbound)
REMOVE-IF (fbound)
REMOVE-IF-NOT (fbound)
```

This works even better using emacs/slime.  If you use the slime command sequence `C-c C-d a`, (all the slime documentation commands start with `C-c C-d`) emacs will ask you for a string. Let's say you typed in `remove-if`.  Emacs will open a buffer like the one below with all the docs strings for similar functions or variables:

{{< figure src="/images/slime-apropos.png" >}}



## Restart from errors

Common lisp has what is called a _condition system_, which is somewhat unique.  One of the features of the condition system is something call _restarts_.  Basically, one part of the system can _signal_ a condition, and another part of it can _handle_ the condition.  One of the ways a signal can be handled is by providing various _restarts_.  Restarts happen  by the debugger, and many users new to Common Lisp tend to shy away from the debugger (this is common to other languages too).  In Common Lisp the debugger is both for developers **and** users.

Well written Lisp programs will provide a good set of restarts for commonly encountered situations.  As an example, suppose we are plotting a data set that has a large number of data points.  Experience has shown that greater than 50,000 data points can cause browser performance issues, so we've added a restart to warn you, seen below:

{{< figure src="/images/restart-example.png" >}}

Here you can see we have options to take all the data, take `n` (that the user will provide) or take up to the maximum recommended number. Always look at the options offered to you by the debugger and see if any of them will fix the problem for you.

## Describe data

You can use the `describe` command to print a description of just
about anything in the Lisp environment.  Lisp-Stat extends this
functionality to describe data.  For example:

```lisp
LS-USER> (describe 'mtcars)
LS-USER::MTCARS
  [symbol]

MTCARS names a special variable:
  Value: #<DATA-FRAME (32 observations of 12 variables)
Motor Trend Car Road Tests>
  Documentation:
    Motor Trend Car Road Tests

	Description
    The data was extracted from the 1974 Motor Trend US magazine, and
	comprises fuel consumption and 10 aspects of automobile design and
	performance for 32 automobiles (1973–74 models).

    Note
    Henderson and Velleman (1981) comment in a footnote to Table 1:
	‘Hocking [original transcriber]'s noncrucial coding of the Mazda's
	rotary engine as a straight six-cylinder engine and the Porsche's
	flat engine as a V engine, as well as the inclusion of the diesel
	Mercedes 240D, have been retained to enable direct comparisons to
	be made with previous analyses.’

    Source
    Henderson and Velleman (1981), Building multiple regression models
	interactively. Biometrics, 37, 391–411.

  Variables:
    Variable | Type         | Unit | Label
    -------- | ----         | ---- | -----------
    MODEL    | STRING       | NIL  | NIL
    MPG      | DOUBLE-FLOAT | M/G  | Miles/(US) gallon
    CYL      | INTEGER      | NA   | Number of cylinders
    DISP     | DOUBLE-FLOAT | IN3  | Displacement (cu.in.)
    HP       | INTEGER      | HP   | Gross horsepower
    DRAT     | DOUBLE-FLOAT | NA   | Rear axle ratio
    WT       | DOUBLE-FLOAT | LB   | Weight (1000 lbs)
    QSEC     | DOUBLE-FLOAT | S    | 1/4 mile time
    VS       | CATEGORICAL  | NA   | Engine (0=v-shaped, 1=straight)
    AM       | CATEGORICAL  | NA   | Transmission (0=automatic, 1=manual)
    GEAR     | CATEGORICAL  | NA   | Number of forward gears
    CARB     | CATEGORICAL  | NA   | Number of carburetors
```

## Documentation

The `documentation` command can be used to read the documentation of a function or variable.  Here's how to read the documentation for the Lisp-Stat `mean` function:

```lisp
LS-USER> (documentation 'mean 'function)
"The mean of elements in OBJECT."
```

You can also view the documentation for variables or data objects:

```lisp
LS-USER> (documentation '*ask-on-redefine* 'variable)

"If non-nil the system will ask the user for confirmation
before redefining a data frame"
```


## Emacs inspector
When Lisp prints an interesting object to emacs/slime, it will be
displayed in orange text.  This indicates that it is a _presentation_, a
special kind of object that we can manipulate.  For example if you type
the name of a data frame, it will return a presentation object:

{{< figure src="/images/presentation.png" >}}

Now if you right click on this object you'll get the presentation menu:

{{< figure src="/images/presentation-menu.png" >}}

From this menu you can go to the source code of the object, inspect &
change values, describe it (as seen above, but within an emacs
window), and copy it.

## Slime inspector
The [slime
inspector](https://slime.common-lisp.dev/doc/html/Inspector.html) is
an alternative inspector for emacs, with some additional
functionality.

## Slime documentation

[Slime documentation](https://slime.common-lisp.dev/doc/html/Documentation.html) provides ways to browse documentation from the editor.  We saw one example above with `apropos`.  You can also browse variable and function documentation.  For example if you have the cursor positioned over a function:

```lisp
(show-data-frames)
```

and you type `C-c C-d f` (describe function at point), you'll see this
in an emacs window:

```
#<FUNCTION SHOW-DATA-FRAMES>
  [compiled function]


Lambda-list: (&KEY (HEAD NIL) (STREAM *STANDARD-OUTPUT*))
Derived type: (FUNCTION (&KEY (:HEAD T) (:STREAM T)) *)
Documentation:
  Print all data frames in the current environment in
  reverse order of creation, i.e. most recently created first.
  If HEAD is not NIL, print the first six rows, similar to the
  HEAD function.
Source file: s:/src/data-frame/src/defdf.lisp
```

## Other help
You can also get help from the [Lisp-Stat community](/community/), the [user mailing list](https://groups.google.com/g/lisp-stat), [github](https://github.com/lisp-stat) or [stackoverflow](https://stackoverflow.com/questions/tagged/xlispstat)



