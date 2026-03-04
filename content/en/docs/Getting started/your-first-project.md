---
title: "Your First Project"
weight: 6
date: 2022-06-18
vega: true
description: >
  How to start your first project
---

Lisp-Stat includes a [project
template](https://github.com/Lisp-Stat/project-template/) that you can
use as a guide for your own projects.

## Use the template
To get started, go to the [project
template](https://github.com/Lisp-Stat/project-template/)

1. Click **Use this template**
2. Select a name for your new project and click **Create repository from template**
3. Make your own local working copy of your new repo using `git
   clone`, replacing https://github.com/me/example.git with your
   repo's URL:
   `git clone --depth 1 https://github.com/me/example.git`
4. You can now edit your own versions of the project's source files.

This will clone the project template into your own github repository
so you can begin adding your own files to it.

## Directory Structure

By convention, we use a directory structure that looks like this:
```
...
├── project
|   ├── data
|   |   ├── foo.csv
|   |   ├── bar.json
|   |   └── baz.tsv
|   └── src
|   |   ├── load.lisp
|   |   └── analyse.lisp
|   |   └── baz.tsv
|   └── tests
|   |   ├── test.lisp
|   └── doc
|   |   ├── project.html
...
```

### data

Often your project will have sample data used for examples
illustrating how to use the system.  Such example data goes here, as
would static data files that your system includes, for example post
codes (zip codes).  For some projects, we keep the project data here
too.  If the data is obtained over the network or a data base, login
credentials and code related to that is kept here.  Basically,
anything neccessary to obtain the data should be kept in this
directory.

### src

The lisp source code for loading, cleaning and analysing your data.
If you are using the template for a Lisp-Stat add-on package, the
source code for the functionality goes here.

### tests

Tests for your code. We recommend [CL-UNIT2](https://github.com/lisp-mirror/clunit2) for test
frameworks.

### docs

Generated documentation goes here.  This could be both API
documentation and user guides and manuals.  If an `index.html` file
appears here, github will automatically display it's contents at
project.github.io, if you have configured the repository to display
documentation that way.

## Load your project

If you've cloned the project template into your local Common Lisp
directory, `~/common-lisp/`, then you can load it with `(ql:quickload
:project)`.  Lisp will download and compile the necessary
dependencies and your project will be loaded.  The first thing you'll
want to do is to configure your project.

## Configure your project

First, change the directory and repository name to suit your
environment and make sure git remotes are working properly.  Save
yourself some time and get git working before configuring the project
further.

### ASDF

The `project.asd` file is the Common Lisp system definition file.
Rename this to be the same as your project directory and edit its
contents to reflect the state of your project.  To start with, don't
change any of the file names; just edit the meta data.  As you add or
rename source code files in the project you'll update the file names
here so Common Lisp will know that to compile.  This file is analgous
to a makefile in C -- it tells lisp how to build your project.


### Initialisation
If you need project-wide initialisation settings, you can do this in
the file `src/init.lisp`.  The template sets up a [logical path
name](https://www.cs.cmu.edu/Groups/AI/html/cltl/clm/node211.html) for
the project:

```lisp
(defun setup-project-translations ()
  (setf (logical-pathname-translations "PROJECT")
	`(("DATA;**;*.*.*"    ,(merge-pathnames "data/**/*.*" (asdf:system-source-directory 'project))))))

(setup-project-translations)
```

To use it, you'll modify the directories and project name for your
project, and then call `(setup-project-translations)` in one of your
lisp initialisation files (either ls-init.lisp or .sbclrc).  By
default, the project data directory will be set to a subdirectory
below the main project directory, and you can access files there with
`PROJECT:DATA;mtcars.csv` for example.  When you configure your
logical pathnames, you'll replace "PROJECT" with your projects name.

We use logical style pathnames throughout the Lisp-Stat documentation,
even if a code level translation isn't in place.


## Basic workflow

The project templates illustrates the basic steps for a simple
analysis.

### Load data

The first step is to load data.  The `PROJECT:SRC;load` file shows
creating three data frames, from three different sources: CSV, TSV and
JSON.  Use this as a template for loading your own data.

### Cleanse data

`load.lisp` also shows some simple cleansing, adding labels, types and
attributes, and transforming (recoding) a variable.  You can follow
these examples for your own data sets, with the goal of creating a
data frame from your data.

### Analyse

`PROJECT:SRC;analyse` shows taking the mean and standard deviation of
the `mpg` variable of the loaded data set.  Your own analysis will, of
course, be different.  The examples here are meant to indicate the
purpose.  You may have one or more files for your analysis, including
supporting functions, joining data sets, etc.

### Plot

Plotting can be useful at any stage of the process.  It's inclusion as
the third step isn't intended to imply a particular importance or
order.  The file `PROJECT:SRC;plot` shows how to plot the information
in the `disasters` data frame.

{{< vega id="first-project-plot" spec="/plots/natural-disaster-deaths.vl.json" >}}

### Save

Finally, you'll want to save your data frame after you've got it where
you want it to be.  You can save project in a 'native' format, a lisp
file, that will preserve all your meta data and is editable, or a CSV
file.  You should only use a CSV file if you need to use the data in
another system.  `PROJECT:SRC;save` containes an example that shows how to save your work.


