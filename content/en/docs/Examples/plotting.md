---
title: "Plotting"
date: 2021-04-26
vega: true
weight: 1
description: >
  Example plots
---

The plots here show equivalents to the [Vega-Lite example
gallery](https://vega.github.io/vega-lite/examples/).  These examples
show how to plot 'raw' data frame data.

## Preliminaries

### Load Vega-Lite

Load Vega-Lite and network libraries:

```lisp
(asdf:load-system :plot/vega)
```

and change to the Lisp-Stat user package:

```lisp
(in-package :ls-user)
```

### Load example data

The examples in this section use the vega-lite data sets. Load them all now:

```lisp
(vega:load-vega-examples)
```

## Bar charts

Bar charts are used to display information about categorical variables.

### Simple bar chart

{{< vega id="bar-chart-simple" spec="/plots/simple-bar-chart.vl.json" >}}

In this simple bar chart example we'll demonstrate using literal
embedded data in the form of a `plist`.  Later you'll see how to use a
`data-frame` as a data source.

```lisp
(plot:plot
 (vega:defplot simple-bar-chart
   `(:mark :bar
     :data (:a #(A B C D E F G H I) :b #(28 55 43 91 81 53 19 87 52))
     :encoding (:x (:field :a :type :nominal :axis ("labelAngle" 0))
                :y (:field :b :type :quantitative)))))
```

### Grouped bar chart

{{< vega id="bar-chart-grouped" spec="/plots/grouped-bar-chart.vl.json" >}}

```lisp
(plot:plot
 (vega:defplot grouped-bar-chart
   '(:mark :bar
     :data (:category #(A A A B B B C C C)
	        :group    #(x y z x y z x y z)
	        :value    #(0.1 0.6 0.9 0.7 0.2 1.1 0.6 0.1 0.2))
     :encoding (:x (:field :category)
                :y (:field :value :type :quantitative)
		        :x-offset (:field :group)
		        :color    (:field group)))))
```

### Stacked bar chart

{{< vega id="bar-chart-stacked" spec="/plots/stacked-bar-chart.vl.json" >}}

For this example, we'll use the Seattle weather example from the Vega
website.  Load it into a data frame like so:

```lisp
(defdf seattle-weather (read-csv vega:seattle-weather))
;=> #<DATA-FRAME (1461 observations of 6 variables)>
```

We'll use a `data-frame` as the data source via the Common Lisp
[backquote
mechanism](http://cl-cookbook.sourceforge.net/macros.html#LtohTOCentry-2).
The spec list begins with a backquote (\`) and then the data frame is
inserted with a comma (`,`).  We'll use this pattern frequently.

```lisp
(plot:plot
 (vega:defplot stacked-bar-chart
   `(:mark :bar
     :data ,seattle-weather
     :encoding (:x (:time-unit :month
		    :field :date
		    :type :ordinal
		    :title "Month of the year")
                :y (:aggregate :count
		    :type :quantitative)
		:color (:field :weather
			:type :nominal
			:title "Weather type"
			:scale (:domain #("sun" "fog" "drizzle" "rain" "snow")
				:range  #("#e7ba52", "#c7c7c7", "#aec7e8", "#1f77b4", "#9467bd")))))))
```

### Population pyramid

{{< vega id="bar-chart-pyramid" spec="/plots/pyramid-bar-chart.vl.json" >}}

Vega calls this a [diverging stacked bar
chart](https://vega.github.io/vega-lite/examples/bar_diverging_stack_population_pyramid.html).
It is a population pyramid for the US in 2000, created using the
[stack](https://vega.github.io/vega-lite/docs/stack.html) feature of
vega-lite.  You could also create one using
[concat](https://vega.github.io/vega-lite/examples/concat_population_pyramid.html).

First, load the population data if you haven't done so:

```lisp
(defdf population (vega:read-vega vega:population))
;=> #<DATA-FRAME (570 observations of 4 variables)>
```

Note the use of `read-vega` in this case.  This is because the data in
the Vega example is in an application specific JSON format (Vega, of
course).

```lisp
(plot:plot
 (vega:defplot pyramid-bar-chart
   `(:mark :bar
     :data ,population
     :width 300
     :height 200
     :transform #((:filter "datum.year == 2000")
		  (:calculate "datum.sex == 2 ? 'Female' : 'Male'" :as :gender)
		  (:calculate "datum.sex == 2 ? -datum.people : datum.people" :as :signed-people))
     :encoding (:x (:aggregate :sum
		    :field :signed-people
		    :title "population")
                :y (:field :age
		    :axis nil
		    :sort :descending)
		:color (:field :gender
			:scale (:range #("#675193" "#ca8861"))))
     :config (:view (:stroke nil)
	      :axis (:grid :false)))))
```

## Histograms & density

### Basic

{{< vega id="imdb" spec="/plots/imdb.vl.json" >}}

For this simple [histogram
example](https://vega.github.io/vega-lite/examples/histogram.html)
we'll use the IMDB film rating data set.

```lisp
(plot:plot
  (vega:defplot imdb-plot
    `(:mark :bar
      :data ,imdb
      :encoding (:x (:bin (:maxbins 8) :field :imdb-rating)
                 :y (:aggregate :count)))))
```

### Relative frequency

{{< vega id="relative-frequency-histogram" spec="/plots/relative-frequency-histogram.vl.json" >}}

Use a relative frequency histogram to compare data sets with different
numbers of observations.

The data is binned with first transform. The number of values per bin
and the total number are calculated in the second and third transform
to calculate the relative frequency in the last transformation step.

```lisp
(plot:plot
 (vega:defplot relative-frequency-histogram
   `(:title "Relative Frequency"
     :data ,vgcars
     :transform #((:bin t
	               :field :horsepower
		           :as #(:bin-horsepower :bin-horsepower-end))
                  (:aggregate #((:op :count
				                 :as "Count"))
                   :groupby   #(:bin-horsepower :bin-horsepower-end))
		          (:joinaggregate #((:op :sum
				                     :field "Count"
				                     :as "TotalCount")))
		          (:calculate "datum.Count/datum.TotalCount"
		                      :as :percent-of-total))
     :mark (:type :bar :tooltip t)
     :encoding (:x (:field :bin-horsepower
	                :title "Horsepower"
		            :bin (:binned t))
		        :x2 (:field :bin-horsepower-end)
	            :y (:field :percent-of-total
		            :type "quantitative"
		            :title "Relative Frequency"
		            :axis (:format ".1~%"))))))
```

### 2D histogram scatterplot

{{< vega id="histogram-scatterplot" spec="/plots/histogram-scatterplot.vl.json" >}}

If you haven't already loaded the `imdb` data set, do so now:

```lisp
(defparameter imdb
  (vega:read-vega vega:movies))
```

```lisp
(plot:plot
  (vega:defplot histogram-scatterplot
    `(:mark :circle
      :data ,imdb
      :encoding (:x (:bin (:maxbins 10) :field imdb-rating)
                 :y (:bin (:maxbins 10) :field :rotten-tomatoes-rating)
	             :size (:aggregate :count)))))
```

### Stacked density

{{< vega id="stacked-density-plot" spec="/plots/stacked-density.vl.json" >}}

```lisp
(plot:plot
 (vega:defplot stacked-density
   `(:title "Distribution of Body Mass of Penguins"
     :width 400
     :height 80
     :data ,penguins
     :mark :bar
     :transform #((:density |BODY-MASS-(G)|
		   :groupby #(:species)
		   :extent #(2500 6500)))
     :encoding (:x (:field :value
		    :type :quantitative
		    :title "Body Mass (g)")
		:y (:field :density
		    :type :quantitative
		    :stack :zero)
		:color (:field :species
			:type :nominal)))))
```

Note the use of the [multiple escape
characters](http://www.lispworks.com/documentation/lw71/CLHS/Body/02_ade.htm)
(|) surrounding the field `BODY-MASS-(G)`.  This is required because
the JSON data set has parenthesis in the variable names, and these are
a reserved character in Common Lisp.  The JSON importer wrapped these
in the escape character.

## Scatter plots

### Basic

{{< vega id="hp-mpg" spec="/plots/hp-mpg.vl.json" >}}

A [basic Vega-Lite
scatterplot](https://vega.github.io/vega-lite/examples/point_2d.html)
showing horsepower and miles per gallon for various cars.

```lisp
(plot:plot
  (vega:defplot hp-mpg
  `(:title "Horsepower vs. MPG"
    :data ,vgcars
    :mark :point
	:encoding (:x (:field :horsepower :type "quantitative")
	           :y (:field :miles-per-gallon :type "quantitative")))))
```

### Colored

{{< vega id="colored-hp-mpg" spec="/plots/colored-hp-mpg.vl.json" >}}

In this example we'll show how to add some additional information to
the cars scatter plot to show the cars origin. The [Vega-Lite
example](https://vega.github.io/vega-lite/examples/point_color_with_shape.html)
shows that we have to add two new directives to the _encoding_ of the
plot:

```lisp
(plot:plot
  (vega:defplot hp-mpg-plot
  `(:title "Vega Cars"
    :data ,vgcars
    :mark :point
	:encoding (:x     (:field :horsepower :type "quantitative")
	           :y     (:field :miles-per-gallon :type "quantitative")
			   :color (:field :origin :type "nominal")
			   :shape (:field :origin :type "nominal")))))
```

With this change we can see that the higher horsepower, lower
efficiency, cars are from the USA, and the higher efficiency cars from
Japan and Europe.

### Text marks

{{< vega id="colored-text-hp-mpg" spec="/plots/colored-text-hp-mpg.vl.json" >}}

The same information, but further indicated with a text marker. This
[Vega-Lite
example](https://vega.github.io/vega-lite/examples/text_scatterplot_colored.html)
uses a data transformation.

```lisp
(plot:plot
  (vega:defplot colored-text-hp-mpg-plot
  `(:title "Vega Cars"
    :data ,vgcars
	:transform #((:calculate "datum.origin[0]" :as "OriginInitial"))
    :mark :text
	:encoding (:x     (:field :horsepower :type "quantitative")
	           :y     (:field :miles-per-gallon :type "quantitative")
	           :color (:field :origin :type "nominal")
			   :text  (:field "OriginInitial" :type "nominal")))))
```

Notice here we use a string for the field value and not a symbol.
This is because Vega is case sensitive, whereas Lisp is not.  We could
have also used a lower-case `:as` value, but did not to highlight this
requirement for certain Vega specifications.


### Mean & SD overlay

These graph types are broken in Vega-Lite when using embedded data. See [issue 8280](https://github.com/vega/vega-lite/issues/8280).  The JSON output by Plot is exactly the same, so only use this with URL data.

This Vega-Lite [scatterplot with mean and standard deviation
overlay](https://vega.github.io/vega-lite/examples/layer_scatter_errorband_1D_stdev_global_mean.html)
demonstrates the use of layers in a plot.

{{< vega id="mean-hp-mpg" spec="/plots/mean-hp-mpg-plot.vl.json" >}}

Lisp-Stat equivalent

```lisp
(plot:plot
  (vega:defplot mean-hp-mpg-plot
  `(:title "Vega Cars"
    :data ,vgcars
    :layer #((:mark :point
	          :encoding (:x (:field :horsepower :type "quantitative")
			             :y (:field :miles-per-gallon
						            :type "quantitative")))
	         (:mark (:type :errorband :extent :stdev :opacity 0.2)
	          :encoding (:y (:field :miles-per-gallon
			                 :type "quantitative"
							 :title "Miles per Gallon")))
	         (:mark :rule
	          :encoding (:y (:field :miles-per-gallon
			                 :type "quantitative"
							 :aggregate :mean)))))))
```

### Linear regression

{{< vega id="scatter-linear-regression" spec="/plots/linear-regression.vl.json" >}}

```lisp
(plot:plot
 (vega:defplot linear-regression
   `(:data ,imdb
     :layer #((:mark (:type :point
	              :filled t)
	       :encoding (:x (:field :rotten-tomatoes-rating
			      :type :quantitative
			      :title "Rotten Tomatoes Rating")
			  :y (:field :imdb-rating
			      :type :quantitative
			      :title "IMDB Rating")))

	       (:mark (:type :line
		       :color "firebrick")
		:transform #((:regression :imdb-rating
			      :on :rotten-tomatoes-rating))
		:encoding (:x (:field :rotten-tomatoes-rating
			       :type :quantitative
			       :title "Rotten Tomatoes Rating")
			   :y (:field :imdb-rating
			       :type :quantitative
			       :title "IMDB Rating")))

	      (:transform #((:regression :imdb-rating
			     :on :rotten-tomatoes-rating
			     :params t)
			    (:calculate "'R²: '+format(datum.rSquared, '.2f')"
			     :as :r2))
	       :mark (:type :text
		      :color "firebrick"
		      :x :width
		      :align :right
		      :y -5)
	       :encoding (:text (:type :nominal
				 :field :r2)))))))
```

### Loess regression

{{< vega id="scatter-loess-regression" spec="/plots/loess-regression.vl.json" >}}

```lisp
(plot:plot
 (vega:defplot loess-regression
   `(:data ,imdb
     :layer #((:mark (:type :point
	              :filled t)
	       :encoding (:x (:field :rotten-tomatoes-rating
			      :type :quantitative
			      :title "Rotten Tomatoes Rating")
			  :y (:field :imdb-rating
			      :type :quantitative
			      :title "IMDB Rating")))

	       (:mark (:type :line
		       :color "firebrick")
		:transform #((:loess :imdb-rating
			      :on :rotten-tomatoes-rating))
		:encoding (:x (:field :rotten-tomatoes-rating
			       :type :quantitative
			       :title "Rotten Tomatoes Rating")
			   :y (:field :imdb-rating
			       :type :quantitative
			       :title "IMDB Rating")))))))
```

### Residuals

{{< vega id="scatter-residuals" spec="/plots/residuals.vl.json" >}}

A dot plot showing each movie in the database, and the difference from
the average movie rating. The display is sorted by year to visualize
everything in sequential order. The graph is for all films before
2019.

```lisp
(plot:plot
 (vega:defplot residuals
   `(:data ,(filter-rows imdb '(and (not (eql imdb-rating :na))
				(local-time:timestamp< release-date (local-time:parse-timestring "2019-01-01"))))
     :transform #((:joinaggregate #((:op :mean ;we could do this above using alexandria:thread-first
				     :field :imdb-rating
				     :as :average-rating)))
		  (:calculate "datum['imdbRating'] - datum.averageRating"
		   :as :rating-delta))
     :mark :point
     :encoding (:x (:field :release-date
		    :type :temporal
		    :title "Release Date")
		:y (:field :rating-delta
		    :type :quantitative
		    :title "Rating Delta")
		:color (:field :rating-delta
			:type :quantitative
			:scale (:domain-mid 0)
			:title "Rating Delta")))))
```

### Query

{{< vega id="scatter-queries" spec="/plots/scatter-queries.vl.json" >}}

The cars scatterplot allows you to see miles per gallon
vs. horsepower.  By adding sliders, you can select points by the
number of cylinders and year as well, effectively examining 4
dimensions of data.  Drag the sliders to highlight different points.

```lisp
(plot:plot
 (vega:defplot scatter-queries
   `(:data ,vgcars
     :transform #((:calculate "year(datum.year)" :as :year))
     :layer #((:params #((:name :cyl-year
			 :value #((:cylinders 4
				   :year 1799))
			 :select (:type :point
				  :fields #(:cylinders :year))
			 :bind (:cylinders (:input :range
					    :min 3
					    :max 8
					    :step 1)
				:year (:input :range
				       :min 1969
				       :max 1981
				       :step 1))))
	       :mark :circle
	       :encoding (:x (:field :horsepower
			      :type :quantitative)
			  :y (:field :miles-per-gallon
			      :type :quantitative)
			  :color (:condition (:param :cyl-year
					      :field :origin
					      :type :nominal)
				  :value "grey")))

	      (:transform #((:filter (:param :cyl-year)))
	       :mark :circle
	       :encoding (:x (:field :horsepower
			      :type :quantitative)
			  :y (:field :miles-per-gallon
			      :type :quantitative)
			  :color (:field :origin
				  :type :nominal)
			  :size (:value 100)))))))
```

### External links

{{< vega id="scatter-external-links" spec="/plots/scatter-external-links.vl.json" >}}

You can add external links to plots.

```lisp
(plot:plot
 (vega:defplot scatter-external-links
   `(:data ,vgcars
     :mark :point
     :transform #((:calculate "'https://www.google.com/search?q=' + datum.name", :as :url))
     :encoding (:x (:field :horsepower
		    :type :quantitative)
		:y (:field :miles-per-gallon
		    :type :quantitative)
		:color (:field :origin
			:type :nominal)
		:tooltip (:field :name
			  :type :nominal)
		:href (:field :url
		       :type :nominal)))))
```

### Strip plot

{{< vega id="cars-strip-plot" spec="/plots/strip-plot.vl.json" >}}

The Vega-Lite [strip plot
example](https://vega.github.io/vega-lite/examples/tick_strip.html)
shows the relationship between horsepower and the number of cylinders
using tick marks.

```lisp
(plot:plot
  (vega:defplot strip-plot
  `(:title "Vega Cars"
    :data ,vgcars
	:mark :tick
	:encoding (:x (:field :horsepower :type :quantitative)
	           :y (:field :cylinders  :type :ordinal)))))
```

### 1D strip plot

{{< vega id="cars-strip-plot-2" spec="/plots/1d-strip-plot.vl.json" >}}

```lisp
(plot:plot
  (vega:defplot 1d-strip-plot
  `(:title "Seattle Precipitation"
    :data ,seattle-weather
	:mark :tick
	:encoding (:x (:field :precipitation :type :quantitative)))))
```

### Bubble plot

This [Vega-Lite
example](https://vega.github.io/vega-lite/examples/circle_natural_disasters.html)
is a visualization of global deaths from natural disasters. A copy of
the chart from [Our World in
Data](https://ourworldindata.org/natural-disasters).

{{< vega id="natural-disaster-deaths" spec="/plots/natural-disaster-deaths.vl.json" >}}

```lisp
(plot:plot
 (vega:defplot natural-disaster-deaths
   `(:title "Deaths from global natural disasters"
     :width 600
     :height 400
     :data ,(filter-rows disasters '(not (string= entity "All natural disasters")))
     :mark (:type :circle
	    :opacity 0.8
	    :stroke :black
	    :stroke-width 1)
     :encoding (:x (:field :year
		    :type :temporal
		    :axis (:grid :false))
		:y (:field :entity
		    :type :nominal
		    :axis (:title ""))
		:size (:field :deaths
		       :type :quantitative
		       :title "Annual Global Deaths"
		       :legend (:clip-height 30)
		       :scale (:range-max 5000))
		:color (:field :entity
			:type :nominal
			:legend nil)))))
```

Note how we modified the example by using a lower case `entity` in the
filter to match our default lower case variable names.  Also note how
we are explicit with parsing the year field as a temporal column.
This is because, when creating a chart with inline data, Vega-Lite
will parse the field as an integer instead of a date.

## Line plots

### Simple

{{< vega id="simple-line-plot" spec="/plots/simple-line-plot.vl.json" >}}

```lisp
(plot:plot
 (vega:defplot simple-line-plot
   `(:title "Google's stock price from 2004 to early 2010"
     :data ,(filter-rows stocks '(string= symbol "GOOG"))
     :mark :line
     :encoding (:x (:field :date
		    :type  :temporal)
		:y (:field :price
		    :type  :quantitative)))))
```

### Point markers

{{< vega id="point-mark-line-plot" spec="/plots/point-mark-line-plot.vl.json" >}}

By setting the point property of the line mark definition to an object
defining a property of the overlaying point marks, we can overlay
point markers on top of line.

```lisp
(plot:plot
 (vega:defplot point-mark-line-plot
   `(:title "Stock prices of 5 Tech Companies over Time"
     :data ,stocks
     :mark (:type :line :point t)
     :encoding (:x (:field :date
		    :time-unit :year)
		:y (:field :price
		    :type :quantitative
		    :aggregate :mean)
		:color (:field :symbol
			:type nominal)))))
```


### Multi-series

{{< vega id="line-chart-multi-series" spec="/plots/multi-series-line-chart.vl.json" >}}

This example uses the custom symbol encoding for variables to
generate the proper types and labels for x, y and color channels.

```lisp
(plot:plot
 (vega:defplot multi-series-line-chart
   `(:title "Stock prices of 5 Tech Companies over Time"
     :data ,stocks
     :mark :line
     :encoding (:x (:field stocks:date)
 :y (:field stocks:price)
		:color (:field stocks:symbol)))))
```

### Step

{{< vega id="step-chart" spec="/plots/step-chart.vl.json" >}}

```lisp
(plot:plot
 (vega:defplot step-chart
   `(:title "Google's stock price from 2004 to early 2010"
     :data ,(filter-rows stocks '(string= symbol "GOOG"))
     :mark (:type :line
	    :interpolate "step-after")
     :encoding (:x (:field stocks:date)
		:y (:field stocks:price)))))
```


### Stroke-dash

{{< vega id="stroke-dash-chart" spec="/plots/stroke-dash.vl.json" >}}

```lisp
(plot:plot
 (vega:defplot stroke-dash
   `(:title "Stock prices of 5 Tech Companies over Time"
     :data ,stocks
     :mark :line
     :encoding (:x (:field stocks:date)
		:y (:field stocks:price)
		:stroke-dash (:field stocks:symbol)))))
```

### Confidence interval

{{< vega id="line-chart-ci" spec="/plots/line-chart-ci.vl.json" >}}

Line chart with a confidence interval band.

```lisp
(plot:plot
 (vega:defplot line-chart-ci
   `(:data ,vgcars
     :encoding (:x (:field :year
		    :time-unit :year))
     :layer #((:mark (:type :errorband
		      :extent :ci)
	       :encoding (:y (:field :miles-per-gallon
			      :type :quantitative
			      :title "Mean of Miles per Gallon (95% CIs)")))

	      (:mark :line
	       :encoding (:y (:field :miles-per-gallon
			      :aggregate :mean)))))))
```

## Area charts

### Simple

{{< vega id="area-chart" spec="/plots/area-chart.vl.json" >}}

```lisp
(plot:plot
 (vega:defplot area-chart
   `(:title "Unemployment across industries"
     :width 300
     :height 200
     :data ,unemployment-ind
     :mark :area
     :encoding (:x (:field :date
		    :time-unit :yearmonth
		    :axis (:format "%Y"))
		:y (:field :count
		    :aggregate :sum
		    :title "count")))))
```

### Stacked

{{< vega id="stacked-area-chart" spec="/plots/stacked-area-chart.vl.json" >}}

Stacked area plots

```lisp
(plot:plot
 (vega:defplot stacked-area-chart
   `(:title "Unemployment across industries"
     :width 300
     :height 200
     :data ,unemployment-ind
     :mark :area
     :encoding (:x (:field :date
		    :time-unit :yearmonth
		    :axis (:format "%Y"))
		:y (:field :count
		    :aggregate :sum
		    :title "count")
		:color (:field :series
			:scale (:scheme "category20b"))))))
```

### Horizon graph


{{< vega id="lasdkfjalsnm" spec="/plots/horizon-graph.vl.json" >}}

A horizon graph is a technique for visualising time series data in a
manner that makes comparisons easier based on work done at the UW
Interactive Data Lab.  See [Sizing the Horizon: The Effects of Chart
Size and Layering on the Graphical Perception of Time Series
Visualizations](https://idl.cs.washington.edu/papers/horizon/) for
more details on Horizon Graphs.

```lisp
(plot:plot
 (vega:defplot horizon-graph
   `(:title "Horizon graph with 2 layers"
     :width 300
     :height 50
     :data (:x ,(aops:linspace 1 20 20)
	    :y #(28 55 43 91 81 53 19 87 52 48 24 49 87 66 17 27 68 16 49 15))
     :encoding (:x (:field :x
		    :scale (:zero :false
			    :nice :false))
		:y (:field :y
		    :type :quantitative
		    :scale (:domain #(0 50))
		    :axis (:title "y")))
     :layer #((:mark (:type :area
		     :clip t
		     :orient :vertical
		     :opacity 0.6))
	      (:transform #((:calculate "datum.y - 50"
			     :as :ny))
	       :mark (:type :area
		      :clip t
		      :orient :vertical)
	       :encoding (:y (:field "ny"
			      :type :quantitative
			      :scale (:domain #(0 50)))
			  :opacity (:value 0.3))))
     :config (:area (:interpolate :monotone)))))
```

### With overlay

{{< vega id="area-with-overlay" spec="/plots/area-with-overlay.vl.json" >}}

Area chart with overlaying lines and point markers.

```lisp
(plot:plot
 (vega:defplot area-with-overlay
   `(:title "Google's stock price"
     :data ,(filter-rows stocks '(string= symbol "GOOG"))
     :mark (:type :area
	    :line t
	    :point t)
     :encoding (:x (:field stocks:date)
		:y (:field stocks:price)))))
```

Note the use of the variable symbols, e.g. `stocks:price` to fill in
the variable's information instead of `:type :quantitative :title ...`


### Stream graph

{{< vega id="area-stream-graph" spec="/plots/stream-graph.vl.json" >}}

```lisp
(plot:plot
 (vega:defplot stream-graph
   `(:title "Unemployment Stream Graph"
     :width 300
     :height 200
     :data ,unemployment-ind
     :mark :area
     :encoding (:x (:field :date
		    :time-unit "yearmonth"
		    :axis (:domain :false
			   :format "%Y"
			   :tick-size 0))
		:y (:field count
		    :aggregate :sum
		    :axis null
		    :stack :center)
		:color (:field :series
			:scale (:scheme "category20b"))))))
```


## Tabular plots

### Table heatmap

{{< vega id="heatmap-table" spec="/plots/table-heatmap.vl.json" >}}

```lisp
(plot:plot
 (vega:defplot table-heatmap
   `(:data ,vgcars
     :mark :rect
     :encoding (:x (:field vgcars:cylinders)
		:y (:field vgcars:origin)
		:color (:field :horsepower
			:aggregate :mean))
     :config (:axis (:grid t :tick-band :extent)))))
```

### Heatmap with labels

{{< vega id="heatmap-labels" spec="/plots/heatmap-labels.vl.json" >}}

Layering text over a table heatmap

```lisp
(plot:plot
 (vega:defplot heatmap-labels
   `(:data ,vgcars
     :transform #((:aggregate #((:op :count :as :num-cars))
		   :groupby #(:origin :cylinders)))
     :encoding (:x (:field :cylinders
		    :type :ordinal)
		:y (:field :origin
		    :type :ordinal))
     :layer #((:mark :rect
	       :encoding (:color (:field :num-cars
				  :type :quantitative
				  :title "Count of Records"
				  :legend (:direction :horizontal
					   :gradient-length 120))))
	      (:mark :text
	       :encoding (:text (:field :num-cars
				 :type :quantitative)
			  :color (:condition (:test "datum['numCars'] < 40"
					      :value :black)
				  :value :white))))
     :config (:axis (:grid t
		     :tick-band :extent)))))
```

### Histogram heatmap

{{< vega id="heatmap-histogram" spec="/plots/heatmap-histogram.vl.json" >}}

```lisp
(plot:plot
 (vega:defplot heatmap-histogram
   `(:data ,imdb
     :transform #((:and #((:field :imdb-rating :valid t)
			  (:field :rotten-tomatoes-rating :valid t))))
     :mark :rect
     :width 300
     :height 200
     :encoding (:x (:bin (:maxbins 60)
		    :field :imdb-rating
		    :type :quantitative
			:title "IMDB Rating")
		:y (:bin (:maxbins 40)
		    :field :rotten-tomatoes-rating
		    :type :quantitative
			:title "Rotten Tomatoes Rating")
		:color (:aggregate :count
			:type :quantitative))
     :config (:view (:stroke :transparent)))))
```

## Circular plots

### Pie chart

{{< vega id="simple-pie-chart" spec="/plots/pie-chart.vl.json" >}}

```lisp
(plot:plot
 (vega:defplot pie-chart
   `(:data (:category ,(aops:linspace 1 6 6)
	    :value #(4 6 10 3 7 8))
     :mark :arc
     :encoding (:theta (:field :value
			:type :quantitative)
		:color (:field :category
			:type :nominal)))))
```

### Donut chart

{{< vega id="simple-donut-chart" spec="/plots/donut-chart.vl.json" >}}

```lisp
(plot:plot
 (vega:defplot donut-chart
   `(:data (:category ,(aops:linspace 1 6 6)
	    :value #(4 6 10 3 7 8))
     :mark (:type :arc :inner-radius 50)
     :encoding (:theta (:field :value
			:type :quantitative)
		:color (:field :category
			:type :nominal)))))
```

### Radial plot

{{< vega id="simple-radial" spec="/plots/radial-plot.vl.json" >}}

This radial plot uses both angular and radial extent to convey
multiple dimensions of data.  However, this approach is not
perceptually effective, as viewers will most likely be drawn to the
total area of the shape, conflating the two dimensions.  This example
also demonstrates a way to add labels to circular plots.

```lisp
(plot:plot
 (vega:defplot radial-plot
   `(:data (:value #(12 23 47 6 52 19))
     :layer #((:mark (:type :arc
		      :inner-radius 20
		      :stroke "#fff"))
	      (:mark (:type :text
		      :radius-offset 10)
	       :encoding (:text (:field :value
				 :type :quantitative))))
     :encoding (:theta (:field :value
			:type :quantitative
			:stack t)
		:radius (:field :value
			 :scale (:type :sqrt
				 :zero t
				 :range-min 20))
		:color (:field :value
			:type :nominal
			:legend nil)))))
```

## Transformations

Normally data transformations should be done in Lisp-Stat with a data
frame.  These examples illustrate how to accomplish transformations
using Vega-Lite.

### Difference from avg

{{< vega id="difference-from-average" spec="/plots/difference-from-average.vl.json" >}}


```lisp
(plot:plot
 (vega:defplot difference-from-average
   `(:data ,(filter-rows imdb '(not (eql imdb-rating :na)))
     :transform #((:joinaggregate #((:op :mean ;we could do this above using alexandria:thread-first
				     :field :imdb-rating
				     :as :average-rating)))
		  (:filter "(datum['imdbRating'] - datum.averageRating) > 2.5"))
     :layer #((:mark :bar
	       :encoding (:x (:field :imdb-rating
			      :type :quantitative
			      :title "IMDB Rating")
			  :y (:field :title
			      :type :ordinal
			      :title "Title")))
	      (:mark (:type :rule :color "red")
	       :encoding (:x (:aggregate :average
			      :field :average-rating
			      :type :quantitative)))))))
```

### Frequency distribution

{{< vega id="cumulative-frequency-distribution" spec="/plots/cumulative-frequency-distribution.vl.json" >}}

Cumulative frequency distribution of films in the IMDB database.

```lisp
(plot:plot
 (vega:defplot cumulative-frequency-distribution
   `(:data ,imdb
     :transform #((:sort #((:field :imdb-rating))
		   :window #((:op :count
			      :field :count as :cumulative-count))
		   :frame #(nil 0)))
     :mark :area
     :encoding (:x (:field :imdb-rating
		    :type :quantitative)
		:y (:field :cumulative-count
		    :type :quantitative)))))
```


### Layered & cumulative histogram

{{< vega id="layered-histogram" spec="/plots/layered-histogram.vl.json" >}}

```lisp
(plot:plot
 (vega:defplot layered-histogram
   `(:data ,(filter-rows imdb '(not (eql imdb-rating :na)))
     :transform #((:bin t
		   :field :imdb-rating
		   :as #(:bin-imdb-rating :bin-imdb-rating-end))
		  (:aggregate #((:op :count :as :count))
		   :groupby #(:bin-imdb-rating :bin-imdb-rating-end))
		  (:sort #((:field :bin-imdb-rating))
		   :window #((:op :sum
			      :field :count :as :cumulative-count))
		   :frame #(nil 0)))
     :encoding (:x (:field :bin-imdb-rating
		    :type :quantitative
		    :scale (:zero :false)
		    :title "IMDB Rating")
		:x2 (:field :bin-imdb-rating-end))
     :layer #((:mark :bar
	       :encoding (:y (:field :cumulative-count
			      :type :quantitative
			      :title "Cumulative Count")))
	      (:mark (:type :bar
		      :color "yellow"
		      :opacity 0.5)
	       :encoding (:y (:field :count
			      :type :quantitative
			      :title "Count")))))))
```

### Layering averages

{{< vega id="layered-averages" spec="/plots/layered-averages.vl.json" >}}

Layering averages over raw values.

```lisp
(plot:plot
 (vega:defplot layered-averages
   `(:data ,(filter-rows stocks '(string= symbol "GOOG"))
     :layer #((:mark (:type :point
		      :opacity 0.3)
	       :encoding (:x (:field :date
			      :time-unit :year)
			  :y (:field :price
			      :type quantitative)))

	      (:mark :line
	       :encoding (:x (:field :date
			      :time-unit :year)
			  :y (:field :price
			      :aggregate :mean)))))))
```




## Error bars

### Confidence interval

{{< vega id="error-bar-ci" spec="/plots/error-bar-ci.vl.json" >}}

Error bars showing confidence intervals.

```lisp
(plot:plot
 (vega:defplot error-bar-ci
   `(:data ,barley
     :encoding (:y (:field :variety
		    :type :ordinal
		    :title "Variety"))
     :layer #((:mark (:type :point
		      :filled t)
	       :encoding (:x (:field :yield
			      :aggregate :mean
			      :type :quantitative
			      :scale (:zero :false)
			      :title "Barley Yield")
			  :color (:value "black")))

	      (:mark (:type :errorbar :extent :ci)
	       :encoding (:x (:field :yield
			      :type :quantitative
			      :title "Barley Yield")))))))
```

### Standard deviation

{{< vega id="error-bar-sd" spec="/plots/error-bar-sd.vl.json" >}}

Error bars showing standard deviation.

```lisp
(plot:plot
 (vega:defplot error-bar-sd
   `(:data ,barley
     :encoding (:y (:field :variety
		    :type :ordinal
		    :title "Variety"))
     :layer #((:mark (:type :point
		      :filled t)
	       :encoding (:x (:field :yield
			      :aggregate :mean
			      :type :quantitative
			      :scale (:zero :false)
			      :title "Barley Yield")
			  :color (:value "black")))

	      (:mark (:type :errorbar :extent :stdev)
	       :encoding (:x (:field :yield
			      :type :quantitative
			      :title "Barley Yield")))))))
```

## Box plots

### Min/max whiskers

{{< vega id="box-plot-min-max" spec="/plots/box-plot-min-max.vl.json" >}}

A vertical box plot showing median, min, and max body mass of penguins.

```lisp
(plot:plot
 (vega:defplot box-plot-min-max
   `(:data ,penguins
     :mark (:type :boxplot
	    :extent "min-max")
     :encoding (:x (:field :species
		    :type :nominal
		    :title "Species")
	        :y (:field |BODY-MASS-(G)|
		    :type :quantitative
		    :scale (:zero :false)
		    :title "Body Mass (g)")
		:color (:field :species
			:type :nominal
			:legend nil)))))
```

### Tukey

{{< vega id="box-plot-tukey" spec="/plots/box-plot-tukey.vl.json" >}}

A vertical box plot showing median and lower and upper quartiles of
the distribution of body mass of penguins.

```lisp
(plot:plot
 (vega:defplot box-plot-tukey
   `(:data ,penguins
     :mark :boxplot
     :encoding (:x (:field :species
		    :type :nominal
		    :title "Species")
	        :y (:field |BODY-MASS-(G)|
		    :type :quantitative
		    :scale (:zero :false)
		    :title "Body Mass (g)")
		:color (:field :species
			:type :nominal
			:legend nil)))))
```


### Summaries

{{< vega id="box-plot-summaries" spec="/plots/box-plot-summaries.vl.json" >}}

Box plot with pre-computed summaries.  Use this pattern to plot
summaries done in a `data-frame`.

```lisp
(plot:plot
 (vega:defplot box-plot-summaries
   `(:title "Body Mass of Penguin Species (g)"
     :data (:species #("Adelie" "Chinstrap" "Gentoo")
	    :lower #(2850 2700 3950)
	    :q1 #(3350 3487.5 4700)
	    :median #(3700 3700 5000)
	    :q3 #(4000 3950 5500)
	    :upper #(4775 4800 6300)
	    :outliers #(#() #(2700 4800) #()))
     :encoding (:y (:field :species
		    :type :nominal
		    :title null))
     :layer #((:mark (:type :rule)
	       :encoding (:x (:field :lower
			      :type :quantitative
			      :scale (:zero :false)
			      :title null)
			  :x2 (:field :upper)))

	      (:mark (:type :bar :size 14)
	       :encoding (:x (:field :q1
			      :type :quantitative)
			  :x2 (:field :q3)
			  :color (:field :species
				  :type :nominal
				  :legend null)))

	      (:mark (:type :tick
		      :color :white
		      :size 14)
	       :encoding (:x (:field :median
			      :type :quantitative)))

	      (:transform #((:flatten #(:outliers)))
	       :mark (:type :point :style "boxplot-outliers")
	       :encoding (:x (:field :outliers
			      :type :quantitative)))))))
```
## Layered

### Rolling average

{{< vega id="moving-average" spec="/plots/moving-average.vl.json" >}}

Plot showing a 30 day rolling average with raw values in the background.

```lisp
(plot:plot
 (vega:defplot moving-average
   `(:width 400
     :height 300
     :data ,seattle-weather
     :transform #((:window #((:field :temp-max
			      :op :mean
			      :as :rolling-mean))
		   :frame #(-15 15)))
     :encoding (:x (:field :date
		    :type :temporal
		    :title "Date")
		:y (:type :quantitative
		    :axis (:title "Max Temperature and Rolling Mean")))
     :layer #((:mark (:type :point :opacity 0.3)
	       :encoding (:y (:field :temp-max
			      :title "Max Temperature")))

	      (:mark (:type :line :color "red" :size 3)
	       :encoding (:y (:field :rolling-mean
			      :title "Rolling Mean of Max Temperature")))))))
```

### Histogram w/mean

{{< vega id="histogram-with-mean" spec="/plots/histogram-with-mean.vl.json" >}}

```lisp
(plot:plot
 (vega:defplot histogram-with-mean
   `(:data ,imdb
     :layer #((:mark :bar
	       :encoding (:x (:field :imdb-rating
			      :bin t
			      :title "IMDB Rating")
			  :y (:aggregate :count)))

	      (:mark :rule
	       :encoding (:x (:field :imdb-rating
			      :aggregate :mean
			      :title "Mean of IMDB Rating")
			  :color (:value "red")
			  :size (:value 5)))))))
```


## Interactive

This section demonstrates interactive plots.

### Scatter plot matrix

This [Vega-Lite interactive scatter plot
matrix](https://vega.github.io/vega-lite/examples/interactive_splom.html)
includes interactive elements and demonstrates creating a SPLOM
(scatter plot matrix).

{{< vega id="vgcars-splom" spec="/plots/vgcars-splom.vl.json" >}}


```lisp
(defparameter vgcars-splom
 (vega::make-plot "vgcars-splom"
		  vgcars
		  `("$schema" "https://vega.github.io/schema/vega-lite/v5.json"
			:title "Scatterplot Matrix for Vega Cars"
			:repeat (:row    #(:horsepower :acceleration :miles-per-gallon)
			         :column #(:miles-per-gallon :acceleration :horsepower))
			:spec (:data (:url "/data/vgcars-splom-data.json")
			:mark :point
			:params #((:name "brush"
			:select (:type "interval"
			         :resolve "union"
					 :on "[mousedown[event.shiftKey], window:mouseup] > window:mousemove!"
					 :translate "[mousedown[event.shiftKey], window:mouseup] > window:mousemove!"
					 :zoom "wheel![event.shiftKey]"))
				    (:name "grid"
					 :select (:type "interval"
					 :resolve "global"
					 :translate "[mousedown[!event.shiftKey], window:mouseup] > window:mousemove!"
					 :zoom "wheel![!event.shiftKey]")
					 :bind :scales))
	        :encoding (:x (:field (:repeat "column") :type "quantitative")
			           :y (:field (:repeat "row") :type "quantitative" :axis ("minExtent" 30))
					   :color (:condition (:param "brush" :field :origin :type "nominal")
					           :value "grey"))))))
(plot:plot vgcars-splom)
```

This example is one of those mentioned in the [plotting
manual](/docs/manuals/plotting/) that uses a non-standard location for
the `data` property.

### Weather exploration

{{< vega id="seattle-weather-exploration" spec="/plots/weather-exploration.vl.json" >}}

This graph shows an interactive view of Seattle’s weather, including
maximum temperature, amount of precipitation, and type of weather.  By
clicking and dragging on the scatter plot, you can see the proportion
of days in that range that have sun, rain, fog, snow, etc.

```lisp
(plot:plot
 (vega:defplot weather-exploration
   `(:title "Seattle Weather, 2012-2015"
     :data ,seattle-weather
     :vconcat #(;; upper graph
		(:encoding (:color (:condition (:param :brush
						:title "Weather"
						:field :weather
						:type :nominal
						:scale (:domain #("sun" "fog" "drizzle" "rain" "snow")
							:range #("#e7ba52", "#a7a7a7", "#aec7e8", "#1f77b4", "#9467bd")))
			            :value "lightgray")
			    :size (:field :precipitation
				   :type  :quantitative
				   :title "Precipitation"
				   :scale (:domain #(-1 50)))
			    :x (:field :date
				:time-unit :monthdate
				:title "Date"
				:axis (:format "%b"))
			    :y (:field :temp-max
				:type :quantitative
				:scale (:domain #(-5 40))
				:title "Maximum Daily Temperature (C)"))
		 :width 600
		 :height 300
		 :mark :point
		 :params #((:name :brush
			    :select (:type :interval
				     :encodings #(:x))))
		 :transform #((:filter (:param :click))))

		;; lower graph
		(:encoding (:color (:condition (:param :click
						:field :weather
						:scale (:domain #("sun", "fog", "drizzle", "rain", "snow")
							:range #("#e7ba52", "#a7a7a7", "#aec7e8", "#1f77b4", "#9467bd")))
				    :value "lightgray")
			    :x (:aggregate :count)
			    :y (:field :weather
				:title "Weather"))
		 :width 600
		 :mark :bar
		 :params #((:name :click
			   :select (:type :point
				    :encodings #(:color))))
		 :transform #((:filter (:param :brush))))))))
```

### Interactive scatterplot

{{< vega id="global-health" spec="/plots/global-health.vl.json" >}}

```lisp
(plot:plot
 (vega:defplot global-health
   `(:title "Global Health Statistics by Country and Year"
     :data ,gapminder
     :width 800
     :height 500
     :layer #((:transform #((:filter (:field :country
				      :equal "afghanistan"))
			    (:filter (:param :year)))
	       :mark (:type :text
		      :font-size 100
		      :x 420
		      :y 250
		      :opacity 0.06)
	       :encoding (:text (:field :year)))

	      (:transform #((:lookup :cluster
			     :from (:key :id
				    :fields #(:name)
				    :data (:values #(("id" 0 "name" "South Asia")
						     ("id" 1 "name" "Europe & Central Asia")
						     ("id" 2 "name" "Sub-Saharan Africa")
						     ("id" 3 "name" "America")
						     ("id" 4 "name" "East Asia & Pacific")
						     ("id" 5 "name" "Middle East & North Africa"))))))
	       :encoding (:x (:field :fertility
			      :type :quantitative
			      :scale (:domain #(0 9))
			      :axis (:tick-count 5
				     :title "Fertility"))
			  :y (:field :life-expect
			      :type :quantitative
			      :scale (:domain #(20 85))
			      :axis (:tick-count 5
				     :title "Life Expectancy")))
	       :layer #((:mark (:type :line
				:size 4
				:color "lightgray"
				:stroke-cap "round")
			 :encoding (:detail (:field :country)
				    :order (:field :year)
				    :opacity (:condition (:test (:or #((:param :hovered :empty :false)
								       (:param :clicked :empty :false)))
							  :value 0.8)
					      :value 0)))

			(:params #((:name :year
				    :value #((:year 1955))
				    :select (:type :point
					     :fields #(:year))
				    :bind (:name :year
					   :input :range
					   :min 1955
					   :max 2005
					   :step 5))
				   (:name :hovered
				    :select (:type :point
					     :fields #(:country)
					     :toggle :false
					     :on :mouseover))
				   (:name :clicked
				    :select (:type :point
					     :fields #(:country))))
			 :transform #((:filter (:param :year)))
			 :mark (:type :circle
				:size 100
				:opacity 0.9)
			 :encoding (:color (:field :name
					    :title "Region")))

			(:transform #((:filter (:and #((:param :year)
						       (:or #((:param :clicked :empty :false)
							      (:param :hovered :empty :false)))))))
			 :mark (:type :text
				:y-offset -12
				:font-size 12
				:font-weight :bold)
			 :encoding (:text (:field :country)
				    :color (:field :name
					    :title "Region")))

			(:transform #((:filter (:param :hovered :empty :false))
				      (:filter (:not (:param :year))))
			 :layer #((:mark (:type :text
					  :y-offset -12
					  :font-size 12
					  :color "gray")
				   :encoding (:text (:field :year)))
				  (:mark (:type :circle
					  :color "gray"))))))))))
```
<!--
### Airport connections

{{< vega id="us-airport-connections" spec="/plots/airport-connections.vl.json" >}}

An interactive visualization of connections among major U.S. airports
in 2008. Based on a U.S. airports example by Mike Bostock.

Note: this spec *exactly* duplicates the [vega-lite
example](https://vega.github.io/vega-lite/examples/airport_connections.html),
but does not render properly.  It appears to be an issue with loading
data.  It fails whether the data is in JSON or CSV format.  I have
confirmed that hugo is serving up the data, in both CSV and JSON
formats.

```lisp
(defparameter airport-connections
  (vega::make-plot "airport-connections"
		   nil
  '("$schema" "https://vega.github.io/schema/vega-lite/v5.json"
    :title "US Airport Connections"
    :layer #((:mark (:type :geoshape
		     :fill "#ddd"
		     :stroke "#fff"
		     :stroke-width 1)
	      :data (:url "/data/us-10m.json"
		     :format (:type :topojson
			      :feature :states)))

	     (:mark (:type :rule
		     :color "#000"
		     :opacity 0.35)
	      :data (:url "/data/flights-airports.csv")
	      :transform #((:filter (:param :org :empty :false))
			   (:lookup :origin
			    :from (:data (:url "/data/airports.csv")
				   :key "iata"
				   :fields #(:latitude :longitude)))
			   (:lookup :destination
			    :from (:data (:url "/data/airports.csv")
				   :key "iata"
				   :fields #(:latitude :longitude))
			    :as #(:lat2 :lon2)))
	      :encoding (:latitude (:field :latitude)
			 :longitude (:field :longitude)
			 :latitude2 (:field :lat2)
			 :longitude2 (:field :lon2)))

	     (:mark (:type :circle)
	      :data (:url "/data/flights-airport.csv")
	      :transform #((:aggregate #((:op :count
					  :as :routes))
			    :groupby #(:origin))
			   (:lookup :origin
			    :from (:data (:url "/data/airports.csv")
				   :key "iata"
				   :fields #(:state :latitude :longitude)))
			   (:filter "datum.state !== 'PR' && datum.state !== 'VI'"))
	      :params #((:name :org
			 :select (:type :point
				  :on :mouseover
				  :nearest t
				  :fields #(:origin))))
	      :encoding (:latitude (:field :latitude)
			 :longitude (:field :longitude)
			 :size (:field :routes
				:type :quantitative
				:scale (:range-max 1000)
				:legend nil)
			 :order (:field :routes
				 :sort :descending))))
    :projection (:type :albers-usa)
    :width 900
    :height 500)))
```
-->
### Crossfilter

{{< vega id="cross-filter" spec="/plots/cross-filter.vl.json" >}}

Cross-filtering makes it easier and more intuitive for viewers of a
plot to interact with the data and understand how one metric affects
another.  With cross-filtering, you can click a data point in one
dashboard view to have all dashboard views automatically filter on
that value.

Click and drag across one of the charts to see the other variables
filtered.

```lisp
(plot:plot
 (vega:defplot cross-filter
   `(:title "Cross filtering of flights"
     :data ,flights-2k
     :transform #((:calculate "hours(datum.date)", :as "time")) ;what does 'hours' do?
     :repeat (:column #(:distance :delay :time))
     :spec (:layer #((:params #((:name :brush
				 :select (:type :interval
					  :encodings #(:x))))
		      :mark :bar
		      :encoding (:x (:field (:repeat :column)
				     :bin (:maxbins 20))
				 :y (:aggregate :count)
				 :color (:value "#ddd")))

		     (:transform #((:filter (:param :brush)))
		      :mark :bar
		      :encoding (:x (:field (:repeat :column)
				     :bin (:maxbins 20))
				 :y (:aggregate :count))))))))
```




