# coalition-conflict-status

Data, code and manuscript for "Coalitions & conflict: A longitudinal analysis of male politics", published in Evolutionary Human Sciences

============

# Requirements for analyses:

- R (v3.6.3 or greater): https://cran.r-project.org
- RSiena (1.2-25 or greater): https://www.stats.ox.ac.uk/~snijders/siena/

# Packages used for visualisation:

- corrplot: https://cran.r-project.org/web/packages/corrplot/corrplot.pdf
- grid: https://cran.r-project.org/web/packages/grid/index.html
- iGraph: https://igraph.org/r/
- Rcolorbrewer: https://cran.r-project.org/web/packages/RColorBrewer/index.html
- tidyverse: https://www.tidyverse.org
- viridis: https://cran.r-project.org/web/packages/viridis/index.html

# Instructions:

In R, set the working directory to that containing this readme file. On Mac, for example, you could say

```
setwd('~/Desktop/coalition-conflicts-status')
```

Check to see if you're in the right place by typing dir() and see whether this readme file is present.


The longitudinal analysis uses thirteen data files as input:

```
'v1-attributes.csv' - Includes status, physical strength and size.
'v1-coalition-1.csv' - The coalition network at time point 1 (2009).
'v1-coalition-2.csv' - The coalition network at time point 2 (2014).
'v1-coalition-3.csv' - The coalition network at time point 3 (2017).
'v1-composition.txt' - Village 1's composition list.
'v1-conflict-1.csv' - The conflict network at time point 1 (2009).
'v1-conflict-2.csv' - The conflict network at time point 2 (2014).
'v1-conflict-3.csv' - The conflict network at time point 3 (2017).
'v1-cooperation-1.csv' - The food sharing and production partnership network at time point 1 (2009).
'v1-cooperation-2.csv' - The food sharing and production partnership network at time point 2 (2014).
'v1-kinship-1.csv' - The kinship network at time point 1 (2009).
'v1-kinship-2.csv' - The kinship network at time point 2 (2014).

```

Please note that only data from the first two time points are used for varying covariates, and varying dyadic covariates (e.g., food-sharing and production, and kinship), in the analysis. Another important note is that the rows in the networks (bar kinship) represent the nominating individuals. At each time point rows do not change and represent the same individuals as the rows in the attributes files.

Please also note that the detailed individual-level conflict data used to create Figure 1 are not freely available due to the sensitivity of the data.

When the project folder is the working directory, the longitudinal analysis may run itself (assuming that you have installed all of the dependencies) by calling

```
source('./scripts/2-village1-analyses.R')
```

However, I would advise to open the analysis script and run it in blocks.

The cross-sectional analysis uses five data files as input:

```
'v2-attributes.csv' - Includes status, physical strength and size, and log age.  
'v2-coalition.csv' - The coalition network.  
'v2-conflict.csv' - The conflict network.  
'v2-cooperation.csv' - The food sharing and production partnership network.  
'v2-kinship.csv' - The kinship network.  
```

When the project folder is the working directory, the cross-sectional analysis may run itself (assuming that you have installed all of the dependencies) by calling

```
source('./scripts/2-village2-analyses.R')
```

However, I would advise to open the analysis script and run it in blocks.

The analyses may take some time. The total time until completion may vary by machine.

To allow for reproduction of the figures and tables in the manuscript, I have included the following script:

```
3-descriptives-plots.R
```

There is code to produce figure 1 (but the data is not available). I have commented out this section so as to allow the script to run (once all analyses are complete) in its entirety by calling

```
source('3-descriptives-plots.R')
```

I advise to run the script in blocks once the analyses have finished running. I include the code for figure 1 in the hope that it will be useful for anyone who wishes to adapt the code to produce a similar figure for their own projects.

The project is maintained by Daniel Redhead (daniel_redhead@eva.mpg.de) and is hosted at https://github.com/danielRedhead
