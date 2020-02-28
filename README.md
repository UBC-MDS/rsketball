<!-- README.md is generated from README.Rmd. Please edit that file -->
rsketball
=========

Analysis of NBA players from 2001/02 to 2018/19 seasons
-------------------------------------------------------

This package is designated for all NBA enthusiasts! This package works to scrap online tabular data from ESPN NBA website and uses various functions to create graphs and statistical analysis. These include boxplots, rankings, and a summary statistics table.
An example of the 2018/19 player stats can be found in the following url:

<https://www.espn.com/nba/stats/player/_/season/2019/seasontype/2>

Functions
---------

`nba_scraper`

-   Scrapes data from ESPN NBA data into a csv file. User can specify the year of the season (2016, 2017, etc) and the season type (regular or playoffs).

`nba_boxplot`

-   Creates a boxplot of the categorical variable of interest on the y-axis and the stat of interest on the x-axis.

`nba_ranking`

-   Generates a ranking and a visualization based on a column of a dataset

`nbastats`

-   Generate summary stats for NBA players. The function filters the dataset further using the arguments provided and produces a tibble with summary statistics for a list of columns of a few players or teams.

R Ecosystem
-----------

This 'rsketball' package aims to further gain understanding of ESPN NBA data and does not have a specific fit to the R ecosystem. There are currently no similar packages in R.

### Installation

You can install the released version of Rcat from [CRAN](https://cran.r-project.org/)

Binding two factors via `fbind()`:

``` r
#install.packages("rsketball")
```

And the development version from [Github](https://github.com/) with:

``` r
#install.packages("devtools")
#devtools::install_github("UBC-MDS/rsketball")
```
