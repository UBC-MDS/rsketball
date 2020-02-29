<!-- README.md is generated from README.Rmd. Please edit that file -->
rsketball
=========

Analysis of NBA players from 2001/02 to 2018/19 seasons using ESPN NBA
----------------------------------------------------------------------

This package is designated for all NBA enthusiasts! This package works
to scrape online tabular data from ESPN NBA website into a csv file. It
also includes various functions to create graphs and statistical
analysis for your interest (such as boxplots, player rankings by stats,
and a summary statistics table).

An example of the ESPN NBA 2018/19 Regular season player stats can be
found in the following url:

<a href="https://www.espn.com/nba/stats/player/_/season/2019/seasontype/2" class="uri">https://www.espn.com/nba/stats/player/_/season/2019/seasontype/2</a>

Functions
---------

`nba_scraper`

-   Scrapes data from ESPN NBA data into a csv file. User can specify
    the year of the season (2016, 2017, etc) and the season type
    (regular or playoffs).

`nba_boxplot`

-   Creates a boxplot of the categorical variable of interest on the
    y-axis and the stat of interest on the x-axis.

`nba_ranking`

-   Generates a ranking and a visualization based on a column of a
    dataset

`nbastats`

-   Generate summary stats for NBA players. The function filters the
    dataset further using the arguments provided and produces a tibble
    with summary statistics for a list of columns of a few players or
    teams.

R Ecosystem
-----------

This ‘rsketball’ package aims to further gain understanding of ESPN NBA
data and does not have a specific fit to the R ecosystem. There are
currently some other library packages such as
[`nbastatR`](https://www.rdocumentation.org/packages/nbastatR/versions/0.1.10131)
that take data from other sources (NBA Stats API, Basketball Insiders,
Basketball-Reference, HoopsHype, and RealGM), but no package that we
currently know of takes data from ESPN NBA specifically.

### Installation

`rsketball` is still in project development. We estimate that by end
March 2020, one can install the released version of `rsketball` from
[CRAN](https://cran.r-project.org/).

Package installation in R:

``` r
install.packages("rsketball")
```

And the development version from [Github](https://github.com/) with:

``` r
install.packages("devtools")
devtools::install_github("UBC-MDS/rsketball")
```
