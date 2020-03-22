<!-- README.md is generated from README.Rmd. Please edit that file -->
<img src="imgs/rsketball_logo.JPG" width="200" height="75" align = "right">

rsketball
=========

<!-- badges: start -->

[![R build
status](https://github.com/UBC-MDS/rsketball/workflows/R-CMD-check/badge.svg)](https://github.com/UBC-MDS/rsketball/actions)

[![codecov](https://codecov.io/gh/UBC-MDS/rsketball/branch/master/graph/badge.svg)](https://codecov.io/gh/UBC-MDS/rsketball)
<!-- badges: end -->

Analysis of NBA players from 2001/02 to 2018/19 seasons using ESPN NBA
----------------------------------------------------------------------

This package is designated for all NBA enthusiasts! The `rsketball`
package works to scrape online tabular data from the ESPN NBA website
into a csv file. It also includes various functions to create graphs and
statistical analysis for your interest (such as boxplots, player
rankings by stats, and a summary statistics table).

An example of the ESPN NBA 2018/19 Regular season player stats can be
found in this [EPSN NBA
url](https://www.espn.com/nba/stats/player/_/season/2019/seasontype/2)

This project is proudly created by: - [Carlina
Kim](https://github.com/carlinakim) - [Andres
Pitta](https://github.com/AndresPitta) - [V Anand
Shankar](https://github.com/vanandsh) - [Kenneth
Foo](https://github.com/kfoofw)

Functions
---------

`nba_scraper`

-   Scrapes data from ESPN NBA data into a csv file. User can specify
    the year of the season (2001 to 2019 at the moment) and the season
    type (“regular” or “postseason”).

`nba_boxplot`

-   Creates a boxplot of the categorical variable of interest (TEAM or
    POS) on the y-axis and the numerical statistic of interest on the
    x-axis.

`nba_ranking`

-   Generates a ranking visualization based on the numerical statistic
    of interest column of a dataset.

`nba_team_stats`

-   Generate summary stats for NBA players. The function provides
    descriptive (mean, median, 25%, and 75% quantiles) team statistics
    of NBA data. It also comes with added functionalities for player
    position grouping per team too.

For a more detailed understanding of the functions and their use cases,
please refer to the [package
vignette](https://ubc-mds.github.io/rsketball/articles/rsketball-vignette.html).

Installation
------------

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

Required Preparation for Scraping with `nba_scraper()`
------------------------------------------------------

The `rsketball::nba_scraper` is based on Selenium (or specifically
RSelenium) which enables automated web browsing through “drivers”. **To
use it, please ensure that `Docker` is installed.**

For installation instructions, please follow the [guide to Docker
installation based on your OS
type](https://ubc-mds.github.io/resources_pages/installation_instructions/).
Docker will be used to pull the relevant Chromedriver image that when
executed as containers, will serve as the “driver” for Selenium.

The following steps are required only for the `nba_scraper` function. If
you already have the scraped data file and wish to use the other
functions (`nba_boxplot`, `nba_rank`, `nbastats`), there is no need to
proceed with these steps.

**Step 1 (Command line/Terminal): Preparation of Docker container**

Pull the docker image with the following code in Terminal. We will stick
to Chrome since it seems compatible with Windows while Firefox is not.

``` sh
docker pull selenium/standalone-chrome
```

**Critical step about setting ports and memory allocation:**

We need to set up the Docker container default port 4444 to our computer
host port 4445. Keep this port number as inputs for the `nba_scraper`
function. We will also allocate 2GB of virtual memory for the container
to scrape effectively.

Run the following code in Terminal:

``` sh
docker run -d -p 4445:4444 --shm-size 2g selenium/standalone-chrome
```

Verify that the docker container is in operation by running the
following code in Terminal:

``` sh
docker ps 
```

**Step 2 (R/RStudio): Scraping with `nba_scraper`**

Now that the container is running with the allocated memory and assigned
port, we can proceed with testing

``` r
library(rsketball)

# Scrape postseason season 2017/18 while saving to a local csv file.
nba_2017_playoffs <- nba_scraper(season_year = 2017, 
                                 season_type = "postseason",
                                 port=4445L, # Port number as per Docker container setup
                                 csv_path = "nba_2017_playoffs.csv")
```

If everything was executed as intended, you should obtain a csv file
called “nba\_2017\_playoffs.csv” containing the scraped data, and a
tibble in your R environment named “nba\_2017\_playoffs”. With the
tibble, you can use the other `rsketball` functions for your analysis.

**Step 3 (Command line/Terminal): Termination of Docker Container**

After test scraping is completed, we can shut down the Docker Container
instance. This will also ensure that your computer memory/resources are
restored.

``` sh
docker stop $(docker ps -q)
```

If you wish to, you can also remove the Docker image from your computer,
where “<image_id>” represents the id of your Docker image.

``` sh
docker image rm <image_id>
```

Usage Examples
--------------

Once the Docker steps are setup with the relevant container running as
mentioned in the Preparation steps above, you can start up R.

### `nba_scraper()`

To load the package:

``` r
library(rsketball)
```

`nba_scraper()` will help you create the dataframe of the NBA season of
interest to conduct further analysis using the functions below. The
following examples is for scraping the playoffs (postseason) season in
2017/18 while saving to a local csv file.

``` r
nba_2017 <- nba_scraper(2017, season_type = "postseason",
                        port=4445L, # Port number as per Docker container setup
                        csv_path = "nba_2017_playoffs.csv")
```

Effective usage of the rest of the functions in `rsketball` may require
certain knowledge of the available columns in the scraped data. For more
context on the column names of the scraped data set, please refer to the
[dataset description
file](https://github.com/UBC-MDS/rsketball/blob/master/dataset_description.md).
This will help the user better understand what columns are included in
the scraped data, as well as what they represent.

For the illustration of the other functions, let’s create a toy dataset
with similar properties as the scraped data from ESPN NBA.

``` r
nba_data <- tibble::tibble(NAME = c("James", "Steph", "Bosh", "Klay", "Kobe"),
                    TEAM = c("MIA","GS","MIA","GS","LAL"),
                    POS = c("SF", "PG", "C", "SG", "SG"),
                    PTS = c(5,4,3,2,10),
                    TO = c(1,2,3,4,3),
                    `3PA` = c(10, 20, 30, 40, 50),
                    `FT%` = c(50, 60, 70, 80, 90))
```

### `nba_boxplot()`

To further compare the different statistics (scoring, steals, rebounds,
etc) across different teams or different player positions, you can use
`nba_boxplot()`.

To look at the distribution of Free Throws Percentage or ‘FT%’ (which is
a numerical column) for specific teams (must pass in a list).

**Important:** Since the column id (FT%) has a “%” character in it, we
must ensure that the input for stats\_column is formatted with backticks
as shown:

``` r
nba_boxplot(nba_data, 
            team_or_position= "team", 
            grouping_list = c("MIA", "GS"), 
            stats_column = `FT%`) # Formatted with backticks.
```

![](README-unnamed-chunk-7-1.png)

### `nba_ranking()`

The `nba_ranking()` function creates a visualization showing the
rankings of a category with a statistic of interest.

In the this example, we rank the top 3 players (NAME) based on their
number of Three Points Attempts (3PA) made in a descending order.

**Important:** Since the column id starts with a number 3, we must
ensure that the input for stats\_column is formatted with backticks as
shown:

``` r
# Find top 3 players for 3 Point Attempts (3PA) where higher is better
nba_ranking(nba_data,
            column = NAME,
            by = `3PA`, # Formatted with backticks.
            top = 3,
            descending = TRUE,
            FUN = mean)
```

![](README-unnamed-chunk-8-1.png)

### `nba_team_stats()`

The `nba_team_stats()` function finds statistics of mean, median, 25%,
and 75% quantiles. This function is primarily focused on team, and
allows for further grouping by player position per team.

In this example, we obtain the descriptive statistics of relevant
numeric columns (PTS and TO) for specific teams (GS and LAL) with added
grouping of their player positions (C and PG).

``` r
# Find specific stats (PTS, TO) for specific teams (GS, LAL) for specific positions (PG, SG)
nba_team_stats(nba_data, 
               stats_filter = c("PTS","TO"),
               teams_filter = c("GS","LAL"), 
               positions_filter = c("SG","PG"))
#> # A tibble: 3 x 10
#> # Groups:   TEAM [2]
#>   TEAM  POS   PTS_mean TO_mean PTS_median TO_median PTS_quantile_25
#>   <chr> <chr>    <dbl>   <dbl>      <dbl>     <dbl>           <dbl>
#> 1 GS    PG           4       2          4         2               4
#> 2 GS    SG           2       4          2         4               2
#> 3 LAL   SG          10       3         10         3              10
#> # … with 3 more variables: TO_quantile_25 <dbl>, PTS_quantile_75 <dbl>,
#> #   TO_quantile_75 <dbl>
```

For a more detailed understanding of the functions and their use cases,
please refer to the [package
vignette](https://ubc-mds.github.io/rsketball/articles/rsketball-vignette.html).

Testing
-------

To do testing of the package functions, please refer to the instructions
found in the [README.md located at the testing subdirectory
folder](https://github.com/UBC-MDS/rsketball/blob/master/tests/README.md).

R Ecosystem
-----------

This `rsketball` package aims to further gain understanding of ESPN NBA
data and does not have a specific fit to the R ecosystem. There are
currently some other library packages such as
[`nbastatR`](https://www.rdocumentation.org/packages/nbastatR/versions/0.1.10131)
that take data from other sources (NBA Stats API, Basketball Insiders,
Basketball-Reference, HoopsHype, and RealGM), but no package that we
currently know of takes data from ESPN NBA specifically.
