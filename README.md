<!-- README.md is generated from README.Rmd. Please edit that file -->
rsketball
=========

Analysis of NBA players from 2001/02 to 2018/19 seasons using ESPN NBA
----------------------------------------------------------------------

This package is designated for all NBA enthusiasts! The `rsketball`
package works to scrape online tabular data from the ESPN NBA website
into a csv file. It also includes various functions to create graphs and
statistical analysis for your interest (such as boxplots, player
rankings by stats, and a summary statistics table).

An example of the ESPN NBA 2018/19 Regular season player stats can be
found in the following url:

<a href="https://www.espn.com/nba/stats/player/_/season/2019/seasontype/2" class="uri">https://www.espn.com/nba/stats/player/_/season/2019/seasontype/2</a>

Functions
---------

`nba_scraper`

-   Scrapes data from ESPN NBA data into a csv file. User can specify
    the year of the season (2001 to 2019 at the moment) and the season
    type (“regular” or “postseason”).

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

Preparation Usage for Scraping with `nba_scraper()`
---------------------------------------------------

The `rsketball::nba_scraper` is based on Selenium (or specifically
RSelenium) which enables automated web browsing through “drivers”. **To
use it, please ensure that `Docker` is installed.**

For installation instructions, please follow the [guide to Docker
installation based on your OS
type](https://ubc-mds.github.io/resources_pages/installation_instructions/).
Docker will be used to pull relevant images that when executed as
containers, will serve as the “drivers” for Selenium.

The following steps are required only for the `nba_scraper` function. If
you already have the scraped data file and wish to use the other
functions (`nba_boxplot`, `nba_rank`, `nbastats`), there is no need to
proceed with these steps.

**Step 1 (Command line/Terminal): Preparation Step (Docker container)**

Pull docker image with the following code in Terminal. We will stick to
Chrome since it seems compatible with Windows while Firefox is not.

``` sh
docker pull selenium/standalone-chrome
```

**Critical step about setting ports and memory allocation:**

We need to set up the Docker container default port 4444 to our computer
host port 4445. Keep this port number as inputs for the `nba_scraper`
function. We will also allocate the virtual memory of the container to
2Gb for it to scrape effectively.

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
# Scrape postseason season 2017/18 using "chrome" driver while saving to a local csv file.
nba_2017_playoffs <- nba_scraper(season_year = 2017, 
                                 season_type = "postseason",
                                 port=4445L, 
                                 sel_browser = "chrome",
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

R Ecosystem
-----------

This `rsketball` package aims to further gain understanding of ESPN NBA
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
