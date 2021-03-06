% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/nba_scraper.R
\name{nba_scraper}
\alias{nba_scraper}
\title{Scrape NBA statistics data from ESPN into tibble and CSV file}
\usage{
nba_scraper(
  season_year = 2018,
  season_type = "regular",
  port = 4445L,
  csv_path = NULL
)
}
\arguments{
\item{season_year}{int from 2001 to 2019 (upper limit based on latest year)}

\item{season_type}{string. Either "regular" or "postseason".}

\item{port}{int with L suffix. Must not be negative. Should be same as port configuration used
for Docker container driver setup.}

\item{csv_path}{string for csv file. Defaults to NULL. If specified, must end with ".csv".}
}
\value{
A tibble of scraped ESPN NBA data
}
\description{
This function scrapes the tabular data from the ESPN NBA website using RSelenium and
returns a tibble object of the scraped data. Users can specify the season year and
season type. Users should also specify the port for Selenium driver, and should be the
same as the port number used for setting up the Docker container driver (if the user
is using Docker as per the "rsketball" package repo instructions). By default, the
function will not write to csv until a string input for "csv_path" is given.
}
\details{
For detailed use cases, please refer to the vignette:
https://ubc-mds.github.io/rsketball/articles/rsketball-vignette.html
}
\examples{
\donttest{
# Scrape regular season 2018/19 without saving to a csv file
nba_2018 <- nba_scraper(2018, season_type = "regular", port=4445L)


# Scrape playoffs season 2017/18 while saving to a local csv file.
nba_2017 <- nba_scraper(2017, season_type = "postseason",
                        port=4445L,
                        csv_path = "nba_2017_playoffs.csv")
}
}
