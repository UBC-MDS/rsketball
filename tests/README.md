# Testing `rsketball`

`rsketball` has a scraper function `nba_scraper` that utilises the library `RSelenium`. Thus, to perform the necessary tests, it is essential to run a Docker image containing the standalone chrome browser driver.

```sh
docker run -d -p 4445:4444 --shm-size 2g selenium/standalone-chrome
```
Note the arguments to map the computer port 4445 to the Docker driver's port 4444. 

Once that is set up, you can proceed to perform testing for `rsketball` by running the following code in the project repo using the R console.

```R
devtools::test()
```
After test scraping is completed, we can shut down the Docker Container
instance. This will also ensure that your computer memory/resources are
restored.

```sh
docker stop $(docker ps -q)
```
