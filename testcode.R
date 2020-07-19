# this script contains the code I'm going to copy into the rmd file

library(tidyverse)

# download and unzip the rawdata
rawdata_url <- "https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip"
if (!file.exists("rawdata//repdataactivity.zip")){
    download.file(url = rawdata_url, destfile = "rawdata//repdataactivity.zip")
}
unzip(zipfile = "rawdata//repdataactivity.zip",
      exdir = "rawdata//repdataactivity")

activity <- read.csv(file = "rawdata//repdataactivity//activity.csv",
                     header = TRUE)
activity[ , 2] <- as.Date(activity[ , 2], format = "%Y-%m-%d")