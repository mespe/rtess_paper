## Download all the reports
## M. Espe
## Feb 2017

library(RCurl)
library(XML)
source("getDocs.R")

u <- "http://rice.ucanr.edu/Reports-Publications/Agronomy_Papers/"

setwd("data/")
getDocs(url = u)

ll <- getURL(u)
ll <- htmlParse(ll)

## Rename files for later
## Get years for files
tmp <- xpathApply(ll, "//a[@href]",xmlValue)
tmp2 <- xpathApply(ll, "//a[@href]", xmlAttrs)
nn <- grep("[0-9]{4} Agronomy Progress Report", tmp)
fileNum <- sapply(tmp2[nn], function(x){
##    browser()
    gsub("http://rice.ucanr.edu/files/", "", x["href"])
    })

newName <- paste0(gsub(" ", "_", tmp[nn]), ".pdf")

file.rename(fileNum, newName)  


