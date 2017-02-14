## Find the files that could not be converted to text
## And run OCR on them
## M. Espe
## Feb 2017

setwd("data/")
## Convert to text

system("for file in *.pdf; do pdftotext $file; done")

ff <- list.files(pattern = "*.txt")

docs <- lapply(ff, readLines)

lns <- sapply(docs, length)

## These need OCR
needOCR <- lns ==1
docs[needOCR]

library(Rtesseract)

## Lets see how it does right out of the box
## First convert to images
targets <- gsub("txt$", "pdf", ff[needOCR])

## This converts to png while making sure the background is converted as white,
## not transparent - seems to help with the OCR
system(paste('for file in ', paste(targets, collapse = " "), '; do convert -density 300 -background white -alpha remove "$file" "pngs/${file%.pdf}.png";  done'))

ocrTars <- list.files(path = "pngs", full.names=TRUE)

grep("-[12].png", ocrTars, value = TRUE)

## Grab just 1994 as an example
is94 <- grep("1994", ocrTars)

ans <- lapply(ocrTars[is94], ocr, pageSegMode = "psm_auto")

sents <- lapply(ans, function(x) toSent(names(x)))

hasTable <- sapply(sents, function(x) grepl("^Table [0-9]{1,2}", x))

## Try NLP on this, even with mispellings
## Using Carl's script as baseline

library(rJava)
library(NLP)
library(openNLP)
library(RWeka)

var_entityType_string = "location"

source("NLPfuns.R")
hasTable

lapply(sents[!hasTable], function(x) extractEntities(entExtract(x)))
