library(tidyverse)
library(haven)
library(dplyr)


filer <- list.files(
  path = "Z:\\Data på Surveybanken\\Oprindelige datafiler!!!\\Datafiler_monadisk",
  pattern = "\\.sav$",
  full.names = TRUE,
  ignore.case = TRUE
)

for (fil in filer) {
  navn <- tools::file_path_sans_ext(basename(fil))
  assign(navn, read_sav(fil))
}

getwd()
