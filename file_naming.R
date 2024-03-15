library(renv)
library(dplyr)


renv::status()
renv::snapshot()


makefn <- function(long_name){
  out <- long_name |>
    tolower() |>
    gsub(pattern = " ", replacement = "_")
  paste(out, ".R", sep = "", collapse = "")
}



makefn("Roman Numerals Decoder")
