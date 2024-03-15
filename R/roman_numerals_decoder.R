# roman_numerals_decoder.R

library(renv)
library(dplyr)
library(readr)
library(data.table)

renv::snapshot()

rm(list=ls());cat('\f');gc()

# https://www.codewars.com/kata/51b6249c4612257ac0000005/train/r
# https://en.wikipedia.org/wiki/Roman_numerals#Standard_form


# Reference----
romtbl <- read_tsv("base_num 	Thousands 	Hundreds 	Tens 	Units
1 	M 	C 	X 	I
2 	MM 	CC 	XX 	II
3 	MMM 	CCC 	XXX 	III
4 		CD 	XL 	IV
5 		D 	L 	V
6 		DC 	LX 	VI
7 		DCC 	LXX 	VII
8 		DCCC 	LXXX 	VIII
9 		CM 	XC 	IX")

cw.data <- data.frame(rn = c("i", "v", "x", "l", "c", "d", "m"), 
                      n = c(1, 5, 10, 50, 100, 500, 1000))

cw.data$rn_ord <- factor(cw.data$rn, 
                         levels = cw.data$rn[order(cw.data$n)]) %>%
  as.ordered()

# working----
romtbl



# Solution----

solution <- function(roman){
  # complete the solution by transforming the roman numeral into an integer
}

# Sample Tests----
test_that("Sample Tests", {
  expect_equal(solution("XXI"), 21)
  expect_equal(solution("I"), 1)
  expect_equal(solution("IV"), 4)
  expect_equal(solution('MMVIII'), 2008)
  expect_equal(solution('MDCLXVI'), 1666)
})