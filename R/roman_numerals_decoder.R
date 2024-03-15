# roman_numerals_decoder.R

library(renv)
library(dplyr)
library(readr)
library(data.table)
library(testthat)

renv::snapshot()

rm(list=ls());cat('\f');gc()

# https://www.codewars.com/kata/51b6249c4612257ac0000005/train/r
# https://en.wikipedia.org/wiki/Roman_numerals#Standard_form


# Reference----
romtbl <- read_tsv("base_num 	Thousands 	Hundreds 	Tens 	Ones
1 	M 	C 	X 	I
2 	MM 	CC 	XX 	II
3 	MMM 	CCC 	XXX 	III
4 		CD 	XL 	IV
5 		D 	L 	V
6 		DC 	LX 	VI
7 		DCC 	LXX 	VII
8 		DCCC 	LXXX 	VIII
9 		CM 	XC 	IX")

# cw.data <- data.frame(rn = c("i", "v", "x", "l", "c", "d", "m"), 
#                       n = c(1, 5, 10, 50, 100, 500, 1000))
# 
# cw.data$rn_ord <- factor(cw.data$rn, 
#                          levels = cw.data$rn[order(cw.data$n)]) %>%
#   as.ordered()

cw.base10 <- data.frame(variable = c("Thousands", "Hundreds", "Tens", "Ones"), 
                        base10   = c(1000, 100, 10, 1))


# working----
romtbl2 <- romtbl %>%
  as.data.table() %>%
  melt(., 
       id.vars = c("base_num")) %>%
  as.data.frame() %>%
  as_tibble()

romtbl2$var_o <- factor(romtbl2$variable, 
                        levels = rev(c("Thousands", "Hundreds", "Tens", "Ones"))) %>%
  as.ordered()

romtbl2 <- romtbl2[!is.na(romtbl2$value),]

romtbl2[romtbl2$variable == "Ones",]$value <- paste(romtbl2[romtbl2$variable == "Ones",]$value, 
                                                    "$", sep = "")

romtbl2$rn_o <- factor(romtbl2$value, 
                       levels = rev(unique(romtbl2$value[order(romtbl2$var_o, 
                                                           romtbl2$base_num, 
                                                           decreasing = T)]))) %>%
  as.ordered()


romtbl2 <- romtbl2[order(romtbl2$var_o, 
                         romtbl2$base_num, 
                         decreasing = T),]
romtbl2$var_o
romtbl2$rn_o



romtbl3 <- left_join(romtbl2, 
                     cw.base10) %>%
  mutate(., 
         numval = base_num * base10) %>%
  .[,c("rn_o", "numval")]



# # test----
roman <- "XXI"  %>%
  toupper()


# cw_base10.2 <- romtbl2 %>%
#   group_by(base_num, variable, value) %>%
#   summarise()

out.b10n <- NULL
for(i in 1:nrow(romtbl3)){
  
  if(grepl(pattern = romtbl3[i,]$rn_o, 
        x = roman)){
    #stop("it works")
    # print(i)
    # print(as.character(romtbl3[i,]$rn_o))
    
    out.b10n <- rbind(out.b10n, 
                      data.frame(rn_o   = romtbl3[i,]$rn_o, 
                                 numval = romtbl3[i,]$numval))
    
  }
          
        
}

out.b10n$base_val <- nchar(out.b10n$numval) %>% as.character()

out.b10n2 <- slice_max(group_by(out.b10n, base_val), 
          order_by = numval, 
          n = 1)

out.number <- sum(out.b10n2$numval)
(out.number)

# Solution----

solution <- function(roman){
  # complete the solution by transforming the roman numeral into an integer
  romtbl <- read_tsv("base_num 	Thousands 	Hundreds 	Tens 	Ones
1 	M 	C 	X 	I
2 	MM 	CC 	XX 	II
3 	MMM 	CCC 	XXX 	III
4 		CD 	XL 	IV
5 		D 	L 	V
6 		DC 	LX 	VI
7 		DCC 	LXX 	VII
8 		DCCC 	LXXX 	VIII
9 		CM 	XC 	IX")
  
  # cw.data <- data.frame(rn = c("i", "v", "x", "l", "c", "d", "m"), 
  #                       n = c(1, 5, 10, 50, 100, 500, 1000))
  # 
  # cw.data$rn_ord <- factor(cw.data$rn, 
  #                          levels = cw.data$rn[order(cw.data$n)]) %>%
  #   as.ordered()
  
  cw.base10 <- data.frame(variable = c("Thousands", "Hundreds", "Tens", "Ones"), 
                          base10   = c(1000, 100, 10, 1))
  
  
  # working----
  romtbl2 <- romtbl %>%
    as.data.table() %>%
    melt(., 
         id.vars = c("base_num")) %>%
    as.data.frame() %>%
    as_tibble()
  
  romtbl2$var_o <- factor(romtbl2$variable, 
                          levels = rev(c("Thousands", "Hundreds", "Tens", "Ones"))) %>%
    as.ordered()
  
  romtbl2 <- romtbl2[!is.na(romtbl2$value),]
  
  romtbl2[romtbl2$variable == "Ones",]$value <- paste(romtbl2[romtbl2$variable == "Ones",]$value, 
                                                      "$", sep = "")
  
  romtbl2$rn_o <- factor(romtbl2$value, 
                         levels = rev(unique(romtbl2$value[order(romtbl2$var_o, 
                                                                 romtbl2$base_num, 
                                                                 decreasing = T)]))) %>%
    as.ordered()
  
  
  romtbl2 <- romtbl2[order(romtbl2$var_o, 
                           romtbl2$base_num, 
                           decreasing = T),]
  romtbl2$var_o
  romtbl2$rn_o
  
  
  
  romtbl3 <- left_join(romtbl2, 
                       cw.base10) %>%
    mutate(., 
           numval = base_num * base10) %>%
    .[,c("rn_o", "numval")]
  
  
  
  # # test----
  # roman <- "MMVIII"  %>%
  #   toupper()
  
  
  # cw_base10.2 <- romtbl2 %>%
  #   group_by(base_num, variable, value) %>%
  #   summarise()
  
  out.b10n <- NULL
  for(i in 1:nrow(romtbl3)){
    
    if(grepl(pattern = romtbl3[i,]$rn_o, 
             x = roman)){
      #stop("it works")
      #print(i)
      #print(as.character(romtbl3[i,]$rn_o))
      
      out.b10n <- rbind(out.b10n, 
                        data.frame(rn_o   = romtbl3[i,]$rn_o, 
                                   numval = romtbl3[i,]$numval))
      
    }
    
    
  }
  
  out.b10n$base_val <- nchar(out.b10n$numval) %>% as.character()
  
  out.b10n2 <- slice_max(group_by(out.b10n, base_val), 
                         order_by = numval, 
                         n = 1)
  
  out.number <- sum(out.b10n2$numval)
  return(out.number)
}



# Sample Tests----
test_that("Sample Tests", {
  expect_equal(solution("XXI"), 21)
  expect_equal(solution("I"), 1)
  expect_equal(solution("IV"), 4)
  expect_equal(solution('MMVIII'), 2008)
  expect_equal(solution('MDCLXVI'), 1666)
})
