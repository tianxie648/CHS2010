########################################################################
#
#  CHS2010
#
#  Summary Statistics (Table A9.1-A9.3)
########################################################################

###' Cleaning and working directory
rm(list = ls())
dev.off()
setwd("D:/OneDrive - University College London/CHS2010")



###' Replicate Table A9-1, supplement
#' install.packages("modelsummary")
library("modelsummary")

#' install.packages('dplyr')
library('dplyr')

#' Load package to read .dta data
library('haven')

#' Load data
dat <- read_dta('Data/data.dta')

#' Generate the age period variable
temp_age <- data.frame(age = c(0,1,3,5,7,9,11,13), 
rep_age = c('Period 1: Year of Birth of Child',
            'Period 2: Ages 1-2',
            'Period 3: Ages 3-4',
            'Period 4: Ages 5-6',
            'Period 5: Ages 7-8',
            'Period 6: Ages 9-10',
            'Period 7: Ages 11-12',
            'Period 8: Ages 13-14')) 
dat <- merge(dat, temp_age, by = 'age')
rm(temp_age)

#' Create a function to exclude obs with -100 gestation length and generate a new variable
#' @param var string, name of the variable
#' @param data dataset
#' @return a new dataset with a new variable added
inputNaN <- function(var, data){
  data[[paste('rep_',var,sep = '')]] <- data[[var]]
  data[[paste('rep_',var,sep = '')]][data[[paste('rep_',var,sep = '')]] < -99] <- NaN
  return(data)
}

#' Create a function to make summaries
#' @param var string, name of the variable
#' @param label string, label of the variable, a string
#' @param data dataset
#' @return a table
sumtable <- function(var, label, data){
  a <- paste('(\'', label,'\' = rep_',var ,')', sep = '')
  b <- '~ rep_age * (N + Mean * Arguments(fmt = \'%.3f\')+ SD * Arguments(fmt = \'%.3f\'))'
  datasummary(as.formula(paste(a,b)),
              sparse_header = FALSE,
              data = data)
}


##' (1/9 Cognitive) Gestation length
#' Pick the first observation of the same childid, because
#' observations with the same childid have the same gestation
dat <- dat %>% group_by(childid) %>% 
  mutate(rep_first = as.numeric(row_number() == 1L) )
dat$rep_first[dat$rep_first == 0] <- NaN

#' Exclude obs with negative gestation length and make the summary
dat <- inputNaN(var = 'gestlenght', data = dat)
dat$rep_gestlenght <- dat$rep_gestlenght * dat$rep_first
sumtable(var = 'gestlenght', label = 'Gestation Length',data = dat)


##' (2/9 Cognitive) Weight at birth 
#' Pick the first observation of the same childid
data <- data %>% group_by(childid) %>% 
  mutate(rep_first = as.numeric(row_number() == 1L) )
data$rep_first[data$rep_first == 0] <- NaN

#' Exclude obs with negative weight at birth and make the summary
dat <- inputNaN(var = 'weightbirth', data = dat)
dat$rep_weightbirth <- dat$rep_weightbirth * dat$rep_first
sumtable(var = 'weightbirth', label = 'Weight at Birth',data = dat)


##' (3/9 Cognitive) Motor-social
#' Exclude obs with -100 score and make the summary
dat <- inputNaN(var = 'msd', data = dat)
sumtable(var = 'msd', label = 'Motor-Social Development Score',data = dat)


##' (4/9 Cognitive) Body parts
#' Exclude obs with -100 and make the summary
dat <- inputNaN(var = 'bp', data = dat)
sumtable(var = 'bp', label = 'Body Parts',data = dat)


##' (5/9 Cognitive) Memory for locations
#' Exclude obs with -100 and make the summary
dat <- inputNaN(var = 'ml', data = dat)
sumtable(var = 'ml', label = 'Memory for Locations',data = dat)


##' (6/9 Cognitive) Peabody picture vocabulary test
#' Exclude obs with -100 and make the summary
dat <- inputNaN(var = 'ppvt', data = dat)
sumtable(var = 'ppvt', label = 'Peabody Picture Vocabulary Test',data = dat)


##' (7/9 Cognitive) PIAT math
#' Exclude obs with -100 and make the summary
dat <- inputNaN(var = 'math', data = dat)
sumtable(var = 'math', label = 'PIAT Math',data = dat)







