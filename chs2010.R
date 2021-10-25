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
data <- read_dta('Data/data.dta')


##' (1/9 Cognitive) Gestation length
#' Pick the first observation of the same childid, because
#' observations with the same childid have the same gestation
data <- data %>% group_by(childid) %>% 
  mutate(rep_first = as.numeric(row_number() == 1L) )
data$rep_first[data$rep_first == 0] <- NaN

#' Exclude obs with negative gestation length
data$rep_first[data$gestlenght < 0] <- NaN
data$rep_gestlenght <- data$rep_first * data$gestlenght

#' Simple summary to check
datasummary(('Gestation Length (10 Weeks)' = 
               rep_gestlenght)~ (N + Mean * Arguments(fmt = "%.3f")+ 
                                    SD * Arguments(fmt = "%.3f")),
            sparse_header = FALSE,
            data = data)


##' (2/9 Cognitive) Weight at birth 
#' Pick the first observation of the same childid
data <- data %>% group_by(childid) %>% 
  mutate(rep_first = as.numeric(row_number() == 1L) )
data$rep_first[data$rep_first == 0] <- NaN

#' Exclude obs with negative weight at birth
data$rep_first[data$weightbirth < 0] <- NaN
data$rep_weightbirth <- data$rep_first * data$weightbirth

#' Simple summary to check
datasummary(('Weight at Birth' = 
               rep_weightbirth)~ (N + Mean * Arguments(fmt = "%.3f")+ 
                                    SD * Arguments(fmt = "%.3f")),
            sparse_header = FALSE,
            data = data)


##' (3/9 Cognitive) Motor-social
#' Exclude obs with -100 score
data$rep_msd <- data$msd
data$rep_msd[data$msd < -99] <- NaN

#' Simple summary to check
temp_age <- data.frame(age = c(0,1,3,5,7,9,11,13), 
                       rep_age = c('Period 1: Year of Birth of Child',
                                    'Period 2: Ages 1-2',
                                    'Period 3: Ages 3-4',
                                    'Period 4: Ages 5-6',
                                    'Period 5: Ages 7-8',
                                    'Period 6: Ages 9-10',
                                    'Period 7: Ages 11-12',
                                    'Period 8: Ages 13-14')) 
data <- merge(data, temp_age, by = 'age')
rm(temp_age)
datasummary(('Motor-Social Development Score' = 
               rep_msd)~ rep_age *
              (N + Mean * Arguments(fmt = "%.3f")+ 
                                     SD * Arguments(fmt = "%.3f")),
            sparse_header = FALSE,
            data = data)


##' (4/9 Cognitive) Body parts
#' Exclude obs with -100
data$rep_bp <- data$bp
data$rep_bp[data$bp < -99] <- NaN

#' Simple summary to check
datasummary(('Body Parts' = 
               rep_bp)~ rep_age *
              (N + Mean * Arguments(fmt = "%.3f")+ 
                 SD * Arguments(fmt = "%.3f")),
            sparse_header = FALSE,
            data = data)


##' (5/9 Cognitive) Memory for locations
#' Exclude obs with -100
data$rep_ml <- data$ml
data$rep_ml[data$ml < -99] <- NaN

#' Simple summary to check
datasummary(('Memory for Locations' = 
               rep_ml)~ rep_age *
              (N + Mean * Arguments(fmt = "%.3f")+ 
                 SD * Arguments(fmt = "%.3f")),
            sparse_header = FALSE,
            data = data)


##' (6/9 Cognitive) Peabody picture vocabulary test
#' Exclude obs with -100
data$rep_ppvt <- data$ppvt
data$rep_ppvt[data$ppvt < -99] <- NaN

#' Simple summary to check
datasummary(('Peabody Picture Vocabulary Test' = 
               rep_ppvt)~ rep_age *
              (N + Mean * Arguments(fmt = "%.3f")+ 
                 SD * Arguments(fmt = "%.3f")),
            sparse_header = FALSE,
            data = data)


##' (7/9 Cognitive) PIAT math
#' Exclude obs with -100
data$rep_math <- data$math
data$rep_math[data$math < -99] <- NaN

#' Simple summary to check
datasummary(('PIAT Math' = 
               rep_math)~ rep_age *
              (N + Mean * Arguments(fmt = "%.3f")+ 
                 SD * Arguments(fmt = "%.3f")),
            sparse_header = FALSE,
            data = data)








