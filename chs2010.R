########################################################################
#
#  CHS2010
#
#  Summary Statistics
########################################################################

### Cleaning and working directory
rm(list = ls())
dev.off()
setwd("D:/OneDrive - University College London/CHS2010")



### Replicate Table A9-1, supplement
# install.packages("modelsummary")
library("modelsummary")

# install.packages('dplyr')
library('dplyr')

# Load package to read .dta data
library('haven')

# Load data
data <- read_dta('Data/data.dta')


## (1/9 Cognitive) Gestation length
# Pick the first observation of the same childid, because
# observations with the same childid have the same gestation
data <- data %>% group_by(childid) %>% 
  mutate(test_first = as.numeric(row_number() == 1L) )
data$test_first[data$test_first == 0] <- NaN

# Exclude obs with negative gestation length
data$test_first[data$gestlenght < 0] <- NaN
data$test_gestlenght <- data$test_first * data$gestlenght

# Simple summary to check
datasummary(('Gestation Length (10 Weeks)' = 
               test_gestlenght)~ (N + Mean * Arguments(fmt = "%.3f")+ 
                                    SD * Arguments(fmt = "%.3f")),
            sparse_header = FALSE,
            data = data)


## (2/9 Cognitive) Weight at birth 
# Pick the first observation of the same childid
data <- data %>% group_by(childid) %>% 
  mutate(test_first = as.numeric(row_number() == 1L) )
data$test_first[data$test_first == 0] <- NaN

# Exclude obs with negative weight at birth
data$test_first[data$weightbirth < 0] <- NaN
data$test_weightbirth <- data$test_first * data$weightbirth

# Simple summary to check
datasummary(('Weight at Birth' = 
               test_weightbirth)~ (N + Mean * Arguments(fmt = "%.3f")+ 
                                    SD * Arguments(fmt = "%.3f")),
            sparse_header = FALSE,
            data = data)


## (3/9 Cognitive) Motor-social
# Exclude obs with -100 score
data$test_msd <- data$msd
data$test_msd[data$msd < -99] <- NaN

# Simple summary to check
temp_age <- data.frame(age = c(0,1,3,5,7,9,11,13), 
                       test_age = c('Period 1: Year of Birth of Child',
                                    'Period 2: Ages 1-2',
                                    'Period 3: Ages 3-4',
                                    'Period 4: Ages 5-6',
                                    'Period 5: Ages 7-8',
                                    'Period 6: Ages 9-10',
                                    'Period 7: Ages 11-12',
                                    'Period 8: Ages 13-14')) 
data <- merge(data, temp_age, by = 'age')
datasummary(('Motor-Social Development Score' = 
               test_msd)~ test_age *
              (N + Mean * Arguments(fmt = "%.3f")+ 
                                     SD * Arguments(fmt = "%.3f")),
            sparse_header = FALSE,
            data = data)


## (4/9 Cognitive) Body parts
# Exclude obs with -100
data$test_bp <- data$bp
data$test_bp[data$bp < -99] <- NaN

# Simple summary to check
datasummary(('Body Parts' = 
               test_bp)~ test_age *
              (N + Mean * Arguments(fmt = "%.3f")+ 
                 SD * Arguments(fmt = "%.3f")),
            sparse_header = FALSE,
            data = data)


## (5/9 Cognitive) Memory for locations
# Exclude obs with -100
data$test_ml <- data$ml
data$test_ml[data$ml < -99] <- NaN

# Simple summary to check
datasummary(('Memory for Locations' = 
               test_ml)~ test_age *
              (N + Mean * Arguments(fmt = "%.3f")+ 
                 SD * Arguments(fmt = "%.3f")),
            sparse_header = FALSE,
            data = data)


## (6/9 Cognitive) Peabody picture vocabulary test
# Exclude obs with -100
data$test_ppvt <- data$ppvt
data$test_ppvt[data$ppvt < -99] <- NaN

# Simple summary to check
datasummary(('Peabody Picture Vocabulary Test' = 
               test_ppvt)~ test_age *
              (N + Mean * Arguments(fmt = "%.3f")+ 
                 SD * Arguments(fmt = "%.3f")),
            sparse_header = FALSE,
            data = data)


