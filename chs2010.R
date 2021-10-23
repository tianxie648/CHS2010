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

## Gestation length
# Pick the first observation of the same childid, because
# observations with the same childid have the same gestation
data <- data %>% group_by(childid) %>% 
  mutate(test_gestlenght = as.numeric(row_number() == 1L) )
data$test_gestlenght[data$test_gestlenght == 0] <- NaN

# Exclude obs with negative gestation length
data$test_gestlenght[data$gestlenght < 0] <- NaN
data$test_gestlenght <- data$test_gestlenght * data$gestlenght

# Simple summary to check
datasummary(('Gestation Length (10 Weeks)' = 
               test_gestlenght)~ (N + Mean * Arguments(fmt = "%.3f")+ 
                                    SD * Arguments(fmt = "%.3f")),
            sparse_header = FALSE,
            data = data)




