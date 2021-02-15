library(tidyverse)

library(ggplot2)

implants_qpcr <- read_csv("Data/implants 2019.csv", na = "null")
implants_qpcr

qplot(data = implants_qpcr, x = sixteens_copy_number) + ylab("16s copy number")


qplot(data = implants_qpcr, x = sixteens_copy_number) + ylab("16s copy number") + 
  xlim(c(0, 250000))


qplot(data = implants_qpcr, x = sixteens_copy_number) + ylab("16s copy number") + 
  xlim(c(0, 120000))


qplot(data = implants_qpcr, x = sixteens_copy_number) + ylab("16s copy number") + 
  xlim(c(0, 70000))

qplot(data = implants_qpcr, x = sixteens_copy_number) + ylab("16s copy number") + 
  xlim(c(0, 45000))




## outlier cut-offs
library(data.table)
outlierReplace = function(dataframe, cols, rows, newValue = NA) {
  if (any(rows)) {
    set(dataframe, rows, cols, newValue)
  }
}

outlierReplace(implants_qpcr, "16s copy number", which(implants_qpcr$sixteens_copy_number > 45000), NA)

qplot(data = implants_qpcr, x = sixteens_copy_number) + ylab("16s copy number")
