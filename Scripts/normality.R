install.packages("dplyr")
library("dplyr")

my_data <- implants_qpcr

## check data - random sample of 10 rows
set.seed(1234)
dplyr::sample_n(my_data, 10)


library("ggpubr")

## visualise by density plot if data normal
ggdensity(my_data$sixteens_copy_number, 
          main = "Density plot of 16s copy number",
          xlab = "16s copy number")


ggqqplot(my_data$sixteens_copy_number)


## normality test
shapiro.test(my_data$sixteens_copy_number)

## If the p-value > 0.05 implying that the distribution of the data are not significantly 
## different from normal distribution. In other words, we can assume the normality.
