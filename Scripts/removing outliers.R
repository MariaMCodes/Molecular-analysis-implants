library(tidyverse)

implants_qpcr <- read_csv("Data/implants 2019.csv", na = "null")
implants_qpcr

# Boxplot to check for outliers
# Divide graph area in 2 columns
par(mfrow=c(1, 2)) 

# Boxplot for 16s copy number
boxplot(implants_qpcr$sixteens_copy_number, main="16s copy number", sub=paste("Outlier rows: ", boxplot.stats(implants_qpcr$sixteens_copy_number)$out))

# Boxplot for total bacteria number
boxplot(implants_qpcr$total_bacteria_per_mg_tissue, main="Total bacteria number", sub=paste("Outlier rows: ", boxplot.stats(implants_qpcr$total_bacteria_per_mg_tissue)$out))



# Remove outlier from 16s results
implants_qpcr2 <- implants_qpcr$sixteens_copy_number[!implants_qpcr$sixteens_copy_number %in% boxplot.stats(implants_qpcr$sixteens_copy_number)$out]

# Remove outlier from total bacteria number results
implants_qpcr3 <- implants_qpcr$total_bacteria_per_mg_tissue[!implants_qpcr$total_bacteria_per_mg_tissue %in% boxplot.stats(implants_qpcr$total_bacteria_per_mg_tissue)$out]


# Regraphing new vectors
par(mfrow=c(1, 2)) 
boxplot(implants_qpcr2, main="16s copy number", sub=paste("Outlier rows: ", boxplot.stats(implants_qpcr2)$out))
boxplot(implants_qpcr3, main="total bacteria number/mg tissue", sub=paste("Outlier rows: ", boxplot.stats(implants_qpcr3)$out))


ggplot(implants_qpcr2, aes(x = sixteens_copy_number)) +
  geom_histogram()


# Density plot to check if response variabe is close to normal
library(e1071)
par(mfrow=c(1,2))

# Density plot for 16s copy number
plot(density(implants_qpcr2), main="Density Plot: 16s copy number", ylab="Frequency", sub=paste("Skewness:", round(e1071::skewness(implants_qpcr2), 2)))

polygon(density(implants_qpcr2), col="blue")

# Density plot for total bacteria number
plot(density(implants_qpcr3), main="Density Plot: total bacteria number", ylab="Frequency", sub=paste("Skewness:", round(e1071::skewness(implants_qpcr3), 2)))

polygon(density(implants_qpcr3), col="blue")






