library(tidyverse)

implants_qpcr <- read_csv("Data/implants 2019.csv", na = "null")
implants_qpcr

## Used this website https://rpubs.com/Mentors_Ubiqum/removing_outliers

## outliers for 16s data
boxplot(implants_qpcr$sixteens_copy_number)$out

## if don't want to see plot again
boxplot(implants_qpcr$sixteens_copy_number, plot=FALSE)$out

## assign the outlier values into a vector
outliers <- boxplot(implants_qpcr$sixteens_copy_number, plot=FALSE)$out

print(outliers)

## find out which rows contain outliers
implants_qpcr[which(implants_qpcr$sixteens_copy_number %in% outliers),]


## remove rows containing outliers
implants_qpcr <- implants_qpcr[-which(implants_qpcr$sixteens_copy_number %in% outliers),]

## check with boxplot
boxplot(implants_qpcr$sixteens_copy_number)


library(ggplot2)

qplot(data = implants_qpcr, x = sixteens_copy_number) + ylab("16s copy number")


ggplot(data = implants_qpcr, aes(x = diagnosis, y = sixteens_copy_number)) +    
  geom_jitter(alpha = 0.5, width = 0.2, aes(color = implant_surface), height = 0.2) +                          # added height so doesnt obscure data because of jitters you've added
  geom_boxplot(alpha = 0)


ggplot(data = implants_qpcr, aes(x = diagnosis, y = sixteens_copy_number)) +    
  geom_jitter(alpha = 0.5, width = 0.2, aes(color = implant_brand), height = 0.2) +                          # added height so doesnt obscure data because of jitters you've added
  geom_boxplot(alpha = 0)


ggplot(data = implants_qpcr, aes(x = diagnosis, y = sixteens_copy_number)) +    
  geom_jitter(alpha = 0.5, width = 0.2, aes(color = ruptured), height = 0.2) +                          # added height so doesnt obscure data because of jitters you've added
  geom_boxplot(alpha = 0)
