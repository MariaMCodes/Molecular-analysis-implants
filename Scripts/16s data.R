library(tidyverse)
library(ggplot2)

implants_qpcr <- read_csv("Data/implants 2019.csv", na = "null")
implants_qpcr

## Checking data
## Barplots to see counts on diagnosis vs implant brand and diagnosis vs implant surface
ggplot(implants_qpcr, aes(x = implant_surface)) + geom_bar(aes(fill = diagnosis), position = "dodge") + 
  labs(x = "Implant diagnosis", y = "Count") +
  facet_grid(~diagnosis)   

#########################################################################################################

## Checking outliers for 16s data
boxplot(implants_qpcr$sixteens_copy_number)$out

## if don't want to see plot again
boxplot(implants_qpcr$sixteens_copy_number, plot=FALSE)$out

## assign the outlier values into a vector
outliers_16s <- boxplot(implants_qpcr$sixteens_copy_number, plot=FALSE)$out

print(outliers_16s)

## find out which rows contain outliers
implants_qpcr[which(implants_qpcr$sixteens_copy_number %in% outliers_16s),]

## remove rows containing outliers
implants_qpcr <- implants_qpcr[-which(implants_qpcr$sixteens_copy_number %in% outliers_16s),]

## check with boxplot
boxplot(implants_qpcr$sixteens_copy_number)

#########################################################################################################

## looking at data now
qplot(data = implants_qpcr, x = sixteens_copy_number) + ylab("16s copy number") + labs(x = "16s copy number", y = "16s copy number")

ggplot(data = implants_qpcr, aes(x = diagnosis, y = sixteens_copy_number)) +    
  geom_jitter(alpha = 0.5, width = 0.2, aes(color = implant_surface), height = 0.2) +       # added height so doesnt obscure data because of jitters you've added
  geom_boxplot(alpha = 0)+
  labs(x = "Diagnosis", y = "16s copy number") +
  rotate_x_text(angle = 45)+
  ylim(0, 30000)

ggplot(data = implants_qpcr, aes(x = diagnosis, y = sixteens_copy_number)) +    
  geom_jitter(alpha = 0.7, width = 0.2, aes(color = implant_brand), height = 0.2) +         # added height so doesnt obscure data because of jitters you've added
  geom_boxplot(alpha = 0)+
  labs(x = "Diagnosis", y = "16s copy number") +
  rotate_x_text(angle = 45)+
  ylim(0, 30000)

ggplot(data = implants_qpcr, aes(x = diagnosis, y = sixteens_copy_number)) +    
  geom_jitter(alpha = 0.5, width = 0.2, aes(color = ruptured), height = 0.2) +              # added height so doesnt obscure data because of jitters you've added
  geom_boxplot(alpha = 0)+
  labs(x = "Diagnosis", y = "16s copy number") +
  rotate_x_text(angle = 45)+
  ylim(0, 30000)


ggplot(data = implants_qpcr, aes(x = implant_brand, y = sixteens_copy_number)) +    
  geom_jitter(alpha = 0.5, width = 0.2, aes(color = ruptured), height = 0.2) +              # added height so doesnt obscure data because of jitters you've added
  geom_boxplot(alpha = 0)+
  labs(x = "Implant brand", y = "16s copy number") +
  rotate_x_text(angle = 45)+
  ylim(0, 30000)

#########################################################################################################

## Checking normality
library("dplyr")
library("ggpubr")

my_data <- implants_qpcr

## visualise by density plot if data normal
ggdensity(my_data$sixteens_copy_number, 
          main = "Density plot of 16s copy number",
          xlab = "16s copy number")


ggqqplot(my_data$sixteens_copy_number)


## normality test
shapiro.test(my_data$sixteens_copy_number)

## Shapiro-Wilk normality test
## data:  my_data$sixteens_copy_number
## W = 0.83213, p-value = 5.769e-14         # Data is not normal since p < 0.05

## If the p-value > 0.05 implying that the distribution of the data are not significantly 
## different from normal distribution. In other words, we can assume the normality.

#########################################################################################################

## FOR DIAGNOSIS
## Default method = "kruskal.test" for multiple groups and since our data is not normal, can use this
ggboxplot(my_data, x = "diagnosis", y = "sixteens_copy_number",
          color = "diagnosis", palette = "jco", legend = "none")+
  rotate_x_text(angle = 45) +
  labs(x = "Diagnosis", y = "16s copy number") +
  ggtitle("16s copy number of implants") +
  theme(plot.title = element_text(hjust = 0.5, size = 12, face  = "bold")) +
  ylim(0, 30000) +
  stat_compare_means()


## Multiple pairwise tests against all (base-mean)
ggboxplot(my_data, x = "diagnosis", y = "sixteens_copy_number", color = "diagnosis", 
          add = "jitter", legend = "none") +
  rotate_x_text(angle = 45)+
  labs(x = "Diagnosis", y = "16s copy number") +
  ggtitle("16s copy number of implants") +
  theme(plot.title = element_text(hjust = 0.5, size = 12, face  = "bold")) +
  stat_compare_means(method = "kruskal.test", label.y = 30000)+   
  stat_compare_means(label = "p.signif", method = "wilcox.test",
                     ref.group = ".all.")      # Pairwise comparison against all


ggboxplot(my_data, x = "diagnosis", y = "sixteens_copy_number", color = "diagnosis", 
          add = "jitter", legend = "none") +
  rotate_x_text(angle = 45)+
  labs(x = "Diagnosis", y = "16s copy number") +
  ggtitle("16s copy numbers present in implants explanted \n from patients with various diagnoses") +  ## \n for next line
  theme(plot.title = element_text(hjust = 0.5, size = 12, face  = "bold")) +
  stat_compare_means(method = "kruskal.test", label.y = 30000)+       
  stat_compare_means(label = "p.signif", method = "wilcox.test",
                     ref.group = ".all.", hide.ns = TRUE)  


## Multiple grouping variables
compare_means(sixteens_copy_number ~ implant_surface, data = my_data, 
              group.by = "diagnosis")

# Box plot facetted by "diagnosis" with implant surface
p <- ggboxplot(my_data, x = "implant_surface", y = "sixteens_copy_number",
               color = "implant_surface", palette = "jco",
               add = "jitter", 
               facet.by = "diagnosis", short.panel.labs = FALSE, legend = "none")+
               labs(x = "Implant surface", y = "16s copy number")+
               ggtitle("Boxplot facetted by diagnosis: \n 16s copy numbers present in textured vs smooth implants") +  ## \n for next line
               theme(plot.title = element_text(hjust = 0.5, size = 12, face  = "bold")) +
               ylim(0, 30000)
# Use only p.format as label. Remove method name.
p + stat_compare_means(label = "p.format", label.x = 1.5)

# Or use significance symbol as label
p + stat_compare_means(label =  "p.signif", label.x = 1.5)



## Multiple grouping variables
compare_means(sixteens_copy_number ~ ruptured, data = my_data, 
              group.by = "diagnosis")
# Box plot facetted by "diagnosis" with ruptured
p <- ggboxplot(my_data, x = "ruptured", y = "sixteens_copy_number",
               color = "ruptured", palette = "jco",
               add = "jitter",
               facet.by = "diagnosis", short.panel.labs = FALSE, legend = "none")+
               labs(x = "Ruptured", y = "16s copy number")+
               ggtitle("Boxplot facetted by diagnosis: \n 16s copy numbers present in ruptured vs non-ruptured implants") +
               theme(plot.title = element_text(hjust = 0.5, size = 12, face  = "bold")) +
               ylim(0, 30000)
# Use only p.format as label. Remove method name.
p + stat_compare_means(label = "p.format", label.x = 1.5)

# Or use significance symbol as label
p + stat_compare_means(label =  "p.signif", label.x = 1.5)


## Multiple grouping variables
compare_means(sixteens_copy_number ~ surface, data = my_data, 
              group.by = "diagnosis")                                # surface is bottom, top, whole not paired test

# Box plot facetted by "diagnosis" with implant surface
p <- ggboxplot(my_data, x = "surface", y = "sixteens_copy_number",
               color = "surface", palette = "jco",
               add = "jitter",
               facet.by = "diagnosis", short.panel.labs = FALSE, legend = "none")+
               labs(x = "Section", y = "16s copy number")+
               ggtitle("Boxplot facetted by diagnosis: \n 16s copy numbers present in the bottom, top or whole section of implants") +
               theme(plot.title = element_text(hjust = 0.5, size = 12, face  = "bold")) +
               ylim(0, 30000)
# Use only p.format as label. Remove method name.
p + stat_compare_means(label = "p.format", label.x = 2)

# Or use significance symbol as label
p + stat_compare_means(label =  "p.signif", label.x = 2)


#########################################################################################################

## FOR IMPLANT BRAND
## Default method = "kruskal.test" for multiple groups and since our data is not normal, can use this
ggboxplot(my_data, x = "implant_brand", y = "sixteens_copy_number",
          color = "implant_brand", palette = "jco", legend = "none")+
  labs(x = "Implant brand", y = "16s copy number") +
  ggtitle("16s copy number of implants") +
  theme(plot.title = element_text(hjust = 0.5, size = 12, face  = "bold")) +
  stat_compare_means()


## Multiple pairwise tests against all (base-mean)
ggboxplot(my_data, x = "implant_brand", y = "sixteens_copy_number", color = "implant_brand", 
          add = "jitter", legend = "none") +
  rotate_x_text(angle = 45)+
  labs(x = "Implant brand", y = "16s copy number") +
  ggtitle("16s copy number of implants") +
  theme(plot.title = element_text(hjust = 0.5, size = 12, face  = "bold")) +
  stat_compare_means(method = "kruskal.test", label.y = 30000)+   
  stat_compare_means(label = "p.signif", method = "wilcox.test",
                     ref.group = ".all.")      # Pairwise comparison against all


ggboxplot(my_data, x = "implant_brand", y = "sixteens_copy_number", color = "implant_brand", 
          add = "jitter", legend = "none") +
  rotate_x_text(angle = 45)+
  labs(x = "Implant brand", y = "16s copy number") +
  ggtitle("16s copy numbers present in the different implant brands") +  ## \n for next line
  theme(plot.title = element_text(hjust = 0.5, size = 12, face  = "bold")) +
  stat_compare_means(method = "kruskal.test", label.y = 30000)+       
  stat_compare_means(label = "p.signif", method = "wilcox.test",
                     ref.group = ".all.", hide.ns = TRUE)  


## Multiple grouping variables
compare_means(sixteens_copy_number ~ implant_surface, data = my_data, 
              group.by = "implant_brand")

# Box plot facetted by "implant brand" with implant surface
p <- ggboxplot(my_data, x = "implant_surface", y = "sixteens_copy_number",
               color = "implant_surface", palette = "jco",
               add = "jitter",
               facet.by = "implant_brand", short.panel.labs = FALSE, legend = "none")+
               labs(x = "Implant surface", y = "16s copy number")+
               ggtitle("Boxplot facetted by implant brand: \n 16s copy numbers present in textured vs smooth implants") +
               theme(plot.title = element_text(hjust = 0.5, size = 12, face  = "bold")) +
               ylim(0, 30000)
# Use only p.format as label. Remove method name.
p + stat_compare_means(label = "p.format", label.x = 1.5)

# Or use significance symbol as label
p + stat_compare_means(label =  "p.signif", label.x = 1.5)



## Multiple grouping variables
compare_means(sixteens_copy_number ~ ruptured, data = my_data, 
              group.by = "implant_brand")
# Box plot facetted by "diagnosis" with ruptured
p <- ggboxplot(my_data, x = "ruptured", y = "sixteens_copy_number",
               color = "ruptured", palette = "jco",
               add = "jitter",
               facet.by = "implant_brand", short.panel.labs = FALSE, legend = "none")+
               labs(x = "Ruptured", y = "16s copy number")+
               ggtitle("Boxplot facetted by implant brand: \n 16s copy numbers present in ruptured vs non-ruptured implants") +
               theme(plot.title = element_text(hjust = 0.5, size = 12, face  = "bold")) +
               ylim(0, 30000)
# Use only p.format as label. Remove method name.
p + stat_compare_means(label = "p.format", label.x = 1.5)

# Or use significance symbol as label
p + stat_compare_means(label =  "p.signif", label.x = 1.5)


## Multiple grouping variables
compare_means(sixteens_copy_number ~ surface, data = my_data, 
              group.by = "implant_brand")                                # surface is bottom, top, whole not paired test

# Box plot facetted by "diagnosis" with implant surface
p <- ggboxplot(my_data, x = "surface", y = "sixteens_copy_number",
               color = "surface", palette = "jco",
               add = "jitter",
               facet.by = "implant_brand", short.panel.labs = FALSE, legend = "none")+
               labs(x = "Section", y = "16s copy number")+
               ggtitle("Boxplot facetted by implant brand: \n 16s copy numbers present in the bottom, top or whole section of implants") +
               theme(plot.title = element_text(hjust = 0.5, size = 12, face  = "bold")) +
               ylim(0, 30000)
# Use only p.format as label. Remove method name.
p + stat_compare_means(label = "p.format", label.x = 2)

# Or use significance symbol as label
p + stat_compare_means(label =  "p.signif", label.x = 2)

#########################################################################################################







