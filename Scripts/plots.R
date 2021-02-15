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


ggplot(data = implants_qpcr, aes(x = diagnosis, y = sixteens_copy_number)) + 
  ylim(c(0, 45000)) +
  geom_point(alpha = 0.4)   


ggplot(data = implants_qpcr, aes(x = diagnosis, y = sixteens_copy_number)) + 
  ylim(c(0, 45000)) +
  geom_jitter(alpha = 0.4)   


ggplot(data = implants_qpcr, aes(x = diagnosis, y = sixteens_copy_number)) + 
  ylim(c(0, 45000)) +
  geom_point(alpha = 0.4, aes(color = diagnosis))  


ggplot(data = implants_qpcr, aes(x = diagnosis, y = sixteens_copy_number)) + 
  ylim(c(0, 45000)) +
  geom_point(alpha = 0.4, aes(color = implant_brand)) 


ggplot(data = implants_qpcr, aes(x = diagnosis, y = sixteens_copy_number)) + 
  ylim(c(0, 45000)) +
  geom_point(alpha = 0.9, aes(color = implant_surface)) 



## boxplots
ggplot(data = implants_qpcr, aes(x = diagnosis, y = sixteens_copy_number)) + 
  geom_boxplot() + 
  ylim(c(0, 45000)) +
  geom_jitter(alpha = 0.5, color = "tomato")     



ggplot(data = implants_qpcr, aes(x = diagnosis, y = sixteens_copy_number)) + 
  geom_jitter(alpha = 0.5, color = "tomato", width = 0.2) +
  geom_boxplot(alpha = 0) +   
  ylim(c(0, 45000)) 


ggplot(data = implants_qpcr, aes(x = diagnosis, y = sixteens_copy_number)) +    
  geom_jitter(alpha = 0.5, width = 0.2, aes(color = implant_brand), height = 0.2) +                          # added height so doesnt obscure data because of jitters you've added
  geom_boxplot(alpha = 0) +
  ylim(c(0, 45000))


ggplot(data = implants_qpcr, aes(x = diagnosis, y = sixteens_copy_number)) +    
  geom_jitter(alpha = 0.5, width = 0.2, aes(color = implant_surface), height = 0.2) +                          # added height so doesnt obscure data because of jitters you've added
  geom_boxplot(alpha = 0) +
  ylim(c(0, 45000))



## barplots
ggplot(implants_qpcr, aes(x = implant_surface)) + geom_bar(aes(fill = diagnosis), position = "dodge") + 
  labs(x = "Implant surface", y = "Count") +
  facet_grid(~diagnosis)                                                                                          # facet allows you to split initial plot by variables                                                                                                                                 
