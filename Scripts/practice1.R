library(tidyverse)

implants_qpcr <- read_csv("Data/implants 2019.csv", na = "null")
implants_qpcr

implants_qpcr %>% 
  select(diagnosis, implant_brand, total_bacteria_per_mg_tissue) %>% 
  filter(diagnosis == "alcl")  


implants_qpcr %>%
  filter(ruptured == "yes") %>%   
  select(diagnosis, implant_brand, total_bacteria_per_mg_tissue) 


summary(implants_qpcr)


implants_qpcr <- read_csv("Data/implants 2019.csv", na = "null") %>%
mutate(batch_run_number = as_factor(batch_run_number), age = as_factor(age), diagnosis = as_factor(diagnosis),
       location = as_factor(location), surface = as_factor(surface), ruptured = as_factor(ruptured), 
       implant_brand = as_factor(implant_brand), implant_surface = as_factor(implant_surface), size = as_factor(size),
       shape = as_factor(shape))

summary(implants_qpcr)


library(ggplot2)


## total bacteria per mg of tissue
ggplot(implants_qpcr, aes(x = total_bacteria_per_mg_tissue)) +
  geom_histogram()


ggplot(implants_qpcr, aes(x = total_bacteria_per_mg_tissue, fill = diagnosis)) +
  geom_histogram(position = "dodge")


ggplot(implants_qpcr, aes(x = diagnosis, y = total_bacteria_per_mg_tissue)) +
  geom_boxplot()


## 16s copy number
ggplot(implants_qpcr, aes(x = sixteens_copy_number)) +
  geom_histogram()


ggplot(implants_qpcr, aes(x = sixteens_copy_number, fill = diagnosis)) +
  geom_histogram(position = "dodge")


ggplot(implants_qpcr, aes(x = diagnosis, y = sixteens_copy_number)) +
  geom_boxplot()


ggplot(implants_qpcr, aes(x = implant_brand, y = sixteens_copy_number)) +
  geom_boxplot()





