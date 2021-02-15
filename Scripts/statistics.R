install.packages("ggpubr")
library(ggpubr)

## used this website http://www.sthda.com/english/articles/24-ggpubr-publication-ready-plots/76-add-p-values-and-significance-levels-to-ggplots/

## compare more than two groups
compare_means(sixteens_copy_number ~ diagnosis,  data = my_data, method = "anova")

# Default method = "kruskal.test" for multiple groups
ggboxplot(my_data, x = "diagnosis", y = "sixteens_copy_number",
          color = "diagnosis", palette = "jco")+
  stat_compare_means()

# Change method to anova
ggboxplot(my_data, x = "diagnosis", y = "sixteens_copy_number",
          color = "diagnosis", palette = "jco")+
  stat_compare_means(method = "anova")


## pairwise comparisons - ruptured data
compare_means(sixteens_copy_number ~ ruptured,  data = my_data)


# Visualize: Specify the comparisons you want
my_comparisons <- list( c("alcl", "cc"), c("alcl", "cosmetic exchange"), c("alcl", "non-alcl"), 
                        c("alcl", "breast implant illness"), c("alcl", "inflammatory seroma"), 
                        c("alcl", "revision"), c("cc", "cosmetic exchange"), c("cc", "non-alcl"), 
                        c("cc", "breast implant illness"), c("cc", "inflammatory seroma"), 
                        c("cc", "revision"), c("cosmetic exchange", "non-alcl"), 
                        c("cosmetic exchange", "breast implant illness"), c("cosmetic exchange", "inflammatory seroma"), 
                        c("cosmetic exchange", "revision"), c("non-alcl", "breast implant illness"), c("non-alcl", "inflammatory seroma"), 
                        c("non-alcl", "revision"), c("breast implant illness", "inflammatory seroma"), 
                        c("breast implant illness", "revision"), c("inflammatory seroma", "revision"))
ggboxplot(my_data, x = "diagnosis", y = "sixteens_copy_number",
          color = "diagnosis", palette = "jco")+ 
  stat_compare_means(comparisons = my_comparisons)+ # Add pairwise comparisons p-value
  stat_compare_means(label.y = 50)     # Add global p-value


## If you want to specify the precise y location of bars, use the argument label.y
## ggboxplot(my_data, x = "diagnosis", y = "sixteens_copy_number",
## color = "diagnosis", palette = "jco")+ 
## stat_compare_means(comparisons = my_comparisons, label.y = c(29, 35, 40))+
## stat_compare_means(label.y = 45)


## Multiple pairwise tests against all (base-mean)
## Comparison of each group against base-mean
compare_means(sixteens_copy_number ~ diagnosis,  data = my_data, ref.group = ".all.",
              method = "t.test")




ggboxplot(my_data, x = "diagnosis", y = "sixteens_copy_number", color = "diagnosis", 
          add = "jitter", legend = "none") +
  rotate_x_text(angle = 45)+
  stat_compare_means(method = "kruskal.test", label.y = 30000)+       
  stat_compare_means(label = "p.signif", method = "kruskal.test",
                     ref.group = ".all.", hide.ns = TRUE)  

     
## Multiple grouping variables
compare_means(sixteens_copy_number ~ implant_surface, data = my_data, 
              group.by = "diagnosis")

# Box plot facetted by "diagnosis"
p <- ggboxplot(my_data, x = "implant_surface", y = "sixteens_copy_number",
               color = "implant_surface", palette = "jco",
               add = "jitter",
               facet.by = "diagnosis", short.panel.labs = FALSE)
# Use only p.format as label. Remove method name.
p + stat_compare_means(label = "p.format")

# Or use significance symbol as label
p + stat_compare_means(label =  "p.signif", label.x = 1.5)




