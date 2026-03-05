# ---------------------------------------------------------

# Melbourne Bioinformatics Training Program

# This exercise to assess your familiarity with R and git. Please follow
# the instructions on the README page and link to your repo in your application.
# If you do not link to your repo, your application will be automatically denied.

# Leave all code you used in this R script with comments as appropriate.
# Let us know if you have any questions!


# You can use the resources available on our training website for help:
# Intro to R: https://mbite.org/intro-to-r
# Version Control with Git: https://mbite.org/intro-to-git/

# ----------------------------------------------------------

# Load libraries -------------------
# You may use base R or tidyverse for this exercise

library(tidyverse)
library(here)
library(ggplot2)

# ex. library(tidyverse)

# Load data here ----------------------
# Load each file with a meaningful variable name.

data_meta<-read_csv(file=here("GSE60450_filtered_metadata.csv"))
data_expression<-read_csv(file=here("GSE60450_GeneLevel_Normalized(CPM.and.TMM)_data.csv"))

# Inspect the data -------------------------

# What are the dimensions of each data set? (How many rows/columns in each?)
# Keep the code here for each file.

## Expression data
nrow(data_expression)
ncol(data_expression)

## Metadata
nrow(data_meta)
ncol(data_meta)

# Prepare/combine the data for plotting ------------------------
# How can you combine this data into one data.frame?

data_expression<-data_expression%>%
  rename("gene_id"=...1)

data_meta<-data_meta%>%
  rename("subject_id"=...1) 

### I changed column 1 of each data frame to make it easier for next steps

data_expression_long<-data_expression%>%
  pivot_longer(
    cols=starts_with("GSM"),
    names_to = "subject_id", 
    values_to = "expression level") 

### Here I need to reshape the expression level tibble so that subject IDs will
    become a column rather than individual column headings, so I can then use
    then use this subject_id column to leftjoin with the sample metadata 

data_combine<-left_join(x=data_meta,
                        y=data_expression_long,
            by="subject_id")

### Now I can simply combine both tibble using the 'subject_id' column 

# Plot the data --------------------------
## Plot the expression by cell type
## Can use boxplot() or geom_boxplot() in ggplot2

### There are over 20000 gene in the expression level data, it would be impossible and not 
    very useful to compare their overall expression between the two cell population. While 
    this can be achieved simply by group_by(immunophenotype), and summarise (expression level),
    to find the overall mean expression level and sderr. I decided it would be much more useful
    to compare individual (or multiple) gene type at once. Below I constructed a boxplot to
    illustrate the difference of Abhd2 gene expression level (randomly selected) between the two
    cell population. 

Abhd2_plot<-data_combine%>%
  filter(gene_symbol=="Abhd2")%>%   ### this step select the gene(s) of interest
  ggplot(
    mapping=aes(x = immunophenotype,
                y = `expression level`,
                fill=immunophenotype))+  ### setting the x and y axis
  theme_minimal()+
  geom_boxplot(width=0.3, alpha=0.6)+   ### generates boxplot
  geom_jitter(width=0.2, alpha=0.5,      
              aes(fill=immunophenotype))+   ### adding individual data points onto boxplot
  scale_fill_brewer(palette = "Set1")+   ### adding a nice palette
  scale_y_continuous(limits = c(0, 1500))+  ### setting the y-axis limits to include all data points 
  theme(
    axis.line = element_line(color = "black"),
    axis.text = element_text(size = 10, color = "black"),
    panel.grid = element_blank(),
    legend.position = "none",
  )+                                      ### here just to make the plot look nicer and clean
  labs(x = element_blank(),
       y = "Expression Level (unit)")     ### finally change the y-axis label 

### If want to represent multiple gene expression level comparsion plot at once,
    facet_wrap (~immunopheotype) can be used

## Save the plot
### Show code for saving the plot with ggsave() or a similar function

ggsave("Abhd2_plot.png", plot=Abhd2_plot,
       width = 6, height = 5, dpi = 200)
