

library(readr)
library(ggplot2)
library(corrplot) #correlation plots
library(grid) #plotting functions
library(gridExtra) #further plotting functions
library(RColorBrewer) #visualisation palettes
library(tidyverse)
library(dplyr)
library(magrittr)
library(knitr)
library(kableExtra)

#Cleansed Dataset using for EDA
FF_LACP_Dataset <- read_csv("Desktop/FF_LACP_Dataset.csv")


head(FF_LACP_Dataset)

median_Reviewer <- median(FF_LACP_Dataset$Rating)
UQ_Reviewer <- quantile(FF_LACP_Dataset$Rating, )[4]
LQ_Reviewer <- quantile(FF_LACP_Dataset$Rating)[2]


##Distribution of rating across the dataset
Pop_hist.plot <- ggplot(FF_LACP_Dataset, aes(x=Rating)) + 
  geom_histogram(colour='black', fill='blue4', bins=40, size=0.2) + 
  theme_bw() +
  geom_vline(mapping = NULL, data = NULL, xintercept=median(FF_LACP_Dataset$Rating), colour='red',
             show.legend = NA) + 
  labs(title='New York City Census Tract Population Distribution',
       y='Frequency of Rating',
       x='Rating',
       caption='Source: Rating Distribution of Reviewer') +
  theme(plot.caption = element_text(size = 8)) 
  
Pop_hist.plot


#Rating_Of_reviewers vs Attendees
Rating_Of_reviewers = FF_LACP_Dataset %>% group_by(Reviewer,`Timeline Selected`,Name)  %>% 
  summarise( 
    Sum_of_Ratings = sum(Rating),
            .groups = 'drop')  %>%  print(n=100)

#Plot
ggplot(Rating_Of_reviewers, aes(x = Name, y = Sum_of_Ratings, fill = Reviewer)) + 
  geom_col() + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  labs(title = "Sum_of_Ratings by Reviewer", x = "Reviewer Name", y = "Sum of Ratings")

#Timeline
Attendees_vs_Timeline = FF_LACP_Dataset %>% 
  group_by(`Timeline Selected`) %>% 
  summarise(Count_Of_Attendees = n_distinct(Name), .groups = 'drop') %>%
  print(n=100)

# Create a bar chart to display the number of attendees by timeline selected
ggplot(Attendees_vs_Timeline, aes(x = `Timeline Selected`, y = Count_Of_Attendees)) +
  geom_bar(stat = "identity", fill = "pink") +
  labs(title = "Number of Attendees by Timeline Selected", x = "Timeline Selected", y = "Number of Attendees") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

