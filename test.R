library(usethis) # to set up Github link

get_yesterday <- function (){
  x <- return(Sys.Date()-1)
  return(x)
}

library(tidyverse)

#1.3.3.1
site <- "https://tjfisher19.github.io/introStatModeling/data/univadmissions.txt"
class(site)

options(readr.show_col_types = FALSE)
uadata <- read_table(site, col_types = cols()) #read.table can read txt
# inspect the data 
head(uadata)
tail(uadata)
glimpse(uadata)

#inspect the data that matches some criteria
uadata %>% 
  filter(gpa.endyr1 > 3.9)
  ## just to look at a certain column
uadata %>% 
  filter(gpa.endyr1 > 3.9, 
         year==1998) %>%
  select(act)

#summary to look at descriptive stats
 ## wide dataset: each variable is a column
 ## long dataset: there are two columns: one is variable name, one is value
   ### Useful!!

uadata %>% 
  pivot_longer(everything()) %>% 
  group_by(name) %>% # Groups the data by the variable names (now in the name column)
  summarize(across(everything(), 
                   list(Mean=mean, SD=sd, Min=min, Median=median, Max=max) ) )

 ## Turning some to factors, so that in the latter summary they won't be analysed by the values
uadata <- uadata %>%
  mutate(id=as.factor(id),
         year=as.factor(year))

uadata %>% 
  summarize(across(everything(), class))


uadata %>% 
  select(where(is.numeric)) %>%
  pivot_longer(everything()) %>% 
  group_by(name) %>%
  summarize(across(everything(), 
                   list(Mean=mean, SD=sd, Min=min, Median=median, Max=max) ) )


# 1.6.3 Graphical Summaries
library(ggplot2)
library(GGally)
 # Overview: 
 #Figure 1.1: Pairs plot showing the relationship between the variables gpa.endy1, hs.pct, act and year.
ggpairs(uadata, columns = 2:5)

 ## Density plots:
ggplot(uadata) +  # the "+" sign is to add a layer
  geom_density(aes(x=gpa.endyr1))

uadata.tall <- uadata %>%
  pivot_longer(c(gpa.endyr1, hs.pct, act), names_to="var_names", values_to="value" )
head(uadata)
head(uadata.tall)
dim(uadata)   
dim(uadata.tall)

 ##　Complex density plots:
 ## Figure 1.3: Side-by-side density plots showing the relative distributions of act, gpa.endyr1 and hs.pct.
ggplot(uadata.tall) +
  geom_density(aes(x=value)) + 
  facet_grid(.~var_names, scales="free_x") + 
  theme_bw()

 ## Build a histogram
ggplot(uadata.tall) +
  geom_histogram(aes(x=value), binwidth = 2.5) + 
  facet_grid(var_names~., scales="free_y") + 
  theme_bw()
