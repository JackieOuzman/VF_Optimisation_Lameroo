##Plots of what the RF is using.


library(dplyr)
library(tidyverse)
library(readr)
library(lubridate)
library(DT)
library(sp)
#install.packages("sf")
library(sf)
library(ggplot2)



collared_animals <- read_csv("W:/VF/Optimising_VF/Lameroo/data_prep/step8_all_animals_collars.csv")

collared_animals$local_time <- as.POSIXct(collared_animals$local_time,  tz = "Australia/Adelaide")


################################################################################
####    remove the time logs I don't want      ###########################
################################################################################
names(collared_animals)
distinct(collared_animals, training_period)

collared_animals <- collared_animals %>% 
  dplyr::filter(training_period != "training")
################################################################################

### Add DOY clm
temp <- collared_animals %>% 
  filter(!is.na(DOY ))

min_DOY <- min(temp$DOY, na.rm = TRUE)
max_DOY <- max(temp$DOY, na.rm = TRUE)

collared_animals <- collared_animals %>% 
  mutate(DOT = (DOY - min_DOY)+1 )

collared_animals$DOY <- as.double(collared_animals$DOY )
################################################################################
### Definition of early behaviour ###
################################################################################

str(collared_animals)

collared_animals <- collared_animals %>% 
  mutate(
         behaviour_stage = case_when(
         DOT == 1 ~ "Early behaviour",
         DOT > 1 ~ "Later behaviour"))
################################################################################
######## PLOTS
################################################################################

ggplot(collared_animals, aes(x=behaviour_stage, y=dist_to_VF)) + 
  geom_boxplot()+
  #facet_wrap(.~compliance_score)+
  theme(axis.title.x=element_blank())
  
ggplot(collared_animals, aes(x=behaviour_stage, y=resting_percentage)) + 
  geom_boxplot()+
  #facet_wrap(.~compliance_score)+
  theme(axis.title.x=element_blank())

ggplot(collared_animals, aes(x=behaviour_stage, y=moving_percentage)) + 
  geom_boxplot()+
  #facet_wrap(.~compliance_score)+
  theme(axis.title.x=element_blank())
