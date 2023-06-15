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
### summary of data  ###
################################################################################
collared_animals_summary <- collared_animals %>% 
  group_by(behaviour_stage) %>% 
  summarise(dist_to_VF_mean = mean(dist_to_VF, na.rm = FALSE),
            dist_to_VF_sd = sd(dist_to_VF, na.rm = FALSE),
            
            resting_percentage_mean = mean(resting_percentage, na.rm = FALSE),
            resting_percentage_sd = sd(resting_percentage, na.rm = FALSE),
            
            moving_percentage_mean = mean(moving_percentage, na.rm = FALSE),
            moving_percentage_sd = sd(moving_percentage, na.rm = FALSE),
            
            resting_percentage_mean = mean(resting_percentage, na.rm = FALSE),
            resting_percentage_VF_sd = sd(resting_percentage, na.rm = FALSE),
            
            grazing_percentage_mean = mean(grazing_percentage, na.rm = FALSE),
            grazing_percentage_sd = sd(grazing_percentage, na.rm = FALSE),
            
            step_mean = mean(step, na.rm = FALSE),
            stepF_sd = sd(step, na.rm = FALSE),
            
            Audio_values_mean = mean(Audio_values, na.rm = FALSE),
            Audio_values_sd = sd(Audio_values, na.rm = FALSE),
              
            Shock_values_values_mean = mean(Shock_values, na.rm = FALSE),
            Shock_values_sd = sd(Shock_values, na.rm = FALSE)
              )


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
