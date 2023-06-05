library(dplyr)
library(tidyverse)
library(readr)
library(lubridate)
library(DT)
library(sp)
#install.packages("sf")
library(sf)

################################################################################
### Merge step 5 and strp 5b 
################################################################################

step5 <- read_csv("W:/VF/Optimising_VF/Lameroo/data_prep/step5_Greg_time_step_dist_travelled.csv")
step5b <- read_csv("W:/VF/Optimising_VF/Lameroo/data_prep/step5b_Greg_time_step_dist_travelled_withCue.csv")

str(step5)
str(step5b)

step5b <- step5b %>% 
  dplyr::select(Time_sheep,cumulativeAudioCount:Shock_values)


all_step5 <- left_join(step5, step5b)


output_path <- "W:/VF/Optimising_VF/Lameroo/data_prep/"  #animals_GPS_trim_time
write.csv(all_step5, 
          paste0(output_path,"/step5_all.csv"), 
          row.names=FALSE)
