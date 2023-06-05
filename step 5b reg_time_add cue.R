### reg time step and distance travelled

################################################################################
library(dplyr)
library(tidyverse)
library(readr)
library(lubridate)
library(DT)
library(sp)
#install.packages("sf")
library(sf)
################################################################################



################################################################################
#### --------------    Bring in data   -------------- ####
################################################################################

GPS_Dist <- read_csv("W:/VF/Optimising_VF/Lameroo/data_prep/step3_clip.csv")
names(GPS_Dist)

### subset the data to the clms I want.

GPS_Dist <- GPS_Dist %>% dplyr::select (ID_jaxs, Sheep_ID, 
                                        #DOT, #I dont have this
                                        local_time, 
                                        date,
                                        DOY, 
                                        cumulativeAudioCount,
                                        cumulativeShockCount,
                                        Audio_values,
                                        Shock_values  )

GPS_Dist$local_time <- as.POSIXct(GPS_Dist$local_time,  tz = "Australia/Adelaide")
GPS_Dist <- GPS_Dist %>%  rename(sheep = Sheep_ID)
str(GPS_Dist)
################################################################################
#### --------------    what is the length of the trail?   -------------- ####
################################################################################


start <- min(GPS_Dist$local_time, na.rm = TRUE)  # "2022-10-17 11:41:16 ACDT"
end <-   max(GPS_Dist$local_time, na.rm = TRUE) # "2022-10-21 11:41:23 ACDT"
#Since we’re dealing with elapsed time between two dates, let’s start with Intervals. We can define an Interval using the %--% operator.
start <- round_date(start, unit="10 mins") #2022-10-17 11:40:00 ACDT"
end <- round_date(end, unit="10 mins") # "2022-10-21 11:40:00 ACDT"

time.interval <- start %--% end
time.interval
#To create a Duration between these two dates, we can use the as.duration function.

time.duration <- as.duration(time.interval)
time.duration # "345600s (~4 days)"

################################################################################
#### --------------    make a regular time step   -------------- ####
################################################################################


regular_time_interval <-data.frame(time_step = seq(from = ymd_hms(start),
                                                   to = ymd_hms(end), 
                                                   by = '10 mins'))

################################################################################
#### ----   Need to round the local time to the closest 10 min  -------------- ####
################################################################################
str(GPS_Dist)
# Need to round the local time to the closest 10 min
GPS_Dist <- GPS_Dist %>% 
  dplyr::mutate(round_local_time =round_date(local_time, unit="10 mins"),
                Time_sheep = paste0(round_local_time,"_", sheep) )


rm(end,start, time.duration, time.interval)


###############################################################################  
## ----- function to produce steps per animal
################################################################################

sheep_list <- GPS_Dist %>% distinct(sheep) %>%  arrange(sheep)
### 26 sheep ID I need regular time interval for each sheep
### List of sites I want to run analysis for:
sheep_list
sheep_list <- c(1:10)
#sheep_list <- 1

#### as a function
for (sheep_list in sheep_list){
  
################################################################################  
#regular_time_interval_per_sheep_ID
################################################################################
  regular_time_interval_sheep <- regular_time_interval %>% 
    dplyr::mutate(Time_sheep = paste0(time_step,"_", sheep_list))
  
################################################################################
#### --------------    Join regular time step to dataset  -------------- ####
################################################################################  
  
  GPS_sheep <- GPS_Dist %>%  filter(sheep == sheep_list)
  
   
  ## the occurrence of a duplicated time_sheep
  
  GPS_sheep <- GPS_sheep %>% 
    distinct(Time_sheep, .keep_all = TRUE)
 
  # It might be a better to split the data into  audio and pulse
  #str(GPS_sheep)
  # Audio_values <- GPS_sheep %>% filter(Audio_values > 0) 
  # Shock_values <- GPS_sheep %>% filter(Shock_values > 0) 
 #names(Audio_values) 
  # Audio_values_max <- GPS_sheep %>% group_by(Time_sheep) %>% 
  #   summarise(Audio_values_max = max(cumulativeAudioCount, na.rm = TRUE) )
  # Shock_values_max <- GPS_sheep %>% group_by(Time_sheep) %>% 
  #   summarise(Shock_values_max = max(cumulativeShockCount, na.rm = TRUE) )
  # 
  
 
  
  # GPS_sheep <- left_join(GPS_sheep, Audio_values_max)
  # GPS_sheep <- left_join(GPS_sheep, Shock_values_max)
  
  GPS_sheep[ is.na(GPS_sheep) ] <- 0
  #GPS_sheep <- GPS_sheep %>%  dplyr::select(-Audio_values,-Shock_values )
  
  GPS_sheep_reg_time <- left_join(regular_time_interval_sheep, GPS_sheep)

  #### Trim the regular time step to match the end of sheep time

  start_sheep <- min(GPS_sheep$local_time, na.rm = TRUE)  
  end_sheep <-   max(GPS_sheep$local_time, na.rm = TRUE) 
  start_sheep <- round_date(start_sheep, unit="10 mins")
  end_sheep <- round_date(end_sheep, unit="10 mins") 
  
  ## trim the joined data to the sheeps ID time in the trial 
  
  #names(GPS_sheep_reg_time)
  GPS_sheep_reg_time <- GPS_sheep_reg_time %>% 
    dplyr::filter(between(time_step, ymd_hms(start_sheep), ymd_hms(end_sheep))) 
#-------------------------------------------------------------------------------- 
  ### add in the audio and pulse counts cal from cumulative 
#--------------------------------------------------------------------------------  
   str(GPS_sheep_reg_time) 
  
  #FILL missing data 
  GPS_sheep_reg_time <- GPS_sheep_reg_time %>% 
    tidyr::fill(cumulativeAudioCount, .direction = "down")
  
  GPS_sheep_reg_time <- GPS_sheep_reg_time %>% 
    tidyr::fill(cumulativeShockCount, .direction = "down")
  
   GPS_sheep_reg_time <- GPS_sheep_reg_time %>% 
    mutate(Audio_values = cumulativeAudioCount - lag(cumulativeAudioCount))
  
  GPS_sheep_reg_time <- GPS_sheep_reg_time %>% 
    mutate(Shock_values = cumulativeShockCount - lag(cumulativeShockCount))  
    
    
##################################################################################################################
    

  rm(
    GPS_sheep,
    regular_time_interval_sheep
    )
  name <- paste0("GPS_sheep_reg_time_step", sheep_list)
  assign(name,GPS_sheep_reg_time)
  
  }       

file_list <- data.frame(name_df = paste0("GPS_sheep_reg_time_step",c(1:10)))






GPS_sheep_reg_time_step_all <- rbind(
  GPS_sheep_reg_time_step1,
  GPS_sheep_reg_time_step2,
  GPS_sheep_reg_time_step3,
  GPS_sheep_reg_time_step4,
  GPS_sheep_reg_time_step5,
  GPS_sheep_reg_time_step6,
  GPS_sheep_reg_time_step7,
  GPS_sheep_reg_time_step8,
  GPS_sheep_reg_time_step9,
  GPS_sheep_reg_time_step10#,
  # GPS_sheep_reg_time_step11,
  # GPS_sheep_reg_time_step12,
  # GPS_sheep_reg_time_step13,
  # GPS_sheep_reg_time_step14,
  # GPS_sheep_reg_time_step15,
  # GPS_sheep_reg_time_step16,
  # GPS_sheep_reg_time_step17,
  # GPS_sheep_reg_time_step18,
  # GPS_sheep_reg_time_step19,
  # GPS_sheep_reg_time_step20,
  # GPS_sheep_reg_time_step21,
  # GPS_sheep_reg_time_step22,
  # GPS_sheep_reg_time_step23,
  # GPS_sheep_reg_time_step24,
  # GPS_sheep_reg_time_step25,
  # GPS_sheep_reg_time_step26,
  # GPS_sheep_reg_time_step27,
  # GPS_sheep_reg_time_step28,
  # GPS_sheep_reg_time_step29,
  # GPS_sheep_reg_time_step30,
  # GPS_sheep_reg_time_step31,
  # GPS_sheep_reg_time_step32,
  # GPS_sheep_reg_time_step33,
  # GPS_sheep_reg_time_step34,
  # GPS_sheep_reg_time_step35,
  # GPS_sheep_reg_time_step36
  )




rm(GPS_sheep_reg_time_step1,
   GPS_sheep_reg_time_step2,
   GPS_sheep_reg_time_step3,
   GPS_sheep_reg_time_step4,
   GPS_sheep_reg_time_step5,
   GPS_sheep_reg_time_step6,
   GPS_sheep_reg_time_step7,
   GPS_sheep_reg_time_step8,
   GPS_sheep_reg_time_step9,
   GPS_sheep_reg_time_step10#,
   # GPS_sheep_reg_time_step11,
   # GPS_sheep_reg_time_step12,
   # GPS_sheep_reg_time_step13,
   # GPS_sheep_reg_time_step14,
   # GPS_sheep_reg_time_step15,
   # GPS_sheep_reg_time_step16,
   # GPS_sheep_reg_time_step17,
   # GPS_sheep_reg_time_step18,
   # GPS_sheep_reg_time_step19,
   # GPS_sheep_reg_time_step20,
   # GPS_sheep_reg_time_step21,
   # GPS_sheep_reg_time_step22,
   # GPS_sheep_reg_time_step23,
   # GPS_sheep_reg_time_step24,
   # GPS_sheep_reg_time_step25,
   # GPS_sheep_reg_time_step26,
   # GPS_sheep_reg_time_step27,
   # GPS_sheep_reg_time_step28,
   # GPS_sheep_reg_time_step29,
   # GPS_sheep_reg_time_step30,
   # GPS_sheep_reg_time_step31,
   # GPS_sheep_reg_time_step32,
   # GPS_sheep_reg_time_step33,
   # GPS_sheep_reg_time_step34,
   # GPS_sheep_reg_time_step35,
   # GPS_sheep_reg_time_step36
   )

##### Think about how to deal with yarding times - I think you may need to remove first step cal after yarding time.
## I dont think I need to do anything here - the animals we yarded over night and the data is trimmed to this already.
### If you had a yard in and out and the data wasnt trimmed this might be a problem.


# It looks to be ok because the GPS log data has it removed already and  I am using this file to join the regular time step to the GPS data





output_path <- "W:/VF/Optimising_VF/Lameroo/data_prep/"  #animals_GPS_trim_time


write.csv(GPS_sheep_reg_time_step_all, 
          paste0(output_path,"/step5b_Greg_time_step_dist_travelled_withCue.csv"), 
          row.names=FALSE)




#################################################################################
#### check #####################################################################
#################################################################################
str(GPS_sheep_reg_time_step_all)
unique(GPS_sheep_reg_time_step_all$VF_EX)



count_exclusion_zone_occurance_per_animal_1 <- GPS_sheep_reg_time_step_all %>%  group_by( sheep,VF_EX ) %>% 
  summarise(count_records = n())
count_exclusion_zone_occurance_per_animal_1



