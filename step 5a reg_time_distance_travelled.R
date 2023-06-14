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

GPS_Dist <- read_csv("W:/VF/Optimising_VF/Lameroo/data_prep/step4_dist_line_VF_zone.csv")
names(GPS_Dist)

### subset the data to the clms I want.

GPS_Dist <- GPS_Dist %>% dplyr::select (ID_jaxs, Sheep_ID, 
                                        #DOT, #I dont have this
                                        local_time, 
                                        date,
                                        DOY, 
                                        X , 
                                        Y, 
                                        dist_to_VF, 
                                        VF_EX,
                                        #Audio_values,#bring in at the next step 5b
                                        #Shock_values, #bring in at the next step 5b
                                        resting_percentage,
                                        moving_percentage,
                                        grazing_percentage ,
                                        training_period)

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
#### ----   Write out regular time step for later reference -------------- #####
################################################################################

write.csv(regular_time_interval, "W:/VF/Optimising_VF/Lameroo/data_prep/reg_time_step_max_min.csv", row.names = FALSE)


################################################################################
#### ----   Need to round the local time to the closest 10 min  -------------- ####
################################################################################
str(GPS_Dist)
# Need to round the local time to the closest 10 min
GPS_Dist <- GPS_Dist %>% 
  dplyr::mutate(round_local_time =round_date(local_time, unit="10 mins"),
                Time_sheep = paste0(round_local_time,"_", sheep) ,
                Time_sheep_zone = paste0(round_local_time,"_", sheep, "_", VF_EX))


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

### as a function
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
  GPS_sheep <- GPS_sheep %>% 
    #dplyr::distinct(Time_sheep, .keep_all = TRUE) #remove this line so I get the records that the animals went over the VF            
    dplyr::distinct(Time_sheep_zone, .keep_all = TRUE)
  
  ## the occurrence of a duplicated time_sheep
  
 # It might be a better to split the data into  outside_VF and inside_VF
  
  outside_VF <- GPS_sheep %>% filter(VF_EX == "outside_VF") %>% dplyr::distinct(Time_sheep, .keep_all = TRUE) #I think I need to add this to Waikerie code
  inside_VF <- GPS_sheep %>% filter(VF_EX == "inside_VF") %>% dplyr::distinct(Time_sheep, .keep_all = TRUE) #I think I need to add this to Waikerie code
  
  GPS_sheep <- rbind(outside_VF,inside_VF )
  
  duplication_report <- GPS_sheep %>% count(Time_sheep)
   
   GPS_sheep <- left_join(GPS_sheep,duplication_report ) %>% rename(occurance = n )
   str(GPS_sheep)
  # 
   GPS_sheep <- GPS_sheep %>% mutate(
     what_to_retain = case_when(
       occurance == 1 & VF_EX == "outside_VF" ~ "retain",
       occurance == 2 & VF_EX == "outside_VF" ~ "retain", #I think I need to add this to Waikerie code
       occurance == 1 & VF_EX == "inside_VF" ~ "retain",
       TRUE                      ~ "discard"
     )
   ) 
  
  # remove the rows tp discard
  GPS_sheep <- GPS_sheep %>% filter(what_to_retain == "retain")
  
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
 
  
  ################################################################################
  #### Do some cals  steps or distance travelled since last logged point ---- ####
  ################################################################################ 

    GPS_sheep_reg_time <- GPS_sheep_reg_time %>% 
    arrange(local_time)
  
  GPS_sheep_reg_time <- GPS_sheep_reg_time %>% 
    dplyr::mutate(step = sqrt( ((lead(X) - X)^ 2) + ((lead(Y) - Y)^ 2) ) )
  
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


##############################################################################
### create a df with the time interval for each animal #####
names(GPS_sheep_reg_time_step1)
names(regular_time_interval)

##animal 1 ###
test1  <- GPS_sheep_reg_time_step1 %>% 
  select(time_step, ID_jaxs) %>% 
  filter(!is.na(ID_jaxs)) %>% 
  rename(sheep1 =ID_jaxs)

GPS_animal_reg_time <- left_join(regular_time_interval, test1)

##animal 2 ###  
test2  <- GPS_sheep_reg_time_step2 %>% 
  select(time_step, ID_jaxs) %>% 
  filter(!is.na(ID_jaxs)) %>% 
  rename(sheep2 =ID_jaxs)
GPS_animal_reg_time <- left_join(GPS_animal_reg_time, test2)

##animal 3 ###  
test3  <- GPS_sheep_reg_time_step3 %>% 
  select(time_step, ID_jaxs) %>% 
  filter(!is.na(ID_jaxs)) %>% 
  rename(sheep3 =ID_jaxs)
GPS_animal_reg_time <- left_join(GPS_animal_reg_time, test3)

##animal 4 ###  
test4  <- GPS_sheep_reg_time_step4 %>% 
  select(time_step, ID_jaxs) %>% 
  filter(!is.na(ID_jaxs)) %>% 
  rename(sheep4 =ID_jaxs)
GPS_animal_reg_time <- left_join(GPS_animal_reg_time, test4)

##animal 5 ###  
test5  <- GPS_sheep_reg_time_step5 %>% 
  select(time_step, ID_jaxs) %>% 
  filter(!is.na(ID_jaxs)) %>% 
  rename(sheep5 =ID_jaxs)
GPS_animal_reg_time <- left_join(GPS_animal_reg_time, test5)

##animal 6 ###  
test6  <- GPS_sheep_reg_time_step6 %>% 
  select(time_step, ID_jaxs) %>% 
  filter(!is.na(ID_jaxs)) %>% 
  rename(sheep6 =ID_jaxs)
GPS_animal_reg_time <- left_join(GPS_animal_reg_time, test6)

##animal 7 ###  
test7  <- GPS_sheep_reg_time_step7 %>% 
  select(time_step, ID_jaxs) %>% 
  filter(!is.na(ID_jaxs)) %>% 
  rename(sheep7 =ID_jaxs)
GPS_animal_reg_time <- left_join(GPS_animal_reg_time, test7)


##animal 8 ###  
test8  <- GPS_sheep_reg_time_step8 %>% 
  select(time_step, ID_jaxs) %>% 
  filter(!is.na(ID_jaxs)) %>% 
  rename(sheep8 =ID_jaxs)
GPS_animal_reg_time <- left_join(GPS_animal_reg_time, test8)

##animal 9 ###  
test9  <- GPS_sheep_reg_time_step9 %>% 
  select(time_step, ID_jaxs) %>% 
  filter(!is.na(ID_jaxs)) %>% 
  rename(sheep9 =ID_jaxs)
GPS_animal_reg_time <- left_join(GPS_animal_reg_time, test9)

##animal 10 ###  
test10  <- GPS_sheep_reg_time_step10 %>% 
  select(time_step, ID_jaxs) %>% 
  filter(!is.na(ID_jaxs)) %>% 
  rename(sheep10 =ID_jaxs)
GPS_animal_reg_time <- left_join(GPS_animal_reg_time, test10)


GPS_animal_reg_time <- GPS_animal_reg_time %>% replace(is.na(.), "no_records")

GPS_animal_reg_time <- GPS_animal_reg_time %>%
  mutate(count_na = 
           str_count(sheep1, "no_records") + str_count(sheep2, "no_records") + str_count(sheep3, "no_records")+
           str_count(sheep4,"no_records") + str_count(sheep5, "no_records") + str_count(sheep6, "no_records")+
           str_count(sheep7, "no_records") + str_count(sheep8, "no_records") + str_count(sheep9, "no_records")+
           str_count(sheep10, "no_records") 
         )

rm(test1, test2, test3, test4, test5, test6, test7, test8, test9, test10)

GPS_animal_reg_time <- GPS_animal_reg_time %>% 
  filter(count_na == 0)

GPS_animal_reg_time <- GPS_animal_reg_time %>% 
  select(time_step)

################################################################################
#### ----   Write out regular time step for later reference -------------- #####
################################################################################

write.csv(GPS_animal_reg_time, "W:/VF/Optimising_VF/Lameroo/data_prep/GPS_animal_reg_time_common.csv", row.names = FALSE)




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
          paste0(output_path,"/step5_Greg_time_step_dist_travelled.csv"), 
          row.names=FALSE)







