library(dplyr)
library(tidyverse)
library(readr)
library(lubridate)
library(DT)
library(sp)
#install.packages("sf")
library(sf)


collared_animals <- read_csv("W:/VF/Optimising_VF/Lameroo/data_prep/step8_all_animals_collars.csv")

collared_animals$local_time <- as.POSIXct(collared_animals$local_time,  tz = "Australia/Adelaide")


################################################################################
####    remove the time logs I don't want      ###########################
################################################################################
names(collared_animals)
distinct(collared_animals, training_period)

collared_animals <- collared_animals %>% 
  dplyr::filter(training_period != training)
################################################################################

### Add DOY clm
temp <- collared_animals %>% 
  filter(!is.na(DOY ))

min_DOY <- min(temp$DOY, na.rm = TRUE)
max_DOY <- max(temp$DOY, na.rm = TRUE)

collared_animals <- collared_animals %>% 
  mutate(DOT = (DOY - min_DOY)+1 )

str(collared_animals)

collared_animals_day1 <- collared_animals %>%  filter(DOT == 1)

RF_df <- collared_animals_day1 %>%  dplyr::select(sheep ) %>% 
  distinct(sheep)

RF_df <- collared_animals %>% distinct(sheep, .keep_all = TRUE) %>% 
  dplyr::select(sheep, compliance_score)
RF_df %>% distinct(compliance_score)
RF_df <- RF_df %>%  filter(compliance_score != "NA" )

rm(temp, max_DOY, min_DOY, collared_animals)
#####################################################################################
### what is the average distance to the fence?
## average distance to VF when inside inclusion zone and when outside inclusion zone
## max distance to Vf 

######################################################################################

str(collared_animals_day1)
#----create new clms distance from VF when inside inclusion zone and when outside inclusion zone
#---- if the record is not in the inclusion zone it gets an NA for dist_frm_VF_inside_inclusion and vice versa
#---- the other option would be to get a zero value which genertaed different mean and max values in the summary data


collared_animals_day1 <- collared_animals_day1 %>%
  dplyr::mutate(dist_frm_VF_inside_inclusion = case_when(VF_EX == "inside_VF" ~ dist_to_VF,
                                                         TRUE ~ NA_real_))

collared_animals_day1 <- collared_animals_day1 %>%
  dplyr::mutate(dist_frm_VF_outside_inclusion = case_when(VF_EX == "outside_VF" ~ dist_to_VF,
                                                         TRUE ~ NA_real_))



#----summaries the distance to VF variable
str(collared_animals_day1)


dist_frm_VF_summary <- collared_animals_day1 %>%  group_by(sheep) %>% 
  summarise(
            mean_dist_frm_VF_inside_inclusion =mean(dist_frm_VF_inside_inclusion, na.rm=TRUE),
            max_dist_frm_VF_inside_inclusion =max(dist_frm_VF_inside_inclusion, na.rm=TRUE),
            
            mean_dist_frm_VF_outside_inclusion =mean(dist_frm_VF_outside_inclusion, na.rm=TRUE), #this will give you a warning message beacuse of the heaps of zero values
            max_dist_frm_VF_outside_inclusion =max(dist_frm_VF_outside_inclusion, na.rm=TRUE)
            )

            
dist_frm_VF_summary <- as.data.frame(dist_frm_VF_summary)
dist_frm_VF_summary[ is.na(dist_frm_VF_summary) ] <- NA

str(dist_frm_VF_summary)

dist_frm_VF_summary <- dist_frm_VF_summary %>% 
  dplyr::mutate(max_dist_frm_VF_outside_inclusion =
                  case_when(max_dist_frm_VF_outside_inclusion > 0 ~ max_dist_frm_VF_outside_inclusion,
                TRUE   ~ NA_real_ ))

###--- join distance to VF variables to the RF df

RF_df <- left_join(RF_df, dist_frm_VF_summary)

rm(dist_frm_VF_summary)

#####################################################################################
### what is the total distance travelled for the length of the trial?

######################################################################################
str(collared_animals_day1)

dist_taken_summary <- collared_animals_day1 %>%  group_by(sheep) %>% 
  summarise(total_dist_travel  =sum(step, na.rm=TRUE))


dist_taken_summary

RF_df <- left_join(RF_df, dist_taken_summary)

dist_taken_summary <- collared_animals_day1 %>%  group_by(sheep) %>% 
  summarise(mean_dist  =mean(step, na.rm=TRUE))

RF_df <- left_join(RF_df, dist_taken_summary)
rm(dist_taken_summary)



#####################################################################################
### what is the total number of cues audio and pulse and ratio for the length of the trial?
### could do average  day 1 and 2
######################################################################################

str(collared_animals_day1)


#rename so it matched other files
collared_animals_day1 <- collared_animals_day1 %>% 
  rename(audio = Audio_values ,
         pulse = Shock_values)



cue_summary <- collared_animals_day1 %>%  group_by(sheep) %>% 
  summarise(total_audio  =sum(audio, na.rm=TRUE),
            total_pulse  =sum(pulse, na.rm=TRUE),
            ratio = total_audio/(total_pulse+total_audio)*100)
cue_summary
cue_summary[ is.na(cue_summary) ] <- NA


RF_df <- left_join(RF_df, cue_summary)
rm(cue_summary)



#### per day cue data
#####################################################
### Add DOY clm
temp <- collared_animals_day1 %>% 
  filter(!is.na(DOY ))

min_DOY <- min(temp$DOY, na.rm = TRUE)
max_DOY <- max(temp$DOY, na.rm = TRUE)

collared_animals_day1 <- collared_animals_day1 %>% 
  mutate(DOT = (DOY - min_DOY)+1 )
  
#####################################################

cue_DOT_summary <- collared_animals_day1 %>%  group_by(sheep, DOT) %>% 
  summarise(total_audio  =sum(audio, na.rm=TRUE),
            total_pulse  =sum(pulse, na.rm=TRUE),
            ratio = total_audio/(total_pulse+total_audio)*100)

cue_DOT_summary <- cue_DOT_summary %>% 
  filter(!is.na(sheep ))


cue_DOT_summary_wide <- cue_DOT_summary %>% 
  pivot_wider(names_from = DOT , 
              values_from = c(total_audio, total_pulse, ratio))


cue_DOT_summary_wide

cue_DOT_summary_wide <- cue_DOT_summary_wide %>% 
  rename(total_audio_Day1 = total_audio_1,
         #total_audio_Day2 = total_audio_2,
         
         total_pulse_Day1 = total_pulse_1,
         #total_pulse_Day2 = total_pulse_2,
         
         ratio_Day1 = ratio_1,
         #ratio_Day2 = ratio_2
         )

cue_DOT_summary_wide[ is.na(cue_DOT_summary_wide) ] <- NA  


RF_df <- left_join(RF_df, cue_DOT_summary_wide)
rm(cue_DOT_summary_wide)



#####################################################################################
### what is the proportion of activity spent resting lying grazing? for the length of the trial?

######################################################################################
str(collared_animals_day1)

beha_summary <- collared_animals_day1 %>%  group_by(sheep) %>% 
  summarise(total_resting   =sum(resting_percentage,  na.rm=TRUE),
            total_grazing  =sum(grazing_percentage , na.rm=TRUE),
            total_moving   =sum(moving_percentage,  na.rm=TRUE),  
            total_beha      =  (total_resting+ total_grazing+ total_moving)                  
  )                  

beha_summary <- beha_summary %>% 
  dplyr::mutate(
    prop_resting = (total_resting/total_beha)*100,
    prop_grazing = (total_grazing/total_beha)*100,
    prop_moving = (total_moving/total_beha)*100,
    check = (prop_resting+prop_grazing+prop_moving)
  )
beha_summary <-beha_summary %>%  dplyr::select(sheep, prop_resting, prop_grazing,prop_moving)       

RF_df <- left_join(RF_df, beha_summary)
rm(beha_summary)


#####################################################################################
### add in the distance between animals
######################################################################################

dist_bewteen <- read_csv("W:/VF/Optimising_VF/Lameroo/data_prep/step7_count_close_animals_DOT1.csv")



## make a new variable hours
dist_bewteen$time_step <- as.POSIXct(dist_bewteen$time_step,  tz = "Australia/Adelaide")
dist_bewteen <- dist_bewteen %>%  dplyr::mutate(date= date(time_step))  

 

#keep sheep with collars only on the days that the trials were run for them



## we have a problem the data has a heap of zero some of these relate to when the trial was run and not run



mean_number_close_animals<- dist_bewteen %>%  group_by(sheep) %>% 
  summarise(mean_number_animals_close = mean(numb_sheep_close, na.rm=TRUE))  %>% 
  arrange(sheep)

mean_number_close_animals


RF_df <- left_join(RF_df, mean_number_close_animals)
rm(mean_number_close_animals)


###################################################################################
###                 write out df ready for the next step                      ###
###################################################################################

output_path <- "W:/VF/Optimising_VF/Lameroo/data_prep/RF_model_input_data"  

write.csv(RF_df, 
          paste0(output_path,"/step9_RF_df_input_DOT1.csv"), 
          row.names=FALSE)
###################################################################################
















#####################################################################################
### what is the total number of cues audio and pulse and ratio for the length of the trial?

######################################################################################

str(collared_animals_day1)
unique(collared_animals_day1$Cue)

collared_animals_day1 <- collared_animals_day1 %>% 
  mutate(audio = 
           case_when(Cue == "A" ~ 1,
                     TRUE ~ NA_real_
           ))

collared_animals_day1 <- collared_animals_day1 %>% 
  mutate(pulse = 
           case_when(Cue == "S" ~ 1,
                     TRUE ~ NA_real_
           ))


cue_summary <- collared_animals_day1 %>%  group_by(sheep) %>% 
  summarise(total_audio  =sum(audio, na.rm=TRUE),
            total_pulse  =sum(pulse, na.rm=TRUE),
            ratio = total_audio/(total_pulse+total_audio)*100)
cue_summary
cue_summary[ is.na(cue_summary) ] <- NA


RF_df <- left_join(RF_df, cue_summary)
rm(cue_summary)






#####################################################################################
### what is the proportion of activity spent resting lying grazing? for the length of the trial?

######################################################################################
str(collared_animals_day1)

beha_summary <- collared_animals_day1 %>%  group_by(sheep) %>% 
  summarise(total_resting   =sum(resting,  na.rm=TRUE),
            total_standing  =sum(standing, na.rm=TRUE),
            total_walking   =sum(walking,  na.rm=TRUE),  
            total_running   =sum(running,  na.rm=TRUE),
            total_beha      =  (total_resting+ total_standing+ total_walking+ total_running)                  
  )                  

beha_summary <- beha_summary %>% 
  dplyr::mutate(
    prop_resting = (total_resting/total_beha)*100,
    prop_standing = (total_standing/total_beha)*100,
    prop_walking = (total_walking/total_beha)*100,
    prop_running = (total_running/total_beha)*100,
    check = (prop_resting+prop_standing+prop_walking+prop_running)
    )
beha_summary <-beha_summary %>%  dplyr::select(sheep, prop_resting,prop_standing,prop_walking,prop_running)       

RF_df <- left_join(RF_df, beha_summary)
rm(beha_summary)


#####################################################################################
### add in the distance between animals
######################################################################################

dist_bewteen_animals_33 <- read_csv("W:/VF/Optimising_VF/Waikerie/data_prep/step7_count_close_animals_33percent.csv")
dist_bewteen_animals_66 <- read_csv("W:/VF/Optimising_VF/Waikerie/data_prep/step7_count_close_animals_66percent.csv")
dist_bewteen_animals_100 <- read_csv("W:/VF/Optimising_VF/Waikerie/data_prep/step7_count_close_animals_100percent.csv")


## make a new variable hours
dist_bewteen_animals_33$time_step <- as.POSIXct(dist_bewteen_animals_33$time_step,  tz = "Australia/Adelaide")
dist_bewteen_animals_33 <- dist_bewteen_animals_33 %>%  dplyr::mutate(date= date(time_step))  

dist_bewteen_animals_66$time_step <- as.POSIXct(dist_bewteen_animals_66$time_step,  tz = "Australia/Adelaide")
dist_bewteen_animals_66 <- dist_bewteen_animals_66 %>%  dplyr::mutate(date= date(time_step))  

dist_bewteen_animals_100$time_step <- as.POSIXct(dist_bewteen_animals_100$time_step,  tz = "Australia/Adelaide")
dist_bewteen_animals_100 <- dist_bewteen_animals_100 %>%  dplyr::mutate(date= date(time_step))  

#keep sheep with collars only on the days that the trials were run for them
 
date_13_14_keep_these <- c(2,3,5,13,14,17,22,30,35) #100% trial was run on the 13th and 14th
date_15_16_keep_these <- c(12,23,25, 10,15,21,27,33,36)  # 33% and 66% trial was run 15th and 16th


dist_bewteen_animals_100 <- dist_bewteen_animals_100 %>%
  filter(date == "2018-03-13" | date == "2018-03-13")  %>%
  filter(sheep %in% date_13_14_keep_these)


dist_bewteen_animals_66 <-
  dist_bewteen_animals_66 %>%  
  filter(date == "2018-03-15" |date == "2018-03-16")  %>%
  filter(sheep %in% date_15_16_keep_these)

dist_bewteen_animals_33 <-
  dist_bewteen_animals_33 %>%
  filter(date == "2018-03-15" | date == "2018-03-16")  %>%
  filter(sheep %in% date_15_16_keep_these)


dist_bewteen_animals_all <- rbind(dist_bewteen_animals_33, dist_bewteen_animals_66, dist_bewteen_animals_100)
rm(dist_bewteen_animals_33,dist_bewteen_animals_66,  dist_bewteen_animals_100)





## we have a problem the data has a heap of zero some of these relate to when the trial was run and not run
#for example on the 13/3 and 14/3 
#This step check df if I have got al of these -  i have no zero in check file - so its all good.

check_what_to_remove <- dist_bewteen_animals_all %>%  group_by(date,sheep) %>% 
  summarise(mean_number_animals_close = mean(numb_sheep_close, na.rm=TRUE))  %>% 
  arrange(sheep, date)



mean_number_close_animals<- dist_bewteen_animals_all %>%  group_by(sheep) %>% 
  summarise(mean_number_animals_close = mean(numb_sheep_close, na.rm=TRUE))  %>% 
  arrange(sheep)

mean_number_close_animals


RF_df <- left_join(RF_df, mean_number_close_animals)
rm(mean_number_close_animals)



###################################################################################
###                 write out df ready for the next step                      ###
###################################################################################

output_path <- "W:/VF/Optimising_VF/Waikerie/data_prep/"  

write.csv(RF_df, 
          paste0(output_path,"/step9_RF_df_input.csv"), 
          row.names=FALSE)
###################################################################################
### condense the data so it can be simply used in RF model - one row per sheep  ###
###################################################################################