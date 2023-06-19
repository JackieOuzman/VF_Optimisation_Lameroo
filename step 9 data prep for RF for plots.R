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

str(collared_animals)

RF_df <- collared_animals %>% distinct(sheep, .keep_all = TRUE) %>% 
  dplyr::select(sheep, compliance_score)
RF_df %>% distinct(compliance_score)
RF_df <- RF_df %>%  filter(compliance_score != "NA" )


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
### Hours for early behaviour and hours for later behaviour


hours_behav <- collared_animals %>% count(behaviour_stage, sheep)

hours_behav$n <- as.double(hours_behav$n)
hours_behav <-  hours_behav %>% mutate(mins = n *10) #each data point is a 10min log

hours_behav
#####################################################################################
### add in the distance between animals
######################################################################################

dist_bewteen <- read_csv("W:/VF/Optimising_VF/Lameroo/data_prep/step7_count_close_animals.csv")



## make a new variable hours
dist_bewteen$time_step <- as.POSIXct(dist_bewteen$time_step,  tz = "Australia/Adelaide")
dist_bewteen <- dist_bewteen %>%  dplyr::mutate(date= date(time_step))  

#### add to the collared animals df
collared_animals <- left_join(collared_animals, dist_bewteen)



#####################################################################################
### what is the average distance to the fence?
## average distance to VF when inside inclusion zone and when outside inclusion zone
## max distance to Vf 

######################################################################################

str(collared_animals)
#----create new clms distance from VF when inside inclusion zone and when outside inclusion zone
#---- if the record is not in the inclusion zone it gets an NA for dist_frm_VF_inside_inclusion and vice versa
#---- the other option would be to get a zero value which genertaed different mean and max values in the summary data


collared_animals <- collared_animals %>%
  dplyr::mutate(dist_frm_VF_inside_inclusion = case_when(VF_EX == "inside_VF" ~ dist_to_VF,
                                                         TRUE ~ NA_real_))

collared_animals <- collared_animals %>%
  dplyr::mutate(dist_frm_VF_outside_inclusion = case_when(VF_EX == "outside_VF" ~ dist_to_VF,
                                                         TRUE ~ NA_real_))



#----summaries the distance to VF variable by grouping early behaviour
str(collared_animals)


dist_frm_VF_summary <- collared_animals %>%  group_by(behaviour_stage, compliance_score) %>% 
  summarise(
            mean_dist_frm_VF_inside_inclusion =mean(dist_frm_VF_inside_inclusion, na.rm=TRUE),
            SD_dist_frm_VF_inside_inclusion =sd(dist_frm_VF_inside_inclusion, na.rm=TRUE),
            
            max_dist_frm_VF_inside_inclusion =max(dist_frm_VF_inside_inclusion, na.rm=TRUE),
            
            
            mean_dist_frm_VF_outside_inclusion =mean(dist_frm_VF_outside_inclusion, na.rm=TRUE), #this will give you a warning message beacuse of the heaps of zero values
            SD_dist_frm_VF_outside_inclusion =sd(dist_frm_VF_outside_inclusion, na.rm=TRUE),
            
            max_dist_frm_VF_outside_inclusion =max(dist_frm_VF_outside_inclusion, na.rm=TRUE),
            
            )
################################################################################
            
dist_frm_VF_summary <- as.data.frame(dist_frm_VF_summary)
dist_frm_VF_summary[ is.na(dist_frm_VF_summary) ] <- NA

str(dist_frm_VF_summary)

### recode the NA or missing data
dist_frm_VF_summary <- dist_frm_VF_summary %>% 
  dplyr::mutate(max_dist_frm_VF_outside_inclusion =
                  case_when(max_dist_frm_VF_outside_inclusion > 0 ~ max_dist_frm_VF_outside_inclusion,
                TRUE   ~ NA_real_ ))

###--- rename the output and add the hours

RF_df <- dist_frm_VF_summary


rm(dist_frm_VF_summary, temp)

#####################################################################################
### what is the total distance travelled for the length of the per hours?

######################################################################################
str(collared_animals)

dist_taken_summary <- collared_animals %>%  group_by(sheep, behaviour_stage) %>% 
  summarise(total_dist_travel  =sum(step, na.rm=TRUE))


dist_taken_summary <- left_join(dist_taken_summary, hours_behav)
dist_taken_summary <- dist_taken_summary %>% 
  mutate(dist_travel_ratio = total_dist_travel/ mins)
dist_taken_summary

temp <- collared_animals %>% dplyr::select(behaviour_stage, compliance_score, sheep)
temp <- temp %>% distinct(behaviour_stage, compliance_score, sheep)
dist_taken_summary <- left_join(dist_taken_summary, temp)

dist_taken_summary_2 <- dist_taken_summary %>%  group_by(behaviour_stage, compliance_score) %>% 
  summarise(mean_dist_ratio  =mean(dist_travel_ratio, na.rm=TRUE))

RF_df <- left_join(RF_df, dist_taken_summary_2)
rm(dist_taken_summary, dist_taken_summary_2,temp)



#####################################################################################
### what is the total number of cues audio and pulse and ratio for the length of the trial?
### could do average  day 1 and 2
######################################################################################

str(collared_animals)


#rename so it matched other files
collared_animals <- collared_animals %>% 
  rename(audio = Audio_values ,
         pulse = Shock_values)

cue_summary_total <- collared_animals %>%  group_by(sheep, behaviour_stage, compliance_score) %>% 
  summarise(total_audio  =sum(audio, na.rm=TRUE),
            total_pulse  =sum(pulse, na.rm=TRUE),
            total_ratio = total_audio/(total_pulse+total_audio)*100)
cue_summary_total
## add in the mins logged data
cue_summary_total <- left_join(cue_summary_total, hours_behav)

cue_summary_total <- cue_summary_total %>% 
  mutate(total_audio_per_logged = total_audio/ mins,
         total_pulse_per_logged = total_pulse/ mins,
         total_ratio_per_logged = ((total_ratio/ mins))
         )
cue_summary_total <- cue_summary_total %>% group_by(behaviour_stage, compliance_score) %>% 
  summarise(Mean_total_audio_per_logged = mean(total_audio_per_logged, na.rm = TRUE),
            SD_total_audio_per_logged = sd(total_audio_per_logged, na.rm = TRUE),
            
            Mean_total_pulse_per_logged = mean(total_pulse_per_logged, na.rm = TRUE),
            SD_total_pulse_per_logged = sd(total_pulse_per_logged, na.rm = TRUE),
            
            Mean_total_ratio_per_logged = mean(total_ratio_per_logged, na.rm = TRUE),
            SD_total_ratio_per_logged = sd(total_ratio_per_logged, na.rm = TRUE))

RF_df <- left_join(RF_df, cue_summary_total)


cue_summary <- collared_animals %>%  group_by(behaviour_stage, compliance_score) %>% 
  summarise(mean_audio  =mean(audio, na.rm=TRUE),
            SD_audio  =sd(audio, na.rm=TRUE),
            
            mean_pulse  =mean(pulse, na.rm=TRUE),
            SD_pulse  =sd(pulse, na.rm=TRUE),
            
            mean_ratio = mean_audio/(mean_pulse+mean_audio)*100)
            
cue_summary
cue_summary[ is.na(cue_summary) ] <- NA


RF_df <- left_join(RF_df, cue_summary)
rm(cue_summary, cue_summary_total )





#####################################################################################
### what is the proportion of activity spent resting lying grazing? for the length of the trial?

######################################################################################
str(collared_animals)

beha_summary_prop <- collared_animals %>%  
  #group_by(sheep, behaviour_stage) %>%  
  dplyr::mutate(
    all_beh = (resting_percentage+ grazing_percentage+ moving_percentage),
    prop_resting = (resting_percentage/all_beh)*100,
    prop_grazing = (grazing_percentage/all_beh)*100,
    prop_moving = (moving_percentage/all_beh)*100    )

beha_summary_prop


beha_summary <- beha_summary_prop %>%  group_by(behaviour_stage, compliance_score) %>% 
  summarise(mean_resting   =mean(prop_resting,  na.rm=TRUE),
            SD_resting   =sd(prop_resting,  na.rm=TRUE),
            
            mean_grazing  =mean(prop_grazing , na.rm=TRUE),
            SD_grazing  =sd(prop_grazing , na.rm=TRUE),
            
            mean_moving   =mean(prop_moving,  na.rm=TRUE),  
            SD_moving   =sd(prop_moving,  na.rm=TRUE)                 
  )                  

beha_summary

     

RF_df <- left_join(RF_df, beha_summary)
rm(beha_summary, beha_summary_prop)


#####################################################################################
### add in the distance between animals
######################################################################################


str(collared_animals)

dist_bewteen <- collared_animals %>% 
  group_by(behaviour_stage, compliance_score) %>% 
  summarise(mean_numb_sheep_close    =mean(numb_sheep_close ,  na.rm=TRUE),
            SD_numb_sheep_close    =sd(numb_sheep_close ,  na.rm=TRUE))
            
dist_bewteen


RF_df <- left_join(RF_df, dist_bewteen)
rm(dist_bewteen)


###################################################################################
###                 write out df ready for the next step                      ###
###################################################################################

output_path <- "W:/VF/Optimising_VF/Lameroo/data_prep/"  

write.csv(RF_df, 
          paste0(output_path,"/step9_RF_for_plots.csv"), 
          row.names=FALSE)
###################################################################################








