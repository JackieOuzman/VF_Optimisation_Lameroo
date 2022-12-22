
library(dplyr)
library(tidyverse)
library(readr)
library(lubridate)
library(DT)
library(sp)
#install.packages("sf")
library(sf)







############################################################################################
############       bring in data created in step1a           ##############################
############################################################################################

path_step1 <- "W:/VF/Optimising_VF/Lameroo/data_prep/"
raw_data <- "W:/VF/Optimising_VF/raw_data/Lameroo/"



GPS <- read_csv(paste0(raw_data, "db_trial_csiro_lemaroo_mob_287_filtered.csv")) 

str(GPS)


#format time and date clm from character to time
GPS <-  GPS %>%
  mutate(timeOfEvent = as.POSIXct(timeOfEvent, tz = "GMT", format = "%d/%m/%Y %H:%M"))


GPS <- GPS %>% 
  mutate(GMT = ymd_hms(timeOfEvent, tz = "GMT"))

GPS <- GPS %>% 
  mutate(local_time = with_tz(GMT, tz = "Australia/Adelaide"))

## Add a clm for ID_jaxs
GPS <- GPS %>% 
  dplyr::mutate( ID_jaxs = row_number())


### what are the fences callled in this dataset?
unique(GPS$fencesID) # we only have 2 14594 and NULL


## reorder the clms
GPS <- GPS %>% 
  dplyr::select(ID_jaxs,deviceUIDHex:local_time)



GPS <- GPS %>% 
  mutate(date = as.Date(local_time, tz= "Australia/Adelaide"),
         DOY = yday(date))


#############################################################################################
####    Assign collar to sheep names #####
unique(GPS$deviceName)

GPS <- GPS %>% 
  mutate(Sheep_ID = case_when(
    deviceName == 1390743 ~ "10",#was called blank
    deviceName == 1390581 ~ "1",
    deviceName == 1390826 ~ "2", #before 18/10 I assume at 10:30
    deviceName == 1391505 ~ "2", #after 18/10 I assume at 10:30
    deviceName == 0490705 ~ "3",
    deviceName == 1390577 ~ "4",
    deviceName == 1390737 ~ "5",
    deviceName == 1390749 ~ "6",
    deviceName == 1390456 ~ "7",
    deviceName == 1390182 ~ "8",
    deviceName == 1390736 ~ "9",
    deviceName == 1390189 ~ "water_pt",
    TRUE                      ~ "other"
    
  ))

#remove sheep 2 records
GPS_no2 <- GPS %>% 
  filter(Sheep_ID != "2" )

#subset data keep only sheep 2 records
animal_2 <- GPS %>% 
  filter(Sheep_ID == "2" ) 

#filter out time sheep 2 used each device
animal_1390826 <- animal_2 %>% 
  filter( deviceName == "1390826" ) %>% 
  filter(local_time <=  ymd_hms("2022-10-18 10:30:00", tz= "Australia/Adelaide")) 

animal_1391505 <- animal_2 %>% 
  filter( deviceName == "1391505" ) %>% 
  filter(local_time >=  ymd_hms("2022-10-18 10:30:00", tz= "Australia/Adelaide")) 


animal_2 <- rbind(animal_1390826,animal_1391505 )

GPS <- rbind (GPS_no2 , animal_2 )
rm(animal_2, animal_1390826,animal_1391505, GPS_no2 )























############################################################################################
############                  Turn into spatial data          ##############################
############################################################################################
str(GPS)

#turn into spatial data
## remove null values in coodinates
GPS <- GPS %>% 
  filter(!is.na(gpsData.lng))

#turn into spatial data
GPS_sf <-
  st_as_sf(GPS,
           coords = c("gpsData.lng", "gpsData.lat"),
           crs = 4326,
           agr = "constant")

GPS_sf_trans <-
  st_transform(GPS_sf, crs = 28354)


rm(GPS_sf)




############################################################################################
############                  bring in boundaries             ##############################
############################################################################################
hard_fence_bound <- st_read("W:/VF/Sheep_Lameroo_2022/spatial_boundary/VF_working/HF_Lameroo_rough_proj.shp")  # this is the hard fences
VF <- st_read("W:/VF/Sheep_Lameroo_2022/spatial_boundary/VF_proj.shp")


hard_fence_bound <-  st_transform(hard_fence_bound, crs = 28354)
VF <-  st_transform(VF, crs = 28354)


str(GPS)

ggplot() +
  geom_sf(data = hard_fence_bound, color = "black", fill = NA) +
  geom_sf(data = VF, color = "black", fill = NA) +
  geom_sf(data = GPS_sf_trans ,alpha = 0.03) +
  theme_bw()+
  facet_wrap(.~ date)+
  theme(legend.position = "none",
        axis.ticks = element_blank(), axis.text.x = element_blank(), axis.text.y = element_blank())+
  labs(title = "")


################################################################################
#### filtering out GPS data based on times start and end of the trial


# start of trial and training period (according to sue) - keep everything after  17th 11:35 or 11:40 s above

GPS_sf_trans <- GPS_sf_trans %>% 
  filter(
    local_time >=  ymd_hms("2022-10-17 11:40:00", tz= "Australia/Adelaide"))

GPS_sf_trans <- GPS_sf_trans %>% 
  filter(
    local_time <=  ymd_hms("2022-10-21 11:50:00", tz= "Australia/Adelaide"))


### define a training period with new clm

GPS_sf_trans <- GPS_sf_trans %>% 
  mutate(training_period = case_when(
    local_time <= ymd_hms("2022-10-17 13:10:00", tz= "Australia/Adelaide")~ "training",
    TRUE                      ~ "non_training"
    
  ))



#### each day the animals were yarded so i need to remove this data

# let divide the data per day
day_17 <- GPS_sf_trans %>%  filter(date == "2022-10-17")
day_18 <- GPS_sf_trans %>%  filter(date == "2022-10-18")
day_19 <- GPS_sf_trans %>%  filter(date == "2022-10-19")
day_20 <- GPS_sf_trans %>%  filter(date == "2022-10-20")
day_21 <- GPS_sf_trans %>%  filter(date == "2022-10-21")

# keep everything after before yarding and after yarding

day_18_before_yarding <- day_18 %>%
  filter(local_time <=  ymd_hms("2022-10-18 09:40:00", tz = "Australia/Adelaide"))
day_18_after_yarding <- day_18 %>%
  filter(local_time >=  ymd_hms("2022-10-18 10:30:00", tz = "Australia/Adelaide"))

day_18_clean <- rbind(day_18_before_yarding, day_18_after_yarding)
rm(day_18_before_yarding, day_18_after_yarding, day_18)


day_19_before_yarding <- day_19 %>%
  filter(local_time <=  ymd_hms("2022-10-19 09:10:00", tz = "Australia/Adelaide"))
day_19_after_yarding <- day_19 %>%
  filter(local_time >=  ymd_hms("2022-10-19 10:18:00", tz = "Australia/Adelaide"))

day_19_clean <- rbind(day_19_before_yarding, day_19_after_yarding)
rm(day_19_before_yarding, day_19_after_yarding, day_19)



day_20_before_yarding <- day_20 %>%
  filter(local_time <=  ymd_hms("2022-10-20 08:58:00", tz = "Australia/Adelaide"))
day_20_after_yarding <- day_20 %>%
  filter(local_time >=  ymd_hms("2022-10-20 10:19:00", tz = "Australia/Adelaide"))

day_20_clean <- rbind(day_20_before_yarding, day_20_after_yarding)
rm(day_20_before_yarding, day_20_after_yarding, day_20)


### put it back togther 

animals_GPS_trim_time <- rbind(day_17, day_18_clean, day_19_clean, day_19_clean, day_20_clean, day_21)

rm(day_17, day_18_clean, day_19_clean, day_21, day_20_clean, GPS_sf_trans)

########################################################################################
########################################################################################

### remove the water and other animals logs

unique(animals_GPS_trim_time$Sheep_ID)

animals_GPS_trim_time <- animals_GPS_trim_time %>% 
  filter(Sheep_ID !=  "other") %>% 
  filter(Sheep_ID !=  "water_pt")




## check

ggplot() +
  geom_sf(data = hard_fence_bound, color = "black", fill = NA) +
  geom_sf(data = VF, color = "black", fill = NA) +
  geom_sf(data = animals_GPS_trim_time ,alpha = 0.03) +
  facet_wrap(.~ date)+
  theme_bw()+
  theme(legend.position = "none",
        axis.ticks = element_blank(), axis.text.x = element_blank(), axis.text.y = element_blank())+
  labs(title = "All animal logs trimmed time",
       subtitle = "log when animals were yarded removed")



# -------------------------------------------------------------------------------------------------- ###


#I think this should be the end of step 1.


#Next steps boundaries trim location and time
## merge in other animal data
## look for weather data ? anything else??




########################################################################################################



output_path <- "W:/VF/Optimising_VF/Lameroo/data_prep"  #animals_GPS_trim_time


############################################################################################################################
### format the aniaml log data so I output the clm with local time and keep time difference cals and have clm for x and y

## convert the geom clm into x and y clms
str(animals_GPS_trim_time)

coordinates <-as.data.frame( st_coordinates(animals_GPS_trim_time))
GPS_trim_time_df <- as.data.frame(animals_GPS_trim_time)

GPS_trim_time_df <- GPS_trim_time_df %>% 
  dplyr::select(-"geometry")


GPS_trim_time <-   cbind(GPS_trim_time_df,coordinates )
## ensure the date and time clms are outputting and outputting in the correct format.


GPS_trim_time$local_time <-   format(GPS_trim_time$local_time, usetz=TRUE)
GPS_trim_time$GMT        <-   format(GPS_trim_time$GMT, usetz=TRUE)
GPS_trim_time$start_fence <-  format(GPS_trim_time$start_fence, usetz=TRUE)
GPS_trim_time$end_fence    <- format(GPS_trim_time$end_fence, usetz=TRUE)
GPS_trim_time$start_trial    <- format(GPS_trim_time$start_trial, usetz=TRUE)

write.csv(GPS_trim_time, 
          paste0(output_path,"/Step1b_animals_GPS_trim_time.csv"), 
          row.names=FALSE)
#############################################################



