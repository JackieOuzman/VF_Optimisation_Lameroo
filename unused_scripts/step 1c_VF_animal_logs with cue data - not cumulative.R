library(dplyr)
library(tidyverse)
library(readr)
library(lubridate)
library(DT)
library(sp)
#install.packages("sf")
library(sf)


## this code takes the cleaned data and splits it into animals and converts the cumm value into counts

path_output_files <- "W:/VF/Optimising_VF/Lameroo/data_prep/"
VF_animals_logs <- read_csv(paste0(path_output_files,"Step1b_animals_GPS_trim_time.csv"))
names(VF_animals_logs)


### check on records

check_records <- VF_animals_logs %>% 
  dplyr::group_by(date,deviceName, Sheep_ID) %>% 
  dplyr::summarise(n_AudioCount = sum(cumulativeAudioCount),
                   n_ShockCount = sum(cumulativeShockCount)
                   )


check_records <- check_records %>% 
  arrange( Sheep_ID,date,deviceName, n_AudioCount )

### check on values
check_min_max <- VF_animals_logs %>% 
  dplyr::group_by(date,deviceName, Sheep_ID) %>% 
  dplyr::summarise(min_AudioCount = min(cumulativeAudioCount,na.rm = TRUE),
            max_AudioCount = max(cumulativeAudioCount,na.rm = TRUE),
            min_ShockCount = sum(cumulativeShockCount,na.rm = TRUE),
            max_ShockCount = max(cumulativeShockCount,na.rm = TRUE)
  )

check_min_max <- check_min_max %>% 
  arrange(Sheep_ID,deviceName,date, min_AudioCount )




#make a list###
str(VF_animals_logs)

#list_animals <- "1_1390038"
list_animals <- VF_animals_logs %>% 
  dplyr::select(deviceName) %>% 
  distinct(deviceName)
list_animals <-ungroup(list_animals)


list_animals$deviceName[1:34]
list_animals <- c(
  490705,
  1390182 ,
  1390456 ,
  1390577 ,
  1390581 ,
  1390737 ,
  1390743 ,
  1390749 ,
  1390736 ,
  1390826 ,
  1391505   
  )



for (list_animals in list_animals){
  
  
  ##################################################################################################################
  
  
  deviceName_x <- sub("*._", "", list_animals)
  
  df <- VF_animals_logs %>% 
    filter(deviceName == deviceName_x)
    #filter(deviceName == 490705)
  
  df <- df %>% arrange(local_time)
  names(df)
  df <- df %>% 
    mutate(Audio_values = cumulativeAudioCount - lag(cumulativeAudioCount))
  #test <- df %>%  select(deviceName, local_time, cumulativeAudioCount, Audio_values)
  df <- df %>% 
    mutate(Shock_values = cumulativeShockCount - lag(cumulativeShockCount))
  
  df <- df %>%  dplyr::select(ID_jaxs:fencesID ,Audio_values, Shock_values, resting_percentage:Y   )
  
  name <- paste0("temp_",deviceName_x)
  assign(name,df)
}

## merge into one file and remove the renaming records



Fence_all <- rbind(
  temp_490705,
  temp_1390182,
  temp_1390456,
  temp_1390577,
  temp_1390581,
  temp_1390737,
  temp_1390743,
  temp_1390749,
  temp_1390736,
  temp_1390826,
  temp_1391505
)
rm( temp_490705,
    temp_1390182,
    temp_1390456,
    temp_1390577,
    temp_1390581,
    temp_1390737,
    temp_1390743,
    temp_1390749,
    temp_1390736,
    temp_1390826,
    temp_1391505
  )






path_output_files
write.csv(Fence_all, 
          paste0(path_output_files,"/Step1c_animals_GPS_trim_time.csv"), 
          row.names=FALSE)

