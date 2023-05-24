### working how to cal a distance from a line.
#https://gis.stackexchange.com/questions/360675/how-to-calculate-the-distance-of-points-to-line-in-r
library(dplyr)
library(tidyverse)
library(readr)
library(lubridate)
library(DT)
library(sp)
#install.packages("sf")
library(sf)



GPS <- read_csv("W:/VF/Optimising_VF/Lameroo/data_prep/step3_clip.csv")

#turn into spatial data
GPS <-   st_as_sf(GPS,
                         coords = c("X", "Y"),
                         crs = 28354,
                         agr = "constant")

GPS <- GPS %>% dplyr::select (ID_jaxs,
                              Sheep_ID,
                              local_time, 
                              date,
                              DOY, 
                              geometry,
                              fencesID,
                              Audio_values,
                              Shock_values,
                              resting_percentage,
                              moving_percentage,
                              grazing_percentage,
                              training_period
                              #ID, 
                              #sheep, 
                              #treatment,
                              #DOT,
                              #herd_postion,
                              #local_time, date,DOY, geometry
                              )
names(GPS)



############################################################################################
############                  bring in boundaries             ##############################
############################################################################################

hard_fence_bound <- st_read("W:/VF/Sheep_Lameroo_2022/spatial_boundary/VF_working/HF_Lameroo_rough_proj.shp")  # this is the hard fences
VF <- st_read("W:/VF/Sheep_Lameroo_2022/spatial_boundary/VF_proj.shp")


hard_fence_bound <-  st_transform(hard_fence_bound, crs = 28354)
VF <-  st_transform(VF, crs = 28354)

exclusion_zone <- st_read("W:/VF/Optimising_VF/raw_data/Lameroo/Exclusion_zone.shp")
exclusion_zone <-   st_as_sf(exclusion_zone,
                         coords = c("X", "Y"),
                         crs = 28354,
                         agr = "constant")


VF_line <- st_read("W:/VF/Optimising_VF/raw_data/Lameroo/VF_line.shp")

############################################################################################

### check by plotting

str(GPS)


ggplot() +
  geom_sf(data = hard_fence_bound, color = "black", fill = NA) +
  geom_sf(data = VF_line, color = "red", fill = NA) +
  geom_sf(data = exclusion_zone, color = "blue", fill = NA) +
  
  geom_sf(data = GPS ,alpha = 0.03) +
  theme_bw()+
  theme(legend.position = "none",
        axis.ticks = element_blank(), axis.text.x = element_blank(), axis.text.y = element_blank())+
  labs(title = "check")


############################################################################################

GPS
VF_line

GPS <- GPS %>% 
  dplyr::mutate(dist_to_VF = st_distance(GPS, VF_line))
############################################################################################
### report if the point is in the exclusion zone

VF <- VF %>%  dplyr::select(Id, geometry)

VF_points <-  st_intersection(GPS, st_difference(VF)) %>% 
  dplyr::mutate(VF_EX = "inside_VF")

Exclusion_points <-  st_intersection(GPS, st_difference(exclusion_zone))%>% 
  dplyr::mutate(VF_EX = "outside_VF")

Exclusion_points <- Exclusion_points %>% dplyr::select(-"POLY_AREA")

names(VF_points)
names(Exclusion_points)

GPS_all <- rbind(VF_points, Exclusion_points)

str(GPS_all)


GPS_all <- GPS_all %>% dplyr::select(ID_jaxs:training_period, dist_to_VF, VF_EX, geometry)



coordinates <-as.data.frame( st_coordinates(GPS_all))
GPS_all_df <- as.data.frame(GPS_all)

GPS_all_df <- GPS_all_df %>% 
  dplyr::select(-"geometry")

names(GPS_all_df)
GPS_all_df <-   cbind(GPS_all_df,coordinates )





path_output_files <- "W:/VF/Optimising_VF/Lameroo/data_prep/" 
path_output_files
write.csv(GPS_all_df, 
          paste0(path_output_files,"/step4_dist_line_VF_zone.csv"), 
          row.names=FALSE)










#we have quite a few records that are logged in the exclusion zone

str(GPS_all_df)

count_exclusion_zone_occurance_per_animal_1 <- GPS_all_df %>%  group_by( Sheep_ID, VF_EX) %>% 
  summarise(count_records = n())
count_exclusion_zone_occurance_per_animal_1













