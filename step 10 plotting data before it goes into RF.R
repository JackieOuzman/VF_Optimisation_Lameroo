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
#install.packages("ggpubr")
library(ggpubr)

Summary_df_for_plots <- read_csv("W:/VF/Optimising_VF/Lameroo/data_prep/step9_RF_for_plots.csv")

Summary_df_for_plots$local_time <- as.POSIXct(Summary_df_for_plots$local_time,  tz = "Australia/Adelaide")

names(Summary_df_for_plots)
################################################################################
######## PLOTS
################################################################################

Mean_VF_graz <- ggplot(Summary_df_for_plots, aes(x=behaviour_stage, y=mean_dist_frm_VF_inside_inclusion, 
                                                 fill= compliance_score 
                                          )) + 
  geom_bar(stat= "identity", position=position_dodge())+
  #theme(legend.position="none")+
  # geom_errorbar(aes(ymin=mean_dist_frm_VF_inside_inclusion-SD_dist_frm_VF_inside_inclusion, 
  #                   ymax=mean_dist_frm_VF_inside_inclusion+SD_dist_frm_VF_inside_inclusion), width=.2,
  #               position=position_dodge(.9))+
  labs(title="Mean Distance from VF when in grazing zone", 
       x="", 
       y = "mean")+
  scale_fill_manual(values=c("#999999",  "#56B4E9"))
   

Max_VF_graz <- ggplot(Summary_df_for_plots, aes(x=behaviour_stage, y=max_dist_frm_VF_inside_inclusion, 
                                                fill= compliance_score 
)) + 
  geom_bar(stat= "identity", position=position_dodge())+
  theme(legend.position="none")+
  labs(title="Max Distance from VF when in grazing zone", 
       x="", 
       y = "Max")+
  scale_fill_manual(values=c("#999999",  "#56B4E9"))

Mean_VF_exl <- ggplot(Summary_df_for_plots, aes(x=behaviour_stage, y=mean_dist_frm_VF_outside_inclusion, 
                                                fill= compliance_score )) + 
  geom_bar(stat= "identity", position=position_dodge())+
  theme(legend.position="none")+
  # geom_errorbar(aes(ymin=mean_dist_frm_VF_outside_inclusion-SD_dist_frm_VF_outside_inclusion, 
  #                   ymax=mean_dist_frm_VF_outside_inclusion+SD_dist_frm_VF_outside_inclusion), width=.2,
  #               position=position_dodge(.9))+
  labs(title="Mean Distance from VF when in exclusion zone", 
       x="", 
       y = "mean")+
  scale_fill_manual(values=c("#999999",  "#56B4E9"))


Max_VF_ex <- ggplot(Summary_df_for_plots, aes(x=behaviour_stage, y=max_dist_frm_VF_outside_inclusion, 
                                              fill= compliance_score )) + 
  geom_bar(stat= "identity", position=position_dodge())+
  theme(legend.position="none")+
  labs(title="Max Distance from VF when in exclusion zone", 
       x="", 
       y = "Max")+
  scale_fill_manual(values=c("#999999",  "#56B4E9"))


mean_dist_travel <- ggplot(Summary_df_for_plots, aes(x=behaviour_stage, y=mean_dist_ratio, 
                                                     fill= compliance_score 
)) + 
  geom_bar(stat= "identity", position=position_dodge())+
  theme(legend.position="none")+
  # geom_errorbar(aes(ymin=mean_dist_ratio-SD_dist_frm_VF_outside_inclusion, 
  #                   ymax=mean_dist_ratio+SD_dist_frm_VF_outside_inclusion), width=.2,
  #               position=position_dodge(.9))+
  labs(title="Mean travelled expressed over mins of logged points", 
       x="", 
       y = "mean")+
  scale_fill_manual(values=c("#999999",  "#56B4E9"))



distance_plots <- ggarrange(Mean_VF_graz, Max_VF_graz, 
          Mean_VF_exl , Max_VF_ex ,mean_dist_travel,
          
          labels = c("A", "B", "C", "D"),
          ncol = 2, nrow = 3)

distance_plots
################################################################################
## Plots for behaviour 
################################################################################
str(Summary_df_for_plots)

Mean_resting <- ggplot(Summary_df_for_plots, aes(x=behaviour_stage, y=mean_resting, 
                                                 fill = behaviour_stage)) + 
  geom_bar(stat= "identity")+
  ylim(0, 100)+
  theme(legend.position="none")+
  labs(title="Mean proportion of time spent resting", 
       x="", 
       y = "mean")+
  scale_fill_manual(values=c("#999999",  "darkseagreen"))

Mean_grazing <- ggplot(Summary_df_for_plots, aes(x=behaviour_stage, y=mean_grazing, 
                                                 fill = behaviour_stage)) + 
  geom_bar(stat= "identity")+
  ylim(0, 100)+
  theme(legend.position="none")+
  labs(title="Mean proportion of time spent grazing", 
       x="", 
       y = "mean")+
  scale_fill_manual(values=c("#999999",  "darkseagreen"))

Mean_moving <- ggplot(Summary_df_for_plots, aes(x=behaviour_stage, y=mean_moving, 
                                                 fill = behaviour_stage)) + 
  geom_bar(stat= "identity")+
  ylim(0, 100)+
  theme(legend.position="none")+
  labs(title="Mean proportion of time spent moving", 
       x="", 
       y = "mean")+
  scale_fill_manual(values=c("#999999",  "darkseagreen"))

behaviour_plots <- ggarrange(Mean_resting, Mean_grazing, 
                             Mean_moving , 
                            
                            labels = c("A", "B", "C"),
                            ncol = 2, nrow = 2)
################################################################################
## Plots for cues 
################################################################################
str(Summary_df_for_plots)

Mean_audio <- ggplot(Summary_df_for_plots, aes(x=behaviour_stage, y=mean_audio, 
                                                fill = behaviour_stage)) + 
  geom_bar(stat= "identity")+
  theme(legend.position="none")+
  labs(title="Mean audio cues", 
       x="", 
       y = "mean")+
  scale_fill_manual(values=c("#999999",  "cornflowerblue"))

Mean_pulse <- ggplot(Summary_df_for_plots, aes(x=behaviour_stage, y=mean_pulse, 
                                               fill = behaviour_stage)) + 
  geom_bar(stat= "identity")+
  theme(legend.position="none")+
  labs(title="Mean pulse cues", 
       x="", 
       y = "mean")+
  scale_fill_manual(values=c("#999999",  "cornflowerblue"))

Mean_ratio <- ggplot(Summary_df_for_plots, aes(x=behaviour_stage, y=mean_ratio, 
                                               fill = behaviour_stage)) + 
  geom_bar(stat= "identity")+
  theme(legend.position="none")+
  labs(title="Mean of ratio", 
       x="", 
       y = "mean")+
  scale_fill_manual(values=c("#999999",  "cornflowerblue"))

Mean_total_audio_logged <- ggplot(Summary_df_for_plots, aes(x=behaviour_stage, y=Mean_total_audio_per_logged, 
                                               fill = behaviour_stage)) + 
  geom_bar(stat= "identity")+
  theme(legend.position="none")+
  labs(title="Mean audio (expressed per logged point)", 
       x="", 
       y = "mean")+
  scale_fill_manual(values=c("#999999",  "cornflowerblue"))

Mean_total_pulse_logged <- ggplot(Summary_df_for_plots, aes(x=behaviour_stage, y=Mean_total_pulse_per_logged, 
                                                            fill = behaviour_stage)) + 
  geom_bar(stat= "identity")+
  theme(legend.position="none")+
  labs(title="Mean pulse (expressed per logged point)", 
       x="", 
       y = "mean")+
  scale_fill_manual(values=c("#999999",  "cornflowerblue"))

Mean_total_ratio_logged <- ggplot(Summary_df_for_plots, aes(x=behaviour_stage, y=Mean_total_ratio_per_logged, 
                                                            fill = behaviour_stage)) + 
  geom_bar(stat= "identity")+
  theme(legend.position="none")+
  labs(title="Mean ratio (expressed per logged point)", 
       x="", 
       y = "mean")+
  scale_fill_manual(values=c("#999999",  "cornflowerblue"))

cue_plots <- ggarrange(Mean_audio, Mean_pulse,Mean_ratio, 
                       Mean_total_audio_logged, Mean_total_pulse_logged, Mean_total_ratio_logged,
                             
                             labels = c("A", "B", "C"),
                             ncol = 3, nrow = 2)
################################################################################
## Plots for animals close 
################################################################################
str(Summary_df_for_plots)
Mean_animals_close <- ggplot(Summary_df_for_plots, aes(x=behaviour_stage, y=mean_numb_sheep_close, 
                                                            fill = behaviour_stage)) + 
  geom_bar(stat= "identity")+
  theme(legend.position="none")+
  labs(title="Mean number of sheep that are close", 
       x="", 
       y = "mean")+
  scale_fill_manual(values=c("#999999",  "blue"))


################################################################################
distance_plots
behaviour_plots
cue_plots

Mean_animals_close
