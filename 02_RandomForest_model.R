
libs <- c("tidyverse", "dplyr",  "ggplot2", "randomForest", "Hmisc")


install.libraries <- function(lib=NULL){
  new <- lib[!(lib %in% installed.packages()[, "Package"])]
  if (length(new)){   
    install.packages(new, dependencies = TRUE)
  }
} 

load.libraries <- function(lib=NULL){
  sapply(libs, require, character.only = TRUE)
}

install.libraries(libs)
load.libraries(libs)

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 



baseDir <- "W:/VF/Optimising_VF/Lameroo/data_prep/RF_model_outputs/"
outDir <- "W:/VF/Optimising_VF/Lameroo/data_prep/RF_model_outputs/"
early_beheaviour_Dir <- "W:/VF/Optimising_VF/Lameroo/data_prep/"

#loading the model
RF_modelVs1 = readRDS(paste0(baseDir, "RF_model_vs1.rda"))

RF_modelVs1



#loading the data for predication 
#day 1 of the trial


RF_df_DOT1 <- read_csv(paste0(early_beheaviour_Dir, "step9_RF_df_input_early_beh.csv"))

RF_df_DOT1[ is.na(RF_df_DOT1) ] <- 0
str(RF_df_DOT1)


names(RF_df_DOT1)      

RF_df_DOT1_for_model <- RF_df_DOT1 %>% dplyr::select(compliance_score,
                                 mean_dist_frm_VF_inside_inclusion,
                                 mean_dist_frm_VF_outside_inclusion,
                                 max_dist_frm_VF_outside_inclusion,
                                 total_dist_travel,
                                 dist_travel_ratio,
                                 mean_audio,
                                 mean_pulse,
                                 mean_ratio,
                                 mean_grazing,
                                 mean_moving,
                                 mean_numb_sheep_close
)





str(RF_df_DOT1_for_model)


pred_test <- predict(RF_modelVs1,
                     newdata = RF_df_DOT1_for_model,
                     type = "class")
pred_test #this gives me a list of my sheep and what class they are assigned to - I hope?

pred_test_for_early_beh <- as.data.frame(pred_test, col.names = names(x))
pred_test_for_early_beh


str(RF_df_DOT1)

All_data_RF_results <- RF_df_DOT1 %>%  dplyr::select (sheep, compliance_score) 
All_data_RF_results <- All_data_RF_results %>% rename(compliance_score_all_data = compliance_score)

pred_test_for_early_beh




#--- delete below
sheep_ID_compliance_score_withRF_results <- cbind(sheep_ID_compliance_score, Check_output_of_RF_model)
sheep_ID_compliance_score_withRF_results <- sheep_ID_compliance_score_withRF_results %>% 
  mutate(pred_compliance_score = case_when(
    pred_test == 1 ~ "non_compliant",
    pred_test == 0 ~ "compliant"
    
  ))

sheep_ID_compliance_score_withRF_results

outDir
write.csv(sheep_ID_compliance_score_withRF_results, 
          paste0(outDir,"/sheep_ID_compliance_score_withRF_results_DOT1.csv"), 
          row.names=FALSE)




