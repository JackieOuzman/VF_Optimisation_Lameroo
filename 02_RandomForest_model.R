
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

#loading the model that was created in step 01 (using all the data)
RF_modelVs1 = readRDS(paste0(baseDir, "RF_model_vs1.rda"))

RF_modelVs1



#loading the data for predication 
#day 1 of the trial (early behaviour phase data)


RF_df_DOT1 <- read_csv(paste0(early_beheaviour_Dir, "step9_RF_df_input_early_beh.csv"))

RF_df_DOT1[ is.na(RF_df_DOT1) ] <- 0
    
### subset the data so the clm match what was used for 01 Random Forest model
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

###############################################################################
### use the early behaviour dataset (with the same clms) and the RF model created in step 01
pred_test <- predict(RF_modelVs1,
                     newdata = RF_df_DOT1_for_model,
                     type = "class")
pred_test 
#turn the results into a DF to compare what we got with all of the data vs early behav data
pred_test_for_early_beh <- as.data.frame(pred_test)

## covert the row ID into clm with animal
pred_test_for_early_beh <- cbind(sheep = rownames(pred_test_for_early_beh), pred_test_for_early_beh)
rownames(pred_test_for_early_beh) <- 1:nrow(pred_test_for_early_beh)
pred_test_for_early_beh$sheep <- as.double(pred_test_for_early_beh$sheep)

pred_test_for_early_beh

pred_test_for_early_beh <- pred_test_for_early_beh %>% 
  mutate(pred_compliance_score = case_when(
    pred_test == 1 ~ "non_compliant",
    pred_test == 0 ~ "compliant"
    
  ))

#############################################################################
## what did the did our criteria assign each animal to
str(RF_df_DOT1)

All_data_RF_results <- RF_df_DOT1 %>%  dplyr::select (sheep, compliance_score) 
All_data_RF_results <- All_data_RF_results %>% rename(compliance_score_all_data = compliance_score)


All_data_RF_results
pred_test_for_early_beh

#############################################################################
## put the two together


RF_results <- left_join(All_data_RF_results, pred_test_for_early_beh)

RF_results



outDir
write.csv(RF_results, 
          paste0(outDir,"/sheep_ID_compliance_score_withRF_results_DOT1.csv"), 
          row.names=FALSE)


rm(All_data_RF_results, pred_test_for_early_beh, RF_df_DOT1, RF_df_DOT1_for_model, RF_modelVs1)


RF_results <- RF_results %>% 
  mutate(correct_class = case_when(
    compliance_score_all_data == "compliant" & pred_compliance_score == "compliant" ~ 1,
    compliance_score_all_data == "non_compliant" & pred_compliance_score == "non_compliant" ~ 1,
    TRUE ~ 0
  ))

classified_correctly_count <- sum(RF_results$correct_class)
animals_count <- count(RF_results)
classified_correctly_percenatge <- (classified_correctly_count/ animals_count) *100
classified_correctly_percenatge
