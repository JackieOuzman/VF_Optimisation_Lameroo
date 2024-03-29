

# 
#RandomForest
# 
# 
# 
# 
# Code used here is based on Valavi et al (2022) supplementary material, to be cited
# as 
# Valavi, R., Guillera-Arroita, G, Lahoz-Monfort, J.J. & Elith, J. (2021) Predictive
# performance of presence-only species distribution models: a benchmark study with
# reproducible code. Ecological Monographs. 
# 
# RF background material:
# https://builtin.com/data-science/random-forest-algorithm


#install.packages("randomForest")
#install.packages("Hmisc")


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



baseDir <- "W:/VF/Optimising_VF/Lameroo/data_prep/"
outDir <- "W:/VF/Optimising_VF/Lameroo/data_prep/RF_model_outputs/"

numRandPts <- 50000

#aggregFactors <- c(1:7)    # using raster input data aggregated over this range of values

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
# load data

RF_df_raw <- read_csv("W:/VF/Optimising_VF/Lameroo/data_prep/step9_RF_df_input.csv")

RF_df <- RF_df_raw
str(RF_df)
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
unique(RF_df$compliance_score)

RF_df <- RF_df %>%
       mutate(compliance_score = replace(compliance_score, compliance_score %in% c("non_compliant"), 1),
              compliance_score = replace(compliance_score, compliance_score %in% c("compliant"), 0)) %>% 
       mutate(compliance_score = as.factor(compliance_score))


### the model wont run could because of the NA values or because its so unbalanced?
#replace all the NA with zero 

RF_df[ is.na(RF_df) ] <- 0


str(RF_df)

RF_df <- RF_df %>% dplyr::select(compliance_score,
                                 mean_dist_frm_VF_inside_inclusion,
                                 mean_dist_frm_VF_outside_inclusion,
                                 max_dist_frm_VF_outside_inclusion,
                                 total_dist_travel,
                                 dist_travel_ratio,
                                 
                                 mean_audio,
                                 mean_pulse,
                                 mean_ratio,
                                 
                                 mean_resting,
                                 mean_grazing,
                                 mean_moving,
                                 mean_numb_sheep_close
                                )



RF_df_corr_input <- RF_df %>% dplyr::select(-compliance_score)
str(RF_df_corr_input)


res2<-rcorr(as.matrix(RF_df_corr_input))
# ++++++++++++++++++++++++++++
# flattenCorrMatrix
# ++++++++++++++++++++++++++++
# cormat : matrix of the correlation coefficients
# pmat : matrix of the correlation p-values
flattenCorrMatrix <- function(cormat, pmat) {
  ut <- upper.tri(cormat)
  data.frame(
    row = rownames(cormat)[row(cormat)[ut]],
    column = rownames(cormat)[col(cormat)[ut]],
    cor  =(cormat)[ut],
    p = pmat[ut]
  )
}

correaltion <- flattenCorrMatrix(res2$r, res2$P)
correaltion <- correaltion %>%   arrange(desc(abs(cor)))
correaltion$cor <- round(correaltion$cor,2)
correaltion

 

write.csv(correaltion, 
          paste0(outDir,"/RF_input_data_Lameroo_correaltion_matrix.csv"), 
          row.names=FALSE)

#---------------------------------------##



str(RF_df)

# remove highly correlated variables
RF_df <- RF_df %>% dplyr::select(compliance_score,
                                 mean_dist_frm_VF_inside_inclusion,
                                 mean_dist_frm_VF_outside_inclusion,
                                 max_dist_frm_VF_outside_inclusion,
                                 #total_dist_travel,
                                 dist_travel_ratio,
                                 
                                 mean_audio,
                                 #mean_pulse,
                                 mean_ratio,
                                 
                                 mean_resting,
                                 #mean_grazing,
                                 mean_moving,
                                 mean_numb_sheep_close
                                 )

names(RF_df)

RF_model <- randomForest(formula = compliance_score ~.,
            data = RF_df,
            ntree = 1500,
            #sampsize = smpsize,
            replace = TRUE)

RF_model



# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
RF_model
plot(RF_model)
importance(RF_model)

whats_important_for_model <- as.data.frame(importance(RF_model))
whats_important_for_model <- whats_important_for_model %>%  arrange(desc(MeanDecreaseGini))

whats_important_for_model$MeanDecreaseGini <- round(whats_important_for_model$MeanDecreaseGini,2)
whats_important_for_model <- as.data.frame(MeanDecreaseGini)

write.csv(whats_important_for_model, 
          paste0(outDir,"/RF_input_data_Lameroo_MeanDecreaseGini.csv"), 
          row.names=TRUE)


# ---------------------------------------------------------------------------------------------------
# saving the model and example of loading the model



saveRDS(RF_model, file = paste0("W:/VF/Optimising_VF/Lameroo/data_prep/RF_model_outputs/", "RF_model_vs1.rda"))
          
         


