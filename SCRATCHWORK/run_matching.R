library(data.table)
library(readr)
library(dplyr)
library(tidyverse)
library(PanelMatch)
library(writexl)
library(foreach)
library(doParallel)

##### Not Using SVC Yet
# 48.82531 mins: no int v2
# 2.949736 hours: no int v3
# 3.685844 mins: int v1
# 5.738919 mins: int v2
# 34 mins for int v3 + int v4
covs_to_check <- c("bachelors_perc", "med_household_income_adj", "white_perc",  "pop_estimate", "med_precip", "med_avg_temp", "TOT_REV", "TOT_ASSET", "NUM_ORGS", "NTEEV2_ART", "NTEEV2_EDU", "NTEEV2_ENV", "NTEEV2_HEL", "NTEEV2_HMS", "NTEEV2_HOS", "NTEEV2_IFA", "NTEEV2_MMB", "NTEEV2_PSB", "NTEEV2_REL", "NTEEV2_UNI", "NTEEV2_UNU", "NTEEV2_NA")

formulas <- list("L1" = "~ TAX_YEAR + lat*lng + I(lag(TOT_ASSET, 1:1)) + I(lag(NTEEV2_ART, 1:1)) + I(lag(NTEEV2_EDU, 1:1)) + I(lag(NTEEV2_ENV, 1:1)) + I(lag(NTEEV2_HEL, 1:1)) + I(lag(NTEEV2_HMS, 1:1)) + I(lag(NTEEV2_HOS, 1:1)) + I(lag(NTEEV2_IFA, 1:1)) + I(lag(NTEEV2_MMB, 1:1)) + I(lag(NTEEV2_PSB, 1:1)) + I(lag(NTEEV2_REL, 1:1)) + I(lag(NTEEV2_UNI, 1:1)) + I(lag(NTEEV2_UNU, 1:1)) + I(lag(NTEEV2_NA, 1:1)) + I(lag(bachelors_perc, 0:1)) + I(lag(med_household_income_adj, 0:1)) + I(lag(white_perc, 0:1)) + I(lag(pop_estimate, 0:1)) + I(lag(TOT_REV, 1:1))", 
                 "L3" = "~ TAX_YEAR + lat*lng + I(lag(TOT_ASSET, 1:3)) + I(lag(NTEEV2_ART, 1:3)) + I(lag(NTEEV2_EDU, 1:3)) + I(lag(NTEEV2_ENV, 1:3)) + I(lag(NTEEV2_HEL, 1:3)) + I(lag(NTEEV2_HMS, 1:3)) + I(lag(NTEEV2_HOS, 1:3)) + I(lag(NTEEV2_IFA, 1:3)) + I(lag(NTEEV2_MMB, 1:3)) + I(lag(NTEEV2_PSB, 1:3)) + I(lag(NTEEV2_REL, 1:3)) + I(lag(NTEEV2_UNI, 1:3)) + I(lag(NTEEV2_UNU, 1:3)) + I(lag(NTEEV2_NA, 1:3)) + I(lag(bachelors_perc, 0:3)) + I(lag(med_household_income_adj, 0:3)) + I(lag(white_perc, 0:3)) + I(lag(pop_estimate, 0:3)) + I(lag(TOT_REV, 1:3))", 
                 "L5" = "~ TAX_YEAR + lat*lng + I(lag(TOT_ASSET, 1:5)) + I(lag(NTEEV2_ART, 1:5)) + I(lag(NTEEV2_EDU, 1:5)) + I(lag(NTEEV2_ENV, 1:5)) + I(lag(NTEEV2_HEL, 1:5)) + I(lag(NTEEV2_HMS, 1:5)) + I(lag(NTEEV2_HOS, 1:5)) + I(lag(NTEEV2_IFA, 1:5)) + I(lag(NTEEV2_MMB, 1:5)) + I(lag(NTEEV2_PSB, 1:5)) + I(lag(NTEEV2_REL, 1:5)) + I(lag(NTEEV2_UNI, 1:5)) + I(lag(NTEEV2_UNU, 1:5)) + I(lag(NTEEV2_NA, 1:5)) + I(lag(bachelors_perc, 0:5)) + I(lag(med_household_income_adj, 0:5)) + I(lag(white_perc, 0:5)) + I(lag(pop_estimate, 0:5)) + I(lag(TOT_REV, 1:5))"
)

refine_methods <- c("none", "ps.match", "CBPS.match", "ps.weight", "CBPS.weight")
save_methods <- list("none" = "no_match", "ps.match" = "ps_match", "CBPS.match" = "CBPS_match", "ps.weight" = "ps_weight", "CBPS.weight" = "CBPS_weight")
matching_flags <- list("none" = FALSE, "ps.match" = TRUE, "CBPS.match" = TRUE, "ps.weight" = TRUE, "CBPS.weight" = TRUE)
# refine_methods <- c("none")
# save_methods <- list("none" = "no_match")
# matching_flags <- list("none" = FALSE)
lags <- c(1,3,5) #1, 3, 5

#### No Interference ####
# panel_df <- readRDS("no_int_panelV2.rds")
# 
# folder_path <- "No_Int_V2/"
# 
# startTime <- Sys.time()
# cluster <- makeCluster(3)
# registerDoParallel(cluster)
# 
# foreach(cur_LAG = lags, .packages = c("PanelMatch", "readr", "writexl", "dplyr"), .verbose = TRUE) %dopar% {
# #for(cur_LAG in lags){
#       print(paste("Lag =", cur_LAG))
#       
#       match_list = list()
#       balance_list = list()
#       for (cur_METHOD in refine_methods){
#             print(paste("Method =", cur_METHOD))
#             
#             m.out <- PanelMatch(panel.data = panel_df,
#                                 lag = cur_LAG,
#                                 refinement.method = cur_METHOD,
#                                 qoi = "att",
#                                 size.match = 10,
#                                 match.missing = FALSE,
#                                 covs.formula = as.formula(formulas[[paste0("L", cur_LAG)]]),
#                                 lead = 1:5,
#                                 matching = matching_flags[[cur_METHOD]])
#             
#             match_list[[cur_METHOD]] <- m.out
# 
#             balance_list[[cur_METHOD]] <- get_covariate_balance(m.out,
#                                                                 panel.data = panel_df,
#                                                                 covariates = covs_to_check)
#       }
#       
#       filename <- paste0("matches/", folder_path, "Lag", cur_LAG, ".rds")
#       print(filename)
#       saveRDS(match_list, file = filename)
# 
#       filename <- paste0("balances/", folder_path, "Lag", cur_LAG, ".rds")
#       print(filename)
#       saveRDS(balance_list, file = filename)
#       
#       temp <- lapply(balance_list, summary)
#       temp <- lapply(temp, as.data.frame)
#       temp <- lapply(temp, tibble::rownames_to_column, var = "time")
#       
#       filename <- paste0("balances/", folder_path, "Lag", cur_LAG, ".xlsx")
#       print(filename)
#       write_xlsx(temp, filename)
# }
# stopCluster(cl = cluster)
# print(Sys.time() - startTime) #Time difference of 7.595101 hours (5 clusters - Mar 3)
# # no matching, lags = 1,3,5, ~12 min
# # ps.match, lags = 1,5, ~12min
# # no matching + ps.match, lags = 1,3,5, ~15 min with 3 clusters
# 
# rm(folder_path)
# folder_path <- "No_Int_V3/"
# 
# startTime <- Sys.time()
# cluster <- makeCluster(3)
# registerDoParallel(cluster)
# 
# foreach(cur_LAG = lags, .packages = c("PanelMatch", "readr", "writexl", "dplyr"), .verbose = TRUE) %dopar% {
#       #for(cur_LAG in lags){
#       print(paste("Lag =", cur_LAG))
#       
#       match_list = list()
#       balance_list = list()
#       for (cur_METHOD in refine_methods){
#             print(paste("Method =", cur_METHOD))
#             
#             m.out <- PanelMatch(panel.data = panel_df,
#                                 lag = cur_LAG,
#                                 refinement.method = cur_METHOD,
#                                 qoi = "att",
#                                 size.match = 10,
#                                 match.missing = TRUE,
#                                 covs.formula = as.formula(formulas[[paste0("L", cur_LAG)]]),
#                                 lead = 1:5,
#                                 matching = matching_flags[[cur_METHOD]])
#             
#             match_list[[cur_METHOD]] <- m.out
#             
#             balance_list[[cur_METHOD]] <- get_covariate_balance(m.out,
#                                                                 panel.data = panel_df,
#                                                                 covariates = covs_to_check)
#       }
#       
#       filename <- paste0("matches/", folder_path, "Lag", cur_LAG, ".rds")
#       print(filename)
#       saveRDS(match_list, file = filename)
#       
#       filename <- paste0("balances/", folder_path, "Lag", cur_LAG, ".rds")
#       print(filename)
#       saveRDS(balance_list, file = filename)
#       
#       temp <- lapply(balance_list, summary)
#       temp <- lapply(temp, as.data.frame)
#       temp <- lapply(temp, tibble::rownames_to_column, var = "time")
#       
#       filename <- paste0("balances/", folder_path, "Lag", cur_LAG, ".xlsx")
#       print(filename)
#       write_xlsx(temp, filename)
# }
# stopCluster(cl = cluster)
# print(Sys.time() - startTime)
# 
# rm(panel_df, folder_path)

#### Interference ####
panel_df <- readRDS("int_panelV2.rds")

folder_path <- "Int_V3/"

startTime <- Sys.time()
cluster <- makeCluster(3)
registerDoParallel(cluster)

foreach(cur_LAG = lags, .packages = c("PanelMatch", "readr", "writexl", "dplyr"), .verbose = TRUE) %dopar% {
#for(cur_LAG in lags){
      print(paste("Lag =", cur_LAG))
      
      match_list = list()
      balance_list = list()
      for (cur_METHOD in refine_methods){
            print(paste("Method =", cur_METHOD))
            
            m.out <- PanelMatch(panel.data = panel_df,
                                lag = cur_LAG,
                                refinement.method = cur_METHOD,
                                qoi = "att",
                                size.match = 10,
                                match.missing = FALSE,
                                covs.formula = as.formula(formulas[[paste0("L", cur_LAG)]]),
                                lead = 1:5,
                                matching = matching_flags[[cur_METHOD]])
            
            match_list[[cur_METHOD]] <- m.out
            
            balance_list[[cur_METHOD]] <- get_covariate_balance(m.out,
                                                                panel.data = panel_df,
                                                                covariates = covs_to_check)
      }
      
      filename <- paste0("matches/", folder_path, "Lag", cur_LAG, ".rds")
      print(filename)
      saveRDS(match_list, file = filename)
      
      filename <- paste0("balances/", folder_path, "Lag", cur_LAG, ".rds")
      print(filename)
      saveRDS(balance_list, file = filename)
      
      temp <- lapply(balance_list, summary)
      temp <- lapply(temp, as.data.frame)
      temp <- lapply(temp, tibble::rownames_to_column, var = "time")
      
      filename <- paste0("balances/", folder_path, "Lag", cur_LAG, ".xlsx")
      print(filename)
      write_xlsx(temp, filename)
}
stopCluster(cl = cluster)
print(Sys.time() - startTime) # No cls: 2.5 min, cls: 1.3 min

rm(folder_path)
folder_path <- "Int_V4/"

startTime <- Sys.time()
cluster <- makeCluster(3)
registerDoParallel(cluster)

foreach(cur_LAG = lags, .packages = c("PanelMatch", "readr", "writexl", "dplyr"), .verbose = TRUE) %dopar% {
      #for(cur_LAG in lags){
      print(paste("Lag =", cur_LAG))
      
      match_list = list()
      balance_list = list()
      for (cur_METHOD in refine_methods){
            print(paste("Method =", cur_METHOD))
            
            m.out <- PanelMatch(panel.data = panel_df,
                                lag = cur_LAG,
                                refinement.method = cur_METHOD,
                                qoi = "att",
                                size.match = 10,
                                match.missing = TRUE,
                                covs.formula = as.formula(formulas[[paste0("L", cur_LAG)]]),
                                lead = 1:5,
                                matching = matching_flags[[cur_METHOD]])
            
            match_list[[cur_METHOD]] <- m.out
            
            balance_list[[cur_METHOD]] <- get_covariate_balance(m.out,
                                                                panel.data = panel_df,
                                                                covariates = covs_to_check)
      }
      
      filename <- paste0("matches/", folder_path, "Lag", cur_LAG, ".rds")
      print(filename)
      saveRDS(match_list, file = filename)
      
      filename <- paste0("balances/", folder_path, "Lag", cur_LAG, ".rds")
      print(filename)
      saveRDS(balance_list, file = filename)
      
      temp <- lapply(balance_list, summary)
      temp <- lapply(temp, as.data.frame)
      temp <- lapply(temp, tibble::rownames_to_column, var = "time")
      
      filename <- paste0("balances/", folder_path, "Lag", cur_LAG, ".xlsx")
      print(filename)
      write_xlsx(temp, filename)
}
stopCluster(cl = cluster)
print(Sys.time() - startTime) 
