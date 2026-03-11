library(data.table)
library(readr)
library(dplyr)
library(tidyverse)
library(PanelMatch)
library(writexl)
library(foreach)
library(doParallel)
source("matches_helper.R")
rm(add_exposed, add_SVC, create_adj_list, no_int_no_SVC_plots, tidy_bal_df)

# refine_methods <- c("none", "ps.match", "CBPS.match", "ps.weight", "CBPS.weight")
refine_methods <- c("ps.match")
save_methods <- list("none" = "no_match", "ps.match" = "ps_match", "CBPS.match" = "CBPS_match", "ps.weight" = "ps_weight", "CBPS.weight" = "CBPS_weight")
matching_flags <- list("none" = FALSE, "ps.match" = TRUE, "CBPS.match" = TRUE, "ps.weight" = TRUE, "CBPS.weight" = TRUE)
lags <- c(3,5)

no_interference_model_list <- list(
      list("full_path" = "no_svc/no_int_panelV1/config1/",
           "data" = "panel_data/no_int_panelV1.rds",
           "match_on_NA" = FALSE,
           "svc_flag" = FALSE),
      list("full_path" = "with_svc/no_int_panelV2/config1/",
           "data" = "panel_data/no_int_panelV2.rds",
           "match_on_NA" = FALSE,
           "svc_flag" = TRUE)
)

interference_model_list <- list(
      # list("full_path" = "no_svc/int_panelV1/config1/",
      #      "data" = "panel_data/int_panelV1.rds",
      #      "match_on_NA" = FALSE,
      #      "svc_flag" = FALSE),
      # list("full_path" = "with_svc/int_panelV2/config1/",
      #      "data" = "panel_data/int_panelV2.rds",
      #      "match_on_NA" = FALSE,
      #      "svc_flag" = TRUE)
)

startTime.noInt <- Sys.time()
for (mdl in no_interference_model_list){
      print(mdl$full_path)

      panel_df <- readRDS(mdl$data)
      res <- create_formulas(panel_df, svc_flag = mdl$svc_flag)
      formulas <- res$formulas
      covs_to_check <- res$covs
      
      startTime <- Sys.time()
      cluster <- makeCluster(length(lags), outfile = "cl_log.txt")
      registerDoParallel(cluster)

      foreach(cur_LAG = lags, .packages = c("PanelMatch", "readr", "writexl", "dplyr"), .verbose = TRUE) %dopar% {
      # for(cur_LAG in lags){
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
                                      match.missing = mdl$match_on_NA,
                                      covs.formula = as.formula(formulas[[paste0("L", cur_LAG)]]),
                                      lead = 1:5,
                                      matching = matching_flags[[cur_METHOD]])

                  match_list[[cur_METHOD]] <- m.out

                  balance_list[[cur_METHOD]] <- get_covariate_balance(m.out,
                                                                      panel.data = panel_df,
                                                                      covariates = covs_to_check)
            }

            filename <- paste0("matches/", mdl$full_path, "Lag", cur_LAG, ".rds")
            print(filename)
            saveRDS(match_list, file = filename)

            filename <- paste0("balances/", mdl$full_path, "Lag", cur_LAG, ".rds")
            print(filename)
            saveRDS(balance_list, file = filename)

            temp <- lapply(balance_list, summary)
            temp <- lapply(temp, as.data.frame)
            temp <- lapply(temp, tibble::rownames_to_column, var = "time")

            filename <- paste0("balances/", mdl$full_path, "Lag", cur_LAG, ".xlsx")
            print(filename)
            write_xlsx(temp, filename)

      }
      stopCluster(cl = cluster)
      
      print(paste("Finished", mdl$full_path))
      print(Sys.time() - startTime)
}
print("Finished No Interference runs")
print(Sys.time() - startTime.noInt)

startTime.int <- Sys.time()
for (mdl in interference_model_list){
      print(mdl$full_path)
      
      panel_df <- readRDS(mdl$data)
      res <- create_formulas(panel_df, svc_flag = mdl$svc_flag)
      formulas <- res$formulas
      covs_to_check <- res$covs
      
      startTime <- Sys.time()
      cluster <- makeCluster(length(lags), outfile = "cl_log.txt")
      registerDoParallel(cluster)

      foreach(cur_LAG = lags, .packages = c("PanelMatch", "readr", "writexl", "dplyr"), .verbose = TRUE) %dopar% {
      # for(cur_LAG in lags){
            print(paste("Lag =", cur_LAG))
            
            match_list = list()
            balance_list = list()
            for (cur_METHOD in refine_methods){
                  # print(paste("Method =", cur_METHOD))
                  
                  m.out <- PanelMatch(panel.data = panel_df,
                                      lag = cur_LAG,
                                      refinement.method = cur_METHOD,
                                      qoi = "att",
                                      size.match = 10,
                                      match.missing = mdl$match_on_NA,
                                      covs.formula = as.formula(formulas[[paste0("L", cur_LAG)]]),
                                      lead = 1:5,
                                      matching = matching_flags[[cur_METHOD]])
                  
                  match_list[[cur_METHOD]] <- m.out
                  
                  balance_list[[cur_METHOD]] <- get_covariate_balance(m.out,
                                                                      panel.data = panel_df,
                                                                      covariates = covs_to_check)
            }
            
            filename <- paste0("matches/", mdl$full_path, "Lag", cur_LAG, ".rds")
            print(filename)
            saveRDS(match_list, file = filename)
            
            filename <- paste0("balances/", mdl$full_path, "Lag", cur_LAG, ".rds")
            print(filename)
            saveRDS(balance_list, file = filename)
            
            temp <- lapply(balance_list, summary)
            temp <- lapply(temp, as.data.frame)
            temp <- lapply(temp, tibble::rownames_to_column, var = "time")
            
            filename <- paste0("balances/", mdl$full_path, "Lag", cur_LAG, ".xlsx")
            print(filename)
            write_xlsx(temp, filename)
            
      }
      stopCluster(cl = cluster)
      
      print(paste("Finished", mdl$full_path))
      print(Sys.time() - startTime)
}
print("Finished Interference runs")
print(Sys.time() - startTime.int)
