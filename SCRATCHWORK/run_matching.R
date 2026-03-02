library(data.table)
library(readr)
library(dplyr)
library(tidyverse)
library(PanelMatch)
library(writexl)
library(foreach)
library(doParallel)

##### Not Using SVC Yet

formulas <- list("L1" = "~ TAX_YEAR + lat*lng + I(lag(NUM_ORGS, 1:1)) + I(lag(TOT_ASSET, 1:1)) + I(lag(bachelors_perc, 0:1)) + I(lag(med_household_income_adj, 0:1)) + I(lag(white_perc, 0:1)) + I(lag(pop_estimate, 0:1)) + I(lag(TOT_REV, 1:1))", 
                 "L3" = "~ TAX_YEAR + lat*lng + I(lag(NUM_ORGS, 1:3)) + I(lag(TOT_ASSET, 1:3)) + I(lag(bachelors_perc, 0:3)) + I(lag(med_household_income_adj, 0:3)) + I(lag(white_perc, 0:3)) + I(lag(pop_estimate, 0:3)) + I(lag(TOT_REV, 1:3))", 
                 "L5" = "~ TAX_YEAR + lat*lng + I(lag(NUM_ORGS, 1:5)) + I(lag(TOT_ASSET, 1:5)) + I(lag(bachelors_perc, 0:5)) + I(lag(med_household_income_adj, 0:5)) + I(lag(white_perc, 0:5)) + I(lag(pop_estimate, 0:5)) + I(lag(TOT_REV, 1:5))"
)

refine_methods <- c("none", "ps.match", "CBPS.match", "ps.weight", "CBPS.weight")
save_methods <- list("none" = "no_match", "ps.match" = "ps_match", "CBPS.match" = "CBPS_match", "ps.weight" = "ps_weight", "CBPS.weight" = "CBPS_weight")
matching_flags <- list("none" = FALSE, "ps.match" = TRUE, "CBPS.match" = TRUE, "ps.weight" = TRUE, "CBPS.weight" = TRUE)
lags <- c(1, 3, 5)
leads <- c(1, 3, 5)
placebo_flag <- FALSE

startTime <- Sys.time()

cluster <- makeCluster(6)
registerDoParallel(cluster)

foreach(cur_LEAD = leads, .packages = c("PanelMatch", "readr", "writexl", "dplyr"), .verbose = TRUE) %dopar% {
      for (cur_METHOD in refine_methods){
            #print(paste("Lead =", cur_LEAD, "Method =", cur_METHOD))
            list_name <- paste0(cur_METHOD, "F", cur_LEAD)
            
            assign(list_name, list())
            for (cur_LAG in lags){
                  #print(paste("Lag =", cur_LAG))
                  match_name <- paste0("L", cur_LAG)
                  
                  temp_list <- list()
                  temp_list[[match_name]] <- PanelMatch(panel.data = panel_df,
                                                        lag = cur_LAG,
                                                        refinement.method = cur_METHOD,
                                                        qoi = "att",
                                                        size.match = 10,
                                                        match.missing = FALSE,
                                                        covs.formula = as.formula(formulas[[paste0("L", cur_LAG)]]),
                                                        lead = cur_LEAD,
                                                        matching = matching_flags[[cur_METHOD]],
                                                        placebo.test = placebo_flag)
                  assign(list_name, c(get(list_name), temp_list))
            }
            filename <- paste0("matches/", save_methods[[cur_METHOD]], "F", cur_LEAD, ".rds")
            print(filename)
            saveRDS(get(list_name), file = filename)
            
            bal_name <- paste0("bal.", cur_METHOD, "F", cur_LEAD)
            assign(bal_name, get_covariate_balance(get(list_name)$L1, get(list_name)$L3, get(list_name)$L5,
                                                   panel.data = panel_df,
                                                   covariates = covs_to_check))
            
            filename <- paste0("balances/", save_methods[[cur_METHOD]], "F", cur_LEAD, ".rds")
            print(filename)
            saveRDS(get(bal_name), file = filename)
            
            temp <- summary(get(bal_name))
            temp <- lapply(temp, as.data.frame)
            temp <- lapply(temp, tibble::rownames_to_column, var = "time")
            
            filename <- paste0("balances/", save_methods[[cur_METHOD]], "F", cur_LEAD, ".xlsx")
            print(filename)
            write_xlsx(temp, filename)
            
      }
}
print(Sys.time() - startTime)
stopCluster(cl = cluster)