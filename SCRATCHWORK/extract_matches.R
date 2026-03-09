library(data.table)
library(readr)
library(dplyr)
library(tidyverse)
library(PanelMatch)
library(writexl)
library(foreach)

##### Not Using SVC Yet
refine_methods <- c("none", "ps.match", "CBPS.match", "ps.weight", "CBPS.weight")
# refine_methods <- c("none")
lags <- c(3,5) #1, 3, 5
folder_paths <- c("No_Int_V2/", "No_Int_V3/", "Int_V1/", "Int_V2/")

#### No Interference ####
startTime <- Sys.time()

for (folder_path in folder_paths){
      print(paste("Folder Path:", folder_path))
      
      for(cur_LAG in lags){
            filename <- paste0("matches/", folder_path, "Lag", cur_LAG, ".rds")
            print(filename)
            match_list <- readRDS(filename)
            extracted_list <- list()
            
            for (cur_METHOD in refine_methods){
                  print(paste("Method =", cur_METHOD))
                  m.out <- match_list[[cur_METHOD]]
                  msets <- extract(m.out)
                  extracted_list[[cur_METHOD]] <- msets
            }
            
            filename <- paste0("matches/", folder_path, "extracted_Lag", cur_LAG, ".rds")
            print(filename)
            saveRDS(extracted_list, file = filename)
      }
}

print(Sys.time() - startTime) # going through Lag = 1 took ~7.5min