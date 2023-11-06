library(rofi)
source("functions/DOA.R")
source("functions/create_ofi2.R")

create.dataset <- function(data.fraction = 1.0, include.doa = TRUE) {
  ## Import data
  datasets <- rofi::import_data()
  
  ## Merge data
  combined.dataset <- rofi::merge_data(datasets)
  
  #
  # CAVE: Temp fix!
  #
  # Dirty temp fix for duplicated columns
  tmp <- ifelse(is.na(combined.dataset$DateTime_FirstNormalBasecess), 
                                                          combined.dataset[duplicated(colnames(combined.dataset))][, 1], 
                                                          combined.dataset$DateTime_FirstNormalBasecess)
  combined.dataset <- combined.dataset[!duplicated(colnames(combined.dataset))]
  combined.dataset$DateTime_FirstNormalBasecess <- unlist(tmp)
  
  # Use a fraction of the dataset for debugging fast
  combined.dataset <- combined.dataset[sample(nrow(combined.dataset), floor(nrow(combined.dataset) * data.fraction)),]
  
  # Create DateTime_Case
  combined.dataset$DateTime_Case <-  as.POSIXct(ifelse(is.na(combined.dataset$DateTime_Of_Trauma), 
                                            combined.dataset$DateTime_ArrivalAtHospital, 
                                            combined.dataset$DateTime_Of_Trauma))
  
  #
  # CAVE: Temp fix!
  #
  ## Create OFI column
  #combined.dataset$ofi <- rofi::create_ofi(combined.dataset)
  combined.dataset$ofi <- create_ofi2(combined.dataset)
  
  # Remove documentation OFI
  combined.dataset$ofi <- ifelse(grepl("Dokumetation|dokumentation|Dokumentation", combined.dataset$Problemomrade_.FMP), "No", combined.dataset$ofi)
  
  combined.dataset <- clean_audit_filters(combined.dataset)
  
  ## Separate and store cases without known outcome
  missing.outcome <- is.na(combined.dataset$ofi)
  combined.dataset <- combined.dataset[!missing.outcome,]
  
  combined.dataset <-
    combined.dataset[combined.dataset$pt_age_yrs > 14,]
  
  ## Fix formating and remove wrong values like 999
  #combined.dataset <- clean_predictor(combined.dataset)
  combined.dataset <- clean_all_predictors(combined.dataset)
  
  if(!include.doa){
    # Remove DOA
    combined.dataset <- DOA(combined.dataset)
  }
  
  combined.dataset <- injury_columns(combined.dataset)
  
  ## Integrate RTS
  combined.dataset <- combine_rts(combined.dataset)
  
  return(combined.dataset)
}