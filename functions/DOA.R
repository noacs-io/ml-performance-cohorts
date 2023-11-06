
DOA <- function(dataset) {
  dataset[is.na(dataset$Fr1.12) | dataset$Fr1.12 == 2 ,"DOA"] <- "No"
  dataset[dataset$Fr1.12 == 1 & is.na(dataset$DOA) == TRUE,"DOA"] <- "Yes"
  dataset2 <- dataset[dataset$DOA == "No",] 
  return(dataset2)
}



