## Welcome!

## This is your project's main script file and together with
## manuscript.Rmd it provides and entry point for you and other people
## coming to the project. The code in this file should give an outline
## of the different steps conducted in your study, from importing data
## to producing results.

## This file should be relatively short, and most of the heavy
## lifting should be done by specialised functions. These functions
## live in the folder functions/ and you create a new function using
## create_function().

## Feel free to remove this introductory text as you get started.

## Source all functions (if you tick the box "Source on save" in
## RStudio functions will be automatically sourced when you save
## them). They all need to be sourced however when you compile your
## manuscript file or run this file as a job, as that happens in a
## clean R session.
library(rofi)
noacsr::source_all_functions()

## Import data
data <- import_data(test = TRUE) # Need to add test = TRUE when you run

combined.datasets <- rofi::merge_data(data,test = TRUE)
combined.datasets$ofi <- rofi::create_ofi(combined.datasets)

# I suggest switching to create.dataset()  function when moving to real data, 
# it adds 2022 data but it doesent work for the test data?
# It does everything from import_data to clean_all_predictors


##############################################
# JA: Added functions from previous projects #
##############################################

#If you run create.data() you can skip the steps untill i mark it
combined.datasets <- clean_audit_filters(combined.datasets)

## Separate and store cases without known outcome
missing.outcome <- is.na(combined.datasets$ofi)
combined.datasets <- combined.datasets[!missing.outcome,]

# We exkluder patients under 15 since they go through a different clinical and review pathway.
combined.datasets <- combined.datasets[combined.datasets$pt_age_yrs > 14,]

## Fix formating and remove wrong values like 999
combined.datasets <- clean_all_predictors(combined.datasets)

# Skip untill the next step

dataset <- combined.datasets

# Adds specific trauma quality cohorts
dataset <- create_cohorts(dataset)

# Adds "other cohort" instead of NA
dataset$cohort[is.na(dataset$cohort) == TRUE] <- "Other cohort"

# You can visualize the cohorts through: table(dataset$cohort)
########################
# End of edits from JA #
########################
# install.packages("pROC") # Install the first time

  library(pROC)
# load results and merge into main dataframe.
results <- readRDS("/opt/data/ml-ofi/random_predictions.rds")
dataset <- merge(dataset, results$cat, by = "did", all.x = TRUE)

# Calculate AUC for the model as a whole
binary_labels <- ifelse(dataset$ofi == "Yes", 1, 0)
print(pROC::auc(binary_labels, dataset$repeat_1))

#### ex på hur man kan räkna AUC för blunt multisystem without Traumatic brain injury  

bm.no.tbi <- dataset[dataset$cohort == "blunt multisystem without TBI",]

bm.no.tbi.auc <- pROC::auc(ifelse(bm.no.tbi$ofi == "Yes", 1, 0), bm.no.tbi$repeat_1)
print(bm.no.tbi.auc)
