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

set.seed(2023) # for reproducibility
dir.create("results", showWarnings = FALSE) # ensure results dir exists

library(rofi)
library(tidymodels)
library(gmodels)
library(dplyr)
library(pROC)
library(ggplot2)

noacsr::source_all_functions()
tidymodels_prefer()

# Settings
n.bootstraps <- 1000
model.name <- "xgb"

## Import data
data <- import_data(silent = TRUE)
dataset <- rofi::merge_data(data)

# Remove DateTime_FirstNormalBasecess due to it causing errors
dataset <- subset(dataset, select = -DateTime_FirstNormalBasecess)

# Process dataset
dataset <-  dataset %>%
  as_tibble() %>%
  mutate(ofi = create_ofi2(.)) %>%
  # Remove missing OFI (have not been screened for OFI) and exclude patient under the age of 15 (differing clinical pathway)
  filter(!is.na(ofi), pt_age_yrs > 14) %>%
  # Clean and add predictors (intub)
  clean_all_predictors() %>%
  # Create trauma cohorts
  mutate(cohort = create_cohorts(.))

# load results and merge into main dataframe.
results <- readRDS("/opt/data/ml-ofi/random_predictions.rds")
dataset <- merge(dataset, results$cat, by = "did", all.x = TRUE)
results <- readRDS("/opt/data/ml-ofi/results/23-11-29-21-39_extract/results.rds")
dataset <- dataset %>%
  left_join(results[[model.name]], by = "did") %>%
  rename(model_prediction = Repeat1) %>%
  as_tibble()

# Pre-split the dataset by cohort using nest
cohort.datasets <- dataset %>%
  group_by(cohort) %>%
  nest()

# Function to resample cohort and calculate AUC
resample.calculate.auc <- function(cohort.data, bootstrap.iter) {
  sample.indices <- sample(nrow(cohort.data), replace = TRUE)
  bootstrap.sample <- cohort.data[sample.indices, ]
  
  roc_auc_vec(bootstrap.sample$ofi, bootstrap.sample$model_prediction, event_level = "second")
}

# Calculate AUC for the model as a whole
binary_labels <- ifelse(dataset$ofi == "Yes", 1, 0)
print(pROC::auc(binary_labels, dataset$repeat_1))
main.results.path <- "results/main_results.rds"

#### ex på hur man kan räkna AUC för blunt multisystem without Traumatic brain injury  

bm.no.tbi <- dataset[dataset$cohort == "blunt multisystem without TBI",]


if(!file.exists(main.results.path)) {
  
  results <- cohort.datasets$data %>%
    # Map over the cohorts
    map(function(cohort.data) {
      # For loop the amount of bootstraps, create a new resample and calculate the auc
      map(1:n.bootstraps, function(bootstrap.idx) {
        resample.calculate.auc(cohort.data, bootstrap_idx)
      })
    })
  names(results) <- cohort.datasets$cohort
  
  saveRDS(results, main.results.path)
} else {
  results <- readRDS(main.results.path)
}

# Unravel data from ANOVA test
unraveled.data <- results %>%
  enframe(name = "cohort", value = "auc") %>%
  unnest(auc) %>%
  mutate(cohort = as.factor(cohort)) %>%
  mutate(auc_value = map_dbl(auc, ~ .x[[1]])) %>%
  select(-auc)

anova.result <- aov(auc_value ~ cohort, data = unraveled.data)
anova.summary <- summary(anova.result)
p.value <- anova.summary[[1]][["Pr(>F)"]][1]

message("p-value between trauma cohorts is: ", p.value, "\n")

cohort.performance.results <- results %>%
  enframe(name = "cohort", value = "auc") %>%
  mutate(AUC = map_chr(auc, function(auc.values){ 
    cohort.ci <- round(gmodels::ci(as.numeric(auc.values), ci = 0.95), digits = 3) 
    
    return(sprintf("%s (%s, %s)", 
                   cohort.ci[["Estimate"]], 
                   cohort.ci[["CI lower"]], 
                   cohort.ci[["CI upper"]]))
    })) %>%
  mutate(N = map_int(cohort, function(cohort.name){
    return(cohort.datasets %>%
      filter(cohort == cohort.name) %>%
      unnest(data) %>%
      nrow())
  })) %>%
  select(-auc) %>%
  rename(Cohort = cohort) %>%
  arrange(desc(N))

# Create ROC curves
cohort.roc.objs <- list()
for(cohort.name in unique(dataset$cohort)){
  cohort.data <- cohort.datasets %>%
    filter(cohort == cohort.name) %>%
    unnest(data) 
  
  probs <- cohort.data$model_prediction
  target <- cohort.data$ofi
  
  cohort.roc.obj <- suppressMessages(roc(as.numeric(target) - 1, probs))
  
  cohort.roc.objs[[cohort.name]] <- cohort.roc.obj
}

cohort.roc <- ggroc(cohort.roc.objs) +
  theme_bw(base_size = 30) +
  theme(
    legend.position = c(.95, .3),
    legend.justification = c("right", "top"),
    legend.box.just = "right",
    legend.box.background = element_rect(colour = "black", size = 1)
  ) +
  theme(
    axis.text.x = element_text(
      angle = 90,
      hjust = 1,
      vjust = 0.5
    ),
    text = element_text(size = 14, family = "Helvetica")
  ) +
  theme(
    axis.ticks.length = unit(.25, "cm"),
    axis.text.y = element_text(color = "black"),
    axis.text.x = element_text(color = "black"),
    axis.ticks = element_line(color = "black", size = 0.8)
  ) +
  theme(
    text = element_text(size = 18),
    legend.title = element_text(size = 16),
    legend.text = element_text(size = 14)
  ) +
  labs(y = "Sensitivity", x = "Specitivity", colour="Cohort") +
  scale_y_continuous(expand = c(0, 0)) +
  scale_x_continuous(expand = c(0, 0), trans = "reverse") +
  annotate("segment",x = 1, xend = 0, y = 0, yend = 1, color="darkgrey", linetype="dashed", lwd=0.75) +
  annotate("text", x=0.41, y=0.485, label= "No discrimination", size = 4.5)

ggsave("figures/roc.pdf", plot = cohort.roc, device = "pdf", height=10, width = 10)

