# Load libraries
library(tidyverse)
library(forcats)
library(survminer)
library(survival)
library(lubridate)
library(reshape2)
library(ggplot2)
library(ggpubr)
library(fastDummies)
library(chron)
library(vtable)

# Set your working directory to wherever you placed your processed FS-PALS dataset.

setwd("/Users/kathrynmurenbeeld/Desktop/NEPA_DELAYS/R/NEPA_Delays")

# Set up to see more rows of output if desired

options(max.print = 10000)

# Load the data for the survival analysis

df_fin <- read.csv("/Users/kathrynmurenbeeld/Desktop/NEPA_DELAYS/Data/df_to_r_surv_an_2009_2021_allpurp_c20230120.csv")

# Need to convert DECISION TYPE to factor
# Need to convert event variables (NEPA_COMP2, PROJ_COMP, PROJ_INIT, and PROJ_AWARDED) from true, false to 1, 0
# All of the duration variables (e.g ASSESSMENT_TIME) need to be numeric

df_fin$DECISION.TYPE <- as.factor(df_fin$DECISION.TYPE)

df_fin$NEPA_COMP2 <- as.integer(as.logical(df_fin$NEPA_COMP2))
df_fin$PROJ_COMP <- as.integer(as.logical(df_fin$PROJ_COMP))
df_fin$PROJ_INIT <- as.integer(as.logical(df_fin$PROJ_INIT))
df_fin$PROJ_AWARDED <- as.integer(as.logical(df_fin$PROJ_AWARDED))

# Calculate the log-rank for the NEPA type KM curves 

# Time to complete NEPA assessment 
surv_diff_assess <- survdiff(Surv(ASSESSMENT_TIME_MNTH, NEPA_COMP2) ~ DECISION.TYPE, data = df_fin)
surv_diff_assess

# Time to complete first activity
surv_diff_init <- survdiff(Surv(NEPA_TO_FIRST_ACT_MAX_MNTH, PROJ_INIT) ~ DECISION.TYPE, data = df_fin)
surv_diff_init

# Time to award first contract
surv_diff_award <- survdiff(Surv(NEPA_TO_CONTRACT_MAX_MNTH, PROJ_AWARDED) ~ DECISION.TYPE, data = df_fin)
surv_diff_award





