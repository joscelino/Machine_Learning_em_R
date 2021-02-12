# LIST OF REQUIRED PACKAGES -----------------------------------------------

required_packages <- c(
  "checkpoint"
)

# install missing packages

new.packages <- required_packages[!(required_packages %in% 
                                      installed.packages()[,"Package"])]

if (length(new.packages)) {
  install.packages(new.packages, dependencies = TRUE)
}

rm(new.packages)

library(checkpoint)
checkpoint(snapshotDate = "2021-01-21")

library(GA)
library(RMySQL)
library(h2o)
library(caTools)
library(caret)
library(class)
library(e1071)
library(RoughSets)
library(OneR)
library(rpart)
library(rpart.plot)
library(randomForest)
library(fuzzyjoin)
library(dplyr)
library(pacman)
library(nortest)
library(tsoutliers)
library(ggplot2)
library(plotly)
library(prophet)
library(tibble)
library(readr)
library(arules)
library(MASS)
library(kernlab)
library(dbscan)
library(cluster)
library(patchwork)
