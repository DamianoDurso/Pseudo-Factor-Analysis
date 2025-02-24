
rm(list=ls())
library(psych)
library(GPArotation)
library(esemComp)
library(Hmisc)
library(devtools)
library(esemComp)

# read files
cosine_sim_matrix_df <- read.csv("matrix_t5.csv", header = TRUE, row.names = 1)
cosine_sim_matrix <- as.matrix(cosine_sim_matrix_df)

# factor analysis using  fa function
# ml, oblique rotation, e.g., promax
fa_result <- fa(r = cosine_sim_matrix, nfactors = 5, fm = "ml", rotate = "promax")
# Print  results
print(fa_result, cut = 0)

