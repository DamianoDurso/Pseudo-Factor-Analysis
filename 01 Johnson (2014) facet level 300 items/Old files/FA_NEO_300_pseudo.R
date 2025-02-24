
rm(list=ls())
library(psych)
library(GPArotation)
library(esemComp)
library(Hmisc)
library(devtools)
library(esemComp)

# read files

cosine_use_dan <- read.csv("matrix_concatenated_USE_DAN.csv", header = TRUE, row.names = 1)
cosine_use_dan <- as.matrix(cosine_use_dan)
fa_result_use_dan <- fa(r = cosine_use_dan, nfactors = 5, fm = "ml", rotate = "promax")
print(fa_result_use_dan, cut = 0)

cosine_ave <- read.csv("matrix_concatenated_average.csv", header = TRUE, row.names = 1)
cosine_ave <- as.matrix(cosine_ave)
fa_result_ave <- fa(r = cosine_ave, nfactors = 5, fm = "ml", rotate = "promax")
print(fa_result_ave, cut = 0)

cosine_t5 <- read.csv("matrix_concatenated_t5.csv", header = TRUE, row.names = 1)
cosine_t5 <- as.matrix(cosine_t5)
fa_result_t5 <- fa(r = cosine_t5, nfactors = 5, fm = "ml", rotate = "promax")
print(fa_result_t5, cut = 0)

cosine_mpnet <- read.csv("matrix_concatenated_mpnet.csv", header = TRUE, row.names = 1)
cosine_mpnet <- as.matrix(cosine_mpnet)
fa_result_mpnet <- fa(r = cosine_mpnet, nfactors = 5, fm = "ml", rotate = "promax")
print(fa_result_mpnet, cut = 0)

cosine_roberta <- read.csv("matrix_concatenated_roberta.csv", header = TRUE, row.names = 1)
cosine_roberta <- as.matrix(cosine_mpnet)
fa_result_mpnet <- fa(r = cosine_roberta, nfactors = 5, fm = "ml", rotate = "promax")
print(fa_result_mpnet, cut = 0)

cosine_miniLM <- read.csv("matrix_concatenated_miniLM.csv", header = TRUE, row.names = 1)
cosine_miniLM <- as.matrix(cosine_miniLM)
fa_result_mpnet <- fa(r = cosine_miniLM, nfactors = 5, fm = "ml", rotate = "promax")
print(fa_result_mpnet, cut = 0)

cosine_dis_roberta <- read.csv("matrix_concatenated_distilroberta.csv", header = TRUE, row.names = 1)
cosine_dis_roberta <- as.matrix(cosine_dis_roberta)
fa_result_dis_roberta <- fa(r = cosine_dis_roberta, nfactors = 5, fm = "ml", rotate = "promax")
print(fa_result_dis_roberta, cut = 0)







