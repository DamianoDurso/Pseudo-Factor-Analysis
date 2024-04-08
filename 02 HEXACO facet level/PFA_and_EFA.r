library(psych)

#please check location of this R file before running code to make sure results are stored in the right folder

#### Functions######
# apparently something goes wrong in the saving of the csv files (double check in python)
# this makes the matrices non-symmetric (which shouldn't be possible). 
# I am going to force it for now but will double-check in python

#In the function below we conduct the following operations
# 1- copy upper triangle of the matrix to lower triangle
# 2- align rownames and colnames
# 3- reorder matrix by the order specified in var 
symm <- function(m, var) {
    m[upper.tri(m)] <- t(m)[upper.tri(m)] 
    rownames(m) <- colnames(m)
    orig_order <- colnames(m)
    ord_ind <- match(var, orig_order)
    reordered_matrix <- m[ord_ind, ord_ind]
    return(reordered_matrix)
}

# Updated function to label factors, return enhanced information, and modify factor names
label_factors_enhanced <- function(loading_matrix, groups, method = "mean", mod = NA) {
  # Validate method input
  if (!method %in% c("mean", "median")) {
    stop("Invalid method. Use 'mean' or 'median'.")
  }

  if (is.na(mod)){
    stop("Provide a model name")
  }
  
  # Initialize list to store results with labels
  labeled_factors <- list()
  # Copy of the original loading_matrix to modify factor names
  labeled_loading_matrix <- loading_matrix
  
  for(i in seq_along(groups)) {
    group <- groups[[i]]
    group_loadings <- abs(loading_matrix[group, ]) # Subset and take absolute values
    
    # Calculate average or median loading for each factor
    if (method == "mean") {
      dominant_values <- colMeans(group_loadings) # Mean
    } else {
      dominant_values <- apply(group_loadings, 2, median) # Median
    }
    
    # Determine which factor has the highest average/median loading
    dominant_factor <- which.max(dominant_values)
    
    # Store result
    labeled_factors[[length(labeled_factors) + 1]] <- list(
      group_index = i,
      group_items = group,
      dominant_factor = dominant_factor,
      dominant_value = dominant_values[dominant_factor]
    )
    
    # Assign new label to the factor column in the modified loading matrix accounting for collapsing
    if (colnames(labeled_loading_matrix)[dominant_factor] == colnames(loading_matrix)[dominant_factor]){
        colnames(labeled_loading_matrix)[dominant_factor] <- paste0(names(groups)[i],mod)
    } else {
        colnames(labeled_loading_matrix)[dominant_factor] <- paste0(colnames(labeled_loading_matrix)[dominant_factor], paste0(names(groups)[i],mod))
    }

  }
  
  # Return both the labeled factors and the modified loading matrix with updated factor names
  return(list(
    labeled_factors = labeled_factors,
    labeled_loading_matrix = labeled_loading_matrix
  ))
}
#####################
#------------------------------------------------------------------------------#
#------------------------------------------------------------------------------#

#first we re-order according to the model
# Specify the desired variable order
var_order = c("Sinc", "Fair", "Gree", "Mode", #H
               "Fear", "Anxi", "Depe", "Sent", #E
               "Expr", "SocB", "Soci", "Live", #X
               "Forg", "Gent", "Flex", "Pati", #A
               "Orga", "Dili", "Perf", "Prud", #C
               "AesA", "Inqu", "Crea", "Unco") #O

#create a list which will be necessary for labeling factors later on
label = list(c(var_order[1:4]), c(var_order[5:8]), c(var_order[9:12]), c(var_order[13:16]), c(var_order[17:21]),
           c(var_order[21:24]))
names(label) <- c("H", "E", "X", "A", "C", "O")

#Below we do the following, for both the empirical data
# and the embeddings data:
# - Do model selection
# - Run EFA with target (and promax) and CFA 
# - Store the loadings and factor correlation results

# read empirical data
df_emp = read.csv('empirical_data.csv')
df_emp = df_emp[,-c(1)]
df_emp = df_emp[,var_order]

# read embeddings data
models = c('distilroberta', 'miniLM', 'mpnet',
           't5', 'psych', 'use_dan')


# create empty lists which we will populate with the
# different cosine matrices
item_embed = list()
item_rev_embed = list()
sent_embed = list()

#load the matries
for (i in 1:length(models)){
    item_embed[[i]] = symm(as.matrix(read.csv(paste0('matrix_concatenated_item_', models[i], '.csv'))), var_order)
    item_rev_embed[[i]] = symm(as.matrix(read.csv(paste0('matrix_concatenated_item_rev_', models[i], '.csv'))), var_order)
    sent_embed[[i]] = symm(as.matrix(read.csv(paste0('matrix_concatenated_', models[i], '.csv'))), var_order)
}

#add average across transformers
item_embed[[i+1]] = apply(simplify2array(item_embed)[,,c(1:5)], 1:2, mean)
item_rev_embed[[i+1]] = apply(simplify2array(item_rev_embed)[,,c(1:5)], 1:2, mean)
sent_embed[[i+1]] = apply(simplify2array(sent_embed)[,,c(1:5)], 1:2, mean)

#------------------------------------------------------------------------------#
#------------------------------------------------------------------------------#
# Parallel analysis
#run parallel analysis on empirical data
par_emp = fa.parallel(df_emp[,-c(1)], n.iter = 100)
par_emp$nfact

#create empty lists containing the number of factors extracted
par_item = NULL
par_item_rev = NULL
par_sent = NULL

#check how psych interprets the data
psych::isCorrelation(item_embed[[1]]) 

#sample size is determined base on...
for (x in 1:length(item_embed)){
    par_item[x] = fa.parallel(item_embed[[x]], n.iter = 3000)$nfact
    par_item_rev[x] = fa.parallel(item_rev_embed[[x]], n.iter = 3000)$nfact
    par_sent[x] = fa.parallel(sent_embed[[x]], n.iter = 3000)$nfact
}

par_item
par_item_rev
par_sent
#------------------------------------------------------------------------------#
#------------------------------------------------------------------------------#

#specify target matrix
                      #H#E#X#A#C#O
target.mat = as.matrix(cbind(c(rep(1,4),rep(0,20)),
                             c(rep(0,4),rep(1,4), rep(0,16)),
                             c(rep(0,8),rep(1,4), rep(0,12)),
                             c(rep(0,12),rep(1,4),rep(0,8)),
                             c(rep(0,16),rep(1,4),rep(0,4)),
                             c(rep(0,20),rep(1,4))))
rownames(target.mat) = var_order
target.mat

### EFA w/ target rotation for empirical data
library(esemComp)
emp_esem_target_efa <- esem_efa(df_emp, 
                            nfactors = 6,
                            target = target.mat,
                            targetAlgorithm = "targetQ", # target rot
                            fm = 'ml') 

# create empty loading matrix for each type of embedding, and store the empirical target-rotated 
# loadings in that matrix. Below we will populate this matrices with the pfa-based target-rotated ones
targ_load_item = targ_load_item_rev = targ_load_sent = emp_esem_target_efa$loadings

colnames(targ_load_item) = colnames(targ_load_item_rev) = colnames(targ_load_sent) = paste0(colnames(targ_load_item), '_emp')
#create list for item embed target rotated loadings

models =  c('distilroberta', 'miniLM', 'mpnet',
           't5', 'psych', 'use_dan', 'avg_trans')

#loop over item embeddings 
for (i in 1:length(item_embed)){
emb_esem_target_efa_item <- esem_efa(item_embed[[i]], 
                            nfactors = 6,
                            target = target.mat,
                            targetAlgorithm = "targetQ", # target rot
                            fm = 'ml')$loadings 
colnames(emb_esem_target_efa_item) = paste0(colnames(emb_esem_target_efa_item), '_item_', models[i])
targ_load_item = cbind(targ_load_item,emb_esem_target_efa_item)

emb_esem_target_efa_rev <- esem_efa(item_rev_embed[[i]], 
                            nfactors = 6,
                            target = target.mat,
                            targetAlgorithm = "targetQ", # target rot
                            fm = 'ml')$loadings 
colnames(emb_esem_target_efa_rev) = paste0(colnames(emb_esem_target_efa_rev), '_item_', models[i])
targ_load_item_rev = cbind(targ_load_item_rev,emb_esem_target_efa_rev)

emb_esem_target_efa_sent <- esem_efa(sent_embed[[i]], 
                            nfactors = 6,
                            target = target.mat,
                            targetAlgorithm = "targetQ", # target rot
                            fm = 'ml')$loadings 
colnames(emb_esem_target_efa_sent) = paste0(colnames(emb_esem_target_efa_sent), '_sent_', models[i])
targ_load_sent = cbind(targ_load_sent,emb_esem_target_efa_sent)
}

#rm(targ_load_item, targ_load_item_rev, targ_load_sent)
#store files that are not ordered
write.csv(round(targ_load_item,3), file = 'Results_target_item.csv')
write.csv(round(targ_load_item_rev,3), file = 'Results_target_item_rev.csv')
write.csv(round(targ_load_sent,3), file = 'Results_target_sent.csv')

#Now label factor using the absolute maximum average loading approach
targ_load_item_labeled = NA
targ_load_item_rev_labeled = NA
targ_load_sent_labeled = NA

for (i in 1:(length(models)+1)){
   x = i*6 #multiple i*6 to select 6 columns at a time
   if (i == 1){
   targ_load_item_labeled <- round(label_factors_enhanced(targ_load_item[,(x-5):x], group = label, mod = '_emp')$labeled_loading_matrix,3)
   targ_load_item_rev_labeled <- round(label_factors_enhanced(targ_load_item_rev[,(x-5):x], group = label, mod = '_emp')$labeled_loading_matrix,3)
   targ_load_sent_labeled <- round(label_factors_enhanced(targ_load_sent[,(x-5):x], group = label, mod = '_emp')$labeled_loading_matrix,3)   
   } else {
   targ_load_item_labeled <- cbind(targ_load_item_labeled, round(label_factors_enhanced(targ_load_item[,(x-5):x], group = label, mod = paste0('_',models[i-1]))$labeled_loading_matrix,3))
   targ_load_item_rev_labeled <- cbind(targ_load_item_rev_labeled, round(label_factors_enhanced(targ_load_item_rev[,(x-5):x], group = label, mod = paste0('_',models[i-1]))$labeled_loading_matrix,3))
   targ_load_sent_labeled <- cbind(targ_load_sent_labeled, round(label_factors_enhanced(targ_load_sent[,(x-5):x], group = label, mod = paste0('_',models[i-1]))$labeled_loading_matrix,3))
   }
}

write.csv(round(targ_load_item_labeled,3), file = 'Results_target_item_labeled.csv')
write.csv(round(targ_load_item_rev_labeled,3), file = 'Results_target_item_rev_labeled.csv')
write.csv(round(targ_load_sent_labeled,3), file = 'Results_target_sent_labeled.csv')
#------------------------------------------------------------------------------#
#------------------------------------------------------------------------------#
### EFA with promax rotation

prom_load <- fa(df_emp, nfactors = 6, rotate = 'promax')$loadings
# create empty loading matrix for each type of embedding, and store the empirical target-rotated 
# loadings in that matrix. Below we will populate this matrices with the pfa-based target-rotated ones
prom_load_item = prom_load_item_rev = prom_load_sent = prom_load
colnames(prom_load_item) = colnames(prom_load_item_rev) = colnames(prom_load_sent) = paste0(colnames(prom_load), '_emp')

for (i in 1:length(item_embed)){
emb_prom_item <- fa(item_embed[[i]], nfactors = 6, rotate = 'promax')$loadings
colnames(emb_prom_item) = paste0(colnames(emb_prom_item), '_item_', models[i])
prom_load_item = cbind(prom_load_item,emb_prom_item)

emb_prom_item_rev <- fa(item_rev_embed[[i]], nfactors = 6, rotate = 'promax')$loadings
colnames(emb_prom_item_rev) = paste0(colnames(emb_prom_item_rev), '_item_', models[i])
prom_load_item_rev = cbind(prom_load_item_rev,emb_prom_item_rev)

emb_prom_sent <- fa(sent_embed[[i]], nfactors = 6, rotate = 'promax')$loadings
colnames(emb_prom_sent) = paste0(colnames(emb_prom_sent), '_item_', models[i])
prom_load_sent = cbind(prom_load_sent,emb_prom_sent)
}

#store unlabeled files
write.csv(round(prom_load_item,3), file = 'Results_prom_item.csv')
write.csv(round(prom_load_item_rev,3), file = 'Results_prom_item_rev.csv')
write.csv(round(prom_load_sent,3), file = 'Results_prom_sent.csv')

#Now label factor using the absolute maximum average loading approach
prom_load_item_labeled = NA
prom_load_item_rev_labeled = NA
prom_load_sent_labeled = NA

for (i in 1:(length(models)+1)){
   x = i*6 #multiple i*6 to select 6 columns at a time
   if (i == 1){
   prom_load_item_labeled <- round(label_factors_enhanced(prom_load_item[,(x-5):x], group = label, mod = '_emp')$labeled_loading_matrix,3)
   prom_load_item_rev_labeled <- round(label_factors_enhanced(prom_load_item_rev[,(x-5):x], group = label, mod = '_emp')$labeled_loading_matrix,3)
   prom_load_sent_labeled <- round(label_factors_enhanced(prom_load_sent[,(x-5):x], group = label, mod = '_emp')$labeled_loading_matrix,3)   
   } else {
   prom_load_item_labeled <- cbind(prom_load_item_labeled, round(label_factors_enhanced(prom_load_item[,(x-5):x], group = label, mod = paste0('_',models[i-1]))$labeled_loading_matrix,3))
   prom_load_item_rev_labeled <- cbind(prom_load_item_rev_labeled, round(label_factors_enhanced(prom_load_item_rev[,(x-5):x], group = label, mod = paste0('_',models[i-1]))$labeled_loading_matrix,3))
   prom_load_sent_labeled <- cbind(prom_load_sent_labeled, round(label_factors_enhanced(prom_load_sent[,(x-5):x], group = label, mod = paste0('_',models[i-1]))$labeled_loading_matrix,3))
   }
}

#store files
write.csv(round(prom_load_item_labeled,3), file = 'Results_prom_item_labeled.csv')
write.csv(round(prom_load_item_rev_labeled,3), file = 'Results_prom_item_rev_labeled.csv')
write.csv(round(prom_load_sent_labeled,3), file = 'Results_prom_sent_labeled.csv')
#------------------------------------------------------------------------------#
#------------------------------------------------------------------------------#

### CFA
library(lavaan)
library(semTools)
model = ' H =~ Sinc + Fair + Gree + Mode
          E =~ Fear + Anxi + Depe + Sent
          X =~ Expr + SocB + Soci + Live
          A =~ Forg + Gent + Flex + Pati
          C =~ Orga + Dili + Perf + Prud
          O =~ AesA + Inqu + Crea + Unco
'

fit = lavaan::cfa(model = model, 
                        data = df_emp,
                        std.lv = T)

cfa_load_item = cfa_load_item_rev = cfa_load_sent = lavInspect(fit, what = 'std')$lambda
colnames(cfa_load_item) = colnames(cfa_load_item_rev) = colnames(cfa_load_sent) = paste0(colnames(cfa_load_item), '_emp')

for (i in 1:length(item_embed)){
emb_fit_item <- cfa(model = model, sample.cov = item_embed[[i]], sample.nobs = 3000, std.lv = T)
load_item = lavInspect(emb_fit_item, what = 'std')$lambda
colnames(load_item) = paste0(colnames(load_item), '_item_', models[i])
cfa_load_item = cbind(cfa_load_item, load_item)

emb_fit_item_rev <- cfa(model = model, sample.cov = item_rev_embed[[i]], sample.nobs = 3000, std.lv = T)
load_item_rev = lavInspect(emb_fit_item_rev, what = 'std')$lambda
colnames(load_item_rev) = paste0(colnames(load_item_rev), '_item_', models[i])
cfa_load_item_rev = cbind(cfa_load_item_rev, load_item_rev)

emb_fit_sent <- cfa(model = model, sample.cov = sent_embed[[i]], sample.nobs = 3000, std.lv = T)
load_sent = lavInspect(emb_fit_sent, what = 'std')$lambda
colnames(load_sent) = paste0(colnames(load_sent), '_item_', models[i])
cfa_load_sent = cbind(cfa_load_sent, load_sent)
}
#store files
write.csv(round(cfa_load_item,3), file = 'Results_cfa_item.csv')
write.csv(round(cfa_load_item_rev,3), file = 'Results_cfa_item_rev.csv')
write.csv(round(cfa_load_sent,3), file = 'Results_cfa_sent.csv')

