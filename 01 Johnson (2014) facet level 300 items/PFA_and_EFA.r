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
label_factors_enhanced <- function(loading_matrix, groups, method = "mean", mod = NA, cor_matrix = NA) {
  # Validate method input
  if (!method %in% c("mean", "median")) {
    stop("Invalid method. Use 'mean' or 'median'.")
  }

  if (is.na(mod)){
    stop("Provide a model name")
  }
  
  if (all(is.na(cor_matrix))){
    stop('Provide a Factor correlation matrix')
  }
  
  # Initialize list to store results with labels
  labeled_factors <- list()
  # Copy of the original loading_matrix to modify factor names
  labeled_loading_matrix <- loading_matrix
  labeled_cor_matrix <- cor_matrix

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
        colnames(labeled_cor_matrix)[dominant_factor] <- rownames(labeled_cor_matrix)[dominant_factor] <- paste0(names(groups)[i],mod)
    } else {
        colnames(labeled_loading_matrix)[dominant_factor] <- paste0(colnames(labeled_loading_matrix)[dominant_factor], paste0(names(groups)[i],mod))
        colnames(labeled_cor_matrix)[dominant_factor] <- rownames(labeled_cor_matrix)[dominant_factor] <- paste0(colnames(labeled_cor_matrix)[dominant_factor], paste0(names(groups)[i],mod))
    }


  }
  
  # Return both the labeled factors and the modified loading matrix with updated factor names
  return(list(
    labeled_factors = labeled_factors,
    labeled_loading_matrix = labeled_loading_matrix,
    labeled_cor_matrix = labeled_cor_matrix
  ))
}

# Target matrix
# Function to create a matrix where NA values are replaced with 1 and rest with 0
replace_na_with_1 <- function(mat) {
  result <- ifelse(is.na(mat), 1, 0)
  return(result)
}

#Weight matrix
# Function to create a matrix where NA values are replaced with 0 and rest with 1
replace_0_with_1 <- function(mat) {
  result <- ifelse(is.na(mat), 0, 1)
  return(result)
}
#####################
#------------------------------------------------------------------------------#
#------------------------------------------------------------------------------#

#first we re-order according to the model
# Specify the desired variable order
var_order = c("Imagination", "Artistic.Interests", "Emotionality",  "Adventurousness", "Intellect", "Liberalism", #Openness
              "Self.Efficacy", "Orderliness", "Dutifulness", "Achievement.Striving", "Self.Discipline", "Cautiousness", #Conscientiousness
              "Friendliness", "Gregariousness", "Assertiveness",  "Activity.Level", "Excitement.Seeking", "Cheerfulness", #Extraversion
              "Trust", "Morality", "Altruism", "Cooperation", "Modesty", "Sympathy", #Agreeableness
              "Anxiety", "Anger", "Depression", "Self.Consciousness", "Immoderation", "Vulnerability") #Neuroticism

#create a list which will be necessary for labeling factors later on
label = list(c(var_order[1:6]), c(var_order[7:12]), c(var_order[13:18]), c(var_order[19:24]), c(var_order[25:30]))
names(label) <- c("O", "C", "E", "A", "N")

#Below we do the following, for both the empirical data
# and the embeddings data:
# - Do model selection
# - Run EFA with target (and promax) and CFA 
# - Store the loadings and factor correlation results

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

# Parallel analysis

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
library(GPArotation)
#specify initial target matrix
                      #H#E#X#A#C#O
target.mat = as.matrix(cbind(c(rep(1,6),rep(0,24)),
                             c(rep(0,6),rep(1,6), rep(0,18)),
                             c(rep(0,12),rep(1,6), rep(0,12)),
                             c(rep(0,18),rep(1,6),rep(0,6)),
                             c(rep(0,24),rep(1,6))))

rownames(target.mat) = var_order
target.mat

### EFA w/ target rotation for empirical data
library(esemComp)

emp_esem_target_efa = matrix(NA, nrow = 30, ncol =5)
colnames(emp_esem_target_efa) = names(label)
rownames(emp_esem_target_efa) = var_order
emp_esem_target_Phi = matrix(rnorm(25), ncol = 5, nrow = 5)
colnames(emp_esem_target_Phi) = rownames(emp_esem_target_Phi) = names(label)

# create empty loading and cor matrix for each type of embedding, and store the empirical target-rotated 
# loadings in that matrix. Below we will populate this matrices with the pfa-based target-rotated ones
targ_load_item = targ_load_item_rev = targ_load_sent = emp_esem_target_efa
targ_cor_item = targ_cor_item_rev = targ_cor_sent = emp_esem_target_Phi

colnames(targ_load_item) = colnames(targ_load_item_rev) = colnames(targ_load_sent) = paste0(colnames(targ_load_item), '_emp')
colnames(targ_cor_item) = colnames(targ_cor_item_rev) = colnames(targ_cor_sent) = paste0(colnames(targ_cor_item), '_emp')
#create list for item embed target rotated loadings

models =  c('distilroberta', 'miniLM', 'mpnet',
           't5', 'psych', 'use_dan', 'avg_trans')

#loop over item embeddings 
for (i in 1:length(item_embed)){
target_efa_item <- esem_efa(item_embed[[i]], 
                            nfactors = 5,
                            target = target.mat,
                            targetAlgorithm = "targetQ", # target rot
                            fm = 'ml')
emb_esem_target_efa_item = target_efa_item$loadings #loadings matrix
emb_cor_item = target_efa_item$Phi #factor cor matrix
colnames(emb_esem_target_efa_item) = colnames(emb_cor_item) = paste0(colnames(emb_esem_target_efa_item), '_item_', models[i]) #rename
targ_load_item = cbind(targ_load_item,emb_esem_target_efa_item) #all loadings in one place
targ_cor_item = cbind(targ_cor_item,emb_cor_item) #all cor in one place

target_efa_item_rev <- esem_efa(item_rev_embed[[i]], 
                            nfactors = 5,
                            target = target.mat,
                            targetAlgorithm = "targetQ", # target rot
                            fm = 'ml')
emb_esem_target_efa_item_rev = target_efa_item_rev$loadings #loadings matrix
emb_cor_item_rev = target_efa_item_rev$Phi #factor cor matrix
colnames(emb_esem_target_efa_item_rev) = colnames(emb_cor_item_rev) = paste0(colnames(emb_esem_target_efa_item_rev), '_item_', models[i]) #rename
targ_load_item_rev = cbind(targ_load_item_rev,emb_esem_target_efa_item_rev) #all loadings in one place
targ_cor_item_rev = cbind(targ_cor_item_rev,emb_cor_item_rev) #all cor in one place

target_efa_sent <- esem_efa(sent_embed[[i]], 
                            nfactors = 5,
                            target = target.mat,
                            targetAlgorithm = "targetQ", # target rot
                            fm = 'ml')
emb_esem_target_efa_sent = target_efa_sent$loadings #loadings matrix
emb_cor_sent = target_efa_sent$Phi #factor cor matrix
colnames(emb_esem_target_efa_sent) = colnames(emb_cor_sent) = paste0(colnames(emb_esem_target_efa_sent), '_item_', models[i]) #rename
targ_load_sent = cbind(targ_load_sent,emb_esem_target_efa_sent) #all loadings in one place
targ_cor_sent = cbind(targ_cor_sent,emb_cor_sent) #all cor in one place
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
targ_cor_item_labeled = NA
targ_cor_item_rev_labeled = NA
targ_cor_sent_labeled = NA

for (i in 1:(length(models)+1)){
   x = i*5 #multiple i*6 to select 6 columns at a time
   if (i == 1){
  #loadings
  print(NA)
  #cor
  print(NA)
   } else {
  #loadings
   targ_load_item_labeled <- cbind(targ_load_item_labeled, round(label_factors_enhanced(targ_load_item[,(x-4):x], group = label, mod = paste0('_',models[i-1]), cor_matrix = targ_cor_item[,(x-4):x])$labeled_loading_matrix,3))
   targ_load_item_rev_labeled <- cbind(targ_load_item_rev_labeled, round(label_factors_enhanced(targ_load_item_rev[,(x-4):x], group = label, mod = paste0('_',models[i-1]), cor_matrix = targ_cor_item_rev[,(x-4):x])$labeled_loading_matrix,3))
   targ_load_sent_labeled <- cbind(targ_load_sent_labeled, round(label_factors_enhanced(targ_load_sent[,(x-4):x], group = label, mod = paste0('_',models[i-1]), cor_matrix = targ_cor_sent[,(x-4):x])$labeled_loading_matrix,3))
  #cor
   targ_cor_item_labeled <- cbind(targ_cor_item_labeled, round(label_factors_enhanced(targ_load_item[,(x-4):x], group = label, mod = paste0('_',models[i-1]), cor_matrix = targ_cor_item[,(x-4):x])$labeled_cor_matrix,3))
   targ_cor_item_rev_labeled <- cbind(targ_cor_item_rev_labeled, round(label_factors_enhanced(targ_load_item_rev[,(x-4):x], group = label, mod = paste0('_',models[i-1]), cor_matrix = targ_cor_item_rev[,(x-4):x])$labeled_cor_matrix,3))
   targ_cor_sent_labeled <- cbind(targ_cor_sent_labeled, round(label_factors_enhanced(targ_load_sent[,(x-4):x], group = label, mod = paste0('_',models[i-1]), cor_matrix = targ_cor_sent[,(x-4):x])$labeled_cor_matrix,3))
   }
}

write.csv(round(targ_load_item_labeled,3), file = 'Results_target_item_labeled.csv')
write.csv(round(targ_load_item_rev_labeled,3), file = 'Results_target_item_rev_labeled.csv')
write.csv(round(targ_load_sent_labeled,3), file = 'Results_target_sent_labeled.csv')

write.csv(round(targ_cor_item_labeled,3), file = 'Results_target_item_labeled_cor.csv')
write.csv(round(targ_cor_item_rev_labeled,3), file = 'Results_target_item_rev_labeled_cor.csv')
write.csv(round(targ_cor_sent_labeled,3), file = 'Results_target_sent_labeled_cor.csv')

#------------------------------------------------------------------------------#
#------------------------------------------------------------------------------#
### EFA with promax rotation 


# create empty loading and cor matrix for each type of embedding, and store the empirical target-rotated 
# loadings in that matrix. Below we will populate this matrices with the pfa-based target-rotated ones
prom_load = emp_esem_target_efa
prom_Phi = emp_esem_target_Phi

prom_load_item = prom_load_item_rev = prom_load_sent = prom_load
prom_cor_item =  prom_cor_item_rev =  prom_cor_sent =  prom_Phi

colnames(prom_load_item) = colnames(prom_load_item_rev) = colnames(prom_load_sent) = paste0(colnames(prom_load_item), '_emp')
colnames(prom_cor_item) =  colnames(prom_cor_item_rev) =  colnames(prom_cor_sent) =  paste0(colnames(prom_cor_item), '_emp')


for (i in 1:length(item_embed)){

emb_prom_item <- fa(item_embed[[i]], nfactors = 5, rotate = 'promax') #estimate efa with promax
emb_prom_item_load = emb_prom_item$loadings #loading matrix
emb_prom_item_cor  = emb_prom_item$Phi #cor matrix
colnames(emb_prom_item_load) = colnames(emb_prom_item_cor) = paste0(colnames(emb_prom_item_load), '_item_', models[i]) #rename
prom_load_item = cbind(prom_load_item, emb_prom_item_load) #all load in one place
prom_cor_item = cbind(prom_cor_item, emb_prom_item_cor) #all cor in one place

emb_prom_item_rev <- fa(item_rev_embed[[i]], nfactors = 5, rotate = 'promax') #estimate efa with promax
emb_prom_item_load_rev = emb_prom_item_rev$loadings #loading matrix
emb_prom_item_cor_rev  = emb_prom_item_rev$Phi #cor matrix
colnames(emb_prom_item_load_rev) = colnames(emb_prom_item_cor_rev) = paste0(colnames(emb_prom_item_load_rev), '_item_rev_', models[i]) #rename
prom_load_item_rev = cbind(prom_load_item_rev, emb_prom_item_load_rev) #all load in one place
prom_cor_item_rev = cbind(prom_cor_item_rev, emb_prom_item_cor_rev) #all cor in one place

emb_prom_sent <- fa(sent_embed[[i]], nfactors = 5, rotate = 'promax') #estimate efa with promax
emb_prom_sent_load = emb_prom_sent$loadings #loading matrix
emb_prom_sent_cor  = emb_prom_sent$Phi #cor matrix
colnames(emb_prom_sent_load) = colnames(emb_prom_sent_cor) = paste0(colnames(emb_prom_sent_load), '_sent_', models[i]) #rename
prom_load_sent = cbind(prom_load_sent, emb_prom_sent_load) #all load in one place
prom_cor_sent = cbind(prom_cor_sent, emb_prom_sent_cor) #all cor in one place
}

#store unlabeled files
write.csv(round(prom_load_item,3), file = 'Results_prom_item.csv')
write.csv(round(prom_load_item_rev,3), file = 'Results_prom_item_rev.csv')
write.csv(round(prom_load_sent,3), file = 'Results_prom_sent.csv')

#Now label factor using the absolute maximum average loading approach
prom_load_item_labeled = NA
prom_load_item_rev_labeled = NA
prom_load_sent_labeled = NA
prom_cor_item_labeled = NA
prom_cor_item_rev_labeled = NA
prom_cor_sent_labeled = NA

for (i in 1:(length(models)+1)){
   x = i*5 #multiple i*6 to select 6 columns at a time
   if (i == 1){
    print(NA)
   } else {
   prom_load_item_labeled <- cbind(prom_load_item_labeled, round(label_factors_enhanced(prom_load_item[,(x-4):x], group = label, mod = paste0('_',models[i-1]), cor_matrix = prom_cor_item[,(x-4):x])$labeled_loading_matrix,3))
   prom_load_item_rev_labeled <- cbind(prom_load_item_rev_labeled, round(label_factors_enhanced(prom_load_item_rev[,(x-4):x], group = label, mod = paste0('_',models[i-1]), cor_matrix = prom_cor_item_rev[,(x-4):x])$labeled_loading_matrix,3))
   prom_load_sent_labeled <- cbind(prom_load_sent_labeled, round(label_factors_enhanced(prom_load_sent[,(x-4):x], group = label, mod = paste0('_',models[i-1]), cor_matrix = prom_cor_sent[,(x-4):x])$labeled_loading_matrix,3))
  #cor
   prom_cor_item_labeled <- cbind(prom_cor_item_labeled, round(label_factors_enhanced(prom_load_item[,(x-4):x], group = label, mod = paste0('_',models[i-1]), cor_matrix = prom_cor_item[,(x-4):x])$labeled_cor_matrix,3))
   prom_cor_item_rev_labeled <- cbind(prom_cor_item_rev_labeled, round(label_factors_enhanced(prom_load_item_rev[,(x-4):x], group = label, mod = paste0('_',models[i-1]), cor_matrix = prom_cor_item_rev[,(x-4):x])$labeled_cor_matrix,3))
   prom_cor_sent_labeled <- cbind(prom_cor_sent_labeled, round(label_factors_enhanced(prom_load_sent[,(x-4):x], group = label, mod = paste0('_',models[i-1]), cor_matrix = prom_cor_sent[,(x-4):x])$labeled_cor_matrix,3))
   }
}

#store files
write.csv(round(prom_load_item_labeled,3), file = 'Results_prom_item_labeled.csv')
write.csv(round(prom_load_item_rev_labeled,3), file = 'Results_prom_item_rev_labeled.csv')
write.csv(round(prom_load_sent_labeled,3), file = 'Results_prom_sent_labeled.csv')

write.csv(round(prom_cor_item_labeled,3), file = 'Results_prom_item_labeled_cor.csv')
write.csv(round(prom_cor_item_rev_labeled,3), file = 'Results_prom_item_rev_labeled_cor.csv')
write.csv(round(prom_cor_sent_labeled,3), file = 'Results_prom_sent_labeled_cor.csv')

#------------------------------------------------------------------------------#
#------------------------------------------------------------------------------#
#DO NOT RUN (not yet adjusted)

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

