### Data Analysis Rotated Results EFA
# 1 - Extract the empirical loading matrix
# 2 - Compare each factor, if recovered, with the corresponding empirical factor
# 3 - If full structure is recovered, compare the full structure? just a test
# 4 - Compare correlation matrices

#### IMPORTANT!!! ALL ANALYSES BELOW HEAVILY DEPENDS ON THE RIGHT ORDER OF THE 'Results loadings overview!.xlsx' file.
#### IF THE ORDER OF THE FILE (E.G., THE FACTORS ORDER) HAS BEEN CHANGED FROM THE SOURCE FILE, YOU NEED TO DOUBLE CHECK 
#### ALL THE ANALYSES ABOVE HAVE TO BE RECHECKED!

#install.packages('readxl')
library(readxl)
library(psych) #for congruence
library(FactoMineR) #for corr matrix similarity

#Function to load and rename results
load_and_prep = function(file, sheet){
    res = as.matrix(read_excel(file, sheet = sheet)) 
    #remove empty row 
    res = res[-c(31),]
    #change rownames
    names = rownames(res) <- res[,1]
    #remove colname unncessary
    res = res[,-c(1)]
    #transform to numeric
    res = apply(res,2, as.numeric)
    #rename rows correctly
    rownames(res) = names
    return(res)
}

#---------------------------------------- Target Rotation ---------------------------------------#
#---------------------------------------- Target Rotation ---------------------------------------#

#---------------------------------------- ATOMIC CONGRUENCE ---------------------------------------#
#---------------------------------------- Validation CONGRUENCE  -------------------------------------#
#---------------------------------------- Validation CONGRUENCE  -------------------------------------#
#---------------------------------------- Validation CONGRUENCE  -------------------------------------#

# Create the data frame with the factor analysis data from the validation study (Lee, 2004)

#first we re-order according to the model
# Specify the desired variable order
var_order = c("Imagination", "Artistic.Interests", "Emotionality",  "Adventurousness", "Intellect", "Liberalism", #Openness
              "Self.Efficacy", "Orderliness", "Dutifulness", "Achievement.Striving", "Self.Discipline", "Cautiousness", #Conscientiousness
              "Friendliness", "Gregariousness", "Assertiveness",  "Activity.Level", "Excitement.Seeking", "Cheerfulness", #Extraversion
              "Trust", "Morality", "Altruism", "Cooperation", "Modesty", "Sympathy", #Agreeableness
              "Anxiety", "Anger", "Depression", "Self.Consciousness", "Immoderation", "Vulnerability") #Neuroticism

#loadings matrix based on Johnson 2014 (table 4, p.86)
                       #E     #C     #N     #A    #O
valid_load = matrix(c(-0.27, -0.03,  0.86,  0.04, -0.03,
                      -0.11,  0.03,  0.74, -0.40, -0.03,
                      -0.38, -0.32,  0.69,  0.10,  0.08,
                      -0.54, -0.30,  0.51,  0.22, -0.12,
                       0.21, -0.34,  0.53, -0.21,  0.08,
                      -0.16, -0.30,  0.82,  0.06, -0.12,
                       0.82,  0.20, -0.15,  0.24, -0.03,
                       0.85,  0.07, -0.06,  0.02, -0.07,
                       0.59,  0.48, -0.13, -0.34,  0.19,
                       0.28,  0.67,  0.01, -0.15,  0.02,
                       0.65, -0.18,  0.00, -0.35,  0.17,
                       0.73,  0.04, -0.17,  0.19,  0.14,
                       0.07, -0.18,  0.15, -0.06,  0.68,
                       0.20,  0.11,  0.11,  0.30,  0.63,
                       0.21,  0.18,  0.56,  0.24,  0.48,
                       0.40,  0.05, -0.31, -0.04,  0.53,
                      -0.10,  0.21, -0.23, -0.07,  0.79,
                      -0.08, -0.31,  0.02,  0.02,  0.55,
                       0.45,  0.01, -0.26,  0.53,  0.02,
                      -0.13,  0.25, -0.06,  0.77, -0.02,
                       0.45,  0.22,  0.03,  0.70,  0.18,
                      -0.05, -0.04, -0.22,  0.81, -0.04,
                      -0.34, -0.20,  0.20,  0.49, -0.21,
                       0.15, -0.03,  0.18,  0.70,  0.34,
                       0.16,  0.68, -0.47,  0.07,  0.23,
                      -0.13,  0.66,  0.08,  0.18, -0.29,
                      -0.08,  0.57, -0.17,  0.59, -0.06,
                       0.14,  0.82, -0.14,  0.07,  0.11,
                       0.07,  0.78, -0.25,  0.13, -0.12,
                      -0.45,  0.49, -0.32,  0.38, -0.07
                      ), byrow = TRUE, 30, 5)

rownames(valid_load) = c("Anxiety", "Anger", "Depression", "Self.Consciousness", "Immoderation", "Vulnerability", #Neuroticism
                        "Friendliness", "Gregariousness", "Assertiveness",  "Activity.Level", "Excitement.Seeking", "Cheerfulness", #Extraversion
                        "Imagination", "Artistic.Interests", "Emotionality",  "Adventurousness", "Intellect", "Liberalism", #Openness                           
                        "Trust", "Morality", "Altruism", "Cooperation", "Modesty", "Sympathy", #Agreeableness
                        "Self.Efficacy", "Orderliness", "Dutifulness", "Achievement.Striving", "Self.Discipline", "Cautiousness") #Conscientiousness

colnames(valid_load) = c("E", "C", "N", "A", "O")
reordered_matrix = valid_load[var_order, ]
reordered_matrix = reordered_matrix[, c("O", "C", "E", "A", "N")] 
reordered_matrix

#---------------------------------------- ATOMIC CONGRUENCE ---------------------------------------#
res = load_and_prep(file ='./01 Johnson (2014) facet level 300 items/Results loadings overview!.xlsx',
              sheet =  'Results_target_item_labeled')
#check which solutions can be used
colnames(res)

#create empty matrix
val_atomic_congruence = matrix(NA, 1, 5)

distilRoberta = psych::factor.congruence(x = reordered_matrix[1:30,c(1:5)], y = res[1:30,c(1:5)])
val_atomic_congruence[1, 1:5] = diag(distilRoberta) #distilroberta

miniLM = psych::factor.congruence(x = reordered_matrix[1:30,c(1:5)], y = res[1:30,c(6:10)])
val_atomic_congruence = rbind(val_atomic_congruence, c(diag(miniLM)[1:2],NA,NA,miniLM[5,4])) #minilm

mpnet = psych::factor.congruence(x = reordered_matrix[1:30,c(1:5)], y = res[1:30,c(11:15)])
val_atomic_congruence = rbind(val_atomic_congruence, diag(mpnet)) 

t5 = psych::factor.congruence(x = reordered_matrix[1:30,c(1:5)], y = res[1:30,c(16:20)])
val_atomic_congruence = rbind(val_atomic_congruence, diag(t5)) 

psych = psych::factor.congruence(x = reordered_matrix[1:30,c(1:5)], y = res[1:30,c(21:25)])
val_atomic_congruence = rbind(val_atomic_congruence, c(NA,diag(psych)[2], NA, psych[4,3], psych[5,4])) 

use_dan = psych::factor.congruence(x = reordered_matrix[1:30,c(1:5)], y = res[1:30,c(26:30)])
val_atomic_congruence = rbind(val_atomic_congruence, c(NA,diag(use_dan)[2], NA, use_dan[4,3], use_dan[5,4])) 

avg_trans = psych::factor.congruence(x = reordered_matrix[1:30,c(1:5)], y = res[1:30,c(31:35)])
val_atomic_congruence = rbind(val_atomic_congruence, diag(avg_trans)) 

#rename rows and cols
rownames(val_atomic_congruence) = c('distilroberta', 'miniLM', 'mpnet', 't5', 'psych', 'use_dan', 'avg_trans')
colnames(val_atomic_congruence) = c('O_atom', 'C_atom', 'E_atom', 'A_atom', 'N_atom')
val_atomic_congruence

#---------------------------------------- ATOMIC CONGRUENCE REV -----------------------------------#
res = load_and_prep(file ='./01 Johnson (2014) facet level 300 items/Results loadings overview!.xlsx',
              sheet =  'Results_target_item_rev_labeled')
#check which solutions can be used
colnames(res)

#create empty matrix
val_atomic_congruence_rev = matrix(NA, 1, 5)

#distilroberta we need to do separately because factor H,E collapsed
distilRob = psych::factor.congruence(x = reordered_matrix[1:30,c(1:5)], #empirical 
                                                    y = res[1:30,c(1:5)])
val_atomic_congruence_rev[1, 1:5] = diag(distilRob)

#miniLM we need to do separately because factor H,E collapsed
miniLM = psych::factor.congruence(x = reordered_matrix[1:30,c(1:5)], #empirical 
                                                    y = res[1:30,c(6:10)])
val_atomic_congruence_rev = rbind(val_atomic_congruence_rev, c(diag(miniLM)[1], NA, diag(miniLM)[3:4], NA))

mpnet = psych::factor.congruence(x = reordered_matrix[1:30,c(1:5)], #empirical 
                                                    y = res[1:30,c(11:15)])
val_atomic_congruence_rev = rbind(val_atomic_congruence_rev, diag(mpnet))

t5 = psych::factor.congruence(x = reordered_matrix[1:30,c(1:5)], #empirical 
                                                    y = res[1:30,c(16:20)])
val_atomic_congruence_rev = rbind(val_atomic_congruence_rev, diag(t5))

psych = psych::factor.congruence(x = reordered_matrix[1:30,c(1:5)], #empirical 
                                                    y = res[1:30,c(21:25)])
val_atomic_congruence_rev = rbind(val_atomic_congruence_rev, c(NA, diag(psych)[2], NA, diag(psych)[3], psych[5,4]))

use_dan = psych::factor.congruence(x = reordered_matrix[1:30,c(1:5)], #empirical 
                                                    y = res[1:30,c(26:30)])
val_atomic_congruence_rev = rbind(val_atomic_congruence_rev, diag(use_dan))

avg_trans = psych::factor.congruence(x = reordered_matrix[1:30,c(1:5)], #empirical 
                                                    y = res[1:30,c(31:35)])
val_atomic_congruence_rev = rbind(val_atomic_congruence_rev, c(NA, diag(avg_trans)[2:3], NA, avg_trans[5,4]))

#rename rows and cols
rownames(val_atomic_congruence_rev) = c('distilroberta', 'miniLM', 'mpnet', 't5', 'psych', 'use_dan', 'avg_trans')
colnames(val_atomic_congruence_rev) = c('O_atom_rev', 'C_atom_rev', 'E_atom_rev', 'A_atom_rev', 'N_atom_rev')
val_atomic_congruence_rev
#--------------------------------------------------------------------------------------------------#

#---------------------------------------- SENT CONGRUENCE REV -------------------------------------#
res = load_and_prep(file ='./01 Johnson (2014) facet level 300 items/Results loadings overview!.xlsx',
              sheet =  'Results_target_sent_labeled')
#check which solutions can be used
colnames(res)

#create empty matrix
val_macro_congruence = matrix(NA, 1, 5)

#distilroberta we need to do separately because factor E,A collapsed
distilRob = psych::factor.congruence(x = reordered_matrix[1:30,1:5], #empirical 
                                                    y = res[1:30,1:5])
val_macro_congruence[1, 1:5] = diag(distilRob)

miniLM = psych::factor.congruence(x = reordered_matrix[1:30,1:5], #empirical 
                                                    y = res[1:30,6:10])
val_macro_congruence = rbind(val_macro_congruence, diag(miniLM))

mpnet = psych::factor.congruence(x = reordered_matrix[1:30,1:5], #empirical 
                                                    y = res[1:30,11:15])
val_macro_congruence = rbind(val_macro_congruence, diag(mpnet))

t5 = psych::factor.congruence(x = reordered_matrix[1:30,1:5], #empirical 
                                                    y = res[1:30,16:20])
val_macro_congruence = rbind(val_macro_congruence, diag(t5))

psych = psych::factor.congruence(x = reordered_matrix[1:30,1:5], #empirical 
                                                    y = res[1:30,21:25])
val_macro_congruence = rbind(val_macro_congruence, c(NA, diag(psych)[2], NA, psych[4,3], psych[5,4]))

use_dan = psych::factor.congruence(x = reordered_matrix[1:30,1:5], #empirical 
                                                    y = res[1:30,26:30])
val_macro_congruence = rbind(val_macro_congruence, c(NA, diag(use_dan)[2:4], NA))

avg_trans = psych::factor.congruence(x = reordered_matrix[1:30,1:5], #empirical 
                                                    y = res[1:30,31:35])
val_macro_congruence = rbind(val_macro_congruence, diag(avg_trans))

#rename rows and cols
rownames(val_macro_congruence) = c('distilroberta', 'miniLM', 'mpnet', 't5', 'psych', 'use_dan', 'avg_trans')
colnames(val_macro_congruence) = c('O_macro', 'C_macro', 'E_macro', 'A_macro', 'N_macro')
val_macro_congruence

#--------------------------------------------------------------------------------------------------#
results_val = cbind(val_atomic_congruence, val_atomic_congruence_rev, val_macro_congruence)
results_val = rbind(results_val, colMeans(results_val, na.rm = TRUE))
results_val = cbind(results_val, rowMeans(results_val, na.rm = TRUE))
results_val
## store congruence results in the appropriate folder. 
write.csv(results_val,
          './01 Johnson (2014) facet level 300 items/Results_after_ordering/Congruence_target_val.csv')

#---------------------------------------- Promax Rotation ---------------------------------------#
#---------------------------------------- Promax Rotation ---------------------------------------#

#---------------------------------------- ATOMIC CONGRUENCE ---------------------------------------#
res = load_and_prep(file ='./01 Johnson (2014) facet level 300 items/Results loadings overview!.xlsx',
              sheet =  'Results_prom_item_labeled')
#check which solutions can be used
colnames(res)

#create empty matrix
atomic_congruence = matrix(NA, 1, 5)
distilRob = psych::factor.congruence(x = reordered_matrix[1:30,c(1:5)], #empirical 
                         y = res[1:30,c(1:5)])
atomic_congruence[1, 1:5] = diag(distilRob)

miniLM =psych::factor.congruence(x = reordered_matrix[1:30,c(1:5)], #empirical 
                         y = res[1:30,c(6:10)])
atomic_congruence = rbind(atomic_congruence,c(NA,NA,NA,NA,NA))

mpnet = psych::factor.congruence(x = reordered_matrix[1:30,c(1:5)], #empirical 
                         y = res[1:30,c(11:15)])
atomic_congruence = rbind(atomic_congruence,diag(mpnet))

t5 = psych::factor.congruence(x = reordered_matrix[1:30,c(1:5)], #empirical 
                         y = res[1:30,c(16:20)])
atomic_congruence = rbind(atomic_congruence, c(diag(t5)[1:2], NA, NA, t5[5,4]))

psych = psych::factor.congruence(x = reordered_matrix[1:30,c(1:5)], #empirical 
                         y = res[1:30,c(21:25)])
atomic_congruence = rbind(atomic_congruence, c(NA, diag(psych)[2], NA, psych[4,3], psych[5,4]))

use_dan = psych::factor.congruence(x = reordered_matrix[1:30,c(1:5)], #empirical 
                         y = res[1:30,c(26:30)])
atomic_congruence = rbind(atomic_congruence,c(NA,diag(use_dan)[2], NA, use_dan[4,3], use_dan[5,4]))

avg_trans = psych::factor.congruence(x = reordered_matrix[1:30,c(1:5)], #empirical 
                         y = res[1:30,c(31:35)])
atomic_congruence = rbind(atomic_congruence, diag(avg_trans))

rownames(atomic_congruence) = c('distilroberta', 'miniLM', 'mpnet', 't5', 'psych', 'use_dan', 'avg_trans')
colnames(atomic_congruence) = c('O_atom', 'C_atom', 'E_atom', 'A_atom', 'N_atom')
atomic_congruence

#---------------------------------------- ATOMIC CONGRUENCE REV -----------------------------------#
res = load_and_prep(file ='./01 Johnson (2014) facet level 300 items/Results loadings overview!.xlsx',
              sheet =  'Results_prom_item_rev_labeled')
#check which solutions can be used
colnames(res)

#create empty matrix
atomic_congruence_rev = matrix(NA, 1, 5)

distilRob = psych::factor.congruence(x = reordered_matrix[1:30,c(1:5)], #empirical 
                         y = res[1:30,c(1:5)])
atomic_congruence_rev[1, 1:5] = c(NA,NA,NA,NA,NA)

miniLM =psych::factor.congruence(x = reordered_matrix[1:30,c(1:5)], #empirical 
                         y = res[1:30,c(6:10)])
atomic_congruence_rev = rbind(atomic_congruence_rev,c(NA,NA,NA,NA,NA))

mpnet = psych::factor.congruence(x = reordered_matrix[1:30,c(1:5)], #empirical 
                         y = res[1:30,c(11:15)])
atomic_congruence_rev = rbind(atomic_congruence_rev,c(diag(mpnet)[1], NA, NA, NA, mpnet[5,3]))

t5 = psych::factor.congruence(x = reordered_matrix[1:30,c(1:5)], #empirical 
                         y = res[1:30,c(16:20)])
atomic_congruence_rev = rbind(atomic_congruence_rev, c(NA, NA,NA, NA, t5[5,3]))

psych = psych::factor.congruence(x = reordered_matrix[1:30,c(1:5)], #empirical 
                         y = res[1:30,c(21:25)])
atomic_congruence_rev = rbind(atomic_congruence_rev, c(NA,NA,NA,NA,NA))

use_dan = psych::factor.congruence(x = reordered_matrix[1:30,c(1:5)], #empirical 
                         y = res[1:30,c(26:30)])
atomic_congruence_rev = rbind(atomic_congruence_rev,c(diag(use_dan)[1], NA, NA, use_dan[4,3],NA))

avg_trans = psych::factor.congruence(x = reordered_matrix[1:30,c(1:5)], #empirical 
                         y = res[1:30,c(31:35)])
atomic_congruence_rev = rbind(atomic_congruence_rev, c(NA,NA,NA,NA,avg_trans[5,3]))

rownames(atomic_congruence_rev) = c('distilroberta', 'miniLM', 'mpnet', 't5', 'psych', 'use_dan', 'avg_trans')
colnames(atomic_congruence_rev) = c('O_atom_rev', 'C_atom_rev', 'E_atom_rev', 'A_atom_rev', 'N_atom_rev')
atomic_congruence_rev

#---------------------------------------- SENT CONGRUENCE -------------------------------------#
res = load_and_prep(file ='./01 Johnson (2014) facet level 300 items/Results loadings overview!.xlsx',
              sheet =  'Results_prom_sent_labeled')
#check which solutions can be used
colnames(res)

#create empty matrix
val_macro_congruence = matrix(NA, 1, 5)

distilRob = psych::factor.congruence(x = reordered_matrix[1:30,c(1:5)], #empirical 
                         y = res[1:30,c(1:5)])
val_macro_congruence[1, 1:5] = diag(distilRob)

miniLM =psych::factor.congruence(x = reordered_matrix[1:30,c(1:5)], #empirical 
                         y = res[1:30,c(6:10)])
val_macro_congruence = rbind(val_macro_congruence,c(diag(miniLM)[1], NA, NA, miniLM[4,3], NA))

mpnet = psych::factor.congruence(x = reordered_matrix[1:30,c(1:5)], #empirical 
                         y = res[1:30,c(11:15)])
val_macro_congruence = rbind(val_macro_congruence,c(NA,NA,NA,NA,mpnet[5,3]))

t5 = psych::factor.congruence(x = reordered_matrix[1:30,c(1:5)], #empirical 
                         y = res[1:30,c(16:20)])
val_macro_congruence = rbind(val_macro_congruence, c(NA, diag(t5)[2], t5[4,3], t5[5,4], NA))

psych = psych::factor.congruence(x = reordered_matrix[1:30,c(1:5)], #empirical 
                         y = res[1:30,c(21:25)])
val_macro_congruence = rbind(val_macro_congruence, c(NA,NA,NA,NA,NA))

use_dan = psych::factor.congruence(x = reordered_matrix[1:30,c(1:5)], #empirical 
                         y = res[1:30,c(26:30)])
val_macro_congruence = rbind(val_macro_congruence,c(NA, diag(use_dan)[2:4],NA))

avg_trans = psych::factor.congruence(x = reordered_matrix[1:30,c(1:5)], #empirical 
                         y = res[1:30,c(31:35)])
val_macro_congruence = rbind(val_macro_congruence, c(NA,diag(avg_trans)[2], NA, avg_trans[4,3], avg_trans[5,4]))

#rename rows and cols
rownames(val_macro_congruence) = c('distilroberta', 'miniLM', 'mpnet', 't5', 'psych', 'use_dan', 'avg_trans')
colnames(val_macro_congruence) = c('O_macro', 'C_macro', 'E_macro', 'A_macro', 'N_macro')
val_macro_congruence


#----------------------------------------------------------------------------------------------#
results_val_prom = cbind(atomic_congruence, atomic_congruence_rev, val_macro_congruence)
results_val_prom = rbind(results_val_prom, colMeans(results_val_prom, na.rm = TRUE))
results_val_prom = cbind(results_val_prom, rowMeans(results_val_prom, na.rm = TRUE))
results_val_prom
## store congruence results
write.csv(results_val_prom,
          './01 Johnson (2014) facet level 300 items/Results_after_ordering/Congruence_prom_val.csv')
