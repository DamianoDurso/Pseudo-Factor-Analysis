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
    res = res[-c(25),]
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


#---------------------------------------- ATOMIC CONGRUENCE ---------------------------------------#
res = load_and_prep(file ='Results loadings overview!.xlsx',
              sheet =  'Results_target_item_labeled')
#check which solutions can be used
colnames(res)

#create empty matrix
atomic_congruence = matrix(NA, 1, 6)

atomic_congruence[1, 1:6] = diag(psych::factor.congruence(x = res[1:23,c(1:6)], #empirical 
                                                     y = res[1:23,c(7:12)])) #distilroberta
atomic_congruence = rbind(atomic_congruence, diag(psych::factor.congruence(x = res[1:23,c(1:6)],
                                                     y = res[1:23,c(13:18)]))) #minilm
atomic_congruence = rbind(atomic_congruence, diag(psych::factor.congruence(x = res[1:23,c(1:6)],
                                                     y = res[1:23,c(19:24)]))) #mpnet
#t5 we need to do separately since factors E, X collapsed. 
t5 = c(diag(psych::factor.congruence(x = res[1:23,c(1,4:6)],y = res[1:23,c(25,27:29)]))[1],
       NA, NA, 
       diag(psych::factor.congruence(x = res[1:23,c(1,4:6)],y = res[1:23,c(25,27:29)]))[2:4])
atomic_congruence = rbind(atomic_congruence, t5) 
atomic_congruence = rbind(atomic_congruence, diag(psych::factor.congruence(x = res[1:23,c(1:6)],
                                                     y = res[1:23,c(31:36)]))) #psych
atomic_congruence = rbind(atomic_congruence, diag(psych::factor.congruence(x = res[1:23,c(1:6)],
                                                     y = res[1:23,c(37:42)]))) #use_dan
atomic_congruence = rbind(atomic_congruence, diag(psych::factor.congruence(x = res[1:23,c(1:6)],
                                                     y = res[1:23,c(43:48)]))) #avg_trans
#rename rows and cols
rownames(atomic_congruence) = c('distilroberta', 'miniLM', 'mpnet', 't5', 'psych', 'use_dan', 'avg_trans')
colnames(atomic_congruence) = c('H_atom', 'E_atom', 'X_atom', 'A_atom', 'C_atom', 'O_atom')
atomic_congruence
#--------------------------------------------------------------------------------------------------#

#---------------------------------------- ATOMIC CONGRUENCE REV -----------------------------------#
res = load_and_prep(file ='Results loadings overview!.xlsx',
              sheet =  'Results_target_item_rev_labeled')
#check which solutions can be used
colnames(res)

#create empty matrix
atomic_congruence_rev = matrix(NA, 1, 6)

#distilroberta we need to do separately because factor H,E collapsed
distilRob = c(NA, NA, diag(psych::factor.congruence(x = res[1:23,c(3:6)], #empirical 
                                                    y = res[1:23,c(8:11)])) ) 
atomic_congruence_rev[1, 1:6] = distilRob

#miniLM we need to do separately because factor H,E collapsed
miniLM = c(NA, NA, diag(psych::factor.congruence(x = res[1:23,c(3:6)], #empirical 
                                                    y = res[1:23,c(14:17)])) ) 
atomic_congruence_rev = rbind(atomic_congruence_rev,miniLM)

#miniLM we need to do separately because factor H,E collapsed
mpnet = c(NA, diag(psych::factor.congruence(x = res[1:23,c(2:5)], #empirical 
                                                    y = res[1:23,c(20:23)])), NA) 
atomic_congruence_rev = rbind(atomic_congruence_rev,mpnet)

t5 = c(NA, NA,diag(psych::factor.congruence(x = res[1:23,c(2:5)], #empirical 
                                                    y = res[1:23,c(20:23)]))) 
atomic_congruence_rev = rbind(atomic_congruence_rev,t5)

psych = c(NA, NA, diag(psych::factor.congruence(x = res[1:23,c(3:6)], #empirical 
                                                    y = res[1:23,c(26:29)])) ) 
atomic_congruence_rev = rbind(atomic_congruence_rev,psych)

use_dan = c(NA, NA, diag(psych::factor.congruence(x = res[1:23,c(3:6)], #empirical 
                                                    y = res[1:23,c(38:41)])) ) 
atomic_congruence_rev = rbind(atomic_congruence_rev,use_dan)

avg_trans = c(NA, NA, diag(psych::factor.congruence(x = res[1:23,c(3:6)], #empirical 
                                                    y = res[1:23,c(44:47)])) ) 
atomic_congruence_rev = rbind(atomic_congruence_rev,avg_trans)

#rename rows and cols
rownames(atomic_congruence_rev) = c('distilroberta', 'miniLM', 'mpnet', 't5', 'psych', 'use_dan', 'avg_trans')
colnames(atomic_congruence_rev) = c('H_atom_rev', 'E_atom_rev', 'X_atom_rev', 'A_atom_rev', 'C_atom_rev', 'O_atom_rev')
atomic_congruence_rev
#--------------------------------------------------------------------------------------------------#

#---------------------------------------- SENT CONGRUENCE REV -------------------------------------#
res = load_and_prep(file ='Results loadings overview!.xlsx',
              sheet =  'Results_target_sent_labeled')
#check which solutions can be used
colnames(res)

#create empty matrix
macro_congruence = matrix(NA, 1, 6)

#distilroberta we need to do separately because factor E,A collapsed
distilRob = c(diag(psych::factor.congruence(x = res[1:23,1], #empirical 
                                                    y = res[1:23,7])), NA, 
                   psych::factor.congruence(x = res[1:23,3], #empirical 
                                                    y = res[1:23,9]), NA,
              diag(psych::factor.congruence(x = res[1:23,5:6], #empirical 
                                                    y = res[1:23,10:11])))
macro_congruence[1, 1:6] = distilRob

macro_congruence = rbind(macro_congruence, diag(psych::factor.congruence(x = res[1:23,c(1:6)],
                                                     y = res[1:23,c(13:18)]))) #minilm
macro_congruence = rbind(macro_congruence, diag(psych::factor.congruence(x = res[1:23,c(1:6)],
                                                     y = res[1:23,c(19:24)]))) #mpnet
#t5 we need to do separately since factors E, X collapsed. 
t5 = c(diag(psych::factor.congruence(x = res[1:23,c(1,4:6)],y = res[1:23,c(25,27:29)]))[1],
       NA, NA, 
       diag(psych::factor.congruence(x = res[1:23,c(1,4:6)],y = res[1:23,c(25,27:29)]))[2:4])
macro_congruence = rbind(macro_congruence, t5) 
macro_congruence = rbind(macro_congruence, diag(psych::factor.congruence(x = res[1:23,c(1:6)],
                                               y = res[1:23,c(31:36)]))) #psych
macro_congruence = rbind(macro_congruence, diag(psych::factor.congruence(x = res[1:23,c(1:6)],
                                                     y = res[1:23,c(37:42)]))) #use_dan
macro_congruence = rbind(macro_congruence, diag(psych::factor.congruence(x = res[1:23,c(1:6)],
                                                     y = res[1:23,c(43:48)]))) #use_dan
#rename rows and cols
rownames(macro_congruence) = c('distilroberta', 'miniLM', 'mpnet', 't5', 'psych', 'use_dan', 'avg_trans')
colnames(macro_congruence) = c('H_macro', 'E_macro', 'X_macro', 'A_macro', 'C_macro', 'O_macro')
macro_congruence
#--------------------------------------------------------------------------------------------------#

## store congruence results
write.csv(cbind(atomic_congruence, atomic_congruence_rev, macro_congruence),
          'Congruence_target.csv')
