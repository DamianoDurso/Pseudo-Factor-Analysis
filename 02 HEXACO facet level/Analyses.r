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

#modified Tucker Congruence Coefficient based on Lovik (2020)
tCC <- function (x, y = NULL, digits = 2, use = NULL, structure = FALSE) {
    direct <- extend <- esem <- factanal <- other <- NA
    obnames <- cs(fa, omega, omegaSem, directSl, direct, omegaDirect, 
        principal, iclust, extend, esem, factanal)
    if (is.null(y) && is.list(x)) {
        n <- length(x)
        for (i in 1:n) {
            xi <- x[[i]]
            if (length(class(xi)) > 1) {
                cln <- inherits(xi, obnames, which = TRUE)
                if (any(cln > 1)) {
                  cln <- obnames[which(cln > 0)]
                }
                else {
                  cln <- "other"
                }
            }
            else {
                cln <- "other"
            }
            switch(cln, fa = {
                if (structure) {
                  xi <- xi$Structure
                } else {
                  xi <- xi$loadings
                }
            }, omega = {
                xi <- xi$schmid$sl
                xi <- as.matrix(xi[, 1:(ncol(xi) - 2)])
            }, omegaSem = {
                xi <- xi$omega.efa$cfa.loads
            }, directSl = {
                xi <- xi$direct
            }, direct = {
                xi <- xi$direct
            }, omegaDirect = {
                xi <- xi$loadings
            }, principal = {
                xi <- xi$loadings
            }, iclust = {
                xi <- xi$loadings
            }, extend = {
                xi <- xi$loadings
            }, esem = {
                xi <- xi$loadings
            }, other = {
                if (inherits(xi, "factanal")) {
                  xi <- xi$loadings
                } else {
                  xi <- as.matrix(xi)
                }
            })
            if (i == 1) {
                xg <- xi
            }
            else {
                xg <- cbind(xg, xi)
            }
        }
        x <- xg
        if (is.null(y)) 
            y <- xg
    }
    else {
        if (length(class(x)) > 1) {
            cln <- inherits(x, obnames, which = TRUE)
            if (any(cln > 1)) {
                cln <- obnames[which(cln > 0)]
            }
            else {
                cln <- "other"
            }
        }
        else {
            cln <- "other"
        }
        switch(cln, fa = {
            if (structure) {
                x <- x$Structure
            } else {
                x <- x$loadings
            }
        }, omega = {
            x <- x$schmid$sl
            x <- as.matrix(x[, 1:(ncol(x) - 2)])
        }, omegaSem = {
            x <- x$omega.efa$cfa.loads
        }, directSl = {
            x <- x$direct
        }, direct = {
            x <- x$direct
        }, omegaDirect = {
            x <- x$loadings
        }, principal = {
            x <- x$loadings
        }, iclust = {
            x <- x$loadings
        }, extend = {
            x <- x$loadings
        }, esem = {
            x <- x$loadings
        }, other = {
            if (inherits(x, "factanal")) {
                x <- x$loadings
            } else {
                x <- as.matrix(x)
            }
        })
    }
    if (length(class(y)) > 1) {
        cln <- inherits(y, obnames, which = TRUE)
        if (any(cln > 1)) {
            cln <- obnames[which(cln > 0)]
        }
        else {
            cln <- "other"
        }
    }
    else {
        cln <- "other"
    }
    switch(cln, fa = {
        if (structure) {
            y <- y$Structure
        } else {
            y <- y$loadings
        }
    }, omega = {
        y <- y$schmid$sl
        y <- as.matrix(y[, 1:(ncol(y) - 2)])
    }, omegaSem = {
        y <- y$omega.efa$cfa.loads
    }, directSl = {
        y <- y$direct
    }, direct = {
        y <- y$direct
    }, omegaDirect = {
        y <- y$loadings
    }, principal = {
        y <- y$loadings
    }, esem = {
        y <- y$loadings
    }, extend = {
        y <- y$loadings
    }, iclust = {
        y <- y$loadings
    }, other = {
        if (inherits(y, "factanal")) {
            y <- y$loadings
        } else {
            y <- as.matrix(y)
        }
    })
    if (any(is.na(x) | any(is.na(y)))) {
        warning("Some loadings were missing.")
        if (!is.null(use)) {
            message("Analysis is  done on complete cases")
            if (any(is.na(x))) {
                xc <- x[complete.cases(x), ]
                y <- y[complete.cases(x), ]
                x <- xc
            }
            if (any(is.na(y))) {
                yc <- y[complete.cases(y), ]
                x <- x[complete.cases(y), ]
                y <- yc
            }
        }
        else {
            warning("Check your data or rerun with the  use = complete option")
        }
    }
    nx <- dim(x)[2]
    ny <- dim(y)[2]
    cross <- abs(t(y) %*% x)
    sumsx <- sqrt(1/diag(t(x) %*% x))
    sumsy <- sqrt(1/diag(t(y) %*% y))
    result <- matrix(rep(0, nx * ny), ncol = nx)
    result <- round(sumsy * (cross * rep(sumsx, each = ny)), 
        digits)
    return(t(result))
}


#---------------------------------------- Validation CONGRUENCE  -------------------------------------#
#---------------------------------------- Validation CONGRUENCE  -------------------------------------#
#---------------------------------------- Validation CONGRUENCE  -------------------------------------#

# Create the data frame with the factor analysis data from the validation study (Lee, 2004)

#first we re-order according to the model
# Specify the desired variable order
var_order = c("Sinc", "Fair", "Gree", "Mode", #H
               "Fear", "Anxi", "Depe", "Sent", #E
               "Expr", "SocB", "Soci", "Live", #X
               "Forg", "Gent", "Flex", "Pati", #A
               "Orga", "Dili", "Perf", "Prud", #C
               "AesA", "Inqu", "Crea", "Unco") #O

valid_load = matrix(c( 0.73, -0.03,  0.11,  0.01,  0.26,  0.11,
                       0.72,  0.09,  0.19, -0.07, -0.17, -0.13,
                       0.68,  0.20, -0.09, -0.13,  0.05, -0.07,
                       0.66, -0.24,  0.24, -0.04, -0.02,  0.03,
                      -0.18,  0.70,  0.09,  0.21, -0.08,  0.05,
                      -0.26,  0.67, -0.09, -0.11,  0.25,  0.07,
                       0.27,  0.61, -0.11, -0.05,  0.00, -0.03,
                      -0.18,  0.57, -0.28,  0.10, -0.02,  0.06,
                       0.13, -0.15,  0.70,  0.07, -0.09, -0.10,
                       0.06,  0.24,  0.70,  0.18,  0.07,  0.11,
                       0.25, -0.14,  0.65,  0.07, -0.11,  0.03,
                      -0.05, -0.18,  0.57, -0.03,  0.10,  0.14,
                      -0.09, -0.08, -0.19,  0.72, -0.19,  0.01,
                      -0.05,  0.08,  0.18,  0.70, -0.03,  0.19,
                      -0.03,  0.17, -0.11,  0.68,  0.12,  0.06,
                      -0.05, -0.14,  0.02,  0.58,  0.12,  0.29,
                       0.07, -0.27,  0.05,  0.04,  0.68,  0.01,
                      -0.09,  0.08, -0.08,  0.08,  0.66, -0.05,
                      -0.07,  0.14, -0.03,  0.21,  0.62,  0.07,
                       0.23, -0.11,  0.04,  0.04,  0.57, -0.11,
                       0.21, -0.04,  0.10,  0.09, -0.06,  0.72,
                       0.00,  0.16,  0.11,  0.06, -0.10,  0.59,
                      -0.05,  0.02, -0.01,  0.11, -0.01,  0.56,
                      -0.28, -0.07, -0.07,  0.18,  0.12,  0.54), byrow = TRUE, 24, 6)
rownames(valid_load) = c("Live", "Expr", "Soci", "SocB", 
            "Sent", "Anxi", "Depe", "Fear", 
            "Unco", "AesA", "Crea", 
            "Inqu", "Gree", "Sinc", "Mode", 
            "Fair", "Pati", "Flex", "Gent", "Forg", 
            "Dili", "Perf", "Orga", "Prud")
reordered_matrix = valid_load[var_order, ]
colnames(reordered_matrix) = c('X', 'E', 'O', 'H', 'A', 'C')
reordered_matrix = reordered_matrix[, c('H','E', 'X', 'A', 'C', 'O')] 
reordered_matrix

#---------------------------------------- ATOMIC CONGRUENCE ---------------------------------------#
res = load_and_prep(file ='./02 HEXACO facet level/Results loadings overview!.xlsx',
              sheet =  'Results_target_item_labeled')
#check which solutions can be used
colnames(res)

#create empty matrix
val_atomic_congruence = matrix(NA, 1, 6)

val_atomic_congruence[1, 1:6] = diag(psych::factor.congruence(x = reordered_matrix[1:23,c(1:6)], #empirical 
                                                     y = res[1:23,c(7:12)])) #distilroberta
val_atomic_congruence = rbind(val_atomic_congruence, diag(psych::factor.congruence(x = reordered_matrix[1:23,c(1:6)],
                                                     y = res[1:23,c(13:18)]))) #minilm
val_atomic_congruence = rbind(val_atomic_congruence, diag(psych::factor.congruence(x = reordered_matrix[1:23,c(1:6)],
                                                     y = res[1:23,c(19:24)]))) #mpnet
#t5 we need to do separately since factors E, X collapsed. 
t5 = c(diag(psych::factor.congruence(x = res[1:23,c(1,4:6)],y = reordered_matrix[1:23,c(25,27:29)]))[1],
       NA, NA, 
       diag(psych::factor.congruence(x = res[1:23,c(1,4:6)],y = reordered_matrix[1:23,c(25,27:29)]))[2:4])
val_atomic_congruence = rbind(val_atomic_congruence, t5) 
val_atomic_congruence = rbind(val_atomic_congruence, diag(psych::factor.congruence(x = reordered_matrix[1:23,c(1:6)],
                                                     y = res[1:23,c(31:36)]))) #psych
val_atomic_congruence = rbind(val_atomic_congruence, diag(psych::factor.congruence(x = reordered_matrix[1:23,c(1:6)],
                                                     y = res[1:23,c(37:42)]))) #use_dan
val_atomic_congruence = rbind(val_atomic_congruence, diag(psych::factor.congruence(x = reordered_matrix[1:23,c(1:6)],
                                                     y = res[1:23,c(43:48)]))) #avg_trans
#rename rows and cols
rownames(val_atomic_congruence) = c('distilroberta', 'miniLM', 'mpnet', 't5', 'psych', 'use_dan', 'avg_trans')
colnames(val_atomic_congruence) = c('H_atom', 'E_atom', 'X_atom', 'A_atom', 'C_atom', 'O_atom')
val_atomic_congruence

#---------------------------------------- ATOMIC CONGRUENCE REV -----------------------------------#
res = load_and_prep(file ='./02 HEXACO facet level/Results loadings overview!.xlsx',
              sheet =  'Results_target_item_rev_labeled')
#check which solutions can be used
colnames(res)

#create empty matrix
val_atomic_congruence_rev = matrix(NA, 1, 6)

#distilroberta we need to do separately because factor H,E collapsed
distilRob = c(NA, NA, diag(psych::factor.congruence(x = reordered_matrix[1:23,c(3:6)], #empirical 
                                                    y = res[1:23,c(8:11)])) ) 
val_atomic_congruence_rev[1, 1:6] = distilRob

#miniLM we need to do separately because factor H,E collapsed
miniLM = c(NA, NA, diag(psych::factor.congruence(x = reordered_matrix[1:23,c(3:6)], #empirical 
                                                    y = res[1:23,c(14:17)])) ) 
val_atomic_congruence_rev = rbind(val_atomic_congruence_rev,miniLM)

#miniLM we need to do separately because factor H,E collapsed
mpnet = c(NA, diag(psych::factor.congruence(x = reordered_matrix[1:23,c(2:5)], #empirical 
                                                    y = reordered_matrix[1:23,c(20:23)])), NA) 
val_atomic_congruence_rev = rbind(val_atomic_congruence_rev,mpnet)

t5 = c(NA, NA,diag(psych::factor.congruence(x = reordered_matrix[1:23,c(2:5)], #empirical 
                                                    y = res[1:23,c(20:23)]))) 
val_atomic_congruence_rev = rbind(val_atomic_congruence_rev,t5)

psych = c(NA, NA, diag(psych::factor.congruence(x = reordered_matrix[1:23,c(3:6)], #empirical 
                                                    y = res[1:23,c(26:29)])) ) 
val_atomic_congruence_rev = rbind(val_atomic_congruence_rev,psych)

use_dan = c(NA, NA, diag(psych::factor.congruence(x = reordered_matrix[1:23,c(3:6)], #empirical 
                                                    y = res[1:23,c(38:41)])) ) 
val_atomic_congruence_rev = rbind(val_atomic_congruence_rev,use_dan)

avg_trans = c(NA, NA, diag(psych::factor.congruence(x = reordered_matrix[1:23,c(3:6)], #empirical 
                                                    y = res[1:23,c(44:47)])) ) 
val_atomic_congruence_rev = rbind(val_atomic_congruence_rev,avg_trans)

#rename rows and cols
rownames(val_atomic_congruence_rev) = c('distilroberta', 'miniLM', 'mpnet', 't5', 'psych', 'use_dan', 'avg_trans')
colnames(val_atomic_congruence_rev) = c('H_atom_rev', 'E_atom_rev', 'X_atom_rev', 'A_atom_rev', 'C_atom_rev', 'O_atom_rev')
val_atomic_congruence_rev
#--------------------------------------------------------------------------------------------------#

#---------------------------------------- SENT CONGRUENCE REV -------------------------------------#
res = load_and_prep(file ='./02 HEXACO facet level/Results loadings overview!.xlsx',
              sheet =  'Results_target_sent_labeled')
#check which solutions can be used
colnames(res)

#create empty matrix
val_macro_congruence = matrix(NA, 1, 6)

#distilroberta we need to do separately because factor E,A collapsed
distilRob = c(diag(psych::factor.congruence(x = reordered_matrix[1:23,1], #empirical 
                                                    y = res[1:23,7])), NA, 
                   psych::factor.congruence(x = reordered_matrix[1:23,3], #empirical 
                                                    y = res[1:23,9]), NA,
              diag(psych::factor.congruence(x = res[1:23,5:6], #empirical 
                                                    y = res[1:23,10:11])))
val_macro_congruence[1, 1:6] = distilRob

val_macro_congruence = rbind(val_macro_congruence, diag(psych::factor.congruence(x = reordered_matrix[1:23,c(1:6)],
                                                     y = res[1:23,c(13:18)]))) #minilm
val_macro_congruence = rbind(val_macro_congruence, diag(psych::factor.congruence(x = reordered_matrix[1:23,c(1:6)],
                                                     y = res[1:23,c(19:24)]))) #mpnet
#t5 we need to do separately since factors E, X collapsed. 
t5 = c(diag(psych::factor.congruence(x = reordered_matrix[1:23,c(1,4:6)],y = res[1:23,c(25,27:29)]))[1],
       NA, NA, 
       diag(psych::factor.congruence(x = reordered_matrix[1:23,c(1,4:6)],y = res[1:23,c(25,27:29)]))[2:4])
val_macro_congruence = rbind(val_macro_congruence, t5) 
val_macro_congruence = rbind(val_macro_congruence, diag(psych::factor.congruence(x = reordered_matrix[1:23,c(1:6)],
                                               y = res[1:23,c(31:36)]))) #psych
val_macro_congruence = rbind(val_macro_congruence, diag(psych::factor.congruence(x = reordered_matrix[1:23,c(1:6)],
                                                     y = res[1:23,c(37:42)]))) #use_dan
val_macro_congruence = rbind(val_macro_congruence, diag(psych::factor.congruence(x = reordered_matrix[1:23,c(1:6)],
                                                     y = res[1:23,c(43:48)]))) #use_dan
#rename rows and cols
rownames(val_macro_congruence) = c('distilroberta', 'miniLM', 'mpnet', 't5', 'psych', 'use_dan', 'avg_trans')
colnames(val_macro_congruence) = c('H_macro', 'E_macro', 'X_macro', 'A_macro', 'C_macro', 'O_macro')
val_macro_congruence

#--------------------------------------------------------------------------------------------------#
results_val = cbind(val_atomic_congruence, val_atomic_congruence_rev, val_macro_congruence)
results_val = rbind(results_val, colMeans(results_val, na.rm = TRUE))
results_val = cbind(results_val, rowMeans(results_val, na.rm = TRUE))
results_val
## store congruence results
write.csv(results_val,
          './02 HEXACO facet level/Results_after_ordering/Congruence_target_val.csv')

#---------------------------------------- Promax Rotation ---------------------------------------#
#---------------------------------------- Promax Rotation ---------------------------------------#

#---------------------------------------- ATOMIC CONGRUENCE ---------------------------------------#
res = load_and_prep(file ='./02 HEXACO facet level/Results loadings overview!.xlsx',
              sheet =  'Results_prom_item_labeled')
#check which solutions can be used
colnames(res)

#create empty matrix
atomic_congruence = matrix(NA, 1, 6)
distilRob = psych::factor.congruence(x = reordered_matrix[1:23,c(1:6)], #empirical 
                         y = res[1:23,c(7:12)])
atomic_congruence[1, 1:6] = c(diag(distilRob)[1], NA, diag(distilRob)[3], NA, distilRob[5,4], distilRob[6,5])

miniLM =psych::factor.congruence(x = reordered_matrix[1:23,c(1:6)], #empirical 
                         y = res[1:23,c(13:18)])
atomic_congruence = rbind(atomic_congruence,c(NA,NA,NA,NA, miniLM[5,3], miniLM[6,4]))

mpnet = psych::factor.congruence(x = reordered_matrix[1:23,c(1:6)], #empirical 
                         y = res[1:23,c(19:24)])
atomic_congruence = rbind(atomic_congruence,c(NA, diag(mpnet)[2], NA, mpnet[4,3], mpnet[5,4], mpnet[6,5]))

t5 = psych::factor.congruence(x = reordered_matrix[1:23,c(1:6)], #empirical 
                         y = res[1:23,c(25:30)])
atomic_congruence = rbind(atomic_congruence, c(NA, NA,NA,NA, t5[5,3], t5[6,4]))

psych = psych::factor.congruence(x = reordered_matrix[1:23,c(1:6)], #empirical 
                         y = res[1:23,c(31:36)])
atomic_congruence = rbind(atomic_congruence, c(diag(psych)[1], NA, diag(psych)[3], NA, psych[5,4], psych[6,5]))

use_dan = psych::factor.congruence(x = reordered_matrix[1:23,c(1:6)], #empirical 
                         y = res[1:23,c(37:42)])
atomic_congruence = rbind(atomic_congruence,c(NA,NA,NA,NA,use_dan[5,3], use_dan[6,4]))

avg_trans = psych::factor.congruence(x = reordered_matrix[1:23,c(1:6)], #empirical 
                         y = res[1:23,c(43:48)])
atomic_congruence = rbind(atomic_congruence, c(NA,NA,NA,NA,avg_trans[5,3], avg_trans[6,4]))


rownames(atomic_congruence) = c('distilroberta', 'miniLM', 'mpnet', 't5', 'psych', 'use_dan', 'avg_trans')
colnames(atomic_congruence) = c('H_atom', 'E_atom', 'X_atom', 'A_atom', 'C_atom', 'O_atom')
atomic_congruence

#---------------------------------------- ATOMIC CONGRUENCE REV -----------------------------------#
res = load_and_prep(file ='./02 HEXACO facet level/Results loadings overview!.xlsx',
              sheet =  'Results_prom_item_rev_labeled')
#check which solutions can be used
colnames(res)

#create empty matrix
atomic_congruence_rev = matrix(NA, 1, 6)

distilRob = psych::factor.congruence(x = reordered_matrix[1:23,c(1:6)], #empirical 
                         y = res[1:23,c(7:12)])
atomic_congruence_rev[1, 1:6] = c(NA, diag(distilRob)[2], NA, distilRob[4,3], distilRob[5,4], NA)

miniLM =psych::factor.congruence(x = reordered_matrix[1:23,c(1:6)], #empirical 
                         y = res[1:23,c(13:18)])
atomic_congruence_rev = rbind(atomic_congruence_rev,c(NA,NA,NA,NA,NA,NA))

mpnet = psych::factor.congruence(x = reordered_matrix[1:23,c(1:6)], #empirical 
                         y = res[1:23,c(19:24)])
atomic_congruence_rev = rbind(atomic_congruence_rev,c(NA, NA, mpnet[3,2], mpnet[4,3], mpnet[5,4], NA))

t5 = psych::factor.congruence(x = reordered_matrix[1:23,c(1:6)], #empirical 
                         y = res[1:23,c(25:30)])
atomic_congruence_rev = rbind(atomic_congruence_rev, c(NA, NA,NA,NA, NA, NA))

psych = psych::factor.congruence(x = reordered_matrix[1:23,c(1:6)], #empirical 
                         y = res[1:23,c(31:36)])
atomic_congruence_rev = rbind(atomic_congruence_rev, c(NA,NA,NA,NA,psych[5,3],NA))

use_dan = psych::factor.congruence(x = reordered_matrix[1:23,c(1:6)], #empirical 
                         y = res[1:23,c(37:42)])
atomic_congruence_rev = rbind(atomic_congruence_rev,c(diag(use_dan)[1], NA, diag(use_dan)[3], NA,NA))

avg_trans = psych::factor.congruence(x = reordered_matrix[1:23,c(1:6)], #empirical 
                         y = res[1:23,c(43:48)])
atomic_congruence_rev = rbind(atomic_congruence_rev, c(NA,NA,NA,NA,NA,NA))

rownames(atomic_congruence_rev) = c('distilroberta', 'miniLM', 'mpnet', 't5', 'psych', 'use_dan', 'avg_trans')
colnames(atomic_congruence_rev) = c('H_atom', 'E_atom', 'X_atom', 'A_atom', 'C_atom', 'O_atom')
atomic_congruence_rev

#---------------------------------------- SENT CONGRUENCE -------------------------------------#
res = load_and_prep(file ='./02 HEXACO facet level/Results loadings overview!.xlsx',
              sheet =  'Results_prom_sent_labeled')
#check which solutions can be used
colnames(res)

#create empty matrix
val_macro_congruence = matrix(NA, 1, 6)

distilRob = psych::factor.congruence(x = reordered_matrix[1:23,c(1:6)], #empirical 
                         y = res[1:23,c(7:12)])
val_macro_congruence[1, 1:6] = c(diag(distilRob)[1], NA, distilRob[3,3], NA, distilRob[5,4], distilRob[6,5])

miniLM =psych::factor.congruence(x = reordered_matrix[1:23,c(1:6)], #empirical 
                         y = res[1:23,c(13:18)])
val_macro_congruence = rbind(val_macro_congruence,c(NA,NA,NA,NA,miniLM[5,3],NA))

mpnet = psych::factor.congruence(x = reordered_matrix[1:23,c(1:6)], #empirical 
                         y = res[1:23,c(19:24)])
val_macro_congruence = rbind(val_macro_congruence,c(diag(mpnet)))

t5 = psych::factor.congruence(x = reordered_matrix[1:23,c(1:6)], #empirical 
                         y = res[1:23,c(25:30)])
val_macro_congruence = rbind(val_macro_congruence, c(NA, NA, NA,NA, t5[5,3], t5[6,4]))

psych = psych::factor.congruence(x = reordered_matrix[1:23,c(1:6)], #empirical 
                         y = res[1:23,c(31:36)])
val_macro_congruence = rbind(val_macro_congruence, c(NA,NA,NA,NA,NA,NA))

use_dan = psych::factor.congruence(x = reordered_matrix[1:23,c(1:6)], #empirical 
                         y = res[1:23,c(37:42)])
val_macro_congruence = rbind(val_macro_congruence,c(NA, diag(use_dan)[2], NA, use_dan[4,3], use_dan[5,4], use_dan[6,5]))

avg_trans = psych::factor.congruence(x = reordered_matrix[1:23,c(1:6)], #empirical 
                         y = res[1:23,c(43:48)])
val_macro_congruence = rbind(val_macro_congruence, c(NA,NA,NA,NA,avg_trans[5,3],NA))

#rename rows and cols
rownames(val_macro_congruence) = c('distilroberta', 'miniLM', 'mpnet', 't5', 'psych', 'use_dan', 'avg_trans')
colnames(val_macro_congruence) = c('H_macro', 'E_macro', 'X_macro', 'A_macro', 'C_macro', 'O_macro')
val_macro_congruence


#----------------------------------------------------------------------------------------------#
results_val_prom = cbind(atomic_congruence, atomic_congruence_rev, val_macro_congruence)
results_val_prom = rbind(results_val_prom, colMeans(results_val_prom, na.rm = TRUE))
results_val_prom = cbind(results_val_prom, rowMeans(results_val_prom, na.rm = TRUE))
results_val_prom
## store congruence results
write.csv(results_val_prom,
          './02 HEXACO facet level/Results_after_ordering/Congruence_prom_val.csv')
