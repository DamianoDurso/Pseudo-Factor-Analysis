
#this will take the .por file as given by Johnson and do a 5 factor EF

rm(list=ls())
library(psych)
#install.packages("haven")  # Install haven package if not already installed
library(haven)  # haven for importing .por

# read files
ipip <- read_por("IPIP300.por")
View(ipip)

# score the composites
ipip$A1_Tru <- ipip$I4 + ipip$I34 + ipip$I64 + ipip$I94
ipip$A2_Mor <- ipip$I9 + ipip$I39 + ipip$I69 + ipip$I99
ipip$A3_Alt <- ipip$I14 + ipip$I44 + ipip$I74 + ipip$I104
ipip$A4_Coo <- ipip$I19 + ipip$I49 + ipip$I79 + ipip$I109
ipip$A5_Mod <- ipip$I24 + ipip$I54 + ipip$I84 + ipip$I114
ipip$A6_Sym <- ipip$I29 + ipip$I59 + ipip$I89 + ipip$I119

ipip$C1_Sfe <- ipip$I5 + ipip$I35 + ipip$I65 + ipip$I95
ipip$C2_Ord <- ipip$I10 + ipip$I40 + ipip$I70 + ipip$I100
ipip$C3_Dut <- ipip$I15 + ipip$I45 + ipip$I75 + ipip$I105
ipip$C4_Ach <- ipip$I20 + ipip$I50 + ipip$I80 + ipip$I110
ipip$C5_Sfd <- ipip$I25 + ipip$I55 + ipip$I85 + ipip$I115
ipip$C6_Cau <- ipip$I30 + ipip$I60 + ipip$I90 + ipip$I120

ipip$E1_Fri <- ipip$I2 + ipip$I32 + ipip$I62 + ipip$I92
ipip$E2_Gre <- ipip$I7 + ipip$I37 + ipip$I67 + ipip$I97
ipip$E3_Ass <- ipip$I12 + ipip$I42 + ipip$I72 + ipip$I102
ipip$E4_Act <- ipip$I17 + ipip$I47 + ipip$I77 + ipip$I107
ipip$E5_Exc <- ipip$I22 + ipip$I52 + ipip$I82 + ipip$I112
ipip$E6_Che <- ipip$I27 + ipip$I57 + ipip$I87 + ipip$I117

ipip$N1_Anx <- ipip$I1 + ipip$I31 + ipip$I61 + ipip$I91
ipip$N2_Ang <- ipip$I6 + ipip$I36 + ipip$I66 + ipip$I96
ipip$N3_Dep <- ipip$I11 + ipip$I41 + ipip$I71 + ipip$I101
ipip$N4_Sfc <- ipip$I16 + ipip$I46 + ipip$I76 + ipip$I106
ipip$N5_Imm <- ipip$I21 + ipip$I51 + ipip$I81 + ipip$I111
ipip$N6_Vul <- ipip$I26 + ipip$I56 + ipip$I86 + ipip$I116

ipip$O1_Ima <- ipip$I3 + ipip$I33 + ipip$I63 + ipip$I93
ipip$O2_Art <- ipip$I8 + ipip$I38 + ipip$I68 + ipip$I98
ipip$O3_Emo <- ipip$I13 + ipip$I43 + ipip$I73 + ipip$I103
ipip$O4_Adv <- ipip$I18 + ipip$I48 + ipip$I78 + ipip$I108
ipip$O5_Int <- ipip$I23 + ipip$I53 + ipip$I83 + ipip$I113
ipip$O6_Lib <- ipip$I28 + ipip$I58 + ipip$I88 + ipip$I118

#subset the composites
composites_ipip <- ipip[, c("A1_Tru", "A2_Mor", "A3_Alt", "A4_Coo",
                            "A5_Mod", "A6_Sym", "C1_Sfe", "C2_Ord",
                            "C3_Dut", "C4_Ach", "C5_Sfd", "C6_Cau",
                            "E1_Fri", "E2_Gre", "E3_Ass", "E4_Act",
                            "E5_Exc", "E6_Che", "N1_Anx", "N2_Ang",
                            "N3_Dep", "N4_Sfc", "N5_Imm", "N6_Vul",
                            "O1_Ima", "O2_Art", "O3_Emo", "O4_Adv",
                            "O5_Int", "O6_Lib")]

#fit a factor model
fa_result <- fa(r = composites_ipip, nfactors = 5, fm = "pa", rotate = "promax")
print(fa_result)


#read_in_loadings

loadings<-read.csv(file='loadings.csv', header=TRUE, sep=',') 
describe(loadings)

loadings_only <- loadings[, (ncol(loadings)-9):ncol(loadings)]

corr.test(loadings_only, y = NULL, use = "pairwise",method="pearson",adjust="holm", 
          alpha=.05,ci=TRUE,minlength=5,normal=TRUE)






