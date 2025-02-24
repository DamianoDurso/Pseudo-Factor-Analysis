# IPIP-300 facet level factor analysis
# this will take the .por file as given by Johnson and do a 5 factor EF

rm(list=ls())
library(psych)
#install.packages("haven")  # Install haven package if not already installed
library(haven)  # haven for importing .por
library(tidyverse)  #Data Manipulation package

# read files
ipip <- read_por("IPIP300.por")
View(ipip)

# score the composites

ipip$N1_Anx	<-	ipip$I1	+	ipip$I31	+	ipip$I61	+	ipip$I91	+	ipip$I121	+	ipip$I151	+	ipip$I181	+	ipip$I211	+	ipip$I241	+	ipip$I271
ipip$N2_Ang	<-	ipip$I6	+	ipip$I36	+	ipip$I66	+	ipip$I96	+	ipip$I26	+	ipip$I156	+	ipip$I186	+	ipip$I216	+	ipip$I246	+	ipip$I276
ipip$N3_Dep	<-	ipip$I11	+	ipip$I41	+	ipip$I71	+	ipip$I101	+	ipip$I131	+	ipip$I161	+	ipip$I191	+	ipip$I221	+	ipip$I251	+	ipip$I281
ipip$N4_Sfc	<-	ipip$I16	+	ipip$I46	+	ipip$I76	+	ipip$I106	+	ipip$I136	+	ipip$I166	+	ipip$I196	+	ipip$I226	+	ipip$I256	+	ipip$I286
ipip$N5_Imm	<-	ipip$I21	+	ipip$I51	+	ipip$I81	+	ipip$I111	+	ipip$I141	+	ipip$I171	+	ipip$I201	+	ipip$I231	+	ipip$I261	+	ipip$I291
ipip$N6_Vul	<-	ipip$I26	+	ipip$I56	+	ipip$I86	+	ipip$I116	+	ipip$I146	+	ipip$I176	+	ipip$I206	+	ipip$I236	+	ipip$I266	+	ipip$I296

ipip$E1_Fri	<-	ipip$I2	+	ipip$I32	+	ipip$I62	+	ipip$I92	+	ipip$I122	+	ipip$I152	+	ipip$I182	+	ipip$I212	+	ipip$I242	+	ipip$I272
ipip$E2_Gre	<-	ipip$I7	+	ipip$I37	+	ipip$I67	+	ipip$I97	+	ipip$I127	+	ipip$I157	+	ipip$I187	+	ipip$I217	+	ipip$I247	+	ipip$I277
ipip$E3_Ass	<-	ipip$I12	+	ipip$I42	+	ipip$I72	+	ipip$I102	+	ipip$I132	+	ipip$I162	+	ipip$I192	+	ipip$I222	+	ipip$I252	+	ipip$I282
ipip$E4_Act	<-	ipip$I17	+	ipip$I47	+	ipip$I77	+	ipip$I107	+	ipip$I137	+	ipip$I167	+	ipip$I197	+	ipip$I227	+	ipip$I257	+	ipip$I287
ipip$E5_Exc	<-	ipip$I22	+	ipip$I52	+	ipip$I82	+	ipip$I112	+	ipip$I142	+	ipip$I172	+	ipip$I202	+	ipip$I232	+	ipip$I262	+	ipip$I292
ipip$E6_Che	<-	ipip$I27	+	ipip$I57	+	ipip$I87	+	ipip$I117	+	ipip$I147	+	ipip$I177	+	ipip$I207	+	ipip$I237	+	ipip$I267	+	ipip$I297

ipip$O1_Ima	<-	ipip$I3	+	ipip$I33	+	ipip$I63	+	ipip$I93	+	ipip$I23	+	ipip$I53	+	ipip$I83	+	ipip$I213	+	ipip$I243	+	ipip$I273
ipip$O2_Art	<-	ipip$I8	+	ipip$I38	+	ipip$I68	+	ipip$I98	+	ipip$I128	+	ipip$I58	+	ipip$I88	+	ipip$I218	+	ipip$I248	+	ipip$I278
ipip$O3_Emo	<-	ipip$I13	+	ipip$I43	+	ipip$I73	+	ipip$I103	+	ipip$I133	+	ipip$I163	+	ipip$I193	+	ipip$I223	+	ipip$I253	+	ipip$I283
ipip$O4_Adv	<-	ipip$I18	+	ipip$I48	+	ipip$I78	+	ipip$I108	+	ipip$I138	+	ipip$I168	+	ipip$I198	+	ipip$I228	+	ipip$I258	+	ipip$I288
ipip$O5_Int	<-	ipip$I23	+	ipip$I53	+	ipip$I83	+	ipip$I113	+	ipip$I143	+	ipip$I173	+	ipip$I203	+	ipip$I233	+	ipip$I263	+	ipip$I293
ipip$O6_Lib	<-	ipip$I28	+	ipip$I58	+	ipip$I88	+	ipip$I118	+	ipip$I148	+	ipip$I178	+	ipip$I208	+	ipip$I238	+	ipip$I268	+	ipip$I298

ipip$A1_Tru	<-	ipip$I4	+	ipip$I34	+	ipip$I64	+	ipip$I94	+	ipip$I124	+	ipip$I154	+	ipip$I184	+	ipip$I214	+	ipip$I244	+	ipip$I274
ipip$A2_Mor	<-	ipip$I9	+	ipip$I39	+	ipip$I69	+	ipip$I99	+	ipip$I129	+	ipip$I159	+	ipip$I189	+	ipip$I219	+	ipip$I249	+	ipip$I279
ipip$A3_Alt	<-	ipip$I14	+	ipip$I44	+	ipip$I74	+	ipip$I104	+	ipip$I134	+	ipip$I164	+	ipip$I194	+	ipip$I224	+	ipip$I254	+	ipip$I284
ipip$A4_Coo	<-	ipip$I19	+	ipip$I49	+	ipip$I79	+	ipip$I109	+	ipip$I139	+	ipip$I169	+	ipip$I199	+	ipip$I229	+	ipip$I259	+	ipip$I289
ipip$A5_Mod	<-	ipip$I24	+	ipip$I54	+	ipip$I84	+	ipip$I114	+	ipip$I144	+	ipip$I174	+	ipip$I204	+	ipip$I234	+	ipip$I264	+	ipip$I294
ipip$A6_Sym	<-	ipip$I29	+	ipip$I59	+	ipip$I89	+	ipip$I119	+	ipip$I149	+	ipip$I179	+	ipip$I209	+	ipip$I239	+	ipip$I269	+	ipip$I299

ipip$C1_Sfe	<-	ipip$I5	+	ipip$I35	+	ipip$I65	+	ipip$I95	+	ipip$I25	+	ipip$I155	+	ipip$I185	+	ipip$I215	+	ipip$I245	+	ipip$I275
ipip$C2_Ord	<-	ipip$I10	+	ipip$I40	+	ipip$I70	+	ipip$I100	+	ipip$I130	+	ipip$I160	+	ipip$I190	+	ipip$I220	+	ipip$I250	+	ipip$I280
ipip$C3_Dut	<-	ipip$I15	+	ipip$I45	+	ipip$I75	+	ipip$I105	+	ipip$I135	+	ipip$I165	+	ipip$I195	+	ipip$I225	+	ipip$I255	+	ipip$I285
ipip$C4_Ach	<-	ipip$I20	+	ipip$I50	+	ipip$I80	+	ipip$I110	+	ipip$I140	+	ipip$I170	+	ipip$I200	+	ipip$I230	+	ipip$I260	+	ipip$I290
ipip$C5_Sfd	<-	ipip$I25	+	ipip$I55	+	ipip$I85	+	ipip$I115	+	ipip$I145	+	ipip$I175	+	ipip$I205	+	ipip$I235	+	ipip$I265	+	ipip$I295
ipip$C6_Cau	<-	ipip$I30	+	ipip$I60	+	ipip$I90	+	ipip$I120	+	ipip$I150	+	ipip$I180	+	ipip$I210	+	ipip$I240	+	ipip$I270	+	ipip$I300

ipip <-ipip %>% filter(COUNTRY == "USA") #Filter on industry

#subset the composites
composites_ipip <- ipip[, c("N1_Anx", "N2_Ang", "N3_Dep", "N4_Sfc", "N5_Imm", "N6_Vul",
                            "E1_Fri", "E2_Gre", "E3_Ass", "E4_Act", "E5_Exc", "E6_Che",
                            "O1_Ima", "O2_Art", "O3_Emo", "O4_Adv", "O5_Int", "O6_Lib",
                            "A1_Tru", "A2_Mor", "A3_Alt", "A4_Coo", "A5_Mod", "A6_Sym",
                            "C1_Sfe", "C2_Ord", "C3_Dut", "C4_Ach", "C5_Sfd", "C6_Cau")]


#fit a factor model
fa_result <- fa(r = composites_ipip, nfactors = 5, fm = "ml", rotate = "promax")
print(fa_result)



