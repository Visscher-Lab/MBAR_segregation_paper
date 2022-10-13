#segregation statistics 8/2/2022
library(ppcor)
#load data
library(readxl)
datasetSegpaper <- read_excel("~/Desktop/Seg_paper_dataset8.16.22forR.xlsx")

#creating overall value
datasetSegpaper$Avg_factor <- (datasetSegpaper$EF+ datasetSegpaper$MEM+ datasetSegpaper$PS+ datasetSegpaper$LANG+ datasetSegpaper$WM)/5

#average cognition correlations
outlierout <- datasetSegpaper[-c(92), ]
cor.test(outlierout$Avg_factor, outlierout$MBAR_ass_segsame)
cor.test(outlierout$Avg_factor, outlierout$MBAR_Ass_mod)
cor.test(outlierout$Avg_factor, outlierout$MBAR_Ass_pcoeff)
cor.test(outlierout$Avg_factor, outlierout$MBAR_ass_mean)
cor.test(outlierout$MBAR_ass_segsame, outlierout$MBAR_Ass_mod)
#fdr corrections for correlations
pvals<-c(.004, .178, .048, .005)
p.adjust(pvals, method = p.adjust.methods, n = length(pvals))
#site and cortical thickness covariates
pcor.test(outlierout$Avg_factor, outlierout$MBAR_ass_segsame, outlierout$sites)
pcor.test(outlierout$Avg_factor, outlierout$MBAR_ass_segsame, outlierout$CT_associationsysMBAR)
pcor.test(outlierout$Avg_factor, outlierout$MBAR_Ass_mod, outlierout$sites)
pcor.test(outlierout$Avg_factor, outlierout$MBAR_Ass_mod, outlierout$CT_associationsysMBAR)

#network correlations with overall cognition
cor.test(datasetSegpaper$Avg_factor, datasetSegpaper$MBAR_DMN_sameseg)
pcor.test(datasetSegpaper$Avg_factor, datasetSegpaper$MBAR_DMN_sameseg, datasetSegpaper$CT_DMNMBAR)
pcor.test(datasetSegpaper$Avg_factor, datasetSegpaper$MBAR_DMN_sameseg, datasetSegpaper$sites)

cor.test(datasetSegpaper$Avg_factor, datasetSegpaper$MBAR_FPN_sameseg)
pcor.test(datasetSegpaper$Avg_factor, datasetSegpaper$MBAR_FPN_sameseg, datasetSegpaper$sites)
pcor.test(datasetSegpaper$Avg_factor, datasetSegpaper$MBAR_FPN_sameseg, datasetSegpaper$CT_FPNMBAR)

cor.test(datasetSegpaper$Avg_factor, datasetSegpaper$MBAR_CON_sameseg)
pcor.test(datasetSegpaper$Avg_factor, datasetSegpaper$MBAR_CON_sameseg, datasetSegpaper$sites)
pcor.test(datasetSegpaper$Avg_factor, datasetSegpaper$MBAR_CON_sameseg, datasetSegpaper$CT_CONMBAR)
#fdr corrections for correlations
pvals<-c(.001, .315, .015)
p.adjust(pvals, method = p.adjust.methods, n = length(pvals))

#network correlations with EF
cor.test(datasetSegpaper$EF, datasetSegpaper$MBAR_DMN_sameseg)
pcor.test(datasetSegpaper$EF, datasetSegpaper$MBAR_DMN_sameseg, datasetSegpaper$CT_DMNMBAR)
pcor.test(datasetSegpaper$EF, datasetSegpaper$MBAR_DMN_sameseg, datasetSegpaper$sites)

cor.test(datasetSegpaper$EF, datasetSegpaper$MBAR_FPN_sameseg)
pcor.test(datasetSegpaper$EF, datasetSegpaper$MBAR_FPN_sameseg, datasetSegpaper$sites)
pcor.test(datasetSegpaper$EF, datasetSegpaper$MBAR_FPN_sameseg, datasetSegpaper$CT_FPNMBAR)

cor.test(datasetSegpaper$EF, datasetSegpaper$MBAR_CON_sameseg)
pcor.test(datasetSegpaper$EF, datasetSegpaper$MBAR_CON_sameseg, datasetSegpaper$sites)
pcor.test(datasetSegpaper$EF, datasetSegpaper$MBAR_CON_sameseg, datasetSegpaper$CT_CONMBAR)
#fdr corrections for correlations
pvals<-c(.044, .337, .092)
p.adjust(pvals, method = p.adjust.methods, n = length(pvals))

#network correlations with Memory
cor.test(datasetSegpaper$MEM, datasetSegpaper$MBAR_DMN_sameseg)
pcor.test(datasetSegpaper$MEM, datasetSegpaper$MBAR_DMN_sameseg, datasetSegpaper$CT_DMNMBAR)
pcor.test(datasetSegpaper$MEM, datasetSegpaper$MBAR_DMN_sameseg, datasetSegpaper$sites)

cor.test(datasetSegpaper$MEM, datasetSegpaper$MBAR_FPN_sameseg)
pcor.test(datasetSegpaper$MEM, datasetSegpaper$MBAR_FPN_sameseg, datasetSegpaper$sites)
pcor.test(datasetSegpaper$MEM, datasetSegpaper$MBAR_FPN_sameseg, datasetSegpaper$CT_FPNMBAR)

cor.test(datasetSegpaper$MEM, datasetSegpaper$MBAR_CON_sameseg)
pcor.test(datasetSegpaper$MEM, datasetSegpaper$MBAR_CON_sameseg, datasetSegpaper$sites)
pcor.test(datasetSegpaper$MEM, datasetSegpaper$MBAR_CON_sameseg, datasetSegpaper$CT_CONMBAR)

#network correlations with Working Memory
cor.test(datasetSegpaper$WM, datasetSegpaper$MBAR_DMN_sameseg)
pcor.test(datasetSegpaper$WM, datasetSegpaper$MBAR_DMN_sameseg, datasetSegpaper$CT_DMNMBAR)
pcor.test(datasetSegpaper$WM, datasetSegpaper$MBAR_DMN_sameseg, datasetSegpaper$sites)

cor.test(datasetSegpaper$WM, datasetSegpaper$MBAR_FPN_sameseg)
pcor.test(datasetSegpaper$WM, datasetSegpaper$MBAR_FPN_sameseg, datasetSegpaper$sites)
pcor.test(datasetSegpaper$WM, datasetSegpaper$MBAR_FPN_sameseg, datasetSegpaper$CT_FPNMBAR)

cor.test(datasetSegpaper$WM, datasetSegpaper$MBAR_CON_sameseg)
pcor.test(datasetSegpaper$WM, datasetSegpaper$MBAR_CON_sameseg, datasetSegpaper$sites)
pcor.test(datasetSegpaper$WM, datasetSegpaper$MBAR_CON_sameseg, datasetSegpaper$CT_CONMBAR)

#network correlations with Language
cor.test(datasetSegpaper$LANG, datasetSegpaper$MBAR_DMN_sameseg)
pcor.test(datasetSegpaper$LANG, datasetSegpaper$MBAR_DMN_sameseg, datasetSegpaper$CT_DMNMBAR)
pcor.test(datasetSegpaper$LANG, datasetSegpaper$MBAR_DMN_sameseg, datasetSegpaper$sites)

cor.test(datasetSegpaper$LANG, datasetSegpaper$MBAR_FPN_sameseg)
pcor.test(datasetSegpaper$LANG, datasetSegpaper$MBAR_FPN_sameseg, datasetSegpaper$sites)
pcor.test(datasetSegpaper$LANG, datasetSegpaper$MBAR_FPN_sameseg, datasetSegpaper$CT_FPNMBAR)

cor.test(datasetSegpaper$LANG, datasetSegpaper$MBAR_CON_sameseg)
pcor.test(datasetSegpaper$LANG, datasetSegpaper$MBAR_CON_sameseg, datasetSegpaper$sites)
pcor.test(datasetSegpaper$LANG, datasetSegpaper$MBAR_CON_sameseg, datasetSegpaper$CT_CONMBAR)

#Processing speed with all parcellations
#MBAR POWER NODES
cor.test(datasetSegpaper$PS, datasetSegpaper$MBAR_ass_segsame)
pcor.test(datasetSegpaper$PS, datasetSegpaper$MBAR_ass_segsame, datasetSegpaper$sites)
pcor.test(datasetSegpaper$PS, datasetSegpaper$MBAR_ass_segsame, datasetSegpaper$CT_associationsysMBAR)

cor.test(datasetSegpaper$PS, datasetSegpaper$MBAR_DMN_sameseg)
pcor.test(datasetSegpaper$PS, datasetSegpaper$MBAR_DMN_sameseg, datasetSegpaper$CT_DMNMBAR)
pcor.test(datasetSegpaper$PS, datasetSegpaper$MBAR_DMN_sameseg, datasetSegpaper$sites)

cor.test(datasetSegpaper$PS, datasetSegpaper$MBAR_FPN_sameseg)
pcor.test(datasetSegpaper$PS, datasetSegpaper$MBAR_FPN_sameseg, datasetSegpaper$sites)
pcor.test(datasetSegpaper$PS, datasetSegpaper$MBAR_FPN_sameseg, datasetSegpaper$CT_FPNMBAR)

cor.test(datasetSegpaper$PS, datasetSegpaper$MBAR_CON_sameseg)
pcor.test(datasetSegpaper$PS, datasetSegpaper$MBAR_CON_sameseg, datasetSegpaper$sites)
pcor.test(datasetSegpaper$PS, datasetSegpaper$MBAR_CON_sameseg, datasetSegpaper$CT_CONMBAR)

cor.test(datasetSegpaper$PS, datasetSegpaper$MBAR_sm_segsame)
pcor.test(datasetSegpaper$PS, datasetSegpaper$MBAR_sm_segsame, datasetSegpaper$sites)
#fdr corrections for correlations
pvals<-c(2.434e-05, 0.0008754, 0.0008927, 0.001675, 0.1953)
p.adjust(pvals, method = p.adjust.methods, n = length(pvals))

#MBAR COMMUNITY DETECTION NODES
cor.test(datasetSegpaper$PS, datasetSegpaper$MBARCD_ass_segsame)
pcor.test(datasetSegpaper$PS, datasetSegpaper$MBARCD_ass_segsame, datasetSegpaper$sites)
pcor.test(datasetSegpaper$PS, datasetSegpaper$MBARCD_ass_segsame, datasetSegpaper$CT_associationsysCDMBAR)

cor.test(datasetSegpaper$PS, datasetSegpaper$MBARCD_DMN_segsame)
pcor.test(datasetSegpaper$PS, datasetSegpaper$MBARCD_DMN_segsame, datasetSegpaper$CT_DMNCDMBAR)
pcor.test(datasetSegpaper$PS, datasetSegpaper$MBARCD_DMN_segsame, datasetSegpaper$sites)

cor.test(datasetSegpaper$PS, datasetSegpaper$MBARCD_FPN_segsame)
pcor.test(datasetSegpaper$PS, datasetSegpaper$MBARCD_FPN_segsame, datasetSegpaper$sites)
pcor.test(datasetSegpaper$PS, datasetSegpaper$MBARCD_FPN_segsame, datasetSegpaper$CT_FPNCDMBAR)

cor.test(datasetSegpaper$PS, datasetSegpaper$MBARCD_CON_segsame)
pcor.test(datasetSegpaper$PS, datasetSegpaper$MBARCD_CON_segsame, datasetSegpaper$sites)
pcor.test(datasetSegpaper$PS, datasetSegpaper$MBARCD_CON_segsame, datasetSegpaper$CT_CONCDMBAR)

cor.test(datasetSegpaper$PS, datasetSegpaper$MBARCD_sm_segsame)
pcor.test(datasetSegpaper$PS, datasetSegpaper$MBARCD_sm_segsame, datasetSegpaper$sites)

#CHAN NODES
cor.test(datasetSegpaper$PS, datasetSegpaper$Chan_ass_sameseg)
pcor.test(datasetSegpaper$PS, datasetSegpaper$Chan_ass_sameseg, datasetSegpaper$sites)
pcor.test(datasetSegpaper$PS, datasetSegpaper$Chan_ass_sameseg, datasetSegpaper$CT_associationsysChan)

cor.test(datasetSegpaper$PS, datasetSegpaper$Chan_DMN_segsame)
pcor.test(datasetSegpaper$PS, datasetSegpaper$Chan_DMN_segsame, datasetSegpaper$CT_DMNChan)
pcor.test(datasetSegpaper$PS, datasetSegpaper$Chan_DMN_segsame, datasetSegpaper$sites)

cor.test(datasetSegpaper$PS, datasetSegpaper$Chan_FPN_segsame)
pcor.test(datasetSegpaper$PS, datasetSegpaper$Chan_FPN_segsame, datasetSegpaper$sites)
pcor.test(datasetSegpaper$PS, datasetSegpaper$Chan_FPN_segsame, datasetSegpaper$CT_FPNChan)

cor.test(datasetSegpaper$PS, datasetSegpaper$Chan_CON_segsame)
pcor.test(datasetSegpaper$PS, datasetSegpaper$Chan_CON_segsame, datasetSegpaper$sites)
pcor.test(datasetSegpaper$PS, datasetSegpaper$Chan_CON_segsame, datasetSegpaper$CT_CONChan)

cor.test(datasetSegpaper$PS, datasetSegpaper$Chan_sm_sameseg)
pcor.test(datasetSegpaper$PS, datasetSegpaper$Chan_sm_sameseg, datasetSegpaper$sites)

#HAN NODES
cor.test(datasetSegpaper$PS, datasetSegpaper$Han_ass_segsame)
pcor.test(datasetSegpaper$PS, datasetSegpaper$Han_ass_segsame, datasetSegpaper$sites)
pcor.test(datasetSegpaper$PS, datasetSegpaper$Han_ass_segsame, datasetSegpaper$CT_associationsysHan)

cor.test(datasetSegpaper$PS, datasetSegpaper$Han_DMN_segsame)
pcor.test(datasetSegpaper$PS, datasetSegpaper$Han_DMN_segsame, datasetSegpaper$CT_DMNHan)
pcor.test(datasetSegpaper$PS, datasetSegpaper$Han_DMN_segsame, datasetSegpaper$sites)

cor.test(datasetSegpaper$PS, datasetSegpaper$Han_FPN_segsame)
pcor.test(datasetSegpaper$PS, datasetSegpaper$Han_FPN_segsame, datasetSegpaper$sites)
pcor.test(datasetSegpaper$PS, datasetSegpaper$Han_FPN_segsame, datasetSegpaper$CT_FPNHan)

cor.test(datasetSegpaper$PS, datasetSegpaper$Han_CON_segsame)
pcor.test(datasetSegpaper$PS, datasetSegpaper$Han_CON_segsame, datasetSegpaper$sites)
pcor.test(datasetSegpaper$PS, datasetSegpaper$Han_CON_segsame, datasetSegpaper$CT_CONHan)

cor.test(datasetSegpaper$PS, datasetSegpaper$Han_sm_sameseg)
pcor.test(datasetSegpaper$PS, datasetSegpaper$Han_sm_sameseg, datasetSegpaper$sites)

################################
# Multiple Linear Regressions
library(QuantPsyc)
fit <- lm(Avg_factor ~ MBAR_ass_mean + MBAR_Ass_pcoeff + MBAR_Ass_mod + MBAR_ass_segsame, data=outlierout)
summary(fit) # show results
coef_fit <- lm.beta(fit)
coef_fit

fit <- lm(Avg_factor ~ MBAR_CON_sameseg+ MBAR_FPN_sameseg+ MBAR_DMN_sameseg, data=datasetSegpaper)
summary(fit) # show results
coef_fit <- lm.beta(fit)
coef_fit

fit <- lm(PS ~ MBAR_CON_sameseg+ MBAR_FPN_sameseg+ MBAR_DMN_sameseg, data=datasetSegpaper)
summary(fit) # show results
coef_fit <- lm.beta(fit)
coef_fit

