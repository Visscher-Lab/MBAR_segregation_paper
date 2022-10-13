#making figures
library(ggplot2)
library(ggExtra)
library(tidyr)
library(tidyverse)
library(readxl)

#Association system metrics and overall cognition
assseg<-ggplot(outlierout, aes(y=Avg_factor, x=MBAR_ass_segsame)) + geom_point(aes()) +xlab("Association System Segregation")+ theme_classic() + theme(text=element_text(size=20))+ ylab("Overall Cognition Factor Score")+geom_smooth(aes(), method="lm")+theme(legend.position="bottom") 
assseg1<-ggMarginal(assseg, margins = "both", xparams=list(fill="grey"), yparams=list(fill="black"),size=8)
assseg1
assmod<-ggplot(outlierout, aes(y=Avg_factor, x=MBAR_Ass_mod)) + geom_point(aes()) +xlab("Association System Modularity")+ theme_classic() + theme(text=element_text(size=20))+ ylab("Overall Cognition Factor Score")+geom_smooth(aes(), method="lm")+theme(legend.position="bottom") 
assmod1<-ggMarginal(assmod, margins = "both", xparams=list(fill="grey"), yparams=list(fill="black"),size=8)
assmod1
assmean<-ggplot(outlierout, aes(y=Avg_factor, x=MBAR_ass_mean)) + geom_point(aes()) +xlab("Association System Mean Connectivity")+ theme_classic() + theme(text=element_text(size=20))+ ylab("Overall Cognition Factor Score")+geom_smooth(aes(), method="lm")+theme(legend.position="bottom") 
assmean1<-ggMarginal(assmean, margins = "both", xparams=list(fill="grey"), yparams=list(fill="black"),size=8)
assmean1
asspc<-ggplot(outlierout, aes(y=Avg_factor, x=MBAR_Ass_pcoeff)) + geom_point(aes()) +xlab("Association Participation Coefficient")+ theme_classic() + theme(text=element_text(size=20))+ ylab("Overall Cognition Factor Score")+geom_smooth(aes(), method="lm")+theme(legend.position="bottom") 
asspc1<-ggMarginal(asspc, margins = "both", xparams=list(fill="grey"), yparams=list(fill="black"),size=8)
asspc1

#Relationship with processing speed across parcllations
Results_Seg_paper_8_3_22 <- read_excel("~/Desktop/Results Seg paper 8.16.22.xlsx")
plotA<-ggplot(Results_Seg_paper_8_3_22, aes(fill=factor(Network, c('Association System', 'DMN','FPN', 'CON', 'Sensory/motor System')), y=r, x=fct_inorder(Nodeset))) + 
  geom_bar(position="dodge", stat="identity", colour="black") + scale_fill_manual(values=c("blue", "red", "gold", "purple", "grey")) + xlab('Parcellation') + scale_y_continuous(breaks = seq(-.1, .35, by=.05)) + ylab('Correlation (r)') +
  ggtitle("Relationship with Processing Speed \nacross Parcellations") + theme_bw(base_size = 20) + 
  theme(axis.line = element_line(color='black'), plot.background = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.border = element_blank())
plotAB <- plotA + guides(fill=guide_legend(title="System/Network")) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  theme(plot.title = element_text(hjust = 0.5)) 
plotAB
ggsave("nodesetPScorrplot11.jpeg", plot=plotAB, dpi=320, path="~/Desktop/")

#Relationship between cortical thickness and age
plotB<-ggplot(datasetSegpaper, aes(y=CT_means_wholebrainMBAR, x=age_visit)) +
  geom_point(stat="identity", colour="black") + xlab('Age') + ylab('Average Whole Brain Cortical Thickness')+geom_smooth(aes(), method="lm") +ggtitle("Relationship between Age and Cortical Thickness") + theme_bw(base_size = 20) + 
  theme(axis.line = element_line(color='black'), plot.background = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.border = element_blank())
plotB1<-plotB + scale_x_continuous(breaks = seq(85, 99, by = 1))
plotB1

###############
#Networks with overall cognition
data_pivoted<-pivot_longer(datasetSegpaper, cols= MBAR_DMN_sameseg:MBAR_CON_sameseg, names_to="network", values_to="network_segregation")
netseg<-ggplot(data_pivoted, aes(y=Avg_factor, x=network_segregation, color=network)) + geom_point(aes()) +xlab("Network Segregation")+ theme_classic() + theme(text=element_text(size=20))+ ylab("Overall Cognition Factor Score")+geom_smooth(aes(), method="lm")+theme(legend.position="bottom")+scale_colour_manual(name="Network", labels= c("FPN", "CON", "DMN"), values = c("MBAR_FPN_sameseg"="gold", "MBAR_CON_sameseg"="purple", "MBAR_DMN_sameseg"="red"))
netseg1<-ggMarginal(netseg, margins = "both", groupColour = TRUE, groupFill = TRUE,yparams=list(fill="black"),size=8)
netseg1
#Networks with processing speed
data_pivoted<-pivot_longer(datasetSegpaper, cols= MBAR_DMN_sameseg:MBAR_CON_sameseg, names_to="network", values_to="network_segregation")
netsegPS<-ggplot(data_pivoted, aes(y=PS, x=network_segregation, color=network)) + geom_point(aes()) +xlab("Network Segregation")+ theme_classic() + theme(text=element_text(size=20))+ ylab("Processing Speed")+geom_smooth(aes(), method="lm")+theme(legend.position="bottom") + scale_colour_manual(name="Network", labels= c("FPN", "CON", "DMN"), values = c("MBAR_FPN_sameseg"="gold", "MBAR_CON_sameseg"="purple", "MBAR_DMN_sameseg"="red"))
netsegPS1<-ggMarginal(netsegPS, margins = "both", groupColour = TRUE, groupFill = TRUE,yparams=list(fill="black"),size=8)
netsegPS1


#segregation across age
SEG_ACROSS_AGE <- read_excel("~/Desktop/SEG ACROSS AGE.xlsx")
segage<-ggplot(SEG_ACROSS_AGE, aes(y=Mean_Seg, x=Age_Group))+ geom_point(aes())+xlab("Age Group")+ theme_classic() + theme(text=element_text(size=20))+ ylab("Mean Association System Segregation")+scale_x_discrete(limits=SEG_ACROSS_AGE$Age_Group)+geom_line(aes(group=1))
segage