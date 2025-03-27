library(ggplot2)
library(ggpubr)
source("Neonate and Pregnancy sim function.R")

#Import results from httk and Gastroplus
httk_GP_results<-read.csv("analysis for paper/httk_GP_6months_results.csv")

stand_plasma_Cplasma<-httk_GP_results$httk.plasma.Cmax
stand_plasma_Cbrain<-httk_GP_results$httk.brain.Cmax 

#Make simulations
physchemOfInterest<-"Fu_adjustment/Fu"
runSimulation_nocorrection<-Run_batch(individual="6_months",
                         partitionQSPR="PKSim",
                         Dose_mg_kg=1,highResol=0.33,lowResol=0.07,
                         permeability="Normal",ionization="ignored",Fu_correction="No")

runSimulation_correction<-Run_batch(individual="6_months",
                                      partitionQSPR="PKSim",
                                      Dose_mg_kg=1,highResol=0.33,lowResol=0.07,
                                      permeability="Normal",ionization="ignored",Fu_correction="Yes")

###Calculate fold differences for simulatiosn without fu corretion###
runSimulation_nocorrection<-runSimulation_nocorrection$tb_results
runSimulation_nocorrection[,"Fold_Plasma_httk"]<-runSimulation_nocorrection[,"Cmax_Plasma_umol_L"]/stand_plasma_Cplasma
runSimulation_nocorrection[,"Fold_Brain_httk"]<-runSimulation_nocorrection[,"Cmax_Brain_umol_L"]/stand_plasma_Cbrain
runSimulation_nocorrection[,"Lipophilicity"]<-input_physchem$Lipophilicity
runSimulation_nocorrection[,"Log_Fub"]<-log10(input_physchem$Fub)
runSimulation_nocorrection[,"Clearance"]<-input_physchem$Clearance..min.
runSimulation_nocorrection[,"MW"]<-input_physchem$MW
runSimulation_nocorrection[,"httkORgp_sim_Kbrain"]<-stand_plasma_Cbrain/stand_plasma_Cplasma

runSimulation_nocorrection[,"physchem"]<-input_physchem$Fu_adjusted/input_physchem$Fub

options(scipen=4)  
PK_Sim_FoldCmaxPlasma_NoC<-ggplot(runSimulation_nocorrection, aes(x =Lipophilicity, y =Fold_Plasma_httk, color = physchem)) +
  geom_point() +
  geom_hline(yintercept=0.1,linetype="dashed") +
  annotate("text", x=-2, y=0.2, label="0.1 fold")+
  geom_hline(yintercept=10,linetype="dashed") +
  annotate("text", x=-2, y=20, label="10 fold")+
  scale_y_log10() +  # Set y-axis to log scale
  labs(title = "Fold differences WITHOUT Fu correction",
       x = "LogP",
       y = "Fold in Cmax plasma",
       color = physchemOfInterest)+
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5))


###Calculate fold differences for simulatiosn WITH fu correction###
runSimulation_correction<-runSimulation_correction$tb_results
runSimulation_correction[,"Fold_Plasma_httk"]<-runSimulation_correction[,"Cmax_Plasma_umol_L"]/stand_plasma_Cplasma
runSimulation_correction[,"Fold_Brain_httk"]<-runSimulation_correction[,"Cmax_Brain_umol_L"]/stand_plasma_Cbrain
runSimulation_correction[,"Lipophilicity"]<-input_physchem$Lipophilicity
runSimulation_correction[,"Log_Fub"]<-log10(input_physchem$Fub)
runSimulation_correction[,"Clearance"]<-input_physchem$Clearance..min.
runSimulation_correction[,"MW"]<-input_physchem$MW
runSimulation_correction[,"httkORgp_sim_Kbrain"]<-stand_plasma_Cbrain/stand_plasma_Cplasma

runSimulation_correction[,"physchem"]<-input_physchem$Fu_adjusted/input_physchem$Fub

PK_Sim_FoldCmaxPlasma_Correc<-ggplot(runSimulation_correction, aes(x =Lipophilicity, y =Fold_Plasma_httk, color = physchem)) +
  geom_point() +
  geom_hline(yintercept=0.1,linetype="dashed") +
  annotate("text", x=-2, y=0.2, label="0.1 fold")+
  geom_hline(yintercept=10,linetype="dashed") +
  annotate("text", x=-2, y=20, label="10 fold")+
  scale_y_log10() +  # Set y-axis to log scale
  labs(title = "Fold differences WITH Fu correction",
       x = "LogP",
       y = "Fold in Cmax plasma",
       color = physchemOfInterest)+
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5))



ggarrange(PK_Sim_FoldCmaxPlasma_NoC,PK_Sim_FoldCmaxPlasma_Correc,ncol=1, nrow=2, common.legend = TRUE)