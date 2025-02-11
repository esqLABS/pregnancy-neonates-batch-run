#Check for 6_months neonate where the largest differences between PK-Sim predictions , httk and gastroplus are. 

#get the function to make batch ssim
source("Neonate and Pregnancy sim function.R")

# #example
Analysis_6_months_predictions(partitionQSPR="Rodger_Rowland",physchemOfInterest="logP",
                              permeability="Normal",
                              comparison="httk",ionization="considered")

#fucntion
Analysis_6_months_predictions<-function(partitionQSPR,physchemOfInterest,permeability,comparison,ionization){
  
  library(ggplot2)
  library(ggpubr)
  
#select the type of model we want to run , if oral permeability and permeability is set by the phys-chem
#of if they are defaulted ot very high values so they are not rate limiting


   runSimulation<-Run_batch(individual="6_months",
                            partitionQSPR=partitionQSPR,
                            Dose_mg_kg=1,highResol=0.33,lowResol=0.07,permeability=permeability,ionization)
       
#Import results from httk and Gastroplus
httk_GP_results<-read.csv("analysis for paper/httk_GP_6months_results.csv")

if (comparison=="httk"){ 
  
    stand_plasma_Cplasma<-httk_GP_results$httk.plasma.Cmax
    stand_plasma_Cbrain<-httk_GP_results$httk.brain.Cmax 
    
} else if (comparison=="GP"){
  
    stand_plasma_Cplasma<-httk_GP_results$GP.plasma.Cmax
    stand_plasma_Cbrain<-httk_GP_results$GP..brain.Cmax
    
} else {print("error comparison")}

###Calculate fold differences###
resultsSimulation<-runSimulation$tb_results
resultsSimulation[,"Fold_Plasma_httk"]<-resultsSimulation[,"Cmax_Plasma_umol_L"]/stand_plasma_Cplasma
resultsSimulation[,"Fold_Brain_httk"]<-resultsSimulation[,"Cmax_Brain_umol_L"]/stand_plasma_Cbrain
resultsSimulation[,"Lipophilicity"]<-input_physchem$Lipophilicity
resultsSimulation[,"Log_Fub"]<-log10(input_physchem$Fub)
resultsSimulation[,"Clearance"]<-input_physchem$Clearance..min.
resultsSimulation[,"MW"]<-input_physchem$MW
resultsSimulation[,"httkORgp_sim_Kbrain"]<-stand_plasma_Cbrain/stand_plasma_Cplasma

if (physchemOfInterest=="logP"){
  
  resultsSimulation[,"physchem"]<-resultsSimulation[,"Lipophilicity"]
  
} else if (physchemOfInterest=="Log Pint"){
  
  resultsSimulation[,"physchem"]<-log10(resultsSimulation[,"Pint"])
  
} else if (physchemOfInterest=="Log_Fub"){
  
  resultsSimulation[,"physchem"]<-resultsSimulation[,"Log_Fub"]
  
} else if (physchemOfInterest=="Log Clearance"){
  
  resultsSimulation[,"physchem"]<-log10(resultsSimulation[,"Clearance"])
  
} else if (physchemOfInterest=="MW"){
  
  resultsSimulation[,"physchem"]<-resultsSimulation[,"MW"]
  
} else {}
  
  
  
###Make plots for PK-Sim partition alg####
options(scipen=4)  
PK_Sim_FoldCmaxPlasma<-ggplot(resultsSimulation, aes(x =Lipophilicity, y =Fold_Plasma_httk, color = physchem)) +
  geom_point() +
  geom_hline(yintercept=0.1,linetype="dashed") +
  annotate("text", x=-2, y=0.2, label="0.1 fold")+
  geom_hline(yintercept=10,linetype="dashed") +
  annotate("text", x=-2, y=20, label="10 fold")+
  scale_y_log10() +  # Set y-axis to log scale
  labs(title = "Plasma fold differences PK-Sim/httk",
       x = "LogP",
       y = "Fold in Cmax plasma",
       color = physchemOfInterest)+
  theme_bw() 


PK_Sim_FoldCmaxBrain<-ggplot(resultsSimulation, aes(x =Lipophilicity, y =Fold_Brain_httk, color = physchem)) +
  geom_hline(yintercept=0.1,linetype="dashed") +
  annotate("text", x=-2, y=0.2, label="0.1 fold")+
  geom_hline(yintercept=10,linetype="dashed") +
  annotate("text", x=-2, y=20, label="10 fold")+
  geom_point() +
  scale_y_log10() +  # Set y-axis to log scale
  labs(title = "Brain fold differences PK-Sim/httk",
       x = "LogP",
       y = "Fold in Cmax brain",
       color = physchemOfInterest)+
  theme_bw() 

PK_Sim_Kbrain_httkVSPKSim<-ggplot(resultsSimulation, aes(x=httkORgp_sim_Kbrain, y =Cmax_Brain_umol_L/Cmax_Plasma_umol_L, color = physchem)) +
  geom_point() +
  geom_abline(intercept = 0, slope = 1)+
  scale_y_log10() +
  scale_x_log10() +
  labs(title = "Simulated Brain/Plasma ",
       x = "httk",
       y = "PK-Sim",
       color = physchemOfInterest)+
  theme_bw() 


PK_Sim_PredSimKbrain<-ggplot(resultsSimulation, aes(x =log10(BrainK), y =log10(Cmax_Brain_umol_L/Cmax_Plasma_umol_L), color = physchem)) +
  geom_point() +
  geom_abline(intercept = 0, slope = 1)+
  labs(title = "Pred vs simulated log10 Kbrain/plasma",
       x = "Predicted log10 Kbrain",
       y = "Simulated log10 Kbrain",
       color = physchemOfInterest)+
  theme_bw() 
     

ggarrange(PK_Sim_FoldCmaxPlasma,PK_Sim_FoldCmaxBrain,PK_Sim_Kbrain_httkVSPKSim,PK_Sim_PredSimKbrain)
 
}
