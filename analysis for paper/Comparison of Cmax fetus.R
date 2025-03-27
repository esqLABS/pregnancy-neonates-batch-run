source("Neonate and Pregnancy sim function.R")


runSimulation_PKSim<-Run_batch(individual="GW24",partitionQSPR="PKSim",
                               Dose_mg_kg=1,highResol=0.33,lowResol=0.07,
                               permeability="Normal",ionization="ignored",Fu_correction="No")

runSimulation_Poulin<-Run_batch(individual="GW24",partitionQSPR="Poulin",
                                Dose_mg_kg=1,highResol=0.33,lowResol=0.07,
                                permeability="Normal",ionization="ignored",Fu_correction="No")

runSimulation_Fu_adj<-Run_batch(individual="GW24",partitionQSPR="PKSim",
                                Dose_mg_kg=1,highResol=0.33,lowResol=0.07,
                                permeability="Normal",ionization="ignored",Fu_correction="Yes")

httk_GP_results<-read.csv("analysis for paper/httk_GP_6months_results.csv")

size<-length(httk_GP_results$GP.plasma.Cmax)

df_G24<-data.frame("values"=c(runSimulation_PKSim$tb_results$Cmax_Fetus_plasma_umol_L,
                              runSimulation_Poulin$tb_results$Cmax_Fetus_plasma_umol_L,
                              runSimulation_Fu_adj$tb_results$Cmax_Fetus_plasma_umol_L,
                             # httk_GP_results$GP.plasma.Cmax,
                              httk_GP_results$httk.plasma.Cmax),
                  "model"=c(rep("PK_Sim",size),
                            rep("Poulin",size),
                            rep("PKSim+Fu_adj",size),
                          #  rep("gp",size),
                            rep("httk",size)))

ggplot(data=df_G24,aes(x=factor(model),y=values))+
  geom_dotplot(binaxis = "y", stackdir = "center",dotsize=0.3)+
  geom_violin(trim = FALSE)
