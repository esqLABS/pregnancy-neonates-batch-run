#Comparin gto in vivo ratio maternal and fetus cord blood
#get the function
source("Neonate and Pregnancy sim function.R")

exp_data<-data.frame("name"=c("Benz[a]anthracene","Dieldrin","Heptachlor","p,p'-DDD",
                              "p,p'-DDT","Perfluorooctanoic acid"),
                    "mean"=c(1.283,0.735,0.803,0.764,0.640,0.814),
                    "min"=c(1.283,0.708,0.365,0.109,0.242,0.536),
                    "max"=c(1.283,1.762,1.241,1.214,1.208,1.266))

input_physchem<-read.csv("test_files/test_batch_2.csv")
#select only the chemicals of interest
input_physchem<-input_physchem[c(which(input_physchem[,1]==exp_data$name[1]),
                                 which(input_physchem[,1]==exp_data$name[2]),
                                 which(input_physchem[,1]==exp_data$name[3]),
                                 which(input_physchem[,1]==exp_data$name[4]),
                                 which(input_physchem[,1]==exp_data$name[5]),
                                 which(input_physchem[,1]==exp_data$name[6])),]
  
PKalgor<-c("Rodger_Rowland","Schmitt","PKSim","Poulin")
nChemicals<-nrow(input_physchem)
runSimulation<-list()
dataModel<-data.frame(matrix(ncol = 3, nrow = nChemicals))
mergeDataModel<-data.frame()

for (i in seq(1:length(PKalgor))){
  
  runSimulation[[i]]<-Run_batch(individual="GW24",partitionQSPR=PKalgor[i],
                                Dose_mg_kg=1,highResol=0.33,lowResol=0.07,
                                permeability="Normal",ionization="ignored",Fu_correction="normal") 
  #need to edit this
  dataModel<-data.frame("Chemicals"=rownames(runSimulation[[i]]$tb_results),
                        "Cmax_plasma_maternal"=runSimulation[[i]]$tb_results[,1],
                        "Tmax_plasma_maternal"=runSimulation[[i]]$tb_results[,2],
                        "Cmax_plasma_fetus"=runSimulation[[i]]$tb_results[,3],
                        "Tmax_plasmafetus"=runSimulation[[i]]$tb_results[,4],
                        "PartitionQSPR"=rep(PKalgor[i],nrow(input_physchem)))
  
  
  mergeDataModel<-bind_rows(mergeDataModel,dataModel)
  
}

colnames(mergeDataModel)=c("Chemicals","Cmax_plasma_maternal","Tmax_plasma_maternal",
                            "Cmax_plasma_fetus","Tmax_plasma_fetus","PartitionQSPR")

mergeDataModel[,"Ratio_fetal_maternal_plasma_concetrations"]<-mergeDataModel[,"Cmax_plasma_fetus"]/mergeDataModel[,"Cmax_plasma_maternal"]
mergeDataModel[,"experimental_mean"]<-rep(exp_data$mean,length(PKalgor))
mergeDataModel[,"experimental_min"]<-rep(exp_data$min,length(PKalgor))
mergeDataModel[,"experimental_max"]<-rep(exp_data$max,length(PKalgor))


ggplot(mergeDataModel)+
  facet_grid(rows=vars(PartitionQSPR))+
  geom_point(aes(x=experimental_mean,y=Ratio_fetal_maternal_plasma_concetrations,col=Chemicals))+
  geom_segment(aes(
    x = experimental_min,
    xend = experimental_max,
    y = Ratio_fetal_maternal_plasma_concetrations,
    yend = Ratio_fetal_maternal_plasma_concetrations,col=Chemicals)) +   # Key for creating the dashed line
  ylim(0.2,3)+
  xlim(0,2)+
  geom_abline(intercept = 0, slope = 1)

  
  
  



