#Decision tree


#get the function
source("Neonate and Pregnancy sim function.R")

#Option 1 - Naive method

#Option 2 - Changing the logP to logMA
logMA <- 1.294+0.304*logP


#Option 3 - Accounting for ionization and high lipo



age<-c(rep("2_weeks",4),rep("6_months",4),rep("GW15",4),rep("GW24",4))
PKalgor<-rep(c("Rodger_Rowland","Schmitt","PKSim","Poulin"),4)

runSimulation<-list()
dataModel<-data.frame(matrix(ncol = 4, nrow = nChemicals))
mergeDataModel<-data.frame()

for (i in seq(1:length(age))){
  ### make this a dataframe
  runSimulation[[i]]<-Run_batch(individual=age[i],partitionQSPR=PKalgor[i],
                                Dose_mg_kg=1,highResol=0.33,lowResol=0.07,
                                permeability="simple")
  if (age[i]=="6_months"|age[i]=="2_weeks"){
    dataModel<-data.frame("Chemicals"=rownames(runSimulation[[i]]$tb_results),
                          "Cmax_plasma"=runSimulation[[i]]$tb_results[,1],
                          "age"=rep(age[i],nrow(input_physchem)),
                          "PartitionQSPR"=rep(PKalgor[i],nrow(input_physchem)))
    
  } else if (age[i]=="GW15"|age[i]=="GW24"){
    
    dataModel<-data.frame("Chemicals"=rownames(runSimulation[[i]]$tb_results),
                          "Cmax_plasma"=runSimulation[[i]]$tb_results[,3],
                          "age"=rep(age[i],nrow(input_physchem)),
                          "PartitionQSPR"=rep(PKalgor[i],nrow(input_physchem)))
    
  }else{}
  mergeDataModel<-bind_rows(mergeDataModel,dataModel)
  
}
colnames(mergeDataModel)=c("Chemicals","Cmax_plasma","age","PartitionQSPR")