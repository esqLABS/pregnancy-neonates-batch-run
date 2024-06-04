#Test function#--------
#It runs all the partitions

#get the function
source("Neonate and Pregnancy sim function.R")

age<-c(rep("2_weeks",4),rep("6_months",4),rep("GW15",4),rep("GW24",4))
PKalgor<-rep(c("Rodger_Rowland","Schmitt","PKSim","Poulin"),4)

runSimulation<-list()
dataModel<-data.frame(matrix(ncol = 4, nrow = nChemicals))
mergeDataModel<-data.frame()

for (i in seq(1:length(age))){
    ### make this a dataframe
  runSimulation[[i]]<-Run_batch(individual=age[i],partitionQSPR=PKalgor[i],
                                    Dose_mg_kg=1,highResol=0.33,lowResol=0.07)
  if (age[i]=="6_months"|age[i]=="2_weeks"){
  dataModel<-data.frame("Chemicals"=rownames(runSimulation[[i]]$tableCmax),
                        "Cmax_plasma"=runSimulation[[i]]$tableCmax[,1],
                        "age"=rep(age[i],nrow(input_physchem)),
                        "PartitionQSPR"=rep(PKalgor[i],nrow(input_physchem)))
  
  } else if (age[i]=="GW15"|age[i]=="GW24"){
    
    dataModel<-data.frame("Chemicals"=rownames(runSimulation[[i]]$tableCmax),
                          "Cmax_plasma"=runSimulation[[i]]$tableCmax[,3],
                          "age"=rep(age[i],nrow(input_physchem)),
                          "PartitionQSPR"=rep(PKalgor[i],nrow(input_physchem)))
    
  }else{}
  mergeDataModel<-bind_rows(mergeDataModel,dataModel)

}
colnames(mergeDataModel)=c("Chemicals","Cmax_plasma","age","PartitionQSPR")


###Plot Maternal Cmax plasma
ggplot(data=mergeDataModel,aes(x=Chemicals,y=Cmax_plasma,shape=PartitionQSPR,color=PartitionQSPR))+
  facet_wrap(vars(age))+
  geom_point(position=position_jitter(0.3),size=2)+
  theme_bw(base_size = 20)+
  theme(axis.title.x = element_blank(),axis.title.y = element_text(color = "grey30", size = 20),
        panel.grid.major.x =element_blank())+
  ylab("Baby/Neonate Venous Plasma Cmax")
