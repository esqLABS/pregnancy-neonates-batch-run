rm(list=ls(all=TRUE))

library(httk)
date.string <- Sys.Date()       

#physiology.data
#write.table(physiology.data, file = "httk_physio_data.txt", sep='\t', col.names = TRUE, row.names = FALSE, quote = FALSE) #why do i read this in?

species_1 <- "Human" 
#expDose <- 1         
ndays <- 1          
dpd <- NULL             
route <- "oral"        
outputUnit <- "uM"   

chemFile <- "chemicalData_DNTIVIVE_050924.txt"
chemical <- as.data.frame(read.table(file = chemFile, header=TRUE, sep = "\t"))  
colnames(chemical)  
chem.physical_and_invitro.data <- add_chemtable(chemical, 
                                                current.table = chem.physical_and_invitro.data, 
                                                data.list = list(Compound = "Substance.Name", CAS = "CASRN",DTXSID="DTXSID",Clint = "Clint", Funbound.plasma = "fu"),
                                                species = species_1, reference = paste0(species_1, "ICE"), overwrite = T)


if(route != "iv"){   
  iv.dose = FALSE
}else{iv.dose =TRUE}


for(this.cas in chemical[,"CASRN"]) {
  this.chem <-  chemical$Substance.Name[chemical$CASRN==this.cas]
  parm <- parameterize_pbtk_adipose_complexbrain(chem.cas= this.cas)
  parm$BW<-8.29 #3.7 for 2w & 8.29 for 6m as from GastroPlus
  output <- solve_pbtk_adipose_complexbrain(chem.cas = this.cas, parameters = parm, tsteps = 21, doses.per.day = dpd, days = 1, 
                             dose = 1, iv.dose =FALSE, output.units = outputUnit, 
                             default.to.human = TRUE, plots = FALSE, suppress.messages = TRUE)

  #write.table(output, file = paste(getwd(), '/output2w', this.chem, '.csv', sep=''), sep=',', col.names = TRUE, row.names = FALSE, quote = FALSE)
#  write.table(output, file = paste(getwd(), 'PedPhI', date.string, this.chem, '.csv', sep=''), sep=',', col.names = TRUE, row.names = FALSE, quote = FALSE)
 
param <- parameterize_pbtk_adipose_complexbrain(chem.cas = this.cas, suppress.messages = TRUE)  
  
write.table(param, file = paste(getwd(), '/parm6m_', this.chem, '.csv', sep=''), sep=',', col.names = TRUE, row.names = FALSE, quote = FALSE)
  
}
