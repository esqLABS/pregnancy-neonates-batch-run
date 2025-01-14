rm(list=ls(all=TRUE))

library(httk)
date.string <- Sys.Date()       
species_1 <- "Human" 
gestationDays <-168   #gestation days (starting dosing); 105 for 15GW, 168 for 24GW
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
  parm <- parameterize_fetal_pbtk(chem.cas= this.cas)
  output <- solve_fetal_pbtk(chem.cas = this.cas, parameters = parm, times=seq(gestationDays,gestationDays+ndays,0.05), tsteps = 21, doses.per.day = dpd, 
                             dose = 1, iv.dose =FALSE, output.units = outputUnit, 
                             default.to.human = TRUE, plots = FALSE, suppress.messages = TRUE)

  write.table(output, file = paste(getwd(), '/outputtstep24GW_', this.chem, '.csv', sep=''), sep=',', col.names = TRUE, row.names = FALSE, quote = FALSE)

#the following lines can be included to gather model parameters, i.e. partition coefficient  
#param <- parameterize_fetal_pbtk(chem.cas = this.cas, suppress.messages = TRUE)  

#write.table(param, file = paste(getwd(), '/parmdef15GW_', this.chem, '.csv', sep=''), sep=',', col.names = TRUE, row.names = FALSE, quote = FALSE)
}


