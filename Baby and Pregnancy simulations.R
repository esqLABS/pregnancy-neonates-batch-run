
library(ospsuite)
library(tidyverse)

Run_batch<-function (individual,partitionQSPR,Dose){
  # We load the pkml for which the batches will be created
  if (individual=="6_months"&partitionQSPR=="Rodger_Rowland") {
  sim1 <- loadSimulation("pkmlFiles/6_month_simulation_R&R.pkml", loadFromCache = FALSE)
  
  } else if  (individual=="6_months"&partitionQSPR=="Schmitt") {
    sim1 <- loadSimulation("pkmlFiles/6_month_simulation_Schmitt.pkml", loadFromCache = FALSE)
    
  } else if  (individual=="6_months"&partitionQSPR=="PKSim") {
    sim1 <- loadSimulation("pkmlFiles/6_month_simulation_PKSim.pkml", loadFromCache = FALSE)
    
  } else if  (individual=="6_months"&partitionQSPR=="Poulin") {
    sim1 <- loadSimulation("pkmlFiles/6_month_simulation_Poulin.pkml", loadFromCache = FALSE)
    
  } else if  (individual=="2_weeks"&partitionQSPR=="Rodger_Rowland") {
    sim1 <- loadSimulation("pkmlFiles/2_weeks_simulation_R&R.pkml", loadFromCache = FALSE)
    
  } else if  (individual=="2_weeks"&partitionQSPR=="Schmitt") {
    sim1 <- loadSimulation("pkmlFiles/2_weeks_simulation_Schmitt.pkml", loadFromCache = FALSE)
    
  } else if  (individual=="2_weeks"&partitionQSPR=="PKSim") {
    sim1 <- loadSimulation("pkmlFiles/2_weeks_simulation_PKSim.pkml", loadFromCache = FALSE)
    
  } else if  (individual=="2_weeks"&partitionQSPR=="Poulin") {
    sim1 <- loadSimulation("pkmlFiles/2_weeks_simulation_Poulin.pkml", loadFromCache = FALSE)
  
  } else {warning("mistake in input model")}
  
  
  #To see the simulation steps
  timesteps<-sim1$outputSchema
  ####CHECK FIRST STRUCTURE####--------------------------------------------------
  #Get parameters location
 tree <- getSimulationTree(sim1)
 tree$Test_Chemical
  
  #If I want to see the value of the parameter and units
  # 
  # Fu<-getParameter("Test_Chemical|Fraction unbound (plasma, reference value)", sim1)
  # Lip<-getParameter("Test_Chemical|Lipophilicity", sim1)
  # Sol<-getParameter("Test_Chemical|Solubility at reference pH",sim1)
  # oralabs<-getParameter("Test_Chemical|Specific intestinal permeability (transcellular)", sim1)
  # clearance<-getParameter("Test_Chemical-Total Hepatic Clearance-database|Specific clearance", sim1)
  # MW<-getParameter("Test_Chemical|Molecular weight", sim1)
  # Dose<-getParameter("Applications|Daily ingestion|Dissolved formulation|Application_1|ProtocolSchemaItem|DosePerBodyWeight", sim1)
  # #setParameterValues(Dose, 2E-6, units = "kg/kg")
  
  #Make just one simulation for the original
  # res <- runSimulations(sim1)
  # outputValues<-getOutputValues(res[[1]])
  
  ####CONSTRUCT BATCH####----------------------------------------------------------
  # define the list of parameter that will be varied between the runs.
  parameterPaths <- c("Test_Chemical|Fraction unbound (plasma, reference value)",
                      "Test_Chemical|Lipophilicity",
                      "Test_Chemical|Solubility at reference pH",
                      "Test_Chemical|Specific intestinal permeability (transcellular)",
                      "Test_Chemical-Total Hepatic Clearance-database|Specific clearance",
                      "Test_Chemical|Molecular weight",
                      "Test_Chemical|Effective molecular weight",
                      "Test_Chemical|pKa value 0",
                      "Test_Chemical|pKa value 1",
                      "Test_Chemical|pKa value 2",
                      "Test_Chemical|Compound type 0",
                      "Test_Chemical|Compound type 1",
                      "Test_Chemical|Compound type 2")
  
  
  # define the simulation batch
  simBatch <- createSimulationBatch(simulation = sim1, parametersOrPaths = parameterPaths)
  
  #load the chemicals_input parameters
  input_physchem<-read.csv("test_batch.csv")
  #from NA to 0
  input_physchem[is.na(input_physchem)]<-0
  nChemicals<-nrow(input_physchem)
  #Check if imported correctly
  #View(input_physchem)

  #The number of parameters to vary for each batch
  #needs to correspond to the vector of parameterPaths and in the same order
  for (i in 1:nChemicals){
    
    #Account for number of halogens
    nF<- str_count(input_physchem[i,"SMILES"], "F")
    nCl<- str_count(input_physchem[i,"SMILES"], "Cl")
    nBr<- str_count(input_physchem[i,"SMILES"], "Br")
    nI<- str_count(input_physchem[i,"SMILES"], "I")
    effective_mw<-input_physchem$MW[i] - nF * 0.000000017 - nCl * 0.000000022 - nBr * 0.000000062 - nI * 0.000000098 
    
    simBatch$addRunValues(parameterValues = c(input_physchem[i,"Fub"],
                                              input_physchem[i,"Lipophilicity"],
                                              input_physchem[i,"Solubility..pH.7..g.mL."],
                                              input_physchem[i,"Papp_.dm.min."],
                                              input_physchem[i,"Clearance..min."],
                                              input_physchem[i,"MW.kg.umol."],
                                              effective_mw,
                                              input_physchem[i,"pKa1"],
                                              input_physchem[i,"pKa2"],
                                              input_physchem[i,"pKa3"],
                                              input_physchem[i,"CompountType1"],
                                              input_physchem[i,"CompountType2"],
                                              input_physchem[i,"CompountType3"]))                                        
    }

  ###SIMULATIONS###
  results <- runSimulationBatches(simBatch)
  #get the name of values
  
   #print(names(unlist(results)))
  
  #Note:The enqueued run values are cleared after calling runSimulationBatches(),
  #so executing the run again would result in an empty results list.
  
  ####STORE DATA AND FIND CMAX####
  #Open list for saving data
  batchResList<-list()
  #open table for Cmax and Tmax
  tableCmax<-data.frame(matrix(ncol=4,nrow=nChemicals))
  ###UNITS####
  colnames(table6month)<-c("Cmax_Plasma_umol_L","Tmax_plasma_min","Cmax_Brain_umol_L","Tmax_brain_min")
  rownames(table6month)<-input_physchem[,1]
  
  for (j in 1:nChemicals){
    
  outputValues1<-getOutputValues(results[[1]][[j]])  
  batchResList[[j]]<-data.frame("Time-min"=outputValues1$data$Time,
                               "VenousPlasma-umol/L"=outputValues1$data$`Organism|PeripheralVenousBlood|Test_Chemical|Plasma (Peripheral Venous Blood)`,
                               "Brain-umol/L"=outputValues1$data$`Organism|Brain|Test_Chemical|Tissue`)
  
  tableCmax[j,1]<-max(batchResList[[j]]$VenousPlasma.umol.L)
  tableCmax[j,2]<-batchResList[[j]]$Time.min[which(batchResList[[j]]$VenousPlasma.umol.L==table6month[j,1])]
  table6Cmax[j,3]<-max(batchResList[[j]]$Brain.umol.L)
  tableCmax[j,4]<-batchResList[[j]]$Time.min[which(batchResList[[j]]$Brain.umol.L==table6month[j,3])]
  }
  
  return(list("tableCmax"=tableCmax,"batchResList"=batchResList))
}

#example of simulation
runSimulation("6_months","Rodger_Rowland",1)
#if you want to see the Cmax
runSimulation$table6month

#If you want to check a plot for a specific chemical
chemical2plot<-"B"
nrChemical<-which(input_physchem[,"Chemical"]==chemical2plot)
### CHANGE SCALE TO LOG###
plot(x=batchResList[[nrChemical]]$Time,y=batchResList[[nrChemical]]$Brain.umol.L,type="l",
     xlab="Time in min",
     ylab="Concentration in umol/L",
     log='y',ylim=c(0.001,1.5))
  lines(x=batchResList[[nrChemical]]$Time,y=log(batchResList[[nrChemical]]$VenousPlasma.umol.L),col="red")
legend(x = "bottomright",          # Position
       legend = c("brain", "plasma"),  # Legend texts
       lty = c(1, 2),           # Line types
       col = c("black", "red"),           # Line colors
       lwd = 2)                 # Line width

