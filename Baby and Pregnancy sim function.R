
library(ospsuite)
library(tidyverse)

#load the chemicals_input parameters
input_physchem<-read.csv("test_batch.csv")

#function 
Run_batch<-function (individual,partitionQSPR){
  
  #Load preganancy gestational age speicifc physiology
  gestationaPhysio<-read.csv("Gestational physiological parameters.csv")
  
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
  
  } else if  (individual=="GW15"&partitionQSPR=="Rodger_Rowland") {
    sim1<-loadSimulation("pkmlFiles/Pregnant_simulation_R&R.pkml", loadFromCache = FALSE)
    
    #Change the physiology to a GW15 physiology
      for (i in 2:ncol(gestationaPhysio)){
        param2change15w<-getParameter(gestationaPhysio[1,i], sim1)
        setParameterValues(param2change15w, as.numeric(gestationaPhysio[2,i]))
      }
    
  } else if  (individual=="GW15"&partitionQSPR=="Schmitt") {
      
    sim1<-loadSimulation("pkmlFiles/Pregnant_simulation_Schmitt.pkml", loadFromCache = FALSE)
    
      for (i in 2:ncol(gestationaPhysio)){
        param2change15w<-getParameter(gestationaPhysio[1,i], sim1)
        setParameterValues(param2change15w, as.numeric(gestationaPhysio[2,i]))
      }
    
  } else if  (individual=="GW15"&partitionQSPR=="PKSim") {
    
    sim1<-loadSimulation("pkmlFiles/Pregnant_simulation_PKSim.pkml", loadFromCache = FALSE)
    
    for (i in 2:ncol(gestationaPhysio)){
      param2change15w<-getParameter(gestationaPhysio[1,i], sim1)
      setParameterValues(param2change15w, as.numeric(gestationaPhysio[2,i]))
    }
    
  } else if  (individual=="GW15"&partitionQSPR=="Poulin") {
    
    sim1<-loadSimulation("pkmlFiles/Pregnant_simulation_Poulin.pkml", loadFromCache = FALSE)
    
    for (i in 2:ncol(gestationaPhysio)){
      param2change15w<-getParameter(gestationaPhysio[1,i], sim1)
      setParameterValues(param2change15w, as.numeric(gestationaPhysio[2,i]))
    }
    
  } else if  (individual=="GW24"&partitionQSPR=="Rodger_Rowland") {  
    
    sim1<-loadSimulation("pkmlFiles/Pregnant_simulation_R&R.pkml", loadFromCache = FALSE)
    
    for (i in 2:ncol(gestationaPhysio)){
    param2change24w<-getParameter(gestationaPhysio[1,i], sim1)
    setParameterValues(param2change24w, as.numeric(gestationaPhysio[3,i]))
    }
    
  } else if  (individual=="GW24"&partitionQSPR=="Schmitt") {  
    
    sim1<-loadSimulation("pkmlFiles/Pregnant_simulation_Schmitt.pkml", loadFromCache = FALSE)
    
    for (i in 2:ncol(gestationaPhysio)){
      param2change24w<-getParameter(gestationaPhysio[1,i], sim1)
      setParameterValues(param2change24w, as.numeric(gestationaPhysio[3,i]))
    }  
    
  } else if  (individual=="GW24"&partitionQSPR=="PKSim") {  
    
    sim1<-loadSimulation("pkmlFiles/Pregnant_simulation_PKSim.pkml", loadFromCache = FALSE)
    
    for (i in 2:ncol(gestationaPhysio)){
      param2change24w<-getParameter(gestationaPhysio[1,i], sim1)
      setParameterValues(param2change24w, as.numeric(gestationaPhysio[3,i]))
    }  
    
  } else if  (individual=="GW24"&partitionQSPR=="Poulin") {  
    
    sim1<-loadSimulation("pkmlFiles/Pregnant_simulation_Poulin.pkml", loadFromCache = FALSE)
    
    for (i in 2:ncol(gestationaPhysio)){
      param2change24w<-getParameter(gestationaPhysio[1,i], sim1)
      setParameterValues(param2change24w, as.numeric(gestationaPhysio[3,i]))
    }  
    
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
  #Dose<-getParameter("Applications|Daily ingestion|Dissolved formulation|Application_1|ProtocolSchemaItem|DosePerBodyWeight", sim1)
  #setParameterValues(Dose, Dose_mg_kg, units = "mg/kg")
  
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
  if (individual=="6_months"|individual=="2_weeks"){
      ####STORE DATA AND FIND CMAX####
      #Open list for saving data
      batchResList<-list()
      #open table for Cmax and Tmax
      tableCmax<-data.frame(matrix(ncol=4,nrow=nChemicals))
      ###UNITS####
      colnames(tableCmax)<-c("Cmax_Plasma_umol_L","Tmax_plasma_min","Cmax_Brain_umol_L","Tmax_brain_min")
      rownames(tableCmax)<-input_physchem[,1]
      
      for (j in 1:nChemicals){
        
      outputValues1<-getOutputValues(results[[1]][[j]])  
      batchResList[[j]]<-data.frame("Time-min"=outputValues1$data$Time,
                                   "VenousPlasma-umol/L"=outputValues1$data$`Organism|PeripheralVenousBlood|Test_Chemical|Plasma (Peripheral Venous Blood)`,
                                   "Brain-umol/L"=outputValues1$data$`Organism|Brain|Test_Chemical|Tissue`)
      
      tableCmax[j,1]<-max(batchResList[[j]]$VenousPlasma.umol.L)
      tableCmax[j,2]<-batchResList[[j]]$Time.min[which(batchResList[[j]]$VenousPlasma.umol.L==tableCmax[j,1])]
      tableCmax[j,3]<-max(batchResList[[j]]$Brain.umol.L)
      tableCmax[j,4]<-batchResList[[j]]$Time.min[which(batchResList[[j]]$Brain.umol.L==tableCmax[j,3])]
      }
  }else if (individual=="GW15"|individual=="GW24"){
    ####STORE DATA AND FIND CMAX####
    #Open list for saving data
    batchResList<-list()
    #open table for Cmax and Tmax
    tableCmax<-data.frame(matrix(ncol=6,nrow=nChemicals))
    ###UNITS####
    colnames(tableCmax)<-c("Cmax_Maternal_plasma_umol_L","Tmax_Maternal_plasma_min",
                           "Cmax_Fetus_plasma_umol_L","Tmax_Fetus_plasma_min",
                           "Cmax_Fetus_umol_L","Tmax_Fetus_min")
    
    rownames(tableCmax)<-input_physchem[,1]
    
    for (j in 1:nChemicals){
      
      outputValues1<-getOutputValues(results[[1]][[j]])  
      batchResList[[j]]<-data.frame("Time-min"=outputValues1$data$Time,
                                    "MaternalVenousPlasma-umol/L"=outputValues1$data$`Organism|PeripheralVenousBlood|Test_Chemical|Plasma (Peripheral Venous Blood)`,
                                    "FetusVenousPlasma-umol/L"=outputValues1$data$`Organism|Fetus|Plasma|Test_Chemical|Concentration in container`,
                                    "Fetus-umol/L"=outputValues1$data$`Organism|Fetus|Test_Chemical|Tissue`)
      
      
      tableCmax[j,1]<-max(batchResList[[j]]$MaternalVenousPlasma.umol.L)
      tableCmax[j,2]<-batchResList[[j]]$Time.min[which(batchResList[[j]]$MaternalVenousPlasma.umol.L==tableCmax[j,1])]
      tableCmax[j,3]<-max(batchResList[[j]]$FetusVenousPlasma.umol.L)
      tableCmax[j,4]<-batchResList[[j]]$Time.min[which(batchResList[[j]]$FetusVenousPlasma.umol.L==tableCmax[j,3])]
      tableCmax[j,5]<-max(batchResList[[j]]$Fetus.umol.L)
      tableCmax[j,6]<-batchResList[[j]]$Time.min[which(batchResList[[j]]$Fetus.umol.L==tableCmax[j,5])]

    }
    
  }
  
  return(list("tableCmax"=tableCmax,"batchResList"=batchResList))
}
