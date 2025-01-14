
library(ospsuite)
library(tidyverse)

#load the chemicals_input parameters
input_physchem<-read.csv("test_batch_2.csv")
nChemicals<-nrow(input_physchem)
#function 
Run_batch_Quick2<-function (individual,partitionQSPR,Dose_mg_kg,highResol,lowResol){
  
  ###Load pregnancy gestational age speicifc physiology
  gestationaPhysio<-read.csv("Gestational physiological parameters.csv")
  
  # We load the pkml for which the batches will be created
  if (individual=="6_months"&partitionQSPR=="Rodger_Rowland") {
    sim1 <- loadSimulation("pkmlFiles and physiological db/6_month_simulation_R&R.pkml", loadFromCache = FALSE)
    
  } else if  (individual=="6_months"&partitionQSPR=="Schmitt") {
    sim1 <- loadSimulation("pkmlFiles and physiological db/6_month_simulation_Schmitt.pkml", loadFromCache = FALSE)
    
  } else if  (individual=="6_months"&partitionQSPR=="PKSim") {
    sim1 <- loadSimulation("pkmlFiles and physiological db/6_month_simulation_PKSim.pkml", loadFromCache = FALSE)
    
  } else if  (individual=="6_months"&partitionQSPR=="Poulin") {
    sim1 <- loadSimulation("pkmlFiles and physiological db/6_month_simulation_Poulin.pkml", loadFromCache = FALSE)
    
  } else if  (individual=="2_weeks"&partitionQSPR=="Rodger_Rowland") {
    sim1 <- loadSimulation("pkmlFiles and physiological db/2_weeks_simulation_R&R.pkml", loadFromCache = FALSE)
    
  } else if  (individual=="2_weeks"&partitionQSPR=="Schmitt") {
    sim1 <- loadSimulation("pkmlFiles and physiological db/2_weeks_simulation_Schmitt.pkml", loadFromCache = FALSE)
    
  } else if  (individual=="2_weeks"&partitionQSPR=="PKSim") {
    sim1 <- loadSimulation("pkmlFiles and physiological db/2_weeks_simulation_PKSim.pkml", loadFromCache = FALSE)
    
  } else if  (individual=="2_weeks"&partitionQSPR=="Poulin") {
    sim1 <- loadSimulation("pkmlFiles and physiological db/2_weeks_simulation_Poulin.pkml", loadFromCache = FALSE)
    
  } else if  (individual=="GW15"&partitionQSPR=="Rodger_Rowland") {
    sim1<-loadSimulation("pkmlFiles and physiological db/Pregnant_simulation_R&R.pkml", loadFromCache = FALSE)
    
    #Change the physiology to a GW15 physiology
    for (i in 2:ncol(gestationaPhysio)){
      param2change15w<-getParameter(gestationaPhysio[1,i], sim1)
      setParameterValues(param2change15w, as.numeric(gestationaPhysio[2,i]))
    }
    
  } else if  (individual=="GW15"&partitionQSPR=="Schmitt") {
    
    sim1<-loadSimulation("pkmlFiles and physiological db/Pregnant_simulation_Schmitt.pkml", loadFromCache = FALSE)
    
    for (i in 2:ncol(gestationaPhysio)){
      param2change15w<-getParameter(gestationaPhysio[1,i], sim1)
      setParameterValues(param2change15w, as.numeric(gestationaPhysio[2,i]))
    }
    
  } else if  (individual=="GW15"&partitionQSPR=="PKSim") {
    
    sim1<-loadSimulation("pkmlFiles and physiological db/Pregnant_simulation_PKSim.pkml", loadFromCache = FALSE)
    
    for (i in 2:ncol(gestationaPhysio)){
      param2change15w<-getParameter(gestationaPhysio[1,i], sim1)
      setParameterValues(param2change15w, as.numeric(gestationaPhysio[2,i]))
    }
    
  } else if  (individual=="GW15"&partitionQSPR=="Poulin") {
    
    sim1<-loadSimulation("pkmlFiles and physiological db/Pregnant_simulation_Poulin.pkml", loadFromCache = FALSE)
    
    for (i in 2:ncol(gestationaPhysio)){
      param2change15w<-getParameter(gestationaPhysio[1,i], sim1)
      setParameterValues(param2change15w, as.numeric(gestationaPhysio[2,i]))
    }
    
  } else if  (individual=="GW24"&partitionQSPR=="Rodger_Rowland") {  
    
    sim1<-loadSimulation("pkmlFiles and physiological db/Pregnant_simulation_R&R.pkml", loadFromCache = FALSE)
    
    for (i in 2:ncol(gestationaPhysio)){
      param2change24w<-getParameter(gestationaPhysio[1,i], sim1)
      setParameterValues(param2change24w, as.numeric(gestationaPhysio[3,i]))
    }
    
  } else if  (individual=="GW24"&partitionQSPR=="Schmitt") {  
    
    sim1<-loadSimulation("pkmlFiles and physiological db/Pregnant_simulation_Schmitt.pkml", loadFromCache = FALSE)
    
    for (i in 2:ncol(gestationaPhysio)){
      param2change24w<-getParameter(gestationaPhysio[1,i], sim1)
      setParameterValues(param2change24w, as.numeric(gestationaPhysio[3,i]))
    }  
    
  } else if  (individual=="GW24"&partitionQSPR=="PKSim") {  
    
    sim1<-loadSimulation("pkmlFiles and physiological db/Pregnant_simulation_PKSim.pkml", loadFromCache = FALSE)
    
    for (i in 2:ncol(gestationaPhysio)){
      param2change24w<-getParameter(gestationaPhysio[1,i], sim1)
      setParameterValues(param2change24w, as.numeric(gestationaPhysio[3,i]))
    }  
    
  } else if  (individual=="GW24"&partitionQSPR=="Poulin") {  
    
    sim1<-loadSimulation("pkmlFiles and physiological db/Pregnant_simulation_Poulin.pkml", loadFromCache = FALSE)
    
    for (i in 2:ncol(gestationaPhysio)){
      param2change24w<-getParameter(gestationaPhysio[1,i], sim1)
      setParameterValues(param2change24w, as.numeric(gestationaPhysio[3,i]))
    }  
    
  } else {warning("mistake in input model")}
  
  ####CHANGE DOSE###
  ###Add outputs###
  if (individual=="6_months"|individual=="2_weeks"){
    
    Dose<-getParameter("Applications|Daily ingestion|Dissolved formulation|Application_1|ProtocolSchemaItem|DosePerBodyWeight",sim1)
    brainKp<-getParameter("Neighborhoods|Brain_int_Brain_cell|Test_Chemical|Partition coefficient (intracellular/plasma)",sim1)
    adiposeKp<-getParameter("Neighborhoods|Fat_int_Fat_cell|Test_Chemical|Partition coefficient (intracellular/plasma)",sim1)
    liverKp<-getParameter("Neighborhoods|Periportal_int_Periportal_cell|Test_Chemical|Partition coefficient (intracellular/plasma)",sim1)
    pInt<-getParameter("Test_Chemical|Intestinal permeability (transcellular)", sim1)
    Permeability<-getParameter("Test_Chemical|Permeability",sim1)
    Fu<-getParameter("Test_Chemical|Fraction unbound (plasma)",sim1)
    massDrug<-getParameter("Test_Chemical|Total drug mass",sim1)
    addOutputs(c(brainKp,adiposeKp,liverKp,pInt,Permeability,Fu,massDrug),simulation = sim1)
    
  }else if (individual=="GW24"|individual=="GW15"){
    Dose<-getParameter("Applications|oral dose|Dissolved|Application_1|ProtocolSchemaItem|DosePerBodyWeight",sim1)
    fetusKp<-getParameter("Neighborhoods|Fetus_int_Fetus_cell|Test_Chemical|Partition coefficient (intracellular/plasma)",sim1)
    PlacentaKp<-getParameter("Neighborhoods|PlacentaMaternal_int_PlacentaMaternal_cell|Test_Chemical|Partition coefficient (intracellular/plasma)",sim1)
    adiposeKp<-getParameter("Neighborhoods|Fat_int_Fat_cell|Test_Chemical|Partition coefficient (intracellular/plasma)",sim1)
    liverKp<-getParameter("Neighborhoods|Periportal_int_Periportal_cell|Test_Chemical|Partition coefficient (intracellular/plasma)",sim1)
    pInt<-getParameter("Test_Chemical|Test_Chemical|Intestinal permeability (transcellular)", sim1)
    addOutputs(c(fetusKp,PlacentaKp,adiposeKp,liverKp,pInt),simulation = sim1)
    
  }else {}
  
  setParameterValues(Dose, Dose_mg_kg, units = "mg/kg")
  
  
  #To see the simulation steps
  clearOutputIntervals(simulation = sim1)
  
  addOutputInterval(simulation = sim1, startTime = 0, endTime = 120, 
                    resolution = highResol,
                    intervalName = "Simulation interval high resolution ")
  
  addOutputInterval(simulation = sim1, startTime = 120, endTime = 1440, 
                    resolution =  lowResol,
                    intervalName = "Simulation interval low resolution ")
  
  
  
  #timesteps<-sim1$outputSchema  #to see the schema
  ####CHECK FIRST STRUCTURE####
  #Get parameters location
  #tree <- getSimulationTree(sim1)
  #tree$Test_Chemical
  
  #If I want to see the value of the parameter and units
  # 
  # Fu<-getParameter("Test_Chemical|Fraction unbound (plasma, reference value)", sim1)
  # Lip<-getParameter("Test_Chemical|Lipophilicity", sim1)
  # Sol<-getParameter("Test_Chemical|Solubility at reference pH",sim1)
  # oralabs<-getParameter("Test_Chemical|Specific intestinal permeability (transcellular)", sim1)
  # clearance<-getParameter("Test_Chemical-Total Hepatic Clearance-database|Specific clearance", sim1)
  # MW<-getParameter("Test_Chemical|Molecular weight", sim1)
  
  ####CONSTRUCT BATCH####
  # define the list of parameter that will be varied between the runs.
  parameterPaths <- c("Test_Chemical|Fraction unbound (plasma, reference value)",
                      "Test_Chemical|Lipophilicity",
                      "Test_Chemical|Solubility at reference pH",
                      "Test_Chemical-Total Hepatic Clearance-database|Specific clearance",
                      "Test_Chemical|Molecular weight",
                      "Test_Chemical|Effective molecular weight",
                      "Test_Chemical|pKa value 0",
                      "Test_Chemical|pKa value 1",
                      "Test_Chemical|pKa value 2",
                      "Test_Chemical|Compound type 0",
                      "Test_Chemical|Compound type 1",
                      "Test_Chemical|Compound type 2",
                      "Test_Chemical|Intestinal permeability (transcellular)",
                      "Test_Chemical|Permeability")
  
  # define the simulation batch
  simBatch <- createSimulationBatch(simulation = sim1, parametersOrPaths = parameterPaths)
  
  #from NA to 0
  input_physchem[is.na(input_physchem)]<-0
  
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
    
    simBatch$addRunValues(parameterValues = c( input_physchem[i,"Fub"],
                                               input_physchem[i,"Lipophilicity"],
                                               input_physchem[i,"Solubility..pH.7..g.mL."],
                                               input_physchem[i,"Clearance..min."],
                                               input_physchem[i,"MW.kg.umol."],
                                               effective_mw,
                                               input_physchem[i,"pKa1"],
                                               input_physchem[i,"pKa2"],
                                               input_physchem[i,"pKa3"],
                                               input_physchem[i,"CompountType1"],
                                               input_physchem[i,"CompountType2"],
                                               input_physchem[i,"CompountType3"],
                                               0.1,10))
  }
  
  ###SIMULATIONS###
  results <- runSimulationBatches(simBatch)
  
  #Note:The enqueued run values are cleared after calling runSimulationBatches(),
  #so executing the run again would result in an empty results list.
  if (individual=="6_months"|individual=="2_weeks"){
    ####STORE DATA AND FIND CMAX####
    #Open list for saving data
    batchResList<-list()
    #open table for Cmax and Tmax
    tableCmax<-data.frame(matrix(ncol=9,nrow=nChemicals))
    ###UNITS####
    colnames(tableCmax)<-c("Cmax_Plasma_umol_L","Tmax_plasma_min",
                           "Cmax_Brain_umol_L","Tmax_brain_min",
                           "BrainK","FatK","LiverK","Pint","Vd_L/kg")
    rownames(tableCmax)<-input_physchem[,1]
    
    for (j in 1:nChemicals){
      #get table results
      outputValues1<-getOutputValues(results[[1]][[j]]) 
      #get PKcalculated parameters
      pkAnalysis <- calculatePKAnalyses(results = results[[1]][[j]])
      batchResList[[j]]<-data.frame("Time-min"=outputValues1$data$Time,
                                    "VenousPlasma-umol/L"=outputValues1$data$`Organism|PeripheralVenousBlood|Test_Chemical|Plasma (Peripheral Venous Blood)`,
                                    "Brain-umol/L"=outputValues1$data$`Organism|Brain|Test_Chemical|Tissue`,
                                    "BrainK"=outputValues1$data$`Neighborhoods|Brain_int_Brain_cell|Test_Chemical|Partition coefficient (intracellular/plasma)`,
                                    "FatK"=outputValues1$data$`Neighborhoods|Fat_int_Fat_cell|Test_Chemical|Partition coefficient (intracellular/plasma)`,
                                    "LiverK"=outputValues1$data$`Neighborhoods|Periportal_int_Periportal_cell|Test_Chemical|Partition coefficient (intracellular/plasma)`,
                                    "Pint"=outputValues1$data$`Test_Chemical|Intestinal permeability (transcellular)`,
                                    "Permeability"=outputValues1$data$`Test_Chemical|Permeability`,
                                    "Fu"=outputValues1$data$`Test_Chemical|Fraction unbound (plasma)`,
                                    "massDrug"=outputValues1$data$`Test_Chemical|Total drug mass`)
      
      
      tableCmax[j,1]<-max(batchResList[[j]]$VenousPlasma.umol.L)
      tableCmax[j,2]<-batchResList[[j]]$Time.min[which(batchResList[[j]]$VenousPlasma.umol.L==tableCmax[j,1])][1]
      tableCmax[j,3]<-max(batchResList[[j]]$Brain.umol.L)
      tableCmax[j,4]<-batchResList[[j]]$Time.min[which(batchResList[[j]]$Brain.umol.L==tableCmax[j,3])][1]
      tableCmax[j,5]<-batchResList[[j]]$BrainK[1]
      tableCmax[j,6]<-batchResList[[j]]$FatK[1]
      tableCmax[j,7]<-batchResList[[j]]$LiverK[1]
      tableCmax[j,8]<-batchResList[[j]]$Pint[1]
      tableCmax[j,9]<-pkAnalysis$pKParameterFor(
        quantityPath = "Organism|PeripheralVenousBlood|Test_Chemical|Plasma (Peripheral Venous Blood)",
        pkParameter = "Vd")$values
      
    }
  }else if (individual=="GW15"|individual=="GW24"){
    ####STORE DATA AND FIND CMAX####
    #Open list for saving data
    batchResList<-list()
    #open table for Cmax and Tmax
    tableCmax<-data.frame(matrix(ncol=12,nrow=nChemicals))
    ###UNITS####
    colnames(tableCmax)<-c("Cmax_Maternal_plasma_umol_L","Tmax_Maternal_plasma_min",
                           "Cmax_Fetus_plasma_umol_L","Tmax_Fetus_plasma_min",
                           "Cmax_Fetus_umol_L","Tmax_Fetus_min",
                           "fetusKp","placentaKp","FatK","LiverK", "Pint","Vd_L/kg")
    
    rownames(tableCmax)<-input_physchem[,1]
    
    for (j in 1:nChemicals){
      
      #get table results
      outputValues1<-getOutputValues(results[[1]][[j]]) 
      #get PKcalculated parameters
      pkAnalysis <- calculatePKAnalyses(results = results[[1]][[j]])
      batchResList[[j]]<-data.frame("Time-min"=outputValues1$data$Time,
                                    "MaternalVenousPlasma-umol/L"=outputValues1$data$`Organism|PeripheralVenousBlood|Test_Chemical|Plasma (Peripheral Venous Blood)`,
                                    "FetusVenousPlasma-umol/L"=outputValues1$data$`Organism|Fetus|Plasma|Test_Chemical|Concentration in container`,
                                    "Fetus-umol/L"=outputValues1$data$`Organism|Fetus|Test_Chemical|Tissue`,
                                    "fetusKp"=outputValues1$data$`Neighborhoods|Fetus_int_Fetus_cell|Test_Chemical|Partition coefficient (intracellular/plasma)`,
                                    "placentaKp"=outputValues1$data$`Neighborhoods|PlacentaMaternal_int_PlacentaMaternal_cell|Test_Chemical|Partition coefficient (intracellular/plasma)`,
                                    "fatK"=outputValues1$data$`Neighborhoods|Fat_int_Fat_cell|Test_Chemical|Partition coefficient (intracellular/plasma)`,
                                    "liverK"=outputValues1$data$`Neighborhoods|Periportal_int_Periportal_cell|Test_Chemical|Partition coefficient (intracellular/plasma)`,
                                    "pint"=outputValues1$dat$`Test_Chemical|Calculated specific intestinal permeability (transcellular)`)
      
      
      tableCmax[j,1]<-max(batchResList[[j]]$MaternalVenousPlasma.umol.L)
      tableCmax[j,2]<-batchResList[[j]]$Time.min[which(batchResList[[j]]$MaternalVenousPlasma.umol.L==tableCmax[j,1])][1]
      tableCmax[j,3]<-max(batchResList[[j]]$FetusVenousPlasma.umol.L)
      tableCmax[j,4]<-batchResList[[j]]$Time.min[which(batchResList[[j]]$FetusVenousPlasma.umol.L==tableCmax[j,3])][1]
      tableCmax[j,5]<-max(batchResList[[j]]$Fetus.umol.L)
      tableCmax[j,6]<-batchResList[[j]]$Time.min[which(batchResList[[j]]$Fetus.umol.L==tableCmax[j,5])][1]
      tableCmax[j,7]<-batchResList[[j]]$fetusKp[1]
      tableCmax[j,8]<-batchResList[[j]]$placentaKp[1]
      tableCmax[j,9]<-batchResList[[j]]$fatK[1]
      tableCmax[j,10]<-batchResList[[j]]$liverK[1]
      tableCmax[j,11]<-batchResList[[j]]$pint[1]
      tableCmax[j,12]<-pkAnalysis$pKParameterFor(
        quantityPath = "Organism|PeripheralVenousBlood|Test_Chemical|Plasma (Peripheral Venous Blood)",
        pkParameter = "Vd")$values
      
    }
    
  }
  
  return(list("tableCmax"=tableCmax,"batchResList"=batchResList))
}
