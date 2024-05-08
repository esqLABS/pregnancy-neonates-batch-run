
library(ospsuite)

# We load the pkml for which the batches will be created
sim1 <- loadSimulation("6_month_simulation.pkml", loadFromCache = FALSE)

####CHECK FIRST STRUCTURE####--------------------------------------------------
#Get parameters location
 tree <- getSimulationTree(sim1)
 tree$Test_Chemical

#If I want to see the value of the parameter and units
Fu<-getParameter("Test_Chemical|Fraction unbound (plasma, reference value)", sim1)

Lip<-getParameter("Test_Chemical|Lipophilicity", sim1)
 
Sol<-getParameter("Test_Chemical|Solubility at reference pH",sim1)
 
oralabs<-getParameter("Test_Chemical|Specific intestinal permeability (transcellular)", sim1)

clearance<-getParameter("Test_Chemical-Total Hepatic Clearance-database|Specific clearance", sim1)

MW<-getParameter("Test_Chemical|Molecular weight", sim1)

F_pa<-getParameter("Test_Chemical|F", sim1)

I_pa<-getParameter("Test_Chemical|I", sim1)

Cl_pa<-getParameter("Test_Chemical|I", sim1)

Br_pa<-getParameter("Test_Chemical|Br", sim1)


#Make just one simulation for the original
res <- runSimulations(sim1)
outputValues<-getOutputValues(res[[1]])

####CONSTRUCT BATCH####----------------------------------------------------------
# define the list of parameter that will be varied between the runs.
parameterPaths <- c("Test_Chemical|Fraction unbound (plasma, reference value)",
                    "Test_Chemical|Lipophilicity",
                    "Test_Chemical|Solubility at reference pH",
                    "Test_Chemical|Specific intestinal permeability (transcellular)",
                    "Test_Chemical-Total Hepatic Clearance-database|Specific clearance",
                    "Test_Chemical|Molecular weight",
                    "Test_Chemical|F",
                    "Test_Chemical|I",
                    "Test_Chemical|Cl",
                    "Test_Chemical|Br")


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
simBatch$addRunValues(parameterValues = c(input_physchem[i,2],input_physchem[i,3],input_physchem[i,4],
                                          input_physchem[i,5],input_physchem[i,6],input_physchem[i,7],
                                          input_physchem[i,8],input_physchem[i,9],input_physchem[i,10],
                                          input_physchem[i,11]))
}

###SIMULATIONS###
results <- runSimulationBatches(simBatch)
#get the name of values
print(names(unlist(results)))
#Note:The enqueued run values are cleared after calling runSimulationBatches(),
#so executing the run again would result in an empty results list.

####STORE DATA AND FIND CMAX####
#Open list for saving data
batchResList<-list()
#open table for Cmax and Tmax
table6month<-data.frame(matrix(ncol=4,nrow=nChemicals))
colnames(table6month)<-c("Cmax_Plasma","Tmax_plasma","Cmax_Brain","Tmax_brain")
rownames(table6month)<-rownames(input_physchem)

for (j in 1:nChemicals){
  
outputValues1<-getOutputValues(results[[1]][[j]])  
batchResList[[j]]<-data.frame("Time-min"=outputValues1$data$Time,
                             "VenousPlasma-umol/L"=outputValues1$data$`Organism|PeripheralVenousBlood|Test_Chemical|Plasma (Peripheral Venous Blood)`,
                             "Brain-umol/L"=outputValues1$data$`Organism|Brain|Test_Chemical|Tissue`)

table6month[j,1]<-max(batchResList[[j]]$VenousPlasma.umol.L)
table6month[j,2]<-batchResList[[j]]$Time.min[which(batchResList[[j]]$VenousPlasma.umol.L==table6month[j,1])]
table6month[j,3]<-max(batchResList[[j]]$Brain.umol.L)
table6month[j,4]<-batchResList[[j]]$Time.min[which(batchResList[[j]]$Brain.umol.L==table6month[j,3])]
}



#If you want to check a plot for a specific chemical
#chemical2plot<-...
plot(x=batchResultsList$Time,y=log(batchResultsList$Brain),type="l",
     xlab="Time in min",
     ylab="Concentration un umol/L")
lines(x=batchResultsList$Time,y=log(batchResultsList$VenousBlood),col="red")
legend(x = "bottomright",          # Position
       legend = c("brain", "plasma"),  # Legend texts
       lty = c(1, 2),           # Line types
       col = c("black", "red"),           # Line colors
       lwd = 2)                 # Line width


View(outputValues1$data)

#Make dataframe with chemicals and Cmax in venous blood and brain and maybe Tmax aswell
######

#Get Cmax in venous plasma and brain
max(simRes[which(path==Organism|Brain|Test_Chemical|Tissue),]$simulationValues)