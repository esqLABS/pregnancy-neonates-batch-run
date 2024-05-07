
library(ospsuite)

# We load the pkml for which the batches will be created
sim1 <- loadSimulation("6_month_simulation.pkml", loadFromCache = FALSE)


#Get parameters location
# tree <- getSimulationTree(sim1)
# tree$Test_Chemical

# define the list of parameter that will be varied between the runs.
# For the first batch, we will vary 2 parameters: 
#Lipophilicity, MW , Water solubility, Fu, Permeability and hepatic clearance
parameterPaths <- c("Test_Chemical|Fraction unbound (plasma)",
                    "Test_Chemical|Lipophilicity",
                     "Test_Chemical|Solubility at reference pH",
                     "Test_Chemical|Specific intestinal permeability (transcellular)",
                     "Test_Chemical-Total Hepatic Clearance-database|Specific clearance",
                    "Test_Chemical|Molecular weight",
                    "Test_Chemical|F",
                    "Test_Chemical|I",
                    "Test_Chemical|Cl",
                    "Test_Chemical|Br")

#If I want to see the value of the parameter
#getParameter("Test_Chemical|Specific intestinal permeability (transcellular)", sim1)

# define a first simulation batch
simBatch <- createSimulationBatch(simulation = sim1, parametersOrPaths = parameterPaths)

#load the chemicals_input parameters
input_physchem<-read.csv("test_batch.csv")
#from NA to 0
input_physchem[is.na(input_physchem)]<-0

#Check if imported correctly
#View(input_physchem)

#the number of parameters to vary for each batch
#Needs to correspond to the vector of parameterPaths
for (i in 1:nrow(input_physchem)){
simBatch$addRunValues(parameterValues = c(input_physchem[i,2],input_physchem[i,3],input_physchem[i,4],
                                          input_physchem[i,5],input_physchem[i,6],input_physchem[i,7],
                                          input_physchem[i,8],input_physchem[i,9],input_physchem[i,10],
                                          input_physchem[i,11]))
}

results <- runSimulationBatches(simBatch)


#get the name of values
print(names(unlist(results)))
#Note:The enqueued run values are cleared after calling runSimulationBatches(),
#so executing the run again would result in an empty results list.


outputValues1<-getOutputValues(results[[1]][[1]])  
########ERROR somewhere values sim are really low################
View(outputValues1$data)

simulationResultsToDataFrame(res[[1]])

#Get Cmax in venous plasma and brain
max(sim_df$simulationValues)