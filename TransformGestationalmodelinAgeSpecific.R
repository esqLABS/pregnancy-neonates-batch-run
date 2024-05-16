#Make pregnancy gestational weeks models

#Load generic PBK simulation
sim<-loadSimulation("pkmlFiles/Pregnant_simulation_PKSim.pkml", loadFromCache = FALSE)

#Load csv with changes physiolical parametes
tree <- getSimulationTree(sim)

gw15weeks<-sim
gw24weeks<-sim

# Can it read the path to the pareameter from the csv?
gestationaPhysio<-read.csv("Gestational physiological parameters.csv")

for (i in 2:ncol(gestationaPhysio)){
param2change15w<-getParameter(gestationaPhysio[1,i], gw15weeks)
param2change24w<-getParameter(gestationaPhysio[1,i], gw24weeks)
setParameterValues(param2change15w, as.numeric(gestationaPhysio[2,i]))data:image/png;base64,iVBORw0KGgoAAAANSUhEUgAAABIAAAASCAYAAABWzo5XAAAAWElEQVR42mNgGPTAxsZmJsVqQApgmGw1yApwKcQiT7phRBuCzzCSDSHGMKINIeDNmWQlA2IigKJwIssQkHdINgxfmBBtGDEBS3KCxBc7pMQgMYE5c/AXPwAwSX4lV3pTWwAAAABJRU5ErkJggg==
setParameterValues(param2change24w, as.numeric(gestationaPhysio[3,i]))
}