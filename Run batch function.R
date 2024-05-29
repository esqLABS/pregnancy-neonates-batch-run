#Code to run the neonates and prgeanancy for 21mg/kg for 24 h and gets the Cmax
#Author: Susana P

#get the function
source("Neonate and Pregnancy sim function.R")

#Run the simulations and get the Cmax and Tmax---------------------------------
#you can select the PBK model 6_months,2_weeks, GW15, GW24
#You can also select the Partition QSAR: Rodger_Rowland, Schmitt, PKSim and Poulin
#Dose_mg_kg is the dose in mg/kg 
#highResol and lowResol are the resolution of the solver in points per minute
#lowResol is the resolution in the 2 first hours and low Resol is for the remaining time

runSimulation_Poulin<-Run_batch(individual="GW24",partitionQSPR="PKSim",
                         Dose_mg_kg=1,highResol=0.33,lowResol=0.07)

#if you want to see the Cmax table
View(runSimulation_Poulin$tableCmax)


#If you want to check a plot for neonates--------------------------------------
chemical2plot<-"B"
nrChemical<-which(input_physchem[,"Chemical"]==chemical2plot)
### CHANGE SCALE TO LOG###
plot(x=runSimulation$batchResList[[nrChemical]]$Time,
     y=runSimulation$batchResList[[nrChemical]]$Brain.umol.L,type="l",
     xlab="Time in min",
     ylab="Concentration in umol/L",
     log='y',ylim=c(0.001,1.5))
lines(x=runSimulation$batchResList[[nrChemical]]$Time,
      y=runSimulation$batchResList[[nrChemical]]$VenousPlasma.umol.L,col="red")
legend(x = "bottomright",          # Position
       legend = c("brain", "plasma"),  # Legend texts
       lty = c(1, 2),           # Line types
       col = c("black", "red"),           # Line colors
       lwd = 2)                 # Line width


#If you want to check a plot for pregnancy-------------------------------------
chemical2plot<-"B"
nrChemical<-which(input_physchem[,"Chemical"]==chemical2plot)
### CHANGE SCALE TO LOG###
plot(x=runSimulation$batchResList[[nrChemical]]$Time,
     y=runSimulation$batchResList[[nrChemical]]$MaternalVenousPlasma.umol.L,type="l",
     xlab="Time in min",
     ylab="Concentration in umol/L",
     log='y',ylim=c(0.005,7))
lines(x=runSimulation$batchResList[[nrChemical]]$Time,
      y=runSimulation$batchResList[[nrChemical]]$FetusVenousPlasma.umol.L,col="red")
lines(x=runSimulation$batchResList[[nrChemical]]$Time,
      y=runSimulation$batchResList[[nrChemical]]$Fetus.umol.L,col="blue")
legend(x = "bottomright",          # Position
       legend = c("maternal plasma","fetus plasma","fetus"),  # Legend texts
       lty = c(1, 2),           # Line types
       col = c("black", "red","blue"),           # Line colors
       lwd = 2)                 # Line width
