#Code to run the neonates and prgeanancy for 21mg/kg for 24 h and gets the Cmax
#Author: Susana

#get the function
source("Neonate and Pregnancy sim function.R")

#Run the simulations and get the Cmax and Tmax---------------------------------
#you can select the PBK model 6months,2weeks, GW15, GW24
#You can also select the Partition QSAR: Rodger_Rowland, Schmitt, PKSim and Poulin
runSimulation<-Run_batch("GW15","Rodger_Rowland")

#if you want to see the Cmax table
View(runSimulation$tableCmax)

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
