# Acute exposure batch PBK simulations for pregnancy and neonates

## Project description

<p align="justify">

This project aim is to run acute exposure in PK-Sim standard PBK model with several<br />
chemicals in batch for neonates (2 weeks and 6 months)and pregnancy (GW 15 and 24) for oral exposure.   
The project started with a collaboration with INOTIV where simulations were compared with 
httk and simulation plus.
This comparison resulted in the publication: 
"Comparison of physiologically based pharmacokinetic modeling platforms for developmental neurotoxicity in vitro to in vivo extrapolation "

In this publication, it was identified that, especially when using logKow as input for lipophilicity, <br />
chemical with very low values (<-1) and very high (>4.5) are the ones where PK-Sim simulations <br />
are more distinct from the other softwares.

For the very polar chemicals it was identified that PK-Sim is underpredicting intestinal and<br />
possibly tissues permeability (e.g. methotrexate)
Hence we are currently evaluating the use of in vitro or other QSARs for parameterizing the <br />
permeability of these chemicals. 

For the very lipophilic chemicals it is not clear which PBK platform leads to best predictions.
But we are also working on improving a workflow for parameterizing PK-Sim PBK models for highly lipophilic chemicals.

These improvements on the PBK model parameterization are being developed in the ONTOX project and will be added here. 

## description code
For the chemicals it is only needed the MW, logP, Fu, SMILES and ionization (pKa nad type ionization). <br />
A example of a file with the physicochemical properties of the chemicals is : test/test_batch_2.
The file "Run batch function.R" contains the code to run the simulations and where options relative
to the dose, partition coefficient and individuals can be selected. 
It cna also be selected how Oral permeability and tissue permeability values are 
as calculated. By default these parameters are calculated with the OSP internal QSAR. 
We added options to run with a very high oral permeability and/or very high tissue permeability. 
Th aim is that we can run the simulatiosn without permeability being a limited rate.
There are options to run the different QSPRs for tissue partitioning: 
 
If you have a new chemical list you want to run, design the csv identically to test_batch_2 and replace
the name of the new chemicals file in line 8 of the "Neonate and Pregnancy sim function.R".




