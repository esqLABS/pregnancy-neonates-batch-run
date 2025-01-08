#Introduction to project

This project aim is to run several chemicals in batch for neonates (2 weeks and 6 months)<br /> 
and pregnancy (GW 15 and 24) for oral exposure.   
The project started with a collaboration with INOTIV where simulations were compared with <br />
httk and simulation plus.
This comparison resulted in the publication: ""
It was identified that, especially when using logKow as input for lipophilicity,
chemical with very low values (<-1) and very high (>4.5) are the ones where PK-Sim simulations <br />
are more distinct from the other softwares.

For the very polar chemicals it was identified that indeed PK-Sim is underpredicting intestinal and<br />
possibly tissues permeability.
Hence we are currently evaluating the use of in vitro or other QSARs for parameterizing the <br />
permeability of these chemicals. 

For the very lipophilic chemicals it is not clear which PBK platform leads to best predictions.
But we are also working on improving a workflow for parameterizing PK-Sim PBK models for highly lipophilic chemicals.

These improvements on the PBK model parameterization are being developed in the ONTOX project and will be added here. 

##description code
For the chemicals it is only needed the MW, logP, Fu, SMILES and ionization (pKa nad type ionization). <br />
A example of a file with the physicochemical properties of the chemicals is :  test_batch_2.
Oral permeability and tissue permeability values are as calculated by default with the OSP internal QSAR. 
There are options to have these values set very high so that the simulations are not dependent on permeability. 

There are options to run the different QSPRs for tissue partitioning: 
PK-Sim default, Rodgers and Rowland, Schmitt and Poulin. 

The file xx in the batch simulations sets the simulations for all the options. 
Then to run the simulations you can use xx file :
there you select the partition coefficient and type individual (e.g. neonate, pregnant women) and run the simulations.
 


