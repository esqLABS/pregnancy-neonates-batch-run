# Acute exposure batch PBK simulations for pregnancy and neonates

## Project description

<p align="justify">

This project aim is to run acute exposure in PK-Sim standard PBK model with several
chemicals in batch for neonates (2 weeks and 6 months)and pregnancy (GW 15 and 24) for oral exposure.   
The project started with a collaboration with INOTIV where simulations were compared with 
httk and Simulation Plus.
This comparison resulted in the publication: 
"Comparison of physiologically based pharmacokinetic modeling platforms for developmental neurotoxicity in vitro to in vivo extrapolation "

In this publication, it was identified that, especially when using logKow as input for lipophilicity,
chemical with very low values (<-1) and very high (>4.5) are the ones where PK-Sim simulations 
are more distinct from the other softwares.

For the very polar chemicals it was identified that PK-Sim model using the internally calculated Pint is underpredicting intestinal and
possibly tissues permeability (e.g. methotrexate).
Using Papp values from Caco-2  does seem to drastically improve predictions for methotrexate.
Hence we are currently evaluating the use of Papp values or other QSARs for parameterizing Pint for  other very polar chemicals. 

For the very lipophilic chemicals it is not clear which PBK platform leads to best predictions.
But we are also working on improving a workflow for parameterizing PK-Sim PBK models for highly lipophilic chemicals.

These improvements on the PBK model parameterization are being developed in the ONTOX project and will be added here. 

## Description of the project

The file [Neonate and Pregnancy sim function.R](https://github.com/esqLABS/pregnancy-neonates-batch-run/blob/master/Neonate%20and%20Pregnancy%20sim%20function.R) has the workflow to make the simulations for the different ages, partition coefficients and get the different outputs.
If file is changed we advised running the file [Run_different_models.R](https://github.com/esqLABS/pregnancy-neonates-batch-run/blob/master/test_files/Run_different_models.R) to make sure there is no errors popping with any of the selections. 

From the chemicals it is only needed the MW, logP, Fu, hepatic clearance (/min) SMILES and ionization (pKa and type ionization). 
A example of a file with the physicochemical properties of the chemicals is : [test_batch_2](https://github.com/esqLABS/pregnancy-neonates-batch-run/tree/master#:~:text=csv%20identically%20to-,test_batch_2,-and%20replace%20the).
If you have a new chemical list you want to run, design the .csv identically to this test_2.csv and replace
the name of the new chemicals file in line 8 of the "Neonate and Pregnancy sim function.R".

The file [Run batch function.R](https://github.com/esqLABS/pregnancy-neonates-batch-run/blob/master/Run%20batch%20function.R)
contains the code to run the simulations and where options relative
to the dose, partition coefficient and individuals can be selected. 
It can also be selected how oral permeability (Pint) and tissue permeability values are 
as calculated. By default these parameters are calculated with the OSP internal QSAR. 
We added options to run with a very high oral permeability and/or very high tissue permeability. 
The aim is that we can run the simulations without permeability being a limited rate.

The codes to run httk files are contained in the folder :
[httk files](https://github.com/esqLABS/pregnancy-neonates-batch-run/tree/master/httk%20files)

Some comparison of the 6-mnoths old simulations from the different softwares in the folder [analysis for paper](https://github.com/esqLABS/pregnancy-neonates-batch-run/tree/master/analysis%20for%20paper)



 





