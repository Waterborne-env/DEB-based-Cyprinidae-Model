DEB-based model for Cyprinidae: How to

Author: Chiara Accolla, Amelie Schmolke, Andy Jacobson, Colleen Roy, Valery E. Forbes, Richard Brain, 
Nika Galic

Last edit: May 16 2022

Disclaimer

The software and associated files uploaded in this repository were used to generate the 
results published in:
Modeling pesticide effects on multiple threatened and endangered Cyprinid fish species: the role of life-
history traits and ecology

This software and associated files are provided "as is" with the sole purpose to allow the 
reproduction of the published results without any warranties of performance or fitness for 
any other purpose.
Trace documentation
TRACE_Cyprinidae.pdf

Data
Data for the effect sub-models are in the folder Data, which contains the raw data, the files used for the 
GUTS calibrations (direct effects) and for the calculation of the SSD curve (indirect effects).

Netlogo code
Cyprinidae_population_model.nlogo (NetLogo 6.2 was used)

R code for data analysis
Analysis_of_data_Cyprinidae.R

Model input files
Temperature
1.	LCR_temperature.txt   for Humpback chub and Spikedace
2.	TS_temperature.txt  for Topeka shiner
3.	DR_temperature.txt  for Devils River minnow

Species-specific parameters
1.	Parameters-HC.txt  for Humpback chub
2.	Parameters-SD.txt for Spikedace
3.	Parameters-TS.txt  for Topeka shiner
4.	Parameters-DRM.txt  for Devils River minnow
5.	Common-param.txt for shared DEB parameters

Exposure data and effect-module parameters
1.	Hydroxy-TX.csv for the exposure profile
2.	Chemical-param-GUTS.txt for GUTS parameters for the first module (E1). Contain the Weibull 
parameters for the module E4, too
3.	Chemical-param-TKTD.txt for TKTD parameters for the effects on egg production (E2 module).
4.	Chemical-param-GUTS-hatching.txt  for GUTS parameters for the hatching-success module (E3).

Run the model

Software requirements
NetLogo 6.2 

In the Netlogo Interface:
1.	Select the species with the chooser "species" (e.g., Humpback chub)
2.	Write the corresponding file name in the input window "Species-parameters" (e.g., Parameters-
HC.txt)
3.	Write the corresponding file names in the input windows (these files are the same regardless of 
the species):
*	"CommonParameters": Common-param.txt
*	"Chemical-Parameters": Chemical-param-GUTS.txt
*	"Chemical-Parameters-Hatching": Chemical-param-GUTS-hatching.txt  
*	"Chemical-Parameters-TKTD": Chemical-param-TKTD.txt
*	"ExposureFile": Hydroxy-TX.csv
4.	Define the time steps: timestep is equal to 1 for all species but the Devils River minnow 
(timestep = 24); timestep2 is equal to 24.
5.	The model has been run with "forcing_variable" = yes, "density-dependence" = yes, and "food-
dynamics" = sinusoidal.
6.	Select which effects should be turned on. For any effect to be represented, the switch 
"chemical" has to be = yes.
7.	BehaviorSpace: for the study, the following variables have been read and saved:
*	count turtles with [U_H > U_H^b]
*	count turtles with [U_H > U_H^p]
*	sum [weight] of turtles with [U_H > U_H^b]
*	w-turtles

Analyze the data using the R code:
1.	Set the right directory in which the .csv files created by BehaviorSpace are saved.
2.	Read the tables
The R code is setup to reproduce the different experiments presented in the paper:
a.	Analyze the effects of different exposure magnification factors (EMF).
b.	Add-up the different effect submodules.
c.	Create the figures for the population dynamics without exposure.
d.	Test the effects of stochastic droughts or density dependence on the dynamics of unexposed 
populations.
