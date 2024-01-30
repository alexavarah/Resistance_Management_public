# This file describes the various R and Rmd scripts used for the manuscript "Managing herbicide resistance doesn’t pay, but acting pre-emptively does".

The files carry out an analysis of economic and productivity outcomes from various crop management strategies in the context of herbicide-resistant Black-grass.  
Details and code for simulating weed density are given in a separate GitHub repo - see https://github.com/Rgoodsell/simulate_management  
  
N.B. The function in file '01. Load ECOMOD' is automatically called in file '02. Run ECOMOD'. 
All other scripts must be run in numerical order.

Script details:  
  
01. ECOMOD  
The ECOMOD function is automatically called in file '02. Run ECOMOD'.  
  
02. Run ECOMOD  
Calls the ECOMOD function from the previous file and runs the management data through ECOMOD.  
  
03. Tidy data  
This tidies the output from ECOMOD and from the density estimation modelling.  
  
04. Density and resistance state distribution  
This contains summaries of the numbers of fields in each county and region. These values are in Table S3 of the Supplementary Info.  
It also explains the process of choosing the cut-off points for 'high' vs 'low' density and resistance categories.  
Line 381 gives the percentage of fields that fall in each density-resistance category (these are presented in the methods - see section 2.1 'Defining initial conditions').  
The script also creates output file 'cerealarea_by_localauthority_and_denres_3DR_2021.csv' which is used in the scaling up procedure in the analysis.  
  
05. Analysis  
This contains the analysis for most of the manuscript and for Tables S5 to S8.  
It doesn't contain Figure 1 (this is created in script '06. Map.R').  
  
06. Map  
This script creates Figure 1.  
  
07. Sensitivity Analysis - ECOMOD  
This script contains 4 versions of ECOMOD: 2 with the lower yield penalties, one each with yield set to 'estimate' and 'actual'; and 2 with the higher yield penalties, one each with yield set to 'estimate' and 'actual.'  
Do not load ECOMOD from this script: it's called from script '08. Sensitivity Analysis - run ECOMOD'.  
  
08. Sensitivity Analysis - run ECOMOD
This script gives the upper and lower limits of the yield penalty used in the sensitivity analysis. This info is given in Table S2 in the Supplementary Info.  
The script also runs ECOMOD with the different yield penalties.  

09. Sensitivity Analysis - Tidy data  
This tidies the output from ECOMOD sensitivity analyses.  
  
10. Sensitivity Analysis - Analyse  
This script calculates weighted values and plots gross profit and wheat yield from sensitivity analyses. The script creates Supplementary Figure S5.  
  
