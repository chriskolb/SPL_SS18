
[<img src="https://github.com/QuantLet/Styleguide-and-FAQ/blob/master/pictures/banner.png" width="888" alt="Visit QuantNet">](http://quantlet.de/)

```yaml

Name of Project : Survival Analysis and Homeownership - Determinants of the Transition Time into Homeownership

Published in: Statistical Programming Languages 

Description : () This repository contains 
                  1. the full source code 
		     master.R, path.R, packages.R, datawrangling.R, functions.R, descriptives.R, analysis.R
	          2. the cleaned data set datfinal.RDA
	          3. Quantlet folders 
	          4. the report (+tex files)
	          5. the presentation (+tex files)
		  
	      () All data files and scripts must be in the same folder
	      
	      () path.R needs to be specified by the user
	      
	      () The code should be run in this order
	          1. modify and run path.R
                  2. run packages.R
	          3. run datawrangling.R
	          4. run functions.R
	          5. run descriptives.R
	          6. run analysis.R
		  
	      () datawrangling.R produces datfinal.RDA and can only be run on the original SOEP files
	         pequiv.csv, pgen.csv, ppfadl.csv, hgen.csv, hbrutto.csv
		 
	      () If the .csv files are not available, skip datawrangling.R
	         and directly continue to functions.R
		 
	      () To check for errors in our code, use master.R to run all scripts consecutively
	     

Author : Alice Drube, Konstantin Göbler, Chris Kolb, Richard v. Maydell

```
