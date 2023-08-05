# bartSPEC
R code for "Adaptive Bayesian Sum of Trees Model for Covariate Dependent Spectral Analysis of Multiple Time Series" by Wang, Li, and Bruce (2022)

Author: Yakun Wang

Date: March 24, 2022

## Dependencies: 
Code was developed using R version 4.0.3, so code
may not function properly on older versions of R.  

## Description:
1) **README.txt**: CFile with quick start guide for demo of the Bayesian sum of trees model, description of folder contents, and instructions for using the proposed estimation procedure on the dataset of maturation in gait dynamics dataset.
2) **demo.R** R script containing all code necessary to run a demo on the three simulated AR processes described in the paper. Additionally, the code to produce the estimation and the convergence diagnostics are included to demonstrate how to visualize the convergence diagnostics measures and the estimated value proposed in the paper.

3) **Abrupt+Smooth_Heatmap.R**: R script containing the code to visualize the estimated and true covariate-dependent conditional log power spectrum described in the paper.

4) **programs**: Folder containing three R packges needed to run the Bayesian 
        sum of trees model, a R script to generate the three simulated AR 
        process described in the paper, R script for the upodate code for
        calculation number of bottom nodes, and a R script to generate the
        corresponding true log power spectrum.

5) **Gait_Maturation**: Folder containing the raw data, the R script to generate 
        the time series and covariate, and the corresponding .RData 
        file of the maturation in gait dynamics dataset. The R script to 
        produce the ALE plots, posterior predictive checks, inverse regression 
        of age estimation and the corresponding .RData described in the paper 
        is included in the folder.    
