%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% README file for "Adaptive Bayesian Sum of Trees Model for Covariate Dependent 
% Spectral Analysis of Multiple Time Series"
%
% 
% Date: March 24, 2022
% 
% Description:
% Instructions for implementing Bayesian sum of trees model estimation 
% from "Adaptive Bayesian Sum of Trees Model for Covariate Dependent Spectral 
% Analysis of Multiple Time Series" 
%
% Dependencies:
% Code was developed using R version 4.0.3
%
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Contents
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
<README.txt> File with quick start guide for demo of the Bayesian 
        sum of trees model, description of folder contents, and instructions 
        for using the proposed estimation procedure on the dataset of 
        maturation in gait dynamics dataset.

<demo.R> R script containing all code necessary to run a demo on the 
        three simulated AR processes described in the paper. Additionally, 
        the code to produce the estimation and the convergence diagnostics 
        are included to demonstrate how to visualize the convergence
	diagnostics measures and the estimated value proposed in the paper.

<Abrupt+Smooth_Heatmap.R>  R script containing the code to visualize the estimated 
        and true covariate-dependent conditional log power spectrum 
        described in the paper.

<programs> Folder containing three R packges needed to run the Bayesian 
        sum of trees model, a R script to generate the three simulated AR 
        process described in the paper, R script for the upodate code for
        calculation number of bottom nodes, and a R script to generate the
        corresponding true log power spectrum.

<Gait_Maturation> Folder containing the raw data, the R script to generate 
        the time series and covariate, and the corresponding .RData 
        file of the maturation in gait dynamics dataset. The R script to 
        produce the ALE plots, posterior predictive checks, inverse regression 
        of age estimation and the corresponding .RData described in the paper 
        is included in the folder.     

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Quick start guide (demo)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

Follow the steps below to simulate data, run the Bayesian sum of trees model,
and create data visualizations to assess the convergence and fit of the 
method for the three simulations described in the paper.

1) Download the .zip file and extract the contents to a folder of your
choosing.  In what follows, we will assume the folder is saved with the 
following path 'C:\BayesSumOfTreesSPEC'.

2) Open R or RStudio (version 4.0.3 or newer is recommended) and path to the
directory to 'C:/BayesSumOfTreesSPEC'.  

3) Open the file 'C:/BayesSumOfTreesSPEC/demo.R'.  Follow the instruction in the 
beginning of the file to install the packages needed to reproduce the 
Bayesian sum of trees model. 

4) Follow the instructions in the comments of the demo file to simulate 
data and apply the Bayesian sum of trees model to obtain spectral estimates 
and summary plots of convergence diagnostic measures and plots 
used to visualize the estimation.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Detailed description for Gait_Maturation folder
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

<data folder>: Folder includes raw data of stride time interval for gait 
               maturation analysis.

<data_manipulation.R>: R file to process and manipulate the raw data following 
                       Hausdorff et al. (1999).

<R_gain_data_stridenumber.RData>: The produced R data after data manipulation 
                           (after running the file data_manipulation.R).

<ALE.R>: R file to generate ALE plots of age gender, and speed for gait maturation 
         data and the convergence diagnostic plots as described in the paper.

<Inverse_Regression.R>: R file to generate inverse regression of age estimation 
                        results for gait maturation data and the plots 
                        as described in the paper.

<PartialDependenceParallel_age.RData>: The produced R data of age-dependence power 
                                       spectrum estimation of inverse regression analysis.
                                       (after running the partial dependence algorithm 
                                        in the Inverse_Regression.R file). 
                       
<Posterior_predictive_checks.R>: R file to generate posterior predictive plots
                                 for gait maturation data as described in the paper.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Using Bayesian sum of trees model on other data
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

Two kinds of inputs are necessary to run the MCMC sampler for the estimation
procedure.

1. Data information: 

1) 'x_t' is a matrix whereby each column contains a realization of the time
series for a given subject.  The rows are indexed by time 
(t=1,...,T) and the columns are indexed by subject (l=1,...,L).  For 
example, x_t[1,1] contains the first time series instance for the first 
subject.  

2) 'x' is a matrix containning the covariates values for each subject. 
The rows are indexed by subject and the columns are indexed by covariates.
For example, x[1,1] is the first covariate value for the first subject.


3) 'Vartype' is a vector containing the variable types corresponding to the 
covariates in 'x'. There are two options: 'ORD' refers to ordinary covariates
and 'CAT' refers to categorical covariates.

4) 'numcut' is a vactor containing the number of possible cutpoints for covariates 
in 'x'. If the covariate type is categorical, the number of possible cutpoints 
should equal to the number of categories.


2. Tree information:

1) 'opt' is a structure containing the options required for the MCMC 
sampler to run: the number of loops ('nloop'), the number of warmup ('nwarmup') 
for the MCMC algorithm, and the number of basis ('nbasis'), the variance of the 
constant term for the Bayesian spline model described in the paper.

2) Hyperparameters for half-t prior dsitribution of the variance of basis 
parameters for the Bayesian spline model and the hyperparameters for Dirichelet 
priors. See 'demo.R' comments for descriptions of all the parameters.


Once you have created these inputs based on your data, you can pass
them into the 'BayesSumOfTreesSPEC' function just as in the demo file for estimation.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%