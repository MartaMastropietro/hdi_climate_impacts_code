
To create projections:
- run scripts/002_models/original_comp_oaat_and_final_lags_models to generate coefficients and covariance matrix 
- run in oder scripts in 001_variables_prep
- run script in 002_climate_prep

-JUNO_MC_impacts_proj_final.R
 takes projected preprocessed climate variables (generated from code in folder 002_climate_prep), 
 takes projected preprocessed ssp variables (generated from code in folder 001_variables_prep) 
 and applies damage functions we select (need coefs, covariance) 
 it saves all bootstraps, intervals for each region, year, ssp, model

-JUNO_boot_hdi_subnat_computation_final.R
 takes all bootstrapped from JUNO_MC_impacts_proj_final.R 
 computes indeces, hdi

-JUNO_boot_hdi_subnat_delta_computation_and_agg.R
 takes all bootstrapped indeces from JUNO_boot_hdi_subnat_computation_final.R
 computes deltas, perc deltas 
 aggregates at global, country level (selected countries) 

-JUNO_boot_hdi_subnat_sign_delta_comput_and_agg.R
 takes deltas, perc deltas but only aggregates using significant values (90% ci of bootstrap + 2/3 models agreement on sign)

boot_agg_plots, boot_agg_plots_tp_extr_sep, preproc_plot_boot_agg_c_sel_data and var_decomp_agg produce the plots and maps present in the paper

scripts marked with JUNO were run on the omonym supercomputer in CMCC's high performance computing center 