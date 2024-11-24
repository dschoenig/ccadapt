#!/bin/bash

Rscript 0_data_preparation.R

Rscript 1_models_varsel.R 1 willingness 4
Rscript 1_models_varsel.R 2 willingness 4
Rscript 1_models_varsel.R 3 willingness 4
Rscript 1_models_varsel.R 4 willingness 4
Rscript 1_models_varsel.R 5 willingness 4
Rscript 1_models_varsel.R 7 willingness 4

Rscript 1_cv_varsel.R 1 willingness loo 8
Rscript 1_cv_varsel.R 2 willingness loo 8
Rscript 1_cv_varsel.R 3 willingness loo 8
Rscript 1_cv_varsel.R 4 willingness loo 8
Rscript 1_cv_varsel.R 5 willingness loo 8
Rscript 1_cv_varsel.R 7 willingness loo 8

Rscript 2_variable_selection willingness

# Model fitting hard-coded to 32 threads:
Rscript 3_irt_2pl.R willingness

# # Validation to be performed manually:
# Rscript 3_irt_validation.R willingness

Rscript 4_marginal_effects.R willingness 4
Rscript 4_comparison_actions.R willingness 4

