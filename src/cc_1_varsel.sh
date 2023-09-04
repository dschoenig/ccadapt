#!/bin/bash
#SBATCH --account=def-cricrime 
#SBATCH --cpus-per-task=4
#SBATCH --mem=18G
#SBATCH --time=24:00:00
#SBATCH --mail-user=schonig.daniel@courrier.uqam.ca
#SBATCH --mail-type=ALL
#SBATCH --job-name=varsel_test

module load StdEnv/2020 gcc/9.3.0 r/4.2.2

Rscript 1_varsel.R
