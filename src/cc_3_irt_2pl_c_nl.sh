#!/bin/bash
#SBATCH --account=def-cricrime 
#SBATCH --cpus-per-task=32
#SBATCH --mem=24G
#SBATCH --time=5:00:00
#SBATCH --mail-user=schonig.daniel@courrier.uqam.ca
#SBATCH --mail-type=ALL
#SBATCH --job-name=2pl_c_nl

module load StdEnv/2023 gcc/12.3 r/4.3.1

Rscript 3_irt_2pl.R TRUE categorical
