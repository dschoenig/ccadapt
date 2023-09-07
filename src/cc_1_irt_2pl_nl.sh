#!/bin/bash
#SBATCH --account=def-cricrime 
#SBATCH --cpus-per-task=32
#SBATCH --mem=24G
#SBATCH --time=5:00:00
#SBATCH --mail-user=schonig.daniel@courrier.uqam.ca
#SBATCH --mail-type=ALL
#SBATCH --job-name=2pl_nl

module load StdEnv/2020 gcc/9.3.0 r/4.2.2

Rscript 1_irt_2pl.R TRUE
