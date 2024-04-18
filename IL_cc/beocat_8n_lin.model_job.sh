#!/bin/bash 

#request RAM, on a per CPU basis
#SBATCH --mem-per-cpu=50G   # Memory per core, use --mem= for memory per node

#request CPUs
#SBATCH --nodes=1
#SBATCH --ntasks-per-node=2

#SBATCH --time=00-12:00:00   # Use the form DD-HH:MM:SS
#SBATCH --job-name=beocat_8n_lin.model_job
#SBATCH --mail-type=ALL   # same as =BEGIN,FAIL,END --mail-user=lucasmunaro@ksu.edu



# run pipeline
module load R
R CMD BATCH beocat_8n_lin.model.R beocat_8n_lin.model.out