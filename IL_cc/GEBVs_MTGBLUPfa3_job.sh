#!/bin/bash

###############################################################################
#
#SBATCH --time=48:05:00                    # Job run time (hh:mm:ss)
#SBATCH --nodes=1                          # Number of nodes
#SBATCH --ntasks-per-node=16               # Number of task (cores/ppn) per node
#SBATCH --job-name=GEBVs_MTGBLUPfa3_job    # Name of batch job
#SBATCH --partition=secondary              # Partition (queue)
#SBATCH --output=GEBVs_MTGBLUPfa3.o%j      # Name of batch job output file
##SBATCH --error=GEBVs_MTGBLUPfa3.e%j      # Name of batch job error file
##SBATCH --mail-user=lucasb4@illinois.edu  # Send email notifications
##SBATCH --mail-type=ALL                   # Type of email notifications to send
#
###############################################################################


# Change to the directory from which the batch job was submitted
# Note: SLURM defaults to running jobs in the directory where
# they are submitted, no need for cd'ing to $SLURM_SUBMIT_DIR

#cd ${SLURM_SUBMIT_DIR}

# Load R
module load R

# Run R
R CMD BATCH troubleshooting.R GEBVs_troubleshooting.out
