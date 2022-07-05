#!/bin/bash

#SBATCH -p defq
#SBATCH -J fold_0
#SBATCH --exclude=gpu01     ### This node is heavily used
#SBATCH --exclude=node01     ### This node is not working
#SBATCH --ntasks=40                                   # Number of CPUs
#SBATCH -o train_fold_0.log
#SBATCH -e train_fold_0.err

# memory
ulimit -s unlimited

source /home/ottersloth/anaconda3/etc/profile.d/conda.sh

conda activate neuralhydrology_cpu

echo "I am running an hourly LSTM now"

# run code
nh-run train --config-file train_fold_5.yml

echo "I am DONE running an hourly LSTM now"
