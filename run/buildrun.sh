#!/bin/bash

../build/build.sh
rm slurm-*
sbatch run_seabreeze2d.sh
