#!/bin/bash 
#SBATCH -N 1
#SBATCH -q debug
#SBATCH -t 00:30:00
#SBATCH -J test
#SBATCH -A nstaff   #user needs to change this
#SBATCH -L scratch,cfs
#SBATCH -C cpu
#SBATCH --tasks-per-node=64

pwd
ntile=4  #number of OpenMP threads per MPI task
#need to set the "numtiles" variable in the wrf namelist (namelist.input) to be the same 

#example using the WRFSIG project CFS directories; 
#files only accesible by the WRFSIG members

bindir="/global/homes/h/hross/codes/WRF1996/run/"
binname="real.exe"
binname2="wrf.exe"
#binname="tc.exe"
rundir="/pscratch/sd/h/hross/simulation/WRF/run_clean/" #user needs to change this

#Modules --------------------------------------------------------------------
#use the example module-loading script given above; accessible for WRF-SIG members
modversion="2023-09" 
loading_script="/global/cfs/cdirs/m4232/scripts/build/load_modules_${modversion}_wrfsig.sh"
source ${loading_script}

#OpenMP settings:
export OMP_NUM_THREADS=$ntile
export OMP_PLACES=threads  #"true" when not using multiple OpenMP threads (i.e., ntile=1)
export OMP_PROC_BIND=spread

cd $rundir

#get ics
srun -n 64 -c 4 --cpu_bind=cores ${bindir}/${binname}

#rename and save the process 0 out and err files
cp rsl.error.0000 rsl.error_ics_$SLURM_JOBID
cp rsl.out.0000 rsl.out_ics_$SLURM_JOBID

#run simulation
srun -n 64 -c 4 --cpu_bind=cores ${bindir}/${binname2}

#rename and save the process 0 out and err files
cp rsl.error.0000 rsl.error_0_$SLURM_JOBID
cp rsl.out.0000 rsl.out_0_$SLURM_JOBID
