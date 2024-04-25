#!/bin/bash
set -e

scname=$BASH_SOURCE  #name of this script

echo "loading modules using ${scname}"

#general modules
   
    
#keep other automatically loaded modules 

#1) load/switch to specific modules needed; 
#specific versions helps later reproducability & debugging
module load cpu/1.0
 
module load PrgEnv-gnu/8.3.3 
module swap gcc/11.2.0 
module swap cray-mpich/8.1.25
module swap craype/2.7.20
    
#module for WRF file I/O
#default modules in September 2023
#for netcdf4 large file & parallel IO 
module load cray-hdf5/1.12.2.3  
module load cray-netcdf/4.9.0.3 
module load cray-parallel-netcdf/1.12.3.3

module list



