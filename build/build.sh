#!/bin/bash
set -e
set -o pipefail 
#https://stackoverflow.com/questions/1550933/catching-error-codes-in-a-shell-pipe

#based on  the direction here
#http://www2.mmm.ucar.edu/wrf/OnLineTutorial/compilation_tutorial.php

imach="pm" #system name, pm for perlmutter

doclean_all=false #true if compiled different configure options

doclean=false

runconf=false

docompile=true

debug=true

# WRF directories
mversion="4.5.1"
script_dir="/global/homes/h/hross/codes/WRF1996/build/"

export WRF_DIR="/global/homes/h/hross/codes/WRF1996/"

#Modules --------------------------------------------------------------------
modversion="2023-09"  #denotes the time of major system update
loading_script="${script_dir}/load_modules_${modversion}_wrfsig.sh"
source ${loading_script}

#set environmental variables used by WRF build system, borrowing environmental variables 
#set by modules

export NETCDF_classic=1               #use classic (CDF1) as default
export WRFIO_NCD_LARGE_FILE_SUPPORT=1 #use 64-bit offset format (CDF2) of netcdf files
export USE_NETCDF4_FEATURES=0         #do not use netcdf4 compression (serial), need hdf5 module
#configure says WRF won't use netcdf4 compression, but I still see a flag
#ETCDF4_IO_OPTS = -DUSE_NETCDF4_FEATURES...
#other spectial flags to test
export PNETCDF_QUILT="0"  #not stable and best configuration not figured out on Perlmutter

#set environmental variables used by WRF build system, borrowing environmental variables 
#set by modules
export HDF5=$HDF5_DIR
export HDF5_LIB="$HDF5_DIR/lib"
export HDF5_BIN="$HDF5_DIR/bin"

export NETCDF=$NETCDF_DIR
export NETCDF_BIN="$NETCDF_DIR/bin"
export NETCDF_LIB="$NETCDF_DIR/lib"

#parallel netcdf library path
#need to append "/gnu/9.1"
export PNETCDF=$CRAY_PARALLEL_NETCDF_DIR/gnu/9.1  


export LD_LIBRARY_PATH="/usr/lib64":${LD_LIBRARY_PATH}
#export PATH=${NETCDF_BIN}:${PATH}
export PATH=${NETCDF_BIN}:${HDF5_BIN}:${PATH}
export LD_LIBRARY_PATH=${NETCDF_LIB}:${LD_LIBRARY_PATH}

echo "LD_LIBRARY_PATH: "$LD_LIBRARY_PATH
echo "PATH: "$PATH
echo "MANPATH: "$MANPATH

echo "NETCDF is $NETCDF"
echo "NETCDF_LIB is $NETCDF_LIB"

echo "HDF5 is $HDF5"
echo "HDF5_LIB is $HDF5_LIB"

echo "PNETCDF: ${PNETCDF}"
echo "PNETCDF_QUILT: ${PNETCDF_QUILT}"


#
##capture starting time for log file name
idate=$(date "+%Y-%m-%d-%H_%M")
#
##run make in the top directory
cd $WRF_DIR

if [ "$doclean_all" = true ]; then
    ./clean -a
    #"The './clean –a' command is required if you have edited the configure.wrf 
    #or any of the Registry files.", but this deletes configure.wrf....
    
fi

if [ "$doclean" = true ]; then
    ./clean
fi

#echo "running configure"
configfile="${WRF_DIR}/configure.wrf"

if [ "$runconf" = true ]; then

    if [ "$debug" = true ]; then
         echo "configure debug mode"
        ./configure -d
    else
        ./configure
    fi

   ##configure options selected are:
   # 32. (serial)  33. (smpar)  34. (dmpar)  35. (dm+sm)   GNU (gfortran/gcc)
   # choose 35 for real (not idealized) cases
    
    #and edit the following in configure.wrf
    #from the original
    #SFC             =       gfortran
    #SCC             =       gcc
    #CCOMP           =       gcc
    #DM_FC           =       mpif90
    #DM_CC           =       mpicc

    #to the following
    #SFC             =       ftn
    #SCC             =       cc
    #CCOMP           =       cc
    #DM_FC           =       ftn
    #DM_CC           =       cc
    
    if [ -f "$configfile" ]; then
        echo "editing configure.wrf"
        #need to remove -cc=$(SCC) in DM_CC
        sed -i 's/-cc=\$(SCC)/ /' ${configfile}
#paralell
#	sed -i 's/mpif90/ftn/' ${configfile}
#        sed -i 's/mpicc/cc/' ${configfile}

#serial
	sed -i 's/gfortran/ftn/'${configfile}
        sed -i 's/gcc/cc/' ${configfile}
        
        #also add (uncomment) "-DRSL0_ONLY" to CFLAGS_LOCAL to supress 
        #rsl.err.xxxx and rsl.out.xxxx files and write only rsl.err.0000 and rsl.out.000
        echo "using the DRSL0_ONLY flag; only the root rank will write the err and out files"
        sed -i 's/# -DRSL0_ONLY/-DRSL0_ONLY/' ${configfile}
    fi
    
fi

if [ "$docompile" = true ]; then
    export J="-j 4"  #build in parallel
    echo "J = $J"
    
    bldlog=${script_dir}/compile_logs/compile_em_${idate}_${imach}.log
    echo  "compile log file is ${bldlog}"
    
    #save modules into a log file
    module list &> ${bldlog}
    
    #./compile em_real &>> ${bldlog}
    
    ./compile em_seabreeze2d_x &>> ${bldlog} 
    #./compile em_les &>> ${bldlog}
    
    set +e 
    #grep command exits the script in case of nomatch after 2022-12 OS update
    grep 'Problems building executables' ${bldlog}
    
    RESULT=$?
    
    set -e    
    if [ $RESULT -eq 0 ]; then
    #if [ "$?" -eq "0" ]; then
        echo "compile failed, check ${bldlog}"        
    else
        echo "compile success"
        cp $WRF_DIR/main/real.exe $WRF_DIR/main/real_${idate}_${imach}.exe
        cp $WRF_DIR/main/wrf.exe $WRF_DIR/main/wrf_${idate}_${imach}.exe
        cp $WRF_DIR/main/ndown.exe $WRF_DIR/main/ndown_${idate}_${imach}.exe
        
        #cp $WRF_DIR/main/ideal.exe $WRF_DIR/main/ideal_${idate}_${imach}.exe
        #cp $WRF_DIR/main/wrf.exe $WRF_DIR/main/wrf_${idate}_${imach}.exe
        
        cp $configfile ${script_dir}/compile_logs/configure_${idate}_${imach}.wrf
    fi

fi
