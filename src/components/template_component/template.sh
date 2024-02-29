#!/bin/bash

# Functions available in this file include:
#   - setup_template 

# Description: Setup template GCClassic run directory
# Usage:
#   setup_template
setup_template() {
    printf "\n=== CREATING TEMPLATE RUN DIRECTORY ===\n"

    cd ${GCClassicPath}/run

    # The createRunDir.sh script assumes the file ~/.geoschem/config exists
    # and contains the path to GEOS-Chem input data
	export GC_USER_REGISTERED=true
    if [[ ! -f ${HOME}/.geoschem/config ]]; then
	mkdir -p ${HOME}/.geoschem
	echo "export GC_DATA_ROOT=${DataPath}" >> ${HOME}/.geoschem/config
	source ${HOME}/.geoschem/config
    fi

    if [[ -d ${RunTemplate} ]]; then
	printf "\nERROR: ${RunTemplate} already exists. Please remove or set 'SetupTemplateRunDir: false' in config.yml.\n"
	exit 9999
    fi

    # Commands to feed to createRunDir.sh
    # Run directories are created for the global domain by default. If needed,
    # the regional domain specified in config.yml will be adjusted for below.
    if [[ "$Met" == "MERRA2" || "$Met" == "MERRA-2" || "$Met" == "merra2" ]]; then
        metNum="1"
    elif [[ "$Met" == "GEOSFP" || "$Met" == "GEOS-FP" || "$Met" == "geosfp" ]]; then
        metNum="2"
    else
        printf "\nERROR: Meteorology field ${Met} is not supported by the IMI. "
        printf "\n Options are GEOSFP or MERRA2.\n"
        exit 1
    fi  
    if [ "$Res" = "4.0x5.0" ]; then
        cmd="3\n${metNum}\n1\n2\n${RunDirs}\n${runDir}\nn\n" #global run
    elif [ "$Res" == "2.0x2.5" ]; then
        cmd="3\n${metNum}\n2\n2\n${RunDirs}\n${runDir}\nn\n" #global run
    elif [ "$Res" == "0.5x0.625" ]; then
        cmd="3\n${metNum}\n3\n1\n2\n${RunDirs}\n${runDir}\nn\n" #global run
    elif [ "$Res" == "0.25x0.3125" ]; then
        cmd="3\n${metNum}\n4\n1\n2\n${RunDirs}\n${runDir}\nn\n" #global run
        #cmd="3\n2\n4\n4\n2\n${RunDirs}\n${runDir}\nn\n" #regional run
    elif [ "$Res" == "0.125x0.15625" ]; then
        cmd="3\n${metNum}\n5\n4\n2\n${RunDirs}\n${runDir}\nn\n" #regional run
    else
        printf "\nERROR: Grid resolution ${Res} is not supported by the IMI. "
        printf "\n Options are 0.25x0.3125, 0.5x0.625, 2.0x2.5, or 4.0x5.0.\n"
        exit 1
    fi

    # Create run directory
    printf ${cmd} | ./createRunDir.sh >> createRunDir.log 2>&1
    rm -f createRunDir.log
    printf "\nCreated ${RunTemplate}\n"

    cd ${RunTemplate}

    if "$isAWS"; then
	# Update GC data download to silence output from aws commands
	sed -i "s/command: 'aws s3 cp --request-payer=requester '/command: 'aws s3 cp --request-payer=requester --only-show-errors '/" download_data.yml
    fi

    # Modify geoschem_config.yml based on settings in config.yml
    sed -i -e "s:20190101:${StartDate}:g" \
           -e "s:20190201:${EndDate}:g" \
           -e "s:-130.0,  -60.0:${Lons}:g" \
           -e "s:9.75,  60.0:${Lats}:g" geoschem_config.yml


    # Also turn on analytical inversion option in HEMCO_Config.rc
    OLD="--> AnalyticalInv          :       false"
    NEW="--> AnalyticalInv          :       true "
    sed -i "s/$OLD/$NEW/g" HEMCO_Config.rc

    # Update time cycling flags to use most recent year
    sed -i "s/RF xy/C xy/g" HEMCO_Config.rc
    
    # Modify path to state vector file in HEMCO_Config.rc
    OLD=" StateVector.nc"
    NEW=" ${RunDirs}/StateVector.nc"
    sed -i -e "s@$OLD@$NEW@g" HEMCO_Config.rc

    # Modify HEMCO_Config.rc if running Kalman filter
    if "$LognormalErrors"; then
        gridded_posterior_filename="gridded_posterior_ln.nc"
    else
        gridded_posterior_filename="gridded_posterior.nc"
    fi
    if "$KalmanMode"; then
        sed -i -e "s|use_emission_scale_factor: false|use_emission_scale_factor: true|g" geoschem_config.yml
        sed -i -e "s|--> Emis_ScaleFactor       :       false|--> Emis_ScaleFactor       :       true|g" \
               -e "s|gridded_posterior.nc|${RunDirs}/ScaleFactors.nc|g" HEMCO_Config.rc
    fi

    # Turn other options on/off according to settings above
    if "$UseEmisSF"; then
	OLD="use_emission_scale_factor: false"
	NEW="use_emission_scale_factor: true"
	sed -i "s/$OLD/$NEW/g" geoschem_config.yml
    fi
    if "$UseOHSF"; then
	OLD="use_OH_scale_factors: false"
	NEW="use_OH_scale_factors: true"
	sed -i "s/$OLD/$NEW/g" geoschem_config.yml
    fi

    # Modify HEMCO_Config.rc based on settings in config.yml
    # Use cropped met fields (add the region to both METDIR and the met files)
    if "$isRegional"; then
        # Modify the METDIR for 0.125x0.15625 simulation
        if [ "$Res" = "0.125x0.15625" ]; then
            sed -i -e "s:GEOS_0.25x0.3125\/GEOS_FP:GEOS_0.25x0.3125_NA\/GEOS_FP:g" HEMCO_Config.rc.gmao_metfields_0125
            OLD="/n/holyscratch01/external_repos/GEOS-CHEM/gcgrid/gcdata/ExtData/GEOS_0.125x0.15625/GEOS_FP"
            NEW="/n/home00/xlwang/xlwang/methane_inversion/InputData/GEOS_0.125x0.15625_NA/GEOS_FP_DerivedWinds"
            sed -i "s|$OLD|$NEW|g" HEMCO_Config.rc.gmao_metfields_0125
            sed -i '/METDIR/d' HEMCO_Config.rc
        else  
            #sed -i -e "s:GEOS_${Res}:GEOS_${Res}_${RegionID}:g" HEMCO_Config.rc #new branch code
            #sed -i -e "s:GEOS_${Res}:GEOS_${Res}_${RegionID}:g" HEMCO_Config.rc.gmao_metfields #new branch code
            sed -i -e "s:GEOS_0.25x0.3125\/GEOS_FP:GEOS_${native}_${NestedRegion}\/${metDir}:g" HEMCO_Config.rc
            sed -i -e "s:GEOS_0.25x0.3125\/GEOS_FP:GEOS_${native}_${NestedRegion}\/${metDir}:g" HEMCO_Config.rc.gmao_metfields      
            sed -i -e "s:\$RES:\$RES.${RegionID}:g" HEMCO_Config.rc.gmao_metfields
        fi
    fi

    # Determine length of inversion period in days
    InvPeriodLength=$(( ( $(date -d ${EndDate} "+%s") - $(date -d ${StartDate} "+%s") ) / 86400))

    # If inversion period is < 32 days, use End diagnostic output frequency
    if (( ${InvPeriodLength} < 32 )) || $KalmanMode; then
        sed -i -e "s|DiagnFreq:                   Monthly|DiagnFreq:                   End|g" HEMCO_Config.rc
    fi

    # Modify path to BC files
    sed -i -e "s:\$ROOT/SAMPLE_BCs/v2021-07/CH4:${fullBCpath}:g" HEMCO_Config.rc
    if [ "$isRegional" == "false" ]; then
        OLD="--> GC_BCs                 :       true "
        NEW="--> GC_BCs                 :       false"
        sed -i "s/$OLD/$NEW/g" HEMCO_Config.rc
    fi

    # Modify HISTORY.rc
    sed -i -e "s:'CH4':#'CH4':g" \
           -e "s:'Metrics:#'Metrics:g" \
           -e "s:'StateMet:#'StateMet:g" HISTORY.rc
    
    # If turned on, save out hourly CH4 concentrations to daily files
    if "$HourlyCH4"; then
        sed -i -e 's/SpeciesConc.frequency:      00000100 000000/SpeciesConc.frequency:      00000000 010000/g' \
    	       -e 's/SpeciesConc.duration:       00000100 000000/SpeciesConc.duration:       00000001 000000/g' \
               -e 's/SpeciesConc.mode:           '\''time-averaged/SpeciesConc.mode:           '\''instantaneous/g' HISTORY.rc
    fi

    if ! "$isAWS"; then
	# Load environment with modules for compiling GEOS-Chem Classic
        source ${GEOSChemEnv}
    fi

    # Remove sample restart file
    rm -f Restarts/GEOSChem.Restart.20190101_0000z.nc4

    # Copy template run script
    cp ${InversionPath}/src/geoschem_run_scripts/ch4_run.template .
    
   # Copy input file for applying emissions perturbations via HEMCO
    cp ${InversionPath}/src/geoschem_run_scripts/Perturbations.txt .
    
    # Compile GEOS-Chem and store executable in template run directory
    printf "\nCompiling GEOS-Chem...\n"
    cd build
    cmake ${InversionPath}/GCClassic >> build_geoschem.log 2>&1
    cmake . -DRUNDIR=..  >> build_geoschem.log 2>&1 
    make -j install >> build_geoschem.log 2>&1
    cd ..
    if [[ -f gcclassic ]]; then
        rm -rf build
        mv build_info ../GEOSChem_build_info
    else
        printf "\nGEOS-Chem build failed! \n\nSee ${RunTemplate}/build/build_geoschem.log for details\n"
        exit 999
    fi
    printf "\nDone compiling GEOS-Chem \n\nSee ${RunDirs}/GEOSChem_build_info for details\n\n"
    
    # Navigate back to top-level directory
    cd ..

    printf "\n=== DONE CREATING TEMPLATE RUN DIRECTORY ===\n"
}
