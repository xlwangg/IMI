#!/bin/bash

# Functions available in this file include:
#   - setup_jacobian 
#   - run_jacobian 
#   - generate_BC_perturb_values

# Description: Setup jacobian run directory
# Usage:
#   setup_jacobian
setup_jacobian() {    
    # Make sure template run directory exists
    if [[ ! -f ${RunTemplate}/geoschem_config.yml ]]; then
        printf "\nTemplate run directory does not exist or has missing files. Please set 'SetupTemplateRundir=true' in config.yml" 
        exit 9999
    fi

    printf "\n=== CREATING JACOBIAN RUN DIRECTORIES ===\n"
    
    cd ${RunDirs}

    # Create directory that will contain all Jacobian run directories
    mkdir -p -v jacobian_runs

    if "$CombineJacobianRuns"; then
        if ! "$UseTotalPriorEmis"; then
             printf "\nERROR: UseTotalPriorEmis must be true when using CombineJacobianRuns. Please check config.yml.\n"
        exit 9999
        fi
 
        # Determine approx. number of CH4 tracers per Jacobian run
        if "$OptimizeBCs"; then
           nTracers=$(((nElements-4)/NumJacobianRuns)) 
           nRuns=$((NumJacobianRuns+4))
        else
           nTracers=$((nElements/NumJacobianRuns)) 
           nRuns=$NumJacobianRuns
        fi    
        printf "\n - CombineJacobianRuns activated -\n"
        printf "\nGenerating $NumJacobianRuns run directories with approx. $nTracers CH4 tracers (reperesenting state vector elements) per run\n"
    else
        nRuns=$nElements
    fi

    # Copy run scripts
    cp ${InversionPath}/src/geoschem_run_scripts/run_jacobian_simulations.sh jacobian_runs/
    sed -i -e "s:{RunName}:${RunName}:g" \
           -e "s:{InversionPath}:${InversionPath}:g" jacobian_runs/run_jacobian_simulations.sh
    cp ${InversionPath}/src/geoschem_run_scripts/submit_jacobian_simulations_array.sh jacobian_runs/
    sed -i -e "s:{START}:1:g" \
           -e "s:{END}:${nRuns}:g" \
           -e "s:{InversionPath}:${InversionPath}:g" jacobian_runs/submit_jacobian_simulations_array.sh
    if [ $MaxSimultaneousRuns -gt 0 ]; then
        # Error check
        if [ $MaxSimultaneousRuns -gt $nRuns ]; then
            printf "\MaxSimultaneousRuns=${MaxSimultaneousRuns} is greater than the total runs=${nRuns}. Please modify MaxSimultenaousRuns in config.yml" 
            exit 9999
        fi
        sed -i -e "s:{JOBS}:%${MaxSimultaneousRuns}:g" jacobian_runs/submit_jacobian_simulations_array.sh
    else
        sed -i -e "s:{JOBS}::g" jacobian_runs/submit_jacobian_simulations_array.sh
    fi
    cp ${InversionPath}/src/geoschem_run_scripts/run_prior_simulation.sh jacobian_runs/
    sed -i -e "s:{RunName}:${RunName}:g" \
           -e "s:{InversionPath}:${InversionPath}:g" jacobian_runs/run_prior_simulation.sh
    cp ${InversionPath}/src/geoschem_run_scripts/run_bkgd_simulation.sh jacobian_runs/
    sed -i -e "s:{RunName}:${RunName}:g" \
           -e "s:{InversionPath}:${InversionPath}:g" jacobian_runs/run_bkgd_simulation.sh

    if "$UseTotalPriorEmis"; then
        printf "\nTurning on use of total prior emissions in HEMCO_Config.rc. This will ignore all other emission inventories.\n"

        # Modify HEMCO_Config.rc to turn off individual emission inventories
        # and use total emissions saved out from prior emissions simulation
        # instead
        # Do this in template run directory to avoid having to repeat for each
        # Jacobian run directory
            sed -i -e "s|UseTotalPriorEmis      :       false|UseTotalPriorEmis      :       true|g" \
                   -e "s|AnalyticalInversion    :       false|AnalyticalInversion    :       true|g" \
                   -e "s|GFED                   : on|GFED                   : off|g" ${RunTemplate}/HEMCO_Config.rc
    fi

    # Initialize (x=0 is base run, i.e. no perturbation; x=1 is state vector element=1; etc.)
    if "$CombineJacobianRuns"; then
        x=1
    else
        x=0
    fi

    # Create run directory for each state vector element so we can
    # apply the perturbation to each
    while [ $x -le $nRuns ]; do

	    # Current state vector element
	    xUSE=$x

	    # Add zeros to string name
	    if [ $x -lt 10 ]; then
	        xstr="000${x}"
	    elif [ $x -lt 100 ]; then
	        xstr="00${x}"
	    elif [ $x -lt 1000 ]; then
	        xstr="0${x}"
	    else
	        xstr="${x}"
	    fi

        create_simulation_dir
    
        # Increment
	    x=$[$x+1]
    done

    if "$LognormalErrors"; then
        x="background"
        xstr=$x
        create_simulation_dir
    fi

    printf "\n=== DONE CREATING JACOBIAN RUN DIRECTORIES ===\n"
}

create_simulation_dir() {
    # Define the run directory name
	name="${RunName}_${xstr}"

	# Make the directory
	runDir="./jacobian_runs/${name}"
	mkdir -p -v ${runDir}

	# Copy run directory files
	cp -r ${RunTemplate}/*  ${runDir}
	cd $runDir

	# Link to GEOS-Chem executable instead of having a copy in each rundir
	rm -rf gcclassic
	ln -s ${RunTemplate}/gcclassic .

    # Link to restart file
    RestartFileFromSpinup=${RunDirs}/spinup_run/Restarts/GEOSChem.Restart.${SpinupEnd}_0000z.nc4
    if test -f "$RestartFileFromSpinup" || "$DoSpinup"; then
        ln -s $RestartFileFromSpinup Restarts/GEOSChem.Restart.${StartDate}_0000z.nc4
	else
	    RestartFile=${RestartFilePrefix}${StartDate}_0000z.nc4
	    ln -s $RestartFile Restarts/GEOSChem.Restart.${StartDate}_0000z.nc4
	    if "$UseBCsForRestart"; then
		    sed -i -e "s|SpeciesRst|SpeciesBC|g" HEMCO_Config.rc
        fi
	fi
   
	# Update settings in geoschem_config.yml except for the base run
    if [ $x -ne 0 ] && [ "$x" != "background" ]; then
	    sed -i -e "s|emission_perturbation: 1.0|emission_perturbation: ${PerturbValue}|g" \
	           -e "s|state_vector_element_number: 0|state_vector_element_number: ${xUSE}|g" geoschem_config.yml
    fi

    # BC optimization setup
    if "$OptimizeBCs"; then
        bcThreshold=$(($nRuns - 4))
        # The last four state vector elements are reserved for BC optimization of NSEW
        # domain edges. If the current state vector element is one of these, then
        # turn on BC optimization for the corresponding edge and revert emission perturbation
        if [ $x -gt $bcThreshold ]; then
            PerturbBCValues=$(generate_BC_perturb_values $bcThreshold $x $PerturbValueBCs)
            sed -i -e "s|CH4_boundary_condition_ppb_increase_NSEW:.*|CH4_boundary_condition_ppb_increase_NSEW: ${PerturbBCValues}|g" \
                -e "s|perturb_CH4_boundary_conditions: false|perturb_CH4_boundary_conditions: true|g" \
                -e "s|emission_perturbation: ${PerturbValue}|emission_perturbation: 1.0|g" \
                -e "s|state_vector_element_number: ${xUSE}|state_vector_element_number: 0|g" geoschem_config.yml
        fi
    else
        bcThreshold=$nRuns
    fi 

	# Update settings in HISTORY.rc
	# Only save out hourly pressure fields to daily files for base run
	if [ $x -eq 0 ] || [ "$x" = "background" ]; then
	    if "$HourlyCH4"; then
            sed -i -e 's/'\''Restart/#'\''Restart/g' \
                   -e 's/#'\''LevelEdgeDiags/'\''LevelEdgeDiags/g' \
                   -e 's/LevelEdgeDiags.frequency:   00000100 000000/LevelEdgeDiags.frequency:   00000000 010000/g' \
                   -e 's/LevelEdgeDiags.duration:    00000100 000000/LevelEdgeDiags.duration:    00000001 000000/g' \
                   -e 's/LevelEdgeDiags.mode:        '\''time-averaged/LevelEdgeDiags.mode:        '\''instantaneous/g' HISTORY.rc
	    fi
    # For all other runs, just disable Restarts
    else
        if "$HourlyCH4"; then
            sed -i -e 's/'\''Restart/#'\''Restart/g' HISTORY.rc
        fi
    fi

    if "$CombineJacobianRuns" && [ $x -le $bcThreshold ]; then

        # Creat directory for perturbations
        PertDir="./Perturbations"
        mkdir -p -v ${PertDir}

        # Determine start and end element numbers for this run directory
        if [ $x -eq 0 ]; then
            start=1
        else
            start=$(( (x-1) * nTracers + (x-1) ))
        fi
        if [ $x -eq $bcThreshold ]; then
            if "$OptimizeBCs"; then
               end=$((nElements - 4))
            else   
               end=$nElements
            fi   
        else
            end=$(( start + nTracers ))
        fi

        # Modify restart file entry in HEMCO_Config.rc
        sed -i -e "s/* SPC_/* SPC_CH4/g"  -e "s/?ALL?/CH4/g" -e "s/EFYO xyz 1 \*/EFYO xyz 1 CH4/g" HEMCO_Config.rc
        sed -i -e "s/* BC_/* BC_CH4/g"    -e "s/?ADV?/CH4/g" -e "s/EFY xyz 1 \*/EFY xyz 1 CH4/g" HEMCO_Config.rc

        # Initialize previous lines to search
        GcPrevLine='- CH4'
        HcoPrevLine1='EFYO xyz 1 CH4 - 1 '
        HcoPrevLine2='EFY xyz 1 CH4 - 1 '
        HcoPrevLine3='CH4 - 1 500'
        HcoPrevLine4='Perturbations.txt - - - xy count 1'
        PertPrevLine='DEFAULT    0     1.0'

        # Loop over element numbers for this run and add as CH4 tracers in
        # configuraton files
        for i in $(seq $start $end); do

            if [ $i -lt 10 ]; then
                istr="000${i}"
            elif [ $i -lt 100 ]; then
                istr="00${i}"
            elif [ $i -lt 1000 ]; then
                istr="0${i}"
            else
                istr="${i}"
            fi

            # Start HEMCO scale factor ID at 2000 to avoid conflicts with
            # preexisting scale factors/masks
            SFnum=$((2000 + i))
            maskID=$i

            # Add lines to geoschem_config.yml
            # Spacing in GcNewLine is intentional
            GcNewLine='\- CH4_'$istr
            sed -i -e "/$GcPrevLine/a $GcNewLine" geoschem_config.yml
            GcPrevLine='- CH4_'$istr

            # Add lines to species_database.yml
            SpcNextLine='CHBr3:'
            SpcNewLines='CH4_'$istr':\n  << : *CH4properties\n  Background_VV: 1.8e-6\n  FullName: Methane'
            sed -i -e "s|$SpcNextLine|$SpcNewLines\n$SpcNextLine|g" species_database.yml

            # Add lines to HEMCO_Config.yml
            HcoNewLine1='\* SPC_CH4_'$istr' - - - - - - CH4_'$istr' - 1 1'
            sed -i -e "/$HcoPrevLine1/a $HcoNewLine1" HEMCO_Config.rc
            HcoPrevLine1='\* SPC_CH4_'$istr' - - - - - - CH4_'$istr' - 1 1'

            # Add lines to HEMCO_Config.yml
            HcoNewLine2='\* BC_CH4_'$istr' - - - - - - CH4_'$istr' - 1 1'
            sed -i -e "/$HcoPrevLine2/a $HcoNewLine2" HEMCO_Config.rc
            HcoPrevLine2='\* BC_CH4_'$istr' - - - - - - CH4_'$istr' - 1 1'

            HcoNewLine3='\0 CH4_Emis_Prior_'$istr' - - - - - - CH4_'$istr' '$SFnum' 1 500'
            sed -i "/$HcoPrevLine3/a $HcoNewLine3" HEMCO_Config.rc
            HcoPrevLine3='CH4_'$istr' '$SFnum' 1 500'

            # HcoNewLine4='\'$SFnum' SCALE_ELEM_'$istr' Perturbations.txt - - - xy count 1'
            # sed -i "/$HcoPrevLine4/a $HcoNewLine4" HEMCO_Config.rc
            # HcoPrevLine4='SCALE_ELEM_'$istr' Perturbations.txt - - - xy count 1'

            HcoNewLine4="${SFnum} SCALE_ELEM_${istr} ./Perturbations/Perturb_${istr}.txt - - - xy count 1"
            sed -i "/$HcoPrevLine4/a $HcoNewLine4" HEMCO_Config.rc
            HcoPrevLine4="Perturb_${istr}.txt - - - xy count 1"

            # Creat perturbation files
            cp ./Perturbations.txt ./Perturbations/Perturb_${istr}.txt

            # Add lines to Perturbations.txt
            PertNewLine='\ELEM_'$istr'  '$maskID'  1.5'
            sed -i "/$PertPrevLine/a $PertNewLine" "./Perturbations/Perturb_${istr}.txt"
            # PertPrevLine='ELEM_'$istr'  '$maskID'  1.5'

        done

	fi

    # for background simulation, disable the emissions
    # needed for lognormal error inversion
    if [ "$x" = "background" ]; then
        # TODO: OH_pert_factor sed will be unnecessary once geoschem Emissions logical is fixed
        # until  then we remove the OH_pert_factor line and add it back in before at line 877
        # sed -i '/2 OH_pert_factor  1.0 - - - xy 1 1/d' HEMCO_Config.rc
        sed -i -e 's/EMISSIONS              :       true/EMISSIONS              :       false/g' \
               -e 's/GFED                   : on    CH4/GFED                   : off    CH4/g'  HEMCO_Config.rc
        
        # TODO: remove this hack once geoschem Emissions logical is fixed
        # echo '2 OH_pert_factor  1.0 - - - xy 1 1' > add.txt
        # sed -i -e '877r add.txt' HEMCO_Config.rc
        # rm add.txt
    fi

	# Create run script from template
	sed -e "s:namename:${name}:g" ch4_run.template > ${name}.run
	rm -f ch4_run.template
	chmod 755 ${name}.run

    ### Turn on observation operators if requested, only for base run
    if [ $x -eq 0 ] || [ "$x" = "background" ]; then
    	activate_observations
    fi

    ### Perform dry run if requested, only for base run
    if [ $x -eq 0 ]; then
        if "$ProductionDryRun"; then
            printf "\nExecuting dry-run for production runs...\n"
            ./gcclassic --dryrun &> log.dryrun
            ./download_data.py log.dryrun aws
        fi
    fi

    # Navigate back to top-level directory
	cd ../..
}

# Description: Run jacobian simulations
# Usage:
#   run_jacobian
run_jacobian() {
    if ! "$PrecomputedJacobian"; then
        jacobian_start=$(date +%s)
        printf "\n=== SUBMITTING JACOBIAN SIMULATIONS ===\n"

        cd ${RunDirs}/jacobian_runs

        if ! "$isAWS"; then
            # Load environment with modules for compiling GEOS-Chem Classic
            source ${GEOSChemEnv} 
        fi

        # Submit job to job scheduler
        source submit_jacobian_simulations_array.sh

        if "$LognormalErrors"; then
            sbatch --mem $SimulationMemory \
                -c $SimulationCPUs \
                -t $RequestedTime \
                -p $SchedulerPartition \
                -W run_bkgd_simulation.sh; wait;
        fi

        # check if any jacobians exited with non-zero exit code
        [ ! -f ".error_status_file.txt" ] || imi_failed $LINENO

        printf "\n=== DONE JACOBIAN SIMULATIONS ===\n"
        jacobian_end=$(date +%s)
    else
        # Add symlink pointing to jacobian matrix files from the reference
        # inversion w/ precomputed Jacobian
        if "$KalmanMode"; then
            cd ${RunDirs}/kf_inversions/period${period_i}
            precomputedJacobianCachePrefix=${ReferenceRunDir}/kf_inversions/period${period_i}
        else
            cd ${RunDirs}/inversion
            precomputedJacobianCachePrefix=${ReferenceRunDir}/inversion
        fi

        precomputedJacobianCacheNormal=${precomputedJacobianCachePrefix}/data_converted
        precomputedJacobianCacheLognormal=${precomputedJacobianCachePrefix}/data_converted_background
        
        if [ -d $precomputedJacobianCacheNormal ]; then
            ln -s $precomputedJacobianCacheNormal data_converted_reference
        fi

        if [ -d $precomputedJacobianCacheLognormal ]; then
            ln -s $precomputedJacobianCacheLognormal data_converted_reference_background
        fi


        # Run the prior simulation
        cd ${JacobianRunsDir}
            
        if ! "$isAWS"; then
            # Load environment with modules for compiling GEOS-Chem Classic
            source ${GEOSChemEnv}
        fi

        # Submit prior simulation to job scheduler
        printf "\n=== SUBMITTING PRIOR SIMULATION ===\n"
        sbatch --mem $SimulationMemory \
                -c $SimulationCPUs \
                -t $RequestedTime \
                -p $SchedulerPartition \
                -W run_prior_simulation.sh; wait;
        printf "=== DONE PRIOR SIMULATION ===\n"

        # Get Jacobian scale factors
        python ${InversionPath}/src/inversion_scripts/get_jacobian_scalefactors.py $period_i $RunDirs $ReferenceRunDir; wait
        printf "Got Jacobian scale factors\n"
    fi
}

# Description: Print perturbation string for BC optimization
#   based on the current state vector element
#   Returns [float, float, float, float]
# Usage:
#   generate_BC_perturb_values <bcThreshold> <element-number> <pert-value>
generate_BC_perturb_values() {
    python -c "import sys;\
    bc_perturb = [0.0, 0.0, 0.0, 0.0];\
    bcThreshold = int(sys.argv[1]) + 1;\
    element = int(sys.argv[2]);\
    pert_index = element % bcThreshold;\
    bc_perturb[pert_index] = float(sys.argv[3]);\
    print(bc_perturb)" $1 $2 $3
}

