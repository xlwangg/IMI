#!/bin/bash

#RunName="Atlanta" ; MaskName="Atlanta"
#RunName="Chicago" ; MaskName="Chicago"
#RunName="Cincinnati" ; MaskName="Cincinnati"
#RunName="Dallas" ; MaskName="Dallas"
#RunName="Detroit" ; MaskName="Detroit"
#RunName="Houston" ; MaskName="Houston"
#RunName="LosAngeles" ; MaskName="Los-Angeles"
#RunName="Miami" ; MaskName="Miami"
#RunName="NewYork" ; MaskName="New-York"
#RunName="Philadelphia" ; MaskName="Philadelphia"

#RunName="Atlanta" ; MaskName="Atlanta_domain4x3"
#RunName="Chicago" ; MaskName="Chicago_domain4x3"
#RunName="Cincinnati" ; MaskName="Cincinnati_domain4x3"
#RunName="Dallas" ; MaskName="Dallas_domain4x3"
#RunName="Detroit" ; MaskName="Detroit_domain4x3"
RunName="Houston" ; MaskName="Houston_domain4x3"
#RunName="LosAngeles" ; MaskName="Los-Angeles_domain4x3"
#RunName="Miami" ; MaskName="Miami_domain4x3"
#RunName="NewYork" ; MaskName="New-York_domain4x3"
#RunName="Philadelphia" ; MaskName="Philadelphia_domain4x3"
#RunName="Boston" ; MaskName="Boston_domain4x3"
#RunName="Washington" ; MaskName="Washington_domain4x3"


cp config.yml.template config.yml.tmp

# Modify geoschem_config.yml based on settings in config.yml
#sed -i -e "s@NewYork@${RunName}@g" \
#       -e "s@New-York@${MaskName}@g" \
#       -e "s@RunSetup: false@RunSetup: true@g" \
#       -e "s@SetupTemplateRundir: false@SetupTemplateRundir: true@g" \
#       config.yml.tmp

# Modify geoschem_config.yml based on settings in config.yml
#sed -i -e "s@NewYork@${RunName}@g" \
#       -e "s@New-York@${MaskName}@g" \
#       -e "s@RunSetup: false@RunSetup: true@g" \
#       -e "s@SetupSpinupRun: false@SetupSpinupRun: true@g" \
#       config.yml.tmp

# Modify geoschem_config.yml based on settings in config.yml
#sed -i -e "s@NewYork@${RunName}@g" \
#       -e "s@New-York@${MaskName}@g" \
#       -e "s@RunSetup: false@RunSetup: true@g" \
#       -e "s@DoPreview: false@DoPreview: true@g" \
#       config.yml.tmp

# Modify geoschem_config.yml based on settings in config.yml
#sed -i -e "s@NewYork@${RunName}@g" \
#       -e "s@New-York@${MaskName}@g" \
#       -e "s@DoSpinup: false@DoSpinup: true@g" \
#       config.yml.tmp

# Modify geoschem_config.yml based on settings in config.yml
#sed -i -e "s@NewYork@${RunName}@g" \
#       -e "s@New-York@${MaskName}@g" \
#       -e "s@RunSetup: false@RunSetup: true@g" \
#       -e "s@SetupJacobianRuns: false@SetupJacobianRuns: true@g" \
#       -e "s@SetupInversion: false@SetupInversion: true@g" \
#       config.yml.tmp

# Modify geoschem_config.yml based on settings in config.yml
sed -i -e "s@NewYork@${RunName}@g" \
       -e "s@New-York@${MaskName}@g" \
       -e "s@DoJacobian: false@DoJacobian: true@g" \
       config.yml.tmp

# Modify geoschem_config.yml based on settings in config.yml
#sed -i -e "s@NewYork@${RunName}@g" \
#       -e "s@New-York@${MaskName}@g" \
#       -e "s@DoInversion: false@DoInversion: true@g" \
#       config.yml.tmp

cp config.yml.tmp config.yml
