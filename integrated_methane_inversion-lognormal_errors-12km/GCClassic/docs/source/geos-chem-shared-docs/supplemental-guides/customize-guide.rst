.. _customguide:

###########################################
Customize simulations with research options
###########################################

Most of the time you will want to use the "out-of-the-box" settings in
your GEOS-Chem simulations, as these are the recommended settings that
have been evaluated with benchmark simulations.  But depending on your
research needs, you may wish to use alternate simulation options.  In
this Guide we will show you how you can select these **research
options** by editing the various GEOS-Chem and HEMCO configuration
files.

.. _customguide-aer:

========
Aerosols
========

.. _customguide-aer-mp:

Aerosol microphysics
--------------------

GEOS-Chem incorporates two different aerosol microphysics schemes: APM
(:cite:t:`Yu_and_Luo_2009`) and TOMAS
(:cite:t:`Trivitayanurak_et_al._2008`) as compile-time options for the
full-chemistry simulation.  Both APM and TOMAS are deactivated by
default due to the extra computational overhead that these
microphysics schemes require.

Follow the steps below to activate either APM or TOMAS microphysics in
your full-chemistry simulation.

APM
~~~

#. Create a run directory for the Full Chemistry simulation with APM
   as the extra simulation option.
#. Navigate to the :file:`build` folder within the run directory.
#. Then type the following:

   .. code-block:: console

      $ cmake .. -DAPM=y
      $ make -j
      $ make install

TOMAS
~~~~~

#. Create a run directory for the Full Chemistry simulation with TOMAS
   as the extra simulation option.
#. Navigate to the :file:`build` folder within the run directory.
#. Then type the following:

   .. code-block:: console

      $ cmake .. -DTOMAS=y -DTOMAS_BINS=15 -DBPCH_DIAG=y
      $ make -j
      $ make install

This will create a GEOS-Chem executable for the TOMAS15 (15 size bins)
simulation.  To generate an executable for the TOMAS40 (40 size-bins)
simulation, replace :code:`-DTOMAS_BINS=15` with
:code:`-DTOMAS_BINS=40` in the :literal:`cmake` step above.

.. _customguide-chem:

=========
Chemistry
=========

.. _customguide-chem-kpp:

Adaptive Rosenbrock solver with mechanism auto-reduction
--------------------------------------------------------

In :cite:t:`Lin_et_al._2023`, the authors introduce an `adaptive
Rosenbrock solver with on-the-fly mechanism reduction
<https://kpp.readthedocs.io/en/stable/tech_info/07_numerical_methods.html#rosenbrock-with-mechanism-auto-reduction>`_
in `The Kinetic PreProcessor (KPP) <https://kpp.readthedocs.io>`_
version 3.0.0 and later.  While this adaptive solver is available for all
GEOS-Chem simulations that use the :literal:`fullchem` simulation, it
is disabled by default.

To activate the adaptive Rosenbrock solver with mechanism
auto-reduction, edit the line of :file:`geoschem_config.yml` indicated
below:

.. code-block:: yaml

   chemistry:
     activate: true
     # ... Previous sub-sections omitted
     autoreduce_solver:
       activate: false   # <== true activates the adaptive Rosenbrock solver
       use_target_threshold:
         activate: true
         oh_tuning_factor: 0.00005
         no2_tuning_factor: 0.0001
       use_absolute_threshold:
         scale_by_pressure: true
         absolute_threshold: 100.0
       keep_halogens_active: false
       append_in_internal_timestep: false

Please see the :cite:t:`Lin_et_al._2023` reference for a detailed
explanation of the other adaptive Rosenbrock solver options.

.. _customguide-chem-mech:

Alternate chemistry mechanisms
------------------------------

GEOS-Chem is compiled "out-of-the-box" with KPP-generated solver code
for the :literal:`fullchem` mechanism.  But you must manually specify
the mechanism name at configuration time for the following instances:

Carbon mechanism
~~~~~~~~~~~~~~~~

Follow these steps to build an executable with the :literal:`carbon`
mechanism:

#. Create a run directory for the Carbon simulation
#. Navigate to the :file:`build` folder within the run directory.
#. Then type the following:

   .. code-block:: console

      $ cmake .. -DMECH=carbon
      $ make -j
      $ make install

Custom full-chemistry mechanism
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

We recommend that you use the :literal:`custom` mechanism instead of
directly modifying the :literal:`fullchem` mechanism.  The
:literal:`custom` mechanism is a copy of :literal:`fullchem`, but the
KPP solver code will be generated in the :file:`KPP/custom`
folder instead of in :file:`KPP/fullchem`.  This lets you keep the
:literal:`fullchem` folder untouched.

Follow these steps:

#. Create a run directory for the full-chemistry simulation (whichever
   configuration you need).
#. Navigate to the :file:`build` folder within the run directory.
#. Then type the following:

   .. code-block:: console

      $ cmake .. -DMECH=custom
      $ make -j
      $ make install

Hg mechanism
~~~~~~~~~~~~
Follow these steps to build an executable with the :literal:`Hg` (mercury)
mechanism:

#. Create a run directory for the Hg simulation.
#. Navigate to the :file:`build` folder within the run directory.\
#. Then type the following:

   .. code-block:: console

      $ cmake .. -DMECH=Hg
      $ make -j
      $ make install

.. _customguide-chem-ho2:

HO2 heterogeneous chemistry reaction probability
------------------------------------------------

You may update the value of :math:`\gamma_{HO2}` (reaction probability for
uptake of HO2 in heterogeneous chemistry) used in your simulations.
Edit the line of :file:`geoschem_config.yml` indicated below:

.. code-block:: yaml

   chemistry:
     activate: true
     # ... Preceding sections omitted ...
     gamma_HO2: 0.2   # <=== add new value here

.. _customguide-chem-pasv:

TransportTracers
-----------------

In GEOS-Chem 14.2.0 and later versions, species belonging to the
TransportTracers simulation (radionuclides and passive species) now
have their properties defined in the :file:`species_database.yml`
file.  For example:

.. code-block:: yaml

   CH3I:
     Background_VV: 1.0e-20
     Formula: CH3I
     FullName: Methyl iodide
     Henry_CR: 3.6e+3
     Henry_K0: 0.20265
     Is_Advected: true
     Is_Gas: true
     Is_Photolysis: true
     Is_Tracer: true
     Snk_Horiz: all
     Snk_Mode: efolding
     Snk_Period: 5
     Snk_Vert: all
     Src_Add: true
     Src_Mode: HEMCO
     MW_g: 141.94

where:

- :literal:`Is_Tracer: true` indicates a TransportTracer species
- :literal:`Snk_*` define species sink properties
- :literal:`Src_*` define species source properties
- :literal:`Units`: specifies the default units for species (added mainly
  for age of air species at this time which are in :literal:`days`)


For TransportTracers species that have a source term in HEMCO, there
will be corresponding entries in :file:`HEMCO_Config.rc`:

.. code-block:: kconfig

    --> OCEAN_CH3I             :       true

    # ... etc ...

    #==============================================================================
    # CH3I emitted over the oceans at rate of 1 molec/cm2/s
    #==============================================================================
    (((OCEAN_CH3I
    0 SRC_2D_CH3I 1.0 - - - xy molec/cm2/s CH3I 1000 1 1
    )))OCEAN_CH3I

Sources and sinks for TransportTracers are now applied in the new source
code module :file:`GeosCore/tracer_mod.F90`.

.. note::

   Sources and sinks for radionuclide species (Rn, Pb, Be isotopes)
   are currently not applied in :file:`GeosCore/tracer_mod.F90` (but
   may be in the future).  Emissions for radionuclide species are
   computed by the HEMCO :literal:`GC-Rn-Pb-Be` extension and
   chemistry is done in :file:`GeosCore/RnPbBe_mod.F90`.

   TransportTracer properties for radionuclide species have been
   added to :file:`species_database.yml` but are currently commented
   out.

.. _customguide-diag:

===========
Diagnostics
===========

GEOS-Chem and HEMCO diagnostics
-------------------------------

Please see our `Diagnostics reference
<https://geos-chem.readthedocs.io/en/latest/gcclassic-user-guide/diagnostics.html>`_
chapter for an overview of how to archive diagnostics from GEOS-Chem
and HEMCO.

RRTMG radiative transfer diagnostics
------------------------------------
You can use the RRTMG radiative transfer model to archive radiative
forcing fluxes to the :literal:`GeosRad` History diagnostic
collection.  RRTMG is implemented as a compile-time option due to the
extra computational overhead that it incurs.

To activate RRTMG, follow these steps:

#. Create a run directory for the Full Chemistry simulation, with
   extra option RRTMG.
#. Navigate to the :file:`build` folder within the run directory.
#. Then type the following:

   .. code-block:: console

      $ cmake .. -DRRTMG=y
      $ make -j
      $ make install

Then also make sure to request the radiative forcing flux diagnostics
that you wish to archive in the :literal:`HISTORY.rc` file.

.. _customguide-emis:

=========
Emissions
=========

.. _customguide-emis-offline:

Offline vs. online emissions
----------------------------

Emission inventories sometimes include dynamic source types and
nonlinear scale factors that have functional dependencies on local
environmental variables such as wind speed or temperature, which are
best calculated online during execution of the model. HEMCO includes a
suite of additional modules (aka `HEMCO extensions
<https://hemco.readthedocs.io/en/stable/hco-ref-guide/extensions.html>`_)
that perform **online emissions** ccalculations for a variety of
sources.

Some types of emissions are highly sensitive to meteorological
variables such as wind speed and temperature.  Because the
meteorological inputs are regridded from their native resolution to
the GEOS-Chem or HEMCO simulation grid, emissions computed with
fine-resolution meteorology can significantly differ from emissions
computed with coarse-resolution meteorology.  This can make it
difficult to compare the output of GEOS-Chem and HEMCO simulations
that use different horizontal resolutions.

In order to provide more consistency in the computed emissions, we now
make available for download **offline emissions**. These offline
emissions are pre-computed with HEMCO standalone simulations using
meteorological inputs at native horizontal resolutions possible.  When
these emissions are regridded within GEOS-Chem and HEMCO, the total
mass emitted will be conserved regardless of the horizontal resolution
of the simulation grid.

You should use offline emissions:

- For all GCHP simulations
- For full-chemistry simulations (except benchmark)

You should use online emissions:

- For benchmark simulations
- If you wish to assess the impact of changing/updating the
  meteorlogical inputs on emissions.

You may toggle offline emissions on (:literal:`true`) or off
(:literal:`false`) in this section of :file:`HEMCO_Config.rc`:

.. code-block:: kconfig

   # ----- OFFLINE EMISSIONS -----------------------------------------------------
   # To use online emissions instead set the offline emissions to 'false' and the
   # corresponding HEMCO extension to 'on':
   #   OFFLINE_DUST        - DustDead or DustGinoux
   #   OFFLINE_BIOGENICVOC - MEGAN
   #   OFFLINE_SEASALT     - SeaSalt
   #   OFFLINE_SOILNOX     - SoilNOx
   #
   # NOTE: When switching between offline and online emissions, make sure to also
   # update ExtNr and Cat in HEMCO_Diagn.rc to properly save out emissions for
   # any affected species.
   #------------------------------------------------------------------------------
       --> OFFLINE_DUST           :       true     # 1980-2019
       --> OFFLINE_BIOGENICVOC    :       true     # 1980-2020
       --> OFFLINE_SEASALT        :       true     # 1980-2019
       -->  CalcBrSeasalt         :       true
       --> OFFLINE_SOILNOX        :       true     # 1980-2020

As stated in the comments, if you switch between offline and online
emissions, you will need to activate the corresponding HEMCO
extension:

.. table:: Offline emissions and corresponding HEMCO extensions
   :align: center

   +-----------------------+-------------+-------------------------------+-------------+
   | Offline base emission | Extension # | Corresponding HEMCO extension | Extension # |
   +=======================+=============+===============================+=============+
   | OFFLINE_DUST          | 0           | DustDead                      | 105         |
   +-----------------------+-------------+-------------------------------+-------------+
   | OFFLINE_BIOGENICVOC   | 0           | MEGAN                         | 108         |
   +-----------------------+-------------+-------------------------------+-------------+
   | OFFLINE_SEASALT       | 0           | SeaSalt                       | 107         |
   +-----------------------+-------------+-------------------------------+-------------+
   | OFFLINE_SOILNOX       | 0           | SoilNOx                       | 104         |
   +-----------------------+-------------+-------------------------------+-------------+

Example: Disabling offline dust emissions
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#. Change the :literal:`OFFLINE_DUST` setting from :literal:`true` to
   :literal:`false` in :file:`HEMCO_Config.rc`:

   .. code-block:: kconfig

      --> OFFLINE_DUST           :       false    # 1980-2019

#. Change the :literal:`DustDead` extension setting from
   :literal:`off` to :literal:`on` in :file:`HEMCO_Config.rc`:

   .. code-block:: kconfig

      105     DustDead               : on    DST1/DST2/DST3/DST4

#. Change the extension number for all dust emission diagnostics from
   :literal:`0` (the extension number for base emissions) to
   :literal:`105` (the extension number for :literal:`DustDead`)
   in :file:`HEMCO_Diagn.rc`.

   .. code-block:: kconfig

      ###############################################################################
      #####  Dust emissions                                                     #####
      ###############################################################################
      EmisDST1_Total     DST1   -1    -1   -1   2   kg/m2/s  DST1_emission_flux_from_all_sectors
      EmisDST1_Anthro    DST1  105     1   -1   2   kg/m2/s  DST1_emission_flux_from_anthropogenic
      EmisDST1_Natural   DST1  105     3   -1   2   kg/m2/s  DST1_emission_flux_from_natural_sources
      EmisDST2_Natural   DST2  105     3   -1   2   kg/m2/s  DST2_emission_flux_from_natural_sources
      EmisDST3_Natural   DST3  105     3   -1   2   kg/m2/s  DST3_emission_flux_from_natural_sources
      EmisDST4_Natural   DST4  105     3   -1   2   kg/m2/s  DST4_emission_flux_from_natural_sources

To enable online emissions again, do the inverse of the steps listed above.

.. _customguide-chem-ssdb:

Sea salt debromination
----------------------

In Zhu *et al.* [`2018
<https://acp.copernicus.org/articles/19/6497/2019/>`_], the authors
present a mechanistic description of sea salt aerosol debromination.
This option was originally enabled by in GEOS-Chem 13.4.0, but
was then changed to be an option (disabled by default) due to the
impact it had on ozone concentrations.

Further chemistry updates to GEOS-Chem have allowed us to re-activate
sea-salt debromination as the default option in GEOS-Chem 14.2.0 and
later versions.  If you wish to disable sea salt debromination in your
simulations, edit the line in :file:`HEMCO_Config.rc` indicated below.

.. code-block:: kconfig

   107     SeaSalt                : on  SALA/SALC/SALACL/SALCCL/SALAAL/SALCAL/BrSALA/BrSALC/MOPO/MOPI
       # ... Preceding options omitted ...
       --> Model sea salt Br-     :       true    # <== false deactivates sea salt debromination
       --> Br- mass ratio         :       2.11e-3

.. _chemguide-phot:

==========
Photolysis
==========

.. _customguide-phot-np:

Particulate nitrate photolysis
------------------------------
A study by Shah *et al.* [`2023
<https://doi.org/10.5194/acp-23-1227-2023>`_] showed that particulate
nitrate photolysis increases GEOS-Chem modeled ozone concentrations by
up to 5 ppbv in the free troposphere in northern extratropical
regions.  This helps to correct a low bias with respect to
observations.

Particulate nitrate photolysis is turned on by default in GEOS-Chem
14.2.0 and later versions.  You may disable this option by editing
the line in :file:`geoschem_config.yml` indicated below:

.. code-block:: yaml

   photolysis:
     activate: true
     # .. preceding sub-sections omitted ...
     photolyze_nitrate_aerosol:
       activate: true   # <=== false deactivates nitrate photolysis
       NITs_Jscale_JHNO3: 100.0
       NIT_Jscale_JHNO2: 100.0
       percent_channel_A_HONO: 66.667
       percent_channel_B_NO2: 33.333

You can also edit the other nitrate photolysis parameters by changing
the appropriate lines above.  See the Shah et al [2023] reference for
more information.

.. _customguide-wetd:

==============
Wet deposition
==============

.. _customguide-wetd-luo:

Luo et al 2020 wetdep parameterization
--------------------------------------

In :cite:t:`Luo_et_al._2020`, the authors introduced an updated wet
deposition parameterization, which is now incorporated into GEOS-Chem as a
compile-time option.  Follow these steps to activate the Luo et al
2020 wetdep scheme in your GEOS-Chem simulations.

#. Create a run directory for the type of simulation that you wish to
   use.

   - CAVEAT: Make sure your simulation uses at least one species that
     can be wet-scavenged.

#. Navigate to the :file:`build` folder within the run directory.
#. Then type the following:

   .. code-block:: console

      $ cmake .. -DLUO_WETDEP=y
      $ make -j
      $ make install
