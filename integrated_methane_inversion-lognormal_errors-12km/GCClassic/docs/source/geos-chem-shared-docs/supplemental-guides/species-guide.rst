 .. _spcguide:

#################################
View GEOS-Chem species properties
#################################

Properties for GEOS-Chem species are stored in the **GEOS-Chem
Species Database**, which is a `YAML <https://yaml.org>`_ file
(:file:`species_database.yml`) that is placed into each GEOS-Chem run
directory.

View species properties from the current stable GEOS-Chem version:

- `View properties for most GEOS-Chem species <https://github.com/geoschem/geos-chem/blob/main/run/shared/species_database.yml>`_
- `View properties for APM microphysics species <https://github.com/geoschem/geos-chem/blob/main/run/shared/species_database_apm.yml>`_
- `View properties for TOMAS microphysics species <https://github.com/geoschem/geos-chem/blob/main/run/shared/species_database_tomas.yml>`_
- `View properties for Hg simulation species <https://github.com/geoschem/geos-chem/blob/main/run/shared/species_database_hg.yml>`_

.. _spcguide-defs:

==========================
Species properties defined
==========================

The following sections contain a detailed description of GEOS-Chem
species properties.

.. _spcguide-defs-defaults:

Required default properties
---------------------------

All GEOS-Chem species should have these properties defined:

.. code-block:: YAML

         Name:
           FullName: full name of the species
           Formula: chemical formula of the species
           MW_g: molecular weight of the species in grams
   EITHER  Is_Gas: true
   OR      Is_Aerosol: true

All other properties are species-dependent.  You may omit properties
that do not apply to a given species. GEOS-Chem will assign a "missing
value" (e.g. :code:`false`, :code:`-999`, :code:`-999.0`, or,
:code:`UNKNOWN`) to these properties when it reads the
:file:`species_database.yml` file from disk.

.. _spcguide-defs-id:

Identification
--------------

.. option:: Name

   Species short name (e.g. :literal:`ISOP`).

.. option:: Formula

   Species chemical formula (e.g. :literal:`CH2=C(CH3)CH=CH2`).  This
   is used to define the species' :literal:`formula` attribute, which
   gets written to GEOS-Chem diagnostic files and restart files.

.. option:: FullName

   Species long name (e.g. :literal:`Isoprene`).  This is used to
   define the species' :literal:`long_name` attribute, which gets
   written to GEOS-Chem diagnostic files and restart files.

.. option:: Is_Aerosol

   Indicates that the species is an aerosol (:literal:`true`), or isn't
   (:literal:`false`).

.. option:: Is_Advected

   Indicates that the species is advected (:literal:`true`), or isn't
   (:literal:`false`).

.. option:: Is_DryAlt

   Indicates that dry deposition diagnostic quantities for the species can
   be archived at a specified altitude above the surface
   (:literal:`true`), or can't (:literal:`false`).

   .. note::

      The :code:`Is_DryAlt` flag only applies to species
      :literal:`O3` and :literal:`HNO3`.

.. option:: Is_DryDep

   Indicates that the species is dry deposited (:literal:`true`), or
   isn't (:literal:`false`).

.. option:: Is_HygroGrowth

   Indicates that the species is an aerosol that is capable of
   hygroscopic growth (:literal:`true`), or isn't (:literal:`false`).

.. option:: Is_Gas

   Indicates that the species is a gas (:literal:`true`), or isn't
   (:literal:`false`).

.. option:: Is_Hg0

   Indicates that the species is elemental mercury (:literal:`true`),
   or isn't (:literal:`false`).

.. option:: Is_Hg2

   Indicates that the species is a mercury compound with oxidation
   state +2 (:literal:`true`), or isn't (:literal:`false`).

.. option:: Is_HgP

   Indicates that the species is a particulate mercury compound
   (:literal:`true`), or isn't (:literal:`false`).

.. option:: Is_Photolysis

   Indicates that the species is photolyzed (:literal:`true`), or isn't
   (:literal:`false`).

.. option:: Is_RadioNuclide

   Indicates that the species is a radionuclide (:literal:`true`), or
   isn't (:literal:`false`).

.. _spcguide-defs-physprop:

Physical properties
-------------------

.. option:: Density

   Density (:math:`kg\ m^{-3}`) of the species.  Typically defined
   only for aerosols.

.. option:: Henry_K0

   Henry's law solubility constant (:math:`M\ atm^{-1}`), used by the
   default wet depositon scheme.

.. option:: Henry_K0_Luo

   Henry's law solubility constant (:math:`M\ atm^{-1}`) used by the
   :cite:t:`Luo_et_al._2020` wet deposition scheme.

.. option:: Henry_CR

   Henry's law volatility constant (:math:`K`) used by the default
   wet deposition scheme.

.. option:: Henry_CR_Luo

   Henry's law volatility constant (:math:`K`) used by the
   :cite:t:`Luo_et_al._2020` wet deposition scheme.

.. option:: Henry_pKa

   Henry's Law pH correction factor.

.. option:: MW_g

   Molecular weight (:math:`g\ mol^{-1}`) of the species.

   .. note::

      Some aerosol-phase species (such as MONITA and IONITA) are given
      the molar mass corresponding to the number of nitrogens that
      they carry, whereas gas-phase species (MONITS and MONITU) get
      the full molar mass of the compounds that they represent.  This
      treatment has its origins in `J. Fisher et al
      [2016] <https://acp.copernicus.org/articles/16/5969/2016/acp-16-5969-2016.pdf>`_.

.. option:: Radius

   Radius (:math:`m`) of the species.  Typically defined only for
   aerosols.

.. _spcguide-defs-drydep:

Dry deposition properties
-------------------------

.. option:: DD_AeroDryDep

   Indicates that dry deposition should consider hygroscopic growth
   for this species (:literal:`true`), or shouldn't
   (:literal:`false`).

   .. note::

     :code:`DD_AeroDryDep` is only defined for sea salt aerosols.

.. option:: DD_DustDryDep

   Indicates that dry deposition should exclude hygroscopic growth for
   this species (:literal:`true`), or shouldn't (:literal:`false`).

   .. note::

     :code:`DD_DustDryDep` is only defined for mineral dust
     aerosols.

.. option:: DD_DvzAerSnow

   Specifies the dry deposition velocity (:math:`cm\ s^{-1}`) over
   ice and snow for certain aerosol species.  Typically,
   :code:`DD_DvzAerSnow = 0.03`.

.. option:: DD_DvzAerSnow_Luo

   Specifies the dry deposition velocity (:math:`cm\ s^{-1}`) over
   ice and snow for certain aerosol species.

   .. note::

      :code:`DD_DvzAerSnow_Luo` is only used when the
      :cite:t:`Luo_et_al._2020` wet scavenging scheme is activated.

.. option:: DD_DvzMinVal

   Specfies minimum dry deposition velocities (:math:`cm\ s^{-1}`) for
   sulfate  species (:literal:`SO2`, :literal:`SO4`, :literal:`MSA`,
   :literal:`NH3`, :literal:`NH4`, :literal:`NIT`).  This follows the
   methodology of the GOCART model.

   :code:`DD_DvzMinVal` is defined as a two-element vector:

   - :code:`DD_DvzMinVal(1)` sets a minimum dry deposition velocity
     onto snow and ice.
   - :code:`DD_DvzMinVal(2)` sets a minimum dry deposition velocity
     over land.

.. option:: DD_Hstar_Old

   Specifies the Henry's law constant (:math:`K_0`) that is used in
   dry deposition.  This will be used to assign the :code:`HSTAR`
   variable in the GEOS-Chem dry deposition module.

   .. note::

      The value of the :code:`DD_Hstar_old` parameter was tuned for
      each species so that the dry deposition velocity would match
      observations.

.. option:: DD_F0

   Specifies the reactivity factor for oxidation of biological
   substances in dry deposition.

.. option:: DD_KOA

   Specifies the octanal-air partition coefficient, used for the dry
   deposition of species :code:`POPG`.

   .. note::

      :code:`DD_KOA` is only used in the `POPs simulation
      <https://wiki.geos-chem.org/POPs_simulation>`_.

.. _spcguide-defs-wetdep:

Wet deposition properties
-------------------------

.. option:: WD_Is_H2SO4

   Indicates that the species is :literal:`H2SO4` (:literal:`true`),
   or isn't (:literal:`false)`.  This allows the wet deposition code
   to perform special calculations when computing  :literal:`H2SO4`
   rainout and washout.

.. option:: WD_Is_HNO3

   Indicates that the species is :literal:`HNO3` (:literal:`true`),
   or isn't (:literal:`false)`.  This allows the wet deposition code
   to perform special calculations when computing  :literal:`HNO3`.
   rainout and washout.

.. option:: WD_Is_SO2

   Indicates that the species is :literal:`SO2` (:literal:`true`),
   or isn't (:literal:`false)`.  This allows the wet deposition code
   to perform special calculations when computing :literal:`SO2`
   rainout and washout.

.. option:: WD_CoarseAer

   Indicates that the species is a coarse aerosol (:literal:`true`),
   or isn't (:literal:`false`).  For wet deposition purposes, the
   definition of coarse aerosol is radius > 1 :math:`\mu m`.

.. option:: WD_LiqAndGas

   Indicates that the the ice-to-gas ratio can be computed for
   this species by co-condensation (:literal:`true`), or can't
   (:literal:`false`).

.. option:: WD_ConvFacI2G

   Specifies the conversion factor (i.e. ratio of sticking
   coefficients on the ice surface) for computing the ice-to-gas ratio
   by co-condensation, as used in the default wet deposition scheme.

   .. note::

      :code:`WD_ConvFacI2G` only needs to be defined for those species
      for which :code:`WD_LiqAndGas` is :literal:`true`.

.. option:: WD_ConvFacI2G_Luo

   Specifies the conversion factor (i.e. ratio of sticking
   coefficients on the ice surface) for computing the ice-to-gas ratio
   by co-condensation, as used in the :cite:t:`Luo_et_al._2020` wet
   deposition scheme.

   .. note::

      :code:`WD_ConvFacI2G_Luo` only needs to be defined for those species
      for which :code:`WD_LiqAndGas` is :literal:`true`, and is only
      used when the :cite:t:`Luo_et_al._2020` wet deposition scheme is
      activated.

.. option:: WD_RetFactor

   Specifies the retention efficiency :math:`R_i` of species in the
   liquid cloud condensate as it is converted to precipitation.
   :math:`R_i` < 1 accounts for volatization during riming.

.. option:: WD_AerScavEff

   Specifies the aerosol scavenging efficiency. This factor multiplies
   :math:`F`, the fraction of aerosol species that is lost to
   convective updraft scavenging.

   - :code:`WD_AerScavEff = 1.0` for most aerosols.
   - :code:`WD_AerScavEff = 0.8` for secondary organic aerosols.
   - :code:`WD_AerScavEff = 0.0` for hydrophobic aerosols.

.. option:: WD_KcScaleFac

   Specifies a temperature-dependent scale factor that is used to
   multiply :math:`K` (aka :math:`K_c`), the rate constant for
   conversion of cloud condensate to precipitation.

   :code:`WD_KcScaleFac` is defined as a 3-element vector:

   - :code:`WD_KcScaleFac(1)` multiplies :math:`K` when
     :math:`T < 237` kelvin.
   - :code:`WD_KcScaleFac(2)` multiplies :math:`K` when
     :math:`237 \le T < 258` kelvin
   - :code:`WD_KcScaleFac(3)` multiplies :math:`K` when
     :math:`T \ge 258` kelvin.

.. option:: WD_KcScaleFac_Luo

   Specifies a temperature-dependent scale factor that is used to
   multiply :math:`K`, aka :math:`K_c`, the rate constant for
   conversion of cloud condensate to precipitation.

   Used only in the :cite:t:`Luo_et_al._2020` wet deposition scheme.

   :code:`WD_KcScaleFac_Luo` is defined as a 3-element vector:

   - :code:`WD_KcScaleFac_Luo(1)` multiplies :math:`K` when
     :math:`T < 237` kelvin.
   - :code:`WD_KcScaleFac_Luo(2)` multiplies :math:`K` when
     :math:`237 \le T < 258` kelvin.
   - :code:`WD_KcScaleFac_Luo(3)` multiplies :math:`K` when
     :math:`T \ge 258` kelvin.

.. option:: WD_RainoutEff

   Specifies a temperature-dependent scale factor that is used to
   multiply :math:`F_i` (aka :literal:`RAINFRAC`), the fraction of
   species scavenged by rainout.

   :code:`WD_RainoutEff` is defined as a 3-element vector:

   - :code:`WD_RainoutEff(1)` multiplies :math:`F_i` when
     :math:`T < 237` kelvin.
   - :code:`WD_RainoutEff(2)` multiplies :math:`F_i` when
     :math:`237 \le T < 258` kelvin.
   - :code:`RainoutEff(3)` multiplies :math:`F_i` when
     :math:`T \ge 258` kelvin.

   This allows us to better simulate scavenging by snow and impaction
   scavenging of BC.  For most species, we need to be able to turn off
   rainout  when :math:`237 \le T <  258` kelvin. This can be easily
   done by setting :code:`RainoutEff(2) = 0`.

   .. note::

      For SOA species, the maximum value of :code:`WD_RainoutEff` will
      be 0.8 instead of 1.0.

.. option:: WD_RainoutEff_Luo

   Specifies a temperature-dependent scale factor that is used to
   multiply :math:`F_i` (aka :literal:`RAINFRAC`), the fraction of
   species scavenged by rainout. (Used only in the
   :cite:`Luo_et_al._2020` wet deposition scheme).

   :code:`WD_RainoutEff_Luo` is defined as a 3-element vector:

   - :code:`WD_RainoutEff_Luo(1)` multiplies :math:`F_i` when
     :math:`T < 237` kelvin.
   - :code:`WD_RainoutEff_Luo(2)` multiplies :math:`F_i` when
     :math:`237 \le T < 258` kelvin.
   - :code:`RainoutEff_Luo(3)` multiplies :math:`F_i` when
     :math:`T \ge 258` kelvin.

   This allows us to better simulate scavenging by snow and impaction
   scavenging of BC.  For most species, we need to be able to turn off
   rainout when :math:`237 \le T <  258` kelvin. This can be easily
   done by setting :code:`RainoutEff(2) = 0`.

   .. note::

      For SOA species, the maximum value of :code:`WD_RainoutEff_Luo`
      will  be 0.8 instead of 1.0.

.. _spcguide-defs-tracer:

Transport tracer properties
---------------------------

These properties are defined for species used in the TransportTracers
simulation.  We will refer to these species as **tracers**.

.. option:: Is_Tracer

   Indicates that the species is a transport tracer (:literal:`true`),
   or is not (:literal:`false`).

.. option:: Snk_Horiz

   Specifies the horizontal domain of the tracer sink term.  Allowable
   values are:

   .. option:: all

      The tracer sink term will be applied throughout the entire
      horizonatal domain of the simulation grid.

   .. option:: lat_zone

      The tracer sink term will be applied only within the latitude
      range specified by :option:`Snk_Lats`.

.. option:: Snk_Lats

   Defines the latitude range :literal:`[min_latitude, max_latitude]` for the
   tracer sink term.  Will only be used if :option:`Snk_Horiz` is
   set to :literal:`lat_zone`.

.. option:: Snk_Mode

   Specifies how the tracer sink term will be applied.  Allowable values are:

   .. option:: constant

      The tracer sink term is a constant value (specified by
      :option:`Snk_Value`).

   .. option:: efolding

      The tracer sink term has an e-folding decay constant (specified in
      :option:`Snk_Period`).

   .. option:: halflife

      A tracer sink term has a half-life (specified in
      :option:`Snk_Period`).

   .. option:: none

      The tracer does not have a sink term.

.. option:: Snk_Period

   Specifies the period (in days) for which the tracer sink term
   will be applied.

.. option:: Snk_Value

   Specifies a value for the tracer sink term.

.. option:: Snk_Vert

   Specifies the vertical domain of the tracer sink term. Allowable
   values are:

   .. option:: all

      The tracer sink term will be applied throughout the entire
      vertical domain of the simulation grid.

   .. option:: boundary_layer

      The tracer sink term will only be applied within the planetary
      boundary layer.

   .. option:: surface

      The tracer sink term will only be applied at the surface.

   .. option:: troposphere

      The tracer sink term will only be applied within the troposphere.

.. option:: Src_Add

   Specifies whether the tracer has a source term (:literal:`true`) or
   not (:literal:`false`).

.. option:: Src_Horiz

   Specifies the horizontal domain of the tracer source term.
   Allowable values are:

   .. option:: all

      The tracer source term will be applied across the entire
      horizontal extent of the simulation grid.

   .. option:: lat_zone

      The tracer source term will only be applied within the latitude
      range specified by :option:`Src_Lats`.

.. option:: Src_Lats

   Defines the latitude range :literal:`[min_latitude, max_latitude]` for the
   tracer source term.  Will only be applied if :option:`Src_Horiz` is
   set to :literal:`lat_zone`.

.. option:: Src_Mode

   Describes the type of tracer source term.  Allowable values are:

   .. option:: constant

      The tracer source term is a constant value (specified by
      :option:`Src_Value`).

   .. option:: decay_of_another_species

      The tracer source term comes from the decay of
      another species (e.g. Pb210 source comes from Rn222 decay).

   .. option:: HEMCO

      The tracer source term will be read from a file via HEMCO.

   .. option:: maintain_mixing_ratio

      The tracer source term will be calculated as needed
      to maintain a constant mixing ratio at the surface.

   .. option:: none

      The tracer does not have a source term.

.. option:: Src_Unit

   Specifies the unit of the source term that will be applied to the
   tracer.

   .. option:: ppbv

      The source term has units of parts per billion by volume.

   .. option:: timestep

      The source term has units of per emissions timestep.

.. option:: Src_Value

   Specifies a value for the tracer source term in :option:`Src_Units`.

.. option:: Src_Vert

   Specifies the vertical domain of the tracer source term.  Allowable
   values are:

   .. option:: all

      The tracer source term will be applied throughout the entire
      vertical domain of the simulation grid.

   .. option:: pressures

      The tracer source term will only be applied within the pressure
      range specified in :option:`Src_Pressures`.

   .. option:: stratosphere

      The tracer source term will only be applied in the stratosphere.

   .. option:: troposphere

      The tracer source term will only be applied in the troposphere.

   .. option:: surface

      The tracer source term will only be applied at the surface.

.. option:: Src_Pressures

   Defines the pressure range :literal:`[min_pressure, max_pressure]`,
   in hPa for the tracer source term.  Will only be used
   if :option:`Src_Vert` is set to :literal:`pressures`.

.. option:: Units

   Specifies the default units of the tracers (e.g. :literal:`aoa`,
   :literal:`aoa_nh`, :literal:`aoa_bl` are carried in units :literal:`days`,
   while all other species in GEOS-Chem are :literal:`kg/kg dry air`).

.. _spcguide-defs-tracer-prop:

Properties used by each transport tracer
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

The list below shows the various :ref:`transport tracer properties <spcguide-defs-tracer>`
that are used in the current TransportTracers simulation.

.. code-block:: none

   Is_Tracer
    - true                     : all

   Snk_Horiz:
    - lat_zone                 : aoa_nh
    - all                      : all others

   Snk_Lats
    - 30 50                    : aoa_nh

   Snk_Mode
    - constant                 : aoa, aoa_bl, aoa_nh
    - efolding                 : CH3I, CO_25
    - none                     : SF6
    - halflife                 : Be7, Be7s, Be10, Be10s

   Snk_Period (days)
    - 5                        : CH3I
    - 25                       : CO_25
    - 50                       : CO_50
    - 90                       : e90, e90_n, e90_s
    - 11742.8                  : Pb210, Pb210s
    - 5.5                      : Rn222
    - 53.3                     : Be7, Be7s
    - 5.84e8                   : Be10, Be10s

   Snk_Value
    - 0                        : aoa, aoa_bl, aoa_nh

   Snk_Vert
    - boundary_layer           : aoa_bl
    - surface                  : aoa, aoa_nh
    - troposphere              : stOx
    - all                      : all others

   Src_Add
    - false                    : Passive, stOx, st80_25
    - true                     : all others

   Src_Horiz
    - lat_zone                 : e90_n, e90_s, nh_5, nh_50
    - all                      : all others

   Src_Lats
    - [ 40.0,   91.0]          : e90_n
    - [-91.0,  -40.0]          : e90_s
    - [ 30.0,   50.0]          : nh_5, nh_50

   Src_Mode
    - constant                 : aoa, aoa_bl, aoa_nh, nh_50, nh_5, st80_25
    - file2d                   : CH3I, CO_25, CO_50, Rn222, SF6  - HEMCO
    - file3d                   : Be10, Be7                       - HEMCO
    - maintain_mixing_ratio    : e_90, e90_n, e90_s
    - decay_of_another_species : Pb210, Pb210s

   Src_Unit
    - ppbv                     : e90, e90_n, e90_s, st80_25
    - timestep                 : aoa, aoa_bl, aoa_nh

   Src_Value
    - 1                        : aoa, aoa_bl, aoa_nh
    - 100                      : e90, e90_n, e90_s
    - 200                      : st80_25

   Src_Vert
    - all                      : aoa, aoa_bl, aoa_nh, Pb210
    - pressures                : st80_25
    - stratosphere             : Be10s, Be7s, Pb210s, stOx
    - surface                  : all others (not specified when Src_Mode: HEMCO)

   Src_Pressures
    - [0, 80]                  : st80_25

   Units
    - days                     : aoa, aoa_bl, aoa_bl

.. _spcguide-defs-other:

Other properties
----------------

.. option:: BackgroundVV

   If a restart file does not contain an global initial concentration
   field for a species, GEOS-Chem will attempt to set the initial
   concentration (in :math:`vol\ vol^{-1}` dry air) to the value
   specified in :code:`BackgroundVV` globally.   But if
   :code:`BackgroundVV` has not been specified, GEOS-Chem will set
   the initial concentration for the species to :math:`10^{-20}
   vol\ vol^{-1}` dry air instead.

   .. note::

      Recent versions of GCHP may require that all initial conditions
      for all species to be used in a simulation be present in the
      restart file.  See `gchp.readthedocs.io
      <https://gchp.readthedocs.io>`_ for more information.

.. option:: MP_SizeResAer

   Indicates that the species is a size-resolved aerosol species
   (:literal:`true`), or isn't (:literal:`false`).  Used only by
   simulations using either `APM
   <http://wiki.geos-chem.org/APM_aerosol_microphysics>`_
   or `TOMAS <http://wiki.geos-chem.org/TOMAS_aerosol_microphysics>`_
   microphysics packages.

.. option:: MP_SizeResNum

   Indicates that the species is a size-resolved aerosol number
   (:literal:`true`), or isn't (:literal:`false`).  Used only by
   simulations using either `APM
   <http://wiki.geos-chem.org/APM_aerosol_microphysics>`_
   or `TOMAS <http://wiki.geos-chem.org/TOMAS_aerosol_microphysics>`_
   microphysics packages.

.. _spcguide-using:

======================================
Access species properties in GEOS-Chem
======================================

In this section we will describe the derived types and objects that
are used to store GEOS-Chem species properties.  We will also describe
how you can extract species properties from the GEOS-Chem Species
Database when you create new GEOS-Chem code routines.

.. _spcguide-access-spctype:

The Species derived type
-------------------------

The `Species
<https://github.com/geoschem/geos-chem/blob/main/Headers/species_mod.F90#L61>`_
derived type (defined in module :file:`Headers/species_mod.F90`)
describes a complete set of properties for a single GEOS-Chem
species. In addition to the fields mentioned in the preceding sections, the
:code:`Species` derived type also contains several species indices.

.. table:: Indices stored in the :code:`Species` derived type
   :align: center

   +-------------------+----------------------------------+
   | Index             | Description                      |
   +===================+==================================+
   | :code:`ModelId`   | Model species index              |
   +-------------------+----------------------------------+
   | :code:`AdvectId`  | Advected species index           |
   +-------------------+----------------------------------+
   | :code:`AerosolId` | Aerosol species index            |
   +-------------------+----------------------------------+
   | :code:`DryAltId`  | Dry dep species at altitude Id   |
   +-------------------+----------------------------------+
   | :code:`DryDepId`  | Dry deposition species index     |
   +-------------------+----------------------------------+
   | :code:`GasSpcId`  | Gas-phase species index          |
   +-------------------+----------------------------------+
   | :code:`HygGrthId` | Hygroscopic growth species index |
   +-------------------+----------------------------------+
   | :code:`KppVarId`  | KPP variable species index       |
   +-------------------+----------------------------------+
   | :code:`KppFixId`  | KPP fixed spcecies index         |
   +-------------------+----------------------------------+
   | :code:`KppSpcId`  | KPP species index                |
   +-------------------+----------------------------------+
   | :code:`PhotolId`  | Photolyis species index          |
   +-------------------+----------------------------------+
   | :code:`RadNuclId` | Radionuclide index               |
   +-------------------+----------------------------------+
   | :code:`TracerId`  | Transport tracer index           |
   +-------------------+----------------------------------+
   | :code:`WetDepId`  | Wet deposition index             |
   +-------------------+----------------------------------+

.. _spcguide-access-spcptrtype:

The SpcPtr derived type
-----------------------

The `SpcPtr
<https://github.com/geoschem/geos-chem/blob/main/Headers/species_mod.F90#L54>`_
derived type (also defined in :file:`Headers/species_mod.F90`)
describes a container for an object of type :ref:`Species
<spcguide-access-spctype>`.

.. code-block:: fortran

   TYPE, PUBLIC :: SpcPtr
      TYPE(Species), POINTER :: Info   ! Single entry of Species Database
   END TYPE SpcPtr

.. _spcguide-access-spcdata:

The GEOS-Chem Species Database object
-------------------------------------

The GEOS-Chem Species database is stored in the
:code:`State_Chm%SpcData` object.  It describes an array, where each
element of the array is of type :ref:`SpcPtr
<spcguide-access-spcptrtype>` (which is a container for an object of type
type :ref:`Species <spcguide-access-spctype>`.

.. code-block:: fortran

    TYPE(SpcPtr),  POINTER :: SpcData(:)   ! GC Species database

.. _spcguide-access-lookup-ind:

Species index lookup with Ind_()
--------------------------------

Use function :code:`Ind_()` (in module
:code:`Headers/state_chm_mod.F90`) to look up species indices by
name. For example:

.. code-block:: fortran

   SUBROUTINE MySub( ..., State_Chm, ... )

      USE State_Chm_Mod, ONLY : Ind_

      ! Local variables
      INTEGER  :: id_O3, id_Br2, id_CO

      ! Find tracer indices with function the Ind_() function
      id_O3   = Ind_( 'O3'  )
      id_Br2  = Ind_( 'Br2' )
      id_CO   = Ind_( 'CO'  )

      ! Print tracer concentrations
      print*, 'O3  at (23,34,1) : ', State_Chm%Species(id_O3 )%Conc(23,34,1)
      print*, 'Br2 at (23,34,1) : ', State_Chm%Species(id_Br2)%Conc(23,34,1)
      print*, 'CO  at (23,34,1) : ', State_Chm%Species(id_CO )%Conc(23,34,1)

      ! Print the molecular weight of O3 (obtained from the Species Database object)
      print*, 'Mol wt of O3 [g]: ', State_Chm%SpcData(id_O3)%Info%MW_g

   END SUBROUTINE MySub

Once you have obtained the species ID (aka :code:`ModelId`) you can
use that to access the individual fields in the Species Database
object. In the example above, we use the species ID for :literal:`O3` (stored in
:code:`id_O3`) to look up the molecular weight of :literal:`O3` from
the Species Database.

You may search for other model indices with :code:`Ind_()` by passing
an optional second argument:

.. code-block:: fortran

   ! Position of HNO3 in the list of advected species
   AdvectId = Ind_( 'HNO3',  'A' )

   ! Position of HNO3 in the list of gas-phase species
   AdvectId = Ind_( 'HNO3',  'G' )

   ! Position of HNO3 in the list of dry deposited species
   DryDepId = Ind_( 'HNO3',  'D' )

   ! Position of HNO3 in the list of wet deposited species
   WetDepId = Ind_( 'HNO3',  'W' )

   ! Position of HNO3 in the lists of fixed KPP, active, & overall KPP species
   KppFixId = Ind_( 'HNO3',  'F' )
   KppVarId = Ind_( 'HNO3',  'V' )
   KppVarId = Ind_( 'HNO3',  'K' )

   ! Position of SALA in the list of hygroscopic growth species
   HygGthId = Ind_( 'SALA',  'H' )

   ! Position of Pb210 in the list of radionuclide species
   HygGthId = Ind_( 'Pb210', 'N' )

   ! Position of ACET in the list of photolysis species
   PhotolId = Ind( 'ACET',   'P' )

:code:`Ind_()` will return -1 if a species does not belong to any of
the above lists.

.. tip::

   For maximum efficiency, we recommend that you use :code:`Ind_()`
   to obtain the species indices during the initialization phase of a
   GEOS-Chem simulation. This will minimize the number of
   name-to-index lookup operations that need to be performed, thus
   reducing computational overhead.

Implementing the tip mentioned above:

.. code-block:: fortran

   MODULE MyModule

     IMPLICIT NONE
     . . .

     ! Species ID of CO.  All subroutines in MyModule can refer to id_CO.
     INTEGER, PRIVATE :: id_CO

   CONTAINS

     . . .  other subroutines  . . .

     SUBROUTINE Init_MyModule

       ! This subroutine only gets called at startup

       . . .

       ! Store ModelId in the global id_CO variable
       id_CO = Ind_('CO')

       . . .

     END SUBROUTINE Init_MyModule

   END MODULE MyModule

.. _spcguide-access-loop:

Species lookup within a loop
----------------------------

If you need to access species properties from within a loop, it is
better not to use the :code:`Ind_()` function, as repeated
name-to-index lookups will incur computational overhead.  Instead, you
can access the species properties directly from the GEOS-Chem Species
Database object, as shown here.

.. code-block:: fortran

   SUBROUTINE MySub( ..., State_Chm, ... )

      !%%% MySub is an example of species lookup within a loop %%%

      ! Uses
      USE Precision_Mod
      USE State_Chm_Mod, ONLY : ChmState
      USE Species_Mod,   ONLY : Species

      ! Chemistry state object (which also holds the species database)
      TYPE(ChmState), INTENT(INOUT) :: State_Chm

      ! Local variables
      INTEGER                       :: N
      TYPE(Species),  POINTER       :: ThisSpc
      INTEGER                       :: ModelId,  DryDepId, WetDepId
      REAL(fp)                      :: Mw_g
      REAL(f8)                      :: Henry_K0, Henry_CR, Henry_pKa

      ! Loop over all species
      DO N = 1, State_Chm%nSpecies

         ! Point to the species database entry for this species
	 ! (this makes the coding simpler)
	 ThisSpc   => State_Chm%SpcData(N)%Info

         ! Get species properties
	 ModelId   =  ThisSpc%ModelId
         DryDepId  =  ThisSpc%DryDepId
         WetDepId  =  ThisSpc%WetDepId
         MW_g      =  ThisSpc%MW_g
         Henry_K0  =  ThisSpc%Henry_K0
         Henry_CR  =  ThisSpc%Henry_CR
	 Henry_pKa =  ThisSpc%Henry_pKA


         IF ( ThisSpc%Is_Gas )
            ! ... The species is a gas-phase species
            ! ... so do something appropriate
         ELSE
            ! ... The species is an aerosol
            ! ... so do something else appropriate
         ENDIF

         IF ( ThisSpc%Is_Advected ) THEN
            ! ... The species is advected
            ! ... (i.e. undergoes transport, PBL mixing, cloud convection)
         ENDIF

         IF ( ThisSpc%Is_DryDep ) THEN
            ! ... The species is dry deposited
         ENDIF

         IF ( ThisSpc%Is_WetDep ) THEN
            ! ... The species is soluble and wet deposits
            ! ... it is also scavenged in convective updrafts
            ! ... it probably has defined Henry's law properties
         ENDIF

         ... etc ...

         ! Free the pointer
         ThisSpc =>  NULL()

       ENDDO

    END SUBROUTINE MySub
