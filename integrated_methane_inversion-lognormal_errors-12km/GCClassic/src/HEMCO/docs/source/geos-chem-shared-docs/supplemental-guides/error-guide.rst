.. |br| raw:: html

   <br />

.. _errguide:

###################################
Understand what error messages mean
###################################

In this Guide we provide information about the different types of errors
that your GEOS-Chem simulation might encounter.

.. important::

   Know the difference between warnings and errors.

   **Warnings** are non-fatal informational messages.  Usually you do
   not have to take any action when encountering a warning.
   Nevertheless, you should always try to investigate why the warning
   was generated in the first place.

   **Errors** are fatal and will halt GEOS-Chem compilation or
   execution. Looking at the error message will give you some clues as
   to why the error occurred.

We strongly encourage that you try to debug the issue using the info
both in this Guide and in our :ref:`debug-guide` Guide.  Please see
our `Support Guidelines
<https://geos-chem.readthedocs.io/en/latest/help-and-reference/SUPPORT.html>`_
for more information.

.. _errguide-where:

====================================
Where does error output get printed?
====================================

`GEOS-Chem Classic <https://geos-chem.readthedocs.io>`_, `GCHP
<https://gchp.readthedocs.io>`_, and `HEMCO
<https://hemco.readthedocs.io>`_, like all Linux-based programs,
send output to two streams: **stdout** and **stderr**.

Most output will go to the **stdout** stream, which takes I/O from the
Fortran :code:`WRITE` and :code:`PRINT` commands. If you run
e.g. GEOS-Chem Classic by just typing the executable name at the Unix
prompt:

.. code-block:: console

   $ ./gcclassic

then the stdout stream will be printed to the terminal window. You can
also redirect the stdout stream to a log file with the redirect command:

.. code-block:: console

   $ ./gcclassic > GC.log 2>&1

The :command:`2>&1` tells the bash script to append the stderr stream
(noted by :literal:`2`) to the stdout stream (noted by :literal:`1`).
This will make sure that any error output also shows up in the log file.

You can also use the Linux :command:`tee` command, which will send
output both to a log file as well as to the terminal window:

.. code-block:: console

   $ ./gcclassic | tee GC.log 2>&1

.. note::

   Please note the following:

   #. We have combined HEMCO and GEOS-Chem informational printouts as
      of GEOS-Chem 14.2.0 and HEMCO 3.7.0.  In previous versions,
      HEMCO informational printouts would have been sent to a separate
      :file:`HEMCO.log` file. |br|
      |br|

   #. We have disabled most GEOS-Chem and HEMCO informational
      printouts by default, starting in GEOS-Chem 14.2.0 and HEMCO
      3.7.0.  These printouts may be restored (e.g. for debugging) by
      enabling verbose output in both :file:`geoschem_config.yml` and
      :file:`HEMCO_Config.rc`. |br|
      |br|

   #. GCHP sends output to several log files as well as to the stdout
      and stderr streams.  Please see `gchp.readthedocs.io
      <https://gchp.readthedocs.io>`_ for more information.

.. _errguide-compile:

===================
Compile-time errors
===================

In this section we discuss some compilation warnings that you may
encounter when building GEOS-Chem.

.. _errguide-compile-ncinc:

Cannot open include file netcdf.inc
-----------------------------------

.. code-block:: console

   error #5102: Cannot open include file 'netcdf.inc'

**Problem:** The :program:`netcdf-fortran` library cannot be found.

**Solution:** Make sure that :ref:`all software dependencies have been
installed <spackguide>` and :ref:`loaded into your Linux environment
<libguide>`.

.. _errguide-compile-flex:

KPP error: Cannot find -lfl
---------------------------

.. code-block:: console

   /usr/bin/ld: cannot find -lfl
   error: ld returned exit 1 status

**Problem:**: The `Kinetic PreProcessor (KPP)
<https://kpp.readthedocs.io>`_ cannot find the :program:`flex`
library, which is one of its dependencies.

**Solution:** Make sure that :ref:`all software dependencies have been
installed <spackguide>` and :ref:`loaded into your
Linux environment <libguide>`.

.. _errguide-compile-interr:

GNU Fortran internal compiler error
-----------------------------------

.. code-block:: console

   f951: internal compiler error: in ___ at ___

**Problem:** Compilation halted due to a compiler issue.  These types
of errors can indicate:

#. An undiagnosed bug in the compiler itself.
#. The inability of the compiler to parse source code adhering to the most
   recent Fortran language standard.

**Solution:** Try switching to a newer compiler:

- For GCHP: Use GNU Compiler Collection 9.3 and later.
- For GEOS-Chem Classic and HEMCO: Use GNU Compiler Collection 7.0 and later

.. _errguide-runtime:

===============
Run-time errors
===============

.. _errguide-runtime-floating:

Floating invalid or floating-point exception error
--------------------------------------------------

.. code-block:: console

   forrtl: error (65): floating invalid    # Error message from Intel Fortran Compiler

   Floating point exception (core dumped)  # Error message from GNU Fortran compiler

**Problem:** An illegal floating-point math operation has occurred.
This error can be generated if one of the following conditions has
been encountered:

#. Division by zero
#. Underflow or overflow
#. Square root of a negative number
#. Logarithm of a negative number
#. Negative or Positive Infinity
#. Undefined value(s) used in an equation

**Solution:** Re-configure GEOS-Chem (or the HEMCO standalone) with
the :code:`-DCMAKE_RELEASE_TYPE=Debug` Cmake option.  This will build
in additional error checking that should alert you to where the error
is occurring.  Once you find the location of the error, you can take
the appropriate steps, such as making sure that the denominator of an
expression never goes to zero, etc.

.. _errguide-runtime-rosenbrock:

Forced exit from Rosenbrock
---------------------------

.. code-block:: none

   Forced exit from Rosenbrock due to the following error:
   --> Step size too small: T + 10*H = T or H < Roundoff
   T=   3044.21151383269      and H=  1.281206877135470E-012
   ### INTEGRATE RETURNED ERROR AT:          40          68           1

   Forced exit from Rosenbrock due to the following error:
   --> Step size too small: T + 10*H = T or H < Roundoff
   T=   3044.21151383269      and H=  1.281206877135470E-012
   ### INTEGRATE FAILED TWICE ###

   ###############################################################################
   ### KPP DEBUG OUTPUT
   ### Species concentrations at problem box           40          68          1
   ###############################################################################
   ... printout of species concentrations ...

   ###############################################################################
   ### KPP DEBUG OUTPUT
   ### Species concentrations at problem box           40          68          1
   ###############################################################################
   ... printout of reaction rates ...

**Problem:** The KPP Rosenbrock integrator could not converge to a
solution at a particular grid box.  This can happen when:

#. The absolute (:literal:`ATOL`) and/or relative (:literal:`RTOL`)
   :ref:`error tolerances <errguide-runtime-errtol>` need to be
   refined.
#. A particular species has numerically underflowed or overflowed.
#. A division by zero occurred in the reaction rate computations.
#. A species has been set to a very low value in another operation
   (e.g. wet scavenging), thus causing the non-convergence.
#. The initial conditions of the simulation may be non-physical.
#. A data file (meteorology or emissions) may be corrupted.

If the non-convergence only happens once, then GEOS-Chem will revert
to prior concentrations and reset the saved KPP internal timestep
(:code:`Hnew`) to zero before calling the Rosenbrock integrator again.
In many instances, this is sufficient for the chemistry to converge to
a soluiton.

In the case that the Rosenbrock integrator fails to converge to a
solution twice in a row, all of the concentrations and
reaction rates at the grid box will be printed to :ref:`stdout
<errguide-where>` and the simulation will terminate.

**Solution:** Look at the error printout.  You will likely notice
species concentrations or reaction rates that are extremely high or
low compared to the others. This will give you a clue as to where in
GEOS-Chem the error may have occurred.

Try performing some short test simulations, turning each operation
(e.g. transport, PBL mixing, convection, etc). off one at a time.
This should isolate the location of the error.  Make sure to turn on
verbose output in both :file:`geoschem_config.yml` and
:file:`HEMCO_Config.rc`; this will send additional printout to the
:ref:`stdout <errguide-where>` stream.  The clue to finding the error
may become obvious by looking at this output.

Check your restart file to make sure that the initial concentrations
make sense.  For certain simulations, using initial conditions from a
simulation that has been sufficiently spun-up makes a difference.

Use a netCDF file viewer like :program:`ncview` to open the
meteorology files on the day that the error occurred.  If a file
does not open properly, it is probably corrupted.  If you suspect that
the file may have been corrupted during download, then download the
file again from its original source.  If this still does not fix the
error, then the file may have been corrupted at its source.  Please
open a new Github issue to alert the GEOS-Chem Support Team.

.. _errguide-runtime-errtol:

More about KPP error tolerances
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

The error tolerances are set in the following locations:

#. **fullchem** mechanism: In routine :code:`Do_FlexChem` (located in
   in :file:`GeosCore/fullchem_mod.F90`).
#. **Hg** mechanism: In routine :code:`ChemMercury` (located in
   :file:`GeosCore/mercury_mod.F90`).

For example, in the fullchem mechanism, :code:`ATOL` and :code:`RTOL` are
defined as:

.. code-block:: Fortran

   !%%%%% CONVERGENCE CRITERIA %%%%%

   ! Absolute tolerance
   ATOL      = 1e-2_dp

   ! Relative tolerance
   ! Changed to 0.5e-3 to avoid integrate errors by halogen chemistry
   !  -- Becky Alexander & Bob Yantosca (24 Jan 2023)
   RTOL      = 0.5e-3_dp

Convergence errors can occur because the system arrives to a state too
far from the truth to be able to converge. By tightening
(i.e. decreasing) the tolerances, you ensure that the system stays
closer to the truth at every time step. Then, the problematic time
steps will start the chemistry with a system closer to the true state,
enabling the chemistry to converge.

CAVEAT: If the first time step of chemistry cannot converge,
tightening the tolerances wouldn't work but loosening the tolerance
would. So you might have to experiment a little bit in order to find
the proper settings for :literal:`ATOL` and :literal:`RTOL` for your
specific mechanism.

.. _errguide-runtime-nofield:

HEMCO Error: Cannot find field
------------------------------

.. code-block:: console

   HEMCO Error: Cannot find field ___.  Please check the name in the config file.

**Problem:** A GEOS-Chem Classic or HEMCO standalone simulation halts
because HEMCO cannot find a certain input field.

**Solution:** Most of the time, this error indicates that a species is
missing from the `GEOS-Chem restart file
<https://geos-chem.readthedocs.io/en/latest/gcclassic-user-guide/restart-files-gc.html/restart-files-gc.html>`_.
By default, the GEOS-Chem restart file (entry :literal:`SPC_` in
`HEMCO_Config.rc
<https://geos-chem.readthedocs.io/en/latest/gcclassic-user-guide/hemco-config.html>`_) uses time cycle flag :literal:`EFYO`.  This
setting tells HEMCO to halt if a species does not have an initial
condition field contained in the GEOS-Chem restart file. Changing this
time cycle flag to :literal:`CYS` will allow the simulation to
proceed.  In this case, species will be given a default background
initial concentration, and the simulation will be allowed to proceed.

.. _errguide-runtime-nofile:

HEMCO Error: Cannot find file for current simulation time
---------------------------------------------------------

.. code-block:: console

   HEMCO ERROR: Cannot find file for current simulation time:
   ./Restarts/GEOSChem.Restart.17120701_0000z.nc4 - Cannot get field SPC_NO.
   Please check file name and time (incl. time range flag) in the config. file

**Problem:** HEMCO tried to read data from a file but could not find the time
slice requested in :file:`HEMCO_Config.rc`.

**Solution:** Make sure that the file is at the path specified in
:file:`HEMCO_Config.rc`.  HEMCO will try to look back in time starting
with the current year and going all the way back to the year 1712
or 1713. So if you see 1712 or 1713 in the error message, that is a
tip-off that the file is missing.

.. _errguide-runtime-runerr:

HEMCO Run Error
---------------

.. code-block:: console

   ===============================================================================
   GEOS-CHEM ERROR: HCO_RUN

   HEMCO ERROR: Please check the HEMCO log file for error messages!

   STOP at HCOI_GC_RUN (hcoi_gc_main_mod.F90)
   ===============================================================================

**Problem:** A GEOS-Chem simulation stopped in the :code:`HCOI_GC_RUN`
routine with an error message similar to that shown above.

**Solution:** Look at the output that was written to the
:ref:`stdout and stderr <errguide-where>` streams.  Error messages
containing :literal:`HCO` originate in HEMCO.

.. _errguide-runtime-wrongtime:

HEMCO time stamps may be wrong
------------------------------

.. code-block:: console

   HEMCO WARNING: ncdf reference year is prior to 1901 - time stamps may be wrong!
   --> LOCATION: GET_TIMEIDX (hco_read_std_mod.F90)

**Problem:** HEMCO reads the files but gives zero emissions and shows
the error listed above.

**Solution:** Do the following:

#. Reset the reference datetime in the netCDF file so that it is
   after 1901. |br|
   |br|

#. Make sure that the :literal:`time:calendar` string is either
   :literal:`standard` or :literal:`gregorian`.  GEOS-Chem Classic,
   GCHP, and HEMCO can only read data placed on calendars with leap
   years.

GCST member `Lizzie Lundgren <https://github.com/lizziel>`_ writes:

   This HEMCO error occurs if the reference time for the netCDF file
   time dimension is prior to 1901. If you do :command:`ncdump –c
   filename` you will be able to see the metadata for the time
   dimension as well as the time variable values. The time units
   should include the reference date.

   You can get around this issue by changing the reference time within
   the file. You can do this with :program:`cdo` (Climate Data
   Operators) using the :program:`setreftime` command.

   Here is a bash script example by GCST member `Melissa Sulprizio
   <https://github.com/msulprizio>`_ that updates the calendar and
   reference time for all files ending in :file:`*.nc` within a
   directory.  This script was made for a user who ran into this issue.
   into the same issue. In that case the first file was for Jan 1, 1950,
   so that was made the new reference time. I would recommend doing the
   same for your dataset so that the first time variable value would be
   :literal:`0`. This script also compresses the file which we
   recommend doing.

   .. code-block:: bash

       #!/bin/bash

       for file in *nc; do
           echo "Processing $file"

	   # Make sure te calendar is "standard" and not e.g. 360 days
           cdo setcalendar,standard $file tmp.nc
           mv tmp.nc $file

	   # Set file reference time to 1950-01-01 at 0z
           cdo setreftime,1950-01-01,0 $file tmp.nc
           mv tmp.nc $file

	   # Compress the file
           nccopy -d1 -c "time/1" $file tmp.nc
           mv tmp.nc $file
       done

   After you update the file you can then again do :command:`ncdump –c
   filename` to check the time dimension. For the case above it looks
   like this after processing.

   .. code-block:: console

          double time(time) ;
                 time:standard_name = "time" ;
                 time:long_name = "time" ;
                 time:bounds = "time_bnds" ;
                 time:units = "days since 1950-01-01 00:00:00" ;
                 time:calendar = "standard" ;
                 . . .

      time = 0, 31, 59, 90, 120, 151, 181, 212, 243, 273, 304, 334, 365, 396, 424,
           455, 485, 516, 546, 577, 608, 638, 669, 699, 730, 761, 790, 821, 851,``
           882, 912, 943, 974, 1004, 1035, 1065, 1096, 1127, 1155,  1186, 1216, 1247 . . .


.. _errguide-runtime-wetdep:

Negative tracer found in WETDEP
-------------------------------

.. code-block:: console

   WETDEP: ERROR at   40  67   1 for species    2 in area WASHOUT: at surface
    LS          :  T
    PDOWN       :    0.0000000000000000
    QQ          :    0.0000000000000000
    ALPHA       :    0.0000000000000000
    ALPHA2      :    0.0000000000000000
    RAINFRAC    :    0.0000000000000000
    WASHFRAC    :    0.0000000000000000
    MASS_WASH   :    0.0000000000000000
    MASS_NOWASH :    0.0000000000000000
    WETLOSS     :                        NaN
    GAINED      :    0.0000000000000000
    LOST        :    0.0000000000000000
    DSpc(NW,:)  :                        NaN   6.0358243778561746E-013   6.5871997362336500E-013   7.2710915872550685E-013   8.0185772698102585E-013   8.7883682997147595E-013   9.6396466805517407E-013   1.0574719517340253E-012   1.1617302070198606E-012   1.2976219851862141E-012   1.4347568254382824E-012   1.5772212240871896E-012   1.7071657565802178E-012   1.8443377617027378E-012   1.9982208320328261E-012   2.1567932874822908E-012   2.2591568422224307E-012   2.2208301198704935E-012   1.8475974519883714E-012   1.7716069173018996E-013   1.7714395985520433E-013   1.7633649101242403E-013   1.6668529114369137E-013   1.3548045738669223E-013   5.1061710020314286E-014   0.0000000000000000        0.0000000000000000        0.0000000000000000        0.0000000000000000        0.0000000000000000        0.0000000000000000        0.0000000000000000        0.0000000000000000        0.0000000000000000        0.0000000000000000        0.0000000000000000        0.0000000000000000        0.0000000000000000        0.0000000000000000        0.0000000000000000        0.0000000000000000        0.0000000000000000        0.0000000000000000        0.0000000000000000        0.0000000000000000        0.0000000000000000        0.0000000000000000
    Spc(I,J,:N) :                        NaN   3.5108056785061143E-009   3.8363969256742307E-009   3.6615166033026556E-009   3.6780394914242783E-009   4.1462343168230006E-009   4.7319942271993657E-009   5.1961472823088513E-009   5.4030830279477525E-009   5.5736845790195336E-009   5.7139596145766606E-009   5.8629212873139874E-009   7.9742789235773213E-009   1.0334311421916619E-008   1.0816150360971255E-008   1.1168715310744298E-008   1.1534959217017146E-008   1.1809950282570185E-008   1.7969626885629474E-008   1.7430760762446019E-008   1.7477810715818748E-008   1.7967321756900857E-008   1.8683742574601477E-008   1.9309929368816065E-008   2.0262386892450682E-008   2.0489969814921647E-008   1.9961590106306151E-008   2.2859284477873924E-008   1.3161046290246557E-008   6.5857053651000387E-009   2.7535806161296159E-009   1.2708780077337107E-009   3.6557775667039418E-010   6.1984105316417057E-011   2.6665694620973736E-011   8.7599157145440813E-012   4.8009375158768866E-012   1.0086435318729046E-012   1.3493529625353547E-013   1.6403790023674963E-014   2.7417226109948757E-015   4.2031825835582592E-014   2.3778709382809943E-013   8.3223532851684382E-013   4.5695049346098890E-012   6.9911523125704209E-012   2.5076669266356582E-012
   ===============================================================================
   ===============================================================================
   GEOS-Chem ERROR: Error encountered in wet deposition!
    -> at SAFETY (in module GeosCore/wetscav_mod.F90)
   ===============================================================================

   ===============================================================================
   GEOS-Chem ERROR: Error encountered in "Safety"!
    -> at Do_Washout_at_Sfc (in module GeosCore/wetscav_mod.F90)
   ===============================================================================

   ===============================================================================
   GEOS-Chem ERROR:
    -> at WetDep (in module GeosCore/wetscav_mod.F90)
   ===============================================================================

   ===============================================================================
   GEOS-Chem ERROR: Error encountered in "Wetdep"!
    -> at Do_WetDep (in module GeosCore/wetscav_mod.F90)
   ===============================================================================

   ===============================================================================
   GEOS-CHEM ERROR: Error encountered in "Do_WetDep"!
   STOP at  -> at GEOS-Chem (in GeosCore/main.F90)
   ===============================================================================
        - CLEANUP: deallocating arrays now...

**Problem:** A GEOS-Chem simulation has encountered either negative
or :literal:`NaN` (not-a-number) concentrations in the wet deposition
module. This can indicate the following:

#. The wet deposition routines have removed too much soluble species
   from within a grid box.
#. Another operation (e.g. transport, convection, etc.) has removed too much
   soluble species from within a grid box.
#. A corrupted or incorrect meteorological input has caused too much
   rainout or washout to occur within a grid box (which leads to
   conditions 1 and/or 2 above).
#. An :ref:`array-out-of-bounds error <errguide-segfault-oob>` has
   corrupted a variable that is used in wet depoosition. |br|
#. For nested-grid simulations, the transport timestep may be too
   large, thus resulting in grid boxes with zero or negative
   concentrations.

**Solution:** Re-configure GEOS-Chem and/or HEMCO with the
:code:`-DCMAKE_RELEASE_TYPE=Debug` CMake option.  This adds in
additional error checks that may help you find where the error
occurs.

Also try adding some :code:`PRINT*` statements before and after the
call to :code:`DO_WETDEP` to check the concentrations entering and
leaving the wetdep module.  That might give you an idea of where the
concetnrations are going negative.

.. _errguide-runtime-perm:

Permission denied error
-----------------------

.. code-block:: console

   geoschem.run: Permission denied

**Problem:** The script :file:`geoschem.run` is not executable.

**Solution:** Change the permission of the script with:

.. code-block:: console

   $ chmod 755 geoschem.run

.. _errguide-fallvel:

Excessive fall velocity error
-----------------------------

.. code-block:: console

   GEOS-CHEM ERROR:  Excessive fall velocity?
   STOP at  CALC_FALLVEL, UCX_mod

**Problem**: The fall velocity (in stratopsheric chemistry routine
:file:`Calc_FallVel` in module :file:`GeosCore/ucx_mod.F90`) exceeds
10 m/s.  This error will most often occur in GEOS-Chem Classic
nested-grid simulations.

**Solution**: Reduce the default timestep settings in
:file:`geoschem_config.yml`.  You may need to use 300 seconds
(transport) and 600 seconds (chemistry) or even smaller depending on
the horizontal resolution of your simulation.

.. _errguide-fileio:

===============
File I/O errors
===============

.. _errguide-fileio-list:

List-directed I/O syntax error
------------------------------

.. code-block:: console

   # Error message from GNU Fortran
   At line NNNN of file filename.F90
   Fortran runtime error: Bad real number|integer number|character in item X of list input

   # Error message from Intel Fortran
   forrtl: severe (59): list-directed I/O syntax error, unit -5, file Internal List-Directed Read

**Problem:** This error indicates that the wrong type of data was read
from a text file.  This can happen when:

#.  Numeric input is expected but character input was read from disk (or
    vice-versa);
#.  A :command:`READ` statement in your code has been omitted or deleted.

**Solution:** Check configuration files (:file:`geoschem_config.yml`,
:file:`HEMCO_Config.rc`, :file:`HEMCO_Diagn.rc`, etc.) for syntax
errors and omissions that could be causing this error.

.. _errguide-fileio-ncdefvar:

Nf_Def_Var: can not define variable
-----------------------------------

.. code-block:: console

   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

   Nf_Def_var: can not define variable: ____

   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

   Code stopped from DO_ERR_OUT (in module NcdfUtil/m_do_err_out.F90)

   This is an error that was encountered in one of the netCDF I/O modules,
   which indicates an error in writing to or reading from a netCDF file!

   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

**Problem:** GEOS-Chem or HEMCO could not write a variable to a netCDF
file.  This error may be caused by:

#. The netCDF file is write-protected and cannot be overwritten.
#. The path to the netCDF file is incorrect (e.g. directory does not exist).
#. The netCDF file already contains a variable with the same name.

**Solution:** Try the following:

#. If GEOS-Chem or HEMCO will be overwriting any existing netCDF files
   (which can often happen during testing & development), make sure
   that the file and containing directory are not write-protected. |br|
   |br|

#. Make sure that the path where you intend to write the netCDF file
   exists. |br|
   |br|

#. Check your :file:`HISTORY.rc` and :file:`HEMCO_Diagn.rc` diagnostic
   configuration files to make sure that you are not writing more than
   one diagnostic variable with the same name.

.. _errguide-fileio-hdf:

NetCDF: HDF Error
-----------------

.. code-block:: console

   NetCDF: HDF error

**Problem:** The netCDF library routines in GEOS-Chem or HEMCO cannot
read a netCDF file.  The error is occurring in the HDF5 library (upon
which netCDF depends).  This may indicate a corrupted or incomplete
netCDF file.

**Solution:** Try re-downloading the file from the `WashU data portal
<https://geoschemdata.wustl.edu>`_.  Downloading a fresh copy of the
file is often sufficient to fix this type of issue.  If the error
persists, please open a new GitHub issue to alert the GEOS-Chem
Support team, as the corruption may have occured at the original
source of te data.

.. _errguide-segfault:

=======================================
Segmentation faults and similar errors
=======================================

.. code-block:: console

   SIGSEGV, segmentation fault occurred

**Problem:** GEOS-Chem or HEMCO tried to access an `invalid memory
location
<http://stackoverflow.com/questions/2346806/what-is-segmentation-fault>`__.

**Solution:** See the sections below for ways to debug segmentation
fault errors.

.. _errguide-segfault-oob:

Array-out-of-bounds error
-------------------------

.. code-block:: console

   Subscript #N of the array THISARRAY has value X which is less than the lower bound of Y

   or

   Subscript #N of the array THISARRAY has value A which is greater than the upper bound of B

**Problem:** An array index variable refers to an element that lies
outside of the array boundaries.

**Solution:** Reconfigure GEOS-Chem with the following options:

.. code-block:: console

   $ cd /path/to/build                  # Your GEOS-Chem or HEMCO build directory
   $ cmake . -DCMAKE_BUILD_TYPE=Debug

This will enable several debugging options, including checking for
array operations indices that going out of bounds.  You wil get an
error message similar to those shown above.

Use the :command:`grep` command to search for all instances of the
array (in this example, :code:`THISARRAY`) in each source code folder:

.. code-block:: console

   grep -i THISARRAY *.F90    # -i means ignore uppercase/lowercase distinction

This should let you quickly locate the issue.  Depending on the
compiler that is used, you might also get a routine name and line
number from the error output.

.. _errguide-segfault-tpcore:

Segmentation fault encountered after TPCORE initialization
----------------------------------------------------------

.. code-block:: console

   NASA-GSFC Tracer Transport Module successfully initialized

**Problem:** A GEOS-Chem simulation dies right after you see this
text.

.. note::

   Starting in GEOS-Chem Classic 14.1.0, the text above will only be
   printed if you have activated verbose output in the
   :file:`geoschem_config.yml` configuration file.

**Solution:** Increase the amount of stack memory available to
GEOS-Chem and HEMCO. `Please follow this link
<https://geos-chem.readthedocs.io/en/latest/gcclassic-user-guide/login-env-parallel.html>`__
for detailed instructions.

.. _errguide-fileio-invalid:

Invalid memory access
---------------------

.. code-block:: console

   severe (174): SIGSEGV, segmentation fault occurred
   This message indicates that the program attempted an invalid memory reference.
   Check the program for possible errors.

**Problem:** GEOS-Chem or HEMCO code tried to read data from an
invalid memory location.  This can happen when data is being read from
a file into an array, but the array is too small to hold all the data.

**Solution:** Use a debugger (like :program:`gdb`) to try to diagnose
the situation. Also try increasing the dimensions of the array that
you suspect might be too small.

.. _errguide-segfault-stack:

Stack overflow
--------------

.. code-block:: console

   severe (174): SIGSEGV, possible program stack overflow occurred
   Program requirements exceed current stacksize resource limit.

**Problem:** GEOS-Chem and/or HEMCO is using more **stack memory** than is
currently available to the system.  Stack memory is a reserved portion
of the memory structure where short-lived variables are stored, such as:

#. Variables that are local to a given subroutine
#. Variables that are NOT globally saved
#. Variables that are NOT declared as an :code:`ALLOCATABLE` array
#. Variables that are NOT declared as a :code:`POINTER` variable or array
#. Variables that are included in an :code:`!$OMP PRIVATE` or
   :code:`!$OMP THREADPRIVATE`

**Solution:** Max out the amount of stack memory that is available to
GEOS-Chem and HEMCO.  `See this section
<http://geos-chem.readthedocs.io/en/latest/getting-started/login-env-parallel.html>`_
for instructions.

.. _errguide-lesscommon:

===================
Less commmon errors
===================

The errors listed below, which occur infrequently, are related to
invalid memory operations. These can especially occur with
:code:`POINTER`-based variables.

.. _errguide-lesscommon-bus:

Bus Error
---------

**Problem:** GEOS-Chem or HEMCO is trying to reference memory that
cannot possibly be there. The website StackOverflow.com has a `definition of
bus error and how it differs from a segmentation
fault <http://stackoverflow.com/questions/212466/what-is-a-bus-errornice>`__.

**Solution:** A bus error may occur when you call a subroutine
with too many arguments.  Check subroutine definitions and
subroutine calls to make sure the correct number of arguments are
passed.

.. _errguide-lesscommon-double:

Double free or corruption
-------------------------

.. code-block:: console

   *** glibc detected *** PROGRAM_NAME: double free or corruption (out): ____ ***

**Problem:** The following error is not common, but can occur under some
circumstances.  Usually this means one of the following has occurred:

#. You are deallocating the same variable more than once.
#. You are deallocating a variable that wasn't allocated, or that has
   already been deallocated.

`Please see this link
<http://stackoverflow.com/questions/2902064/how-to-track-down-a-double-free-or-corruption-error-in-c-with-gdb>`_
for more details.

**Solution:** Try setting all deleted pointers to :code:`NULL()`.

You can also use a debugger like :program:`gdb`, which will show you a
backtrace from your crash. This will contain information about in
which routine and line number the code crashed, and what other
routines were called before the crash happened.

Remember these three basic rules when working with
:code:`POINTER`-based variables:

#. Set pointer to NULL after free.
#. Check for NULL before freeing.
#. Initialize pointer to NULL in the start.

Using these rules helps to prevent this type of error.

Also note, you may see this error when a software library required by
GEOS-Chem and/or HEMCO is not (e.g. :program:`netcdf` or
:program:`netcdf-fortran` has not been installed.  GEOS-Chem and/or
HEMCO may be making calls to the missing library, which results in the
error.  If this is the case, the solution would be to :ref:`install
all required libraries <spackguide>`.

.. _errguide-lesscommon-dwarf:

Dwarf subprogram entry error
----------------------------

.. code-block:: console

    Dwarf subprogram entry L_ROUTINE-NAME__LINE-NUMBER__par_loop2_2_576 has high_pc < low_pc.
    This warning will not be repeated for other occurrences.

**Problem:** GEOS-Chem or HEMCO code tried to use a
:code:`POINTER`-based variable that is **unassociated** (i.e. not
pointing to any other variable or memory) from within an OpenMP
parallel loop.

This error can happen when a :code:`POINTER`-based variable is set to
:code:`NULL()` where it is declared:

.. code-block:: fortran

   TYPE(Species), POINTER :: ThisSpc => NULL()

The above declaration causes use pointer variable :code:`ThisSpc` to
be implicitly declared with the :code:`SAVE` attribute. This causes a
segmentation fault, because all pointers used within an OpenMP
parallel region must be associated and nullified on the same thread.

**Solution:** Make sure that any :code:`POINTER`-based variables (such
as :code:`ThisSpc` in this example) point to their target and are
nullified within the same OpenMP parallel loop.

.. code-block:: fortran

   TYPE(Species), POINTER :: ThisSpc   ! Do not set to NULL() here!!!

    ... etc ...

   !$OMP PARALLEL DO(
   !$OMP DEFAULT( SHARED ) &
   !$OMP PRIVATE( I, J, L, N, ThisSpc, ... )
   DO N = 1, nSpecies
   DO L = 1, NZ
   DO J = 1, NY
   DO I = 1, NX

      ... etc ...

      ! Point to species database entry
      ThisSpc => State_Chm%Species(N)%Info

      ... etc ...

      ! Free pointer at end of loop
      ThisSpc => NULL()

   ENDDO
   ENDDO
   ENDDO
   ENDDO

Note that you must also add  :code:`POINTER`-based variables (such as
:code:`ThisSpc`) to the :code:`!$OMP PRIVATE` clause for the parallel
loop.

For more information about this type of error, `please see this
article <http://www.cs.rpi.edu/~szymansk/OOF90/bugs.html#4>`__.

.. _errguide-lesscommon-free:

Free: invalid size
------------------

.. code-block:: console

   Error in PROGRAM_NAME free(): invalid size: 0x00000000 0662e090

**Problem:**  This error is not common.  It can happen when:

#. You are trying to free a pointer that wasn't allocated.
#. You are trying to delete an object that wasn't created.
#. You may be trying to nullify or deallocate an object more than once.
#. You may be overflowing a buffer.
#. You may be writing to memory that you shouldn't be writing to.

**Solution:** Any number of programming errors can cause this
problem. You need to use a debugger (such as :program:`gdb`), get a
backtrace, and see what your program is doing when the error
occurs. If that fails and you determine you have corrupted the memory
at some previous point in time, you may be in for some painful
debugging (it may not be too painful if the project is small enough
that you can tackle it piece by piece).

`See this link <http://stackoverflow.com/question/error-free-invalid-next-size-fast>`_ for more information.

.. _errguide-lesscommon-munmap:

Munmap_chunk: invalid pointer
-----------------------------

.. code-block:: console

   ** glibc detected *** PROGRAM_NAME: munmap_chunk(): invalid pointer: 0x00000000059aac30 ***

**Problem:** This is not a common error, but can happen if you
deallocate or nullify a :code:`POINTER`-based variable that has
already been deallocated or modified.

**Solution:** Use a debugger (like :program:`gdb`) to see where in
GEOS-Chem or HEMCO the error occurs.  You will likely have to remove a
duplicate :code:`DEALLOCATE` or :code:`=> NULL()` statement.  `See
this link
<http://stackoverflow.com/questions/6199729/how-to-solve-munmap-ch unk-invalid-pointer-error-in-c>`_
for more information.

.. _errguide-lesscommon-outmem:

Out of memory asking for NNNNN
------------------------------

.. code-block:: console

    Fatal compilation error: Out of memory asking for 36864.

**Problem:** This error may be caused by the :literal:`datasize` limit
not being maxed out in your Linux login environment.  `See this link
<http://software.intel.com/en-us/forums/topic/268149>`_ for more
information.

**Solution:** Use this command to check the status of the
:literal:`datasize` limit:

.. code-block:: console

   $ ulimit -d
   unlimited

If the result of this command is not :literal:`unlimited`, then set it
to unlimited with this command:

.. code-block:: console

   $ ulimit -d unlimited

.. note::

   The two most important limits for GEOS-Chem and HEMCO
   are :literal:`datasize` and :literal:`stacksize`  These should both
   be set to :literal:`unlimited`.
