.. _ncguide:

######################
Work with netCDF files
######################

On this page we provide some useful information about working with data
files in netCDF format.

.. _ncguide-useful-tools:

============
Useful tools
============


There are many free and open-source software packages readily available
for visualizing and manipulating netCDF files.

.. option:: cdo

   :program:`Climate Data Operators`: Highly-optimized command-line tools
   for manipulating and analyzing netCDF files.  Contains features
   that are especially useful for Earth Science applications.

   See: https://code.zmaw.de/projects/cdo

.. option:: GCPy

   :program:`GEOS-Chem Python toolkit`: Python package for visualizing
   and analyzing GEOS-Chem output.  Used for creating the GEOS-Chem
   benchmark plots.  Also contains some useful routines for creating
   single-panel plots and multi-panel difference plots, as well as
   file regridding utilities.

   See: https://gcpy.readthedocs.io

.. option:: ncdump

   Generates a text representation of netCDF data and can be used to
   quickly view the variables contained in a netCDF file.
   :program:`ncdump` is installed to the :file:`bin/` folder of your
   netCDF library distribution.

   See: https://www.unidata.ucar.edu/software/netcdf/workshops/2011/utilities/Ncdump.html

.. option:: nco

   :program:`netCDF operators`: Highly-optimized command-line tools for
   manipulating and analyzing netCDF files.

   See: http://nco.sourceforge.net

.. option:: ncview

   Visualization package for netCDF files. :program:`Ncview` has limited
   features, but is great for a quick look at the contents of netCDF
   files.

   See: http://meteora.ucsd.edu/~pierce/ncview_home_page.html

.. option:: netcdf-scripts

   Our repository of useful netCDF utility scripts for GEOS-Chem.

   See: https://github.com/geoschem/netcdf-scripts

.. option:: Panoply

   Java-based data viewer for netCDF files.  This package offers an
   alternative to ncview. From our experience, Panoply works nicely
   when installed on the desktop, but is slow to respond in the Linux
   environment.

   See: https://www.giss.nasa.gov/tools/panoply/

.. option:: xarray

   Python package that lets you read the contents of a netCDF file
   into a data structure.  The data can then be further manipulated or
   converted to numpy or dask arrays for further procesing.

   See: https://xarray.readthedocs.io

Some of the tools listed above, such as :program:`ncdump` and
:program:`ncview` may come pre-installed on your system. Others may
need to be installed or loaded (e.g. via the :command:`module load`
command). Check with your system administrator or IT staff to see what
is available on your system.

.. _ncguide-examine-contents:

=======================================
Examine the contents of a netCDF file
=======================================

An easy way to examine the contents of a netCDF file is to use
:program:`ncdump` as follows:

.. code-block:: console

   $ ncdump -ct GEOSChem.SpeciesConc.20190701_0000z.nc4

You will see output similar to this:

.. code-block:: console

   netcdf GEOSChem.SpeciesConc.20190701_0000z {
   dimensions:
   	time = UNLIMITED ; // (1 currently)
   	lev = 72 ;
   	ilev = 73 ;
   	lat = 46 ;
   	lon = 72 ;
   	nb = 2 ;
   variables:
   	double time(time) ;
   		time:long_name = "Time" ;
   		time:units = "minutes since 2019-07-01 00:00:00" ;
   		time:calendar = "gregorian" ;
   		time:axis = "T" ;
   	double lev(lev) ;
   		lev:long_name = "hybrid level at midpoints ((A/P0)+B)" ;
   		lev:units = "level" ;
   		lev:axis = "Z" ;
   		lev:positive = "up" ;
   		lev:standard_name = "atmosphere_hybrid_sigma_pressure_coordinate" ;
   		lev:formula_terms = "a: hyam b: hybm p0: P0 ps: PS" ;
   	double ilev(ilev) ;
   		ilev:long_name = "hybrid level at interfaces ((A/P0)+B)" ;
   		ilev:units = "level" ;
   		ilev:positive = "up" ;
   		ilev:standard_name = "atmosphere_hybrid_sigma_pressure_coordinate" ;
   		ilev:formula_terms = "a: hyai b: hybi p0: P0 ps: PS" ;
   	double lat_bnds(lat, nb) ;
   		lat_bnds:long_name = "Latitude bounds (CF-compliant)" ;
   		lat_bnds:units = "degrees_north" ;
   	double lat(lat) ;
   		lat:long_name = "Latitude" ;
   		lat:units = "degrees_north" ;
   		lat:axis = "Y" ;
   		lat:bounds = "lat_bnds" ;
   	double lon_bnds(lon, nb) ;
   		lon_bnds:long_name = "Longitude bounds (CF-compliant)" ;
   		lon_bnds:units = "degrees_east" ;
   	double lon(lon) ;
   		lon:long_name = "Longitude" ;
   		lon:units = "degrees_east" ;
   		lon:axis = "X" ;
   		lon:bounds = "lon_bnds" ;
   	double hyam(lev) ;
   		hyam:long_name = "hybrid A coefficient at layer midpoints" ;
   		hyam:units = "hPa" ;
   	double hybm(lev) ;
   		hybm:long_name = "hybrid B coefficient at layer midpoints" ;
   		hybm:units = "1" ;
   	double hyai(ilev) ;
   		hyai:long_name = "hybrid A coefficient at layer interfaces" ;
   		hyai:units = "hPa" ;
   	double hybi(ilev) ;
   		hybi:long_name = "hybrid B coefficient at layer interfaces" ;
   		hybi:units = "1" ;
   	double P0 ;
   		P0:long_name = "reference pressure" ;
   		P0:units = "hPa" ;
   	float AREA(lat, lon) ;
   		AREA:long_name = "Surface area" ;
   		AREA:units = "m2" ;
   	float SpeciesConc_RCOOH(time, lev, lat, lon) ;
   		SpeciesConc_RCOOH:long_name = "Dry mixing ratio of species RCOOH" ;
   		SpeciesConc_RCOOH:units = "mol mol-1 dry" ;
   		SpeciesConc_RCOOH:averaging_method = "time-averaged" ;
   	float SpeciesConc_O2(time, lev, lat, lon) ;
   		SpeciesConc_O2:long_name = "Dry mixing ratio of species O2" ;
   		SpeciesConc_O2:units = "mol mol-1 dry" ;
   		SpeciesConc_O2:averaging_method = "time-averaged" ;
   	float SpeciesConc_N2(time, lev, lat, lon) ;
   		SpeciesConc_N2:long_name = "Dry mixing ratio of species N2" ;
   		SpeciesConc_N2:units = "mol mol-1 dry" ;
   		SpeciesConc_N2:averaging_method = "time-averaged" ;
   	float SpeciesConc_H2(time, lev, lat, lon) ;
   		SpeciesConc_H2:long_name = "Dry mixing ratio of species H2" ;
   		SpeciesConc_H2:units = "mol mol-1 dry" ;
   		SpeciesConc_H2:averaging_method = "time-averaged" ;
   	float SpeciesConc_O(time, lev, lat, lon) ;
   		SpeciesConc_O:long_name = "Dry mixing ratio of species O" ;
   		SpeciesConc_O:units = "mol mol-1 dry" ;

		... etc ...

   // global attributes:
   		:title = "GEOS-Chem diagnostic collection: SpeciesConc" ;
   		:history = "" ;
   		:format = "not found" ;
   		:conventions = "COARDS" ;
   		:ProdDateTime = "" ;
   		:reference = "www.geos-chem.org; wiki.geos-chem.org" ;
   		:contact = "GEOS-Chem Support Team (geos-chem-support@g.harvard.edu)" ;
   		:simulation_start_date_and_time = "2019-07-01 00:00:00z" ;
   		:simulation_end_date_and_time = "2019-07-01 01:00:00z" ;
   data:

    time = "2019-07-01 00:30" ;

    lev = 0.99250002413, 0.97749990013, 0.962499776, 0.947499955, 0.93250006,
       0.91749991, 0.90249991, 0.88749996, 0.87249996, 0.85750006, 0.842500125,
       0.82750016, 0.8100002, 0.78750002, 0.762499965, 0.737500105, 0.7125001,
       0.6875001, 0.65625015, 0.6187502, 0.58125015, 0.5437501, 0.5062501,
       0.4687501, 0.4312501, 0.3937501, 0.3562501, 0.31279158, 0.26647905,
       0.2265135325, 0.192541016587707, 0.163661504087706, 0.139115, 0.11825,
       0.10051436, 0.085439015, 0.07255786, 0.06149566, 0.05201591, 0.04390966,
       0.03699271, 0.03108891, 0.02604911, 0.021761005, 0.01812435, 0.01505025,
       0.01246015, 0.010284921, 0.008456392, 0.0069183215, 0.005631801,
       0.004561686, 0.003676501, 0.002948321, 0.0023525905, 0.00186788,
       0.00147565, 0.001159975, 0.00090728705, 0.0007059566, 0.0005462926,
       0.0004204236, 0.0003217836, 0.00024493755, 0.000185422, 0.000139599,
       0.00010452401, 7.7672515e-05, 5.679251e-05, 4.0142505e-05, 2.635e-05,
       1.5e-05 ;

    ilev = 1, 0.98500004826, 0.969999752, 0.9549998, 0.94000011, 0.92500001,
       0.90999981, 0.89500001, 0.87999991, 0.86500001, 0.85000011, 0.83500014,
       0.82000018, 0.80000022, 0.77499982, 0.75000011, 0.7250001, 0.7000001,
       0.6750001, 0.6375002, 0.6000002, 0.5625001, 0.5250001, 0.4875001,
       0.4500001, 0.4125001, 0.3750001, 0.3375001, 0.28808306, 0.24487504,
       0.208152025, 0.176930008175413, 0.150393, 0.127837, 0.108663, 0.09236572,
       0.07851231, 0.06660341, 0.05638791, 0.04764391, 0.04017541, 0.03381001,
       0.02836781, 0.02373041, 0.0197916, 0.0164571, 0.0136434, 0.0112769,
       0.009292942, 0.007619842, 0.006216801, 0.005046801, 0.004076571,
       0.003276431, 0.002620211, 0.00208497, 0.00165079, 0.00130051, 0.00101944,
       0.0007951341, 0.0006167791, 0.0004758061, 0.0003650411, 0.0002785261,
       0.000211349, 0.000159495, 0.000119703, 8.934502e-05, 6.600001e-05,
       4.758501e-05, 3.27e-05, 2e-05, 1e-05 ;

    lat = -89, -86, -82, -78, -74, -70, -66, -62, -58, -54, -50, -46, -42, -38,
       -34, -30, -26, -22, -18, -14, -10, -6, -2, 2, 6, 10, 14, 18, 22, 26, 30,
       34, 38, 42, 46, 50, 54, 58, 62, 66, 70, 74, 78, 82, 86, 89 ;

    lon = -180, -175, -170, -165, -160, -155, -150, -145, -140, -135, -130,
       -125, -120, -115, -110, -105, -100, -95, -90, -85, -80, -75, -70, -65,
       -60, -55, -50, -45, -40, -35, -30, -25, -20, -15, -10, -5, 0, 5, 10, 15,
       20, 25, 30, 35, 40, 45, 50, 55, 60, 65, 70, 75, 80, 85, 90, 95, 100, 105,
       110, 115, 120, 125, 130, 135, 140, 145, 150, 155, 160, 165, 170, 175 ;
   }


You can also use :program:`ncdump` to display the data values for a
given variable in the netCDF file. This command will display the
values in the :literal:`SpeciesRst_O3` variable to the screen:

.. code-block:: console

   $ ncdump -v SpeciesConc_O3 GEOSChem.SpeciesConc.20190701_0000z.nc4 | less

Or you can redirect the output to a file:

.. code-block:: console

   $ ncdump -v SpeciesConc_O3 GEOSChem.SpeciesConc.20190701_0000z.nc4 > log

.. _ncguide-reading-files:

==================================
Read the contents of a netCDF file
==================================

.. _ncguide-reading-w-python:

Read data with Python
---------------------

The easiest way to read a netCDF file is to use the `xarray Python
package <https://xarray.readthedocs.io>`_.

.. code-block::  python

   #!/usr/bin/env python

   # Imports
   import numpy as np
   import xarray as xr

   # Read a restart file into an xarray Dataset object
   ds = xr.open_dataset("GEOSChem.SpeciesConc.20190701_0000z.nc4")

   # Print the contents of the DataSet
   print(ds)

   # Print units of data
   print(f"\nUnits of SpeciesRst_O3: {ds['SpeciesConc_O3'].units}")

   # Print the sum, max, and min of the data
   # NOTE .values returns a numpy ndarray so that we can use
   # other numpy functions like np.sum() on the data
   print(f"Sum of SpeciesRst_O3: {np.sum(ds['SpeciesConc_O3'].values)}")
   print(f"Max of SpeciesRst_O3: {np.max(ds['SpeciesConc_O3'].values)}")
   print(f"Min of SpeciesRst_O3: {np.min(ds['SpeciesConc_O3'].values)}")

This above script will print the following output:

.. code-block:: console

   <xarray.Dataset>
   Dimensions:               (ilev: 73, lat: 46, lev: 72, lon: 72, nb: 2, time: 1)
   Coordinates:
     * time                  (time) datetime64[ns] 2019-07-01T00:30:00
     * lev                   (lev) float64 0.9925 0.9775 ... 2.635e-05 1.5e-05
     * ilev                  (ilev) float64 1.0 0.985 0.97 ... 3.27e-05 2e-05 1e-05
     * lat                   (lat) float64 -89.0 -86.0 -82.0 ... 82.0 86.0 89.0
     * lon                   (lon) float64 -180.0 -175.0 -170.0 ... 170.0 175.0
   Dimensions without coordinates: nb
   Data variables: (12/315)
       lat_bnds              (lat, nb) float64 ...
       lon_bnds              (lon, nb) float64 ...
       hyam                  (lev) float64 ...
       hybm                  (lev) float64 ...
       hyai                  (ilev) float64 ...
       hybi                  (ilev) float64 ...
       ...                    ...
       SpeciesConc_AONITA    (time, lev, lat, lon) float32 ...
       SpeciesConc_ALK4      (time, lev, lat, lon) float32 ...
       SpeciesConc_ALD2      (time, lev, lat, lon) float32 ...
       SpeciesConc_AERI      (time, lev, lat, lon) float32 ...
       SpeciesConc_ACTA      (time, lev, lat, lon) float32 ...
       SpeciesConc_ACET      (time, lev, lat, lon) float32 ...
   Attributes:
       title:                           GEOS-Chem diagnostic collection: Species...
       history:
       format:                          not found
       conventions:                     COARDS
       ProdDateTime:
       reference:                       www.geos-chem.org; wiki.geos-chem.org
       contact:                         GEOS-Chem Support Team (geos-chem-suppor...
       simulation_start_date_and_time:  2019-07-01 00:00:00z
       simulation_end_date_and_time:    2019-07-01 01:00:00z

   Units of SpeciesRst_O3: mol mol-1 dry
   Sum of SpeciesRst_O3: 0.4052325189113617
   Max of SpeciesRst_O3: 1.01212954177754e-05
   Min of SpeciesRst_O3: 3.758645839013752e-09

.. _ncguide-reading-multiple-files-w-python:

Read data from multiple files in Python
---------------------------------------

The xarray package will also let you read data from multiple files into
a single Dataset object. This is done with the open_mfdataset (open
multi-file-dataset) function as shown below:

.. code-block:: python

   #!/usr/bin/env python

   # Imports
   import xarray as xr

   # Create a list of files to open
   filelist = [
       'GEOSChem.SpeciesConc.20160101_0000z.nc4',
       'GEOSChem.SpeciesConc_20160201_0000z.nc4',
       ...
   ]

   # Read a restart file into an xarray Dataset object
   ds = xr.open_mfdataset(filelist)

.. _ncguide-coards-compliant:

================================================
Determining if a netCDF file is COARDS-compliant
================================================

All netCDF files used as input to GEOS-Chem and/or HEMCO must adhere
to the :ref:`COARDS netCDF conventions <coards-guide>`.  You can use
the `isCoards script
<https://github.com/geoschem/netcdf-scripts/blob/main/scripts/isCoards>`_
(from our `netcdf-scripts repository at GitHub
<https://github.com/geoschem/netcdf-scripts>`_) to determine if a
netCDF file adheres to the COARDS conventions.

Run the :file:`isCoards` script at the command line on any netCDF file, and
you will receive a report as to which elements of the file do not
comply with the COARDS conventions.

.. code-block:: console

   $ isCoards myfile.nc

   ===========================================================================
   Filename: myfile.nc
   ===========================================================================

   The following items adhere to the COARDS standard:
   ---------------------------------------------------------------------------
   -> Dimension "time" adheres to standard usage
   -> Dimension "lev" adheres to standard usage
   -> Dimension "lat" adheres to standard usage
   -> Dimension "lon" adheres to standard usage
   -> time(time)
   -> time is monotonically increasing
   -> time:axis = "T"
   -> time:calendar = "gregorian"
   -> time:long_name = "Time"
   -> time:units = "hours since 1985-1-1 00:00:0.0"
   -> lev(lev)
   -> lev is monotonically decreasing
   -> lev:axis = "Z"
   -> lev:positive = "up"
   -> lev:long_name = "GEOS-Chem levels"
   -> lev:units = "sigma_level"
   -> lat(lat)
   -> lat is monotonically increasing
   -> lat:axis = "Y"
   -> lat:long_name = "Latitude"
   -> lat:units = "degrees_north"
   -> lon(lon)
   -> lon is monotonically increasing
   -> lon:axis = "X"
   -> lon:long_name = "Longitude"
   -> lon:units = "degrees_east"
   -> OH(time,lev,lat,lon)
   -> OH:long_name = "Chemically produced OH"
   -> OH:units = "kg/m3"
   -> OH:long_name = 1.e+30f
   -> OH:missing_value = 1.e+30f
   -> conventions: "COARDS"
   -> history: "Mon Apr  3 08:26:19 2017"
   -> title: "COARDS/netCDF file created by BPCH2COARDS (GAMAP v2-17+)"
   -> format: "NetCDF-3"

   The following items DO NOT ADHERE to the COARDS standard:
   ---------------------------------------------------------------------------
   -> time[0] != 0 (problem for GCHP)

   The following optional items are RECOMMENDED:
   ---------------------------------------------------------------------------
   -> Consider adding the "references" global attribute

.. _ncguide-edit-vars-attrs:

=============================
Edit variables and attributes
=============================

As discussed :ref:`in the preceding section
<ncguide-coards-compliant>`, you may find that you need to edit your
netCDF files for COARDS-compliance.  Below are several useful commands
for editing netCDF files.  Many of these commands utilize the
:option:`nco` and :option:`cdo` utilities.

#. Display the header and coordinate variables of a netCDF file, with
   the time variable displayed in human-readable format.  Also show
   status of file :ref:`compression and/or chunking <ncguide-chunk-deflate>`.

   .. code-block:: console

      $ ncdump -cts file.nc

#. :ref:`Compress a netCDF file <ncguide-chunk-deflate>`.  This can
   considerably reduce the file size!

   .. code-block:: console

      # No deflation
      $ nccopy -d0 myfile.nc tmp.nc
      $ mv tmp.nc myfile.nc

      # Minimum deflation (good for most applications)
      $ nccopy -d1 myfile.nc tmp.nc
      $ mv tmp.nc myfile.nc

      # Medium deflation
      $ nccopy -d5 myfile.nc tmp.nc
      $ mv tmp.nc myfile.nc

      # Maximum deflation
      $ nccopy -d9 myfile.nc tmp.nc
      $ mv tmp.nc myfile.nc

#. Change variable name from :literal:`SpeciesConc_NO` to :literal:`NO`:

   .. code-block:: console

      $ ncrename -v SpeciesConc_NO,NO myfile.nc

#. Set all missing values to zero:

   .. code-block:: console

      $ cdo setemisstoc,0 myfile.nc tmp.nc
      $ mv tmp.nc myfile.nc

#. Add/change the long-name attribute of the vertical coordinates
   (lev) to "GEOS-Chem levels".  This will ensure that `HEMCO
   <https://hemco.readthedocs.io>`_ recognizes the vertical levels of
   the input file as GEOS-Chem model levels.

   .. code-block:: console

      $ ncatted -a long_name,lev,o,c,"GEOS-Chem levels" myfile.nc

#. Add/change the axis and positive attributes to the vertical
   coordinate (lev):

   .. code-block:: console

      $ ncatted -a axis,lev,o,c,"Z" myfile.nc
      $ ncatted -a positive,lev,o,c,"up" myfile.nc

#. Add/change the :literal:`units` attribute of the latitude (lat) coordinate to
   :literal:`degrees_north`:

   .. code-block:: console

      $ ncatted -a units,lat,o,c,"degrees_north" myfile.nc

#. Convert the :literal:`units` attribute of the CHLA variable from
   :literal:`mg/m3` to :literal:`kg/m3`

   .. code-block:: console

      $ ncap2 -v -s "CHLA=CHLA/1000000.0f" myfile.nc tmp.nc
      $ ncatted -a units,CHLA,o,c,"kg/m3" tmp.nc
      $ mv tmp.nc myfile.nc

#. Add/change the :literal:`references`, :literal:`title`, and
   :literal:`history` global attributes

   .. code-block:: console

      $ ncatted -a references,global,o,c,"www.geos-chem.org; wiki.geos-chem.org" myfile.nc
      $ ncatted -a history,global,o,c,"Tue Mar  3 12:18:38 EST 2015" myfile.nc
      $ ncatted -a title,global,o,c,"XYZ data from ABC source" myfile.nc

#. Remove the :literal:`references` global attribute:

   .. code-block:: console

      $ ncatted -a references,global,d,, myfile.nc

#. Add a :literal:`time` dimension to a file that does not have one:

   .. code-block:: console

      $ ncap2 -h -s 'defdim(“time”,1);time[time]=0.0;time@long_name=“time”;time@calendar=“standard”;time@units=“days since 2007-01-01 00:00:00”' -O myfile.nc tmp.nc
      $ mv tmp.nc myfile.nc

#. Add a :literal:`time` dimension to a variable:

   .. code-block:: console

      # Assume myVar has lat and lon dimensions to start with
      $ ncap2 -h -s 'myVar[$time,$lat,$lon]=myVar;' myfile.nc tmp.nc
      $ mv tmp.nc myfile.nc

#. Make the :literal:`time` dimension unlimited:

   .. code-block:: console

      $ ncks --mk_rec_dmn time myfile.nc tmp.nc
      $ mv tmp.nc myfile.nc

#. Change the file reference date and time (i.e. :literal:`time:units`)
   from 1 Jan 1985 to 1 Jan 2000:

   .. code-block:: console

      $ cdo setreftime,2000-01-01,00:00:00 myfile.nc tmp.nc
      $ mv tmp.nc myfile.nc

#. Shift all time values ahead or back by 1 hour in a file:

   .. code-block:: console

      # Shift ahead 1 hour
      $ cdo shifttime,1hour myfile.nc tmp.nc
      $ mv tmp.nc myfile.nc

      # Shift back 1 hour
      $ cdo shiftime,-1hour myfile.nc tmp.nc
      $ mv tmp.nc myfile.nc

#. Set the date of all variables in the file.  (Useful for files that
   have only one time point.)

   .. code-block:: console

      $ cdo setdate,2019-07-02 myfile.nc tmp.nc
      $ mv tmp.nc myfile.nc

   .. tip::

      The following :program:`cdo` commands are similar to
      :command:`cdo setdate`, but allow you to manipulate other time
      variables:

      .. code-block:: console

         $ cdo settime,03:00:00 ...  # Sets time to 03:00 UTC
	 $ cdo setday,26, ...        # Sets day of month to 26
	 $ cdo setmon,10, ...        # Sets month to 10 (October)
	 $ cdo setyear,1992, ...     # Sets year to 1992

      See the `cdo user manual
      <https://code.mpimet.mpg.de/projects/cdo/embedded/index.html#x1-2690002.6.4>`_
      for more information.

#. Change the :literal:`time:calendar` attribute:

   GEOS-Chem and HEMCO cannot read data from netCDF files where:

   .. code-block:: none

      time:calendar = "360_day"
      time:calendar = "365_day"
      time:calendar = "noleap"

   We recommend converting the calendar used in the netCDF file to the
   :literal:`standard` netCDF calendar with these commands:

   .. code-block:: console

      $ cdo setcalendar,standard myfile.nc tmp.nc
      $ mv tmp.nc myfile.nc

.. _ncguide-concat-files:

========================
Concatenate netCDF files
========================

There are a couple of ways to concatenate multiple netCDF files into a
single netCDF file, as shown in the sections below.

.. _ncguide-concat-nco:

Concatenate with the netCDF operators
-------------------------------------

You can use the :program:`ncrcat` utility (from :option:`nco`)
to concatenate the individual netCDF files into a single netCDF file.

Let's assume we want to combine 12 monthy data files
(e.g. :file:`month_01.nc`, :file:`month_02.nc`, .. :file:`month_12.nc`
into a single file called :file:`annual_data.nc`.

First, make sure that each of the :file:`month_*nc` files has an
unlimited :literal:`time` dimension.  Type this at the command line:

.. code-block:: console

   $ ncdump -ct month_01.nc | grep "time"

Then you should see this as the first line in the output:

.. code-block:: console

   time = UNLIMITED ; // (1 currently)

This indicates that the time dimension is unlimited.  If on the other
hand you see this output:

.. code-block:: console

   time = 1 ;

Then it means that the time dimension is fixed.  If this is the case,
you will have to use the :program:`ncks` command to make the time
dimension unlimited, as follows:

.. code-block:: console

   $ ncks --mk_rec_dmn time month_01.nc tmp.nc
   $ mv tmp.nc month_01.nc
   ... etc for the other files ...

Then use :program:`ncrcat` to combine the monthly data
along the time dimension, and save the result to a single netCDF file:

.. code-block:: console

   $ ncrcat -hO month_*nc annual_data.nc

You may then discard the :file:`month_*.nc` files if so desired.

.. _ncguide-concat-python:

Concatenate with Python
-----------------------

You can use the `xarray <http://xarray.pydata.org/en/stable/>`__
Python package to create a single netCDF file from multiple files. `Click
HERE
<https://github.com/geoschem/gcpy/blob/main/examples/working_with_files/concatenate_files.py>`__ to view a sample Python script that does this.

.. _ncguide-regridding:

===================
Regrid netCDF files
===================

The following tools can be used to regrid netCDF data files (such as
GEOS-Chem restart files and GEOS-Chem diagnostic files.

.. _ncguide-regrid-cdo:

Regrid with cdo
---------------
:option:`cdo` includes several tools for regridding netCDF files. For
example:

   .. code-block:: console

      # Apply conservative regridding
      $ cdo remapcon,gridfile infile.nc outfile.nc

For :file:`gridfile`, you can use the files `here
<https://geoschemdata.wustl.edu/ExtData/HEMCO/grids/>`_.  Also see
`this reference
<http://www.climate-cryosphere.org/wiki/index.php?title=Regridding_with_CDO%7Cthis>`_.

.. _ncguide-regrid-cdo-issue:

Issue with cdo remapdis regridding tool
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

GEOS-Chem user **Bram Maasakkers** wrote:

   I have noticed a problem regridding GEOS-Chem diagnostic file to
   2x2.5 using :program:`cdo` version 1.9.4. When I use:

   .. code-block:: console

      $ cdo remapdis,geos.2x25.grid GEOSChem.Restart.4x5.nc GEOSChem.Restart.2x25.nc

   The last latitudinal band (-89.5) remains empty and gets filled with
   the standard missing value of cdo, which is really large. This leads
   to immediate problems in the methane simulation as enormous
   concentrations enter the domain from the South Pole. For now I’ve
   solved this problem by just using bicubic interpolation

   .. code-block:: console

      $ cdo remapbic,geos.2x25.grid GEOSChem.Restart.4x5.nc GEOSChem.Restart.2x25.nc

You can also use conservative regridding:

.. code-block:: console

   $ cdo remapcon,geos.2x25.grid GEOSChem.Restart.4x5.nc GEOSChem.Restart.2x25.nc

.. _ncguide-

.. _ncguide-gcpy:

Regrid with GCPy
----------------

GCPy (the GEOS-Chem Python Toolkit) has contains file regridding
utilities that allow you to regrid from lat/lon to cubed-sphere grids
(and vice versa).  Regridding weights can be generated on-the-fly, or
can be archived and reused.  For detailed instructions, please see the
please see the `GCPy Regridding documentation
<https://gcpy.readthedocs.io/en/latest/Regridding.html>`_.

.. _ncguide-regrid-nco:

Regrid with nco
---------------
:option:`nco` also includes several regridding utilities.  See the
`Regridding section of the NCO User Guide
<http://nco.sourceforge.net/nco.html#Regridding>`_ for more
information.

.. _ncguide-regrid-xarray:

Regrid with xarray
------------------

The `xarray <https://xarray.readthedocs.io>`_ Python package has a
built-in capability for 1-D interpolation. It wraps the `SciPy
interpolation module
<https://docs.scipy.org/doc/scipy/reference/interpolate.html>`_. This
functionality can also be used for vertical regridding.

.. _ncguide-regrid-xesmf:

Regrid with xESMF
-----------------

`xESMF <https://xesmf.readthedocs.io>`_ is a universal regridding tool
for geospatial data, which is written in Python. It can be used to
regrid data not only on cartesian grids, but also on cubed-sphere and
unstructured grids.

.. note::

   :program:`xESMF` only handles horizontal regridding.

.. _ncguide-cropping:

=================
Crop netCDF files
=================

If needed, a netCDF file can be cropped to a subset of the globe with
the :program:`nco` or :program:`cdo` utilities
(cf. :ref:`ncguide-useful-tools`).

For example, :program:`cdo` has a :command:`selbox` operator for
selecting a box by specifying the lat/lon bounds:

.. code-block:: console

   $ cdo sellonlatbox,lon1,lon2,lat1,lat2 myfile.nc tmp.nc
   $ mv tmp.nc myfile.nc

See the `cdo guide
<https://code.zmaw.de/projects/cdo/embedded/cdo.pdf>`__ for more
information.

.. _ncguide-adding-new-var:

===================================
Add a new variable to a netCDF file
===================================

You have a couple of options for adding a new variable to a netCDF file
(for example, when having to add a new species to an existing GEOS-Chem
restart file).

#. You can use :program:`cdo` and :program:`nco` utilities to copy the
   data from one variable to another variable. For example:

   .. code-block:: bash

      #!/bin/bash

      # Extract field SpeciesRst_PMN from the original restart file
      cdo selvar,SpeciesRst_PMN initial_GEOSChem_rst.4x5_standard.nc NPMN.nc4

      # Rename selected field to SpeciesRst_NPMN
      ncrename -h -v SpeciesRst_PMN,Species_Rst_NPMN NMPN.nc4

      # Append new species to existing restart file
      ncks -h -A -M NMPN.nc4 initial_GEOSChem_rst.4x5_standard.nc

#. **Sal Farina** wrote a simple Python script for adding a new
   species to a netCDF restart file:

   .. code-block:: python

      #!/usr/bin/env python

      import netCDF4 as nc
      import sys
      import os

      for nam in sys.argv[1:]:
          f = nc.Dataset(nam,mode='a')
          try:
              o = f['SpeciesRst_OCPI']
          except:
              print "SpeciesRst_OCPI not defined"
          f.createVariable('SpeciesRst_SOAP',o.datatype,dimensions=o.dimensions,fill_value=o._FillValue)
          soap = f['SpeciesRst_SOAP']
          soap[:] = 0.0
          soap.long_name= 'SOAP species'
          soap.units =  o.units
          soap.add_offset = 0.0
          soap.scale_factor = 1.0
          soap.missing_value = 1.0e30
          f.close()

#. Bob Yantosca wrote this Python script to insert a fake species into
   GEOS-Chem Classic and GCHP restart files (13.3.0)

   .. code-block:: python

      #!/usr/bin/env python
      """
      Adds an extra DataArray for into restart files:
      Calling sequence:
          ./append_species_into_restart.py
      """
      # Imports
      import gcpy.constants as gcon
      import xarray as xr
      from xarray.coding.variables import SerializationWarning
      import warnings

      # Suppress harmless run-time warnings (mostly about underflow or NaNs)
      warnings.filterwarnings("ignore", category=RuntimeWarning)
      warnings.filterwarnings("ignore", category=UserWarning)
      warnings.filterwarnings("ignore", category=SerializationWarning)

      def main():
          """
          Appends extra species to restart files.
          """
          # Data vars to skip
          skip_vars = gcon.skip_these_vars
          # List of dates
          file_list = [
              'GEOSChem.Restart.fullchem.20190101_0000z.nc4',
              'GEOSChem.Restart.fullchem.20190701_0000z.nc4',
              'GEOSChem.Restart.TOMAS15.20190701_0000z.nc4',
              'GEOSChem.Restart.TOMAS40.20190701_0000z.nc4',
              'GCHP.Restart.fullchem.20190101_0000z.c180.nc4',
              'GCHP.Restart.fullchem.20190101_0000z.c24.nc4',
              'GCHP.Restart.fullchem.20190101_0000z.c360.nc4',
              'GCHP.Restart.fullchem.20190101_0000z.c48.nc4',
              'GCHP.Restart.fullchem.20190101_0000z.c90.nc4',
              'GCHP.Restart.fullchem.20190701_0000z.c180.nc4',
              'GCHP.Restart.fullchem.20190701_0000z.c24.nc4',
              'GCHP.Restart.fullchem.20190701_0000z.c360.nc4',
              'GCHP.Restart.fullchem.20190701_0000z.c48.nc4',
              'GCHP.Restart.fullchem.20190701_0000z.c90.nc4'
          ]
          # Keep all netCDF attributes
          with xr.set_options(keep_attrs=True):
              # Loop over dates
              for f in file_list:
                  # Input and output files
                  infile = '../' + f
                  outfile = f
                  print("Creating " + outfile)

                  # Open input file
                  ds = xr.open_dataset(infile, drop_variables=skip_vars)
                  # Create a new DataArray from a given species (EDIT ACCORDINGLY)
                  if "GCHP" in infile:
                      dr = ds["SPC_ETO"]
                      dr.name = "SPC_ETOO"
                  else:
                      dr = ds["SpeciesRst_ETO"]
                      dr.name = "SpeciesRst_ETOO"

                  # Update attributes (EDIT ACCORDINGLY)
                  dr.attrs["FullName"] = "peroxy radical from ethene"
                  dr.attrs["Is_Gas"] = "true"
                  dr.attrs["long_name"] = "Dry mixing ratio of species ETOO"
                  dr.attrs["MW_g"] = 77.06
                  # Merge the new DataArray into the Dataset
                  ds = xr.merge([ds, dr], compat="override")

                  # Create a new file
                  ds.to_netcdf(outfile)

                  # Free memory by setting ds to a null dataset
                  ds = xr.Dataset()

      if __name__ == "__main__":
          main()

.. _ncguide-chunk-deflate:

==============================================
Chunk and deflate a netCDF file to improve I/O
==============================================

We recommend that you **chunk** the data in your netCDF file. Chunking
specifies the order in along which the data will be read from
disk. The Unidata web site has `a good overview of why chunking a
netCDF file matters
<https://www.unidata.ucar.edu/blogs/developer/entry/chunking_data_why_it_matters>`_.

For `GEOS-Chem with the high-performance option (aka GCHP)
<https://gchp.readthedocs.io>`_, the best file I/O performance occurs
when the file is split into one chunk per level (assuming your data
has a lev dimension). This allows each individual vertical level of
data to be read in parallel.

You can use the :program:`nccopy` command of :option:`nco` to do the
chunking. For example, say you have a netCDF file called
:file:`myfile.nc` with these dimensions:

.. code-block:: console

   dimensions:
           time = UNLIMITED ; // (12 currently)
           lev = 72 ;
           lat = 181 ;
           lon = 360 ;

Then you can use the :program:`nccopy` command to apply the optimal
chunking along levels:

.. code-block:: console

   $ nccopy -c lon/360,lat/181,lev/1,time/1 -d1 myfile.nc tmp.nc
   $ mv tmp.nc myfile.nc

This will create a new file called :file:`tmp.nc` that has the proper
chunking. We then replace :file:`myfile.nc` with this temporary file.

You can specify the chunk sizes that will be applied to the variables
in the netCDF file with the :command:`-c`  argument to
:program:`nccopy`. To obtain the optimal chunking, the :literal:`lon`
chunksize must be identical to the number of values along the
longitude dimension (e.g. :literal:`lon/360` and the :literal:`lat`
chunksize must be equal to the number of points in the latitude
dimension (e.g. :literal:`lat/181`).

We also recommend that you :program:`deflate` (i.e. compress) the
netCDF data variables at the same time you apply the
chunking. Deflating can substantially reduce the file size, especially
for emissions data that are only defined over the land but not over
the oceans. You can deflate the data in a netCDF file by specifying
the \ -d\  argumetnt to nccopy. There are 10 possible deflation
levels, ranging from 0 (no deflation) to 9 (max deflation). For most
purposes, a deflation level of 1 (:command:`d1`) is sufficient.

The `GEOS-Chem Support Team
<https://wiki.geos-chem.org/GEOS-Chem_Support_Team>`_ has created a
Perl script named  `nc_chunk.pl
<https://github.com/geoschem/netcdf-scripts/blob/main/scripts/nc_chunk.pl>`_
(contained in the `netcdf-scripts repository at GitHub
<https://github.com/geoschem/netcdf-scripts>`_)  that will
automatically chunk and  compress data for you.

.. code-block:: console

   $ nc_chunk.pl myfile.nc    # Chunk netCDF file
   $ nc_chunk.pl myfile.nc 1  # Chunk and compress file using deflate level 1

You can use the :command:`ncdump -cts myfile.nc` command to view the chunk size
and deflation level in the file. After applying the chunking and
compression to myfile.nc, you would see output such as this:

.. code-block:: console

    dimensions:
            time = UNLIMITED ; // (12 currently)
            lev = 72 ;
            lat = 181 ;
            lon = 360 ;
    variables:
            float PRPE(time, lev, lat, lon) ;
                    PRPE:long_name = "Propene" ;
                    PRPE:units = "kgC/m2/s" ;
                    PRPE:add_offset = 0.f ;
                    PRPE:scale_factor = 1.f ;
                    PRPE:_FillValue = 1.e+15f ;
                    PRPE:missing_value = 1.e+15f ;
                    PRPE:gamap_category = "ANTHSRCE" ;
                    PRPE:_Storage = "chunked" ;
                    PRPE:_ChunkSizes = 1, 1, 181, 360 ;
                    PRPE:_DeflateLevel = 1 ;
                    PRPE:_Endianness = "little" ;\
            float CO(time, lev, lat, lon) ;
                    CO:long_name = "CO" ;
                    CO:units = "kg/m2/s" ;
                    CO:add_offset = 0.f ;
                    CO:scale_factor = 1.f ;
                    CO:_FillValue = 1.e+15f ;
                    CO:missing_value = 1.e+15f ;
                    CO:gamap_category = "ANTHSRCE" ;
                    CO:_Storage = "chunked" ;
                    CO:_ChunkSizes = 1, 1, 181, 360 ;
                    CO:_DeflateLevel = 1 ;
                    CO:_Endianness = "little" ;\

The attributes that begin with a :literal:`_` character are "hidden"
netCDF attributes. They represent file properties instead of
user-defined properties (like the long name, units, etc.). The
"hidden" attributes can be shown by adding the :command:`-s` argument
to :command:`ncdump`.
