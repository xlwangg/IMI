.. _libguide:

###################################
Load software into your environment
###################################

This supplemental guide describes the how to load the
required software dependencies for :program:`GEOS-Chem` and
:program:`HEMCO` into your computational environment.

.. _libguide-cloud:

================================
On the Amazon Web Services Cloud
================================

All of the required software dependencies for :program:`GEOS-Chem` and
:program:`HEMCO` will be included in the Amazon Machine Image (AMI)
that you use to initialize your Amazon Elastic Cloud Compute (EC2)
instance. For more information, please see our `our GEOS-Chem cloud
computing tutorial <http://geos-chem-cloud.readthedocs.io>`_.

.. _libguide-cluster:

============================
On a shared computer cluster
============================

If you plan to use :program:`GEOS-Chem` or :program:`HEMCO` on a
shared computational cluster (e.g. at a university or research
institution), then there is a good chance that your IT staff will have
already installed several of the required software dependencis.

Depending on your system's setup, there are a few different ways in
which you can activate these software pacakges in your computational
environment, as shown below.

.. _libguide-check-modules:

1. Check if libraries are available as modules
----------------------------------------------
Many high-performance computing (HPC) clusters use a module manager such
as `Lmod <https://lmod.readthedocs.io/en/latest/>`_ or
`environment-modules <https://modules.readthedocs.io/en/latest/>`_
to load software packages and libraries. A module manager allows you to
load different compilers and libraries with simple commands.

One downside of using a module manager is that you are locked into using
only those compiler and software versions that have already been
installed on your system by your sysadmin or IT support staff.  But in
general, module managers succeed in ensuring that only well-tested
compiler/software combinations are made available to users.

.. tip::

   Ask your sysadmin or IT support staff for the software loading
   instructions specific to your system.

.. _example-loading-gcc-820:

1a. Module load example
~~~~~~~~~~~~~~~~~~~~~~~

The commands shown below are specific to the Harvard Cannon
cluster, but serve to demonstrate the process.  Note that the names of
software packages being loaded may contain version numbers and/or ID
strings that serve to differentiate one build from another.

.. code-block:: console

   $ module load gcc/10.2.0-fasrc01             # gcc / g++ / gfortran
   $ module load openmpi/4.1.0-fasrc01          # MPI
   $ module load netcdf-c/4.8.0-fasrc01         # netcdf-c
   $ module load netcdf-fortran/4.5.3-fasrc01   # netcdf-fortran
   $ module load flex/2.6.4-fasrc01             # Flex lexer (needed for KPP)
   $ module load cmake/3.25.2-fasrc01           # CMake (needed to compile)

Note that it is often not necessary to load all modules.  For example,
loading :program:`netcdf-fortran` will also cause its dependencies
(such as :program:`netcdf-c`, :program:`hdf5`, etc.) to also be loaded
into your environment.

Here is a summary of what the above commands do:

.. option:: module purge

   Removes all previously loaded modules

.. option:: module load git/...

   Loads Git (version control system)

.. option:: module load gcc/...

   Loads the GNU Compiler Collection (suite of C, C++, and Fortran
   compilers)

.. option:: module load openmpi/...

   Loads the OpenMPI library (a dependency of netCDF)

.. option:: module load netcdf/..

   Loads the netCDF library

   .. important::

      Depending on how the netCDF libraries have been installed on
      your system, you might also need to load the netCDF-Fortran
      library separately, e.g.:

      .. code-block:: console

	 module load netcdf-fortran/...

.. option:: module load perl/...

   Loads Perl (scripting language)

.. option:: module load cmake/...

   Loads Cmake (needed to compile GEOS-Chem)

.. option:: module load flex/...

   Loads the Flex lexer (needed for `The Kinetic PreProcessor
   <https://kpp.readthedocs.io>`_).


.. _libguide-check-spack:

2. Check if Spack-built libraries are available
-----------------------------------------------

If your system doesn't have a module manager installed, check to see
if the required libraries for :program:`GEOS-Chem` and
:program:`HEMCO` were :ref:`built with the Spack package manager
<spackguide>`.  Type

.. code-block:: console

   $ spack find

to locate any Spack-built software libraries on your system.  If there
Spack-built libraries are found, you may present, you may load them
into your computational environment with :program:`spack load`
commands such as:

.. code-block:: console

   $ spack load gcc@10.2.0
   $ spack load netcdf-c%gcc@10.2.0
   $ spack load netcdf-fortran%gcc@10.2.0
   ... etc ...

When loading a Spack-built library, you can specify its version
number.  For example, :command:`spack load gcc@10.2.0` tells Spack to
load the GNU Compiler Collection version 10.2.0.

You may also specify a library by the compiler it was built with.  For
example, :command:`spack load netcdf-fortran%gcc@10.2.0` tells Spack
to load the version of netCDF-Fortran that was built with GNU Compiler
Collection version 10.2.0.

These specification methods are often necessary to select a given
library in case there are several available builds to choose from.

We recommend that you place :command:`spack load` commands into an
`environment file
<https://geos-chem.readthedocs.io/getting-started/login-env-files.html>`_.

If a `Spack environment
<https://spack-tutorial.readthedocs.io/en/latest/tutorial_environments.html>`_
has been installed on your system, type:

.. code-block:: console

   spack env activate -p ENVIRONMENT-NAME

to load all of the libraries in the environment together.

To deactivate the environment, type:

.. code-block:: console

   spack deactivate

.. _libguide-check-manual:

3. Check if libaries have been manually installed
-------------------------------------------------

If your computer system does not use a module manager and does not use
Spack, check for a manual library installation. Very often, common
software libraries are installed into standard locations (such as the
:file:`/usr/lib` or :file:`/usr/local/lib` system folders).  Ask your
sysadmin for more information.

Once you know the location of the compiler and netCDF libraries, you can
set the proper environment variables for GEOS-Chem and HEMCO.

.. _libguide-install-spack:

4. If there are none of these, install them with Spack
------------------------------------------------------

If your system has none of the required software packages that
:program:`GEOS-Chem` and :program:`HEMCO` need, then we recommend that
you :ref:`use Spack to build the libraries yourself <spackguide>`.
Spack makes the process easy and will make sure that all software
dependences are resolved.

Once you have installed the libraries with Spack, you can load the
libraries into your computational environment :ref:`as described above
<libguide-check-spack>`.
