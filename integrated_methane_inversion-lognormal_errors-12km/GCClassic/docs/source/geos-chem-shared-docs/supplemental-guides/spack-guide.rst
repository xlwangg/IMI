.. |br| raw:: html

   <br />

.. _spackguide:

##################################
Build required software with Spack
##################################

This page has instructions for building **dependencies** for
`GEOS-Chem Classic <https://geos-chem.readthedocs.io>`_, `GCHP
<https://gchp.readthedocs.io>`_, and `HEMCO
<https://hemco.readthedocs.io>`_ These are the **software libraries**
that are needed to compile and execute these programs.


Before proceeding, please also check if the dependencies for
GEOS-Chem, GCHP, and HEMCO are already found on your computational
cluster or cloud environment. If this is the case, you may use the
pre-installed versions of these software libraries and won't have
to install your own versions.

For more information about software dependencies, see:

- `GEOS-Chem Classic software requirements <https://geos-chem.readthedocs.io/en/stable/gcc-guide/01-startup/system-req-soft.html>`_
- `GCHP software requirements <https://gchp.readthedocs.io/en/stable/getting-started/requirements.html#software-requirements>`_
- `HEMCO software requirements <https://hemco.readthedocs.io/en/stable/hco-sa-guide/software.html>`_

.. _spackguide-intro:

============
Introduction
============

In the sections below, we will show you how to **build a single
software environment containing all software dependencies for GEOS-Chem
Classic, GCHP, and HEMCO**.  This will be especially of use for those
users working on a computational cluster where these dependencies have
not yet been installed.

We will be using the `Spack <https://spack.readthedocs.io>`_ package
manager to download and build all required software dependencies for GEOS-Chem
Classic, GCHP and HEMCO.

.. note::

   Spack is not the only way to build the dependencies.
   It is possible to download and compile the source code for each
   library manually.  Spack automates this process, thus it is the
   recommended method.

You will be using this workflow:

#. :ref:`spackguide-setup`
#. :ref:`spackguide-model`
#. :ref:`spackguide-compiler`
#. :ref:`spackguide-build`
#. :ref:`spackguide-envfile`
#. :ref:`spackguide-cleanup`

.. _spackguide-setup:

=====================================
Install Spack and do first-time setup
=====================================

Decide where you want to install Spack (aka the **Spack root
directory**).  A few details you should consider are:

- The Spack root directory will be ~5-10 GB.  Keep in mind that some
  computational clusters restrict the size of your home directory (aka
  :code:`${HOME}`) to a few GB). |br|
  |br|

- This Spack root directory cannot be moved.  Instead, you will have
  to reinstall Spack to a different directory location (and rebuild
  all software packages). |br|
  |br|

- The Spack root directory should be placed in a shared drive if
  several users need to access it.

Once you have chosen an location for the Spack root directory, you may
continue with the Spack download and setup process.

.. important::

   Execute all commands in this tutorial from the same directory.
   This is typically one directory level higher than the Spack root
   directory.

   For example, if you install Spack as a subdirectory of
   :envvar:`${HOME}`, then you will issue all commands from
   :envvar:`${HOME}`.

Use the commands listed below to install Spack and perform first-time
setup.  You can copy-paste these commands, but lookout for lines
marked with a  :literal:`# (modifiable) ...` comment as they might
require modification.

.. code-block:: console

   $ cd ${HOME}                             # (modifiable) cd to the install location you chose

   $ git clone -c feature.manyFiles=true https://github.com/spack/spack.git  # download Spack

   $ source spack/share/spack/setup-env.sh  # Load Spack

   $ spack external find                    # Tell Spack to look for existing software

   $ spack compiler find                    # Tell Spack to look for existing complilers

.. note::

   If you should encounter this error:

   .. code-block:: console

      $ spack external find
      ==> Error: 'name'

   then Spack could not find any external software on your system.

   Spack searches for executables that are located within your search
   path (i.e. the list of directories contained in your :envvar:`$PATH`
   environment variable), but not within software modules. Because of
   this, you might have to :ref:`load a software package into your
   environment <libguide>` before Spack can detect it.  Ask your
   sysadmin or IT staff for more information about your system's
   specific setup.

After the first-time setup has been completed, an environment variable
named  :envvar:`SPACK_ROOT`, will be created in your Unix/Linux
environment.  This contains to the absolute path of the Spack root
directory.  Use this command to view the value of :envvar:`SPACK_ROOT`:

.. code-block:: console

   $ echo ${SPACK_ROOT}
   /path/to/home/spack    # Path to Spack root, assumes installation to a subdir of ${HOME}

.. _spackguide-model:

=========================================
Clone a copy of GCClassic, GCHP, or HEMCO
=========================================

The `GCClassic  <https://github.com/geoschem/GCClassic>`_, `GCHP
<https://github.com/geoschem/GCHP>`_ , and `HEMCO
<https://github.com/geoschem/HEMCO>`_ repositories each contain a
:file:`spack/` subdirectory with customized Spack configuration files
:file:`modules.yaml` and :file:`packages.yaml`.  We have updated these
YAML files with the proper settings in order to ensure a smooth
software build process with Spack.

First, define the :envvar:`model`, :envvar:`scope_dir`, and
:envvar:`scope_args` environment variables as shown below.

.. code-block:: console

   $ model=GCClassic               # Use this if you will be working with GEOS-Chem Classic
   $ model=GCHP                    # Use this if you will be working with GCHP
   $ model=HEMCO                   # Use this if you will be working with HEMCO standalone

   $ scope_dir="${model}/spack"    # Folder where customized YAML files are stored

   $ scope_args="-C ${scope_dir}"  # Tell spack to for custom YAML files in scope_dir

You will use these environment variables in the steps below.

When you have completed this step, download the source code for your
preferred model (e.g. GEOS-Chem Classic, GCHP, or HEMCO standalone):

.. code-block:: console

   $ git clone --recurse-submodules https://github.com/geoschem/${model}.git

.. _spackguide-compiler:

================================
Install the recommended compiler
================================

Next, install the recommended compiler, :program:`gcc` (aka the GNU
Compiler Collection).  Use the :envvar:`scope_args` environment
variable that you defined in the :ref:`previous step <spackguide-model>`.

.. code-block:: console

   $ spack ${scope_args} install gcc     # Install GNU Compiler Collection

.. note::

   Requested version numbers for software packages (including the
   compiler) are listed in the :literal:`${scope_dir}/packages.yaml`
   file.  We have selected software package versions that have been
   proven to work together.  You should not have to change any of
   the settings in :literal:`${scope_dir}/packages.yaml`.

   As of this writing, the default compiler is `gcc 10.2.0
   <https://gcc.gnu.org/onlinedocs/10.2.0/>`_ (includes C, C++, and
   Fortran compilers).  We will upgrade to newer compiler and software
   package versions as necessary.

The compiler installation should take several minutes (or longer if
you have a slow internet connection).

Register the compiler with Spack after it has been installed.  This
will allow Spack to use this compiler to build other software
packages.  Use this command:

.. code-block:: console

   $ spack compiler add $(spack location -i gcc)     # Register GNU Compiler Collection

You will then see output similar to this:

.. code-block:: console

   ==> Added 1 new compiler to /path/to/home/.spack/linux/compilers.yaml
       gcc@X.Y.Z
   ==> Compilers are defined in the following files:
       /path/to/home/.spack/linux/compilers.yaml

where

- :file:`/path/to/home` indicates the absolute path of your home
  directory (aka :literal:`${HOME}`)
- :literal:`X.Y.Z` indicates the version of the GCC compiler that you
  just built with Spack.

.. tip::

   Use this command to view the list of compilers that have been
   registered with Spack:

   .. code-block:: console

      $ spack compiler list

   Use this command to view the installation location for a
   Spackguide-built software package:

   .. code-block:: console

      $ spack location -i <package-name>

.. _spackguide-build:

=============================================
Build GEOS-Chem dependencies and useful tools
=============================================

Once the compiiler has been built and registered, you may proceed to
building the software dependencies for GEOS-Chem Classic, GCHP, and
HEMCO.

The Spack installation commands that you will use take the form:

.. code-block:: console

   $ spack ${scope_args} install <package-name>%gcc^openmpi

where

- :literal:`${scope_args}` is the environment variable that
  :ref:`you defined above <spackguide-model>`; |br|
  |br|

- :literal:`<package-name>` is a placeholder for the name of the
  software package that you wish to install; |br|
  |br|

- :literal:`%gcc` tells Spack that it should use the GNU Compiler
  Collection version that you just built; |br|
  |br|

- :literal:`^openmpi` tells Spack to use OpenMPI when building
  software packages.  You may omit this setting for packages that do
  not require it.

Spack will download and build :literal:`<package-name>` plus all of
its dependencies that have not already been installed.

.. note::

   Use this command to find out what other packages will be built
   along with :literal:`<package-name>`:

   .. code-block:: console

      $ spack spec <package-name>

   This step is not required, but may be useful for informational
   purposes.

Use the following commands to build dependencies for GEOS-Chem
Classic, GCHP, and HEMCO, as well as some useful tools for working
with GEOS-Chem data:

#. Build the :program:`esmf` (Earth System Model Framework),
   :program:`hdf5`, :program:`netcdf-c`, :program:`netcdf-fortran`,
   and :program:`openmpi` packages:

   .. code-block:: console

      $ spack ${scope_args} install esmf%gcc^openmpi

   The above command will build all of the above-mentioned packages in
   a single step.

   .. note::

      GEOS-Chem Classic does not require :program:`esmf`.  However, we
      recommend that you build ESMF anyway so that it will already be
      installed in case you decide to use GCHP in the future.

   |br|

#. Build the :program:`cdo` (Climate Data Operators) and :program:`nco`
   (netCDF operators) packages.  These are command-line tools for
   editing and manipulating data contained in netCDF files.

   .. code-block:: console

      $ spack ${scope_args} install cdo%gcc^openmpi

      $ spack ${scope_args} install nco%gcc^openmpi

   |br|

#. Build the :program:`ncview` package, which is a quick-and-dirty
   netCDF file viewer.

   .. code-block:: console

      $ spack ${scope_args} install ncview%gcc^openmpi

   |br|

#. Build the :program:`flex` (Fast Lexical Analyzer) package.  This is
   a dependency of the `Kinetic PreProcessor (KPP)
   <https://kpp.readthedocs.io>`_, with which you can update GEOS-Chem
   chemical mechanisms.

   .. code-block:: console

      $ spack ${scope_args} install flex%gcc

   .. note::

      The :program:`flex` package does not use OpenMPI.  Therefore, we
      can omit :literal:`^openmpi` from the above command.

At any time, you may see a list of installed packages by using this
command:

.. code-block:: console

   $ spack find

.. _spackguide-envfile:

====================================================
Add ``spack load`` commands to your environment file
====================================================

We recommend "sourcing" the load_script that you created in the
:ref:`previous section <spackguide-build>` from within an **environment
file**.  This is a file that not only loads the required modules but
also defines settings that you need to run GEOS-Chem Classic, GCHP, or
the HEMCO standalone.

Please see the following links for sample environment files.

- `Sample GEOS-Chem Classic environment file
  <https://geos-chem.readthedocs.io/en/stable/gcc-guide/01-startup/login-env-files-gnu.html>`_
- `Sample GCHP environment file
  <https://github.com/geoschem/geos-chem/blob/main/run/GCHP/runScriptSamples/operational_examples/harvard_cannon/gchp.gfortran10.2_openmpi4_cannon.env>`_
- `Sample HEMCO environment file
  <https://hemco.readthedocs.io/en/stable/hco-sa-guide/login-env.html>`_

Copy and paste the code below into a file named :code:`${model}.env` (using
the :code:`${model}` environment variable that :ref:`you defined
above <spackguide-model>`).  Then replace any existing :code:`module load`
commands with the following code:

.. code-block:: bash

   #=========================================================================
   # Load Spackguide-built modules
   #=========================================================================

   # Setup Spack if it hasn't already been done
   # ${SPACK_ROOT} will be blank if the setup-env.sh script hasn't been called.
   # (modifiable) Replace "/path/to/spack" with the path to your Spack root directory
   if [[ "x${SPACK_ROOT}" == "x" ]]; fi
      source /path/to/spack/source/spack/setup-env.sh
   fi

   # Load esmf, hdf5, netcdf-c, netcdf-fortran, openmpi
   spack load esmf%gcc^openmpi

   # Load netCDF packages (cdo, nco, ncview)
   spack load cdo%gcc^openmpi
   spack load nco%gcc^openmpi
   spack load ncview

   # Load flex
   spack load flex

   #=========================================================================
   # Set environment variables for compilers
   #=========================================================================
   export CC=gcc
   export CXX=g++
   export FC=gfortran
   export F77=gfortran

   #=========================================================================
   # Set environment variables for Spack-built modules
   #=========================================================================

   # openmpi (needed for GCHP)
   export MPI_ROOT=$(spack-location -i openmpi%gcc)

   # esmf (needed for GCHP)
   export ESMF_DIR=$(spack location -i esmf%gcc^openmpi)
   export ESMF_LIB=${ESMF_DIR}/lib
   export ESMF_COMPILER=gfortran
   export ESMF_COMM=openmpi
   export ESMF_INSTALL_PREFIX=${ESMF_DIR}/INSTALL_gfortran10_openmpi4

   # netcdf-c
   export NETCDF_HOME=$(spack location -i netcdf-c%gcc^openmpi)
   export NETCDF_LIB=$NETCDF_HOME/lib

   # netcdf-fortran
   export NETCDF_FORTRAN_HOME=$(spack location -i netcdf-fortran%gcc^openmpi)
   export NETCDF_FORTRAN_LIB=$NETCDF_FORTRAN_HOME/lib

   # flex
   export FLEX_HOME=$(spack location -i flex%gcc^openmpi)
   export FLEX_LIB=$NETCDF_FORTRAN_HOME/lib
   export KPP_FLEX_LIB_DIR=${FLEX_LIB}       # OPTIONAL: Needed for KPP

To apply these settings into your login environment, type

.. code-block:: console

   source ${model}.env  # One of GCClassic.env, GCHP.env, HEMCO.env

To test if the modules have been loaded properly, type:

.. code-block:: console

   $ nf-config --help   # netcdf-fortran configuration utility

If you see a screen similar to this, you know that the modules have
been installed properly.

.. code-block:: console

   Usage: nf-config [OPTION]

   Available values for OPTION include:

     --help        display this help message and exit
     --all         display all options
     --cc          C compiler
     --fc          Fortran compiler
     --cflags      pre-processor and compiler flags
     --fflags      flags needed to compile a Fortran program
     --has-dap     whether OPeNDAP is enabled in this build
     --has-nc2     whether NetCDF-2 API is enabled
     --has-nc4     whether NetCDF-4/HDF-5 is enabled in this build
     --has-f90     whether Fortran 90 API is enabled in this build
     --has-f03     whether Fortran 2003 API is enabled in this build
     --flibs       libraries needed to link a Fortran program
     --prefix      Install prefix
     --includedir  Include directory
     --version     Library version

.. _spackguide-cleanup:

========
Clean up
========

At this point, you can remove the :code:`${model}` directory as it is
not needed.  (Unless you would like to keep it to build the executable
for your research with GEOS-Chem Classic, GCHP, or HEMCO.)

The :file:`spack` directory needs to remain.  :ref:`As mentioned above
<spackguide-setup>`, this directory cannot be moved.

You can clean up any Spack temporary build stage information with:

.. code-block:: console

   $ spack clean -m
   ==> Removing cached information on repositories

That's it!
