.. _debug-guide:

################################
Debug GEOS-Chem and HEMCO errors
################################

If your :program:`GEOS-Chem` or :program:`HEMCO` simulation dies
unexpectedly with an error or takes much longer to execute than it
should, the most important thing is to try to isolate the source of
the error or bottleneck right away. Below are some debugging tips that
you can use.

.. _debug-guide-github:

=============================================
Check if a solution has been posted to Github
=============================================

We have migrated support requests from the `GEOS-Chem wiki
<https://wiki.geos-chem.org>`_ to **Github issues**.  A quick search
of Github issues (both open and closed) might reveal the answer to your
question or provide a solution to your problem.

You should also feel free to open a new issue at one of these Github
links:

- `GEOS-Chem Classic new issues page
  <https://github.com/geoschem/geos-chem/issues/new/choose/>`_
- `GCHP new issues page
  <https://github.com/geoschem/GCHP/issues/new/choose>`_
- `HEMCO new issues page
  <https://github.com/geoschem/HEMCO/issues/new/choose>`_

If you are new to Github, we recommend viewing our Github tutorial
videos at `our GEOS-Chem Youtube site <https://youtube.com/c/geoschem>`_.

.. _debug-guide-config:

==============================================================
Check if your computational environment is configured properly
==============================================================

Many :program:`GEOS-Chem` and :program:`HEMCO` errors occur due to
improper configuration settings (i.e. missing libraries,
incorrectly-specified environment variables, etc.) in your
computational environment.  Take a moment and refer back to these
manual pages (on ReadTheDocs) for information on configuring your
environment:

- `GEOS-Chem Classic manual <https://geos-chem.readthedocs.io>`_
- `GCHP manual <https://gchp.readthedocs.io>`_
- `HEMCO manual <https://hemco.readthedocs.io>`_

.. _debug-guide-usermod:

================================================
Check any code modifications that you have added
================================================

If you have made modifications to a "fresh out-of-the-box"
:program:`GEOS-Chem` or :program:`HEMCO` version, look over your code
edits to search for sources of potential error.

You can also use Git to revert to the last stable version, which is
always in the :command:`main` branch.

.. _debug-guide-limits:

=================================================
Check if your runs exceeded time or memory limits
=================================================

If you are running :program:`GEOS-Chem` or :program:`HEMCO` on a
shared computer system, you will probably have to use a **job
scheduler** (such as :program:`SLURM`) to submit your jobs to a
computational queue. You should be aware of the run time and memory
limits for each of the queues on your system.

If your job uses more memory or run time than the computational queue
allows, it can be cancelled by the scheduler. You will usually get an
error message printed out to the stderr stream, and maybe also an
email stating that the run was terminated. Be sure to check all of the
log files created by your jobs for such error messages.

To solve this issue, try submitting your :program:`GEOS-Chem` or
:program:`HEMCO` simulations to a queue with larger run-time and
memory limits.  You can also try splitting up your long simulations
into several smaller stages (e.g. monthly) that take less time to run
to completion.

.. _debug-guide-printout:

====================================
Send debug printout to the log files
====================================

If your :program:`GEOS-Chem` simulation stopped with an error, but you
cannot tell where, turn on the the :code:`debug_printout` option.
This is found in the **Simulation Settings** section of
:file:`geoschem_config.yml`:

.. code-block:: yaml

   #============================================================================
   # Simulation settings
   #============================================================================
   simulation:
     name: fullchem
     start_date: [20190701, 000000]
     end_date: [20190801, 000000]
     root_data_dir: /path/to/ExtData
     met_field: MERRA2
     species_database_file: ./species_database.yml
     debug_printout: false  # <---- set this to true
     use_gcclassic_timers: false

This will send additional output to the :program:`GEOS-Chem` log file,
which may help you to determine where the simulation stopped.

If your :program:`HEMCO` simulation stopped with an error, turn on debug
printout by editing the :code:`Verbose` and :code:`Warnings` settings
at the top of the :file:`HEMCO_Config.rc` configuration file:

.. code-block:: console

   ###############################################################################
   ### BEGIN SECTION SETTINGS
   ###############################################################################

   ROOT:                        /path/to/ExtData/HEMCO
   METDIR:                      MERRA2
   GCAP2SCENARIO:               none
   GCAP2VERTRES:                none
   Logfile:                     HEMCO.log
   DiagnFile:                   HEMCO_Diagn.rc
   DiagnPrefix:                 ./OutputDir/HEMCO_diagnostics
   DiagnFreq:                   Monthly
   Wildcard:                    *
   Separator:                   /
   Unit tolerance:              1
   Negative values:             0
   Only unitless scale factors: false
   Verbose:                     0      # <---- set this to 3
   Warnings:                    1      # <---- set this to 3

Both :code:`Verbose` and :code:`Warnings` settings can have values
from 0 to 3.  The higher the number, the more information will be
printed out to the :file:`HEMCO.log` file.  A value of 0 disables
debug printout.

Having this extra debug printout in your log file output may provide
insight as to where your simulation is halting.

.. _debug-guide-traceback:

============================
Look at the traceback output
============================

An **error traceback** will be printed out whenever a
:program:`GEOS-Chem` or :program:`HEMCO` simulation halts with an
error.  This is a list of routines that were called when the error
occurred.

An sample error traceback is shown here:

.. code-block:: console

   forrtl: severe (174): SIGSEGV, segmentation fault occurred

   Image              PC                Routine            Line        Source
   gcclassic          0000000000C82023  Unknown               Unknown  Unknown
   libpthread-2.17.s  00002AACE8015630  Unknown               Unknown  Unknown
   gcclassic          000000000095935E  error_mod_mp_erro         437  error_mod.F90
   gcclassic          000000000040ABB7  MAIN__                    422  main.F90
   gcclassic          0000000000406B92  Unknown               Unknown  Unknown
   libc-2.17.so       00002AACE8244555  __libc_start_main     Unknown  Unknown
   gcclassic          0000000000406AA9  Unknown               Unknown  Unknown

The top line with a valid routine name and line number printed is the
routine that exited with an error (:file:`error_mod.F90`, line 437).
You might also have to look at the other listed files as well to get
some more information about the error (e.g. :file:`main.F90`, line
422).

.. _debug-happens-consistently:

===============================================
Identify whether the error happens consistently
===============================================

If your :program:`GEOS-Chem` or :program:`HEMCO` error always happens
at the same model date and time, this could indicate corrupted
meteorology or emissions input data files. In this case, you may be
able to fix the issue simply by re-downloading the files to your disk
space.

If the error happened only once, it could be caused by a network
problem or other such transient condition.

.. _debug-guide-isolate:

===========================================
Isolate the error to a particular operation
===========================================

If you are not sure where a :program:`GEOS-Chem` error is occurring,
turn off operations (such as transport, chemistry, dry deposition,
etc.) one at a time in the :file:`geoschem_config.yml` configuration
file, and rerun your simulation.

Similarly, if you are debugging a :program:`HEMCO` error, turn off
different emissions inventories and extensions one at a time in the
:file:`HEMCO_Config.rc` file, and rerun your simulation.

Repeating this process should eventually lead you to the source of the
error.

.. _debug-guide-debug-flags:

==============================
Compile with debugging options
==============================

You can compile :program:`GEOS-Chem` or :program:`HEMCO` in debug
mode.  This will activate several additional error run-time error
checks (such as looking for assignments that go outside of array
bounds or floating point math errors) that can give you more insight
as to where your simulation is dying.

Configure your code for debug mode with the
:command:`-DCMAKE_RELEASE_TYPE=Debug` option.  From your run
directory, type these commands:

.. code-block:: console

   cd build
   cmake ../CodeDir -DCMAKE_RELEASE_TYPE=Debug -DRUNDIR=..
   make -j
   make -j install
   cd ..

.. attention::

   Compiling in debug mode will add a significant amount of
   computational overhead to your simulation.  Therefore, we recommend
   to activate these additional error checks only in short simulations
   and not in long production runs.

.. _debug-guide-debugger:

==============
Use a debugger
==============

You can save yourself a lot of time and hassle by using a debugger
such as :program:`gdb` (the GNU debugger).  With a debugger you can:

-  Examine data when a program stops
-  Navigate the stack when a program stops
-  Set break points

To run :program:`GEOS-Chem` or :program:`HEMCO` in the :program:`gdb`
debugger, you should first :ref:`compile in debug mode
<debug-guide-debug-flags>`. This will turn on the :code:`-g` compiler
flag (which tells the compiler to generate symbolic information for
debugging) and the :code:`-O0` compiler flag (which shuts off all
optimizations.  Once the executable has been created, type one of the
following commands, which will start :program:`gdb`:

.. code-block:: console

   $ gdb gcclassic    # for GEOS-Chem Classic
   $ gdb gchp         # for GCHP
   $ gdb hemco        # for HEMCO standalone

At the :program:`gdb` prompt, type one of these commands:

.. code-block:: console

   (gdb) run                     # for GEOS-Chem Classic or GCHP
   (gdb) run HEMCO_sa_Config.rc  # for HEMCO standalone

With :program:`gdb`, you can also go directly to the point of
the error without having to re-run GEOS-Chem or HEMCO.  When your
GEOS-Chem or HEMCO simulation dies, it will create a **corefile**
such as  :file:`core.12345`.  The :code:`12345` refers to the process
ID assigned to your executable by the operating system; this number is
different for each running process on your system.

Typing one of these commands:

.. code-block:: console

   $ gdb gcclassic core.12345         # for GEOS-Chem Classic
   $ gdb gchp core.12345              # for GCHP
   $ gdb hemco_standalone core.12345  # for HEMCO standalone

will open :program:`gdb` and bring you immediately to the point of the
error.  If you then type at the :code:`(gdb)` prompt:

.. code-block:: console

   (gdb) where

You will get a :ref:`traceback <debug-guide-traceback>` listing.

To exit :program:`gdb`, type :code:`quit`.

.. _debug-guide-print-it-out:

=================================
Print it out if you are in doubt!
=================================

Add :code:`print*,` statements to write values of variables in the
area of the code where you suspect the error is occurring.  Also add
the :code:`call flush(6)` statement to flush the output to the screen
and/or log file immediately after printing.  Maybe you will see
something wrong in the output.

You can often detect numerical errors by adding debugging print
statements into your source code:

#. Use :code:`MINVAL` and :code:`MAXVAL` functions to get the minimum
   and maximum values of an array:

   .. code-block:: fortran

      PRINT*, '### Min, Max: ', MINVAL( ARRAY ), MAXVAL( ARRAY )
      CALL FLUSH( 6 )

#. Use the :code:`SUM` function to check the sum of an array:

   .. code-block:: fortran

      PRINT*, '### Sum of X : ', SUM( ARRAY )
      CALL FLUSH( 6 )

.. _debug-guide-brute-force:

==============================================
Use the brute-force method when all else fails
==============================================

If the bug is difficult to locate, then comment out a large section of
code and run your :program:`GEOS-Chem` or :program:`HEMCO` simulation
again.  If the error does not occur, then uncomment some more code and
run again.  Repeat the process until you find the location of the
error. The brute force method may be tedious, but it will usually lead
you to the source of the problem.

.. _debug-guide-profiling:

===============================================
Identify poorly-performing code with a profiler
===============================================

If you think your :program:`GEOS-Chem` or :program:`HEMCO` simulation
is taking too long to run, consider using profiling tools to generate
a list of the time that is spent in each routine. This can help you
identify badly written and/or poorly-parallelized code.  For more
information, please see `our Profiling GEOS-Chem wiki
page <https://wiki.geos-chem.org/Profiling_GEOS-Chem>`_.
