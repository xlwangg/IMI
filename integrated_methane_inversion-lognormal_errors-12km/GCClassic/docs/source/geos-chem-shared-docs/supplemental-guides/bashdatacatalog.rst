.. _bashdatacatalog:

##########################################
Manage a data archive with bashdatacatalog
##########################################

If you need to download a large amount of input data for
:program:`GEOS-Chem` or :program:`HEMCO` (e.g. in support of a large
user group at your institution) you may find
:program:`bashdatacatalog` helpful.

.. _bashdatacatalog-what-is:

========================
What is bashdatacatalog?
========================

The :program:`bashdatacatalog` is a command-line tool (written by
`Liam Bindle <https://github.com/LiamBindle>`_) that facilitates
synchronizing local data collections with a remote data
source. With the :program:`bashdatacatalog`, you can run queries on
your local data collections to answer questions like "What files am I
missing?" or "What files aren't bitwise identical to remote
data?". Queries can include a date range, in which case collections
with temporal assets are filtered-out accordingly. The
:program:`bashdatacatalog` can format the results of queries as: a URL
download list, a Globus transfer list, an rsync transfer list, or
simply a file list.

The :program:`bashdatacatalog` was written to facilitate downloading
input data for users of the `GEOS-Chem atmospheric chemistry model
<http://geos-chem.org>`_. The canonical GEOS-Chem input data
repository has >1 M files and >100 TB of data, and the input data
required for a simulation depends on the model version and simulation
parameters such as start and end date.

.. _bashdatacatalog-usage:

==================
Usage instructions
==================

For detailed instructions on using :program:`bashdatacatalog`, please
see the `bashdatacatalog wiki on Github
<https://github.com/geoschem/bashdatacatalog/wiki/Instructions-for-GEOS-Chem-Users>`_.

Also see our `input-data-catalogs Github repository
<https://github.com/geoschem/input-data-catalogs>`_ for
comma-separated input lists of GEOS-Chem data, separated by model version.
