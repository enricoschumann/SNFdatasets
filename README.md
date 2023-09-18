# SNSFdatasets

Functions for downloading data from the [Swiss National
Science Foundation](https://snf.ch) (SNF, FNS, SNSF).
The package is lightweight and without dependencies.
Downloaded data can optionally be cached, to avoid
repeated downloads of the same files.  There are also
utilities for comparing different versions of datasets,
i.e. to find changes.

## Installation

To install the package from a running R session, type:

    install.packages('SNSFdatasets',
                     repos = c('http://enricoschumann.net/R',
                               getOption('repos')))


or clone/build the repository's latest version.
