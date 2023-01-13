
<!-- README.md is generated from README.Rmd. Please edit that file -->

# GXPAinterface

<!-- badges: start -->

[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
[![](https://img.shields.io/badge/codecov-96%25-green.svg)](https://covr.r-lib.org/)
<!-- badges: end -->

## Overview

[{GXPAinterface}](https://biogit.pri.bms.com/pages/IOCT-discovery-preclin/GXPAinterface/)
is an R package with various function to access GXPA API and collection
of functions to pre-process the data for importing into GXPA.

- Importing from GXPA:
  - `get_GXPA_session_list()` to see all the sessions already on GXPA
  - `get_GXPA_session_details()` to see details on a specific sessions
  - `get_data_from_gxpa()` to import data from GXPA
  - `get_series_info_from_gxpa()` to import series information from GXPA
- Exporting to GXPA:
  - `begin_new_session()` to create a session (staging area) on GXPA
  - `send_file_to_session()` to export a file to GXPA
  - `login_and_get_user_cookie()` to obtain user cookie needed to export
    to GXPA
  - `dry_run_session()` after you load files, you can do a dry run to
    make sure files are formatted properly (quick or full dry run
    options).
  - `load_session()` after your load files, you can run this to trigger
    the task to load data from the session to the SQL database.
  - `remove_session()` if you need to remove a series or session,
    respectively.
- File Preparation
  - `make_series_info_file()` to create a series information file
  - `check_files_for_GXPA()` to run many checks on expression data and
    metadata

## Installation

Install the released version of
[{GXPAinterface}](https://biogit.pri.bms.com/pages/IOCT-discovery-preclin/GXPAinterface/)
from BMS RStudio Package Manager (BRAN):

``` r
my_repos <- c(
  "BMS CRAN mirror" = "http://pm.rdcloud.bms.com:4242/prod-cran/latest",
  "BMS RSPM" = "http://pm.rdcloud.bms.com:4242/bms-cg-biogit-bran/latest"
)

install.packages("GXPAinterface", repos = my_repos)
```

Or install the development version from BMS BioGit with:

``` r
remotes::install_github(
  repo = "IOCT-discovery-preclin/GXPAinterface", 
  host = "https://biogit.pri.bms.com/api/v3"
)
```

or:

``` r
remotes::install_git(
  "https://biogit.pri.bms.com/IOCT-discovery-preclin/GXPAinterface.git"
)
```

## Note About Connecting to GXPA API

[{GXPAinterface}](https://biogit.pri.bms.com/pages/IOCT-discovery-preclin/GXPAinterface/)
can make use of R environments for accessing the GXPA API. When
importing from GXPA your GXPA API token must be provided, and when
importing from GXPA your GXPA username and password must be provided.
While is it possible to provide these each time the respective function
is used, it is more convenient to set these for the current R session,
or even set for all sessions.

To set the token and login information for the current session:

``` r
Sys.setenv(GXPA_TOKEN = "MY_GXPA_USER_TOKEN")
Sys.setenv(GXPA_USERNAME = "MY_GXPA_USERNAME")
Sys.setenv(GXPA_PASSWORD = "MY_GXPA_PASSWORD")
```

More convenient is to set the following environment variables in a
`.Renviron` file (`usethis::edit_r_environ()`):

    GXPA_TOKEN  = MY_GXPA_USER_TOKEN
    GXPA_USERNAME = MY_GXPA_USERNAME
    GXPA_PASSWORD = MY_GXPA_PASSWORD

These environment variable values will be used automatically if defined
in your R session.

## Code of Conduct

Please note that this project is released with a [Contributor Code of
Conduct](https://contributor-covenant.org/version/2/0/CODE_OF_CONDUCT.html).
By contributing to this project, you agree to abide by its terms.
