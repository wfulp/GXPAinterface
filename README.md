
<!-- README.md is generated from README.Rmd. Please edit that file -->

# GXPAinterface

<!-- badges: start -->

[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
[![](https://img.shields.io/badge/codecov-96%25-green.svg)](https://covr.r-lib.org/)
<!-- badges: end -->

## Overview

[{GXPAinterface}](https://wfulp.github.io/GXPAinterface/) is an R
package with various function to access GXPA API and collection of
functions to pre-process the data for importing into GXPA.

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

### Quick Start Example

Here is a example script to grab two genes from TCGA/GTEX (Xena Toil).
This dataset is open to all without needing to log in and get a token,
so you don’t need a token for it. Other datasets may require it, see
below for how to setup the token.

    # load this library (after you install it)
    library(GXPAinterface)

    # get the names of all the series in gxpa that you can acces with no token
    all_dat <- get_series_info_from_gxpa()
    all_dat$name[1:10]

    # find the one of interest (in this case, the TCGA/GTX Xena dataset)
    found_rows = all_dat[grep(".*TCGA.*Xena.*", all_dat$name), ]; found_rows
    series_id = found_rows$id; series_id

    # get PDCD1 and LAG3
    my.genes = c("PDCD1", "LAG3")
    df = get_data_from_gxpa(series_id, my.genes)

    # take a peek
    head(df); dim(df)

### Xpress 2.0 to GXPA Template

This template is a parameterized RMarkdown document for pulling data
from Xpress 2.0, for a given Xpress ID, and uploading to GXPA. There are
other parameters to specify QC entries and additional information you
may want to pass to GXPA (i.e.  addition metadata and series
description). The user may chose to load to GXPA through this template
or save files and load to GXPA manually.

Xpress 2.0 to GXPA template can be used directly and edited as needed.
For a new report in RStudio select **File -\> New File -\> R Markdown
-\> From Template -\> Xpress to GXPA Workflow**, or use the
`rmarkdown::draft()` function code.

``` r
rmarkdown::draft("xpress_01234_workflow.Rmd",
  template = "xpress_gxpa_workflow",
  package = "GXPAinterface"
)
```

## Installation

Install the released version of
[{GXPAinterface}](https://wfulp.github.io/GXPAinterface/) from GitHub:

``` r
remotes::install_github(
  repo = "wfulp/GXPAinterface"
)
```

Or install the development version with:

``` r
remotes::install_github(
  repo = "wfulp/GXPAinterface",
  ref = "devlop"
)
```

## Note About Connecting to GXPA API

[{GXPAinterface}](https://wfulp.github.io/GXPAinterface/) can make use
of R environments for accessing the GXPA API. When importing from GXPA
your GXPA API token must be provided, and when importing from GXPA your
GXPA username and password must be provided. While is it possible to
provide these each time the respective function is used, it is more
convenient to set these for the current R session, or even set for all
sessions.

To set the token and login information for the current session:

``` r
Sys.setenv(GXPA_SERVER = "MY_GXPA_SERVER")
Sys.setenv(GXPA_TOKEN = "MY_GXPA_USER_TOKEN")
Sys.setenv(GXPA_USERNAME = "MY_GXPA_USERNAME")
Sys.setenv(GXPA_PASSWORD = "MY_GXPA_PASSWORD")
```

More convenient is to set the following environment variables in a
`.Renviron` file (`usethis::edit_r_environ()`):

    GXPA_SERVER = GXPA_SERVER
    GXPA_TOKEN  = MY_GXPA_USER_TOKEN
    GXPA_USERNAME = MY_GXPA_USERNAME
    GXPA_PASSWORD = MY_GXPA_PASSWORD

These environment variable values will be used automatically if defined
in your R session.

## Code of Conduct

Please note that this project is released with a [Contributor Code of
Conduct](https://contributor-covenant.org/version/2/0/CODE_OF_CONDUCT.html).
By contributing to this project, you agree to abide by its terms.
