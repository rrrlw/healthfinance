This is a resubmission.

First comment: add references to DESCRIPTION.
Response: the functionality of the package is limited to generally accepted managerial accounting arithmetic, which would not require a citation. However, we have added a reference to a standard managerial accounting textbook at the end of the package description so that users have a resource if they are looking for in-depth explanations.

Second comment: against CRAN policy to modify global environment
Response: all uses of `<<-` have been changed to `<-` from inst/server.R (only R code that modified the global environment).

Thank you for the helpful comments.

## Test environments

* local R installation, R 4.0.2
* Ubuntu 16.04 (on Travis CI), R 4.0.0
* Windows Server 2012 R2 (on AppVeyor CI), R 4.0.2
* win-builder, r-devel

## R CMD check results

There were no ERRORs or WARNINGs. There was 1 NOTE (below).

* This is a new submission to CRAN.
