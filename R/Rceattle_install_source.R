# Steps to install Rceattle from the source file
# Grant D. Adams adamsgd@uw.edu

# Step 1 - install compilers
# Windows install rtools: https://cran.r-project.org/bin/windows/Rtools/
# Mac install clang and gfortran https://cran.r-project.org/bin/macosx/tools/
# Macs will also need to update makevars file

# Step 2 - Install TMB
# Instructions can be found here for non-pc: https://github.com/kaskr/adcomp/wiki/Download
install.packages('TMB', type = 'source')
# Try "TMB::runExample(all = TRUE)" to see if TMB works

# Step 3 - Install dependencies
install.packages(c("TMB",
                   "testthat",
                   "reshape2",
                   "oce",
                   "TMBhelper",
                   "plyr",
                   "readxl",
                   "writexl",
                   "tidyr",
                   "gplots",
                   "ggplot2",
                   "devtools"))

devtools::install_github("kaskr/TMB_contrib_R/TMBhelper")


# Step 4 - Install Rceattle from source
install.packages("src/Rceattle_1.0.0.0000.tar.gz", repos = NULL, type="source", dependencies = TRUE)
