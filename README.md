# GOAceattle
Final code to run the analysis from Adams et al. 2022 via the Rceattle [package](https://github.com/grantdadams/Rceattle) to fit multiple multi-species age-structured statistical catch at age (MSCAA) models to data for groundfish in the Gulf of Alaska. An model averaging approach is then conducted and subsequent figures and tables are produced.

**File description:**

Code to run analyses can be found in the "R" folder:
1. "Rceattle_install_source.R" installs the Rceattle package from source (found in the "src" folder) used for analyses (Rceattle has since been updated for further simulation testing/analyses)
2. "Diet_proportion_calculation.R" runs the empirical diet proportion estimator
3. "Fit_fixed_effects_models.R" fits the MSCAA via fixed effects
4. "Fit_random_effects_models.R" updates the fixed effects MSCAA to estimate random effects
5. "Figures.R" runs multimodel averaging functions, plots, and tables
6. "Results.Rmd" runs the results section.

All data used for the analysis are included in the "Data" folder. Details on data structures can be found in the meta_data sheet of the excel files, roxygen help files for functions, or the vignette in the package.

Tables and plots can be found in the "Results" folder.


**Citation**

Adams, G., K. Holsman, S. Barbeaux, M. Dorn, J. Ianelli, I. Spies, I. Stewart, A. Punt. 2022. An ensemble approach to understand predation mortality for groundfish in the Gulf of Alaska. Fisheries Research.

**Abstract**

There is increasing consensus of the need for ecosystem-based fisheries management (EBFM), which accounts for trophic interactions and environmental conditions when managing exploited marine resources. Continued development and testing of analytical tools that are expected to address EBFM needs are essential for guiding the management of fisheries resources in achieving and balancing multiple social, economic, and conservation objectives. To address these needs, we present and compare alternative climate-informed multi-species statistical catch-at-age models to account for spatio-temporal differences in stock distributions, with application to four groundfish species (walleye pollock Gadus chalcogrammus, Pacific cod Gadus macrocephalus , arrowtooth flounder Atheresthes stomias , and Pacific halibut Hippoglossus stenolepis ) in the Gulf of Alaska, USA. We integrate across multiple forms of uncertainty regarding the data and distribution of Pacific halibut using an ensemble modelling approach. Models developed here can be used to supplement current tactical fisheries management and inform on the trade-offs between harvesting across groundfish in the Gulf of Alaska. This approach may be applicable for other situations where spatial and temporal overlap is extensive among closely coupled species.
