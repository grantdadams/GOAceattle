library(Rceattle)
rm(list=ls())


################################################
# Set up model switches
################################################
inits_M1_df <- data.frame(
  # Model number
  Model = 1:16,

  # Multi (1) or single (0) species mode
  MsmMode = c(0, rep(1,7), # Long
              0, rep(1,2), # Medium
              0, rep(1,4)), # Short

  # Estimate M1 switches
  EstM1 = c(0, rep(1, 7), # Long
            0, rep(1, 2), # Medium
            0, rep(1, 4)), # Short

  # Initialize model from single species
  InitModel = c(NA, rep(1,7), # Long
                NA, rep(9,2), # Medium
                NA, rep(12,4)) # Short
)
inits_M1_df$Class = ifelse(inits_M1_df$MsmMode == 0, "Single-species", "Multi-species")

################################################
# Estimate random effects
################################################
for(i in 1:16){

  # Load fixed effects models
  load("Models/GOAceattle_2022-01-05.RData")
  mod_fe = mod_list_all[[i]]
  data_tmp <-  mod_fe$data_list
  inits_tmp <- mod_fe$estimated_params

  # Remove
  rm(mod_fe)
  rm(mod_list_all)

  # Fit model - 3 iterations
  mod_re <- try( fit_mod(
    data_list = data_tmp,
    inits = inits_tmp, # Start from ms mod
    file = NULL, # Don't save
    debug = 0, # Estimate
    random_rec = TRUE, # Random recruitment
    msmMode = data_tmp$msmMode,
    verbose = 1,
    phase = NULL,
    getJointPrecision = TRUE,
    niter = 3),
    silent = FALSE)


  if(!is.null(mod_re)){
    if(class(mod_re) != "try-error"){
      save(mod_re, file = paste0("Models/Random_effects_models/GOAceattle_mod_re_",i,"_",Sys.Date(),".Rdata"))
    }
  }
  gc()
  rm(mod_re)
  rm(inits_tmp)
  rm(data_tmp)
}
