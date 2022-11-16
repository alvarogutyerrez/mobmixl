library(mlogit)
library(partykit)
library(strucchange)
library(sandwich)
library(stringr)
library(MobMixlogit)
source(here::here('utils/utils_using_MobMixlogit.R'))

#-----------------------------------------#
# Loading the data
database <- (function(...)get(data(...,envir = new.env(),package ="MobMixlogit")))("data_DeLaMaza_2021")
#creating choice id
database$id_choice <- 1:nrow(database)
#-----------------------------------------#
# MOB control parameters
mob_alpha   <- 0.05
mob_draws   <- c(1000,5000,10000)
mob_minsize <- 360
# Random parameters definition for mlogit::mlogit() 
mlogit_rpar<- c(forest            = "t",  # Triangular
                morbidity         = "t",  # Triangular
                cost              = "zbt" # Zero-bounded triangular
)

## Fitting MOB-MIXLOGIT
bag_of_models <- MobMixlogit::mob_mixlogit(
  database    =  database,
  mob_alpha   = mob_alpha,
  mob_draws   = mob_draws,
  mob_minsize = mob_minsize,
  mlogit_rpar = mlogit_rpar )
#------------------------------------------------------#
# Recovering name of the model from the file name
model_name_plus_extension <- MobMixlogit::get_scriptname(MobMixlogit::get_scriptpath())
# Only model name (dropping ".R")
model_name <- stringr::str_sub(string = model_name_plus_extension,end    = -3)
# write on disk
saveRDS(
  object = bag_of_models,
  file   = paste0("empirical_application/MobMixlogit_models/",model_name,".rds")
)


