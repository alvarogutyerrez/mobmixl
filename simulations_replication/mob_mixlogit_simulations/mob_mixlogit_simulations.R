library(MobMixlogit)

mob_mixlogit_simulations.rds <- MobMixlogit::simwrapper(
  n.indiv   = c(250,500) ,
  n.choices = c(6,12) ,
  draws     = 1000 ,
  xi        = c(0.5,0.8),
  delta     = c(0.5,1),
  nrep      = 50,
  scenario = c("stump","tree"),
  run_in_parallel = TRUE,
  nClusters = 30
  ) 

saveRDS(
  object = mob_mixlogit_simulations.rds,
  file   = "simulations_replication/mob_mixlogit_simulations/mob_mixlogit_simulations.rds" 
) 