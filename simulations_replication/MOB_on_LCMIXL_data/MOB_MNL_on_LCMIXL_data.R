library(MobMixlogit)
library(partykit)
library(mlogit)
library(strucchange)
library(sandwich)
library(purrr)
library(tidyr)
library(plyr)
library(xtable)

# Functions
source(here::here('utils/utils_using_MobMixlogit.R'))

## Create data from latent class using 3 classes.
gamma_c1  <- c(0,0,0,0,0)
gamma_c2  <- c(2, 2, 1, 0, 0)
gamma_c3  <- c(-2, -2, -1, 0, 0)
ctes      <- c(0, 1, -1)
seed      <- 123456L
N_ind     <- 1000
t_ind     <- 10
delta     <- 1
perc_data <- 0.1
N_trials  <- 100  

data_wide <- MobMixlogit::dgp_LC_3(N = N_ind,
                                   t = t_ind,
                                   gamma_c1 = gamma_c1,
                                   gamma_c2 = gamma_c2,
                                   gamma_c3 = gamma_c3,
                                   ctes = ctes, 
                                   seed = seed) %>%
  as.data.frame() %>% 
  MobMixlogit::long_to_wide()



partition_var_total_trial <- lapply(1:N_trials, 
                                    function(trial_i){
  
  set.seed(trial_i)
  #Block Bootstraping the data
  data_wide_trial_i <- data_wide %>%
    split(.$id_ind) %>%
    .[sample(names(.),replace = TRUE)] %>%
    bind_rows %>% 
    arrange(id_ind) %>% 
    mutate(id_ind = data_wide$id_ind,
           id = data_wide$id)
  
  start_time <- Sys.time()
  fitted_tree <- partykit::mob(
    formula = as.formula(choice_wide ~  x1_1 + x1_2 + x1_3 + x2_1 + x2_2 + x2_3 + id + id_ind + 0  | 
                           z1 +z2 + z3 + z4 + z5)  , 
    data    = data_wide_trial_i, 
    fit     = fit_mob_mnl_LC_MIXL_data,
    cluster = id_ind,
    control = mob_control(ytype = "data.frame",
                          xtype = "data.frame",
                          minsize = (N_ind*t_ind) * perc_data,
                          alpha = 0.05,
                          ordinal = "l2", 
                          nrep = 100000)
  )
  
  end_time <- Sys.time()
  time_iter <- round(end_time - start_time, digits = 4)
  
  # Partition variables from original tree
  partition_vars <- get_partitions_vars(fitted_tree = fitted_tree)
  # Partition variables after prunning
  partition_vars_pruned <- prune.modelparty(fitted_tree,
                                            type = "BIC") %>% get_partitions_vars()
  
  cat("\n","Trial ",trial_i, "time taken: ", time_iter)

  return(list(partition_vars = partition_vars,
         partition_vars_pruned = partition_vars_pruned))
  })
 

all_partition_vars <- vector("list", N_trials)
all_partition_vars_pruned <- vector("list", N_trials)
all_main_node <- vector("list", N_trials)

counter <- 1
for (x in partition_var_total_trial) {
  
  all_main_node[[counter]]             <- x$partition_vars[[1]]
  all_partition_vars[[counter]]        <- table(unlist(x$partition_vars))
  all_partition_vars_pruned[[counter]] <- table(unlist(x$partition_vars_pruned))
 
  counter <-  counter + 1
}



df_first_partition_var <- as.data.frame(rbind(table(unlist(all_main_node))))

df_partition_vars <- plyr::ldply(all_partition_vars, rbind) %>% 
  apply(2, sum, na.rm=TRUE)%>% 
  rbind() %>% 
  as.data.frame()

df_partition_vars_pruned <- plyr::ldply(all_partition_vars_pruned, rbind)%>% 
  apply(2, sum, na.rm=TRUE) %>% 
  rbind() %>% 
  as.data.frame()

nm <- list(df_first_partition_var,
           df_partition_vars,
           df_partition_vars_pruned)


final_df <- plyr::ldply(nm, rbind) %>% 
  rbind() %>% 
  as.data.frame() %>% 
  mutate_all(~replace(., is.na(.), 0)) %>% 
  select(sort(names(.))) 

#percentages 
final_df <- final_df/ N_trials


# Rownames LaTeX
rownames(final_df) <- c("Root node split (\\%)",
                        "Average number of splits",
                        "Average number of splits after pruning"
)

# Colnames LaTeX
colnames(final_df) <- c("$Z_{1}$",
                        "$Z_{2}$",
                        "$Z_{3}$",
                        "$Z_{4}$",
                        "$Z_{5}$")

# Create LaTeX table
tab_to_latex<- print.xtable(xtable(final_df),
                            include.rownames=T,
                            sanitize.text.function = function(x){x},
                            floating = F)


fileConn <-file("simulations_replication/MOB_on_LCMIXL_data/res_MOB_on_LCMIXL_data.tex")
writeLines(tab_to_latex, fileConn)
close(fileConn)





