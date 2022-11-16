library(rstudioapi)
library(here)
library(stringr)
library(MobMixlogit)
## Reading info of the folder and script name.
scriptpath <- get_scriptpath()
scriptname <- get_scriptname(scriptpath)

## Current folder path for selecting the files to run
path_current_folder <- stringr::str_remove(scriptpath, scriptname)

## Reading files in the folder
filenames <- as.list(list.files(path       = path_current_folder,
                                pattern    = "\\.R$",
                                full.names = TRUE ) )

## Delete the current file from execution so it avoids circular invocation.
filenames <- filenames[!grepl(scriptname, filenames) ]
## Delete file that creates the LaTeX tables.
filenames <- filenames[!grepl("LC_MIXL_table_generation.R", filenames) ]

## Number of repetitions
## Push wrapper to jobs using rstudioapi::jobRunScript() function.
lapply(filenames ,
       function(iter_file){
         jobs <- rstudioapi::jobRunScript(path       = iter_file    ,
                                          workingDir = here::here() ,
                                          importEnv  =  FALSE       ,
                                          exportEnv  = FALSE       )
       })
