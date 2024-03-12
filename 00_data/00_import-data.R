
# packages ----------------------------------------------------------------
packages <- c("haven", "here")
groundhog_day <- "2024-01-11"

# (install and) load package versions available on the specified day to try
# to ensure reproducibility

library(groundhog)

groundhog::meta.groundhog(groundhog_day)

groundhog::groundhog.library(pkg = packages, date = groundhog_day)



data_paths <- list(
  baseline_raw_path = here::here("00_data/FF_wave1_2020v2_SAS.sas7bdat"),
  year_one_raw_path = here::here("00_data/FF_wave2_2020v2_SAS.sas7bdat")
)


# functions ---------------------------------------------------------------

get_data_sas <- function(path){
  dat <- haven::read_sas(path)
  
  return(dat)
}

# getting the data --------------------------------------------------------

# takes a list of here:: paths as input and outputs a list with the
# corresponding data. only takes in sas files

data_raw <- lapply(
  X = data_paths,
  FUN = get_data_sas
)

# saving the data ---------------------------------------------------------

saveRDS(
  object = data_raw,
  file = here::here("01_data-processing", "data_private", "data_raw.rds")
)


