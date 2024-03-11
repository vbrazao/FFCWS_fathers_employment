

# packages ----------------------------------------------------------------
packages <- c("tidyverse", "here")
groundhog_day <- "2024-01-11"

# (install and) load package versions available on the specified day to try
# to ensure reproducibility

library(groundhog)

groundhog::meta.groundhog(groundhog_day)

groundhog::groundhog.library(pkg = packages, date = groundhog_day)

# get data from .rds ------------------------------------------------------

dat <- readRDS(file = here::here(
  "01_data-processing", 
  "data_private", 
  "data_raw.rds"
  )
)

baseline_raw <- dat[[1]]

year_one_raw <- dat[[2]]


baseline_renamed <- baseline_raw |> 
  dplyr::select(
    ID = idnum,
    
    # national sample flag
    f_national_sample = cf1natsm,
    
    # was father interviewed in baseline survey
    f_interviewed_zero = cf1fint,
    
    # two cities flag
    f_two_cities = cf1twoc,
    
    # father's race 1 is white, 2 is black 
    f_race = f1h3
  )


year_one_renamed <- year_one_raw |> 
  dplyr::select(
    ID = idnum,
    
    # was mother interviewed at year-one (according to mother's records)
    m_interviewed_one = cm2mint,
    
    # relationship status
    # still with father
    m_romantically_father_now = m2d5,
    
    # with father before
    m_romantically_father_before = m2d7
  )

dat_zero_one <- baseline_renamed |> 
  dplyr::left_join(year_one_renamed, by = "ID") 

dat_zero_one_include <- dat_zero_one |> 
  dplyr::mutate(
    f_two_cities = case_when(
      f_two_cities == 0 ~ 1, 
      .default = 0
    ),
    m_in_relationship = case_when(
      m_romantically_father_now == 1 | m_romantically_father_before == 1 ~ 1,
      .default = 0
    ),
    f_race = case_when(
      f_race %in% c(1,2) ~ 1,
      .default = 0
    )
  ) |> 
  dplyr::mutate(
    # because we will set .keep = "used", which only keeps the columns generated
    # within the mutate() call and those used to generate them, but want to keep 
    # ID also even though we don't modify it
    ID = ID,
    
    fit_criterion_1 = f_national_sample == 1,
    fit_criterion_2 = fit_criterion_1 & f_interviewed_zero == 1,
    fit_criterion_3 = fit_criterion_2 & f_two_cities == 1,
    fit_criterion_4 = fit_criterion_3 & f_race == 1,
    fit_criterion_5 = fit_criterion_4 & m_in_relationship == 1,
    fit_criterion_6 = fit_criterion_5 & m_interviewed_one == 1,
    
    .keep = "used"
  )


included_ids <- dat_zero_one_include |> 
  dplyr::filter(fit_criterion_6 == 1) |> 
  dplyr::select(ID)

# save data with sample delineation

saveRDS(
  dat_zero_one_include, 
  file = here::here(
    "01_data-processing", "data_public",
    "delineation.RDS"
  )
)

# save included IDs

saveRDS(
  included_ids, 
  file = here::here(
    "01_data-processing", "data_public",
    "included_IDs.RDS"
  )
)

rm(list = ls())