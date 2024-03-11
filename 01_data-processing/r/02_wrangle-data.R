

# packages ----------------------------------------------------------------
packages <- c("tidyverse", "here", "missForest")
groundhog_day <- "2024-01-11"

# (install and) load package versions available on the specified day to try
# to ensure reproducibility

library(groundhog)

groundhog::meta.groundhog(groundhog_day)

groundhog::groundhog.library(pkg = packages, date = groundhog_day)

# set seed for reproducible imputation

set.seed(2740301)

# get data from .rds ------------------------------------------------------

dat <- readRDS(
  file = here::here(
    "01_data-processing", 
    "data_private",
    "data_raw.rds"
  )
)

baseline_raw <- dat[[1]]

year_one_raw <- dat[[2]]


# filter based on our delineation -----------------------------------------

included_ids <- readRDS(
  here::here(
    "01_data-processing", 
    "data_public",
    "included_IDs.RDS"
  )
) |> 
  # make it a vector
  dplyr::pull()

baseline_included <- baseline_raw |> 
  dplyr::filter(
    idnum %in% included_ids
  ) |> 
  dplyr::mutate(
    idnum = as.numeric(idnum)
  )

year_one_included <- year_one_raw |> 
  dplyr::filter(
    idnum %in% included_ids
  ) |> 
  dplyr::mutate(
    idnum = as.numeric(idnum)
  )


# CREATE ONE VARIABLE AT A TIME -------------------------------------------
# IPV ---------------------------------------------------------------------

# which relationship to take IPV from
dat_ipv_source <- year_one_included |> 
  dplyr::select(
    idnum,
    # relationship status
    # still with father
    m_romantically_father_now = m2d5,
    
    # with father before
    m_romantically_father_before = m2d7
  ) |> 
  dplyr::mutate(
    source_ipv = dplyr::case_when(
      m_romantically_father_now == 1 ~ "Father now",
      m_romantically_father_before == 1 ~ "Father past",
      .default = NA
    ) |> 
      forcats::as_factor(),
    .keep = "unused"
  )

# ipv variables

dat_ipv <- year_one_included |> 
  dplyr::select(
    idnum,
    
    # IPV - intimate partner violence scales #
    # _r indicates a positive item that will need to be reverse scored so it's 
    # negative
    # behavior of father now
    ipv_01_r = m2d6a,
    ipv_02_r = m2d6b,
    ipv_03 = m2d6c,
    ipv_04_r = m2d6d,
    ipv_05 = m2d6e,
    ipv_06 = m2d6f,
    ipv_07 = m2d6g,
    ipv_08 = m2d6h,
    ipv_09 = m2d6i,
    ipv_10 = m2d6j,
    ipv_11_r = m2d6k,
    ipv_12_r = m2d6l,
    
    # behavior in last month of relationship with father (no longer together)
    ipv_01_past_r = m2d8a,
    ipv_02_past_r = m2d8b,
    ipv_03_past = m2d8c,
    ipv_04_past_r = m2d8d,
    ipv_05_past = m2d8e,
    ipv_06_past = m2d8f,
    ipv_07_past = m2d8g,
    ipv_08_past = m2d8h,
    ipv_09_past = m2d8i,
    ipv_10_past = m2d8j,
    ipv_11_past_r = m2d8k,
    ipv_12_past_r = m2d8l
  ) |> 
  # recode ipv values
  dplyr::mutate(
    # create ipv variables with "_recoded" suffix
    # if they are negative (no "_r" suffix), then 3 becomes 0, 2 becomes 1,
    # and 1 becomes 2,and NA signifiers are NA. 
    dplyr::across(
      .cols = dplyr::starts_with("ipv_") & -dplyr::contains("_r"),
      .fns = ~ dplyr::case_when(
        .x == 3 ~ 0,
        .x == 2 ~ 1,
        .x == 1 ~ 2,
        .default = NA
      ),
      .names = "recoded_{.col}"
    ),
    # if they are positive (with "_r" suffix), then 3 becomes 2, 2 becomes 1,
    # 1 becomes 0, and NA signifiers are NA.
    dplyr::across(
      .cols = dplyr::starts_with("ipv_") & dplyr::contains("_r"),
      .fns = ~ dplyr::case_when(
        .x == 3 ~ 2,
        .x == 2 ~ 1,
        .x == 1 ~ 0,
        .default = NA
      ),
      .names = "recoded_{.col}"
    ),
    # turn all ipv variables into factors
    dplyr::across(
      .cols = dplyr::starts_with("recoded_ipv_"),
      .fns = ~ forcats::as_factor(.x) |> 
        forcats::fct_expand("0", "1", "2") |> 
        forcats::fct_relevel("0", "1", "2")
    ),
    .keep = "unused"
  ) 



var_ipv_and_source <- dat_ipv_source |> 
  dplyr::left_join(dat_ipv, by = "idnum") |> 
  #pivot so that all ipv values are in one column
  tidyr::pivot_longer(
    cols = dplyr::starts_with("recoded_ipv"),
    names_to = "ipv_type",
    values_to = "ipv_value"
  ) |> 
  #keep only the ipv values that match the ipv source
  dplyr::filter(
    (source_ipv == "Father now" & 
       stringr::str_detect(ipv_type, "past", negate = TRUE)) |
      (source_ipv == "Father past" &
         stringr::str_detect(ipv_type, "past"))
  ) |> 
  #clean up the names of ipv types
  dplyr::mutate(
    ipv_type = ipv_type |> 
      stringr::str_remove("recoded_") |> 
      stringr::str_remove("_past") |> 
      stringr::str_remove("_r")
  ) |> 
  # reorder so that when we pivot back ipv items are in the right order
  dplyr::arrange(
    idnum, ipv_type
  ) |> 
  # get all the variables their own column
  tidyr::pivot_wider(
    names_from = ipv_type,
    values_from = ipv_value
  )

# Race --------------------------------------------------------------------

var_race <- baseline_included |> 
  dplyr::select(
    idnum,
    f_race = f1h3
  ) |> 
  dplyr::mutate(
    f_race = dplyr::case_when(
      f_race == 1 ~ "White",
      f_race == 2 ~ "Black"
    ) |> 
      forcats::as_factor() |> 
      forcats::fct_relevel("White", "Black")
  )


# Employment --------------------------------------------------------------

var_employment <- baseline_included |> 
  dplyr::select(
    idnum,
    
    # last week, any regular work for pay
    f_employment = f1j1
  ) |> 
  
  dplyr::mutate(
    f_employment = dplyr::case_when(
      f_employment == 1 ~ "Employed",
      f_employment == 2 ~ "Unemployed",
      .default = NA
    ) |> 
      forcats::as_factor() |> 
      forcats::fct_relevel("Unemployed", "Employed")
  )

# Age ---------------------------------------------------------------------

var_age <- baseline_included |> 
  dplyr::select(
    idnum,
    
    # fathers's age at interview (constructed)
    f_age = cf1age
  ) |> 
  dplyr::mutate(
    f_age = dplyr::case_when(
      f_age > 0 ~ f_age,
      .default = NA
    )
  )

# Education ---------------------------------------------------------------

var_education <- baseline_included |> 
  dplyr::select(
    idnum,
    
    # father's level of education
    f_education = f1i1
  ) |> 
  dplyr::mutate(
    # create education categories, turn into factor, make the reference factor
    # be "Below HS" 
    f_education = dplyr::case_when(
      f_education >= 4 ~ "HS and above",
      f_education >= 1 ~ "Below HS",
      .default = NA
    ) |> 
      forcats::as_factor() |> 
      forcats::fct_relevel("Below HS", "HS and above")
  )

# Alcohol use -------------------------------------------------------------

var_alcohol <- baseline_included |> 
  dplyr::select(
    idnum,
    
    # father's alcohol use
    f_alcohol = f1g2
  ) |> 
  dplyr::mutate(
    # create factor for alcohol use, make the reference factor be "Never"
    f_alcohol = dplyr::case_when(
      f_alcohol == 5 ~ "Never",
      f_alcohol == 4 ~ "<1 / month",
      f_alcohol %in% c(3, 2, 1) ~ ">1 / month",
      .default = NA
    ) |> 
      forcats::as_factor() |> 
      forcats::fct_relevel("Never", "<1 / month", ">1 / month")
  )

# Drug use ----------------------------------------------------------------

var_drugs <- baseline_included |> 
  dplyr::select(
    idnum,
    
    # father's drug use
    f_drugs = f1g3
  ) |> 
  dplyr::mutate(
    # create factor for drug use, make the reference factor be "Never"
    f_drugs = dplyr::case_when(
      f_drugs == 5 ~ "Never",
      f_drugs == 4 ~ "<1 / month",
      f_drugs %in% c(3, 2, 1) ~ ">1 / month",
      .default = NA
    ) |> 
      forcats::as_factor() |> 
      forcats::fct_relevel("Never", "<1 / month", ">1 / month"),
  )

# Depression --------------------------------------------------------------

var_depression <- baseline_included |> 
  dplyr::select(
    idnum,
    
    f_depression1 = f1g9a,
    f_depression2 = f1g9b,
    f_depression3 = f1g9c,
    f_depression4 = f1g9d,
    f_depression5 = f1g9e,
    f_depression6 = f1g9f,
    f_depression7 = f1g9g,
    f_depression8 = f1g9h,
    f_depression9 = f1g9i,
    f_depression10 = f1g9j,
    f_depression11 = f1g9k,
    f_depression12 = f1g9l,
    
  ) |> 
  # recode NAs
  
  dplyr::mutate(
    across(
      .cols = -idnum,
      .fns = ~ dplyr::case_when(
        .x < 0 ~ NA,
        .default = .x
      )
    )
  ) |> 
  
  # take the mean
  
  tidyr::pivot_longer(
    cols = dplyr::starts_with("f_dep"),
    names_to = "item",
    values_to = "response"
  ) |> 
  dplyr::summarise(
    .by = idnum,
    f_depression = mean(response, na.rm = FALSE)
  )


# Incarceration -----------------------------------------------------------

# incarceration based on fathers report, baseline

var_incarceration_f_baseline <- baseline_included |> 
  dplyr::select(
    idnum,
    
    # all the items where the answer can be "due to incarceration"
    # 1 = yes, 2 = no
    f_incarceration1 = f1b4c,
    f_incarceration2 = f1b9a5,
    f_incarceration3 = f1b9b5,
    f_incarceration4 = f1b10a5,
    f_incarceration5 = f1b10b5,
    # incarcerated = 101
    f_incarceration6 = f1j3b,
  ) |> 
  
  dplyr::mutate(
    f_incarceration_f_baseline = dplyr::case_when(
      dplyr::if_all(
        .cols = -idnum,
        .fns = ~ .x < 0
      ) ~ NA,
      f_incarceration1 == 1 |
        f_incarceration2 == 1 |
        f_incarceration3 == 1 |
        f_incarceration4 == 1 |
        f_incarceration5 == 1 |
        f_incarceration6 == 101 ~ "Experienced incarceration",
      .default = "No incarceration"
    ) |> 
      forcats::as_factor() |> 
      forcats::fct_relevel("No incarceration", "Experienced incarceration")
  ) |> 
  
  dplyr::select(
    idnum, f_incarceration_f_baseline
  )

# incarceration based on father's report, year 1

var_incarceration_f_year_one <- year_one_included |> 
  dplyr::select(
    ever_jail = f2h22,
    ever_adult_jail = f2h24,
    idnum
  ) |> 
  dplyr::mutate(
    f_incarceration_f_year_one = dplyr::case_when(
      dplyr::if_all(
        .cols = -idnum,
        .fns = ~ .x < 0
      ) ~ NA,
      ever_jail == 1 | 
        ever_adult_jail == 1 ~ "Experienced incarceration",
      .default = "No incarceration"
    )
  ) |> 
  dplyr::select(
    f_incarceration_f_year_one,
    idnum
  )

# incarceration based on mother's report, baseline

var_incarceration_m_baseline <- baseline_included |> 
  dplyr::select(
    doing_last_week = m1i6,
    lives_most_of_time = m1i11,
    end_bc_incarceration = m1b4c,
    not_plan_live_together_bc_incarceration = m1b9a5, # to check: is this just about the father?
    not_live_together_bc_incarceration = m1b9b5, # same to check as above
    not_plan_marry_bc_incarceration = m1b10a5,
    not_married_bc_incarceration = m1b10b5,
    idnum
  ) |> 
  dplyr::mutate(
    f_incarceration_m_baseline = dplyr::case_when(
      dplyr::if_all(
        .cols = -idnum,
        .fns = ~ .x < 0
      ) ~ NA,
      doing_last_week == 101 |
        lives_most_of_time == 101 |
        end_bc_incarceration == 1 |
        not_plan_live_together_bc_incarceration == 1 |
        not_live_together_bc_incarceration == 1 |
        not_plan_marry_bc_incarceration == 1 |
        not_married_bc_incarceration == 1 ~ "Experienced incarceration",
      .default = "No incarceration"
    )
  ) |> 
  dplyr::select(
    idnum, f_incarceration_m_baseline
  )

# incarceration based on mother's report, year one

var_incarceration_m_year_one <- year_one_included |> 
  dplyr::select(
    marriage_ended_incarceration_father = m2a9d,
    father_ever_incarceration = m2c36,
    idnum
  ) |> 
  dplyr::mutate(
    f_incarceration_m_year_one = dplyr::case_when(
      dplyr::if_all(
        .cols = -idnum,
        .fns = ~ .x < 0
      ) ~ NA,
      marriage_ended_incarceration_father == 1 |
        father_ever_incarceration == 1 ~ "Experienced incarceration",
      .default = "No incarceration"
    )
  ) |> 
  dplyr::select(
    idnum, f_incarceration_m_year_one
  )

# putting them together 

var_incarceration <- var_incarceration_f_baseline |> 
  dplyr::left_join(var_incarceration_f_year_one, by = "idnum") |> 
  dplyr::left_join(var_incarceration_m_baseline, by = "idnum") |> 
  dplyr::left_join(var_incarceration_m_year_one, by = "idnum") |> 
  dplyr::mutate(
    f_incarceration = dplyr::case_when(
      dplyr::if_any(
        .cols = dplyr::contains("incarceration"),
        .fns = ~ .x == "Experienced incarceration"
      ) ~ "Experienced incarceration",
      dplyr::if_all(
        .cols = dplyr::contains("incarceration"),
        .fns = ~ is.na(.x)
      ) ~ NA,
      .default = "No incarceration"
    ) |> 
      forcats::as_factor()
  ) |> 
  dplyr::select(
    idnum, f_incarceration
  )

# Number of children ------------------------------------------------------

var_children <- baseline_included |> 
  dplyr::select(
    idnum,
    
    # number of other biological children
    f_children_binary = f1a6, # 1 = yes, 2 = no
    f_children_number = f1a6a
  ) |> 
  dplyr::mutate(
    # join the two variables for number of children so that fathers without
    # other children get assigned a 0, and values are taken from the 
    # f_children_number variable
    f_children = dplyr::case_when(
      f_children_binary == 2 ~ 0,
      f_children_number < 0 ~ NA,
      .default = f_children_number
    )
  ) |> 
  dplyr::select(
    idnum, f_children
  )


# Poverty -----------------------------------------------------------------

var_poverty <- baseline_included |> 
  dplyr::select(
    idnum,
    
    f_poverty = cf1inpov
  )


# Home ownership ----------------------------------------------------------

var_home <- baseline_included |> 
  dplyr::select(
    idnum,
    
    # home owned or rented
    f_home = f1f2
  ) |> 
  dplyr::mutate(
    f_home = dplyr::case_when(
      f_home == 2 ~ "Rented",
      f_home == 1 ~ "Owned",
      .default = NA
    ) |> 
      forcats::as_factor() |> 
      forcats::fct_relevel("Owned", "Rented")
  )



# Weights -----------------------------------------------------------------

var_weights <- baseline_included |> 
  dplyr::select(
    idnum,
    
    f1natwt,
    dplyr::starts_with("f1natwt_"),
    
    f1citywt,
    dplyr::starts_with("f1citywt_")
  )

# National and city sample flags ------------------------------------------

var_flag <- baseline_included |> 
  dplyr::select(
    idnum,
    
    f_national_sample = cf1natsm,
    f_city_sample = cf1citsm
  )


# Put it all together -----------------------------------------------------

dat_full <- var_race |> 
  left_join(var_ipv_and_source, by = "idnum")  |> 
  left_join(var_age, by = "idnum") |> 
  left_join(var_education, by = "idnum") |> 
  left_join(var_alcohol, by = "idnum") |> 
  left_join(var_drugs, by = "idnum") |> 
  left_join(var_depression, by = "idnum") |> 
  left_join(var_employment, by = "idnum") |> 
  left_join(var_incarceration, by = "idnum") |> 
  left_join(var_children, by = "idnum") |> 
  left_join(var_poverty, by = "idnum") |>  
  left_join(var_home, by = "idnum")


# Imputing missing values and recoding ------------------------------------


dat_full_ids <- dat_full |> dplyr::select(idnum)

dat_full_imputed_obj <- dat_full |>
  dplyr::select(-idnum) |>
  as.data.frame() |>
  missForest::missForest()

dat_full_imputed <- dat_full_imputed_obj$ximp |> 
  dplyr::mutate(
    idnum = dat_full_ids$idnum
  ) |> 
  dplyr::relocate(
    idnum
  )

dat_full_cc <- dat_full|> 
  dplyr::filter(
    complete.cases(dat_full)
  )

recode_outcomes <- function(data){
  df <- data |> 
    dplyr::mutate(
      across(
        .cols = dplyr::starts_with("ipv_"), 
        .fns = ~ as.character(.x) |> as.numeric()
      ),
      ipv_sum = (ipv_01 + ipv_02 + ipv_03 + ipv_04 + ipv_05 + ipv_06 + ipv_07 +
                   ipv_08 + ipv_09 + ipv_10 + ipv_11 + ipv_12),
      ipv_prop = ipv_sum/24,
      ipv_max = 24,
      ipv_physical = ipv_08 + ipv_09 + ipv_10,
      ipv_physical_prop = ipv_physical / 6,
      ipv_physical_max = 6,
      ipv_emotional = ipv_01 + ipv_02 + ipv_03 + ipv_04 + ipv_11 + ipv_12,
      ipv_emotional_prop = ipv_emotional / 12,
      ipv_emotional_max = 12,
      ipv_controlling = ipv_05 + ipv_06 + ipv_07,
      ipv_controlling_prop = ipv_controlling / 6,
      ipv_controlling_max = 6
    ) |> 
    left_join(var_weights, by = "idnum") |> 
    left_join(var_flag, by = "idnum")
  
  return(df)
}

data_sets_recoded <- list(dat_full, dat_full_cc, dat_full_imputed) |> 
  purrr::map(.f = recode_outcomes)

# save the datasets -------------------------------------------------------

saveRDS(data_sets_recoded[[1]], file = here::here("01_data-processing", "data_private", "data_final.RDS"))
saveRDS(data_sets_recoded[[2]], file = here::here("01_data-processing", "data_private", "data_final_complete_cases.RDS"))
saveRDS(data_sets_recoded[[3]], file = here::here("01_data-processing", "data_private", "data_final_imputed_cases.RDS"))

rm(list = ls())