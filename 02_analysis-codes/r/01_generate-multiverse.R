
# code that generates the multiverse analysis and saves the results

# packages ----------------------------------------------------------------
packages <- c("tidyverse", "broom", "multiverse", "marginaleffects", "srvyr", "survey", "future", "beepr")
groundhog_day <- "2024-04-22"

# (install and) load package versions available on the specified day to try
# to ensure reproducibility

library(groundhog)

groundhog::meta.groundhog(groundhog_day)

groundhog::groundhog.library(pkg = packages, date = groundhog_day)

# allow parallelization (depending on available cores in machine)
future::plan(multisession)

# create multiverse object
M <- multiverse::multiverse()

# generate multiverse
# ignore warnings about non-integer #successes in binomial glm

multiverse::inside(
  M,
  {
    
    
    dat_cc <- readRDS(here::here("01_data-processing", "data_private", "data_final_complete_cases.RDS"))
    dat_imputed <- readRDS(here::here("01_data-processing", "data_private", "data_final_imputed_cases.RDS"))

    
    dat <- branch(missing,
                  "imputed" ~ dat_imputed,
                  "complete_cases" ~ dat_cc
    ) |> dplyr::filter(f_national_sample == 1) |> 
      dplyr::mutate(
        f_employment_binary = dplyr::case_when(
          f_employment == "Employed" ~ 1,
          f_employment == "Unemployed" ~ 0
        )
      )
    
    dat <- 
      branch(
        proportion,
        "continuous" ~ dat,
        "binomial" ~ dat,
        "binary" ~ dat |> dplyr::mutate(
          ipv_prop = ifelse(ipv_prop == 0, 0, 1),
          ipv_physical_prop = ifelse(ipv_physical_prop == 0, 0, 1),
          ipv_emotional_prop = ifelse(ipv_emotional_prop == 0, 0, 1),
          ipv_controlling_prop = ifelse(ipv_controlling_prop == 0, 0, 1)
        )
      )
    
    dat_design <- srvyr::as_survey_rep(
      .data = dat,
        repweights = dplyr::contains("f1natwt_rep"),
        weights = f1natwt,
        combined_weights = TRUE,
        # why: https://stats.stackexchange.com/questions/409463/duplicating-stata-survey-design-using-svrepdesign-from-survey-package-in-r
        type = "JKn",
        scales = 1,
        rscales = 1,
        mse = TRUE
      )
    
    model.emp.race <- branch(
      weights,
      "weighted" ~ survey::svyglm(
        formula = f_employment_binary ~ branch(
          covariates,
          "adjusted" ~ f_race +
            f_age + f_education + f_alcohol + f_drugs +
            f_children + f_poverty + f_incarceration +
            f_home + f_depression,
          "unadjusted" ~ f_race
        ),
        design = dat_design,
        family = branch(
          proportion,
          "continuous" ~ "gaussian",
          "binary" ~ "binomial",
          "binomial" ~ "binomial"
        )
      ),
      "unweighted" ~ glm(
        formula = f_employment_binary ~ branch(
          covariates,
          "adjusted" ~ f_race +
            f_age + f_education + f_alcohol + f_drugs +
            f_children + f_poverty + f_incarceration +
            f_home + f_depression,
          "unadjusted" ~ f_race
        ),
        data = dat,
        family = branch(
          proportion,
          "continuous" ~ "gaussian",
          "binary" ~ "binomial",
          "binomial" ~ "binomial"
        )
      )
    )
    
    comp_emp_race <- marginaleffects::avg_comparisons(
      model.emp.race, 
      variables = "f_race",
      vcov = branch(
        proportion,
        "continuous" ~ branch(
          weights,
          "weighted" ~ NULL,
          "unweighted" ~ "HC"
        ),
        "binary" ~ NULL,
        "binomial" ~ NULL
      )
    ) |> broom::tidy() |> 
      dplyr::mutate(comp = "emp_race")
    
    model.ipv.race <- branch(
      weights,
      "weighted" ~ survey::svyglm(
        formula = branch(
          outcome,
          "total" ~ ipv_prop,
          "physical" ~ ipv_physical_prop,
          "emotional" ~ ipv_emotional_prop,
          "controlling" ~ ipv_controlling_prop
        ) ~ branch(
          covariates,
          "adjusted" ~ f_race +
            f_age + f_education + f_alcohol + f_drugs +
            f_children + f_poverty + f_incarceration +
            f_home + f_depression,
          "unadjusted" ~ f_race
        ),
        weights = branch(
          proportion,
          "continuous" ~ NULL,
          "binary" ~ NULL,
          "binomial" ~ branch(
            outcome,
            "total" ~ ipv_max,
            "physical" ~ ipv_physical_max,
            "emotional" ~ ipv_emotional_max,
            "controlling" ~ ipv_controlling_max
          )
        ),
        design = dat_design,
        family = branch(
          proportion,
          "continuous" ~ "gaussian",
          "binary" ~ "quasibinomial",
          "binomial" ~ "binomial"
        )
      ),
      "unweighted" ~ glm(
        formula = branch(
          outcome,
          "total" ~ ipv_prop,
          "physical" ~ ipv_physical_prop,
          "emotional" ~ ipv_emotional_prop,
          "controlling" ~ ipv_controlling_prop
        ) ~ branch(
          covariates,
          "adjusted" ~ f_race * f_employment +
            f_age + f_education + f_alcohol + f_drugs +
            f_children + f_poverty + f_incarceration +
            f_home + f_depression,
          "unadjusted" ~ f_race * f_employment
        ),
        weights = branch(
          proportion,
          "continuous" ~ NULL,
          "binary" ~ NULL,
          "binomial" ~ branch(
            outcome,
            "total" ~ ipv_max,
            "physical" ~ ipv_physical_max,
            "emotional" ~ ipv_emotional_max,
            "controlling" ~ ipv_controlling_max
          )
        ),
        data = dat,
        family = branch(
          proportion,
          "continuous" ~ "gaussian",
          "binary" ~ "quasibinomial",
          "binomial" ~ "binomial"
        )
      )
    )
    
    comp_ipv_race <- marginaleffects::avg_comparisons(
      model.ipv.race, 
      variables = "f_race",
      vcov = branch(
        proportion,
        "continuous" ~ branch(
          weights,
          "weighted" ~ NULL,
          "unweighted" ~ "HC"
        ),
        "binary" ~ NULL,
        "binomial" ~ NULL
      )
    ) |> broom::tidy() |> 
      dplyr::mutate(comp = "ipv_race")
    
    model.ipv.race.emp <- branch(
      weights,
      "weighted" ~ survey::svyglm(
        formula = branch(
          outcome,
          "total" ~ ipv_prop,
          "physical" ~ ipv_physical_prop,
          "emotional" ~ ipv_emotional_prop,
          "controlling" ~ ipv_controlling_prop
        ) ~ branch(
          covariates,
          "adjusted" ~ f_race + f_employment +
            f_age + f_education + f_alcohol + f_drugs +
            f_children + f_poverty + f_incarceration +
            f_home + f_depression,
          "unadjusted" ~ f_race * f_employment
        ),
        weights = branch(
          proportion,
          "continuous" ~ NULL,
          "binary" ~ NULL,
          "binomial" ~ branch(
            outcome,
            "total" ~ ipv_max,
            "physical" ~ ipv_physical_max,
            "emotional" ~ ipv_emotional_max,
            "controlling" ~ ipv_controlling_max
          )
        ),
        design = dat_design,
        family = branch(
          proportion,
          "continuous" ~ "gaussian",
          "binary" ~ "quasibinomial",
          "binomial" ~ "binomial"
        )
      ),
      "unweighted" ~ glm(
        formula = branch(
          outcome,
          "total" ~ ipv_prop,
          "physical" ~ ipv_physical_prop,
          "emotional" ~ ipv_emotional_prop,
          "controlling" ~ ipv_controlling_prop
        ) ~ branch(
          covariates,
          "adjusted" ~ f_race + f_employment +
            f_age + f_education + f_alcohol + f_drugs +
            f_children + f_poverty + f_incarceration +
            f_home + f_depression,
          "unadjusted" ~ f_race * f_employment
        ),
        weights = branch(
          proportion,
          "continuous" ~ NULL,
          "binary" ~ NULL,
          "binomial" ~ branch(
            outcome,
            "total" ~ ipv_max,
            "physical" ~ ipv_physical_max,
            "emotional" ~ ipv_emotional_max,
            "controlling" ~ ipv_controlling_max
          )
        ),
        data = dat,
        family = branch(
          proportion,
          "continuous" ~ "gaussian",
          "binary" ~ "quasibinomial",
          "binomial" ~ "binomial"
        )
      )
    )
    
    comp_ipv_emp <- marginaleffects::avg_comparisons(
      model.ipv.race.emp, 
      variables = "f_employment",
      vcov = branch(
        proportion,
        "continuous" ~ branch(
          weights,
          "weighted" ~ NULL,
          "unweighted" ~ "HC"
        ),
        "binary" ~ NULL,
        "binomial" ~ NULL
      )
    ) |> broom::tidy() |> 
      dplyr::mutate(comp = "ipv_emp")
    
    model.ipv.int <- branch(
      weights,
      "weighted" ~ survey::svyglm(
        formula = branch(
          outcome,
          "total" ~ ipv_prop,
          "physical" ~ ipv_physical_prop,
          "emotional" ~ ipv_emotional_prop,
          "controlling" ~ ipv_controlling_prop
        ) ~ branch(
          covariates,
          "adjusted" ~ f_race * f_employment +
            f_age + f_education + f_alcohol + f_drugs +
            f_children + f_poverty + f_incarceration +
            f_home + f_depression,
          "unadjusted" ~ f_race * f_employment
        ),
        weights = branch(
          proportion,
          "continuous" ~ NULL,
          "binary" ~ NULL,
          "binomial" ~ branch(
            outcome,
            "total" ~ ipv_max,
            "physical" ~ ipv_physical_max,
            "emotional" ~ ipv_emotional_max,
            "controlling" ~ ipv_controlling_max
          )
        ),
        design = dat_design,
        family = branch(
          proportion,
          "continuous" ~ "gaussian",
          "binary" ~ "quasibinomial",
          "binomial" ~ "binomial"
        )
      ),
      "unweighted" ~ glm(
        formula = branch(
          outcome,
          "total" ~ ipv_prop,
          "physical" ~ ipv_physical_prop,
          "emotional" ~ ipv_emotional_prop,
          "controlling" ~ ipv_controlling_prop
        ) ~ branch(
          covariates,
          "adjusted" ~ f_race * f_employment +
            f_age + f_education + f_alcohol + f_drugs +
            f_children + f_poverty + f_incarceration +
            f_home + f_depression,
          "unadjusted" ~ f_race * f_employment
        ),
        weights = branch(
          proportion,
          "continuous" ~ NULL,
          "binary" ~ NULL,
          "binomial" ~ branch(
            outcome,
            "total" ~ ipv_max,
            "physical" ~ ipv_physical_max,
            "emotional" ~ ipv_emotional_max,
            "controlling" ~ ipv_controlling_max
          )
        ),
        data = dat,
        family = branch(
          proportion,
          "continuous" ~ "gaussian",
          "binary" ~ "quasibinomial",
          "binomial" ~ "binomial"
        )
      )
    )
    
    comp_ipv_emp_by_race <- marginaleffects::avg_comparisons(
      model.ipv.int, 
      variables = "f_employment", 
      by = "f_race", vcov = branch(
        proportion,
        "continuous" ~ branch(
          weights,
          "weighted" ~ NULL,
          "unweighted" ~ "HC"
        ),
        "binary" ~ NULL,
        "binomial" ~ NULL
      )
    ) |> broom::tidy() |> 
      dplyr::mutate(comp = "ipv_emp_by_race")
    
    # (WE - WU) - (BE - BU)
    comp_ipv_interaction <- marginaleffects::avg_comparisons(
      model.ipv.int,
      variables = "f_employment", by = "f_race",
      hypothesis = "pairwise", vcov = branch(
        proportion,
        "continuous" ~ branch(
          weights,
          "weighted" ~ NULL,
          "unweighted" ~ "HC"
        ),
        "binary" ~ NULL,
        "binomial" ~ NULL
      )
    ) |> broom::tidy() |> 
      dplyr::mutate(comp = "ipv_interaction")
    
    comps <- dplyr::bind_rows(comp_emp_race, comp_ipv_race, comp_ipv_emp, comp_ipv_emp_by_race, comp_ipv_interaction)
  }
)

# run ALL the analyses
# ignore warnings about weights being taken as sampling weights
# (that is what we wanted and expected)
multiverse::execute_multiverse(M, parallel = TRUE)

# store all the analyses
multi_results <- multiverse::expand(M)

# retrieve just the comps results
comps_results <- multi_results|>
  dplyr::mutate(summary_comps = purrr::map(.results, "comps")) |> 
  tidyr::unnest(cols = summary_comps) |> 
  
  # rename to make it easier to work with
  dplyr::mutate(
    term = dplyr::case_when(
      !is.na(f_race) ~ f_race,
      is.na(f_race) ~ term
    ),
    term = dplyr::case_when(
      term == "Black" ~ "B",
      term == "White" ~ "W",
      term == "White - Black" ~ "W - B",
      term == "f_race" ~ "B - W",
      term == "f_employment" ~ "E - U"
    ) |> forcats::as_factor() |> 
      forcats::fct_relevel("B - W", "E - U", "B")
  ) |> 
  
  # remove columns that take too much space and are saved with the full results
  dplyr::select(
    -c(.parameter_assignment, .code, .results, .errors)
  )

# store the parameters
multi_parameters <- multiverse::parameters(M)

# save all the results
# takes a little bit
saveRDS(
  object = multi_results,
  file = here::here("02_analysis-codes", "outputs", "multiverse_results.RDS")
)

# save the parameters
saveRDS(
  object = multi_parameters,
  file = here::here("02_analysis-codes", "outputs", "multiverse_parameters.RDS")
)

# save just the comps results
saveRDS(
  object = comps_results,
  file = here::here("02_analysis-codes", "outputs", "multiverse_comps_results.RDS")
)

# stop parallelization
future::plan(sequential)

# "notify" me when it's finished computing
beepr::beep()

rm(list = ls())
