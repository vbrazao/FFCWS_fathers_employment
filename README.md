# FFCWS: Using the Future of Families Data to Explore the Role of Race and Employment on IPV Perpetration

This repository contains files necessary for inspecting and reproducing analyses conducted by Priya Devendran (Melbourne University) and Vasco Brazão (no affiliation) using the Fragile Families and Child Wellbeing dataset (public use data, waves 1 and 2).

(Work in progress)

The following is an overview of the repository (the hierarchy of the list mirrors the hierarchy within the directory):

-   Folder `00_data` is meant to facilitate reproducibility. It contains the file `00_import-data.R`. When the correct raw data is saved in this folder, the script can be run to then populate the folder `01_data-processing/data_private` with the raw data file that is then used by the scripts in `01_data-processing/r`.
-   Folder `01_data-processing` generally contains data files and related code.
    -   `CODEBOOK.Rmd` is an RMarkdown document describing all the variables used in the analysis and how they were constructed from the raw dataset, in the same order as the code found under `r/02_wrangle-data.R`. When knitted, it produces `CODEBOOK.docx`, which is also available for download.
    -   The folder `data_private` locally contains raw data and wrangled data produced by the code in the `r` folder, but is not uploaded to GitHub for privacy reasons. It contains a `.gitkeep` file to make it possible to upload the folder without any of its local contents. This way, if someone downloads the repository, manually adds a raw data file, and runs the code in order, this folder should be correctly populated with the wrangled data, etc.
    -   The folder `data_public` contains data outputs that are anonymized enough that they can be shared publicly.
        -   `delineation.RDS` is a dataframe of all rows in the FFCWS data, with only the ID columns as well as those variables used for data delineation. Further, it includes logical variables `fit_criterion_x` (x being an integer from 1 to 6) that encode whether each mother-father pair cumulatively fulfills our inclusion criteria. Only pairs for which `fit_criterion_6` is `TRUE` were used in the analysis.
        -   `included_IDs.RDS` is a dataframe with just the column `ID` populated only with the ID values that fit our inclusion criteria.
    -   The folder `r` contains R scripts that delineate the sample and wrangle the data.
        -   `01_delineate-sample.R` takes the raw data and produces the two files that populate the `data-public` folder (`delineation.RDS` and `included_IDs.RDS`)
        -   `02_wrangle-data.R` takes the raw data, filters it to include only the relevant IDs, creates each variable from the raw data, performs data imputation where relevant, and produces the three final datasets that are stored in `data-private`: `data_final.RDS`, `data_final_complete_cases.RDS`, `data_final_imputed_cases.RDS`.
-   Folder `02_analysis-codes` contains code and a subfolder for the resulting multiverse analyses.
    -   Folder `r` contains the script.
        -   `01_generate-multiverse.R` takes quite some time to run and, if you let it, will use all the cores on your machine. If it runs successfully, it will save all results from the multiverse analysis in `02_analysis-codes/outputs/multiverse_results.RDS` and also the different parameters separately in `../multiverse_parameters.RDS`.
    -   Folder `outputs` stores the outputs mentioned above locally and includes a `.gitkeep` file so that the "empty" folder can be part of the GitHub repository.
-   Folder `03_generated-reports` contains Quarto files and their outputs, as well as the .csl and .bib files required for the multiverse report. 

The following is a set of instructions for those who would like to reproduce our analysis.

-   step 1 etc.
