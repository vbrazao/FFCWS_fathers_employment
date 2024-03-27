# FFCWS: Using the Future of Families Data to Explore the Role of Race and Employment on IPV Perpetration

This repository contains files necessary for inspecting and reproducing analyses conducted by Priya Devendran (Deakin University) and Vasco Brazão (no affiliation) using the Fragile Families and Child Wellbeing dataset (public use data, waves 1 and 2).

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
    -   Folder `outputs` stores the outputs mentioned above locally and includes a `.gitkeep` file so that the "empty" folder can be part of the GitHub repository. Our outputs from the multiverse analysis are stored separately on the Open Science Framework because it allows the storage of larger files than this GitHub repository ([link to the OSF storage](https://osf.io/w6hu5/)).
-   Folder `03_generated-reports` contains Quarto files and their outputs, as well as the .csl and .bib files required for the multiverse report.

The following is a set of instructions for those who would like to reproduce our analysis.

-   Make sure you have a recent version of RStudio installed (which ships with Quarto already), running R version 4.3.3. (see [here for how to have multiple R versions on your machine and why you might want that](https://groundhogr.com/many)).
-   Make sure you have the `groundhog` package installed (it is used to manage the package versions called by the scripts, to make reproducibility more likely — [read more about the `groundhog` package here](https://datacolada.org/100)). **IMPORTANT: Because we are using the `groundhog` package, you might have to run each script twice - first `groundhog` does some magic, then it asks you in the console to restart R, and then the script should run fully and as normal.**
-   Make sure you have the `rmarkdown` package installed.
-   Get a local copy of this repository (e.g., by forking, cloning, or simply downloading the contents).
-   Get a copy of the FFCWS public-use SAS-format Wave 1 and Wave 2 data, and save the files in the folder `00_data`. Make sure the names of the files correspond to the paths used by the `00_import-data.R` script, namely:
    -   Wave 1: `00_data/FF_wave1_2020v2_SAS.sas7bdat`
    -   Wave 2: `00_data/FF_wave2_2020v2_SAS.sas7bdat`
-   Open the R Project (double-clicking the `FFCWS_fathers_employment.Rproj` file).
-   Run the script `00_data/00_import-data.R`. **IMPORTANT: Because we are using the `groundhog` package, you might have to run each script twice - first `groundhog` does some magic, then it asks you in the console to restart R, and then the script should run fully and as normal.**
-   Run the remaining scripts in order. Beware that the script to generate the multiverse will take a while and use all available cores. To be clear, this is the intended order:
    1.  In folder `01_data-processing/r`, first run `01_delineate-sample.R`, then run `02_wrangle-data.R`.
    2.  In folder `02_analysis-codes/r` run `01_generate-multiverse.R`. Raise the volume on your laptop and wait for the "beep" that will signal the end of the computations.
-   In folder `03_generated-reports` open each `.qmd` file in ascending numerical order and render the files to generate the `.docx` documents. You should be able to change the output format to `html` if you prefer.
