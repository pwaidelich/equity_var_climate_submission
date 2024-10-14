# The risks of climate tipping points for financial investors

This repository for Waidelich, Klaaßen, and Steffen (2024) processes output data derived from a modified version of the META integrated assessment model (Dietz et al., 2021, PNAS; for our modifications regarding the GDP-temperature damage function, the calibration of a catastrophic AMOC collapse and the omission of ocean methane hydrates and an AMOC slowdown, see Methods and the Supplementary Information), which is publicly available in Julia. The R code in this repository then performs dividend discount modeling based on the GDP reductions by country and year in META's output data to calculate the impacts of climate change GDP impacts with and without climate tipping points on present values (PVs) of future dividends at the country and stock index level. If you have questions about the code or find any errors/bugs, please contact Paul Waidelich (paul.waidelich[at]gess.ethz.ch, corresponding author).

*NOTE: This clean repository serves for the review process only to allow for transparency regarding the scripts & analyses underlying the submitted manuscript. It does not feature the full output files from META's Julia version (which, as of August 2024, amount to around 60 GB) or the full output files created by the `R/calculate_pvloss_by_market.R` script in this repository (which, as of August 2024, amount to over 55 GB). Therefore, the R scripts are executable using this stand-alone repository alone until the `R/calculate_pvloss_by_market.R` script, which requires the full output of META's Julia version to be available. An extended Zenodo repository that also includes all larger output files will be published upon acceptance.*

*NOTE: As the MSCI data for index composition used in the paper is proprietary, the public repository and, specifically, the `data/Calibration.xlsx` Excel workbook contains only censored versions of the original data. To obtain full index composition data, please reach out to MSCI or contact the corresponding author with proof that you have the required license to receive the full workbook version.*

## Overview of folder structure

1. `R`: hosts all R scripts described below.
2. `data`: hosts raw input files sourced from other sources (with date stamps in the filename indicating the download date), as well as the `data/Calibration.xlsx` Excel workbook, in which we calculate the weight of countries in the market capitalization of the different stock indices and the current market capitalization of each index based on stock index composition data provided by MSCI. `data/Calibration.xlsx` also features the key inputs used to calculate PVs of future dividends (risk-free rate, equity risk premium, inflation rate) and the country risk premium data from the Damodaran database, which are used in the `R/create_expectedpv_weights.R` script. The folder also contains the `Berkeley Earth GWL Calculation.xlsx` workbook, in which we calculate the baseline temperature for the COACCH damage functions (see Supplementary Note 1).
3. `data/COACCH_damagefunctions`: hosts the COACCH damage function parameters (`data/COACCH_damagefunctions/COACCH.csv`) from the COACCH Zenodo repository (https://zenodo.org/records/5546264) and matching files taken from the MIMOSA GitHub repo (https://github.com/UtrechtUniversity/mimosa) used to assign each country in META to one of the regions in `data/COACCH_damagefunctions/COACCH.csv` (based on the IMAGE model regions). These files are used in the `R/prepare_coacch_betas.R` script.
4. `data/META AMOC`: hosts the `AMOCparams.csv` file taken directly from the `/data` directory of the META GitHub repository, which is used in the `R/prepare_amoc_gdp_impacts.R` script.
5. `data/META MC results`: hosts all relevant outputs from the META model's Julia version (annual reductions in GDP as `lossfactor_conspc`, global warming as `T_AT`, global sea level rise sa `SLR`), with one subfolder for META specification (e.g., 10,000 Monte Carlo runs for RCP4.5-SSP2 using all tipping points and the COACCH damage function). All specifications used are listed in `data/META MC results/specifications_META.csv` and the Monte Carlo draws used for META parameters are stored in `data/META MC results/mc_draws_used_n10000.csv`. In addition, the `data/META MC results/ssp_countrydata_nocc` subfolder hosts GDP, GDPpc and population numbers by country and year as per the SSP scenarios used by META.
6. `data/MSCI Factsheets`: hosts all MSCI index factsheets, incl. country-level indices, from which we take dividend yields for the validation of our dividend discounting model in our Supplementary Information. Dividend yields from these factsheets are manually added to the  `data/Calibration.xlsx` Excel file (sheet `Portfolio`).
7. `data/intermediate`: hosts intermediate data objects created as part of the R pipeline, such as the GDP growth data created based on the raw SSP input files, which is created by the `R/calculate_gdpgrowthrates.R` script and used by `R/create_expectedpv_weights.R`.
8. `data/output`: hosts CSV files created by `R/prepare_coacch_betas.R`, `R/prepare_iso3_with_portfolioweights.R`, and `R/prepare_amoc_gdp_impacts.R` that are used as inputs to the META model's Julia version to calibrate model input parameters (for the COACCH GDP-temperature damage function, for subsetting output files to countries that feature in any of our MSCI indices, and to calibrate the GDP impacts of a catastrophic AMOC collapse).
9. `data/portfolio_simulation`: hosts the PV loss distributions returned by `R/calculate_pvloss_by_market.R`, with different subfolders for the PV reduction by country (`data/portfolio_simulation/df_npv_bymarket`) and by stock index (`data/portfolio_simulation/df_npv_fullportfolio`).
10. `data/weights`: hosts the weights of a given year in a country's overall PV of future dividends calculated by `R/create_expectedpv_weights.R` as RDS files. Note that one file of weights will be created for each combination of the time horizon considered (e.g., from 2024 to 2100 in our baseline setting) and the equity risk premium (ERP) used in the investor discount rate (for instance, `..._erp0.05` in the filename indicates that a 5% ERP was used).
11. `graphs`: directory where all figures (created by the `R/analyse_portfolioimpacts.R` script) will be saved.
12. `tables`: directory where all tables (created by the `R/analyse_portfolioimpacts.R` script) will be saved in TEX format.


## Overview of scripts

For all scripts, the working directory must be set to the highest level of this repository.

The following scripts provide inputs for the modified META model version and, therefore, must be executed *before* executing the Monte Carlo runs in META.
1.  `R/prepare_coacch_betas.R`: loads the region-specific COACCH damage functions (excl. sea level rise impacts because these are already covered in META by a different damage function), estimates their distribution based on the published percentile data points (see Supplementary Note 1) and assigns them to countries based on matching files from the COACCH Zenodo repository and the MIMOSA GitHub repository (Van der Wijst et al., 2023, NCC). We impute the COACCH region for the following countries due to missing data: Andorra, Liechtenstein and San Marino are assigned to Western Europe (WEU), Palestine is assigned to the Middle East (ME), Tuvalu and Nauru are assigned to Oceania (OCE).
2.  `R/prepare_amoc_gdp_impacts.R`: calculates how the 15% global GDP impact in case of an AMOC collapse from the central specification of Cai et al. (2016, NCC) is allocated to countries based on AMOC-related temperature shifts in META's AMOC module (see Supplementary Note 3).
3.  `R/prepare_iso3_with_portfolioweights.R`: checks for which countries (identified by ISO3 codes) we have non-zero portfolio weights in any of the MSCI indices under consideration. These ISO3 codes are returned in a CSV file. This file is then used in the META repository to subset Monte Carlo output files to the countries of interest, which reduces storage requirements considerably.

After executing these scripts, copy the created CSV files under `data/output` into the `data` folder of the META GitHub repository and execute the Monte Carlo runs in META by running the `src/extract_lossfactors.jl` Julia script (see next README section).

After all Monte Carlo runs in META have been carried out and the `data/META MC results` of this repository has been populated with the META output files, the following scripts process these outputs and must be executed in the specified order:

4. `R/calculate_gdpgrowthrates.R`: loads country-level GDP trajectories in the absence of climate change as per META, extrapolates the values for Hong Kong to Taiwan (which is missing in the META output), and saves year-to-year growth rates under `data/intermediate/gdp_growth.rds`. Values are used by `R/create_expectedpv_weights.R` to calculate the weight of different years in a country's PV of future dividends under different discount rate regimes (i.e., with and without a country risk premium).
5. `R/create_expectedpv_weights.R`: projects the growth of dividends based on GDP growth and calculates the share of a given year's dividends in a country's total PV of future dividends, based on user-specified parameters for the investor discount rate and the time horizon under consideration. These weights are used by `R/calculate_pvloss_by_market.R` to translate the reductions in future GDP returned by META's Monte Carlo runs into reductions of future dividends and, ultimately, the PV of future dividends at the country level or for different stock indices.
6. `R/calculate_pvloss_by_market.R`: uses the reductions in GDP from META's Monte Carlo runs and the PV weights created in `R/create_expectedpv_weights.R` to calculate reductions in the PV of future dividends due to climate change impacts on GDP. Damages for Taiwan (which is missing in META) are extrapolated based on Hong Kong. To reduce runtime and avoid redundant calculations, PV reductions for a given country are calculated as the dot product of each year's weight in a country's PV and the respective year's reductions in GDP; portfolio-level PV reductions are calculated as the dot product of countries' current share in the index's market cap and their PV reduction. Results for country-level and index-level PV reductions are saved under `data/portfolio_simulation/df_npv_bymarket` and `data/portfolio_simulation/df_npv_fullportfolio`, respectively.
7. `R/analyse_portfolioimpacts.R`: calculates the relevant summary statistics of PV loss distributions based on the output created by `R/calculate_pvloss_by_market.R` for different model specifications (e.g., with and without climate tipping points) and RCP-SSP scenarios and creates all charts, tables, and summary statistics in the main manuscript and the Supplementary Information, sourcing helper functions such as `R/summarize_npv_distribution.R`. Charts are saved under `/graphs`, tables under `/tables`.
8. `R/conduct_dividend_regressions_and_validation.R`: loads the historical GDP, stock return and dividend data from the Jorda-Schularick-Taylor Macrohistory database and regresses inflation-adjusted dividend growth on economic growth to validate the assumption in the dividend discount model in `R/create_expectedpv_weights.R` (see Supplementary Note 5). Also calculates current dividends based on MSCI market cap data and dividend yield information by country/index obtained from MSCI factsheets to validate the market cap implied by the dividend discount model against the actual market cap data (see Supplementary Note 4).

The following scripts are helper functions sourced and used in `R/analyse_portfolioimpacts.R` and do NOT need to be executed by the user:

9. `R/utils/summarize_npv_distribution.R`: calculates mean/median/sd/min/max and quantiles of the PV loss distribution created by `R/calculate_pvloss_by_market.R`.
10. `R/utils/clean_value_labels.R`: cleans character labels of country names, model specifications, climate tipping points, RCP scenarios, and stock indices.

## Monte Carlo analysis in META

The original version of the META-2021 Julia version can be found here (https://github.com/openmodels/META-2021) and has been forked here (https://github.com/pwaidelich/META-2021) to implement all modifications outlined in our Supplementary Information (branch: `current_analysis_newdamage`). To calculate the GDP reductions used as an input to our dividend discount modeling, open the script `src/extract_lossfactors.jl` in our modified META repository and adjust the `dir_output_global` in the header of the script to a folder where you want to store all META outputs and which features at least 200 GB of storage to accommodate all intermediate and output files. Then execute the `src/extract_lossfactors.jl` script in Julia with the working directory set to the `src` subfolder. Note that you may have to install the required Julia packages first, such as `Mimi` or `Random`.

The `src/extract_lossfactors.jl` script will create one subfolder per META specification, with the following naming conventions: `n[MC SAMPLE SIZE]_[RCP-SSP scenario]_persist[PERSISTENCE ASSUMPTION]_tip[TIPPING POINTS INCLUDED]_tdamage[GDP-TEMPERATURE DAMAGE FUNCTION SELECTED]_omh[SETTING FOR OCEAN METHANE HYDRATES TIPPING POINT]_amoc[SETTING FOR THE AMOC TIPPING POINT]`. For the placeholder in brackets, the following values are used:
1. MC SAMPLE SIZE: an integer indicating the number of Monte Carlo runs (e.g., `10000`)
2. RCP-SSP scenario: the selected META scenario (e.g., `RCP45SSP2`)
3. PERSISTENCE ASSUMPTION: either `1.0` (= no persistence), `0.0` (= full persistence) or `Distribution` (= sampling persistence from a 0-1 uniform distribution following Dietz et al., 2021)
4. TIPPING POINTS INCLUDED: either `all`, `none` or the acronym of an individual tipping point (e.g., `GIS` for a run that features only the Greenland Ice Sheet and no other tipping points)
5. GDP-TEMPERATURE DAMAGE FUNCTION SELECTED: either `coacch_distribution` for COACCH damage functions or `bhm_distribution` for Burke et al., 2015
6. SETTING FOR OCEAN METHANE HYDRATES TIPPING POINT: typically `none`, as we do not include this tipping point but set to `default` if we include it to emulate Dietz et al. (2021) for a robustness check
7. SETTING FOR THE AMOC TIPPING POINT: typically `none`, as we do not include this tipping point but set to `Cai` to include GDP impacts of an AMOC collapse following Cai et al. (2016) or `IPSL` to emulate Dietz et al. (2021) for a robustness check

In addition, the script will create a `specifications_META.csv` file listing all META specifications included in the analysis and a `mc_draws_used_n10000.csv` file that contains the Monte Carlo draws used for all stochastic parameters in META.

Note that almost all specifications include the surface albedo feedback (SAF) but if it is excluded (to emulate the No-Tipping-Points setting in Dietz et al., 2021, where the SAF is treated as a tipping point), the naming convention features an additional "_safnone" suffix.

Ideally, set the `dir_output_global` output directory in `src/extract_lossfactors.jl` directly to the `data/META MC results` subfolder of this repository. If this is not feasible (e.g., due to the storage requirements), you need to copy the entire content of the `dir_output_global` directory after executing `src/extract_lossfactors.jl` manually into `data/META MC results`. Once this is completed, proceed with the R scripts starting from 4. `R/calculate_gdpgrowthrates.R`.


## Data files in the repository
1. `data/Calibration.xlsx`: the main compilation of input data used for the dividend discount modeling (for more details, see sheet "README" in the Excel workbook). Input data sheets in the workbook are taken from the files below (except for the MSCI index composition raw data, which is proprietary and hence omitted in the public repository).
2. `data/240814 Damodaran ctrypremJuly24.xlsx`: the July 2024 update of country-specific risk premiums from the NYU Damodaran database (used to calibrate country risk premiums in our model's investor discount rate)
3. `data/240814 Damodaran histretSP.xls`: the 2024 update of historical US return data from the NYU Damodaran database (used to calibrate the risk-free rate in our model's investor discount rate, based on sovereign debt data for the US)
4. `data/240814 WB WDI US Inflation_Data.csv`: US inflation data from the World Bank's World Development Indicator database (used to calibrate the inflation rate in our model's investor discount rate)
5. `data/240115 JSTdatasetR6.xlsx`: the R6 version of the Jorda-Schularick-Taylor Macrohistory database (used for regressing dividend on GDP growth in the Supplementary Information). Additional documentation PDF files for the database are also included under `/data`
6. `data/240202 Berkeley Earth Global Annual Temperature.txt`: global annual temperature anomalies taken from Berkeley Earth (used for calibrating the COACCH damage functions' baseline temperature)
7. `data/230503 World Bank GDP_2015USD.csv`: country-level GDP information from the World Bank's World Development Indicator database (used to compare the allocation of GDP and market capitalization in the Supplementary Information)
8. `data/231207 World Bank Country Groups.xlsx`: World Bank data on income-based country groupings. As of August 2024, this is not directly used in the paper but serves to inform the discussion on emerging and frontier markets with their income level status

Date stamps in the filenames indicate the download date.

## System requirements

All R scripts in this repository were executed on a local machine with 32GB RAM and an i7 processor. The runtime for `R/calculate_pvloss_by_market.R` was several hours, with all other scripts requiring a few minutes or less to execute. Note that due to the size of the output files from META's Monte Carlo runs, the scripts may not work on local machines with lower memory. Versions of all R packages used are listed below.


## sessionInfo() in R

```
R version 4.3.1 (2023-06-16 ucrt)
Platform: x86_64-w64-mingw32/x64 (64-bit)
Running under: Windows 11 x64 (build 22631)

Matrix products: default


locale:
[1] LC_COLLATE=English_United States.utf8  LC_CTYPE=English_United States.utf8    LC_MONETARY=English_United States.utf8 LC_NUMERIC=C                           LC_TIME=English_United States.utf8    

time zone: Europe/Zurich
tzcode source: internal

attached base packages:
[1] stats     graphics  grDevices utils     datasets  methods   base     

other attached packages:
 [1] rriskDistributions_2.1.2 fixest_0.11.2            plm_2.6-3                vroom_1.6.4              waterfalls_1.0.0         janitor_2.2.0            xtable_1.8-4             readxl_1.4.3            
 [9] sf_1.0-17                rnaturalearthdata_1.0.0  rnaturalearth_1.0.1      ggpubr_0.6.0             dtplyr_1.3.1             PupillometryR_0.0.5      rlang_1.1.1              lubridate_1.9.3         
[17] forcats_1.0.0            stringr_1.5.1            dplyr_1.1.3              purrr_1.0.2              readr_2.1.4              tidyr_1.3.0              tibble_3.2.1             ggplot2_3.5.0           
[25] tidyverse_2.0.0         

loaded via a namespace (and not attached):
 [1] tidyselect_1.2.1       digest_0.6.33          timechange_0.2.0       lifecycle_1.0.4        dreamerr_1.3.0         mc2d_0.2.0             survival_3.5-5         terra_1.7-55          
 [9] magrittr_2.0.3         compiler_4.3.1         tools_4.3.1            utf8_1.2.4             eha_2.11.4             data.table_1.14.8      collapse_1.9.6         ggsignif_0.6.4        
[17] bit_4.0.5              classInt_0.4-10        abind_1.4-5            KernSmooth_2.23-21     expm_0.999-9           withr_3.0.1            numDeriv_2016.8-1.1    grid_4.3.1            
[25] fansi_1.0.5            e1071_1.7-13           colorspace_2.1-0       scales_1.3.0           MASS_7.3-60            mvtnorm_1.2-3          cli_3.6.1              crayon_1.5.2          
[33] miscTools_0.6-28       generics_0.1.3         rstudioapi_0.15.0      httr_1.4.7             tzdb_0.4.0             bdsmatrix_1.3-6        msm_1.7.1              DBI_1.1.3             
[41] proxy_0.4-27           splines_4.3.1          parallel_4.3.1         cellranger_1.1.0       marginaleffects_0.17.0 vctrs_0.6.3            Matrix_1.6-1.1         sandwich_3.0-2        
[49] jsonlite_1.8.7         carData_3.0-5          car_3.1-2              hms_1.1.3              bit64_4.0.5            ggrepel_0.9.3          rstatix_0.7.2          Formula_1.2-5         
[57] units_0.8-5            glue_1.6.2             codetools_0.2-19       stringi_1.7.12         gtable_0.3.5           lmtest_0.9-40          munsell_0.5.1          pillar_1.9.0          
[65] R6_2.5.1               maxLik_1.5-2           Rdpack_2.5             lattice_0.21-8         rbibutils_2.2.15       backports_1.4.1        snakecase_0.11.1       broom_1.0.5           
[73] class_7.3-22           Rcpp_1.0.11            nlme_3.1-162           zoo_1.8-12             pkgconfig_2.0.3 
```
