|:warning: | Find a condensed version of the GDP, population, and SDP estimates [here](https://github.com/thereseanders/gdppc_pop_gdp). |
| --- | --- |

# Replication materials

Anders, Therese, Christopher Fariss, and Jonathan Markowitz (2020): Bread before guns or butter: Introducing Surplus Domestic Product (SDP). In *International Studies Quarterly 64*(2), pp. 392-405.

Notes for users: 

- `replication_models_tables.R` estimates all models and reproduces all tables (tables 1 and 2 from the main text and table 4 from the appendix). This script saves model coefficients as .rds files, which can be read by `replication_figures.R` to produce coefficient plots.
- `replication_figures.R` reproduces all figures from the main text and online appendix.
- In order to reproduce the coefficent plots (figure 6, appendix figures 23-27), you must first run `replication_models_tables.R` to estimate the regression models. - `replication_models_tables.R` saves the model coefficients in .rds files, which can then be read by `replication_figures.R`.

All data neccessary to reproduce the tables and figures can be forund in `sdp_master.RData`. This can be loaded using the load statement provided in `replication_figures.R`, which may take a couple minutes to run.
