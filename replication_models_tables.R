####################################################################
#
# Estimates all regression models and reproduces all tables and 
# coefficent plots from
# Anders, Fariss, and Markowitz, Bread before guns or butter: 
# Introducing Surplus Domestic Product (SDP)
#
####################################################################

library(plm)
library(stargazer)
library(clubSandwich)
library(tidyverse)

setwd("/Volumes/GoogleDrive/My Drive/sdp_note_replication/replication files")
load("./data/sdp_master.RData")

# Scaling variables
vars_comp_surplus1095_unscaled <- c("comp_surplus1095_polity_invlog_nodenom_all",
                                    "comp_surplus1095_boix_invlog_nodenom_all",
                                    "comp_surplus1095_uds_invlog_nodenom_all",
                                    "comp_surplus1095_riv_invlog_nodenom_all",
                                    "comp_surplus1095_defense_invlog_nodenom_all",
                                    "comp_surplus1095_sscores_invlog_nodenom_all",
                                    "comp_surplus1095_absidealdiff_invlog_nodenom_all",
                                    "comp_surplus1095_igo_invlog_nodenom_all",
                                    "comp_surplus1095_diplo_invlog_nodenom_all",
                                    "comp_surplus1095_bitrade_invlog_nodenom_all",
                                    "comp_surplus1095_nopreference_invlog_nodenom_all",
                                    "comp_surplus1095_pecpc_invlog_nodenom_all",
                                    "comp_surplus1095_atopkappa_invlog_nodenom_all",
                                    "comp_surplus1095_atopbin_invlog_nodenom_all")

vars_comp_surplus1095_scaled <- c()
for(i in 1:length(vars_comp_surplus1095_unscaled)){
  vars_comp_surplus1095_scaled[i] <- paste("scaled", vars_comp_surplus1095_unscaled[i], sep = "_")
  master[[vars_comp_surplus1095_scaled[i]]] <- c(scale(master[,vars_comp_surplus1095_unscaled[i]]))
}

vars_comp_surplus730_unscaled <- c("comp_surplus730_polity_invlog_nodenom_all",
                                   "comp_surplus730_boix_invlog_nodenom_all",
                                   "comp_surplus730_uds_invlog_nodenom_all",
                                   "comp_surplus730_riv_invlog_nodenom_all",
                                   "comp_surplus730_defense_invlog_nodenom_all",
                                   "comp_surplus730_sscores_invlog_nodenom_all",
                                   "comp_surplus730_absidealdiff_invlog_nodenom_all",
                                   "comp_surplus730_igo_invlog_nodenom_all",
                                   "comp_surplus730_diplo_invlog_nodenom_all",
                                   "comp_surplus730_bitrade_invlog_nodenom_all",
                                   "comp_surplus730_nopreference_invlog_nodenom_all",
                                   "comp_surplus730_pecpc_invlog_nodenom_all",
                                   "comp_surplus730_atopkappa_invlog_nodenom_all",
                                   "comp_surplus730_atopbin_invlog_nodenom_all")

vars_comp_surplus730_scaled <- c()
for(i in 1:length(vars_comp_surplus730_unscaled)){
  vars_comp_surplus730_scaled[i] <- paste("scaled", vars_comp_surplus730_unscaled[i], sep = "_")
  master[[vars_comp_surplus730_scaled[i]]] <- c(scale(master[,vars_comp_surplus730_unscaled[i]]))
}

vars_comp_surplus365_unscaled <- c("comp_surplus365_polity_invlog_nodenom_all",
                                   "comp_surplus365_boix_invlog_nodenom_all",
                                   "comp_surplus365_uds_invlog_nodenom_all",
                                   "comp_surplus365_riv_invlog_nodenom_all",
                                   "comp_surplus365_defense_invlog_nodenom_all",
                                   "comp_surplus365_sscores_invlog_nodenom_all",
                                   "comp_surplus365_absidealdiff_invlog_nodenom_all",
                                   "comp_surplus365_igo_invlog_nodenom_all",
                                   "comp_surplus365_diplo_invlog_nodenom_all",
                                   "comp_surplus365_bitrade_invlog_nodenom_all",
                                   "comp_surplus365_nopreference_invlog_nodenom_all",
                                   "comp_surplus365_pecpc_invlog_nodenom_all",
                                   "comp_surplus365_atopkappa_invlog_nodenom_all",
                                   "comp_surplus365_atopbin_invlog_nodenom_all")

vars_comp_surplus365_scaled <- c()
for(i in 1:length(vars_comp_surplus365_unscaled)){
  vars_comp_surplus365_scaled[i] <- paste("scaled", vars_comp_surplus365_unscaled[i], sep = "_")
  master[[vars_comp_surplus365_scaled[i]]] <- c(scale(master[,vars_comp_surplus365_unscaled[i]]))
}

# GDP measures
vars_comp_gdp_unscaled <- c("comp_gdp_polity_invlog_nodenom_all",
                            "comp_gdp_boix_invlog_nodenom_all",
                            "comp_gdp_uds_invlog_nodenom_all",
                            "comp_gdp_riv_invlog_nodenom_all",
                            "comp_gdp_defense_invlog_nodenom_all",
                            "comp_gdp_sscores_invlog_nodenom_all",
                            "comp_gdp_absidealdiff_invlog_nodenom_all",
                            "comp_gdp_igo_invlog_nodenom_all",
                            "comp_gdp_diplo_invlog_nodenom_all",
                            "comp_gdp_bitrade_invlog_nodenom_all",
                            "comp_gdp_nopreference_invlog_nodenom_all",
                            "comp_gdp_pecpc_invlog_nodenom_all",
                            "comp_gdp_atopkappa_invlog_nodenom_all",
                            "comp_gdp_atopbin_invlog_nodenom_all")

vars_comp_gdp_scaled <- c()
for(i in 1:length(vars_comp_gdp_unscaled)){
  vars_comp_gdp_scaled[i] <- paste("scaled", vars_comp_gdp_unscaled[i], sep = "_")
  master[[vars_comp_gdp_scaled[i]]] <- c(scale(master[,vars_comp_gdp_unscaled[i]]))
}

# Pop measures
vars_comp_pop_unscaled <- c("comp_pop_polity_invlog_nodenom_all",
                            "comp_pop_boix_invlog_nodenom_all",
                            "comp_pop_uds_invlog_nodenom_all",
                            "comp_pop_riv_invlog_nodenom_all",
                            "comp_pop_defense_invlog_nodenom_all",
                            "comp_pop_sscores_invlog_nodenom_all",
                            "comp_pop_absidealdiff_invlog_nodenom_all",
                            "comp_pop_igo_invlog_nodenom_all",
                            "comp_pop_diplo_invlog_nodenom_all",
                            "comp_pop_bitrade_invlog_nodenom_all",
                            "comp_pop_nopreference_invlog_nodenom_all",
                            "comp_pop_pecpc_invlog_nodenom_all",
                            "comp_pop_atopkappa_invlog_nodenom_all",
                            "comp_pop_atopbin_invlog_nodenom_all")

vars_comp_pop_scaled <- c()
for(i in 1:length(vars_comp_pop_unscaled)){
  vars_comp_pop_scaled[i] <- paste("scaled", vars_comp_pop_unscaled[i], sep = "_")
  master[[vars_comp_pop_scaled[i]]] <- c(scale(master[,vars_comp_pop_unscaled[i]]))
}

# Creating one year lags
data2 <- master %>%
  mutate(year = year + 1)
names(data2)[!(names(data2) %in% c("ccode", "year"))] <- paste0(names(data2)[!(names(data2) %in% c("ccode", "year"))], "_l1")

master <- left_join(master, data2, by = c("ccode", "year"))


#frame as TSCS data
master_tscs <- pdata.frame(master, index = c("ccode", "year"), drop.index = F, row.names = TRUE)


#######################################
# Appendix Table 4: Summary statistics 
#######################################

vars_summary <- c("ln_milex_gdp",
                  "ln_tonnageimputed_gdp",
                  "ln_milex_surplus1095",
                  "ln_tonnageimputed_surplus1095",
                  "comp_surplus1095_polity_invlog_nodenom_all",
                  "comp_surplus1095_boix_invlog_nodenom_all",
                  "comp_surplus1095_uds_invlog_nodenom_all",
                  "comp_surplus1095_riv_invlog_nodenom_all",
                  "comp_surplus1095_defense_invlog_nodenom_all",
                  "comp_surplus1095_sscores_invlog_nodenom_all",
                  "comp_surplus1095_absidealdiff_invlog_nodenom_all",
                  "comp_surplus1095_igo_invlog_nodenom_all",
                  "comp_surplus1095_diplo_invlog_nodenom_all",
                  "comp_surplus1095_bitrade_invlog_nodenom_all",
                  "comp_surplus1095_pecpc_invlog_nodenom_all",
                  "comp_surplus1095_atopkappa_invlog_nodenom_all",
                  "comp_surplus1095_atopbin_invlog_nodenom_all",
                  "comp_surplus1095_nopreference_invlog_nodenom_all",
                  "comp_gdp_polity_invlog_nodenom_all",
                  "comp_gdp_boix_invlog_nodenom_all",
                  "comp_gdp_uds_invlog_nodenom_all",
                  "comp_gdp_riv_invlog_nodenom_all",
                  "comp_gdp_defense_invlog_nodenom_all",
                  "comp_gdp_sscores_invlog_nodenom_all",
                  "comp_gdp_absidealdiff_invlog_nodenom_all",
                  "comp_gdp_igo_invlog_nodenom_all",
                  "comp_gdp_diplo_invlog_nodenom_all",
                  "comp_gdp_bitrade_invlog_nodenom_all",
                  "comp_gdp_pecpc_invlog_nodenom_all",
                  "comp_gdp_atopkappa_invlog_nodenom_all",
                  "comp_gdp_atopbin_invlog_nodenom_all",
                  "comp_gdp_nopreference_invlog_nodenom_all",
                  "comp_pop_polity_invlog_nodenom_all",
                  "comp_pop_boix_invlog_nodenom_all",
                  "comp_pop_uds_invlog_nodenom_all",
                  "comp_pop_riv_invlog_nodenom_all",
                  "comp_pop_defense_invlog_nodenom_all",
                  "comp_pop_sscores_invlog_nodenom_all",
                  "comp_pop_absidealdiff_invlog_nodenom_all",
                  "comp_pop_igo_invlog_nodenom_all",
                  "comp_pop_diplo_invlog_nodenom_all",
                  "comp_pop_bitrade_invlog_nodenom_all",
                  "comp_pop_pecpc_invlog_nodenom_all",
                  "comp_pop_atopkappa_invlog_nodenom_all",
                  "comp_pop_atopbin_invlog_nodenom_all",
                  "comp_pop_nopreference_invlog_nodenom_all",
                  "WorldBank_gdp_2011_ppp_estimate_ln",
                  "ln_gdp_surplus1095_truncatedone",
                  "ln_subsistence1095",
                  "WorldBank_pop_estimate_ln",
                  "polity2")

master_summary <- master %>%
  select(one_of(vars_summary))

stargazer(master_summary,
          summary.stat = c("min", "median", "mean", "max", "sd", "n"),
          #type = "html",
          #out = "table_A4.html",
          covariate.labels = c("ln Military expenditure/GDP",
                               "ln Naval tonnage/GDP",
                               "ln Military Expenditure/SDP",
                               "ln Naval tonnage/SDP",
                               "Potential threat (SDP) joint democracy (Polity)",
                               "Potential threat (SDP) joint democracy (Boix et al.)",
                               "Potential threat (SDP) joint democracy (UDS)",
                               "Potential threat (SDP) rivalry",
                               "Potential threat (SDP) defense alliances",
                               "Potential threat (SDP) s-scores",
                               "Potential threat (SDP) absolute ideal difference",
                               "Potential threat (SDP) joint IGO membership",
                               "Potential threat (SDP) diplomatic exchange",
                               "Potential threat (SDP) bilateral trade",
                               "Potential threat (SDP) per capita energy consumption",
                               "Potential threat (SDP) ATOP Alliances (continuous)",
                               "Potential threat (SDP) ATOP Alliances (binary)",
                               "Potential threat (SDP) no interest variable",
                               "Potential threat (GDP) joint democracy (Polity)",
                               "Potential threat (GDP) joint democracy (Boix et al.)",
                               "Potential threat (GDP) joint democracy (UDS)",
                               "Potential threat (GDP) rivalry",
                               "Potential threat (GDP) defense alliances",
                               "Potential threat (GDP) s-scores",
                               "Potential threat (GDP) absolute ideal difference",
                               "Potential threat (GDP) joint IGO membership",
                               "Potential threat (GDP) diplomatic exchange",
                               "Potential threat (GDP) bilateral trade",
                               "Potential threat (GDP) per capita energy consumption",
                               "Potential threat (GDP) ATOP Alliances (continuous)",
                               "Potential threat (GDP) ATOP Alliances (binary)",
                               "Potential threat (GDP) no interest variable",
                               "Potential threat (population) joint democracy (Polity)",
                               "Potential threat (population) joint democracy (Boix et al.)",
                               "Potential threat (population) joint democracy (UDS)",
                               "Potential threat (population) rivalry",
                               "Potential threat (population) defense alliances",
                               "Potential threat (population) s-scores",
                               "Potential threat (population) absolute ideal difference",
                               "Potential threat (population) joint IGO membership",
                               "Potential threat (population) diplomatic exchange",
                               "Potential threat (population) bilateral trade",
                               "Potential threat (population) per capita energy consumption",
                               "Potential threat (population) ATOP Alliances (continuous)",
                               "Potential threat (population) ATOP Alliances (binary)",
                               "Potential threat (population) no interest variable",
                               "ln GDP",
                               "ln SDP",
                               "ln Subsistence",
                               "ln Population",
                               "Polity2 score"),
          title = "Summary statistics for key variables.",
          notes = c("\\textit{Notes}:",
                    "GDP measure in constant 2011 international PPP dollars.",
                    "SDP is based on a \\$3 per diem subsistence level.",
                    "Loss of strength gradient measured using the following formula $\\frac{1}{\\log(distance)}$.",
                    "Very small values are rounded to 0 in the output above."),
          font.size = "scriptsize",
          digits = 2)

################
#
# **************
#
# $3/day Surplus
#
# **************
#
################

############## Setting up variables ##################
# Main variables
vars_comp <- c("scaled_comp_surplus1095_polity_invlog_nodenom_all_l1",
               "scaled_comp_surplus1095_boix_invlog_nodenom_all_l1",
               "scaled_comp_surplus1095_uds_invlog_nodenom_all_l1",
               "scaled_comp_surplus1095_riv_invlog_nodenom_all_l1",
               "scaled_comp_surplus1095_defense_invlog_nodenom_all_l1",
               "scaled_comp_surplus1095_sscores_invlog_nodenom_all_l1",
               "scaled_comp_surplus1095_absidealdiff_invlog_nodenom_all_l1",
               "scaled_comp_surplus1095_igo_invlog_nodenom_all_l1",
               "scaled_comp_surplus1095_diplo_invlog_nodenom_all_l1",
               "scaled_comp_surplus1095_bitrade_invlog_nodenom_all_l1",
               "scaled_comp_surplus1095_nopreference_invlog_nodenom_all_l1",
               "scaled_comp_surplus1095_pecpc_invlog_nodenom_all_l1",
               "scaled_comp_surplus1095_atopkappa_invlog_nodenom_all_l1",
               "scaled_comp_surplus1095_atopbin_invlog_nodenom_all_l1")

varlabels_comp <- c("$\\text{PT SDP Joint Democracy (Polity)}_{i,t-1}$",
                    "$\\text{PT SDP Joint Democracy (Boix et al.)}_{i,t-1}$",
                    "$\\text{PT SDP Joint Democracy (UDS)}_{i,t-1}$",
                    "$\\text{PT SDP Rivalry}_{i,t-1}$",
                    "$\\text{PT SDP Defense Pacts}_{i,t-1}$",
                    "$\\text{PT SDP S-Scores}_{i,t-1}$",
                    "$\\text{PT SDP Ideal Difference}_{i,t-1}$",
                    "$\\text{PT SDP IGO Membership}_{i,t-1}$",
                    "$\\text{PT SDP Diplo. Exchange}_{i,t-1}$",
                    "$\\text{PT SDP Bilateral Trade}_{i,t-1}$",
                    "$\\text{PT SDP No Interest Variable}_{i,t-1}$",
                    "$\\text{PT SDP Energy consumption}_{i,t-1}$",
                    "$\\text{PT SDP Alliances (continuous)}_{i,t-1}$",
                    "$\\text{PT SDP Alliances (binary)}_{i,t-1}$")


# Main variables
vars_comppop <- c("scaled_comp_pop_polity_invlog_nodenom_all_l1",
                  "scaled_comp_pop_boix_invlog_nodenom_all_l1",
                  "scaled_comp_pop_uds_invlog_nodenom_all_l1",
                  "scaled_comp_pop_riv_invlog_nodenom_all_l1",
                  "scaled_comp_pop_defense_invlog_nodenom_all_l1",
                  "scaled_comp_pop_sscores_invlog_nodenom_all_l1",
                  "scaled_comp_pop_absidealdiff_invlog_nodenom_all_l1",
                  "scaled_comp_pop_igo_invlog_nodenom_all_l1",
                  "scaled_comp_pop_diplo_invlog_nodenom_all_l1",
                  "scaled_comp_pop_bitrade_invlog_nodenom_all_l1",
                  "scaled_comp_pop_nopreference_invlog_nodenom_all_l1",
                  "scaled_comp_pop_pecpc_invlog_nodenom_all_l1",
                  "scaled_comp_pop_atopkappa_invlog_nodenom_all_l1",
                  "scaled_comp_pop_atopbin_invlog_nodenom_all_l1")

varlabels_comppop <- c("$\\text{PT Pop Joint Democracy (Polity)}_{i,t-1}$",
                       "$\\text{PT Pop Joint Democracy (Boix et al.)}_{i,t-1}$",
                       "$\\text{PT Pop Joint Democracy (UDS)}_{i,t-1}$",
                       "$\\text{PT Pop Rivalry}_{i,t-1}$",
                       "$\\text{PT Pop Defense Pacts}_{i,t-1}$",
                       "$\\text{PT Pop S-Scores}_{i,t-1}$",
                       "$\\text{PT Pop Ideal Difference}_{i,t-1}$",
                       "$\\text{PT Pop IGO Membership}_{i,t-1}$",
                       "$\\text{PT Pop Diplo. Exchange}_{i,t-1}$",
                       "$\\text{PT Pop Bilateral Trade}_{i,t-1}$",
                       "$\\text{PT Pop No Interest Variable}_{i,t-1}$",
                       "$\\text{PT Pop Energy consumption}_{i,t-1}$",
                       "$\\text{PT Pop Alliances (continuous)}_{i,t-1}$",
                       "$\\text{PT Pop Alliances (binary)}_{i,t-1}$")

######################################
# Running $3 SDP models
######################################

## Controls 
# SDP
controlval_surplus1095_fe <- c("ln_gdp_surplus1095_truncatedone_l1",
                               "ln_subsistence1095_l1",
                               "polity2_l1")

controlvalalt_surplus1095_fe <- c("ln_gdp_surplus1095_truncatedone_l1",
                                  "polity2_l1")

controllab_surplus1095_fe <- c("$\\text{ln SDP}_{i,t-1}$",
                               "$\\text{ln Subsistence \\$3}_{i,t-1}$",
                               "$\\text{Polity2}_{i,t-1}$")

controllabalt_surplus1095_fe <- c("$\\text{ln SDP}_{i,t-1}$",
                                  "$\\text{Polity2}_{i,t-1}$")


dvval_surplus1095_milex <- c("ln_milex_surplus1095")

dvval_surplus1095_tonnageimputed <- c("ln_tonnageimputed_surplus1095")

dvlab_surplus1095_milex <- c("$\\ln\\text{ Military Expenditure/SDP}_{i,t}$")

dvlab_surplus1095_tonnageimputed <- c("$\\ln\\text{ Naval Tonnage/SDP}_{i,t}$")

#---------------------
# milex
#---------------------

### surplus1095
# FECY model, no controls
model_milex_fecy_surplus1095nocontrols <- list()
modelfits_milex_fecy_surplus1095nocontrols <- list()
coefs_milex_fecy_surplus1095nocontrols <- c()
ses_milex_fecy_surplus1095nocontrols <- list()
r2_milex_fecy_surplus1095nocontrols <- c()
for(i in 1) {
  modformula <- paste(dvval_surplus1095_milex, 
                      vars_comp[i],
                      sep = "~")
  model <- plm(as.formula(modformula), data = master_tscs, model = "within", effect = "twoways")
  model_milex_fecy_surplus1095nocontrols[[i]] <- model
  modelfits_milex_fecy_surplus1095nocontrols[[i]] <- coef_test(model, vcov = "CR2", cluster = "individual", test = "Satterthwaite")
  coefs_milex_fecy_surplus1095nocontrols[[i]] <- modelfits_milex_fecy_surplus1095nocontrols[[i]]$beta
  ses_milex_fecy_surplus1095nocontrols[[i]] <- modelfits_milex_fecy_surplus1095nocontrols[[i]]$SE
  r2_milex_fecy_surplus1095nocontrols[i] <- summary(model)$r.squared[2]
}


### surplus1095
# FECY model, controls
model_milex_fecy_surplus1095 <- list()
modelfits_milex_fecy_surplus1095 <- list()
coefs_milex_fecy_surplus1095 <- c()
ses_milex_fecy_surplus1095 <- list()
r2_milex_fecy_surplus1095 <- c()
for(i in 1) {
  modformula <- paste(dvval_surplus1095_milex, 
                      paste(vars_comp[i], 
                            controlval_surplus1095_fe[1], 
                            controlval_surplus1095_fe[2],
                            controlval_surplus1095_fe[3], sep = "+"), 
                      sep = "~")
  model <- plm(as.formula(modformula), data = master_tscs, model = "within", effect = "twoways")
  model_milex_fecy_surplus1095[[i]] <- model
  modelfits_milex_fecy_surplus1095[[i]] <- coef_test(model, vcov = "CR2", cluster = "individual", test = "Satterthwaite")
  coefs_milex_fecy_surplus1095[[i]] <- modelfits_milex_fecy_surplus1095[[i]]$beta
  ses_milex_fecy_surplus1095[[i]] <- modelfits_milex_fecy_surplus1095[[i]]$SE
  r2_milex_fecy_surplus1095[i] <- summary(model)$r.squared[2]
}


### surplus1095
# FECY model, controls alternative
model_milex_fecy_surplus1095alt <- list()
modelfits_milex_fecy_surplus1095alt <- list()
coefs_milex_fecy_surplus1095alt <- c()
ses_milex_fecy_surplus1095alt <- list()
r2_milex_fecy_surplus1095alt <- c()
for(i in 1) {
  modformula <- paste(dvval_surplus1095_milex,
                      paste(vars_comp[i],
                            controlvalalt_surplus1095_fe[1],
                            controlvalalt_surplus1095_fe[2], sep = "+"),
                      sep = "~")
  model <- plm(as.formula(modformula), data = master_tscs, model = "within", effect = "twoways")
  model_milex_fecy_surplus1095alt[[i]] <- model
  modelfits_milex_fecy_surplus1095alt[[i]] <- coef_test(model, vcov = "CR2", cluster = "individual", test = "Satterthwaite")
  coefs_milex_fecy_surplus1095alt[[i]] <- modelfits_milex_fecy_surplus1095alt[[i]]$beta
  ses_milex_fecy_surplus1095alt[[i]] <- modelfits_milex_fecy_surplus1095alt[[i]]$SE
  r2_milex_fecy_surplus1095alt[i] <- summary(model)$r.squared[2]
}


### pop
# FECY model, controls
model_milex_fecy_pop <- list()
modelfits_milex_fecy_pop <- list()
coefs_milex_fecy_pop <- c()
ses_milex_fecy_pop <- list()
r2_milex_fecy_pop <- c()
for(i in 1) {
  modformula <- paste(dvval_surplus1095_milex, 
                      paste(vars_comppop[i], 
                            controlval_surplus1095_fe[1], 
                            controlval_surplus1095_fe[2],
                            controlval_surplus1095_fe[3], sep = "+"), 
                      sep = "~")
  model <- plm(as.formula(modformula), data = master_tscs, model = "within", effect = "twoways")
  model_milex_fecy_pop[[i]] <- model
  modelfits_milex_fecy_pop[[i]] <- coef_test(model, vcov = "CR2", cluster = "individual", test = "Satterthwaite")
  coefs_milex_fecy_pop[[i]] <- modelfits_milex_fecy_pop[[i]]$beta
  ses_milex_fecy_pop[[i]] <- modelfits_milex_fecy_pop[[i]]$SE
  r2_milex_fecy_pop[i] <- summary(model)$r.squared[2]
}


# ### pop
# # FECY model, controls alternative
# model_milex_fecy_popalt <- list()
# modelfits_milex_fecy_popalt <- list()
# coefs_milex_fecy_popalt <- c()
# ses_milex_fecy_popalt <- list()
# r2_milex_fecy_popalt <- c()
# for(i in 1) {
#   modformula <- paste(dvval_surplus1095_milex,
#                       paste(vars_comppop[i],
#                             controlvalalt_surplus1095_fe[1],
#                             controlvalalt_surplus1095_fe[2], sep = "+"),
#                       sep = "~")
#   model <- plm(as.formula(modformula), data = master_tscs, model = "within", effect = "twoways")
#   model_milex_fecy_popalt[[i]] <- model
#   modelfits_milex_fecy_popalt[[i]] <- coef_test(model, vcov = "CR2", cluster = "individual", test = "Satterthwaite")
#   coefs_milex_fecy_popalt[[i]] <- modelfits_milex_fecy_popalt[[i]]$beta
#   ses_milex_fecy_popalt[[i]] <- modelfits_milex_fecy_popalt[[i]]$SE
#   r2_milex_fecy_popalt[i] <- summary(model)$r.squared[2]
# }


### surplus1095, pop
# FECY model, controls
model_milex_fecy_surplus1095pop <- list()
modelfits_milex_fecy_surplus1095pop <- list()
coefs_milex_fecy_surplus1095pop <- c()
ses_milex_fecy_surplus1095pop <- c()
r2_milex_fecy_surplus1095pop <- c()
for(i in 1:length(vars_comp)) {
  modformula <- paste(dvval_surplus1095_milex, 
                      paste(vars_comp[i], 
                            vars_comppop[i], 
                            controlval_surplus1095_fe[1], 
                            controlval_surplus1095_fe[2],
                            controlval_surplus1095_fe[3], sep = "+"), 
                      sep = "~")
  model <- plm(as.formula(modformula), data = master_tscs, model = "within", effect = "twoways")
  model_milex_fecy_surplus1095pop[[i]] <- model
  modelfits_milex_fecy_surplus1095pop[[i]] <- coef_test(model, vcov = "CR2", cluster = "individual", test = "Satterthwaite")
  coefs_milex_fecy_surplus1095pop[[i]] <- modelfits_milex_fecy_surplus1095pop[[i]]$beta
  ses_milex_fecy_surplus1095pop[[i]] <- modelfits_milex_fecy_surplus1095pop[[i]]$SE
  r2_milex_fecy_surplus1095pop[i] <- summary(model)$r.squared[2]
}


### surplus1095, pop
# FECY model, controls alternative
model_milex_fecy_surplus1095popalt <- list()
modelfits_milex_fecy_surplus1095popalt <- list()
coefs_milex_fecy_surplus1095popalt <- c()
ses_milex_fecy_surplus1095popalt <- c()
r2_milex_fecy_surplus1095popalt <- c()
for(i in 1:length(vars_comp)) {
  modformula <- paste(dvval_surplus1095_milex,
                      paste(vars_comp[i],
                            vars_comppop[i],
                            controlvalalt_surplus1095_fe[1],
                            controlvalalt_surplus1095_fe[2], sep = "+"),
                      sep = "~")
  model <- plm(as.formula(modformula), data = master_tscs, model = "within", effect = "twoways")
  model_milex_fecy_surplus1095popalt[[i]] <- model
  modelfits_milex_fecy_surplus1095popalt[[i]] <- coef_test(model, vcov = "CR2", cluster = "individual", test = "Satterthwaite")
  coefs_milex_fecy_surplus1095popalt[[i]] <- modelfits_milex_fecy_surplus1095popalt[[i]]$beta
  ses_milex_fecy_surplus1095popalt[[i]] <- modelfits_milex_fecy_surplus1095popalt[[i]]$SE
  r2_milex_fecy_surplus1095popalt[i] <- summary(model)$r.squared[2]
}


### surplus1095 interaction pop
# FECY model, controls
model_milex_fecy_surplus1095Xpop <- list()
modelfits_milex_fecy_surplus1095Xpop <- list()
coefs_milex_fecy_surplus1095Xpop <- c()
ses_milex_fecy_surplus1095Xpop <- c()
r2_milex_fecy_surplus1095Xpop <- c()
for(i in 1) {
  modformula <- paste(dvval_surplus1095_milex, 
                      paste(paste(vars_comp[i], vars_comppop[i], sep = "*"), 
                            controlval_surplus1095_fe[1], 
                            controlval_surplus1095_fe[2],
                            controlval_surplus1095_fe[3], sep = "+"), 
                      sep = "~")
  model <- plm(as.formula(modformula), data = master_tscs, model = "within", effect = "twoways")
  model_milex_fecy_surplus1095Xpop[[i]] <- model
  modelfits_milex_fecy_surplus1095Xpop[[i]] <- coef_test(model, vcov = "CR2", cluster = "individual", test = "Satterthwaite")
  coefs_milex_fecy_surplus1095Xpop[[i]] <- modelfits_milex_fecy_surplus1095Xpop[[i]]$beta
  ses_milex_fecy_surplus1095Xpop[[i]] <- modelfits_milex_fecy_surplus1095Xpop[[i]]$SE
  r2_milex_fecy_surplus1095Xpop[i] <- summary(model)$r.squared[2]
}


# ### surplus1095 interaction pop
# # FECY model, controls alternative
# model_milex_fecy_surplus1095Xpopalt <- list()
# modelfits_milex_fecy_surplus1095Xpopalt <- list()
# coefs_milex_fecy_surplus1095Xpopalt <- c()
# ses_milex_fecy_surplus1095Xpopalt <- c()
# r2_milex_fecy_surplus1095Xpopalt <- c()
# for(i in 1:length(vars_comp)) {
#   modformula <- paste(dvval_surplus1095_milex,
#                       paste(paste(vars_comp[i], vars_comppop[i], sep = "*"),
#                             controlvalalt_surplus1095_fe[1],
#                             controlvalalt_surplus1095_fe[2], sep = "+"),
#                       sep = "~")
#   model <- plm(as.formula(modformula), data = master_tscs, model = "within", effect = "twoways")
#   model_milex_fecy_surplus1095Xpopalt[[i]] <- model
#   modelfits_milex_fecy_surplus1095Xpopalt[[i]] <- coef_test(model, vcov = "CR2", cluster = "individual", test = "Satterthwaite")
#   coefs_milex_fecy_surplus1095Xpopalt[[i]] <- modelfits_milex_fecy_surplus1095Xpopalt[[i]]$beta
#   ses_milex_fecy_surplus1095Xpopalt[[i]] <- modelfits_milex_fecy_surplus1095Xpopalt[[i]]$SE
#   r2_milex_fecy_surplus1095Xpopalt[i] <- summary(model)$r.squared[2]
# }


#---------------------
# tonnageimputed
#---------------------

### surplus1095
# FECY model, no controls
model_tonnageimputed_fecy_surplus1095nocontrols <- list()
modelfits_tonnageimputed_fecy_surplus1095nocontrols <- list()
coefs_tonnageimputed_fecy_surplus1095nocontrols <- c()
ses_tonnageimputed_fecy_surplus1095nocontrols <- c()
r2_tonnageimputed_fecy_surplus1095nocontrols <- c()
for(i in 1) {
  modformula <- paste(dvval_surplus1095_tonnageimputed, 
                      vars_comp[i], 
                      sep = "~")
  model <- plm(as.formula(modformula), data = master_tscs, model = "within", effect = "twoways")
  model_tonnageimputed_fecy_surplus1095nocontrols[[i]] <- model
  modelfits_tonnageimputed_fecy_surplus1095nocontrols[[i]] <- coef_test(model, vcov = "CR2", cluster = "individual", test = "Satterthwaite")
  coefs_tonnageimputed_fecy_surplus1095nocontrols[[i]] <- modelfits_tonnageimputed_fecy_surplus1095nocontrols[[i]]$beta
  ses_tonnageimputed_fecy_surplus1095nocontrols[[i]] <- modelfits_tonnageimputed_fecy_surplus1095nocontrols[[i]]$SE
  r2_tonnageimputed_fecy_surplus1095nocontrols[i] <- summary(model)$r.squared[2]
}


### surplus1095
# FECY model, controls
model_tonnageimputed_fecy_surplus1095 <- list()
modelfits_tonnageimputed_fecy_surplus1095 <- list()
coefs_tonnageimputed_fecy_surplus1095 <- c()
ses_tonnageimputed_fecy_surplus1095 <- c()
r2_tonnageimputed_fecy_surplus1095 <- c()
for(i in 1) {
  modformula <- paste(dvval_surplus1095_tonnageimputed, 
                      paste(vars_comp[i], 
                            controlval_surplus1095_fe[1], 
                            controlval_surplus1095_fe[2],
                            controlval_surplus1095_fe[3], sep = "+"), 
                      sep = "~")
  model <- plm(as.formula(modformula), data = master_tscs, model = "within", effect = "twoways")
  model_tonnageimputed_fecy_surplus1095[[i]] <- model
  modelfits_tonnageimputed_fecy_surplus1095[[i]] <- coef_test(model, vcov = "CR2", cluster = "individual", test = "Satterthwaite")
  coefs_tonnageimputed_fecy_surplus1095[[i]] <- modelfits_tonnageimputed_fecy_surplus1095[[i]]$beta
  ses_tonnageimputed_fecy_surplus1095[[i]] <- modelfits_tonnageimputed_fecy_surplus1095[[i]]$SE
  r2_tonnageimputed_fecy_surplus1095[i] <- summary(model)$r.squared[2]
}


# FECY model, controls alternative
model_tonnageimputed_fecy_surplus1095alt <- list()
modelfits_tonnageimputed_fecy_surplus1095alt <- list()
coefs_tonnageimputed_fecy_surplus1095alt <- c()
ses_tonnageimputed_fecy_surplus1095alt <- c()
r2_tonnageimputed_fecy_surplus1095alt <- c()
for(i in 1) {
  modformula <- paste(dvval_surplus1095_tonnageimputed,
                      paste(vars_comp[i],
                            controlvalalt_surplus1095_fe[1],
                            controlvalalt_surplus1095_fe[2], sep = "+"),
                      sep = "~")
  model <- plm(as.formula(modformula), data = master_tscs, model = "within", effect = "twoways")
  model_tonnageimputed_fecy_surplus1095alt[[i]] <- model
  modelfits_tonnageimputed_fecy_surplus1095alt[[i]] <- coef_test(model, vcov = "CR2", cluster = "individual", test = "Satterthwaite")
  coefs_tonnageimputed_fecy_surplus1095alt[[i]] <- modelfits_tonnageimputed_fecy_surplus1095alt[[i]]$beta
  ses_tonnageimputed_fecy_surplus1095alt[[i]] <- modelfits_tonnageimputed_fecy_surplus1095alt[[i]]$SE
  r2_tonnageimputed_fecy_surplus1095alt[i] <- summary(model)$r.squared[2]
}



### pop
# FECY model, controls
model_tonnageimputed_fecy_pop <- list()
modelfits_tonnageimputed_fecy_pop <- list()
coefs_tonnageimputed_fecy_pop <- c()
ses_tonnageimputed_fecy_pop <- c()
r2_tonnageimputed_fecy_pop <- c()
for(i in 1) {
  modformula <- paste(dvval_surplus1095_tonnageimputed, 
                      paste(vars_comppop[i], 
                            controlval_surplus1095_fe[1], 
                            controlval_surplus1095_fe[2],
                            controlval_surplus1095_fe[3], sep = "+"), 
                      sep = "~")
  model <- plm(as.formula(modformula), data = master_tscs, model = "within", effect = "twoways")
  model_tonnageimputed_fecy_pop[[i]] <- model
  modelfits_tonnageimputed_fecy_pop[[i]] <- coef_test(model, vcov = "CR2", cluster = "individual", test = "Satterthwaite")
  coefs_tonnageimputed_fecy_pop[[i]] <- modelfits_tonnageimputed_fecy_pop[[i]]$beta
  ses_tonnageimputed_fecy_pop[[i]] <- modelfits_tonnageimputed_fecy_pop[[i]]$SE
  r2_tonnageimputed_fecy_pop[i] <- summary(model)$r.squared[2]
}

# # FECY model, controls alternative
# model_tonnageimputed_fecy_popalt <- list()
# modelfits_tonnageimputed_fecy_popalt <- list()
# coefs_tonnageimputed_fecy_popalt <- c()
# ses_tonnageimputed_fecy_popalt <- c()
# r2_tonnageimputed_fecy_popalt <- c()
# for(i in 1) {
#   modformula <- paste(dvval_surplus1095_tonnageimputed,
#                       paste(vars_comppop[i],
#                             controlvalalt_surplus1095_fe[1],
#                             controlvalalt_surplus1095_fe[2], sep = "+"),
#                       sep = "~")
#   model <- plm(as.formula(modformula), data = master_tscs, model = "within", effect = "twoways")
#   model_tonnageimputed_fecy_popalt[[i]] <- model
#   modelfits_tonnageimputed_fecy_popalt[[i]] <- coef_test(model, vcov = "CR2", cluster = "individual", test = "Satterthwaite")
#   coefs_tonnageimputed_fecy_popalt[[i]] <- modelfits_tonnageimputed_fecy_popalt[[i]]$beta
#   ses_tonnageimputed_fecy_popalt[[i]] <- modelfits_tonnageimputed_fecy_popalt[[i]]$SE
#   r2_tonnageimputed_fecy_popalt[i] <- summary(model)$r.squared[2]
# }


### surplus1095, pop
# FECY model, controls
model_tonnageimputed_fecy_surplus1095pop <- list()
modelfits_tonnageimputed_fecy_surplus1095pop <- list()
coefs_tonnageimputed_fecy_surplus1095pop <- c()
ses_tonnageimputed_fecy_surplus1095pop <- c()
r2_tonnageimputed_fecy_surplus1095pop <- c()
for(i in 1:length(vars_comp)) {
  modformula <- paste(dvval_surplus1095_tonnageimputed, 
                      paste(vars_comp[i], 
                            vars_comppop[i], 
                            controlval_surplus1095_fe[1], 
                            controlval_surplus1095_fe[2],
                            controlval_surplus1095_fe[3], sep = "+"), 
                      sep = "~")
  model <- plm(as.formula(modformula), data = master_tscs, model = "within", effect = "twoways")
  model_tonnageimputed_fecy_surplus1095pop[[i]] <- model
  modelfits_tonnageimputed_fecy_surplus1095pop[[i]] <- coef_test(model, vcov = "CR2", cluster = "individual", test = "Satterthwaite")
  coefs_tonnageimputed_fecy_surplus1095pop[[i]] <- modelfits_tonnageimputed_fecy_surplus1095pop[[i]]$beta
  ses_tonnageimputed_fecy_surplus1095pop[[i]] <- modelfits_tonnageimputed_fecy_surplus1095pop[[i]]$SE
  r2_tonnageimputed_fecy_surplus1095pop[i] <- summary(model)$r.squared[2]
}

# FECY model, controls alternative
model_tonnageimputed_fecy_surplus1095popalt <- list()
modelfits_tonnageimputed_fecy_surplus1095popalt <- list()
coefs_tonnageimputed_fecy_surplus1095popalt <- c()
ses_tonnageimputed_fecy_surplus1095popalt <- c()
r2_tonnageimputed_fecy_surplus1095popalt <- c()
for(i in 1:length(vars_comp)) {
  modformula <- paste(dvval_surplus1095_tonnageimputed,
                      paste(vars_comp[i],
                            vars_comppop[i],
                            controlvalalt_surplus1095_fe[1],
                            controlvalalt_surplus1095_fe[2], sep = "+"),
                      sep = "~")
  model <- plm(as.formula(modformula), data = master_tscs, model = "within", effect = "twoways")
  model_tonnageimputed_fecy_surplus1095popalt[[i]] <- model
  modelfits_tonnageimputed_fecy_surplus1095popalt[[i]] <- coef_test(model, vcov = "CR2", cluster = "individual", test = "Satterthwaite")
  coefs_tonnageimputed_fecy_surplus1095popalt[[i]] <- modelfits_tonnageimputed_fecy_surplus1095popalt[[i]]$beta
  ses_tonnageimputed_fecy_surplus1095popalt[[i]] <- modelfits_tonnageimputed_fecy_surplus1095popalt[[i]]$SE
  r2_tonnageimputed_fecy_surplus1095popalt[i] <- summary(model)$r.squared[2]
}


### surplus1095 interaction pop
# FECY model, controls
model_tonnageimputed_fecy_surplus1095Xpop <- list()
modelfits_tonnageimputed_fecy_surplus1095Xpop <- list()
coefs_tonnageimputed_fecy_surplus1095Xpop <- c()
ses_tonnageimputed_fecy_surplus1095Xpop <- c()
r2_tonnageimputed_fecy_surplus1095Xpop <- c()
for(i in 1) {
  modformula <- paste(dvval_surplus1095_tonnageimputed, 
                      paste(paste(vars_comp[i], vars_comppop[i], sep = "*"), 
                            controlval_surplus1095_fe[1], 
                            controlval_surplus1095_fe[2],
                            controlval_surplus1095_fe[3], sep = "+"), 
                      sep = "~")
  model <- plm(as.formula(modformula), data = master_tscs, model = "within", effect = "twoways")
  model_tonnageimputed_fecy_surplus1095Xpop[[i]] <- model
  modelfits_tonnageimputed_fecy_surplus1095Xpop[[i]] <- coef_test(model, vcov = "CR2", cluster = "individual", test = "Satterthwaite")
  coefs_tonnageimputed_fecy_surplus1095Xpop[[i]] <- modelfits_tonnageimputed_fecy_surplus1095Xpop[[i]]$beta
  ses_tonnageimputed_fecy_surplus1095Xpop[[i]] <- modelfits_tonnageimputed_fecy_surplus1095Xpop[[i]]$SE
  r2_tonnageimputed_fecy_surplus1095Xpop[i] <- summary(model)$r.squared[2]
}

# 
# # FECY model, controls alternative
# model_tonnageimputed_fecy_surplus1095Xpopalt <- list()
# modelfits_tonnageimputed_fecy_surplus1095Xpopalt <- list()
# coefs_tonnageimputed_fecy_surplus1095Xpopalt <- c()
# ses_tonnageimputed_fecy_surplus1095Xpopalt <- c()
# r2_tonnageimputed_fecy_surplus1095Xpopalt <- c()
# for(i in 1:length(vars_comp)) {
#   modformula <- paste(dvval_surplus1095_tonnageimputed,
#                       paste(paste(vars_comp[i], vars_comppop[i], sep = "*"),
#                             controlvalalt_surplus1095_fe[1],
#                             controlvalalt_surplus1095_fe[2], sep = "+"),
#                       sep = "~")
#   model <- plm(as.formula(modformula), data = master_tscs, model = "within", effect = "twoways")
#   model_tonnageimputed_fecy_surplus1095Xpopalt[[i]] <- model
#   modelfits_tonnageimputed_fecy_surplus1095Xpopalt[[i]] <- coef_test(model, vcov = "CR2", cluster = "individual", test = "Satterthwaite")
#   coefs_tonnageimputed_fecy_surplus1095Xpopalt[[i]] <- modelfits_tonnageimputed_fecy_surplus1095Xpopalt[[i]]$beta
#   ses_tonnageimputed_fecy_surplus1095Xpopalt[[i]] <- modelfits_tonnageimputed_fecy_surplus1095Xpopalt[[i]]$SE
#   r2_tonnageimputed_fecy_surplus1095Xpopalt[i] <- summary(model)$r.squared[2]
# }

#############################################
#############################################
# Table 1 (Main Text)
#############################################
#############################################

stargazer(model_milex_fecy_surplus1095nocontrols[[1]],
          model_milex_fecy_surplus1095[[1]],
          model_milex_fecy_pop[[1]],
          model_milex_fecy_surplus1095pop[[1]],
          model_milex_fecy_surplus1095Xpop[[1]],
          
          model_tonnageimputed_fecy_surplus1095nocontrols[[1]],
          model_tonnageimputed_fecy_surplus1095[[1]],
          model_tonnageimputed_fecy_pop[[1]],
          model_tonnageimputed_fecy_surplus1095pop[[1]],
          model_tonnageimputed_fecy_surplus1095Xpop[[1]],
          
          # Cluster SES
          se=list(ses_milex_fecy_surplus1095nocontrols[[1]],
                  ses_milex_fecy_surplus1095[[1]],
                  ses_milex_fecy_pop[[1]],
                  ses_milex_fecy_surplus1095pop[[1]],
                  ses_milex_fecy_surplus1095Xpop[[1]],
                  
                  ses_tonnageimputed_fecy_surplus1095nocontrols[[1]],
                  ses_tonnageimputed_fecy_surplus1095[[1]],
                  ses_tonnageimputed_fecy_pop[[1]],
                  ses_tonnageimputed_fecy_surplus1095pop[[1]],
                  ses_tonnageimputed_fecy_surplus1095Xpop[[1]]),
          
          # Formatting
          font.size = "scriptsize",
          digits = 2,
          omit.stat = c("f","ser","rsq"),
          column.sep.width = "-6pt",
          dep.var.labels.include = F,
          star.cutoffs = c(0.05, 0.01, 0.001),
          no.space = TRUE,
          model.names = FALSE,
          
          # Labeling
          dep.var.caption  = "Dependent variables",
          covariate.labels = c("$\\text{Potential threat (SDP)}_{i,t-1}$",
                               "$\\text{Potential threat (Population)}_{i,t-1}$",
                               "$\\text{ln SDP}_{i,t-1}$",
                               "$\\text{ln Subsistence}_{i,t-1}$",
                               "$\\text{Polity2}_{i,t-1}$",
                               "$\\text{Interaction Potential threat}_{i,t-1}$"),
          add.lines = list(c("Fixed-effects", rep("CY", 10))),
          column.separate = c(5, 5),
          column.labels   = c("Military expenditure/SDP", 
                              "Naval tonnage/SDP"),
          title = "Regression models relating different specifications of the potential threat variable to investments in arming and power projection. Power-resources are measured using SDP at a \\$3 per diem subsistence level. The loss of strength gradient is conceptualized as curvilinear using the formula $\\frac{1}{\\log(distance)}$. Interest compatibility based joint democracy using Polity scores.",
          
          #############################################
          # uncomment to save output to working directory
          # type = "html",
          # out = "Table_1.html",
          #############################################
          
          notes = c("Clustered standard errors by country (Satterthwaite correction) in parentheses.",
                    "Potential threat variables are standardized.",
                    "CY denotes two-way fixed effects.",
                    "Period of observation: 1816-2012."))


# #############################################
# # Saving Coefficients
# #############################################
econ_surplus1095 <- data.frame(vars = str_replace_all(vars_comp, "_l1", ""),

                                  unlist(map(coefs_milex_fecy_surplus1095nocontrols, 1)),
                                  unlist(map(coefs_milex_fecy_surplus1095, 1)),
                                  unlist(map(coefs_milex_fecy_surplus1095pop, 1)),
                                  unlist(map(coefs_milex_fecy_surplus1095Xpop, 1)),
                                  unlist(map(ses_milex_fecy_surplus1095nocontrols, 1)),
                                  unlist(map(ses_milex_fecy_surplus1095, 1)),
                                  unlist(map(ses_milex_fecy_surplus1095pop, 1)),
                                  unlist(map(ses_milex_fecy_surplus1095Xpop, 1)),

                                  unlist(map(coefs_tonnageimputed_fecy_surplus1095nocontrols, 1)),
                                  unlist(map(coefs_tonnageimputed_fecy_surplus1095, 1)),
                                  unlist(map(coefs_tonnageimputed_fecy_surplus1095pop, 1)),
                                  unlist(map(coefs_tonnageimputed_fecy_surplus1095Xpop, 1)),
                                  unlist(map(ses_tonnageimputed_fecy_surplus1095nocontrols, 1)),
                                  unlist(map(ses_tonnageimputed_fecy_surplus1095, 1)),
                                  unlist(map(ses_tonnageimputed_fecy_surplus1095pop, 1)),
                                  unlist(map(ses_tonnageimputed_fecy_surplus1095Xpop, 1))) %>%

  gather(indicator, value, -vars) %>%
  mutate(indicator = str_replace_all(indicator, "unlist.map.", ""),
         indicator = str_replace_all(indicator, "..1..", "")) %>%
  separate(indicator, c("indicator", "dv", "model", "controls"), sep = "_") %>%
  spread(indicator, value) %>%
  mutate(lower95 = coefs - 1.96*ses, #95% CI
         lower99 = coefs - 2.58*ses, #99% CI
         upper95 = coefs + 1.96*ses,
         upper99 = coefs + 2.58*ses) %>%
  mutate(controls = str_replace_all(controls, "surpl5", "surplus1095")) %>%
  mutate(coef = "econ",
         type_control = "wsub") 


econ_surplus1095_alt <- data.frame(vars = str_replace_all(vars_comp, "_l1", ""),

                                  unlist(map(coefs_milex_fecy_surplus1095nocontrols, 1)),
                                  unlist(map(coefs_milex_fecy_surplus1095alt, 1)),
                                  unlist(map(coefs_milex_fecy_surplus1095popalt, 1)),
                                  #unlist(map(coefs_milex_fecy_surplus1095Xpopalt, 1)),
                                  unlist(map(ses_milex_fecy_surplus1095nocontrols, 1)),
                                  unlist(map(ses_milex_fecy_surplus1095alt, 1)),
                                  unlist(map(ses_milex_fecy_surplus1095popalt, 1)),
                                 # unlist(map(ses_milex_fecy_surplus1095Xpopalt, 1)),

                                  unlist(map(coefs_tonnageimputed_fecy_surplus1095nocontrols, 1)),
                                  unlist(map(coefs_tonnageimputed_fecy_surplus1095alt, 1)),
                                  unlist(map(coefs_tonnageimputed_fecy_surplus1095popalt, 1)),
                                  #unlist(map(coefs_tonnageimputed_fecy_surplus1095Xpopalt, 1)),
                                  unlist(map(ses_tonnageimputed_fecy_surplus1095nocontrols, 1)),
                                  unlist(map(ses_tonnageimputed_fecy_surplus1095alt, 1)),
                                  unlist(map(ses_tonnageimputed_fecy_surplus1095popalt, 1))) %>%
                                  #unlist(map(ses_tonnageimputed_fecy_surplus1095Xpopalt, 1))

  gather(indicator, value, -vars) %>%
  mutate(indicator = str_replace_all(indicator, "unlist.map.", ""),
         indicator = str_replace_all(indicator, "..1..", "")) %>%
  separate(indicator, c("indicator", "dv", "model", "controls"), sep = "_") %>%
  spread(indicator, value) %>%
  mutate(lower95 = coefs - 1.96*ses, #95% CI
         lower99 = coefs - 2.58*ses, #99% CI
         upper95 = coefs + 1.96*ses,
         upper99 = coefs + 2.58*ses) %>%
  mutate(controls = str_replace_all(controls, "surpl5", "surplus1095")) %>%
  mutate(coef = "econ",
         type_control = "wosub")

######################
#
# ********************
#
# $2/day Surplus 
#
# ********************
#
######################

############## Setting up variables ##################
# Main variables
vars_comp <- c("scaled_comp_surplus730_polity_invlog_nodenom_all_l1",
               "scaled_comp_surplus730_boix_invlog_nodenom_all_l1",
               "scaled_comp_surplus730_uds_invlog_nodenom_all_l1",
               "scaled_comp_surplus730_riv_invlog_nodenom_all_l1",
               "scaled_comp_surplus730_defense_invlog_nodenom_all_l1",
               "scaled_comp_surplus730_sscores_invlog_nodenom_all_l1",
               "scaled_comp_surplus730_absidealdiff_invlog_nodenom_all_l1",
               "scaled_comp_surplus730_igo_invlog_nodenom_all_l1",
               "scaled_comp_surplus730_diplo_invlog_nodenom_all_l1",
               "scaled_comp_surplus730_bitrade_invlog_nodenom_all_l1",
               "scaled_comp_surplus730_nopreference_invlog_nodenom_all_l1",
               "scaled_comp_surplus730_pecpc_invlog_nodenom_all_l1",
               "scaled_comp_surplus730_atopkappa_invlog_nodenom_all_l1",
               "scaled_comp_surplus730_atopbin_invlog_nodenom_all_l1")

varlabels_comp <- c("$\\text{PT SDP Joint Democracy (Polity)}_{i,t-1}$",
                    "$\\text{PT SDP Joint Democracy (Boix et al.)}_{i,t-1}$",
                    "$\\text{PT SDP Joint Democracy (UDS)}_{i,t-1}$",
                    "$\\text{PT SDP Rivalry}_{i,t-1}$",
                    "$\\text{PT SDP Defense Pacts}_{i,t-1}$",
                    "$\\text{PT SDP S-Scores}_{i,t-1}$",
                    "$\\text{PT SDP Ideal Difference}_{i,t-1}$",
                    "$\\text{PT SDP IGO Membership}_{i,t-1}$",
                    "$\\text{PT SDP Diplo. Exchange}_{i,t-1}$",
                    "$\\text{PT SDP Bilateral Trade}_{i,t-1}$",
                    "$\\text{PT SDP No Interest Variable}_{i,t-1}$",
                    "$\\text{PT SDP Energy consumption}_{i,t-1}$",
                    "$\\text{PT SDP Alliances (continuous)}_{i,t-1}$",
                    "$\\text{PT SDP Alliances (binary)}_{i,t-1}$")

# Controls
# SDP
controlval_surplus730_fe <- c("ln_gdp_surplus730_truncatedone_l1",
                              "ln_subsistence730_l1",
                              "polity2_l1")

controlvalalt_surplus730_fe <- c("ln_gdp_surplus730_truncatedone_l1",
                                 "polity2_l1")

controllab_surplus730_fe <- c("$\\text{ln SDP}_{i,t-1}$",
                              "$\\text{ln Subsistence \\$2}_{i,t-1}$",
                              "$\\text{Polity2}_{i,t-1}$")

controllabalt_surplus730_fe <- c("$\\text{ln SDP}_{i,t-1}$",
                                 "$\\text{Polity2}_{i,t-1}$")


dvval_surplus730_milex <- c("ln_milex_surplus730")

dvval_surplus730_tonnageimputed <- c("ln_tonnageimputed_surplus730")

dvlab_surplus730_milex <- c("$\\ln\\text{ Military Expenditure/SDP}_{i,t}$")

dvlab_surplus730_tonnageimputed <- c("$\\ln\\text{ Naval Tonnage/SDP}_{i,t}$")

######################################
# Running $2 SDP models
######################################

#---------------------
# milex
#---------------------

### surplus730
# FECY model, no controls
model_milex_fecy_surplus730nocontrols <- list()
modelfits_milex_fecy_surplus730nocontrols <- list()
coefs_milex_fecy_surplus730nocontrols <- c()
ses_milex_fecy_surplus730nocontrols <- list()
r2_milex_fecy_surplus730nocontrols <- c()
for(i in 1) {
  modformula <- paste(dvval_surplus730_milex, 
                      vars_comp[i],
                      sep = "~")
  model <- plm(as.formula(modformula), data = master_tscs, model = "within", effect = "twoways")
  model_milex_fecy_surplus730nocontrols[[i]] <- model
  modelfits_milex_fecy_surplus730nocontrols[[i]] <- coef_test(model, vcov = "CR2", cluster = "individual", test = "Satterthwaite")
  coefs_milex_fecy_surplus730nocontrols[[i]] <- modelfits_milex_fecy_surplus730nocontrols[[i]]$beta
  ses_milex_fecy_surplus730nocontrols[[i]] <- modelfits_milex_fecy_surplus730nocontrols[[i]]$SE
  r2_milex_fecy_surplus730nocontrols[i] <- summary(model)$r.squared[2]
}


### surplus730
# FECY model, controls
model_milex_fecy_surplus730 <- list()
modelfits_milex_fecy_surplus730 <- list()
coefs_milex_fecy_surplus730 <- c()
ses_milex_fecy_surplus730 <- list()
r2_milex_fecy_surplus730 <- c()
for(i in 1) {
  modformula <- paste(dvval_surplus730_milex, 
                      paste(vars_comp[i], 
                            controlval_surplus730_fe[1], 
                            controlval_surplus730_fe[2],
                            controlval_surplus730_fe[3], sep = "+"), 
                      sep = "~")
  model <- plm(as.formula(modformula), data = master_tscs, model = "within", effect = "twoways")
  model_milex_fecy_surplus730[[i]] <- model
  modelfits_milex_fecy_surplus730[[i]] <- coef_test(model, vcov = "CR2", cluster = "individual", test = "Satterthwaite")
  coefs_milex_fecy_surplus730[[i]] <- modelfits_milex_fecy_surplus730[[i]]$beta
  ses_milex_fecy_surplus730[[i]] <- modelfits_milex_fecy_surplus730[[i]]$SE
  r2_milex_fecy_surplus730[i] <- summary(model)$r.squared[2]
}


### surplus730
# FECY model, controls alternative
model_milex_fecy_surplus730alt <- list()
modelfits_milex_fecy_surplus730alt <- list()
coefs_milex_fecy_surplus730alt <- c()
ses_milex_fecy_surplus730alt <- list()
r2_milex_fecy_surplus730alt <- c()
for(i in 1) {
  modformula <- paste(dvval_surplus730_milex, 
                      paste(vars_comp[i], 
                            controlvalalt_surplus730_fe[1], 
                            controlvalalt_surplus730_fe[2], sep = "+"), 
                      sep = "~")
  model <- plm(as.formula(modformula), data = master_tscs, model = "within", effect = "twoways")
  model_milex_fecy_surplus730alt[[i]] <- model
  modelfits_milex_fecy_surplus730alt[[i]] <- coef_test(model, vcov = "CR2", cluster = "individual", test = "Satterthwaite")
  coefs_milex_fecy_surplus730alt[[i]] <- modelfits_milex_fecy_surplus730alt[[i]]$beta
  ses_milex_fecy_surplus730alt[[i]] <- modelfits_milex_fecy_surplus730alt[[i]]$SE
  r2_milex_fecy_surplus730alt[i] <- summary(model)$r.squared[2]
}


# 
# ### pop
# # FECY model, controls
# model_milex_fecy_pop <- list()
# modelfits_milex_fecy_pop <- list()
# coefs_milex_fecy_pop <- c()
# ses_milex_fecy_pop <- list()
# r2_milex_fecy_pop <- c()
# for(i in 1) {
#   modformula <- paste(dvval_surplus730_milex, 
#                       paste(vars_comppop[i], 
#                             controlval_surplus730_fe[1], 
#                             controlval_surplus730_fe[2],
#                             controlval_surplus730_fe[3], sep = "+"), 
#                       sep = "~")
#   model <- plm(as.formula(modformula), data = master_tscs, model = "within", effect = "twoways")
#   model_milex_fecy_pop[[i]] <- model
#   modelfits_milex_fecy_pop[[i]] <- coef_test(model, vcov = "CR2", cluster = "individual", test = "Satterthwaite")
#   coefs_milex_fecy_pop[[i]] <- modelfits_milex_fecy_pop[[i]]$beta
#   ses_milex_fecy_pop[[i]] <- modelfits_milex_fecy_pop[[i]]$SE
#   r2_milex_fecy_pop[i] <- summary(model)$r.squared[2]
# }
# 
# 
# ### pop
# # FECY model, controls alternative
# model_milex_fecy_popalt <- list()
# modelfits_milex_fecy_popalt <- list()
# coefs_milex_fecy_popalt <- c()
# ses_milex_fecy_popalt <- list()
# r2_milex_fecy_popalt <- c()
# for(i in 1) {
#   modformula <- paste(dvval_surplus730_milex, 
#                       paste(vars_comppop[i], 
#                             controlvalalt_surplus730_fe[1], 
#                             controlvalalt_surplus730_fe[2], sep = "+"), 
#                       sep = "~")
#   model <- plm(as.formula(modformula), data = master_tscs, model = "within", effect = "twoways")
#   model_milex_fecy_popalt[[i]] <- model
#   modelfits_milex_fecy_popalt[[i]] <- coef_test(model, vcov = "CR2", cluster = "individual", test = "Satterthwaite")
#   coefs_milex_fecy_popalt[[i]] <- modelfits_milex_fecy_popalt[[i]]$beta
#   ses_milex_fecy_popalt[[i]] <- modelfits_milex_fecy_popalt[[i]]$SE
#   r2_milex_fecy_popalt[i] <- summary(model)$r.squared[2]
# }


### surplus730, pop
# FECY model, controls
model_milex_fecy_surplus730pop <- list()
modelfits_milex_fecy_surplus730pop <- list()
coefs_milex_fecy_surplus730pop <- c()
ses_milex_fecy_surplus730pop <- c()
r2_milex_fecy_surplus730pop <- c()
for(i in 1:length(vars_comp)) {
  modformula <- paste(dvval_surplus730_milex, 
                      paste(vars_comp[i], 
                            vars_comppop[i], 
                            controlval_surplus730_fe[1], 
                            controlval_surplus730_fe[2],
                            controlval_surplus730_fe[3], sep = "+"), 
                      sep = "~")
  model <- plm(as.formula(modformula), data = master_tscs, model = "within", effect = "twoways")
  model_milex_fecy_surplus730pop[[i]] <- model
  modelfits_milex_fecy_surplus730pop[[i]] <- coef_test(model, vcov = "CR2", cluster = "individual", test = "Satterthwaite")
  coefs_milex_fecy_surplus730pop[[i]] <- modelfits_milex_fecy_surplus730pop[[i]]$beta
  ses_milex_fecy_surplus730pop[[i]] <- modelfits_milex_fecy_surplus730pop[[i]]$SE
  r2_milex_fecy_surplus730pop[i] <- summary(model)$r.squared[2]
}


### surplus730, pop
# FECY model, controls alternative
model_milex_fecy_surplus730popalt <- list()
modelfits_milex_fecy_surplus730popalt <- list()
coefs_milex_fecy_surplus730popalt <- c()
ses_milex_fecy_surplus730popalt <- c()
r2_milex_fecy_surplus730popalt <- c()
for(i in 1:length(vars_comp)) {
  modformula <- paste(dvval_surplus730_milex, 
                      paste(vars_comp[i], 
                            vars_comppop[i], 
                            controlvalalt_surplus730_fe[1], 
                            controlvalalt_surplus730_fe[2], sep = "+"), 
                      sep = "~")
  model <- plm(as.formula(modformula), data = master_tscs, model = "within", effect = "twoways")
  model_milex_fecy_surplus730popalt[[i]] <- model
  modelfits_milex_fecy_surplus730popalt[[i]] <- coef_test(model, vcov = "CR2", cluster = "individual", test = "Satterthwaite")
  coefs_milex_fecy_surplus730popalt[[i]] <- modelfits_milex_fecy_surplus730popalt[[i]]$beta
  ses_milex_fecy_surplus730popalt[[i]] <- modelfits_milex_fecy_surplus730popalt[[i]]$SE
  r2_milex_fecy_surplus730popalt[i] <- summary(model)$r.squared[2]
}



# ### surplus730 interaction pop
# # FECY model, controls
# model_milex_fecy_surplus730Xpop <- list()
# modelfits_milex_fecy_surplus730Xpop <- list()
# coefs_milex_fecy_surplus730Xpop <- c()
# ses_milex_fecy_surplus730Xpop <- c()
# r2_milex_fecy_surplus730Xpop <- c()
# for(i in 1) {
#   modformula <- paste(dvval_surplus730_milex, 
#                       paste(paste(vars_comp[i], vars_comppop[i], sep = "*"), 
#                             controlval_surplus730_fe[1], 
#                             controlval_surplus730_fe[2],
#                             controlval_surplus730_fe[3], sep = "+"), 
#                       sep = "~")
#   model <- plm(as.formula(modformula), data = master_tscs, model = "within", effect = "twoways")
#   model_milex_fecy_surplus730Xpop[[i]] <- model
#   modelfits_milex_fecy_surplus730Xpop[[i]] <- coef_test(model, vcov = "CR2", cluster = "individual", test = "Satterthwaite")
#   coefs_milex_fecy_surplus730Xpop[[i]] <- modelfits_milex_fecy_surplus730Xpop[[i]]$beta
#   ses_milex_fecy_surplus730Xpop[[i]] <- modelfits_milex_fecy_surplus730Xpop[[i]]$SE
#   r2_milex_fecy_surplus730Xpop[i] <- summary(model)$r.squared[2]
# }


# ### surplus730 interaction pop
# # FECY model, controls alternative
# model_milex_fecy_surplus730Xpopalt <- list()
# modelfits_milex_fecy_surplus730Xpopalt <- list()
# coefs_milex_fecy_surplus730Xpopalt <- c()
# ses_milex_fecy_surplus730Xpopalt <- c()
# r2_milex_fecy_surplus730Xpopalt <- c()
# for(i in 1:length(vars_comp)) {
#   modformula <- paste(dvval_surplus730_milex, 
#                       paste(paste(vars_comp[i], vars_comppop[i], sep = "*"), 
#                             controlvalalt_surplus730_fe[1], 
#                             controlvalalt_surplus730_fe[2], sep = "+"), 
#                       sep = "~")
#   model <- plm(as.formula(modformula), data = master_tscs, model = "within", effect = "twoways")
#   model_milex_fecy_surplus730Xpopalt[[i]] <- model
#   modelfits_milex_fecy_surplus730Xpopalt[[i]] <- coef_test(model, vcov = "CR2", cluster = "individual", test = "Satterthwaite")
#   coefs_milex_fecy_surplus730Xpopalt[[i]] <- modelfits_milex_fecy_surplus730Xpopalt[[i]]$beta
#   ses_milex_fecy_surplus730Xpopalt[[i]] <- modelfits_milex_fecy_surplus730Xpopalt[[i]]$SE
#   r2_milex_fecy_surplus730Xpopalt[i] <- summary(model)$r.squared[2]
# }


#---------------------
# tonnageimputed
#---------------------

### surplus730
# FECY model, no controls
model_tonnageimputed_fecy_surplus730nocontrols <- list()
modelfits_tonnageimputed_fecy_surplus730nocontrols <- list()
coefs_tonnageimputed_fecy_surplus730nocontrols <- c()
ses_tonnageimputed_fecy_surplus730nocontrols <- c()
r2_tonnageimputed_fecy_surplus730nocontrols <- c()
for(i in 1) {
  modformula <- paste(dvval_surplus730_tonnageimputed, 
                      vars_comp[i], 
                      sep = "~")
  model <- plm(as.formula(modformula), data = master_tscs, model = "within", effect = "twoways")
  model_tonnageimputed_fecy_surplus730nocontrols[[i]] <- model
  modelfits_tonnageimputed_fecy_surplus730nocontrols[[i]] <- coef_test(model, vcov = "CR2", cluster = "individual", test = "Satterthwaite")
  coefs_tonnageimputed_fecy_surplus730nocontrols[[i]] <- modelfits_tonnageimputed_fecy_surplus730nocontrols[[i]]$beta
  ses_tonnageimputed_fecy_surplus730nocontrols[[i]] <- modelfits_tonnageimputed_fecy_surplus730nocontrols[[i]]$SE
  r2_tonnageimputed_fecy_surplus730nocontrols[i] <- summary(model)$r.squared[2]
}


### surplus730
# FECY model, controls
model_tonnageimputed_fecy_surplus730 <- list()
modelfits_tonnageimputed_fecy_surplus730 <- list()
coefs_tonnageimputed_fecy_surplus730 <- c()
ses_tonnageimputed_fecy_surplus730 <- c()
r2_tonnageimputed_fecy_surplus730 <- c()
for(i in 1) {
  modformula <- paste(dvval_surplus730_tonnageimputed, 
                      paste(vars_comp[i], 
                            controlval_surplus730_fe[1], 
                            controlval_surplus730_fe[2],
                            controlval_surplus730_fe[3], sep = "+"), 
                      sep = "~")
  model <- plm(as.formula(modformula), data = master_tscs, model = "within", effect = "twoways")
  model_tonnageimputed_fecy_surplus730[[i]] <- model
  modelfits_tonnageimputed_fecy_surplus730[[i]] <- coef_test(model, vcov = "CR2", cluster = "individual", test = "Satterthwaite")
  coefs_tonnageimputed_fecy_surplus730[[i]] <- modelfits_tonnageimputed_fecy_surplus730[[i]]$beta
  ses_tonnageimputed_fecy_surplus730[[i]] <- modelfits_tonnageimputed_fecy_surplus730[[i]]$SE
  r2_tonnageimputed_fecy_surplus730[i] <- summary(model)$r.squared[2]
}


# FECY model, controls alternative
model_tonnageimputed_fecy_surplus730alt <- list()
modelfits_tonnageimputed_fecy_surplus730alt <- list()
coefs_tonnageimputed_fecy_surplus730alt <- c()
ses_tonnageimputed_fecy_surplus730alt <- c()
r2_tonnageimputed_fecy_surplus730alt <- c()
for(i in 1) {
  modformula <- paste(dvval_surplus730_tonnageimputed, 
                      paste(vars_comp[i], 
                            controlvalalt_surplus730_fe[1], 
                            controlvalalt_surplus730_fe[2], sep = "+"), 
                      sep = "~")
  model <- plm(as.formula(modformula), data = master_tscs, model = "within", effect = "twoways")
  model_tonnageimputed_fecy_surplus730alt[[i]] <- model
  modelfits_tonnageimputed_fecy_surplus730alt[[i]] <- coef_test(model, vcov = "CR2", cluster = "individual", test = "Satterthwaite")
  coefs_tonnageimputed_fecy_surplus730alt[[i]] <- modelfits_tonnageimputed_fecy_surplus730alt[[i]]$beta
  ses_tonnageimputed_fecy_surplus730alt[[i]] <- modelfits_tonnageimputed_fecy_surplus730alt[[i]]$SE
  r2_tonnageimputed_fecy_surplus730alt[i] <- summary(model)$r.squared[2]
}



# ### pop
# # FECY model, controls
# model_tonnageimputed_fecy_pop <- list()
# modelfits_tonnageimputed_fecy_pop <- list()
# coefs_tonnageimputed_fecy_pop <- c()
# ses_tonnageimputed_fecy_pop <- c()
# r2_tonnageimputed_fecy_pop <- c()
# for(i in 1) {
#   modformula <- paste(dvval_surplus730_tonnageimputed, 
#                       paste(vars_comppop[i], 
#                             controlval_surplus730_fe[1], 
#                             controlval_surplus730_fe[2],
#                             controlval_surplus730_fe[3], sep = "+"), 
#                       sep = "~")
#   model <- plm(as.formula(modformula), data = master_tscs, model = "within", effect = "twoways")
#   model_tonnageimputed_fecy_pop[[i]] <- model
#   modelfits_tonnageimputed_fecy_pop[[i]] <- coef_test(model, vcov = "CR2", cluster = "individual", test = "Satterthwaite")
#   coefs_tonnageimputed_fecy_pop[[i]] <- modelfits_tonnageimputed_fecy_pop[[i]]$beta
#   ses_tonnageimputed_fecy_pop[[i]] <- modelfits_tonnageimputed_fecy_pop[[i]]$SE
#   r2_tonnageimputed_fecy_pop[i] <- summary(model)$r.squared[2]
# }
# 
# # FECY model, controls alternative
# model_tonnageimputed_fecy_popalt <- list()
# modelfits_tonnageimputed_fecy_popalt <- list()
# coefs_tonnageimputed_fecy_popalt <- c()
# ses_tonnageimputed_fecy_popalt <- c()
# r2_tonnageimputed_fecy_popalt <- c()
# for(i in 1) {
#   modformula <- paste(dvval_surplus730_tonnageimputed, 
#                       paste(vars_comppop[i], 
#                             controlvalalt_surplus730_fe[1], 
#                             controlvalalt_surplus730_fe[2], sep = "+"), 
#                       sep = "~")
#   model <- plm(as.formula(modformula), data = master_tscs, model = "within", effect = "twoways")
#   model_tonnageimputed_fecy_popalt[[i]] <- model
#   modelfits_tonnageimputed_fecy_popalt[[i]] <- coef_test(model, vcov = "CR2", cluster = "individual", test = "Satterthwaite")
#   coefs_tonnageimputed_fecy_popalt[[i]] <- modelfits_tonnageimputed_fecy_popalt[[i]]$beta
#   ses_tonnageimputed_fecy_popalt[[i]] <- modelfits_tonnageimputed_fecy_popalt[[i]]$SE
#   r2_tonnageimputed_fecy_popalt[i] <- summary(model)$r.squared[2]
# }


### surplus730, pop
# FECY model, controls
model_tonnageimputed_fecy_surplus730pop <- list()
modelfits_tonnageimputed_fecy_surplus730pop <- list()
coefs_tonnageimputed_fecy_surplus730pop <- c()
ses_tonnageimputed_fecy_surplus730pop <- c()
r2_tonnageimputed_fecy_surplus730pop <- c()
for(i in 1:length(vars_comp)) {
  modformula <- paste(dvval_surplus730_tonnageimputed, 
                      paste(vars_comp[i], 
                            vars_comppop[i], 
                            controlval_surplus730_fe[1], 
                            controlval_surplus730_fe[2],
                            controlval_surplus730_fe[3], sep = "+"), 
                      sep = "~")
  model <- plm(as.formula(modformula), data = master_tscs, model = "within", effect = "twoways")
  model_tonnageimputed_fecy_surplus730pop[[i]] <- model
  modelfits_tonnageimputed_fecy_surplus730pop[[i]] <- coef_test(model, vcov = "CR2", cluster = "individual", test = "Satterthwaite")
  coefs_tonnageimputed_fecy_surplus730pop[[i]] <- modelfits_tonnageimputed_fecy_surplus730pop[[i]]$beta
  ses_tonnageimputed_fecy_surplus730pop[[i]] <- modelfits_tonnageimputed_fecy_surplus730pop[[i]]$SE
  r2_tonnageimputed_fecy_surplus730pop[i] <- summary(model)$r.squared[2]
}

# FECY model, controls alternative
model_tonnageimputed_fecy_surplus730popalt <- list()
modelfits_tonnageimputed_fecy_surplus730popalt <- list()
coefs_tonnageimputed_fecy_surplus730popalt <- c()
ses_tonnageimputed_fecy_surplus730popalt <- c()
r2_tonnageimputed_fecy_surplus730popalt <- c()
for(i in 1:length(vars_comp)) {
  modformula <- paste(dvval_surplus730_tonnageimputed, 
                      paste(vars_comp[i], 
                            vars_comppop[i], 
                            controlvalalt_surplus730_fe[1], 
                            controlvalalt_surplus730_fe[2], sep = "+"), 
                      sep = "~")
  model <- plm(as.formula(modformula), data = master_tscs, model = "within", effect = "twoways")
  model_tonnageimputed_fecy_surplus730popalt[[i]] <- model
  modelfits_tonnageimputed_fecy_surplus730popalt[[i]] <- coef_test(model, vcov = "CR2", cluster = "individual", test = "Satterthwaite")
  coefs_tonnageimputed_fecy_surplus730popalt[[i]] <- modelfits_tonnageimputed_fecy_surplus730popalt[[i]]$beta
  ses_tonnageimputed_fecy_surplus730popalt[[i]] <- modelfits_tonnageimputed_fecy_surplus730popalt[[i]]$SE
  r2_tonnageimputed_fecy_surplus730popalt[i] <- summary(model)$r.squared[2]
}


# ### surplus730 interaction pop
# # FECY model, controls
# model_tonnageimputed_fecy_surplus730Xpop <- list()
# modelfits_tonnageimputed_fecy_surplus730Xpop <- list()
# coefs_tonnageimputed_fecy_surplus730Xpop <- c()
# ses_tonnageimputed_fecy_surplus730Xpop <- c()
# r2_tonnageimputed_fecy_surplus730Xpop <- c()
# for(i in 1) {
#   modformula <- paste(dvval_surplus730_tonnageimputed, 
#                       paste(paste(vars_comp[i], vars_comppop[i], sep = "*"), 
#                             controlval_surplus730_fe[1], 
#                             controlval_surplus730_fe[2],
#                             controlval_surplus730_fe[3], sep = "+"), 
#                       sep = "~")
#   model <- plm(as.formula(modformula), data = master_tscs, model = "within", effect = "twoways")
#   model_tonnageimputed_fecy_surplus730Xpop[[i]] <- model
#   modelfits_tonnageimputed_fecy_surplus730Xpop[[i]] <- coef_test(model, vcov = "CR2", cluster = "individual", test = "Satterthwaite")
#   coefs_tonnageimputed_fecy_surplus730Xpop[[i]] <- modelfits_tonnageimputed_fecy_surplus730Xpop[[i]]$beta
#   ses_tonnageimputed_fecy_surplus730Xpop[[i]] <- modelfits_tonnageimputed_fecy_surplus730Xpop[[i]]$SE
#   r2_tonnageimputed_fecy_surplus730Xpop[i] <- summary(model)$r.squared[2]
# }

# 
# # FECY model, controls alternative
# model_tonnageimputed_fecy_surplus730Xpopalt <- list()
# modelfits_tonnageimputed_fecy_surplus730Xpopalt <- list()
# coefs_tonnageimputed_fecy_surplus730Xpopalt <- c()
# ses_tonnageimputed_fecy_surplus730Xpopalt <- c()
# r2_tonnageimputed_fecy_surplus730Xpopalt <- c()
# for(i in 1:length(vars_comp)) {
#   modformula <- paste(dvval_surplus730_tonnageimputed, 
#                       paste(paste(vars_comp[i], vars_comppop[i], sep = "*"), 
#                             controlvalalt_surplus730_fe[1], 
#                             controlvalalt_surplus730_fe[2], sep = "+"), 
#                       sep = "~")
#   model <- plm(as.formula(modformula), data = master_tscs, model = "within", effect = "twoways")
#   model_tonnageimputed_fecy_surplus730Xpopalt[[i]] <- model
#   modelfits_tonnageimputed_fecy_surplus730Xpopalt[[i]] <- coef_test(model, vcov = "CR2", cluster = "individual", test = "Satterthwaite")
#   coefs_tonnageimputed_fecy_surplus730Xpopalt[[i]] <- modelfits_tonnageimputed_fecy_surplus730Xpopalt[[i]]$beta
#   ses_tonnageimputed_fecy_surplus730Xpopalt[[i]] <- modelfits_tonnageimputed_fecy_surplus730Xpopalt[[i]]$SE
#   r2_tonnageimputed_fecy_surplus730Xpopalt[i] <- summary(model)$r.squared[2]
# }

#############################################
# Saving Coefficients
#############################################
econ_surplus730 <- data.frame(vars = str_replace_all(vars_comp, "_l1", ""),
                                 
                                 unlist(map(coefs_milex_fecy_surplus730nocontrols, 1)),
                                 unlist(map(coefs_milex_fecy_surplus730, 1)),
                                 unlist(map(coefs_milex_fecy_surplus730pop, 1)),
                                 #unlist(map(coefs_milex_fecy_surplus730Xpop, 1)),
                                 unlist(map(ses_milex_fecy_surplus730nocontrols, 1)),
                                 unlist(map(ses_milex_fecy_surplus730, 1)),
                                 unlist(map(ses_milex_fecy_surplus730pop, 1)),
                                 #unlist(map(ses_milex_fecy_surplus730Xpop, 1)),
                                 
                                 unlist(map(coefs_tonnageimputed_fecy_surplus730nocontrols, 1)),
                                 unlist(map(coefs_tonnageimputed_fecy_surplus730, 1)),
                                 unlist(map(coefs_tonnageimputed_fecy_surplus730pop, 1)),
                                 #unlist(map(coefs_tonnageimputed_fecy_surplus730Xpop, 1)),
                                 unlist(map(ses_tonnageimputed_fecy_surplus730nocontrols, 1)),
                                 unlist(map(ses_tonnageimputed_fecy_surplus730, 1)),
                                 unlist(map(ses_tonnageimputed_fecy_surplus730pop, 1))) %>%
                                 #unlist(map(ses_tonnageimputed_fecy_surplus730Xpop, 1))) %>%
  
  gather(indicator, value, -vars) %>%
  mutate(indicator = str_replace_all(indicator, "unlist.map.", ""),
         indicator = str_replace_all(indicator, "..1..", "")) %>%
  separate(indicator, c("indicator", "dv", "model", "controls"), sep = "_") %>%
  spread(indicator, value) %>%
  mutate(lower95 = coefs - 1.96*ses, #95% CI
         lower99 = coefs - 2.58*ses, #99% CI
         upper95 = coefs + 1.96*ses,
         upper99 = coefs + 2.58*ses) %>%
  mutate(coef = "econ", type_control = "wsub")

econ_surplus730_alt <- data.frame(vars = str_replace_all(vars_comp, "_l1", ""),
                                    
                                    unlist(map(coefs_milex_fecy_surplus730nocontrols, 1)),
                                    unlist(map(coefs_milex_fecy_surplus730alt, 1)),
                                    unlist(map(coefs_milex_fecy_surplus730popalt, 1)),
                                    #unlist(map(coefs_milex_fecy_surplus730Xpopalt, 1)),
                                    unlist(map(ses_milex_fecy_surplus730nocontrols, 1)),
                                    unlist(map(ses_milex_fecy_surplus730alt, 1)),
                                    unlist(map(ses_milex_fecy_surplus730popalt, 1)),
                                    #unlist(map(ses_milex_fecy_surplus730Xpopalt, 1)),
                                    
                                    unlist(map(coefs_tonnageimputed_fecy_surplus730nocontrols, 1)),
                                    unlist(map(coefs_tonnageimputed_fecy_surplus730alt, 1)),
                                    unlist(map(coefs_tonnageimputed_fecy_surplus730popalt, 1)),
                                    #unlist(map(coefs_tonnageimputed_fecy_surplus730Xpopalt, 1)),
                                    unlist(map(ses_tonnageimputed_fecy_surplus730nocontrols, 1)),
                                    unlist(map(ses_tonnageimputed_fecy_surplus730alt, 1)),
                                    unlist(map(ses_tonnageimputed_fecy_surplus730popalt, 1))) %>%
                                    #unlist(map(ses_tonnageimputed_fecy_surplus730Xpopalt, 1))) %>%
  
  gather(indicator, value, -vars) %>%
  mutate(indicator = str_replace_all(indicator, "unlist.map.", ""),
         indicator = str_replace_all(indicator, "..1..", "")) %>%
  separate(indicator, c("indicator", "dv", "model", "controls"), sep = "_") %>%
  spread(indicator, value) %>%
  mutate(lower95 = coefs - 1.96*ses, #95% CI
         lower99 = coefs - 2.58*ses, #99% CI
         upper95 = coefs + 1.96*ses,
         upper99 = coefs + 2.58*ses) %>%
  mutate(coef = "econ", type_control = "wosub")


######################
#
# ********************
#
# $1/day Surplus
#
# ********************
#
######################

############## Setting up variables ##################
# Main variables
vars_comp <- c("scaled_comp_surplus365_polity_invlog_nodenom_all_l1",
               "scaled_comp_surplus365_boix_invlog_nodenom_all_l1",
               "scaled_comp_surplus365_uds_invlog_nodenom_all_l1",
               "scaled_comp_surplus365_riv_invlog_nodenom_all_l1",
               "scaled_comp_surplus365_defense_invlog_nodenom_all_l1",
               "scaled_comp_surplus365_sscores_invlog_nodenom_all_l1",
               "scaled_comp_surplus365_absidealdiff_invlog_nodenom_all_l1",
               "scaled_comp_surplus365_igo_invlog_nodenom_all_l1",
               "scaled_comp_surplus365_diplo_invlog_nodenom_all_l1",
               "scaled_comp_surplus365_bitrade_invlog_nodenom_all_l1",
               "scaled_comp_surplus365_nopreference_invlog_nodenom_all_l1",
               "scaled_comp_surplus365_pecpc_invlog_nodenom_all_l1",
               "scaled_comp_surplus365_atopkappa_invlog_nodenom_all_l1",
               "scaled_comp_surplus365_atopbin_invlog_nodenom_all_l1")

varlabels_comp <- c("$\\text{PT SDP Joint Democracy (Polity)}_{i,t-1}$",
                    "$\\text{PT SDP Joint Democracy (Boix et al.)}_{i,t-1}$",
                    "$\\text{PT SDP Joint Democracy (UDS)}_{i,t-1}$",
                    "$\\text{PT SDP Rivalry}_{i,t-1}$",
                    "$\\text{PT SDP Defense Pacts}_{i,t-1}$",
                    "$\\text{PT SDP S-Scores}_{i,t-1}$",
                    "$\\text{PT SDP Ideal Difference}_{i,t-1}$",
                    "$\\text{PT SDP IGO Membership}_{i,t-1}$",
                    "$\\text{PT SDP Diplo. Exchange}_{i,t-1}$",
                    "$\\text{PT SDP Bilateral Trade}_{i,t-1}$",
                    "$\\text{PT SDP No Interest Variable}_{i,t-1}$",
                    "$\\text{PT SDP Energy consumption}_{i,t-1}$",
                    "$\\text{PT SDP Alliances (continuous)}_{i,t-1}$",
                    "$\\text{PT SDP Alliances (binary)}_{i,t-1}$")

## Controls 
# SDP
controlval_surplus365_fe <- c("ln_gdp_surplus365_truncatedone_l1",
                              "ln_subsistence365_l1",
                              "polity2_l1")

controlvalalt_surplus365_fe <- c("ln_gdp_surplus365_truncatedone_l1",
                                 "polity2_l1")

controllab_surplus365_fe <- c("$\\text{ln SDP}_{i,t-1}$",
                              "$\\text{ln Subsistence \\$1}_{i,t-1}$",
                              "$\\text{Polity2}_{i,t-1}$")

controllabalt_surplus365_fe <- c("$\\text{ln SDP}_{i,t-1}$",
                                 "$\\text{Polity2}_{i,t-1}$")


dvval_surplus365_milex <- c("ln_milex_surplus365")

dvval_surplus365_tonnageimputed <- c("ln_tonnageimputed_surplus365")

dvlab_surplus365_milex <- c("$\\ln\\text{ Military Expenditure/SDP}_{i,t}$")

dvlab_surplus365_tonnageimputed <- c("$\\ln\\text{ Naval Tonnage/SDP}_{i,t}$")


#---------------------
# milex
#---------------------

### surplus365
# FECY model, no controls
model_milex_fecy_surplus365nocontrols <- list()
modelfits_milex_fecy_surplus365nocontrols <- list()
coefs_milex_fecy_surplus365nocontrols <- c()
ses_milex_fecy_surplus365nocontrols <- list()
r2_milex_fecy_surplus365nocontrols <- c()
for(i in 1) {
  modformula <- paste(dvval_surplus365_milex, 
                      vars_comp[i],
                      sep = "~")
  model <- plm(as.formula(modformula), data = master_tscs, model = "within", effect = "twoways")
  model_milex_fecy_surplus365nocontrols[[i]] <- model
  modelfits_milex_fecy_surplus365nocontrols[[i]] <- coef_test(model, vcov = "CR2", cluster = "individual", test = "Satterthwaite")
  coefs_milex_fecy_surplus365nocontrols[[i]] <- modelfits_milex_fecy_surplus365nocontrols[[i]]$beta
  ses_milex_fecy_surplus365nocontrols[[i]] <- modelfits_milex_fecy_surplus365nocontrols[[i]]$SE
  r2_milex_fecy_surplus365nocontrols[i] <- summary(model)$r.squared[2]
}


### surplus365
# FECY model, controls
model_milex_fecy_surplus365 <- list()
modelfits_milex_fecy_surplus365 <- list()
coefs_milex_fecy_surplus365 <- c()
ses_milex_fecy_surplus365 <- list()
r2_milex_fecy_surplus365 <- c()
for(i in 1) {
  modformula <- paste(dvval_surplus365_milex, 
                      paste(vars_comp[i], 
                            controlval_surplus365_fe[1], 
                            controlval_surplus365_fe[2],
                            controlval_surplus365_fe[3], sep = "+"), 
                      sep = "~")
  model <- plm(as.formula(modformula), data = master_tscs, model = "within", effect = "twoways")
  model_milex_fecy_surplus365[[i]] <- model
  modelfits_milex_fecy_surplus365[[i]] <- coef_test(model, vcov = "CR2", cluster = "individual", test = "Satterthwaite")
  coefs_milex_fecy_surplus365[[i]] <- modelfits_milex_fecy_surplus365[[i]]$beta
  ses_milex_fecy_surplus365[[i]] <- modelfits_milex_fecy_surplus365[[i]]$SE
  r2_milex_fecy_surplus365[i] <- summary(model)$r.squared[2]
}


### surplus365
# FECY model, controls alternative
model_milex_fecy_surplus365alt <- list()
modelfits_milex_fecy_surplus365alt <- list()
coefs_milex_fecy_surplus365alt <- c()
ses_milex_fecy_surplus365alt <- list()
r2_milex_fecy_surplus365alt <- c()
for(i in 1) {
  modformula <- paste(dvval_surplus365_milex, 
                      paste(vars_comp[i], 
                            controlvalalt_surplus365_fe[1], 
                            controlvalalt_surplus365_fe[2], sep = "+"), 
                      sep = "~")
  model <- plm(as.formula(modformula), data = master_tscs, model = "within", effect = "twoways")
  model_milex_fecy_surplus365alt[[i]] <- model
  modelfits_milex_fecy_surplus365alt[[i]] <- coef_test(model, vcov = "CR2", cluster = "individual", test = "Satterthwaite")
  coefs_milex_fecy_surplus365alt[[i]] <- modelfits_milex_fecy_surplus365alt[[i]]$beta
  ses_milex_fecy_surplus365alt[[i]] <- modelfits_milex_fecy_surplus365alt[[i]]$SE
  r2_milex_fecy_surplus365alt[i] <- summary(model)$r.squared[2]
}


# 
# ### pop
# # FECY model, controls
# model_milex_fecy_pop <- list()
# modelfits_milex_fecy_pop <- list()
# coefs_milex_fecy_pop <- c()
# ses_milex_fecy_pop <- list()
# r2_milex_fecy_pop <- c()
# for(i in 1) {
#   modformula <- paste(dvval_surplus365_milex, 
#                       paste(vars_comppop[i], 
#                             controlval_surplus365_fe[1], 
#                             controlval_surplus365_fe[2],
#                             controlval_surplus365_fe[3], sep = "+"), 
#                       sep = "~")
#   model <- plm(as.formula(modformula), data = master_tscs, model = "within", effect = "twoways")
#   model_milex_fecy_pop[[i]] <- model
#   modelfits_milex_fecy_pop[[i]] <- coef_test(model, vcov = "CR2", cluster = "individual", test = "Satterthwaite")
#   coefs_milex_fecy_pop[[i]] <- modelfits_milex_fecy_pop[[i]]$beta
#   ses_milex_fecy_pop[[i]] <- modelfits_milex_fecy_pop[[i]]$SE
#   r2_milex_fecy_pop[i] <- summary(model)$r.squared[2]
# }
# 
# 
# ### pop
# # FECY model, controls alternative
# model_milex_fecy_popalt <- list()
# modelfits_milex_fecy_popalt <- list()
# coefs_milex_fecy_popalt <- c()
# ses_milex_fecy_popalt <- list()
# r2_milex_fecy_popalt <- c()
# for(i in 1) {
#   modformula <- paste(dvval_surplus365_milex, 
#                       paste(vars_comppop[i], 
#                             controlvalalt_surplus365_fe[1], 
#                             controlvalalt_surplus365_fe[2], sep = "+"), 
#                       sep = "~")
#   model <- plm(as.formula(modformula), data = master_tscs, model = "within", effect = "twoways")
#   model_milex_fecy_popalt[[i]] <- model
#   modelfits_milex_fecy_popalt[[i]] <- coef_test(model, vcov = "CR2", cluster = "individual", test = "Satterthwaite")
#   coefs_milex_fecy_popalt[[i]] <- modelfits_milex_fecy_popalt[[i]]$beta
#   ses_milex_fecy_popalt[[i]] <- modelfits_milex_fecy_popalt[[i]]$SE
#   r2_milex_fecy_popalt[i] <- summary(model)$r.squared[2]
# }


### surplus365, pop
# FECY model, controls
model_milex_fecy_surplus365pop <- list()
modelfits_milex_fecy_surplus365pop <- list()
coefs_milex_fecy_surplus365pop <- c()
ses_milex_fecy_surplus365pop <- c()
r2_milex_fecy_surplus365pop <- c()
for(i in 1:length(vars_comp)) {
  modformula <- paste(dvval_surplus365_milex, 
                      paste(vars_comp[i], 
                            vars_comppop[i], 
                            controlval_surplus365_fe[1], 
                            controlval_surplus365_fe[2],
                            controlval_surplus365_fe[3], sep = "+"), 
                      sep = "~")
  model <- plm(as.formula(modformula), data = master_tscs, model = "within", effect = "twoways")
  model_milex_fecy_surplus365pop[[i]] <- model
  modelfits_milex_fecy_surplus365pop[[i]] <- coef_test(model, vcov = "CR2", cluster = "individual", test = "Satterthwaite")
  coefs_milex_fecy_surplus365pop[[i]] <- modelfits_milex_fecy_surplus365pop[[i]]$beta
  ses_milex_fecy_surplus365pop[[i]] <- modelfits_milex_fecy_surplus365pop[[i]]$SE
  r2_milex_fecy_surplus365pop[i] <- summary(model)$r.squared[2]
}


### surplus365, pop
# FECY model, controls alternative
model_milex_fecy_surplus365popalt <- list()
modelfits_milex_fecy_surplus365popalt <- list()
coefs_milex_fecy_surplus365popalt <- c()
ses_milex_fecy_surplus365popalt <- c()
r2_milex_fecy_surplus365popalt <- c()
for(i in 1:length(vars_comp)) {
  modformula <- paste(dvval_surplus365_milex, 
                      paste(vars_comp[i], 
                            vars_comppop[i], 
                            controlvalalt_surplus365_fe[1], 
                            controlvalalt_surplus365_fe[2], sep = "+"), 
                      sep = "~")
  model <- plm(as.formula(modformula), data = master_tscs, model = "within", effect = "twoways")
  model_milex_fecy_surplus365popalt[[i]] <- model
  modelfits_milex_fecy_surplus365popalt[[i]] <- coef_test(model, vcov = "CR2", cluster = "individual", test = "Satterthwaite")
  coefs_milex_fecy_surplus365popalt[[i]] <- modelfits_milex_fecy_surplus365popalt[[i]]$beta
  ses_milex_fecy_surplus365popalt[[i]] <- modelfits_milex_fecy_surplus365popalt[[i]]$SE
  r2_milex_fecy_surplus365popalt[i] <- summary(model)$r.squared[2]
}



# ### surplus365 interaction pop
# # FECY model, controls
# model_milex_fecy_surplus365Xpop <- list()
# modelfits_milex_fecy_surplus365Xpop <- list()
# coefs_milex_fecy_surplus365Xpop <- c()
# ses_milex_fecy_surplus365Xpop <- c()
# r2_milex_fecy_surplus365Xpop <- c()
# for(i in 1) {
#   modformula <- paste(dvval_surplus365_milex, 
#                       paste(paste(vars_comp[i], vars_comppop[i], sep = "*"), 
#                             controlval_surplus365_fe[1], 
#                             controlval_surplus365_fe[2],
#                             controlval_surplus365_fe[3], sep = "+"), 
#                       sep = "~")
#   model <- plm(as.formula(modformula), data = master_tscs, model = "within", effect = "twoways")
#   model_milex_fecy_surplus365Xpop[[i]] <- model
#   modelfits_milex_fecy_surplus365Xpop[[i]] <- coef_test(model, vcov = "CR2", cluster = "individual", test = "Satterthwaite")
#   coefs_milex_fecy_surplus365Xpop[[i]] <- modelfits_milex_fecy_surplus365Xpop[[i]]$beta
#   ses_milex_fecy_surplus365Xpop[[i]] <- modelfits_milex_fecy_surplus365Xpop[[i]]$SE
#   r2_milex_fecy_surplus365Xpop[i] <- summary(model)$r.squared[2]
# }


# ### surplus365 interaction pop
# # FECY model, controls alternative
# model_milex_fecy_surplus365Xpopalt <- list()
# modelfits_milex_fecy_surplus365Xpopalt <- list()
# coefs_milex_fecy_surplus365Xpopalt <- c()
# ses_milex_fecy_surplus365Xpopalt <- c()
# r2_milex_fecy_surplus365Xpopalt <- c()
# for(i in 1:length(vars_comp)) {
#   modformula <- paste(dvval_surplus365_milex, 
#                       paste(paste(vars_comp[i], vars_comppop[i], sep = "*"), 
#                             controlvalalt_surplus365_fe[1], 
#                             controlvalalt_surplus365_fe[2], sep = "+"), 
#                       sep = "~")
#   model <- plm(as.formula(modformula), data = master_tscs, model = "within", effect = "twoways")
#   model_milex_fecy_surplus365Xpopalt[[i]] <- model
#   modelfits_milex_fecy_surplus365Xpopalt[[i]] <- coef_test(model, vcov = "CR2", cluster = "individual", test = "Satterthwaite")
#   coefs_milex_fecy_surplus365Xpopalt[[i]] <- modelfits_milex_fecy_surplus365Xpopalt[[i]]$beta
#   ses_milex_fecy_surplus365Xpopalt[[i]] <- modelfits_milex_fecy_surplus365Xpopalt[[i]]$SE
#   r2_milex_fecy_surplus365Xpopalt[i] <- summary(model)$r.squared[2]
# }


#---------------------
# tonnageimputed
#---------------------

### surplus365
# FECY model, no controls
model_tonnageimputed_fecy_surplus365nocontrols <- list()
modelfits_tonnageimputed_fecy_surplus365nocontrols <- list()
coefs_tonnageimputed_fecy_surplus365nocontrols <- c()
ses_tonnageimputed_fecy_surplus365nocontrols <- c()
r2_tonnageimputed_fecy_surplus365nocontrols <- c()
for(i in 1) {
  modformula <- paste(dvval_surplus365_tonnageimputed, 
                      vars_comp[i], 
                      sep = "~")
  model <- plm(as.formula(modformula), data = master_tscs, model = "within", effect = "twoways")
  model_tonnageimputed_fecy_surplus365nocontrols[[i]] <- model
  modelfits_tonnageimputed_fecy_surplus365nocontrols[[i]] <- coef_test(model, vcov = "CR2", cluster = "individual", test = "Satterthwaite")
  coefs_tonnageimputed_fecy_surplus365nocontrols[[i]] <- modelfits_tonnageimputed_fecy_surplus365nocontrols[[i]]$beta
  ses_tonnageimputed_fecy_surplus365nocontrols[[i]] <- modelfits_tonnageimputed_fecy_surplus365nocontrols[[i]]$SE
  r2_tonnageimputed_fecy_surplus365nocontrols[i] <- summary(model)$r.squared[2]
}


### surplus365
# FECY model, controls
model_tonnageimputed_fecy_surplus365 <- list()
modelfits_tonnageimputed_fecy_surplus365 <- list()
coefs_tonnageimputed_fecy_surplus365 <- c()
ses_tonnageimputed_fecy_surplus365 <- c()
r2_tonnageimputed_fecy_surplus365 <- c()
for(i in 1) {
  modformula <- paste(dvval_surplus365_tonnageimputed, 
                      paste(vars_comp[i], 
                            controlval_surplus365_fe[1], 
                            controlval_surplus365_fe[2],
                            controlval_surplus365_fe[3], sep = "+"), 
                      sep = "~")
  model <- plm(as.formula(modformula), data = master_tscs, model = "within", effect = "twoways")
  model_tonnageimputed_fecy_surplus365[[i]] <- model
  modelfits_tonnageimputed_fecy_surplus365[[i]] <- coef_test(model, vcov = "CR2", cluster = "individual", test = "Satterthwaite")
  coefs_tonnageimputed_fecy_surplus365[[i]] <- modelfits_tonnageimputed_fecy_surplus365[[i]]$beta
  ses_tonnageimputed_fecy_surplus365[[i]] <- modelfits_tonnageimputed_fecy_surplus365[[i]]$SE
  r2_tonnageimputed_fecy_surplus365[i] <- summary(model)$r.squared[2]
}


# FECY model, controls alternative
model_tonnageimputed_fecy_surplus365alt <- list()
modelfits_tonnageimputed_fecy_surplus365alt <- list()
coefs_tonnageimputed_fecy_surplus365alt <- c()
ses_tonnageimputed_fecy_surplus365alt <- c()
r2_tonnageimputed_fecy_surplus365alt <- c()
for(i in 1) {
  modformula <- paste(dvval_surplus365_tonnageimputed, 
                      paste(vars_comp[i], 
                            controlvalalt_surplus365_fe[1], 
                            controlvalalt_surplus365_fe[2], sep = "+"), 
                      sep = "~")
  model <- plm(as.formula(modformula), data = master_tscs, model = "within", effect = "twoways")
  model_tonnageimputed_fecy_surplus365alt[[i]] <- model
  modelfits_tonnageimputed_fecy_surplus365alt[[i]] <- coef_test(model, vcov = "CR2", cluster = "individual", test = "Satterthwaite")
  coefs_tonnageimputed_fecy_surplus365alt[[i]] <- modelfits_tonnageimputed_fecy_surplus365alt[[i]]$beta
  ses_tonnageimputed_fecy_surplus365alt[[i]] <- modelfits_tonnageimputed_fecy_surplus365alt[[i]]$SE
  r2_tonnageimputed_fecy_surplus365alt[i] <- summary(model)$r.squared[2]
}



# ### pop
# # FECY model, controls
# model_tonnageimputed_fecy_pop <- list()
# modelfits_tonnageimputed_fecy_pop <- list()
# coefs_tonnageimputed_fecy_pop <- c()
# ses_tonnageimputed_fecy_pop <- c()
# r2_tonnageimputed_fecy_pop <- c()
# for(i in 1) {
#   modformula <- paste(dvval_surplus365_tonnageimputed, 
#                       paste(vars_comppop[i], 
#                             controlval_surplus365_fe[1], 
#                             controlval_surplus365_fe[2],
#                             controlval_surplus365_fe[3], sep = "+"), 
#                       sep = "~")
#   model <- plm(as.formula(modformula), data = master_tscs, model = "within", effect = "twoways")
#   model_tonnageimputed_fecy_pop[[i]] <- model
#   modelfits_tonnageimputed_fecy_pop[[i]] <- coef_test(model, vcov = "CR2", cluster = "individual", test = "Satterthwaite")
#   coefs_tonnageimputed_fecy_pop[[i]] <- modelfits_tonnageimputed_fecy_pop[[i]]$beta
#   ses_tonnageimputed_fecy_pop[[i]] <- modelfits_tonnageimputed_fecy_pop[[i]]$SE
#   r2_tonnageimputed_fecy_pop[i] <- summary(model)$r.squared[2]
# }
# 
# # FECY model, controls alternative
# model_tonnageimputed_fecy_popalt <- list()
# modelfits_tonnageimputed_fecy_popalt <- list()
# coefs_tonnageimputed_fecy_popalt <- c()
# ses_tonnageimputed_fecy_popalt <- c()
# r2_tonnageimputed_fecy_popalt <- c()
# for(i in 1) {
#   modformula <- paste(dvval_surplus365_tonnageimputed, 
#                       paste(vars_comppop[i], 
#                             controlvalalt_surplus365_fe[1], 
#                             controlvalalt_surplus365_fe[2], sep = "+"), 
#                       sep = "~")
#   model <- plm(as.formula(modformula), data = master_tscs, model = "within", effect = "twoways")
#   model_tonnageimputed_fecy_popalt[[i]] <- model
#   modelfits_tonnageimputed_fecy_popalt[[i]] <- coef_test(model, vcov = "CR2", cluster = "individual", test = "Satterthwaite")
#   coefs_tonnageimputed_fecy_popalt[[i]] <- modelfits_tonnageimputed_fecy_popalt[[i]]$beta
#   ses_tonnageimputed_fecy_popalt[[i]] <- modelfits_tonnageimputed_fecy_popalt[[i]]$SE
#   r2_tonnageimputed_fecy_popalt[i] <- summary(model)$r.squared[2]
# }


### surplus365, pop
# FECY model, controls
model_tonnageimputed_fecy_surplus365pop <- list()
modelfits_tonnageimputed_fecy_surplus365pop <- list()
coefs_tonnageimputed_fecy_surplus365pop <- c()
ses_tonnageimputed_fecy_surplus365pop <- c()
r2_tonnageimputed_fecy_surplus365pop <- c()
for(i in 1:length(vars_comp)) {
  modformula <- paste(dvval_surplus365_tonnageimputed, 
                      paste(vars_comp[i], 
                            vars_comppop[i], 
                            controlval_surplus365_fe[1], 
                            controlval_surplus365_fe[2],
                            controlval_surplus365_fe[3], sep = "+"), 
                      sep = "~")
  model <- plm(as.formula(modformula), data = master_tscs, model = "within", effect = "twoways")
  model_tonnageimputed_fecy_surplus365pop[[i]] <- model
  modelfits_tonnageimputed_fecy_surplus365pop[[i]] <- coef_test(model, vcov = "CR2", cluster = "individual", test = "Satterthwaite")
  coefs_tonnageimputed_fecy_surplus365pop[[i]] <- modelfits_tonnageimputed_fecy_surplus365pop[[i]]$beta
  ses_tonnageimputed_fecy_surplus365pop[[i]] <- modelfits_tonnageimputed_fecy_surplus365pop[[i]]$SE
  r2_tonnageimputed_fecy_surplus365pop[i] <- summary(model)$r.squared[2]
}

# FECY model, controls alternative
model_tonnageimputed_fecy_surplus365popalt <- list()
modelfits_tonnageimputed_fecy_surplus365popalt <- list()
coefs_tonnageimputed_fecy_surplus365popalt <- c()
ses_tonnageimputed_fecy_surplus365popalt <- c()
r2_tonnageimputed_fecy_surplus365popalt <- c()
for(i in 1:length(vars_comp)) {
  modformula <- paste(dvval_surplus365_tonnageimputed, 
                      paste(vars_comp[i], 
                            vars_comppop[i], 
                            controlvalalt_surplus365_fe[1], 
                            controlvalalt_surplus365_fe[2], sep = "+"), 
                      sep = "~")
  model <- plm(as.formula(modformula), data = master_tscs, model = "within", effect = "twoways")
  model_tonnageimputed_fecy_surplus365popalt[[i]] <- model
  modelfits_tonnageimputed_fecy_surplus365popalt[[i]] <- coef_test(model, vcov = "CR2", cluster = "individual", test = "Satterthwaite")
  coefs_tonnageimputed_fecy_surplus365popalt[[i]] <- modelfits_tonnageimputed_fecy_surplus365popalt[[i]]$beta
  ses_tonnageimputed_fecy_surplus365popalt[[i]] <- modelfits_tonnageimputed_fecy_surplus365popalt[[i]]$SE
  r2_tonnageimputed_fecy_surplus365popalt[i] <- summary(model)$r.squared[2]
}


# ### surplus365 interaction pop
# # FECY model, controls
# model_tonnageimputed_fecy_surplus365Xpop <- list()
# modelfits_tonnageimputed_fecy_surplus365Xpop <- list()
# coefs_tonnageimputed_fecy_surplus365Xpop <- c()
# ses_tonnageimputed_fecy_surplus365Xpop <- c()
# r2_tonnageimputed_fecy_surplus365Xpop <- c()
# for(i in 1) {
#   modformula <- paste(dvval_surplus365_tonnageimputed, 
#                       paste(paste(vars_comp[i], vars_comppop[i], sep = "*"), 
#                             controlval_surplus365_fe[1], 
#                             controlval_surplus365_fe[2],
#                             controlval_surplus365_fe[3], sep = "+"), 
#                       sep = "~")
#   model <- plm(as.formula(modformula), data = master_tscs, model = "within", effect = "twoways")
#   model_tonnageimputed_fecy_surplus365Xpop[[i]] <- model
#   modelfits_tonnageimputed_fecy_surplus365Xpop[[i]] <- coef_test(model, vcov = "CR2", cluster = "individual", test = "Satterthwaite")
#   coefs_tonnageimputed_fecy_surplus365Xpop[[i]] <- modelfits_tonnageimputed_fecy_surplus365Xpop[[i]]$beta
#   ses_tonnageimputed_fecy_surplus365Xpop[[i]] <- modelfits_tonnageimputed_fecy_surplus365Xpop[[i]]$SE
#   r2_tonnageimputed_fecy_surplus365Xpop[i] <- summary(model)$r.squared[2]
# }


# # FECY model, controls alternative
# model_tonnageimputed_fecy_surplus365Xpopalt <- list()
# modelfits_tonnageimputed_fecy_surplus365Xpopalt <- list()
# coefs_tonnageimputed_fecy_surplus365Xpopalt <- c()
# ses_tonnageimputed_fecy_surplus365Xpopalt <- c()
# r2_tonnageimputed_fecy_surplus365Xpopalt <- c()
# for(i in 1:length(vars_comp)) {
#   modformula <- paste(dvval_surplus365_tonnageimputed, 
#                       paste(paste(vars_comp[i], vars_comppop[i], sep = "*"), 
#                             controlvalalt_surplus365_fe[1], 
#                             controlvalalt_surplus365_fe[2], sep = "+"), 
#                       sep = "~")
#   model <- plm(as.formula(modformula), data = master_tscs, model = "within", effect = "twoways")
#   model_tonnageimputed_fecy_surplus365Xpopalt[[i]] <- model
#   modelfits_tonnageimputed_fecy_surplus365Xpopalt[[i]] <- coef_test(model, vcov = "CR2", cluster = "individual", test = "Satterthwaite")
#   coefs_tonnageimputed_fecy_surplus365Xpopalt[[i]] <- modelfits_tonnageimputed_fecy_surplus365Xpopalt[[i]]$beta
#   ses_tonnageimputed_fecy_surplus365Xpopalt[[i]] <- modelfits_tonnageimputed_fecy_surplus365Xpopalt[[i]]$SE
#   r2_tonnageimputed_fecy_surplus365Xpopalt[i] <- summary(model)$r.squared[2]
# }

#############################################
# Creating an overview table
#############################################
econ_surplus365 <- data.frame(vars = str_replace_all(vars_comp, "_l1", ""),
                                 
                                 unlist(map(coefs_milex_fecy_surplus365nocontrols, 1)),
                                 unlist(map(coefs_milex_fecy_surplus365, 1)),
                                 unlist(map(coefs_milex_fecy_surplus365pop, 1)),
                                # unlist(map(coefs_milex_fecy_surplus365Xpop, 1)),
                                 unlist(map(ses_milex_fecy_surplus365nocontrols, 1)),
                                 unlist(map(ses_milex_fecy_surplus365, 1)),
                                 unlist(map(ses_milex_fecy_surplus365pop, 1)),
                                 #unlist(map(ses_milex_fecy_surplus365Xpop, 1)),
                                 
                                 unlist(map(coefs_tonnageimputed_fecy_surplus365nocontrols, 1)),
                                 unlist(map(coefs_tonnageimputed_fecy_surplus365, 1)),
                                 unlist(map(coefs_tonnageimputed_fecy_surplus365pop, 1)),
                                 #unlist(map(coefs_tonnageimputed_fecy_surplus365Xpop, 1)),
                                 unlist(map(ses_tonnageimputed_fecy_surplus365nocontrols, 1)),
                                 unlist(map(ses_tonnageimputed_fecy_surplus365, 1)),
                                 unlist(map(ses_tonnageimputed_fecy_surplus365pop, 1))) %>%
                                 #unlist(map(ses_tonnageimputed_fecy_surplus365Xpop, 1))) %>%
  
  gather(indicator, value, -vars) %>%
  mutate(indicator = str_replace_all(indicator, "unlist.map.", ""),
         indicator = str_replace_all(indicator, "\\.\\.[0-9]\\.\\.", "")) %>%
  separate(indicator, c("indicator", "dv", "model", "controls"), sep = "_") %>%
  spread(indicator, value) %>%
  mutate(lower95 = coefs - 1.96*ses, #95% CI
         lower99 = coefs - 2.58*ses, #99% CI
         upper95 = coefs + 1.96*ses,
         upper99 = coefs + 2.58*ses) %>%
  mutate(coef = "econ", type_control = "wsub")

econ_surplus365_alt <- data.frame(vars = str_replace_all(vars_comp, "_l1", ""),
                                    
                                    unlist(map(coefs_milex_fecy_surplus365nocontrols, 1)),
                                    unlist(map(coefs_milex_fecy_surplus365alt, 1)),
                                    unlist(map(coefs_milex_fecy_surplus365popalt, 1)),
                                    #unlist(map(coefs_milex_fecy_surplus365Xpopalt, 1)),
                                    unlist(map(ses_milex_fecy_surplus365nocontrols, 1)),
                                    unlist(map(ses_milex_fecy_surplus365alt, 1)),
                                    unlist(map(ses_milex_fecy_surplus365popalt, 1)),
                                    #unlist(map(ses_milex_fecy_surplus365Xpopalt, 1)),
                                    
                                    unlist(map(coefs_tonnageimputed_fecy_surplus365nocontrols, 1)),
                                    unlist(map(coefs_tonnageimputed_fecy_surplus365alt, 1)),
                                    unlist(map(coefs_tonnageimputed_fecy_surplus365popalt, 1)),
                                    #unlist(map(coefs_tonnageimputed_fecy_surplus365Xpopalt, 1)),
                                    unlist(map(ses_tonnageimputed_fecy_surplus365nocontrols, 1)),
                                    unlist(map(ses_tonnageimputed_fecy_surplus365alt, 1)),
                                    unlist(map(ses_tonnageimputed_fecy_surplus365popalt, 1))) %>%
                                    #unlist(map(ses_tonnageimputed_fecy_surplus365Xpopalt, 1))) %>%
  
  gather(indicator, value, -vars) %>%
  mutate(indicator = str_replace_all(indicator, "unlist.map.", ""),
         indicator = str_replace_all(indicator, "\\.\\.[0-9]\\.\\.", "")) %>%
  separate(indicator, c("indicator", "dv", "model", "controls"), sep = "_") %>%
  spread(indicator, value) %>%
  mutate(lower95 = coefs - 1.96*ses, #95% CI
         lower99 = coefs - 2.58*ses, #99% CI
         upper95 = coefs + 1.96*ses,
         upper99 = coefs + 2.58*ses) %>% 
  mutate(coef = "econ", type_control = "wosub")

######################
#
# ********************
#
# $0/day Surplus (GDP)
#
# ********************
#
######################

############## Setting up variables ##################
# Main variables
vars_comp <- c("scaled_comp_gdp_polity_invlog_nodenom_all_l1",
               "scaled_comp_gdp_boix_invlog_nodenom_all_l1",
               "scaled_comp_gdp_uds_invlog_nodenom_all_l1",
               "scaled_comp_gdp_riv_invlog_nodenom_all_l1",
               "scaled_comp_gdp_defense_invlog_nodenom_all_l1",
               "scaled_comp_gdp_sscores_invlog_nodenom_all_l1",
               "scaled_comp_gdp_absidealdiff_invlog_nodenom_all_l1",
               "scaled_comp_gdp_igo_invlog_nodenom_all_l1",
               "scaled_comp_gdp_diplo_invlog_nodenom_all_l1",
               "scaled_comp_gdp_bitrade_invlog_nodenom_all_l1",
               "scaled_comp_gdp_nopreference_invlog_nodenom_all_l1",
               "scaled_comp_gdp_pecpc_invlog_nodenom_all_l1",
               "scaled_comp_gdp_atopkappa_invlog_nodenom_all_l1",
               "scaled_comp_gdp_atopbin_invlog_nodenom_all_l1")

varlabels_comp <- c("$\\text{PT GDP Joint Democracy (Polity)}_{i,t-1}$",
                    "$\\text{PT GDP Joint Democracy (Boix et al.)}_{i,t-1}$",
                    "$\\text{PT GDP Joint Democracy (UDS)}_{i,t-1}$",
                    "$\\text{PT GDP Rivalry}_{i,t-1}$",
                    "$\\text{PT GDP Defense Pacts}_{i,t-1}$",
                    "$\\text{PT GDP S-Scores}_{i,t-1}$",
                    "$\\text{PT GDP Ideal Difference}_{i,t-1}$",
                    "$\\text{PT GDP IGO Membership}_{i,t-1}$",
                    "$\\text{PT GDP Diplo. Exchange}_{i,t-1}$",
                    "$\\text{PT GDP Bilateral Trade}_{i,t-1}$",
                    "$\\text{PT GDP No Interest Variable}_{i,t-1}$",
                    "$\\text{PT GDP Energy consumption}_{i,t-1}$",
                    "$\\text{PT GDP Alliances (continuous)}_{i,t-1}$",
                    "$\\text{PT GDP Alliances (binary)}_{i,t-1}$")

## Controls 
# GDP
controlval_gdp_fe <- c("WorldBank_gdp_2011_ppp_estimate_ln_l1",
                       "WorldBank_pop_estimate_ln_l1",
                       "polity2_l1")

controllab_gdp_fe <- c("$\\text{ln GDP}_{i,t-1}$",
                       "$\\text{ln Population}_{i,t-1}$",
                       "$\\text{Polity2}_{i,t-1}$")


controlvalalt_gdp_fe <- c("WorldBank_gdp_2011_ppp_estimate_ln_l1",
                          "polity2_l1")

controllabalt_gdp_fe <- c("$\\text{ln GDP}_{i,t-1}$",
                          "$\\text{Polity2}_{i,t-1}$")


dvval_gdp_milex <- c("ln_milex_gdp")

dvval_gdp_tonnageimputed <- c("ln_tonnageimputed_gdp")

dvlab_gdp_milex <- c("$\\ln\\text{ Military Expenditure/GDP}_{i,t}$")

dvlab_gdp_tonnageimputed <- c("$\\ln\\text{ Naval Tonnage/GDP}_{i,t}$")


############## Running the Models ##################

#---------------------
# milex
#---------------------

### gdp
# FECY model, no controls
model_milex_fecy_gdpnocontrols <- list()
modelfits_milex_fecy_gdpnocontrols <- list()
coefs_milex_fecy_gdpnocontrols <- c()
ses_milex_fecy_gdpnocontrols <- list()
r2_milex_fecy_gdpnocontrols <- c()
for(i in 1) {
  modformula <- paste(dvval_gdp_milex, 
                      vars_comp[i],
                      sep = "~")
  model <- plm(as.formula(modformula), data = master_tscs, model = "within", effect = "twoways")
  model_milex_fecy_gdpnocontrols[[i]] <- model
  modelfits_milex_fecy_gdpnocontrols[[i]] <- coef_test(model, vcov = "CR2", cluster = "individual", test = "Satterthwaite")
  coefs_milex_fecy_gdpnocontrols[[i]] <- modelfits_milex_fecy_gdpnocontrols[[i]]$beta
  ses_milex_fecy_gdpnocontrols[[i]] <- modelfits_milex_fecy_gdpnocontrols[[i]]$SE
  r2_milex_fecy_gdpnocontrols[i] <- summary(model)$r.squared[2]
}


### gdp
# FECY model, controls
model_milex_fecy_gdp <- list()
modelfits_milex_fecy_gdp <- list()
coefs_milex_fecy_gdp <- c()
ses_milex_fecy_gdp <- list()
r2_milex_fecy_gdp <- c()
for(i in 1) {
  modformula <- paste(dvval_gdp_milex, 
                      paste(vars_comp[i], 
                            controlval_gdp_fe[1], 
                            controlval_gdp_fe[2],
                            controlval_gdp_fe[3], sep = "+"), 
                      sep = "~")
  model <- plm(as.formula(modformula), data = master_tscs, model = "within", effect = "twoways")
  model_milex_fecy_gdp[[i]] <- model
  modelfits_milex_fecy_gdp[[i]] <- coef_test(model, vcov = "CR2", cluster = "individual", test = "Satterthwaite")
  coefs_milex_fecy_gdp[[i]] <- modelfits_milex_fecy_gdp[[i]]$beta
  ses_milex_fecy_gdp[[i]] <- modelfits_milex_fecy_gdp[[i]]$SE
  r2_milex_fecy_gdp[i] <- summary(model)$r.squared[2]
}


## gdp
# FECY model, alternative controls
model_milex_fecy_gdpalt <- list()
modelfits_milex_fecy_gdpalt <- list()
coefs_milex_fecy_gdpalt <- c()
ses_milex_fecy_gdpalt <- list()
r2_milex_fecy_gdpalt <- c()
for(i in 1) {
  modformula <- paste(dvval_gdp_milex,
                      paste(vars_comp[i],
                            controlvalalt_gdp_fe[1],
                            controlvalalt_gdp_fe[2], sep = "+"),
                      sep = "~")
  model <- plm(as.formula(modformula), data = master_tscs, model = "within", effect = "twoways")
  model_milex_fecy_gdpalt[[i]] <- model
  modelfits_milex_fecy_gdpalt[[i]] <- coef_test(model, vcov = "CR2", cluster = "individual", test = "Satterthwaite")
  coefs_milex_fecy_gdpalt[[i]] <- modelfits_milex_fecy_gdpalt[[i]]$beta
  ses_milex_fecy_gdpalt[[i]] <- modelfits_milex_fecy_gdpalt[[i]]$SE
  r2_milex_fecy_gdpalt[i] <- summary(model)$r.squared[2]
}


### pop
# FECY model, controls
model_milex_fecy_pop <- list()
modelfits_milex_fecy_pop <- list()
coefs_milex_fecy_pop <- c()
ses_milex_fecy_pop <- list()
r2_milex_fecy_pop <- c()
for(i in 1) {
  modformula <- paste(dvval_gdp_milex, 
                      paste(vars_comppop[i], 
                            controlval_gdp_fe[1], 
                            controlval_gdp_fe[2],
                            controlval_gdp_fe[3], sep = "+"), 
                      sep = "~")
  model <- plm(as.formula(modformula), data = master_tscs, model = "within", effect = "twoways")
  model_milex_fecy_pop[[i]] <- model
  modelfits_milex_fecy_pop[[i]] <- coef_test(model, vcov = "CR2", cluster = "individual", test = "Satterthwaite")
  coefs_milex_fecy_pop[[i]] <- modelfits_milex_fecy_pop[[i]]$beta
  ses_milex_fecy_pop[[i]] <- modelfits_milex_fecy_pop[[i]]$SE
  r2_milex_fecy_pop[i] <- summary(model)$r.squared[2]
}


# ### pop
# # FECY model, controls alternative
# model_milex_fecy_popalt <- list()
# modelfits_milex_fecy_popalt <- list()
# coefs_milex_fecy_popalt <- c()
# ses_milex_fecy_popalt <- list()
# r2_milex_fecy_popalt <- c()
# for(i in 1) {
#   modformula <- paste(dvval_gdp_milex,
#                       paste(vars_comppop[i],
#                             controlvalalt_gdp_fe[1],
#                             controlvalalt_gdp_fe[2], sep = "+"),
#                       sep = "~")
#   model <- plm(as.formula(modformula), data = master_tscs, model = "within", effect = "twoways")
#   model_milex_fecy_popalt[[i]] <- model
#   modelfits_milex_fecy_popalt[[i]] <- coef_test(model, vcov = "CR2", cluster = "individual", test = "Satterthwaite")
#   coefs_milex_fecy_popalt[[i]] <- modelfits_milex_fecy_popalt[[i]]$beta
#   ses_milex_fecy_popalt[[i]] <- modelfits_milex_fecy_popalt[[i]]$SE
#   r2_milex_fecy_popalt[i] <- summary(model)$r.squared[2]
# }



### gdp, pop
# FECY model, controls
model_milex_fecy_gdppop <- list()
modelfits_milex_fecy_gdppop <- list()
coefs_milex_fecy_gdppop <- c()
ses_milex_fecy_gdppop <- c()
r2_milex_fecy_gdppop <- c()
for(i in 1:length(vars_comp)) {
  modformula <- paste(dvval_gdp_milex, 
                      paste(vars_comp[i], 
                            vars_comppop[i], 
                            controlval_gdp_fe[1], 
                            controlval_gdp_fe[2],
                            controlval_gdp_fe[3], sep = "+"), 
                      sep = "~")
  model <- plm(as.formula(modformula), data = master_tscs, model = "within", effect = "twoways")
  model_milex_fecy_gdppop[[i]] <- model
  modelfits_milex_fecy_gdppop[[i]] <- coef_test(model, vcov = "CR2", cluster = "individual", test = "Satterthwaite")
  coefs_milex_fecy_gdppop[[i]] <- modelfits_milex_fecy_gdppop[[i]]$beta
  ses_milex_fecy_gdppop[[i]] <- modelfits_milex_fecy_gdppop[[i]]$SE
  r2_milex_fecy_gdppop[i] <- summary(model)$r.squared[2]
}

### gdp, pop
# FECY model, controls alternative
model_milex_fecy_gdppopalt <- list()
modelfits_milex_fecy_gdppopalt <- list()
coefs_milex_fecy_gdppopalt <- c()
ses_milex_fecy_gdppopalt <- c()
r2_milex_fecy_gdppopalt <- c()
for(i in 1:length(vars_comp)) {
  modformula <- paste(dvval_gdp_milex,
                      paste(vars_comp[i],
                            vars_comppop[i],
                            controlvalalt_gdp_fe[1],
                            controlvalalt_gdp_fe[2], sep = "+"),
                      sep = "~")
  model <- plm(as.formula(modformula), data = master_tscs, model = "within", effect = "twoways")
  model_milex_fecy_gdppopalt[[i]] <- model
  modelfits_milex_fecy_gdppopalt[[i]] <- coef_test(model, vcov = "CR2", cluster = "individual", test = "Satterthwaite")
  coefs_milex_fecy_gdppopalt[[i]] <- modelfits_milex_fecy_gdppopalt[[i]]$beta
  ses_milex_fecy_gdppopalt[[i]] <- modelfits_milex_fecy_gdppopalt[[i]]$SE
  r2_milex_fecy_gdppopalt[i] <- summary(model)$r.squared[2]
}


### gdp interaction pop
# FECY model, controls
model_milex_fecy_gdpXpop <- list()
modelfits_milex_fecy_gdpXpop <- list()
coefs_milex_fecy_gdpXpop <- c()
ses_milex_fecy_gdpXpop <- c()
r2_milex_fecy_gdpXpop <- c()
for(i in 1) {
  modformula <- paste(dvval_gdp_milex, 
                      paste(paste(vars_comp[i], vars_comppop[i], sep = "*"), 
                            controlval_gdp_fe[1], 
                            controlval_gdp_fe[2],
                            controlval_gdp_fe[3], sep = "+"), 
                      sep = "~")
  model <- plm(as.formula(modformula), data = master_tscs, model = "within", effect = "twoways")
  model_milex_fecy_gdpXpop[[i]] <- model
  modelfits_milex_fecy_gdpXpop[[i]] <- coef_test(model, vcov = "CR2", cluster = "individual", test = "Satterthwaite")
  coefs_milex_fecy_gdpXpop[[i]] <- modelfits_milex_fecy_gdpXpop[[i]]$beta
  ses_milex_fecy_gdpXpop[[i]] <- modelfits_milex_fecy_gdpXpop[[i]]$SE
  r2_milex_fecy_gdpXpop[i] <- summary(model)$r.squared[2]
}


# ### gdp interaction pop
# # FECY model, controls alternative
# model_milex_fecy_gdpXpopalt <- list()
# modelfits_milex_fecy_gdpXpopalt <- list()
# coefs_milex_fecy_gdpXpopalt <- c()
# ses_milex_fecy_gdpXpopalt <- c()
# r2_milex_fecy_gdpXpopalt <- c()
# for(i in 1:length(vars_comp)) {
#   modformula <- paste(dvval_gdp_milex,
#                       paste(paste(vars_comp[i], vars_comppop[i], sep = "*"),
#                             controlvalalt_gdp_fe[1],
#                             controlvalalt_gdp_fe[2], sep = "+"),
#                       sep = "~")
#   model <- plm(as.formula(modformula), data = master_tscs, model = "within", effect = "twoways")
#   model_milex_fecy_gdpXpopalt[[i]] <- model
#   modelfits_milex_fecy_gdpXpopalt[[i]] <- coef_test(model, vcov = "CR2", cluster = "individual", test = "Satterthwaite")
#   coefs_milex_fecy_gdpXpopalt[[i]] <- modelfits_milex_fecy_gdpXpopalt[[i]]$beta
#   ses_milex_fecy_gdpXpopalt[[i]] <- modelfits_milex_fecy_gdpXpopalt[[i]]$SE
#   r2_milex_fecy_gdpXpopalt[i] <- summary(model)$r.squared[2]
# }

#---------------------
# tonnageimputed
#---------------------

### gdp
# FECY model, no controls
model_tonnageimputed_fecy_gdpnocontrols <- list()
modelfits_tonnageimputed_fecy_gdpnocontrols <- list()
coefs_tonnageimputed_fecy_gdpnocontrols <- c()
ses_tonnageimputed_fecy_gdpnocontrols <- c()
r2_tonnageimputed_fecy_gdpnocontrols <- c()
for(i in 1) {
  modformula <- paste(dvval_gdp_tonnageimputed, 
                      vars_comp[i], 
                      sep = "~")
  model <- plm(as.formula(modformula), data = master_tscs, model = "within", effect = "twoways")
  model_tonnageimputed_fecy_gdpnocontrols[[i]] <- model
  modelfits_tonnageimputed_fecy_gdpnocontrols[[i]] <- coef_test(model, vcov = "CR2", cluster = "individual", test = "Satterthwaite")
  coefs_tonnageimputed_fecy_gdpnocontrols[[i]] <- modelfits_tonnageimputed_fecy_gdpnocontrols[[i]]$beta
  ses_tonnageimputed_fecy_gdpnocontrols[[i]] <- modelfits_tonnageimputed_fecy_gdpnocontrols[[i]]$SE
  r2_tonnageimputed_fecy_gdpnocontrols[i] <- summary(model)$r.squared[2]
}


### gdp
# FECY model, controls
model_tonnageimputed_fecy_gdp <- list()
modelfits_tonnageimputed_fecy_gdp <- list()
coefs_tonnageimputed_fecy_gdp <- c()
ses_tonnageimputed_fecy_gdp <- c()
r2_tonnageimputed_fecy_gdp <- c()
for(i in 1) {
  modformula <- paste(dvval_gdp_tonnageimputed, 
                      paste(vars_comp[i], 
                            controlval_gdp_fe[1], 
                            controlval_gdp_fe[2],
                            controlval_gdp_fe[3], sep = "+"), 
                      sep = "~")
  model <- plm(as.formula(modformula), data = master_tscs, model = "within", effect = "twoways")
  model_tonnageimputed_fecy_gdp[[i]] <- model
  modelfits_tonnageimputed_fecy_gdp[[i]] <- coef_test(model, vcov = "CR2", cluster = "individual", test = "Satterthwaite")
  coefs_tonnageimputed_fecy_gdp[[i]] <- modelfits_tonnageimputed_fecy_gdp[[i]]$beta
  ses_tonnageimputed_fecy_gdp[[i]] <- modelfits_tonnageimputed_fecy_gdp[[i]]$SE
  r2_tonnageimputed_fecy_gdp[i] <- summary(model)$r.squared[2]
}

### gdp
# FECY model, alternative controls
model_tonnageimputed_fecy_gdpalt <- list()
modelfits_tonnageimputed_fecy_gdpalt <- list()
coefs_tonnageimputed_fecy_gdpalt <- c()
ses_tonnageimputed_fecy_gdpalt <- list()
r2_tonnageimputed_fecy_gdpalt <- c()
for(i in 1) {
  modformula <- paste(dvval_gdp_tonnageimputed,
                      paste(vars_comp[i],
                            controlvalalt_gdp_fe[1],
                            controlvalalt_gdp_fe[2], sep = "+"),
                      sep = "~")
  model <- plm(as.formula(modformula), data = master_tscs, model = "within", effect = "twoways")
  model_tonnageimputed_fecy_gdpalt[[i]] <- model
  modelfits_tonnageimputed_fecy_gdpalt[[i]] <- coef_test(model, vcov = "CR2", cluster = "individual", test = "Satterthwaite")
  coefs_tonnageimputed_fecy_gdpalt[[i]] <- modelfits_tonnageimputed_fecy_gdpalt[[i]]$beta
  ses_tonnageimputed_fecy_gdpalt[[i]] <- modelfits_tonnageimputed_fecy_gdpalt[[i]]$SE
  r2_tonnageimputed_fecy_gdpalt[i] <- summary(model)$r.squared[2]
}


### pop
# FECY model, controls
model_tonnageimputed_fecy_pop <- list()
modelfits_tonnageimputed_fecy_pop <- list()
coefs_tonnageimputed_fecy_pop <- c()
ses_tonnageimputed_fecy_pop <- c()
r2_tonnageimputed_fecy_pop <- c()
for(i in 1) {
  modformula <- paste(dvval_gdp_tonnageimputed, 
                      paste(vars_comppop[i], 
                            controlval_gdp_fe[1], 
                            controlval_gdp_fe[2],
                            controlval_gdp_fe[3], sep = "+"), 
                      sep = "~")
  model <- plm(as.formula(modformula), data = master_tscs, model = "within", effect = "twoways")
  model_tonnageimputed_fecy_pop[[i]] <- model
  modelfits_tonnageimputed_fecy_pop[[i]] <- coef_test(model, vcov = "CR2", cluster = "individual", test = "Satterthwaite")
  coefs_tonnageimputed_fecy_pop[[i]] <- modelfits_tonnageimputed_fecy_pop[[i]]$beta
  ses_tonnageimputed_fecy_pop[[i]] <- modelfits_tonnageimputed_fecy_pop[[i]]$SE
  r2_tonnageimputed_fecy_pop[i] <- summary(model)$r.squared[2]
}

# ### pop
# # FECY model, controls alternative
# model_tonnageimputed_fecy_popalt <- list()
# modelfits_tonnageimputed_fecy_popalt <- list()
# coefs_tonnageimputed_fecy_popalt <- c()
# ses_tonnageimputed_fecy_popalt <- c()
# r2_tonnageimputed_fecy_popalt <- c()
# for(i in 1) {
#   modformula <- paste(dvval_gdp_tonnageimputed,
#                       paste(vars_comppop[i],
#                             controlvalalt_gdp_fe[1],
#                             controlvalalt_gdp_fe[2], sep = "+"),
#                       sep = "~")
#   model <- plm(as.formula(modformula), data = master_tscs, model = "within", effect = "twoways")
#   model_tonnageimputed_fecy_popalt[[i]] <- model
#   modelfits_tonnageimputed_fecy_popalt[[i]] <- coef_test(model, vcov = "CR2", cluster = "individual", test = "Satterthwaite")
#   coefs_tonnageimputed_fecy_popalt[[i]] <- modelfits_tonnageimputed_fecy_popalt[[i]]$beta
#   ses_tonnageimputed_fecy_popalt[[i]] <- modelfits_tonnageimputed_fecy_popalt[[i]]$SE
#   r2_tonnageimputed_fecy_popalt[i] <- summary(model)$r.squared[2]
# }

### gdp, pop
# FECY model, controls
model_tonnageimputed_fecy_gdppop <- list()
modelfits_tonnageimputed_fecy_gdppop <- list()
coefs_tonnageimputed_fecy_gdppop <- c()
ses_tonnageimputed_fecy_gdppop <- c()
r2_tonnageimputed_fecy_gdppop <- c()
for(i in 1:length(vars_comp)) {
  modformula <- paste(dvval_gdp_tonnageimputed, 
                      paste(vars_comp[i], 
                            vars_comppop[i], 
                            controlval_gdp_fe[1], 
                            controlval_gdp_fe[2],
                            controlval_gdp_fe[3], sep = "+"), 
                      sep = "~")
  model <- plm(as.formula(modformula), data = master_tscs, model = "within", effect = "twoways")
  model_tonnageimputed_fecy_gdppop[[i]] <- model
  modelfits_tonnageimputed_fecy_gdppop[[i]] <- coef_test(model, vcov = "CR2", cluster = "individual", test = "Satterthwaite")
  coefs_tonnageimputed_fecy_gdppop[[i]] <- modelfits_tonnageimputed_fecy_gdppop[[i]]$beta
  ses_tonnageimputed_fecy_gdppop[[i]] <- modelfits_tonnageimputed_fecy_gdppop[[i]]$SE
  r2_tonnageimputed_fecy_gdppop[i] <- summary(model)$r.squared[2]
}


### gdp, pop
# FECY model, controls alternative
model_tonnageimputed_fecy_gdppopalt <- list()
modelfits_tonnageimputed_fecy_gdppopalt <- list()
coefs_tonnageimputed_fecy_gdppopalt <- c()
ses_tonnageimputed_fecy_gdppopalt <- c()
r2_tonnageimputed_fecy_gdppopalt <- c()
for(i in 1:length(vars_comp)) {
  modformula <- paste(dvval_gdp_tonnageimputed,
                      paste(vars_comp[i],
                            vars_comppop[i],
                            controlvalalt_gdp_fe[1],
                            controlvalalt_gdp_fe[2], sep = "+"),
                      sep = "~")
  model <- plm(as.formula(modformula), data = master_tscs, model = "within", effect = "twoways")
  model_tonnageimputed_fecy_gdppopalt[[i]] <- model
  modelfits_tonnageimputed_fecy_gdppopalt[[i]] <- coef_test(model, vcov = "CR2", cluster = "individual", test = "Satterthwaite")
  coefs_tonnageimputed_fecy_gdppopalt[[i]] <- modelfits_tonnageimputed_fecy_gdppopalt[[i]]$beta
  ses_tonnageimputed_fecy_gdppopalt[[i]] <- modelfits_tonnageimputed_fecy_gdppopalt[[i]]$SE
  r2_tonnageimputed_fecy_gdppopalt[i] <- summary(model)$r.squared[2]
}

### gdp interaction pop
# FECY model, controls
model_tonnageimputed_fecy_gdpXpop <- list()
modelfits_tonnageimputed_fecy_gdpXpop <- list()
coefs_tonnageimputed_fecy_gdpXpop <- c()
ses_tonnageimputed_fecy_gdpXpop <- c()
r2_tonnageimputed_fecy_gdpXpop <- c()
for(i in 1) {
  modformula <- paste(dvval_gdp_tonnageimputed, 
                      paste(paste(vars_comp[i], vars_comppop[i], sep = "*"), 
                            controlval_gdp_fe[1], 
                            controlval_gdp_fe[2],
                            controlval_gdp_fe[3], sep = "+"), 
                      sep = "~")
  model <- plm(as.formula(modformula), data = master_tscs, model = "within", effect = "twoways")
  model_tonnageimputed_fecy_gdpXpop[[i]] <- model
  modelfits_tonnageimputed_fecy_gdpXpop[[i]] <- coef_test(model, vcov = "CR2", cluster = "individual", test = "Satterthwaite")
  coefs_tonnageimputed_fecy_gdpXpop[[i]] <- modelfits_tonnageimputed_fecy_gdpXpop[[i]]$beta
  ses_tonnageimputed_fecy_gdpXpop[[i]] <- modelfits_tonnageimputed_fecy_gdpXpop[[i]]$SE
  r2_tonnageimputed_fecy_gdpXpop[i] <- summary(model)$r.squared[2]
}

# ### gdp interaction pop
# # FECY model, controls alternative
# model_tonnageimputed_fecy_gdpXpopalt <- list()
# modelfits_tonnageimputed_fecy_gdpXpopalt <- list()
# coefs_tonnageimputed_fecy_gdpXpopalt <- c()
# ses_tonnageimputed_fecy_gdpXpopalt <- c()
# r2_tonnageimputed_fecy_gdpXpopalt <- c()
# for(i in 1:length(vars_comp)) {
#   modformula <- paste(dvval_gdp_tonnageimputed,
#                       paste(paste(vars_comp[i], vars_comppop[i], sep = "*"),
#                             controlvalalt_gdp_fe[1],
#                             controlvalalt_gdp_fe[2], sep = "+"),
#                       sep = "~")
#   model <- plm(as.formula(modformula), data = master_tscs, model = "within", effect = "twoways")
#   model_tonnageimputed_fecy_gdpXpopalt[[i]] <- model
#   modelfits_tonnageimputed_fecy_gdpXpopalt[[i]] <- coef_test(model, vcov = "CR2", cluster = "individual", test = "Satterthwaite")
#   coefs_tonnageimputed_fecy_gdpXpopalt[[i]] <- modelfits_tonnageimputed_fecy_gdpXpopalt[[i]]$beta
#   ses_tonnageimputed_fecy_gdpXpopalt[[i]] <- modelfits_tonnageimputed_fecy_gdpXpopalt[[i]]$SE
#   r2_tonnageimputed_fecy_gdpXpopalt[i] <- summary(model)$r.squared[2]
# }

#############################################
#############################################
## Table 2 (Main Text)
#############################################
#############################################

stargazer(model_milex_fecy_gdpnocontrols[[1]],
          model_milex_fecy_gdp[[1]],
          model_milex_fecy_pop[[1]],
          model_milex_fecy_gdppop[[1]],
          model_milex_fecy_gdpXpop[[1]],
          
          model_tonnageimputed_fecy_gdpnocontrols[[1]],
          model_tonnageimputed_fecy_gdp[[1]],
          model_tonnageimputed_fecy_pop[[1]],
          model_tonnageimputed_fecy_gdppop[[1]],
          model_tonnageimputed_fecy_gdpXpop[[1]],
          
          # Cluster SES
          se=list(ses_milex_fecy_gdpnocontrols[[1]],
                  ses_milex_fecy_gdp[[1]],
                  ses_milex_fecy_pop[[1]],
                  ses_milex_fecy_gdppop[[1]],
                  ses_milex_fecy_gdpXpop[[1]],
                  
                  ses_tonnageimputed_fecy_gdpnocontrols[[1]],
                  ses_tonnageimputed_fecy_gdp[[1]],
                  ses_tonnageimputed_fecy_pop[[1]],
                  ses_tonnageimputed_fecy_gdppop[[1]],
                  ses_tonnageimputed_fecy_gdpXpop[[1]]),
          
          # Formatting
          font.size = "scriptsize",
          digits = 2,
          omit.stat = c("f","ser","rsq"),
          column.sep.width = "-6pt",
          dep.var.labels.include = F,
          star.cutoffs = c(0.05, 0.01, 0.001),
          no.space = TRUE,
          model.names = FALSE,
          
          # Labeling
          dep.var.caption  = "Dependent variables",
          covariate.labels = c("$\\text{Potential threat (GDP)}_{i,t-1}$",
                               "$\\text{Potential threat (Population)}_{i,t-1}$",
                               "$\\text{ln GDP}_{i,t-1}$",
                               "$\\text{ln Population}_{i,t-1}$",
                               "$\\text{Polity2}_{i,t-1}$",
                               "$\\text{Interaction Potential threat}_{i,t-1}$"),
          add.lines = list(c("Fixed-effects", rep("CY", 10))),
          column.separate = c(5,5),
          column.labels   = c("Military expenditure/GDP", 
                              "Naval tonnage/GDP"),
          title = "Regression models relating different specifications of the potential threat variable to investments in arming and power projection. Power-resources are measured using GDP. The loss of strength gradient is conceptualized as curvilinear using the formula $\\frac{1}{\\log(distance)}$. Interest compatibility based joint democracy using Polity scores.",
          
          #############################################
          # uncomment to save output to working directory
          # type = "html",
          # out = "Table_2.html",
          #############################################
          
          notes = c("Clustered standard errors by country (Satterthwaite correction) in parentheses.",
                    "Potential threat variables are standardized.",
                    "CY denotes two-way fixed effects.",
                    "Period of observation: 1816-2012."))

# #############################################
# # saving coefficients
# #############################################
econ_gdp <- data.frame(vars = str_replace_all(vars_comp, "_l1", ""),

                                  unlist(map(coefs_milex_fecy_gdpnocontrols, 1)),
                                  unlist(map(coefs_milex_fecy_gdp, 1)),
                                  unlist(map(coefs_milex_fecy_gdppop, 1)),
                                  unlist(map(coefs_milex_fecy_gdpXpop, 1)),
                                  unlist(map(ses_milex_fecy_gdpnocontrols, 1)),
                                  unlist(map(ses_milex_fecy_gdp, 1)),
                                  unlist(map(ses_milex_fecy_gdppop, 1)),
                                  unlist(map(ses_milex_fecy_gdpXpop, 1)),

                                  unlist(map(coefs_tonnageimputed_fecy_gdpnocontrols, 1)),
                                  unlist(map(coefs_tonnageimputed_fecy_gdp, 1)),
                                  unlist(map(coefs_tonnageimputed_fecy_gdppop, 1)),
                                  unlist(map(coefs_tonnageimputed_fecy_gdpXpop, 1)),
                                  unlist(map(ses_tonnageimputed_fecy_gdpnocontrols, 1)),
                                  unlist(map(ses_tonnageimputed_fecy_gdp, 1)),
                                  unlist(map(ses_tonnageimputed_fecy_gdppop, 1)),
                                  unlist(map(ses_tonnageimputed_fecy_gdpXpop, 1))) %>%

  gather(indicator, value, -vars) %>%
  mutate(indicator = str_replace_all(indicator, "unlist.map.", ""),
         indicator = str_replace_all(indicator, "..1..", "")) %>%
  separate(indicator, c("indicator", "dv", "model", "controls"), sep = "_") %>%
  spread(indicator, value) %>%
  mutate(lower95 = coefs - 1.96*ses, #95% CI
         lower99 = coefs - 2.58*ses, #99% CI
         upper95 = coefs + 1.96*ses,
         upper99 = coefs + 2.58*ses) %>%
  mutate(coef = "econ", type_control = "wsub") 

## Omitting population as a control
econ_gdp_alt <- data.frame(vars = str_replace_all(vars_comp, "_l1", ""),

                          unlist(map(coefs_milex_fecy_gdpalt, 1)),
                          unlist(map(coefs_milex_fecy_gdppopalt, 1)),
                          #unlist(map(coefs_milex_fecy_gdpXpopalt, 1)),
                          unlist(map(ses_milex_fecy_gdpalt, 1)),
                          unlist(map(ses_milex_fecy_gdppopalt, 1)),
                          #unlist(map(ses_milex_fecy_gdpXpopalt, 1)),

                          unlist(map(coefs_tonnageimputed_fecy_gdpalt, 1)),
                          unlist(map(coefs_tonnageimputed_fecy_gdppopalt, 1)),
                          #unlist(map(coefs_tonnageimputed_fecy_gdpXpopalt, 1)),
                          unlist(map(ses_tonnageimputed_fecy_gdpalt, 1)),
                          unlist(map(ses_tonnageimputed_fecy_gdppopalt, 1))) %>%
                          #unlist(map(ses_tonnageimputed_fecy_gdpXpopalt, 1))) %>%

  gather(indicator, value, -vars) %>%
  mutate(indicator = str_replace_all(indicator, "unlist.map.", ""),
         indicator = str_replace_all(indicator, "..1..", "")) %>%
  separate(indicator, c("indicator", "dv", "model", "controls"), sep = "_") %>%
  spread(indicator, value) %>%
  mutate(lower95 = coefs - 1.96*ses, #95% CI
         lower99 = coefs - 2.58*ses, #99% CI
         upper95 = coefs + 1.96*ses,
         upper99 = coefs + 2.58*ses) %>%
  mutate(coef = "econ", type_control = "wosub") 

# Figure 6: influence of potential threat on arming and power projection

merged <- bind_rows(econ_surplus1095, 
                    econ_surplus730, 
                    econ_surplus365, 
                    econ_gdp,
                    econ_surplus1095_alt, 
                    econ_surplus730_alt, 
                    econ_surplus365_alt, 
                    econ_gdp_alt) %>%
  dplyr::mutate(vars = str_replace_all(vars, "scaled_comp_", "")) %>%
  dplyr::mutate(vars = str_replace_all(vars, "_invlog_nodenom_all", "")) %>%
  separate(vars, c("variant", "vars")) %>%
  
  #mutate(variant = replace(variant, str_detect(controls, "alt"), "gdpnopop")) %>%
  
  mutate(controls = stringr::str_remove_all(controls, "alt")) %>%
  
  mutate(controls = stringr::str_replace_all(controls, "gdp", "simple")) %>%
  mutate(controls = stringr::str_replace_all(controls, "pop", "simple")) %>%
  mutate(controls = stringr::str_replace_all(controls, "surplus1460", "simple")) %>%
  mutate(controls = stringr::str_replace_all(controls, "surplus1095", "simple")) %>%
  mutate(controls = stringr::str_replace_all(controls, "surplus730", "simple")) %>%
  mutate(controls = stringr::str_replace_all(controls, "surplus365", "simple")) %>%
  # Recoding milex
  mutate(colcat = ifelse(dv == "milex" & controls == "simple", "milex_c", NA)) %>%
  mutate(colcat = replace(colcat, dv == "milex" & controls == "simplenocontrols", "milex_nc")) %>%
  mutate(colcat = replace(colcat, dv == "milex" & controls == "simplesimple", "milex_cc")) %>%
  mutate(colcat = replace(colcat, dv == "milex" & controls == "simpleXsimple", "milex_i")) %>%
  
  # Recoding milper
  mutate(colcat = replace(colcat, dv == "milper" & controls == "simple", "milper_c")) %>%
  mutate(colcat = replace(colcat, dv == "milper" & controls == "simplenocontrols", "milper_nc")) %>%
  mutate(colcat = replace(colcat, dv == "milper" & controls == "simplesimple", "milper_cc")) %>%
  mutate(colcat = replace(colcat, dv == "milper" & controls == "simpleXsimple", "milper_i")) %>%
  
  # Recoding tonnageimputed
  mutate(colcat = replace(colcat, dv == "tonnageimputed" & controls == "simple", "tonnage_c")) %>%
  mutate(colcat = replace(colcat, dv == "tonnageimputed" & controls == "simplenocontrols", "tonnage_nc")) %>%
  mutate(colcat = replace(colcat, dv == "tonnageimputed" & controls == "simplesimple", "tonnage_cc")) %>%
  mutate(colcat = replace(colcat, dv == "tonnageimputed" & controls == "simpleXsimple", "tonnage_i")) %>%
  
  # milexp
  mutate(colcat = replace(colcat, dv == "milexp" & controls == "simple", "milexp_c")) %>%
  mutate(colcat = replace(colcat, dv == "milexp" & controls == "simplenocontrols", "milexp_nc")) %>%
  mutate(colcat = replace(colcat, dv == "milexp" & controls == "simplesimple", "milexp_cc")) %>%
  mutate(colcat = replace(colcat, dv == "milexp" & controls == "simpleXsimple", "milexp_i")) %>%
  filter(controls != "simpleXsimple",
         dv != "milexp") %>%
  
  mutate(colcat_sub = ifelse(type_control == "wsub", 
                             paste(colcat, "wsub", sep = "_"),
                             paste(colcat, "wosub", sep = "_")))

merged$model <- factor(merged$model,
                       levels = c("fec",
                                  "fecy"),
                       labels = c("Country Fixed Effects",
                                  "Two-Way Fixed Effects"))

merged$variant <- factor(merged$variant,
                         levels = rev(c("surplus1095", 
                                        "surplus730", 
                                        "surplus365", 
                                        "gdp")),
                         labels = rev(c("SDP 3USD",
                                        "SDP 2USD",
                                        "SDP 1USD",
                                        "GDP")))



merged$dv <- factor(merged$dv,
                    levels = c("milex", "tonnageimputed"),
                    labels = c(expression("DV: ln Military expenditure index"),
                               expression("DV: ln Naval tonnage index")))



merged$vars <- factor(merged$vars,
                      levels = c("polity",
                                 "boix",
                                 "uds",
                                 "riv",
                                 "defense",
                                 "sscores",
                                 "absidealdiff",
                                 "igo",
                                 "diplo",
                                 "bitrade",
                                 "pecpc",
                                 "atopkappa",
                                 "atopbin",
                                 "nopreference"),
                      labels = c(expression("Joint democracy Polity"),
                                 expression("Joint democracy Boix"),
                                 expression("Joint democracy UDS"),
                                 expression("Rivalry"),
                                 expression("Defense pacts"),
                                 expression("S-scores"),
                                 expression("Ideal difference"),
                                 expression("IGO membership"),
                                 expression("Diplomatic exchange"),
                                 expression("Bilateral trade"),
                                 expression("Energy consumption"),
                                 expression("Alliances (continuous)"),
                                 expression("Alliances (binary)"),
                                 expression("No interest variable")))


merged$controls <- factor(merged$controls,
                          levels = c("simplenocontrols",
                                     "simple",
                                     "simplesimple"),
                          labels = c("No controls",
                                     "Basic controls",
                                     "Basic controls + alternative PT"))

merged$coef <- factor(merged$coef,
                      levels = c("econ", "pop"),
                      labels = c("Potential threat (economic)",
                                 "Potential threat (population)"))


merged$type_control <- factor(merged$type_control,
                              levels = c("wsub",
                                         "wosub"),
                              labels = c("With subsistence control",
                                         "Without subsistence control"))

### Polity
## Version in paper 
sub <- merged %>%
  filter(dv %in% c(expression("DV: ln Military expenditure index"),
                   expression("DV: ln Naval tonnage index"))) %>%
  filter(coef %in% c("Potential threat (economic)"),
         model %in% c("Two-Way Fixed Effects")) 


sub_polity <- sub %>%
  filter(vars %in% c(expression("Joint democracy Polity"))) %>%
  mutate(ymax = ifelse(dv %in% "DV: ln Military expenditure index", 2, 5),
         ymin = ifelse(dv %in% "DV: ln Military expenditure index", -1, -1.5))

### Main version - controlling for population-based PT
ggplot(subset(sub_polity, controls %in% c("Basic controls + alternative PT")),
       aes(x = variant, 
           y = coefs, 
           shape = type_control,
           color = colcat_sub,
           ymin = lower95, 
           ymax = upper95)) +
  geom_hline(yintercept = 0, colour = gray(1/2), lty = 2, alpha = 1) +
  geom_point(position = position_dodge(width = 1/2), size = 2.5) +
  geom_linerange(position = position_dodge(width = 1/2), size = .5) +
  theme_bw() +
  coord_flip() +
  labs(title = "Influence of potential threat on arming and power projection (1816-2012)",
       x = "",
       y = expression("Coefficient of standardized potential threat (Polity) variable"["i, t-1"])) +
  scale_shape_manual(name = "",
                     values = c(17, 16)) +
  scale_color_manual(name = "",
                     values = c("milex_cc_wosub" = "#006687",
                                "milex_cc_wsub" = "#003f54",
                                "tonnage_cc_wosub" = "#870023",
                                "tonnage_cc_wsub" = "#540016"),
                     guide = F) +
  facet_wrap(~ dv , scales = "free") + #Interprets as plotmath expression
  theme(panel.grid.minor = element_blank(),
        legend.position = "top",
        plot.title = element_text(size = 9, margin = margin(0,0,0,0)),
        legend.text = element_text(size = 6),
        legend.margin=margin(3,0,0,0),
        axis.text = element_text(size = 5),
        axis.title = element_text(size = 8),
        strip.text = element_text(size = 6)) +
  geom_blank(aes(y = ymax)) +
  geom_blank(aes(y = ymin)) +
  scale_y_continuous(breaks = seq(-1, 5, 1))

# Figure 23

## Dropping population-based PT controls
ggplot(subset(sub_polity, controls %in% c("Basic controls")),
       aes(x = variant, 
           y = coefs, 
           shape = type_control,
           color = colcat_sub,
           ymin = lower95, 
           ymax = upper95)) +
  geom_hline(yintercept = 0, colour = gray(1/2), lty = 2, alpha = 0.8) +
  geom_point(position = position_dodge(width = 1/2), size = 2.5) +
  geom_linerange(position = position_dodge(width = 1/2), size = .5) +
  theme_bw() +
  coord_flip() +
  labs(title = "Influence of potential threat on arming and power projection (1816-2012)",
       x = "",
       y = expression("Coefficient of standardized potential threat (Polity) variable"["i, t-1"])) +
  #scale_shape_manual(name = "",
  #values = c(17, 16)) +
  scale_color_manual(name = "",
                     values = c("milex_c_wosub" = "#006687",
                                "milex_c_wsub" = "#003f54",
                                "tonnage_c_wosub" = "#870023",
                                "tonnage_c_wsub" = "#540016"),
                     guide = F) +
  facet_wrap(~ dv , scales = "free") + #Interprets as plotmath expression
  theme(panel.grid.minor = element_blank(),
        legend.position = "top",
        strip.text = element_text(size = 10)) +
  geom_blank(aes(y = ymax)) +
  geom_blank(aes(y = ymin)) +
  scale_y_continuous(breaks = seq(-1, 5, 1))

# Figure 24

ggplot(subset(sub_polity, controls %in% c("No controls") & str_detect(colcat_sub, "wsub")),
       aes(x = variant, 
           y = coefs, 
           color = colcat_sub,
           ymin = lower95, 
           ymax = upper95)) +
  geom_hline(yintercept = 0, colour = gray(1/2), lty = 2, alpha = 0.8) +
  geom_point(position = position_dodge(width = 1/2), size = 2.5) +
  geom_linerange(position = position_dodge(width = 1/2), size = .5) +
  theme_bw() +
  coord_flip() +
  labs(title = "Influence of potential threat on arming and power projection (1816-2012)",
       subtitle = "Omitting control variables",
       x = "",
       y = expression("Coefficient of standardized potential threat (Polity) variable"["i, t-1"])) +
  scale_color_manual(name = "",
                     values = c("milex_nc_wosub" = "#006687",
                                "milex_nc_wsub" = "#003f54",
                                "tonnage_nc_wosub" = "#870023",
                                "tonnage_nc_wsub" = "#540016"),
                     guide = F) +
  facet_wrap(~ dv , scales = "free") + #Interprets as plotmath expression
  theme(panel.grid.minor = element_blank(),
        legend.position = "top",
        strip.text = element_text(size = 10)) +
  geom_blank(aes(y = ymax)) +
  geom_blank(aes(y = ymin)) +
  scale_y_continuous(breaks = seq(-1, 5, 1))

# Figure 26

sub_a <- sub %>%
  filter(vars %in% c(expression("Joint democracy Polity"),
                     expression("Joint democracy Boix"),
                     expression("Joint democracy UDS"),
                     expression("Rivalry"),
                     expression("Defense pacts"),
                     expression("Alliances (continuous)"),
                     expression("Alliances (binary)"))) %>%
  mutate(ymax = ifelse(dv %in% "DV: ln Military expenditure index", 1.5, 5),
         ymin = ifelse(dv %in% "DV: ln Military expenditure index", -2, -2))

ggplot(subset(sub_a, controls %in% c("Basic controls + alternative PT")),
       aes(x = variant, 
           y = coefs,
           shape = type_control,
           color = colcat_sub,
           ymin = lower95, 
           ymax = upper95)) +
  geom_hline(yintercept = 0, colour = gray(1/2), lty = 2, alpha = 0.8) +
  geom_point(position = position_dodge(width = 1/2), size = 2.5) +
  geom_linerange(aes(), size = .5, position = position_dodge(width = 1/2)) +
  theme_bw() +
  coord_flip() +
  labs(title = "Influence of potential threat on arming and power projection (1816-2012)",
       x = "",
       y = expression("Coefficient of standardized potential threat variable"["i, t-1"])) +
  scale_shape_manual(name = "",
                     values = c(17, 16)) +
  scale_color_manual(name = "Controlling for potential threat from population",
                     values = c("milex_cc_wosub" = "#006687",
                                "milex_cc_wsub" = "#003f54",
                                "tonnage_cc_wosub" = "#870023",
                                "tonnage_cc_wsub" = "#540016"),
                     guide = F) +
  facet_grid(vars ~ dv , scales = "free_x") + #Interprets as plotmath expression
  theme(panel.grid.minor = element_blank(),
        legend.position = "top",
        strip.text = element_text(size = 9)) +
  geom_blank(aes(y = ymax)) +
  geom_blank(aes(y = ymin)) +
  scale_y_continuous(breaks = seq(-1, 5, 1))

# Figure 27

sub_b <- sub %>%
  filter(!(vars %in% c(expression("Joint democracy Polity"),
                       expression("Joint democracy Boix"),
                       expression("Joint democracy UDS"),
                       expression("Rivalry"),
                       expression("Defense pacts"),
                       expression("Alliances (continuous)"),
                       expression("Alliances (binary)"))))  %>%
  mutate(ymax = ifelse(dv %in% "DV: ln Military expenditure index", 1.5, 5),
         ymin = ifelse(dv %in% "DV: ln Military expenditure index", -2, -2))

ggplot(subset(sub_b, controls %in% c("Basic controls + alternative PT")),
       aes(x = variant, 
           y = coefs,
           shape = type_control,
           color = colcat_sub,
           ymin = lower95, 
           ymax = upper95)) +
  geom_hline(yintercept = 0, colour = gray(1/2), lty = 2, alpha = 0.8) +
  geom_point(position = position_dodge(width = 1/2), size = 2.5) +
  geom_linerange(aes(), size = .5, position = position_dodge(width = 1/2)) +
  theme_bw() +
  coord_flip() +
  labs(title = "Influence of potential threat on arming and power projection (1816-2012)",
       x = "",
       y = expression("Coefficient of standardized potential threat variable"["i, t-1"])) +
  scale_shape_manual(name = "",
                     values = c(17, 16)) +
  scale_color_manual(name = "Controlling for potential threat from population",
                     values = c("milex_cc_wosub" = "#006687",
                                "milex_cc_wsub" = "#003f54",
                                "tonnage_cc_wosub" = "#870023",
                                "tonnage_cc_wsub" = "#540016"),
                     guide = F) +
  facet_grid(vars ~ dv , scales = "free_x") + #Interprets as plotmath expression
  theme(panel.grid.minor = element_blank(),
        legend.position = "top",
        strip.text = element_text(size = 9)) +
  geom_blank(aes(y = ymax)) +
  geom_blank(aes(y = ymin)) +
  scale_y_continuous(breaks = seq(-1, 5, 1))

####################
#
# Post-1945 Analysis
#
####################

master <- master %>% filter(year >= 1946)

vars_comp <- c("scaled_comp_surplus1095_polity_invlog_nodenom_all_l1")
varlabels_comp <- c("$\\text{PT SDP Joint Democracy (Polity)}_{i,t-1}$")

#---------------------
# milex
#---------------------

### surplus1095
# FECY model, controls
model_milex_fecy_surplus1095 <- list()
modelfits_milex_fecy_surplus1095 <- list()
coefs_milex_fecy_surplus1095 <- c()
ses_milex_fecy_surplus1095 <- list()
r2_milex_fecy_surplus1095 <- c()
for(i in 1:length(vars_comp)) {
  modformula <- paste(dvval_surplus1095_milex, 
                      paste(vars_comp[i], 
                            controlval_surplus1095_fe[1], 
                            controlval_surplus1095_fe[2],
                            controlval_surplus1095_fe[3], sep = "+"), 
                      sep = "~")
  model <- plm(as.formula(modformula), data = master_tscs, model = "within", effect = "twoways")
  model_milex_fecy_surplus1095[[i]] <- model
  modelfits_milex_fecy_surplus1095[[i]] <- coef_test(model, vcov = "CR2", cluster = "individual", test = "Satterthwaite")
  coefs_milex_fecy_surplus1095[[i]] <- modelfits_milex_fecy_surplus1095[[i]]$beta
  ses_milex_fecy_surplus1095[[i]] <- modelfits_milex_fecy_surplus1095[[i]]$SE
  r2_milex_fecy_surplus1095[i] <- summary(model)$r.squared[2]
}


### surplus1095
# FECY model, controls alternative
model_milex_fecy_surplus1095alt <- list()
modelfits_milex_fecy_surplus1095alt <- list()
coefs_milex_fecy_surplus1095alt <- c()
ses_milex_fecy_surplus1095alt <- list()
r2_milex_fecy_surplus1095alt <- c()
for(i in 1:length(vars_comp)) {
  modformula <- paste(dvval_surplus1095_milex, 
                      paste(vars_comp[i], 
                            controlvalalt_surplus1095_fe[1], 
                            controlvalalt_surplus1095_fe[2], sep = "+"), 
                      sep = "~")
  model <- plm(as.formula(modformula), data = master_tscs, model = "within", effect = "twoways")
  model_milex_fecy_surplus1095alt[[i]] <- model
  modelfits_milex_fecy_surplus1095alt[[i]] <- coef_test(model, vcov = "CR2", cluster = "individual", test = "Satterthwaite")
  coefs_milex_fecy_surplus1095alt[[i]] <- modelfits_milex_fecy_surplus1095alt[[i]]$beta
  ses_milex_fecy_surplus1095alt[[i]] <- modelfits_milex_fecy_surplus1095alt[[i]]$SE
  r2_milex_fecy_surplus1095alt[i] <- summary(model)$r.squared[2]
}

### surplus1095, pop
# FECY model, controls
model_milex_fecy_surplus1095pop <- list()
modelfits_milex_fecy_surplus1095pop <- list()
coefs_milex_fecy_surplus1095pop <- c()
ses_milex_fecy_surplus1095pop <- c()
r2_milex_fecy_surplus1095pop <- c()
for(i in 1:length(vars_comp)) {
  modformula <- paste(dvval_surplus1095_milex, 
                      paste(vars_comp[i], 
                            vars_comppop[i], 
                            controlval_surplus1095_fe[1], 
                            controlval_surplus1095_fe[2],
                            controlval_surplus1095_fe[3], sep = "+"), 
                      sep = "~")
  model <- plm(as.formula(modformula), data = master_tscs, model = "within", effect = "twoways")
  model_milex_fecy_surplus1095pop[[i]] <- model
  modelfits_milex_fecy_surplus1095pop[[i]] <- coef_test(model, vcov = "CR2", cluster = "individual", test = "Satterthwaite")
  coefs_milex_fecy_surplus1095pop[[i]] <- modelfits_milex_fecy_surplus1095pop[[i]]$beta
  ses_milex_fecy_surplus1095pop[[i]] <- modelfits_milex_fecy_surplus1095pop[[i]]$SE
  r2_milex_fecy_surplus1095pop[i] <- summary(model)$r.squared[2]
}


### surplus1095, pop
# FECY model, controls alternative
model_milex_fecy_surplus1095popalt <- list()
modelfits_milex_fecy_surplus1095popalt <- list()
coefs_milex_fecy_surplus1095popalt <- c()
ses_milex_fecy_surplus1095popalt <- c()
r2_milex_fecy_surplus1095popalt <- c()
for(i in 1:length(vars_comp)) {
  modformula <- paste(dvval_surplus1095_milex, 
                      paste(vars_comp[i], 
                            vars_comppop[i], 
                            controlvalalt_surplus1095_fe[1], 
                            controlvalalt_surplus1095_fe[2], sep = "+"), 
                      sep = "~")
  model <- plm(as.formula(modformula), data = master_tscs, model = "within", effect = "twoways")
  model_milex_fecy_surplus1095popalt[[i]] <- model
  modelfits_milex_fecy_surplus1095popalt[[i]] <- coef_test(model, vcov = "CR2", cluster = "individual", test = "Satterthwaite")
  coefs_milex_fecy_surplus1095popalt[[i]] <- modelfits_milex_fecy_surplus1095popalt[[i]]$beta
  ses_milex_fecy_surplus1095popalt[[i]] <- modelfits_milex_fecy_surplus1095popalt[[i]]$SE
  r2_milex_fecy_surplus1095popalt[i] <- summary(model)$r.squared[2]
}


#---------------------
# tonnageimputed
#---------------------

### surplus1095
# FECY model, controls
model_tonnageimputed_fecy_surplus1095 <- list()
modelfits_tonnageimputed_fecy_surplus1095 <- list()
coefs_tonnageimputed_fecy_surplus1095 <- c()
ses_tonnageimputed_fecy_surplus1095 <- c()
r2_tonnageimputed_fecy_surplus1095 <- c()
for(i in 1:length(vars_comp)) {
  modformula <- paste(dvval_surplus1095_tonnageimputed, 
                      paste(vars_comp[i], 
                            controlval_surplus1095_fe[1], 
                            controlval_surplus1095_fe[2],
                            controlval_surplus1095_fe[3], sep = "+"), 
                      sep = "~")
  model <- plm(as.formula(modformula), data = master_tscs, model = "within", effect = "twoways")
  model_tonnageimputed_fecy_surplus1095[[i]] <- model
  modelfits_tonnageimputed_fecy_surplus1095[[i]] <- coef_test(model, vcov = "CR2", cluster = "individual", test = "Satterthwaite")
  coefs_tonnageimputed_fecy_surplus1095[[i]] <- modelfits_tonnageimputed_fecy_surplus1095[[i]]$beta
  ses_tonnageimputed_fecy_surplus1095[[i]] <- modelfits_tonnageimputed_fecy_surplus1095[[i]]$SE
  r2_tonnageimputed_fecy_surplus1095[i] <- summary(model)$r.squared[2]
}


# FECY model, controls alternative
model_tonnageimputed_fecy_surplus1095alt <- list()
modelfits_tonnageimputed_fecy_surplus1095alt <- list()
coefs_tonnageimputed_fecy_surplus1095alt <- c()
ses_tonnageimputed_fecy_surplus1095alt <- c()
r2_tonnageimputed_fecy_surplus1095alt <- c()
for(i in 1:length(vars_comp)) {
  modformula <- paste(dvval_surplus1095_tonnageimputed, 
                      paste(vars_comp[i], 
                            controlvalalt_surplus1095_fe[1], 
                            controlvalalt_surplus1095_fe[2], sep = "+"), 
                      sep = "~")
  model <- plm(as.formula(modformula), data = master_tscs, model = "within", effect = "twoways")
  model_tonnageimputed_fecy_surplus1095alt[[i]] <- model
  modelfits_tonnageimputed_fecy_surplus1095alt[[i]] <- coef_test(model, vcov = "CR2", cluster = "individual", test = "Satterthwaite")
  coefs_tonnageimputed_fecy_surplus1095alt[[i]] <- modelfits_tonnageimputed_fecy_surplus1095alt[[i]]$beta
  ses_tonnageimputed_fecy_surplus1095alt[[i]] <- modelfits_tonnageimputed_fecy_surplus1095alt[[i]]$SE
  r2_tonnageimputed_fecy_surplus1095alt[i] <- summary(model)$r.squared[2]
}


### surplus1095, pop
# FECY model, controls
model_tonnageimputed_fecy_surplus1095pop <- list()
modelfits_tonnageimputed_fecy_surplus1095pop <- list()
coefs_tonnageimputed_fecy_surplus1095pop <- c()
ses_tonnageimputed_fecy_surplus1095pop <- c()
r2_tonnageimputed_fecy_surplus1095pop <- c()
for(i in 1:length(vars_comp)) {
  modformula <- paste(dvval_surplus1095_tonnageimputed, 
                      paste(vars_comp[i], 
                            vars_comppop[i], 
                            controlval_surplus1095_fe[1], 
                            controlval_surplus1095_fe[2],
                            controlval_surplus1095_fe[3], sep = "+"), 
                      sep = "~")
  model <- plm(as.formula(modformula), data = master_tscs, model = "within", effect = "twoways")
  model_tonnageimputed_fecy_surplus1095pop[[i]] <- model
  modelfits_tonnageimputed_fecy_surplus1095pop[[i]] <- coef_test(model, vcov = "CR2", cluster = "individual", test = "Satterthwaite")
  coefs_tonnageimputed_fecy_surplus1095pop[[i]] <- modelfits_tonnageimputed_fecy_surplus1095pop[[i]]$beta
  ses_tonnageimputed_fecy_surplus1095pop[[i]] <- modelfits_tonnageimputed_fecy_surplus1095pop[[i]]$SE
  r2_tonnageimputed_fecy_surplus1095pop[i] <- summary(model)$r.squared[2]
}

# FECY model, controls alternative
model_tonnageimputed_fecy_surplus1095popalt <- list()
modelfits_tonnageimputed_fecy_surplus1095popalt <- list()
coefs_tonnageimputed_fecy_surplus1095popalt <- c()
ses_tonnageimputed_fecy_surplus1095popalt <- c()
r2_tonnageimputed_fecy_surplus1095popalt <- c()
for(i in 1:length(vars_comp)) {
  modformula <- paste(dvval_surplus1095_tonnageimputed, 
                      paste(vars_comp[i], 
                            vars_comppop[i], 
                            controlvalalt_surplus1095_fe[1], 
                            controlvalalt_surplus1095_fe[2], sep = "+"), 
                      sep = "~")
  model <- plm(as.formula(modformula), data = master_tscs, model = "within", effect = "twoways")
  model_tonnageimputed_fecy_surplus1095popalt[[i]] <- model
  modelfits_tonnageimputed_fecy_surplus1095popalt[[i]] <- coef_test(model, vcov = "CR2", cluster = "individual", test = "Satterthwaite")
  coefs_tonnageimputed_fecy_surplus1095popalt[[i]] <- modelfits_tonnageimputed_fecy_surplus1095popalt[[i]]$beta
  ses_tonnageimputed_fecy_surplus1095popalt[[i]] <- modelfits_tonnageimputed_fecy_surplus1095popalt[[i]]$SE
  r2_tonnageimputed_fecy_surplus1095popalt[i] <- summary(model)$r.squared[2]
}

#############################################
# savingcoefficients
#############################################
econ_surplus1095 <- data.frame(vars = str_replace_all(vars_comp, "_l1", ""),
                                  unlist(map(coefs_milex_fecy_surplus1095, 1)),
                                  unlist(map(ses_milex_fecy_surplus1095, 1)),
                                  unlist(map(coefs_milex_fecy_surplus1095pop, 1)),
                                  unlist(map(ses_milex_fecy_surplus1095pop, 1)),
                                  unlist(map(coefs_tonnageimputed_fecy_surplus1095, 1)),
                                  unlist(map(ses_tonnageimputed_fecy_surplus1095, 1)),
                                  unlist(map(coefs_tonnageimputed_fecy_surplus1095pop, 1)),
                                  unlist(map(ses_tonnageimputed_fecy_surplus1095pop, 1))) %>%
  
  gather(indicator, value, -vars) %>%
  mutate(indicator = str_replace_all(indicator, "unlist.map.", ""),
         indicator = str_replace_all(indicator, "\\.\\.[0-9]\\.\\.", "")) %>%
  separate(indicator, c("indicator", "dv", "model", "controls"), sep = "_") %>%
  spread(indicator, value) %>%
  mutate(lower95 = coefs - 1.96*ses, #95% CI
         lower99 = coefs - 2.58*ses, #99% CI
         upper95 = coefs + 1.96*ses,
         upper99 = coefs + 2.58*ses) %>%
  mutate(controls = str_replace_all(controls, "surpl5", "surplus1095")) %>%
  mutate(coef = "econ",
         type_control = "wsub")


econ_surplus1095_alt <- data.frame(vars = str_replace_all(vars_comp, "_l1", ""),
                                     unlist(map(coefs_milex_fecy_surplus1095alt, 1)),
                                     unlist(map(ses_milex_fecy_surplus1095alt, 1)),
                                     unlist(map(coefs_milex_fecy_surplus1095popalt, 1)),
                                     unlist(map(ses_milex_fecy_surplus1095popalt, 1)),
                                     unlist(map(coefs_tonnageimputed_fecy_surplus1095alt, 1)),
                                     unlist(map(ses_tonnageimputed_fecy_surplus1095alt, 1)),
                                     unlist(map(coefs_tonnageimputed_fecy_surplus1095popalt, 1)), 
                                     unlist(map(ses_tonnageimputed_fecy_surplus1095popalt, 1))) %>%
  
  gather(indicator, value, -vars) %>%
  mutate(indicator = str_replace_all(indicator, "unlist.map.", ""),
         indicator = str_replace_all(indicator, "\\.\\.[0-9]\\.\\.", "")) %>%
  separate(indicator, c("indicator", "dv", "model", "controls"), sep = "_") %>%
  spread(indicator, value) %>%
  mutate(lower95 = coefs - 1.96*ses, #95% CI
         lower99 = coefs - 2.58*ses, #99% CI
         upper95 = coefs + 1.96*ses,
         upper99 = coefs + 2.58*ses) %>%
  mutate(controls = str_replace_all(controls, "surpl5", "surplus1095")) %>%
  mutate(coef = "econ",
         type_control = "wosub")

####### Surplus 730

vars_comp <- c("scaled_comp_surplus730_polity_invlog_nodenom_all_l1")
varlabels_comp <- c("$\\text{PT SDP Joint Democracy (Polity)}_{i,t-1}$")

#---------------------
# milex
#---------------------

### surplus730
# FECY model, controls
model_milex_fecy_surplus730 <- list()
modelfits_milex_fecy_surplus730 <- list()
coefs_milex_fecy_surplus730 <- c()
ses_milex_fecy_surplus730 <- list()
r2_milex_fecy_surplus730 <- c()
for(i in 1:length(vars_comp)) {
  modformula <- paste(dvval_surplus730_milex, 
                      paste(vars_comp[i], 
                            controlval_surplus730_fe[1], 
                            controlval_surplus730_fe[2],
                            controlval_surplus730_fe[3], sep = "+"), 
                      sep = "~")
  model <- plm(as.formula(modformula), data = master_tscs, model = "within", effect = "twoways")
  model_milex_fecy_surplus730[[i]] <- model
  modelfits_milex_fecy_surplus730[[i]] <- coef_test(model, vcov = "CR2", cluster = "individual", test = "Satterthwaite")
  coefs_milex_fecy_surplus730[[i]] <- modelfits_milex_fecy_surplus730[[i]]$beta
  ses_milex_fecy_surplus730[[i]] <- modelfits_milex_fecy_surplus730[[i]]$SE
  r2_milex_fecy_surplus730[i] <- summary(model)$r.squared[2]
}


### surplus730
# FECY model, controls alternative
model_milex_fecy_surplus730alt <- list()
modelfits_milex_fecy_surplus730alt <- list()
coefs_milex_fecy_surplus730alt <- c()
ses_milex_fecy_surplus730alt <- list()
r2_milex_fecy_surplus730alt <- c()
for(i in 1:length(vars_comp)) {
  modformula <- paste(dvval_surplus730_milex, 
                      paste(vars_comp[i], 
                            controlvalalt_surplus730_fe[1], 
                            controlvalalt_surplus730_fe[2], sep = "+"), 
                      sep = "~")
  model <- plm(as.formula(modformula), data = master_tscs, model = "within", effect = "twoways")
  model_milex_fecy_surplus730alt[[i]] <- model
  modelfits_milex_fecy_surplus730alt[[i]] <- coef_test(model, vcov = "CR2", cluster = "individual", test = "Satterthwaite")
  coefs_milex_fecy_surplus730alt[[i]] <- modelfits_milex_fecy_surplus730alt[[i]]$beta
  ses_milex_fecy_surplus730alt[[i]] <- modelfits_milex_fecy_surplus730alt[[i]]$SE
  r2_milex_fecy_surplus730alt[i] <- summary(model)$r.squared[2]
}

### surplus730, pop
# FECY model, controls
model_milex_fecy_surplus730pop <- list()
modelfits_milex_fecy_surplus730pop <- list()
coefs_milex_fecy_surplus730pop <- c()
ses_milex_fecy_surplus730pop <- c()
r2_milex_fecy_surplus730pop <- c()
for(i in 1:length(vars_comp)) {
  modformula <- paste(dvval_surplus730_milex, 
                      paste(vars_comp[i], 
                            vars_comppop[i], 
                            controlval_surplus730_fe[1], 
                            controlval_surplus730_fe[2],
                            controlval_surplus730_fe[3], sep = "+"), 
                      sep = "~")
  model <- plm(as.formula(modformula), data = master_tscs, model = "within", effect = "twoways")
  model_milex_fecy_surplus730pop[[i]] <- model
  modelfits_milex_fecy_surplus730pop[[i]] <- coef_test(model, vcov = "CR2", cluster = "individual", test = "Satterthwaite")
  coefs_milex_fecy_surplus730pop[[i]] <- modelfits_milex_fecy_surplus730pop[[i]]$beta
  ses_milex_fecy_surplus730pop[[i]] <- modelfits_milex_fecy_surplus730pop[[i]]$SE
  r2_milex_fecy_surplus730pop[i] <- summary(model)$r.squared[2]
}


### surplus730, pop
# FECY model, controls alternative
model_milex_fecy_surplus730popalt <- list()
modelfits_milex_fecy_surplus730popalt <- list()
coefs_milex_fecy_surplus730popalt <- c()
ses_milex_fecy_surplus730popalt <- c()
r2_milex_fecy_surplus730popalt <- c()
for(i in 1:length(vars_comp)) {
  modformula <- paste(dvval_surplus730_milex, 
                      paste(vars_comp[i], 
                            vars_comppop[i], 
                            controlvalalt_surplus730_fe[1], 
                            controlvalalt_surplus730_fe[2], sep = "+"), 
                      sep = "~")
  model <- plm(as.formula(modformula), data = master_tscs, model = "within", effect = "twoways")
  model_milex_fecy_surplus730popalt[[i]] <- model
  modelfits_milex_fecy_surplus730popalt[[i]] <- coef_test(model, vcov = "CR2", cluster = "individual", test = "Satterthwaite")
  coefs_milex_fecy_surplus730popalt[[i]] <- modelfits_milex_fecy_surplus730popalt[[i]]$beta
  ses_milex_fecy_surplus730popalt[[i]] <- modelfits_milex_fecy_surplus730popalt[[i]]$SE
  r2_milex_fecy_surplus730popalt[i] <- summary(model)$r.squared[2]
}


#---------------------
# tonnageimputed
#---------------------


### surplus730
# FECY model, controls
model_tonnageimputed_fecy_surplus730 <- list()
modelfits_tonnageimputed_fecy_surplus730 <- list()
coefs_tonnageimputed_fecy_surplus730 <- c()
ses_tonnageimputed_fecy_surplus730 <- c()
r2_tonnageimputed_fecy_surplus730 <- c()
for(i in 1:length(vars_comp)) {
  modformula <- paste(dvval_surplus730_tonnageimputed, 
                      paste(vars_comp[i], 
                            controlval_surplus730_fe[1], 
                            controlval_surplus730_fe[2],
                            controlval_surplus730_fe[3], sep = "+"), 
                      sep = "~")
  model <- plm(as.formula(modformula), data = master_tscs, model = "within", effect = "twoways")
  model_tonnageimputed_fecy_surplus730[[i]] <- model
  modelfits_tonnageimputed_fecy_surplus730[[i]] <- coef_test(model, vcov = "CR2", cluster = "individual", test = "Satterthwaite")
  coefs_tonnageimputed_fecy_surplus730[[i]] <- modelfits_tonnageimputed_fecy_surplus730[[i]]$beta
  ses_tonnageimputed_fecy_surplus730[[i]] <- modelfits_tonnageimputed_fecy_surplus730[[i]]$SE
  r2_tonnageimputed_fecy_surplus730[i] <- summary(model)$r.squared[2]
}


# FECY model, controls alternative
model_tonnageimputed_fecy_surplus730alt <- list()
modelfits_tonnageimputed_fecy_surplus730alt <- list()
coefs_tonnageimputed_fecy_surplus730alt <- c()
ses_tonnageimputed_fecy_surplus730alt <- c()
r2_tonnageimputed_fecy_surplus730alt <- c()
for(i in 1:length(vars_comp)) {
  modformula <- paste(dvval_surplus730_tonnageimputed, 
                      paste(vars_comp[i], 
                            controlvalalt_surplus730_fe[1], 
                            controlvalalt_surplus730_fe[2], sep = "+"), 
                      sep = "~")
  model <- plm(as.formula(modformula), data = master_tscs, model = "within", effect = "twoways")
  model_tonnageimputed_fecy_surplus730alt[[i]] <- model
  modelfits_tonnageimputed_fecy_surplus730alt[[i]] <- coef_test(model, vcov = "CR2", cluster = "individual", test = "Satterthwaite")
  coefs_tonnageimputed_fecy_surplus730alt[[i]] <- modelfits_tonnageimputed_fecy_surplus730alt[[i]]$beta
  ses_tonnageimputed_fecy_surplus730alt[[i]] <- modelfits_tonnageimputed_fecy_surplus730alt[[i]]$SE
  r2_tonnageimputed_fecy_surplus730alt[i] <- summary(model)$r.squared[2]
}

### surplus730, pop
# FECY model, controls
model_tonnageimputed_fecy_surplus730pop <- list()
modelfits_tonnageimputed_fecy_surplus730pop <- list()
coefs_tonnageimputed_fecy_surplus730pop <- c()
ses_tonnageimputed_fecy_surplus730pop <- c()
r2_tonnageimputed_fecy_surplus730pop <- c()
for(i in 1:length(vars_comp)) {
  modformula <- paste(dvval_surplus730_tonnageimputed, 
                      paste(vars_comp[i], 
                            vars_comppop[i], 
                            controlval_surplus730_fe[1], 
                            controlval_surplus730_fe[2],
                            controlval_surplus730_fe[3], sep = "+"), 
                      sep = "~")
  model <- plm(as.formula(modformula), data = master_tscs, model = "within", effect = "twoways")
  model_tonnageimputed_fecy_surplus730pop[[i]] <- model
  modelfits_tonnageimputed_fecy_surplus730pop[[i]] <- coef_test(model, vcov = "CR2", cluster = "individual", test = "Satterthwaite")
  coefs_tonnageimputed_fecy_surplus730pop[[i]] <- modelfits_tonnageimputed_fecy_surplus730pop[[i]]$beta
  ses_tonnageimputed_fecy_surplus730pop[[i]] <- modelfits_tonnageimputed_fecy_surplus730pop[[i]]$SE
  r2_tonnageimputed_fecy_surplus730pop[i] <- summary(model)$r.squared[2]
}

# FECY model, controls alternative
model_tonnageimputed_fecy_surplus730popalt <- list()
modelfits_tonnageimputed_fecy_surplus730popalt <- list()
coefs_tonnageimputed_fecy_surplus730popalt <- c()
ses_tonnageimputed_fecy_surplus730popalt <- c()
r2_tonnageimputed_fecy_surplus730popalt <- c()
for(i in 1:length(vars_comp)) {
  modformula <- paste(dvval_surplus730_tonnageimputed, 
                      paste(vars_comp[i], 
                            vars_comppop[i], 
                            controlvalalt_surplus730_fe[1], 
                            controlvalalt_surplus730_fe[2], sep = "+"), 
                      sep = "~")
  model <- plm(as.formula(modformula), data = master_tscs, model = "within", effect = "twoways")
  model_tonnageimputed_fecy_surplus730popalt[[i]] <- model
  modelfits_tonnageimputed_fecy_surplus730popalt[[i]] <- coef_test(model, vcov = "CR2", cluster = "individual", test = "Satterthwaite")
  coefs_tonnageimputed_fecy_surplus730popalt[[i]] <- modelfits_tonnageimputed_fecy_surplus730popalt[[i]]$beta
  ses_tonnageimputed_fecy_surplus730popalt[[i]] <- modelfits_tonnageimputed_fecy_surplus730popalt[[i]]$SE
  r2_tonnageimputed_fecy_surplus730popalt[i] <- summary(model)$r.squared[2]
}

#############################################
# savingcoefficients
#############################################
econ_surplus730 <- data.frame(vars = str_replace_all(vars_comp, "_l1", ""),
                                  unlist(map(coefs_milex_fecy_surplus730, 1)),
                                  unlist(map(ses_milex_fecy_surplus730, 1)),
                                  unlist(map(coefs_milex_fecy_surplus730pop, 1)),
                                  unlist(map(ses_milex_fecy_surplus730pop, 1)),
                                  unlist(map(coefs_tonnageimputed_fecy_surplus730, 1)),
                                  unlist(map(ses_tonnageimputed_fecy_surplus730, 1)),
                                  unlist(map(coefs_tonnageimputed_fecy_surplus730pop, 1)),
                                  unlist(map(ses_tonnageimputed_fecy_surplus730pop, 1))) %>%
  
  gather(indicator, value, -vars) %>%
  mutate(indicator = str_replace_all(indicator, "unlist.map.", ""),
         indicator = str_replace_all(indicator, "\\.\\.[0-9]\\.\\.", "")) %>%
  separate(indicator, c("indicator", "dv", "model", "controls"), sep = "_") %>%
  spread(indicator, value) %>%
  mutate(lower95 = coefs - 1.96*ses, #95% CI
         lower99 = coefs - 2.58*ses, #99% CI
         upper95 = coefs + 1.96*ses,
         upper99 = coefs + 2.58*ses) %>%
  mutate(coef = "econ",
         type_control = "wsub")

econ_surplus730_alt <- data.frame(vars = str_replace_all(vars_comp, "_l1", ""),
                                    unlist(map(coefs_milex_fecy_surplus730alt, 1)),
                                    unlist(map(ses_milex_fecy_surplus730alt, 1)),
                                     unlist(map(coefs_milex_fecy_surplus730popalt, 1)),
                                     unlist(map(ses_milex_fecy_surplus730popalt, 1)),
                                    unlist(map(coefs_tonnageimputed_fecy_surplus730alt, 1)),
                                    unlist(map(ses_tonnageimputed_fecy_surplus730alt, 1)),
                                     unlist(map(coefs_tonnageimputed_fecy_surplus730popalt, 1)),
                                     unlist(map(ses_tonnageimputed_fecy_surplus730popalt, 1))) %>%
  
  gather(indicator, value, -vars) %>%
  mutate(indicator = str_replace_all(indicator, "unlist.map.", ""),
         indicator = str_replace_all(indicator, "\\.\\.[0-9]\\.\\.", "")) %>%
  separate(indicator, c("indicator", "dv", "model", "controls"), sep = "_") %>%
  spread(indicator, value) %>%
  mutate(lower95 = coefs - 1.96*ses, #95% CI
         lower99 = coefs - 2.58*ses, #99% CI
         upper95 = coefs + 1.96*ses,
         upper99 = coefs + 2.58*ses) %>%
  mutate(coef = "econ",
         type_control = "wosub")

########## Surplus 365

vars_comp <- c("scaled_comp_surplus365_polity_invlog_nodenom_all_l1")
varlabels_comp <- c("$\\text{PT SDP Joint Democracy (Polity)}_{i,t-1}$")

#---------------------
# milex
#---------------------

### surplus365
# FECY model, controls
model_milex_fecy_surplus365 <- list()
modelfits_milex_fecy_surplus365 <- list()
coefs_milex_fecy_surplus365 <- c()
ses_milex_fecy_surplus365 <- list()
r2_milex_fecy_surplus365 <- c()
for(i in 1:length(vars_comp)) {
  modformula <- paste(dvval_surplus365_milex, 
                      paste(vars_comp[i], 
                            controlval_surplus365_fe[1], 
                            controlval_surplus365_fe[2],
                            controlval_surplus365_fe[3], sep = "+"), 
                      sep = "~")
  model <- plm(as.formula(modformula), data = master_tscs, model = "within", effect = "twoways")
  model_milex_fecy_surplus365[[i]] <- model
  modelfits_milex_fecy_surplus365[[i]] <- coef_test(model, vcov = "CR2", cluster = "individual", test = "Satterthwaite")
  coefs_milex_fecy_surplus365[[i]] <- modelfits_milex_fecy_surplus365[[i]]$beta
  ses_milex_fecy_surplus365[[i]] <- modelfits_milex_fecy_surplus365[[i]]$SE
  r2_milex_fecy_surplus365[i] <- summary(model)$r.squared[2]
}


### surplus365
# FECY model, controls alternative
model_milex_fecy_surplus365alt <- list()
modelfits_milex_fecy_surplus365alt <- list()
coefs_milex_fecy_surplus365alt <- c()
ses_milex_fecy_surplus365alt <- list()
r2_milex_fecy_surplus365alt <- c()
for(i in 1:length(vars_comp)) {
  modformula <- paste(dvval_surplus365_milex, 
                      paste(vars_comp[i], 
                            controlvalalt_surplus365_fe[1], 
                            controlvalalt_surplus365_fe[2], sep = "+"), 
                      sep = "~")
  model <- plm(as.formula(modformula), data = master_tscs, model = "within", effect = "twoways")
  model_milex_fecy_surplus365alt[[i]] <- model
  modelfits_milex_fecy_surplus365alt[[i]] <- coef_test(model, vcov = "CR2", cluster = "individual", test = "Satterthwaite")
  coefs_milex_fecy_surplus365alt[[i]] <- modelfits_milex_fecy_surplus365alt[[i]]$beta
  ses_milex_fecy_surplus365alt[[i]] <- modelfits_milex_fecy_surplus365alt[[i]]$SE
  r2_milex_fecy_surplus365alt[i] <- summary(model)$r.squared[2]
}

### surplus365, pop
# FECY model, controls
model_milex_fecy_surplus365pop <- list()
modelfits_milex_fecy_surplus365pop <- list()
coefs_milex_fecy_surplus365pop <- c()
ses_milex_fecy_surplus365pop <- c()
r2_milex_fecy_surplus365pop <- c()
for(i in 1:length(vars_comp)) {
  modformula <- paste(dvval_surplus365_milex, 
                      paste(vars_comp[i], 
                            vars_comppop[i], 
                            controlval_surplus365_fe[1], 
                            controlval_surplus365_fe[2],
                            controlval_surplus365_fe[3], sep = "+"), 
                      sep = "~")
  model <- plm(as.formula(modformula), data = master_tscs, model = "within", effect = "twoways")
  model_milex_fecy_surplus365pop[[i]] <- model
  modelfits_milex_fecy_surplus365pop[[i]] <- coef_test(model, vcov = "CR2", cluster = "individual", test = "Satterthwaite")
  coefs_milex_fecy_surplus365pop[[i]] <- modelfits_milex_fecy_surplus365pop[[i]]$beta
  ses_milex_fecy_surplus365pop[[i]] <- modelfits_milex_fecy_surplus365pop[[i]]$SE
  r2_milex_fecy_surplus365pop[i] <- summary(model)$r.squared[2]
}


### surplus365, pop
# FECY model, controls alternative
model_milex_fecy_surplus365popalt <- list()
modelfits_milex_fecy_surplus365popalt <- list()
coefs_milex_fecy_surplus365popalt <- c()
ses_milex_fecy_surplus365popalt <- c()
r2_milex_fecy_surplus365popalt <- c()
for(i in 1:length(vars_comp)) {
  modformula <- paste(dvval_surplus365_milex, 
                      paste(vars_comp[i], 
                            vars_comppop[i], 
                            controlvalalt_surplus365_fe[1], 
                            controlvalalt_surplus365_fe[2], sep = "+"), 
                      sep = "~")
  model <- plm(as.formula(modformula), data = master_tscs, model = "within", effect = "twoways")
  model_milex_fecy_surplus365popalt[[i]] <- model
  modelfits_milex_fecy_surplus365popalt[[i]] <- coef_test(model, vcov = "CR2", cluster = "individual", test = "Satterthwaite")
  coefs_milex_fecy_surplus365popalt[[i]] <- modelfits_milex_fecy_surplus365popalt[[i]]$beta
  ses_milex_fecy_surplus365popalt[[i]] <- modelfits_milex_fecy_surplus365popalt[[i]]$SE
  r2_milex_fecy_surplus365popalt[i] <- summary(model)$r.squared[2]
}


#---------------------
# tonnageimputed
#---------------------

### surplus365
# FECY model, controls
model_tonnageimputed_fecy_surplus365 <- list()
modelfits_tonnageimputed_fecy_surplus365 <- list()
coefs_tonnageimputed_fecy_surplus365 <- c()
ses_tonnageimputed_fecy_surplus365 <- c()
r2_tonnageimputed_fecy_surplus365 <- c()
for(i in 1:length(vars_comp)) {
  modformula <- paste(dvval_surplus365_tonnageimputed, 
                      paste(vars_comp[i], 
                            controlval_surplus365_fe[1], 
                            controlval_surplus365_fe[2],
                            controlval_surplus365_fe[3], sep = "+"), 
                      sep = "~")
  model <- plm(as.formula(modformula), data = master_tscs, model = "within", effect = "twoways")
  model_tonnageimputed_fecy_surplus365[[i]] <- model
  modelfits_tonnageimputed_fecy_surplus365[[i]] <- coef_test(model, vcov = "CR2", cluster = "individual", test = "Satterthwaite")
  coefs_tonnageimputed_fecy_surplus365[[i]] <- modelfits_tonnageimputed_fecy_surplus365[[i]]$beta
  ses_tonnageimputed_fecy_surplus365[[i]] <- modelfits_tonnageimputed_fecy_surplus365[[i]]$SE
  r2_tonnageimputed_fecy_surplus365[i] <- summary(model)$r.squared[2]
}


# FECY model, controls alternative
model_tonnageimputed_fecy_surplus365alt <- list()
modelfits_tonnageimputed_fecy_surplus365alt <- list()
coefs_tonnageimputed_fecy_surplus365alt <- c()
ses_tonnageimputed_fecy_surplus365alt <- c()
r2_tonnageimputed_fecy_surplus365alt <- c()
for(i in 1:length(vars_comp)) {
  modformula <- paste(dvval_surplus365_tonnageimputed, 
                      paste(vars_comp[i], 
                            controlvalalt_surplus365_fe[1], 
                            controlvalalt_surplus365_fe[2], sep = "+"), 
                      sep = "~")
  model <- plm(as.formula(modformula), data = master_tscs, model = "within", effect = "twoways")
  model_tonnageimputed_fecy_surplus365alt[[i]] <- model
  modelfits_tonnageimputed_fecy_surplus365alt[[i]] <- coef_test(model, vcov = "CR2", cluster = "individual", test = "Satterthwaite")
  coefs_tonnageimputed_fecy_surplus365alt[[i]] <- modelfits_tonnageimputed_fecy_surplus365alt[[i]]$beta
  ses_tonnageimputed_fecy_surplus365alt[[i]] <- modelfits_tonnageimputed_fecy_surplus365alt[[i]]$SE
  r2_tonnageimputed_fecy_surplus365alt[i] <- summary(model)$r.squared[2]
}

### surplus365, pop
# FECY model, controls
model_tonnageimputed_fecy_surplus365pop <- list()
modelfits_tonnageimputed_fecy_surplus365pop <- list()
coefs_tonnageimputed_fecy_surplus365pop <- c()
ses_tonnageimputed_fecy_surplus365pop <- c()
r2_tonnageimputed_fecy_surplus365pop <- c()
for(i in 1:length(vars_comp)) {
  modformula <- paste(dvval_surplus365_tonnageimputed, 
                      paste(vars_comp[i], 
                            vars_comppop[i], 
                            controlval_surplus365_fe[1], 
                            controlval_surplus365_fe[2],
                            controlval_surplus365_fe[3], sep = "+"), 
                      sep = "~")
  model <- plm(as.formula(modformula), data = master_tscs, model = "within", effect = "twoways")
  model_tonnageimputed_fecy_surplus365pop[[i]] <- model
  modelfits_tonnageimputed_fecy_surplus365pop[[i]] <- coef_test(model, vcov = "CR2", cluster = "individual", test = "Satterthwaite")
  coefs_tonnageimputed_fecy_surplus365pop[[i]] <- modelfits_tonnageimputed_fecy_surplus365pop[[i]]$beta
  ses_tonnageimputed_fecy_surplus365pop[[i]] <- modelfits_tonnageimputed_fecy_surplus365pop[[i]]$SE
  r2_tonnageimputed_fecy_surplus365pop[i] <- summary(model)$r.squared[2]
}

# FECY model, controls alternative
model_tonnageimputed_fecy_surplus365popalt <- list()
modelfits_tonnageimputed_fecy_surplus365popalt <- list()
coefs_tonnageimputed_fecy_surplus365popalt <- c()
ses_tonnageimputed_fecy_surplus365popalt <- c()
r2_tonnageimputed_fecy_surplus365popalt <- c()
for(i in 1:length(vars_comp)) {
  modformula <- paste(dvval_surplus365_tonnageimputed, 
                      paste(vars_comp[i], 
                            vars_comppop[i], 
                            controlvalalt_surplus365_fe[1], 
                            controlvalalt_surplus365_fe[2], sep = "+"), 
                      sep = "~")
  model <- plm(as.formula(modformula), data = master_tscs, model = "within", effect = "twoways")
  model_tonnageimputed_fecy_surplus365popalt[[i]] <- model
  modelfits_tonnageimputed_fecy_surplus365popalt[[i]] <- coef_test(model, vcov = "CR2", cluster = "individual", test = "Satterthwaite")
  coefs_tonnageimputed_fecy_surplus365popalt[[i]] <- modelfits_tonnageimputed_fecy_surplus365popalt[[i]]$beta
  ses_tonnageimputed_fecy_surplus365popalt[[i]] <- modelfits_tonnageimputed_fecy_surplus365popalt[[i]]$SE
  r2_tonnageimputed_fecy_surplus365popalt[i] <- summary(model)$r.squared[2]
}

#############################################
# savingcoefficients
#############################################
econ_surplus365 <- data.frame(vars = str_replace_all(vars_comp, "_l1", ""),
                                 unlist(map(coefs_milex_fecy_surplus365, 1)),
                                 unlist(map(ses_milex_fecy_surplus365, 1)),
                                  unlist(map(coefs_milex_fecy_surplus365pop, 1)),
                                  unlist(map(ses_milex_fecy_surplus365pop, 1)),
                                 unlist(map(coefs_tonnageimputed_fecy_surplus365, 1)),
                                 unlist(map(ses_tonnageimputed_fecy_surplus365, 1)),
                                  unlist(map(coefs_tonnageimputed_fecy_surplus365pop, 1)),
                                  unlist(map(ses_tonnageimputed_fecy_surplus365pop, 1))) %>%
  
  gather(indicator, value, -vars) %>%
  mutate(indicator = str_replace_all(indicator, "unlist.map.", ""),
         indicator = str_replace_all(indicator, "\\.\\.[0-9]\\.\\.", "")) %>%
  separate(indicator, c("indicator", "dv", "model", "controls"), sep = "_") %>%
  spread(indicator, value) %>%
  mutate(lower95 = coefs - 1.96*ses, #95% CI
         lower99 = coefs - 2.58*ses, #99% CI
         upper95 = coefs + 1.96*ses,
         upper99 = coefs + 2.58*ses) %>%
  mutate(coef = "econ",
         type_control = "wsub")

econ_surplus365_alt <- data.frame(vars = str_replace_all(vars_comp, "_l1", ""),
                                    unlist(map(coefs_milex_fecy_surplus365alt, 1)),
                                    unlist(map(ses_milex_fecy_surplus365alt, 1)),
                                     unlist(map(coefs_milex_fecy_surplus365popalt, 1)),
                                     unlist(map(ses_milex_fecy_surplus365popalt, 1)),
                                    unlist(map(coefs_tonnageimputed_fecy_surplus365alt, 1)),
                                    unlist(map(ses_tonnageimputed_fecy_surplus365alt, 1)),
                                     unlist(map(coefs_tonnageimputed_fecy_surplus365popalt, 1)),
                                     unlist(map(ses_tonnageimputed_fecy_surplus365popalt, 1))) %>%
  
  gather(indicator, value, -vars) %>%
  mutate(indicator = str_replace_all(indicator, "unlist.map.", ""),
         indicator = str_replace_all(indicator, "\\.\\.[0-9]\\.\\.", "")) %>%
  separate(indicator, c("indicator", "dv", "model", "controls"), sep = "_") %>%
  spread(indicator, value) %>%
  mutate(lower95 = coefs - 1.96*ses, #95% CI
         lower99 = coefs - 2.58*ses, #99% CI
         upper95 = coefs + 1.96*ses,
         upper99 = coefs + 2.58*ses) %>%
  mutate(coef = "econ",
         type_control = "wosub")

####### GDP ($0 surplus)

vars_comp <- c("scaled_comp_gdp_polity_invlog_nodenom_all_l1")

varlabels_comp <- c("$\\text{PT GDP Joint Democracy (Polity)}_{i,t-1}$")


### gdp
# FECY model, controls
model_milex_fecy_gdp <- list()
modelfits_milex_fecy_gdp <- list()
coefs_milex_fecy_gdp <- c()
ses_milex_fecy_gdp <- list()
r2_milex_fecy_gdp <- c()
for(i in 1:length(vars_comp)) {
  modformula <- paste(dvval_gdp_milex, 
                      paste(vars_comp[i], 
                            controlval_gdp_fe[1], 
                            controlval_gdp_fe[2],
                            controlval_gdp_fe[3], sep = "+"), 
                      sep = "~")
  model <- plm(as.formula(modformula), data = master_tscs, model = "within", effect = "twoways")
  model_milex_fecy_gdp[[i]] <- model
  modelfits_milex_fecy_gdp[[i]] <- coef_test(model, vcov = "CR2", cluster = "individual", test = "Satterthwaite")
  coefs_milex_fecy_gdp[[i]] <- modelfits_milex_fecy_gdp[[i]]$beta
  ses_milex_fecy_gdp[[i]] <- modelfits_milex_fecy_gdp[[i]]$SE
  r2_milex_fecy_gdp[i] <- summary(model)$r.squared[2]
}


### gdp
# FECY model, alternative controls
model_milex_fecy_gdpalt <- list()
modelfits_milex_fecy_gdpalt <- list()
coefs_milex_fecy_gdpalt <- c()
ses_milex_fecy_gdpalt <- list()
r2_milex_fecy_gdpalt <- c()
for(i in 1:length(vars_comp)) {
  modformula <- paste(dvval_gdp_milex, 
                      paste(vars_comp[i], 
                            controlvalalt_gdp_fe[1], 
                            controlvalalt_gdp_fe[2], sep = "+"), 
                      sep = "~")
  model <- plm(as.formula(modformula), data = master_tscs, model = "within", effect = "twoways")
  model_milex_fecy_gdpalt[[i]] <- model
  modelfits_milex_fecy_gdpalt[[i]] <- coef_test(model, vcov = "CR2", cluster = "individual", test = "Satterthwaite")
  coefs_milex_fecy_gdpalt[[i]] <- modelfits_milex_fecy_gdpalt[[i]]$beta
  ses_milex_fecy_gdpalt[[i]] <- modelfits_milex_fecy_gdpalt[[i]]$SE
  r2_milex_fecy_gdpalt[i] <- summary(model)$r.squared[2]
}


### gdp, pop
# FECY model, controls
model_milex_fecy_gdppop <- list()
modelfits_milex_fecy_gdppop <- list()
coefs_milex_fecy_gdppop <- c()
ses_milex_fecy_gdppop <- c()
r2_milex_fecy_gdppop <- c()
for(i in 1:length(vars_comp)) {
  modformula <- paste(dvval_gdp_milex, 
                      paste(vars_comp[i], 
                            vars_comppop[i], 
                            controlval_gdp_fe[1], 
                            controlval_gdp_fe[2],
                            controlval_gdp_fe[3], sep = "+"), 
                      sep = "~")
  model <- plm(as.formula(modformula), data = master_tscs, model = "within", effect = "twoways")
  model_milex_fecy_gdppop[[i]] <- model
  modelfits_milex_fecy_gdppop[[i]] <- coef_test(model, vcov = "CR2", cluster = "individual", test = "Satterthwaite")
  coefs_milex_fecy_gdppop[[i]] <- modelfits_milex_fecy_gdppop[[i]]$beta
  ses_milex_fecy_gdppop[[i]] <- modelfits_milex_fecy_gdppop[[i]]$SE
  r2_milex_fecy_gdppop[i] <- summary(model)$r.squared[2]
}

### gdp, pop
# FECY model, controls alternative
model_milex_fecy_gdppopalt <- list()
modelfits_milex_fecy_gdppopalt <- list()
coefs_milex_fecy_gdppopalt <- c()
ses_milex_fecy_gdppopalt <- c()
r2_milex_fecy_gdppopalt <- c()
for(i in 1:length(vars_comp)) {
  modformula <- paste(dvval_gdp_milex, 
                      paste(vars_comp[i], 
                            vars_comppop[i], 
                            controlvalalt_gdp_fe[1], 
                            controlvalalt_gdp_fe[2], sep = "+"), 
                      sep = "~")
  model <- plm(as.formula(modformula), data = master_tscs, model = "within", effect = "twoways")
  model_milex_fecy_gdppopalt[[i]] <- model
  modelfits_milex_fecy_gdppopalt[[i]] <- coef_test(model, vcov = "CR2", cluster = "individual", test = "Satterthwaite")
  coefs_milex_fecy_gdppopalt[[i]] <- modelfits_milex_fecy_gdppopalt[[i]]$beta
  ses_milex_fecy_gdppopalt[[i]] <- modelfits_milex_fecy_gdppopalt[[i]]$SE
  r2_milex_fecy_gdppopalt[i] <- summary(model)$r.squared[2]
}

### gdp
# FECY model, controls
model_tonnageimputed_fecy_gdp <- list()
modelfits_tonnageimputed_fecy_gdp <- list()
coefs_tonnageimputed_fecy_gdp <- c()
ses_tonnageimputed_fecy_gdp <- c()
r2_tonnageimputed_fecy_gdp <- c()
for(i in 1:length(vars_comp)) {
  modformula <- paste(dvval_gdp_tonnageimputed, 
                      paste(vars_comp[i], 
                            controlval_gdp_fe[1], 
                            controlval_gdp_fe[2],
                            controlval_gdp_fe[3], sep = "+"), 
                      sep = "~")
  model <- plm(as.formula(modformula), data = master_tscs, model = "within", effect = "twoways")
  model_tonnageimputed_fecy_gdp[[i]] <- model
  modelfits_tonnageimputed_fecy_gdp[[i]] <- coef_test(model, vcov = "CR2", cluster = "individual", test = "Satterthwaite")
  coefs_tonnageimputed_fecy_gdp[[i]] <- modelfits_tonnageimputed_fecy_gdp[[i]]$beta
  ses_tonnageimputed_fecy_gdp[[i]] <- modelfits_tonnageimputed_fecy_gdp[[i]]$SE
  r2_tonnageimputed_fecy_gdp[i] <- summary(model)$r.squared[2]
}

### gdp
# FECY model, alternative controls
model_tonnageimputed_fecy_gdpalt <- list()
modelfits_tonnageimputed_fecy_gdpalt <- list()
coefs_tonnageimputed_fecy_gdpalt <- c()
ses_tonnageimputed_fecy_gdpalt <- list()
r2_tonnageimputed_fecy_gdpalt <- c()
for(i in 1:length(vars_comp)) {
  modformula <- paste(dvval_gdp_tonnageimputed, 
                      paste(vars_comp[i], 
                            controlvalalt_gdp_fe[1], 
                            controlvalalt_gdp_fe[2], sep = "+"), 
                      sep = "~")
  model <- plm(as.formula(modformula), data = master_tscs, model = "within", effect = "twoways")
  model_tonnageimputed_fecy_gdpalt[[i]] <- model
  modelfits_tonnageimputed_fecy_gdpalt[[i]] <- coef_test(model, vcov = "CR2", cluster = "individual", test = "Satterthwaite")
  coefs_tonnageimputed_fecy_gdpalt[[i]] <- modelfits_tonnageimputed_fecy_gdpalt[[i]]$beta
  ses_tonnageimputed_fecy_gdpalt[[i]] <- modelfits_tonnageimputed_fecy_gdpalt[[i]]$SE
  r2_tonnageimputed_fecy_gdpalt[i] <- summary(model)$r.squared[2]
}


### gdp, pop
# FECY model, controls
model_tonnageimputed_fecy_gdppop <- list()
modelfits_tonnageimputed_fecy_gdppop <- list()
coefs_tonnageimputed_fecy_gdppop <- c()
ses_tonnageimputed_fecy_gdppop <- c()
r2_tonnageimputed_fecy_gdppop <- c()
for(i in 1:length(vars_comp)) {
  modformula <- paste(dvval_gdp_tonnageimputed, 
                      paste(vars_comp[i], 
                            vars_comppop[i], 
                            controlval_gdp_fe[1], 
                            controlval_gdp_fe[2],
                            controlval_gdp_fe[3], sep = "+"), 
                      sep = "~")
  model <- plm(as.formula(modformula), data = master_tscs, model = "within", effect = "twoways")
  model_tonnageimputed_fecy_gdppop[[i]] <- model
  modelfits_tonnageimputed_fecy_gdppop[[i]] <- coef_test(model, vcov = "CR2", cluster = "individual", test = "Satterthwaite")
  coefs_tonnageimputed_fecy_gdppop[[i]] <- modelfits_tonnageimputed_fecy_gdppop[[i]]$beta
  ses_tonnageimputed_fecy_gdppop[[i]] <- modelfits_tonnageimputed_fecy_gdppop[[i]]$SE
  r2_tonnageimputed_fecy_gdppop[i] <- summary(model)$r.squared[2]
}


### gdp, pop
# FECY model, controls alternative
model_tonnageimputed_fecy_gdppopalt <- list()
modelfits_tonnageimputed_fecy_gdppopalt <- list()
coefs_tonnageimputed_fecy_gdppopalt <- c()
ses_tonnageimputed_fecy_gdppopalt <- c()
r2_tonnageimputed_fecy_gdppopalt <- c()
for(i in 1:length(vars_comp)) {
  modformula <- paste(dvval_gdp_tonnageimputed, 
                      paste(vars_comp[i], 
                            vars_comppop[i], 
                            controlvalalt_gdp_fe[1], 
                            controlvalalt_gdp_fe[2], sep = "+"), 
                      sep = "~")
  model <- plm(as.formula(modformula), data = master_tscs, model = "within", effect = "twoways")
  model_tonnageimputed_fecy_gdppopalt[[i]] <- model
  modelfits_tonnageimputed_fecy_gdppopalt[[i]] <- coef_test(model, vcov = "CR2", cluster = "individual", test = "Satterthwaite")
  coefs_tonnageimputed_fecy_gdppopalt[[i]] <- modelfits_tonnageimputed_fecy_gdppopalt[[i]]$beta
  ses_tonnageimputed_fecy_gdppopalt[[i]] <- modelfits_tonnageimputed_fecy_gdppopalt[[i]]$SE
  r2_tonnageimputed_fecy_gdppopalt[i] <- summary(model)$r.squared[2]
}

#############################################
# savingcoefficients
#############################################
econ_gdp <- data.frame(vars = str_replace_all(vars_comp, "_l1", ""),
                          unlist(map(coefs_milex_fecy_gdp, 1)),
                          unlist(map(ses_milex_fecy_gdp, 1)),
                                 unlist(map(coefs_milex_fecy_gdppop, 1)),
                                 unlist(map(ses_milex_fecy_gdppop, 1)),
                          unlist(map(coefs_tonnageimputed_fecy_gdp, 1)),
                          unlist(map(ses_tonnageimputed_fecy_gdp, 1)),
                                 unlist(map(coefs_tonnageimputed_fecy_gdppop, 1)),
                                 unlist(map(ses_tonnageimputed_fecy_gdppop, 1))) %>%
  
  gather(indicator, value, -vars) %>%
  mutate(indicator = str_replace_all(indicator, "unlist.map.", ""),
         indicator = str_replace_all(indicator, "\\.\\.[0-9]\\.\\.", "")) %>%
  separate(indicator, c("indicator", "dv", "model", "controls"), sep = "_") %>%
  spread(indicator, value) %>%
  mutate(lower95 = coefs - 1.96*ses, #95% CI
         lower99 = coefs - 2.58*ses, #99% CI
         upper95 = coefs + 1.96*ses,
         upper99 = coefs + 2.58*ses) %>%
  mutate(coef = "econ",
         type_control = "wsub") 


econ_gdp_alt <- data.frame(vars = str_replace_all(vars_comp, "_l1", ""),
                             unlist(map(coefs_milex_fecy_gdpalt, 1)),
                             unlist(map(ses_milex_fecy_gdpalt, 1)),
                                    unlist(map(coefs_milex_fecy_gdppopalt, 1)),
                                    unlist(map(ses_milex_fecy_gdppopalt, 1)),
                             unlist(map(coefs_tonnageimputed_fecy_gdpalt, 1)),
                             unlist(map(ses_tonnageimputed_fecy_gdpalt, 1)),
                                    unlist(map(coefs_tonnageimputed_fecy_gdppopalt, 1)),
                                    unlist(map(ses_tonnageimputed_fecy_gdppopalt, 1))) %>%
  
  gather(indicator, value, -vars) %>%
  mutate(indicator = str_replace_all(indicator, "unlist.map.", ""),
         indicator = str_replace_all(indicator, "\\.\\.[0-9]\\.\\.", "")) %>%
  separate(indicator, c("indicator", "dv", "model", "controls"), sep = "_") %>%
  spread(indicator, value) %>%
  mutate(lower95 = coefs - 1.96*ses, #95% CI
         lower99 = coefs - 2.58*ses, #99% CI
         upper95 = coefs + 1.96*ses,
         upper99 = coefs + 2.58*ses) %>%
  mutate(coef = "econ",
         type_control = "wosub") 

# Figure 25

# https://cran.r-project.org/web/packages/viridis/viridis.pdf
merged <- bind_rows(econ_surplus1095, 
                    econ_surplus730, 
                    econ_surplus365, 
                    econ_gdp,
                    econ_surplus1095_alt, 
                    econ_surplus730_alt, 
                    econ_surplus365_alt, 
                    econ_gdp_alt) %>%
  dplyr::mutate(vars = str_replace_all(vars, "scaled_comp_", "")) %>%
  dplyr::mutate(vars = str_replace_all(vars, "_invlog_nodenom_all", "")) %>%
  separate(vars, c("variant", "vars")) %>%
  
  #mutate(variant = replace(variant, str_detect(controls, "alt"), "gdpnopop")) %>%
  
  mutate(controls = stringr::str_remove_all(controls, "alt")) %>%
  
  mutate(controls = stringr::str_replace_all(controls, "gdp", "simple")) %>%
  mutate(controls = stringr::str_replace_all(controls, "pop", "simple")) %>%
  mutate(controls = stringr::str_replace_all(controls, "surplus1460", "simple")) %>%
  mutate(controls = stringr::str_replace_all(controls, "surplus1095", "simple")) %>%
  mutate(controls = stringr::str_replace_all(controls, "surplus730", "simple")) %>%
  mutate(controls = stringr::str_replace_all(controls, "surplus365", "simple")) %>%
  # Recoding milex
  mutate(colcat = ifelse(dv == "milex" & controls == "simple", "milex_c", NA)) %>%
  mutate(colcat = replace(colcat, dv == "milex" & controls == "simplenocontrols", "milex_nc")) %>%
  mutate(colcat = replace(colcat, dv == "milex" & controls == "simplesimple", "milex_cc")) %>%
  mutate(colcat = replace(colcat, dv == "milex" & controls == "simpleXsimple", "milex_i")) %>%
  
  # Recoding milper
  mutate(colcat = replace(colcat, dv == "milper" & controls == "simple", "milper_c")) %>%
  mutate(colcat = replace(colcat, dv == "milper" & controls == "simplenocontrols", "milper_nc")) %>%
  mutate(colcat = replace(colcat, dv == "milper" & controls == "simplesimple", "milper_cc")) %>%
  mutate(colcat = replace(colcat, dv == "milper" & controls == "simpleXsimple", "milper_i")) %>%
  
  # Recoding tonnageimputed
  mutate(colcat = replace(colcat, dv == "tonnageimputed" & controls == "simple", "tonnage_c")) %>%
  mutate(colcat = replace(colcat, dv == "tonnageimputed" & controls == "simplenocontrols", "tonnage_nc")) %>%
  mutate(colcat = replace(colcat, dv == "tonnageimputed" & controls == "simplesimple", "tonnage_cc")) %>%
  mutate(colcat = replace(colcat, dv == "tonnageimputed" & controls == "simpleXsimple", "tonnage_i")) %>%
  
  # milexp
  mutate(colcat = replace(colcat, dv == "milexp" & controls == "simple", "milexp_c")) %>%
  mutate(colcat = replace(colcat, dv == "milexp" & controls == "simplenocontrols", "milexp_nc")) %>%
  mutate(colcat = replace(colcat, dv == "milexp" & controls == "simplesimple", "milexp_cc")) %>%
  mutate(colcat = replace(colcat, dv == "milexp" & controls == "simpleXsimple", "milexp_i")) %>%
  filter(controls != "simpleXsimple",
         dv != "milexp") %>%
  
  mutate(colcat_sub = ifelse(type_control == "wsub", 
                             paste(colcat, "wsub", sep = "_"),
                             paste(colcat, "wosub", sep = "_")))

merged$model <- factor(merged$model,
                       levels = c("fec",
                                  "fecy"),
                       labels = c("Country Fixed Effects",
                                  "Two-Way Fixed Effects"))

merged$variant <- factor(merged$variant,
                         levels = rev(c("surplus1095", 
                                        "surplus730", 
                                        "surplus365", 
                                        "gdp")),
                         labels = rev(c("SDP 3USD",
                                        "SDP 2USD",
                                        "SDP 1USD",
                                        "GDP")))

merged$dv <- factor(merged$dv,
                    levels = c("milex", "tonnageimputed"),
                    labels = c(expression("DV: ln Military expenditure index"),
                               expression("DV: ln Naval tonnage index")))

merged$vars <- factor(merged$vars,
                      levels = c("polity",
                                 "boix",
                                 "uds",
                                 "riv",
                                 "defense",
                                 "sscores",
                                 "absidealdiff",
                                 "igo",
                                 "diplo",
                                 "bitrade",
                                 "pecpc",
                                 "atopkappa",
                                 "atopbin",
                                 "nopreference"),
                      labels = c(expression("Joint democracy Polity"),
                                 expression("Joint democracy Boix"),
                                 expression("Joint democracy UDS"),
                                 expression("Rivalry"),
                                 expression("Defense pacts"),
                                 expression("S-scores"),
                                 expression("Ideal difference"),
                                 expression("IGO membership"),
                                 expression("Diplomatic exchange"),
                                 expression("Bilateral trade"),
                                 expression("Energy consumption"),
                                 expression("Alliances (continuous)"),
                                 expression("Alliances (binary)"),
                                 expression("No interest variable")))

merged$controls <- factor(merged$controls,
                          levels = c("simplenocontrols",
                                     "simple",
                                     "simplesimple"),
                          labels = c("No controls",
                                     "Basic controls",
                                     "Basic controls + alternative PT"))

merged$coef <- factor(merged$coef,
                      levels = c("econ", "pop"),
                      labels = c("Potential threat (economic)",
                                 "Potential threat (population)"))

merged$type_control <- factor(merged$type_control,
                              levels = c("wsub",
                                         "wosub"),
                              labels = c("With subsistence control",
                                         "Without subsistence control"))

sub <- merged %>%
  filter(dv %in% c(expression("DV: ln Military expenditure index"),
                   expression("DV: ln Naval tonnage index"))) %>%
  filter(coef %in% c("Potential threat (economic)"),
         model %in% c("Two-Way Fixed Effects")) 

sub_polity <- sub %>%
  filter(vars %in% c(expression("Joint democracy Polity"))) %>%
  mutate(ymax = ifelse(dv %in% "DV: ln Military expenditure index", 2, 5),
         ymin = ifelse(dv %in% "DV: ln Military expenditure index", -1, -1.5))

ggplot(subset(sub_polity, controls %in% c("Basic controls + alternative PT")),
       aes(x = variant, 
           y = coefs, 
           shape = type_control,
           color = colcat_sub,
           ymin = lower95, 
           ymax = upper95)) +
  geom_hline(yintercept = 0, colour = gray(1/2), lty = 2, alpha = 0.8) +
  geom_point(position = position_dodge(width = 1/2), size = 2.5) +
  geom_linerange(position = position_dodge(width = 1/2), size = .5) +
  theme_bw() +
  coord_flip() +
  labs(title = "(a) Influence of potential threat on arming and power projection (1946-2012)",
       subtitle = "Control for population based potential threat measure",
       x = "",
       y = expression("Coefficient of standardized potential threat (Polity) variable"["i, t-1"])) +
  scale_shape_manual(name = "",
                     values = c(17, 16)) +
  scale_color_manual(name = "",
                     values = c("milex_cc_wosub" = "#006687",
                                "milex_cc_wsub" = "#003f54",
                                "tonnage_cc_wosub" = "#870023",
                                "tonnage_cc_wsub" = "#540016"),
                     guide = F) +
  facet_wrap(~ dv , scales = "free") + #Interprets as plotmath expression
  theme(panel.grid.minor = element_blank(),
        legend.position = "top",
        strip.text = element_text(size = 10)) +
  geom_blank(aes(y = ymax)) +
  geom_blank(aes(y = ymin)) +
  scale_y_continuous(breaks = seq(-1, 5, 1))

ggplot(subset(sub_polity, controls %in% c("Basic controls")),
       aes(x = variant, 
           y = coefs, 
           shape = type_control,
           color = colcat_sub,
           ymin = lower95, 
           ymax = upper95)) +
  geom_hline(yintercept = 0, colour = gray(1/2), lty = 2, alpha = 0.8) +
  geom_point(position = position_dodge(width = 1/2), size = 2.5) +
  geom_linerange(position = position_dodge(width = 1/2), size = .5) +
  theme_bw() +
  coord_flip() +
  labs(title = "(b) Influence of potential threat on arming and power projection (1946-2012)",
       subtitle = "No control for population based potential threat measure",
       x = "",
       y = expression("Coefficient of standardized potential threat (Polity) variable"["i, t-1"])) +
  scale_shape_manual(name = "",
                     values = c(17, 16)) +
  scale_color_manual(name = "",
                     values = c("milex_c_wosub" = "#006687",
                                "milex_c_wsub" = "#003f54",
                                "tonnage_c_wosub" = "#870023",
                                "tonnage_c_wsub" = "#540016"),
                     guide = F) +
  facet_wrap(~ dv , scales = "free") + #Interprets as plotmath expression
  theme(panel.grid.minor = element_blank(),
        legend.position = "top",
        strip.text = element_text(size = 10)) +
  geom_blank(aes(y = ymax)) +
  geom_blank(aes(y = ymin)) +
  scale_y_continuous(breaks = seq(-1, 5, 1))

