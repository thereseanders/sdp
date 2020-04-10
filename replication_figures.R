####################################################################
#
# Reproduces all figures from the main text and online appendix of
# Anders, Fariss, and Markowitz, Bread before guns or butter: 
# Introducing Surplus Domestic Product (SDP)
#
# note: code to reproduce coefficent plots is in the script
# `replication_models_tables.R`
#
####################################################################

library(countrycode)
library(tidyverse)
library(ggpubr)
library(gridExtra)
library(ggrepel)
library(Hmisc)
library(corrplot)
library(RColorBrewer)
library(colorspace)
library(maptools)
library(rgeos)
library(ggforce)


load("./data/sdp_master.RData")

# Main Text

# Figure 1: 
master_annual <- master %>%
  dplyr::select(ccode,
                year,
                surplus1095,
                WorldBank_gdp_2011_ppp_estimate,
                WorldBank_pop_estimate) %>%
  dplyr::mutate(surplus1095_truncatedzero = ifelse(surplus1095 < 0, 0, surplus1095)) %>%
  group_by(year) %>%
  dplyr::summarise(sumgdp = sum(WorldBank_gdp_2011_ppp_estimate, na.rm = T),
                   sumsurplus = sum(surplus1095_truncatedzero, na.rm = T),
                   sumpop = sum(WorldBank_pop_estimate, na.rm = T))

master_relsurplus <- master %>%
  dplyr::select(ccode,
                year,
                surplus1095,
                WorldBank_gdp_2011_ppp_estimate) %>%
  dplyr::mutate(surplus1095_truncatedzero = ifelse(surplus1095 < 0, 0, surplus1095)) %>%
  left_join(master_annual, by = "year") %>%
  dplyr::mutate(relsurplus = 1/sumsurplus*surplus1095_truncatedzero,
                relgdp = 1/sumgdp*WorldBank_gdp_2011_ppp_estimate) %>%
  gather(indicator, value, relsurplus:relgdp) %>%
  filter(ccode %in% c(2, 200, 710, 732, 770, 140)) %>%
  mutate(great = ifelse(ccode %in% c(2, 200, 710, 1, 0), 1, 0))


master_relsurplus$country <- factor(master_relsurplus$ccode,
                                    levels = c(710, 2, 200, 732, 770, 140),
                                    labels = c("China",
                                               "United States",
                                               "United Kingdom",
                                               "South Korea",
                                               "Pakistan",
                                               "Brazil"))

master_relsurplus$indicator <- factor(master_relsurplus$indicator,
                                      levels = c("relsurplus",
                                                 "relgdp"),
                                      labels = c("Share of global SDP",
                                                 "Share of global GDP"))


# Relative shares
master_relsurplus_p1 <- filter(master_relsurplus, great == 1) %>%
  droplevels()
master_relsurplus_p2 <- filter(master_relsurplus, great == 0) %>%
  droplevels()


p1 <- ggplot(master_relsurplus_p1,
             aes(x = year, y = value, color = indicator, linetype = indicator)) +
  geom_line() +
  facet_wrap(~country, nrow = 1, scales = "free_y") +
  theme_bw() +
  scale_color_manual(name = "Relative power-resources",
                     values = c("#da7c30", #orange
                                "gray50")) +
  scale_linetype_manual(name = "Relative power-resources",
                        values = c("solid",
                                   "21")) +
  theme(legend.position = "top",
        axis.text = element_text(size = 6),
        plot.margin = unit(c(t = .5, r = 0.2, b = 0.2, l = 0.2), "cm"),
        strip.text = element_text(size = 7),
        legend.text = element_text(size = 8),
        legend.title = element_text(size = 8),
        axis.title = element_text(size = 8)) +
  labs(x = "Year", y = "Share of global power-resources") +
  scale_y_continuous(labels = scales::percent) +
  coord_cartesian(ylim = c(0,0.4))


p2 <- ggplot(master_relsurplus_p2,
             aes(x = year, y = value, color = indicator, linetype = indicator)) +
  geom_line() +
  facet_wrap(~country, nrow = 1, scales = "free_y") +
  theme_bw() +
  scale_color_manual(name = "Relative power-resources",
                     values = c("#da7c30", #orange
                                "gray50")) +
  scale_linetype_manual(name = "Relative power-resources",
                        values = c("solid",
                                   "21")) +
  theme(legend.position = "top",
        axis.text = element_text(size = 6),
        plot.margin = unit(c(t = .5, r = 0.2, b = 0.2, l = 0.2), "cm"),
        strip.text = element_text(size = 7),
        legend.text = element_text(size = 8),
        legend.title = element_text(size = 8),
        axis.title = element_text(size = 8)) +
  labs(x = "Year", y = "Share of global power-resources") +
  scale_y_continuous(labels = scales::percent)

plot_all <- ggarrange(p1, p2, 
                      common.legend = TRUE, 
                      legend="top", 
                      nrow = 2,
                      vjust = -.2,
                      heights = c(2,2),
                      font.label = list(size = 14, face = "bold"))
annotate_figure(plot_all,
                top = text_grob("Evolution of relative power-resources based on global shares of SDP versus GDP", 
                                size = 10,
                                face = "bold"))


# Figure 2: 

#cincdat <- readRDS("./data/cincdat.rds")

cinc_annualtotal <- cincdat %>%
  dplyr::select(ccode,
                year,
                irst,
                pec,
                tpop,
                upop,
                milex,
                milper,
                country) %>%
  group_by(year) %>%
  dplyr::summarise(irst_total = sum(irst, na.rm = T),
                   pec_total = sum(pec, na.rm = T),
                   tpop_total = sum(tpop, na.rm = T),
                   upop_total = sum(upop, na.rm = T),
                   milex_total = sum(milex, na.rm = T),
                   milper_total = sum(milper, na.rm = T))


cinc_new <- cincdat %>%
  left_join(cinc_annualtotal) %>%
  dplyr::mutate(irst_sumirst = irst/irst_total,
                pec_sumpec = pec/pec_total,
                tpop_sumtpop = tpop/tpop_total,
                upop_sumupop = upop/upop_total,
                milex_summilex = milex/milex_total,
                milper_summilper = milper/milper_total) %>%
  dplyr::mutate(econ_capacity = rowMeans(data.frame(irst_sumirst,
                                                    pec_sumpec,
                                                    tpop_sumtpop,
                                                    upop_sumupop)),
                econflow_capacity = rowMeans(data.frame(irst_sumirst,
                                                        pec_sumpec)),
                mil_capacity = rowMeans(data.frame(milex_summilex,
                                                   milper_summilper)),
                cinc_recomputed = rowMeans(data.frame(irst_sumirst,
                                                      pec_sumpec,
                                                      tpop_sumtpop,
                                                      upop_sumupop,
                                                      milex_summilex,
                                                      milper_summilper)))


master_relsurplus_all <- master %>%
  dplyr::select(ccode,
                year,
                surplus1095,
                WorldBank_gdp_2011_ppp_estimate,
                WorldBank_pop_estimate) %>%
  dplyr::mutate(surplus1095_truncatedzero = ifelse(surplus1095 < 0, 0, surplus1095)) %>%
  left_join(master_annual, by = "year") %>%
  dplyr::mutate(relsurplus = 1/sumsurplus*surplus1095_truncatedzero,
                relgdp = 1/sumgdp*WorldBank_gdp_2011_ppp_estimate,
                relpop = 1/sumpop*WorldBank_pop_estimate,
                gdppc = WorldBank_gdp_2011_ppp_estimate/WorldBank_pop_estimate) %>%
  filter(year >= 1816) %>%
  left_join(cinc_new)

all_grouped <- master_relsurplus_all %>%
  dplyr::select(ccode,
                country,
                year,
                relsurplus,
                relgdp,
                cinc) %>%
  dplyr::mutate(period = ifelse(year >= 1816 & year <= 1869, 1, NA)) %>%
  dplyr::mutate(period = replace(period, year >= 1870 & year <= 1913, 2)) %>%
  dplyr::mutate(period = replace(period, year >= 1946 & year <= 1989, 3)) %>%
  dplyr::mutate(period = replace(period, year >= 1991 & year <= 2012, 4)) %>%
  filter(!is.na(period)) %>%
  group_by(period, country) %>%
  dplyr::summarise(av_relsurplus = mean(relsurplus, na.rm = T),
                   av_relgdp = mean(relgdp, na.rm = T),
                   av_cinc = mean(cinc, na.rm = T))

all_grouped[all_grouped == "NaN"] <- NA

## Overview
all_grouped_summary <- all_grouped %>%
  dplyr::select(-av_cinc) %>%
  ungroup() %>%
  gather(indicator, value, -period, -country) %>%
  group_by(period, indicator) %>%
  arrange(desc(value)) %>%
  slice(1:10) %>%
  dplyr::mutate(rank = seq(1,10)) %>%
  dplyr::mutate(country = replace(country, country == "United States of America", "USA"),
                country = replace(country, country == "Italy/Sardinia", "Italy"),
                country_alt = country,
                country_alt = replace(country_alt, country == "Russia (Soviet Union)", "Russia"),
                country_alt = replace(country_alt, country == "Germany (Prussia)", "Germany"),
                country_alt = replace(country_alt, country == "German Federal Republic", "Germany"),
                country_alt = replace(country_alt, country == "United Kingdom", "UK"),
                country = str_trim(country),
                country_alt = str_trim(country_alt))

all_grouped_summary$period <- factor(all_grouped_summary$period,
                                     levels = seq(1,4),
                                     labels = c("1816 to 1869",
                                                "1870 to 1913",
                                                "1946 to 1989",
                                                "1991 to 2012"))

all_grouped_summary$indicator <- factor(all_grouped_summary$indicator,
                                        levels = rev(c("av_relsurplus",
                                                       "av_relgdp")),
                                        labels = rev(c("Share of global SDP",
                                                       "Share of global GDP")))

ggplot(all_grouped_summary,
       aes(x = reorder(factor(rank), value), y = value, color = indicator, shape = indicator)) +
  geom_point(position = position_dodge(width = 0.7), size = .75) +
  geom_text(aes(label = country),
            hjust = -0.15, 
            vjust = 0.5,
            position = position_dodge(width = 0.7),
            show.legend = F,
            size = 1.75) +
  facet_wrap(~period, nrow = 1) +
  theme_bw() +
  coord_flip(ylim = c(0,0.35)) +
  theme(legend.position = "top",
        legend.text = element_text(size = 6),
        legend.margin=margin(0,0,0,0),
        strip.text = element_text(size = 6),
        legend.title=element_text(size=6),
        axis.title = element_text(size = 6),
        axis.text = element_text(size = 5),
        plot.title = element_text(size = 7, margin = margin(0,0,0,0)),
        panel.grid = element_blank()) +
  labs(title = "Top 10 powers by average share of global power-resources",
       x = "Rank",
       y = "Share of global power-resources") +
  scale_color_manual(values = c("gray40",
                                "#da7c30"), #green
                     name = "Relative power-resources",
                     guide = guide_legend(reverse=TRUE)) +
  scale_shape_manual(name = "Relative power-resources",
                     values = c(17, 16),
                     guide = guide_legend(reverse=TRUE)) +
  scale_y_continuous(labels = scales::percent) +
  guides(color = guide_legend(override.aes = list(size = 3, alpha = 1)),
         shape = guide_legend(override.aes = list(size = 3, alpha = 1)))

# Figure 3:

master_relsurplus_sub <- master_relsurplus_all %>%
  dplyr::select(ccode,
                year,
                relsurplus,
                relgdp,
                irst_sumirst,
                pec_sumpec,
                upop_sumupop,
                tpop_sumtpop,
                relpop,
                milex_summilex,
                milper_summilper,
                gdppc,
                cinc)


cors_new <- list()
years2 <- unique(master_relsurplus_all$year)

for(y in 1:length(years2)){
  
  # Subsetting data fram
  sub <- master_relsurplus_sub %>%
    filter(year == years2[y]) 
  
  ##### SDP #####
  # irst
  result_sdp_irst <- tryCatch({sdp_irst <- cor.test(sub$relsurplus, sub$irst_sumirst, conf.level = 0.95)
  val_sdp_irst <- as.numeric(sdp_irst$estimate)
  val_sdp_irst_cilo <- as.numeric(sdp_irst$conf.int[1])
  val_sdp_irst_cihi <- as.numeric(sdp_irst$conf.int[2])
  }, error = function(e) {
    print(paste("Error on year ", years2[y]))
    val_sdp_irst <- NA
    val_sdp_irst_cilo <- NA
    val_sdp_irst_cihi <- NA      
  })
  
  # pec
  result_sdp_pec <- tryCatch({sdp_pec <- cor.test(sub$relsurplus, sub$pec_sumpec, conf.level = 0.95)
  val_sdp_pec <- as.numeric(sdp_pec$estimate)
  val_sdp_pec_cilo <- as.numeric(sdp_pec$conf.int[1])
  val_sdp_pec_cihi <- as.numeric(sdp_pec$conf.int[2])
  }, error = function(e) {
    print(paste("Error on year ", years2[y]))
    val_sdp_pec <- NA
    val_sdp_pec_cilo <- NA
    val_sdp_pec_cihi <- NA      
  })
  
  # upop
  result_sdp_upop <- tryCatch({sdp_upop <- cor.test(sub$relsurplus, sub$upop_sumupop, conf.level = 0.95)
  val_sdp_upop <- as.numeric(sdp_upop$estimate)
  val_sdp_upop_cilo <- as.numeric(sdp_upop$conf.int[1])
  val_sdp_upop_cihi <- as.numeric(sdp_upop$conf.int[2])
  }, error = function(e) {
    print(paste("Error on year ", years2[y]))
    val_sdp_upop <- NA
    val_sdp_upop_cilo <- NA
    val_sdp_upop_cihi <- NA      
  })
  
  # tpop
  result_sdp_tpop <- tryCatch({sdp_tpop <- cor.test(sub$relsurplus, sub$tpop_sumtpop, conf.level = 0.95)
  val_sdp_tpop <- as.numeric(sdp_tpop$estimate)
  val_sdp_tpop_cilo <- as.numeric(sdp_tpop$conf.int[1])
  val_sdp_tpop_cihi <- as.numeric(sdp_tpop$conf.int[2])
  }, error = function(e) {
    print(paste("Error on year ", years2[y]))
    val_sdp_tpop <- NA
    val_sdp_tpop_cilo <- NA
    val_sdp_tpop_cihi <- NA      
  })
  
  # relpop
  result_sdp_relpop <- tryCatch({sdp_relpop <- cor.test(sub$relsurplus, sub$relpop, conf.level = 0.95)
  val_sdp_relpop <- as.numeric(sdp_relpop$estimate)
  val_sdp_relpop_cilo <- as.numeric(sdp_relpop$conf.int[1])
  val_sdp_relpop_cihi <- as.numeric(sdp_relpop$conf.int[2])
  }, error = function(e) {
    print(paste("Error on year ", years2[y]))
    val_sdp_relpop <- NA
    val_sdp_relpop_cilo <- NA
    val_sdp_relpop_cihi <- NA      
  })
  
  # milex
  result_sdp_milex <- tryCatch({sdp_milex <- cor.test(sub$relsurplus, sub$milex_summilex, conf.level = 0.95)
  val_sdp_milex <- as.numeric(sdp_milex$estimate)
  val_sdp_milex_cilo <- as.numeric(sdp_milex$conf.int[1])
  val_sdp_milex_cihi <- as.numeric(sdp_milex$conf.int[2])
  }, error = function(e) {
    print(paste("Error on year ", years2[y]))
    val_sdp_milex <- NA
    val_sdp_milex_cilo <- NA
    val_sdp_milex_cihi <- NA      
  })
  
  # milper
  result_sdp_milper <- tryCatch({sdp_milper <- cor.test(sub$relsurplus, sub$milper_summilper, conf.level = 0.95)
  val_sdp_milper <- as.numeric(sdp_milper$estimate)
  val_sdp_milper_cilo <- as.numeric(sdp_milper$conf.int[1])
  val_sdp_milper_cihi <- as.numeric(sdp_milper$conf.int[2])
  }, error = function(e) {
    print(paste("Error on year ", years2[y]))
    val_sdp_milper <- NA
    val_sdp_milper_cilo <- NA
    val_sdp_milper_cihi <- NA      
  })
  
  # gdppc
  result_sdp_gdppc <- tryCatch({sdp_gdppc <- cor.test(sub$relsurplus, sub$gdppc, conf.level = 0.95)
  val_sdp_gdppc <- as.numeric(sdp_gdppc$estimate)
  val_sdp_gdppc_cilo <- as.numeric(sdp_gdppc$conf.int[1])
  val_sdp_gdppc_cihi <- as.numeric(sdp_gdppc$conf.int[2])
  }, error = function(e) {
    print(paste("Error on year ", years2[y]))
    val_sdp_gdppc <- NA
    val_sdp_gdppc_cilo <- NA
    val_sdp_gdppc_cihi <- NA      
  })
  
  # cinc
  result_sdp_cinc <- tryCatch({sdp_cinc <- cor.test(sub$relsurplus, sub$cinc, conf.level = 0.95)
  val_sdp_cinc <- as.numeric(sdp_cinc$estimate)
  val_sdp_cinc_cilo <- as.numeric(sdp_cinc$conf.int[1])
  val_sdp_cinc_cihi <- as.numeric(sdp_cinc$conf.int[2])
  }, error = function(e) {
    print(paste("Error on year ", years2[y]))
    val_sdp_cinc <- NA
    val_sdp_cinc_cilo <- NA
    val_sdp_cinc_cihi <- NA      
  })
  
  ##### GDP #####
  # irst
  result_gdp_irst <- tryCatch({gdp_irst <- cor.test(sub$relgdp, sub$irst_sumirst, conf.level = 0.95)
  val_gdp_irst <- as.numeric(gdp_irst$estimate)
  val_gdp_irst_cilo <- as.numeric(gdp_irst$conf.int[1])
  val_gdp_irst_cihi <- as.numeric(gdp_irst$conf.int[2])
  }, error = function(e) {
    print(paste("Error on year ", years2[y]))
    val_gdp_irst <- NA
    val_gdp_irst_cilo <- NA
    val_gdp_irst_cihi <- NA      
  })
  
  # pec
  result_gdp_pec <- tryCatch({gdp_pec <- cor.test(sub$relgdp, sub$pec_sumpec, conf.level = 0.95)
  val_gdp_pec <- as.numeric(gdp_pec$estimate)
  val_gdp_pec_cilo <- as.numeric(gdp_pec$conf.int[1])
  val_gdp_pec_cihi <- as.numeric(gdp_pec$conf.int[2])
  }, error = function(e) {
    print(paste("Error on year ", years2[y]))
    val_gdp_pec <- NA
    val_gdp_pec_cilo <- NA
    val_gdp_pec_cihi <- NA      
  })
  
  # upop
  result_gdp_upop <- tryCatch({gdp_upop <- cor.test(sub$relgdp, sub$upop_sumupop, conf.level = 0.95)
  val_gdp_upop <- as.numeric(gdp_upop$estimate)
  val_gdp_upop_cilo <- as.numeric(gdp_upop$conf.int[1])
  val_gdp_upop_cihi <- as.numeric(gdp_upop$conf.int[2])
  }, error = function(e) {
    print(paste("Error on year ", years2[y]))
    val_gdp_upop <- NA
    val_gdp_upop_cilo <- NA
    val_gdp_upop_cihi <- NA      
  })
  
  # tpop
  result_gdp_tpop <- tryCatch({gdp_tpop <- cor.test(sub$relgdp, sub$tpop_sumtpop, conf.level = 0.95)
  val_gdp_tpop <- as.numeric(gdp_tpop$estimate)
  val_gdp_tpop_cilo <- as.numeric(gdp_tpop$conf.int[1])
  val_gdp_tpop_cihi <- as.numeric(gdp_tpop$conf.int[2])
  }, error = function(e) {
    print(paste("Error on year ", years2[y]))
    val_gdp_tpop <- NA
    val_gdp_tpop_cilo <- NA
    val_gdp_tpop_cihi <- NA      
  })
  
  # relpop
  result_gdp_relpop <- tryCatch({gdp_relpop <- cor.test(sub$relgdp, sub$relpop, conf.level = 0.95)
  val_gdp_relpop <- as.numeric(gdp_relpop$estimate)
  val_gdp_relpop_cilo <- as.numeric(gdp_relpop$conf.int[1])
  val_gdp_relpop_cihi <- as.numeric(gdp_relpop$conf.int[2])
  }, error = function(e) {
    print(paste("Error on year ", years2[y]))
    val_gdp_relpop <- NA
    val_gdp_relpop_cilo <- NA
    val_gdp_relpop_cihi <- NA      
  })
  
  # milex
  result_gdp_milex <- tryCatch({gdp_milex <- cor.test(sub$relgdp, sub$milex_summilex, conf.level = 0.95)
  val_gdp_milex <- as.numeric(gdp_milex$estimate)
  val_gdp_milex_cilo <- as.numeric(gdp_milex$conf.int[1])
  val_gdp_milex_cihi <- as.numeric(gdp_milex$conf.int[2])
  }, error = function(e) {
    print(paste("Error on year ", years2[y]))
    val_gdp_milex <- NA
    val_gdp_milex_cilo <- NA
    val_gdp_milex_cihi <- NA      
  })
  
  # milper
  result_gdp_milper <- tryCatch({gdp_milper <- cor.test(sub$relgdp, sub$milper_summilper, conf.level = 0.95)
  val_gdp_milper <- as.numeric(gdp_milper$estimate)
  val_gdp_milper_cilo <- as.numeric(gdp_milper$conf.int[1])
  val_gdp_milper_cihi <- as.numeric(gdp_milper$conf.int[2])
  }, error = function(e) {
    print(paste("Error on year ", years2[y]))
    val_gdp_milper <- NA
    val_gdp_milper_cilo <- NA
    val_gdp_milper_cihi <- NA      
  })
  
  # gdppc
  result_gdp_gdppc <- tryCatch({gdp_gdppc <- cor.test(sub$relgdp, sub$gdppc, conf.level = 0.95)
  val_gdp_gdppc <- as.numeric(gdp_gdppc$estimate)
  val_gdp_gdppc_cilo <- as.numeric(gdp_gdppc$conf.int[1])
  val_gdp_gdppc_cihi <- as.numeric(gdp_gdppc$conf.int[2])
  }, error = function(e) {
    print(paste("Error on year ", years2[y]))
    val_gdp_gdppc <- NA
    val_gdp_gdppc_cilo <- NA
    val_gdp_gdppc_cihi <- NA      
  })
  
  # cinc
  result_gdp_cinc <- tryCatch({gdp_cinc <- cor.test(sub$relgdp, sub$cinc, conf.level = 0.95)
  val_gdp_cinc <- as.numeric(gdp_cinc$estimate)
  val_gdp_cinc_cilo <- as.numeric(gdp_cinc$conf.int[1])
  val_gdp_cinc_cihi <- as.numeric(gdp_cinc$conf.int[2])
  }, error = function(e) {
    print(paste("Error on year ", years2[y]))
    val_gdp_cinc <- NA
    val_gdp_cinc_cilo <- NA
    val_gdp_cinc_cihi <- NA      
  })
  
  
  cors_new[[y]] <- c(years2[y], 
                     
                     # SDP correlations
                     val_sdp_irst, val_sdp_irst_cilo, val_sdp_irst_cihi,
                     val_sdp_pec, val_sdp_pec_cilo, val_sdp_pec_cihi,
                     val_sdp_upop, val_sdp_upop_cilo, val_sdp_upop_cihi,
                     val_sdp_tpop, val_sdp_tpop_cilo, val_sdp_tpop_cihi,
                     val_sdp_relpop, val_sdp_relpop_cilo, val_sdp_relpop_cihi,
                     val_sdp_milex, val_sdp_milex_cilo, val_sdp_milex_cihi,
                     val_sdp_milper, val_sdp_milper_cilo, val_sdp_milper_cihi,
                     val_sdp_gdppc, val_sdp_gdppc_cilo, val_sdp_gdppc_cihi,
                     val_sdp_cinc, val_sdp_cinc_cilo, val_sdp_cinc_cihi,
                     
                     # GDP correlations
                     val_gdp_irst, val_gdp_irst_cilo, val_gdp_irst_cihi,
                     val_gdp_pec, val_gdp_pec_cilo, val_gdp_pec_cihi,
                     val_gdp_upop, val_gdp_upop_cilo, val_gdp_upop_cihi,
                     val_gdp_tpop, val_gdp_tpop_cilo, val_gdp_tpop_cihi,
                     val_gdp_relpop, val_gdp_relpop_cilo, val_gdp_relpop_cihi,
                     val_gdp_milex, val_gdp_milex_cilo, val_gdp_milex_cihi,
                     val_gdp_milper, val_gdp_milper_cilo, val_gdp_milper_cihi,
                     val_gdp_gdppc, val_gdp_gdppc_cilo, val_gdp_gdppc_cihi,
                     val_gdp_cinc, val_gdp_cinc_cilo, val_gdp_cinc_cihi)
}

df_cors_new <- matrix(unlist(cors_new), nrow = length(years2), byrow = T) %>%
  as.data.frame() 

names(df_cors_new) <- c("year", 
                        # SDP correlations
                        "sdp_irst", "sdp_irst_cilo", "sdp_irst_cihi",
                        "sdp_pec", "sdp_pec_cilo", "sdp_pec_cihi",
                        "sdp_upop", "sdp_upop_cilo", "sdp_upop_cihi",
                        "sdp_tpop", "sdp_tpop_cilo", "sdp_tpop_cihi",
                        "sdp_relpop", "sdp_relpop_cilo", "sdp_relpop_cihi",
                        "val_sdp_milex", "val_sdp_milex_cilo", "val_sdp_milex_cihi",
                        "val_sdp_milper", "val_sdp_milper_cilo", "val_sdp_milper_cihi",
                        "val_sdp_gdppc", "val_sdp_gdppc_cilo", "val_sdp_gdppc_cihi",
                        "val_sdp_cinc", "val_sdp_cinc_cilo", "val_sdp_cinc_cihi",
                        
                        # GDP correlations
                        "gdp_irst", "gdp_irst_cilo", "gdp_irst_cihi",
                        "gdp_pec", "gdp_pec_cilo", "gdp_pec_cihi",
                        "gdp_upop", "gdp_upop_cilo", "gdp_upop_cihi",
                        "gdp_tpop", "gdp_tpop_cilo", "gdp_tpop_cihi",
                        "gdp_relpop", "gdp_relpop_cilo", "gdp_relpop_cihi",
                        "val_gdp_milex", "val_gdp_milex_cilo", "val_gdp_milex_cihi",
                        "val_gdp_milper", "val_gdp_milper_cilo", "val_gdp_milper_cihi",
                        "val_gdp_gdppc", "val_gdp_gdppc_cilo", "val_gdp_gdppc_cihi",
                        "val_gdp_cinc", "val_gdp_cinc_cilo", "val_gdp_cinc_cihi")

df_cors_new_long <- df_cors_new %>%
  gather(indicator, value, -year) %>%
  dplyr::mutate(type = ifelse(str_detect(indicator, "cilo"), "cilo", "estimate"),
                type = replace(type, str_detect(indicator, "cihi"), "cihi")) %>%
  dplyr::mutate(comp = ifelse(str_detect(indicator, "sdp"), "sdp", "gdp")) %>%
  dplyr::mutate(variable = ifelse(str_detect(indicator, "irst"), "irst", NA),
                variable = replace(variable, str_detect(indicator, "pec"), "pec"),
                variable = replace(variable, str_detect(indicator, "tpop"), "tpop"),
                variable = replace(variable, str_detect(indicator, "upop"), "upop"),
                variable = replace(variable, str_detect(indicator, "relpop"), "relpop"),
                variable = replace(variable, str_detect(indicator, "milex"), "milex"),
                variable = replace(variable, str_detect(indicator, "milper"), "milper"),
                variable = replace(variable, str_detect(indicator, "gdppc"), "gdppc"),
                variable = replace(variable, str_detect(indicator, "cinc"), "cinc")) %>%
  dplyr::select(-indicator) %>% 
  spread(type, value)

df_cors_new_long$comp <- factor(df_cors_new_long$comp,
                                levels = c("sdp", "gdp"),
                                labels = c("Share of global SDP",
                                           "Share of global GDP"))

df_cors_new_long$variable <- factor(df_cors_new_long$variable,
                                    levels = c("irst",
                                               "pec",
                                               "upop",
                                               "relpop", #these are our new estimates
                                               "tpop",
                                               "milex",
                                               "milper",
                                               "gdppc",
                                               "cinc"),
                                    labels = c("Share of global iron and steel production",
                                               "Share of global primary energy consumption",
                                               "Share of global urban population",
                                               "Share of global population",
                                               "Share of global population ",
                                               "Share of global military expenditure",
                                               "Share of global military personnel",
                                               "Per capita GDP",
                                               "CINC"))


ggplot(subset(df_cors_new_long, !is.na(estimate) & variable %in% c("Share of global iron and steel production",
                                                                   "Share of global primary energy consumption")),
       aes(x = year, y = estimate, color = comp, shape = comp)) +
  geom_linerange(aes(ymin = cilo, ymax = cihi), size = 0.1, alpha = 1, show.legend = F) +
  geom_point(size = 0.6) +
  theme_bw() +
  labs(x = "Year",
       y = "Pearson's product-moment correlation coefficient",
       title = "Correlation between CINC components and global shares of SDP vs. GDP") +
  scale_color_manual(values = c("#da7c30", #orange
                                "gray50"), 
                     name = "Share of global economic power-resources") +
  scale_shape_discrete(name = "Share of global economic power-resources") +
  theme(legend.position = "top",
        panel.grid.minor = element_blank(),
        strip.text = element_text(size = 6),
        plot.title = element_text(size = 7, margin = margin(0,0,0,0)),
        legend.text = element_text(size = 6),
        legend.title = element_text(size = 6),
        legend.margin=margin(0,0,0,0),
        axis.text = element_text(size = 5),
        axis.title = element_text(size = 7)) +
  guides(color = guide_legend(override.aes = list(size = 3, alpha = 1))) +
  coord_cartesian(xlim = c(1816,2012),
                  ylim = c(0,1)) +
  facet_wrap(~variable, ncol = 1) +
  scale_x_continuous(breaks = seq(1800, 2000, 25))


ggplot(subset(df_cors_new_long, !is.na(estimate) & variable %in% c("Share of global urban population",
                                                                   "Share of global population")),
       aes(x = year, y = estimate, color = comp, shape = comp)) +
  geom_linerange(aes(ymin = cilo, ymax = cihi), size = 0.1, alpha = 1, show.legend = F) +
  geom_point(size = 0.6) +
  theme_bw() +
  labs(x = "Year",
       y = "Pearson's product-moment correlation coefficient") +
  scale_color_manual(values = c("#da7c30", #orange
                                "gray50"), 
                     name = "Share of global economic power-resources") +
  scale_shape_discrete(name = "Share of global economic power-resources") +
  theme(legend.position = "top",
        panel.grid.minor = element_blank(),
        strip.text = element_text(size = 6),
        plot.title = element_text(size = 7, margin = margin(0,0,0,0)),
        legend.text = element_text(size = 6),
        legend.title = element_text(size = 6),
        legend.margin=margin(0,0,0,0),
        axis.text = element_text(size = 5),
        axis.title = element_text(size = 7)) +
  guides(color = guide_legend(override.aes = list(size = 5, alpha = 1))) +
  coord_cartesian(xlim = c(1816,2012),
                  ylim = c(0,1)) +
  facet_wrap(~variable, ncol = 1) +
  scale_x_continuous(breaks = seq(1800, 2000, 25))

# b& white version for print

ggplot(subset(df_cors_new_long, !is.na(estimate) & variable %in% c("Share of global iron and steel production",
                                                                   "Share of global primary energy consumption")),
       aes(x = year, y = estimate, color = comp, shape = comp)) +
  geom_linerange(aes(ymin = cilo, ymax = cihi), size = 0.1, alpha = 1, show.legend = F) +
  geom_point(size = 0.6) +
  theme_bw() +
  labs(x = "Year",
       y = "Pearson's product-moment correlation coefficient",
       title = "Correlation between CINC components and global shares of SDP vs. GDP") +
  scale_color_manual(values = c("gray25",
                                "gray50"), 
                     name = "Share of global economic power-resources") +
  scale_shape_discrete(name = "Share of global economic power-resources") +
  theme(legend.position = "top",
        panel.grid.minor = element_blank(),
        strip.text = element_text(size = 6),
        plot.title = element_text(size = 7, margin = margin(0,0,0,0)),
        legend.text = element_text(size = 6),
        legend.title = element_text(size = 6),
        legend.margin=margin(0,0,0,0),
        axis.text = element_text(size = 5),
        axis.title = element_text(size = 7)) +
  guides(color = guide_legend(override.aes = list(size = 3, alpha = 1))) +
  coord_cartesian(xlim = c(1816,2012),
                  ylim = c(0,1)) +
  facet_wrap(~variable, ncol = 1) +
  scale_x_continuous(breaks = seq(1800, 2000, 25))

ggplot(subset(df_cors_new_long, !is.na(estimate) & variable %in% c("Share of global urban population",
                                                                   "Share of global population")),
       aes(x = year, y = estimate, color = comp, shape = comp)) +
  geom_linerange(aes(ymin = cilo, ymax = cihi), size = 0.1, alpha = 1, show.legend = F) +
  geom_point(size = 0.6) +
  theme_bw() +
  labs(x = "Year",
       y = "Pearson's product-moment correlation coefficient") +
  scale_color_manual(values = c("gray25",
                                "gray50"), 
                     name = "Share of global economic power-resources") +
  scale_shape_discrete(name = "Share of global economic power-resources") +
  theme(legend.position = "top",
        panel.grid.minor = element_blank(),
        strip.text = element_text(size = 6),
        plot.title = element_text(size = 7, margin = margin(0,0,0,0)),
        legend.text = element_text(size = 6),
        legend.title = element_text(size = 6),
        legend.margin=margin(0,0,0,0),
        axis.text = element_text(size = 5),
        axis.title = element_text(size = 7)) +
  guides(color = guide_legend(override.aes = list(size = 5, alpha = 1))) +
  coord_cartesian(xlim = c(1816,2012),
                  ylim = c(0,1)) +
  facet_wrap(~variable, ncol = 1) +
  scale_x_continuous(breaks = seq(1800, 2000, 25))

# Figure 4: Top 10 potentially threatening states (rankorder graphs)
#rankorder_surplus1095_invlog_nodem_topn_polity <- readRDS("./data/rankorder_surplus1095_invlog_topn_polity.rds")
#rankorder_gdp_invlog_nodem_topn_polity <- readRDS("./data/rankorder_gdp_invlog_topn_polity.rds")
#rankorder_pop_invlog_nodem_topn_polity <- readRDS("./data/rankorder_pop_invlog_topn_polity.rds")

COLOR <- c("darkgreen",
           "darkorange2",
           "darkorchid",
           "dodgerblue",
           "magenta4",
           "mediumseagreen",
           "navy",
           "orange",
           "blue",
           "aquamarine4",
           "azure4",
           "antiquewhite4", 
           "brown",
           "orchid",
           "palevioletred",
           "saddlebrown")

colfunc <- colorRampPalette(COLOR)

### Setting up the vector with countries to look at
select_countries <- c(2, 710, 740, 365, 220, 225, 816, 160, 140, 200)

## Setting up tile colors
tilecolor_df1095 <- data.frame()
tilecolor_dfgdp <- data.frame()
tilecolor_dfpop <- data.frame()

for(i in 1:length(select_countries)){
  ccode1095 <- c(rankorder_surplus1095_invlog_nodem_topn_polity[[i]]$country)
  ccodegdp <- c(rankorder_gdp_invlog_nodem_topn_polity[[i]]$country)
  ccodepop <- c(rankorder_pop_invlog_nodem_topn_polity[[i]]$country)
  targetcountry1095 <- c(rep(select_countries[i], length(ccode1095)))
  targetcountrygdp <- c(rep(select_countries[i], length(ccodegdp)))
  targetcountrypop <- c(rep(select_countries[i], length(ccodepop)))
  tilecolor_df1095 <- bind_rows(tilecolor_df1095, data.frame(ccode1095, targetcountry1095, "surplus1095"))
  tilecolor_dfgdp <- bind_rows(tilecolor_dfgdp, data.frame(ccodegdp, targetcountrygdp, "gdp"))
  tilecolor_dfpop <- bind_rows(tilecolor_dfpop, data.frame(ccodepop, targetcountrypop, "pop"))
}


names(tilecolor_df1095) <- c("ccode", "targetcountry", "variant")
names(tilecolor_dfgdp) <- c("ccode", "targetcountry", "variant")
names(tilecolor_dfpop) <- c("ccode", "targetcountry", "variant")

tilecolor_df <- bind_rows(tilecolor_df1095, tilecolor_dfgdp, tilecolor_dfpop)
tilecolor_df <- unique(tilecolor_df)

tilecolor_df_unique <- data.frame("ccode" = unique(tilecolor_df$ccode),
                                  stringsAsFactors = F)
tilecolor_df_unique$color <- colfunc(length(tilecolor_df_unique$ccode))

tilecolor_df <- left_join(tilecolor_df, tilecolor_df_unique, by = c("ccode")) %>%
  arrange(targetcountry, variant, ccode)

tilecolor_df <- unique(tilecolor_df)
names(tilecolor_df)[1] <- "ccode"

##################
# US Graph
##################


### 1095 ###
rankorder_surplus1095_invlog_nodem_topn_polity_2 <- rankorder_surplus1095_invlog_nodem_topn_polity[[1]]
rankorder_surplus1095_invlog_nodem_topn_polity_2 <- dplyr::filter(rankorder_surplus1095_invlog_nodem_topn_polity_2, decade != 2010)
names(rankorder_surplus1095_invlog_nodem_topn_polity_2)[names(rankorder_surplus1095_invlog_nodem_topn_polity_2) == "country"] <- "ccode"
rankorder_surplus1095_invlog_nodem_topn_polity_2$ccode <- as.character(rankorder_surplus1095_invlog_nodem_topn_polity_2$ccode)

tilecolor_df_surplus1095_2 <- tilecolor_df %>%
  filter(targetcountry == 2, variant == "surplus1095") %>%
  select(ccode, color) %>%
  arrange(color) %>%
  dplyr::mutate(ccode = as.character(ccode))

rankorder_surplus1095_invlog_nodem_topn_polity_2 <- left_join(rankorder_surplus1095_invlog_nodem_topn_polity_2, tilecolor_df_surplus1095_2)
rankorder_surplus1095_invlog_nodem_topn_polity_2$countryname <- factor(rankorder_surplus1095_invlog_nodem_topn_polity_2$countryname,
                                                                       levels = unique(rankorder_surplus1095_invlog_nodem_topn_polity_2$countryname))
rankorder_surplus1095_invlog_nodem_topn_polity_2$color <- factor(rankorder_surplus1095_invlog_nodem_topn_polity_2$color,
                                                                 levels = unique(rankorder_surplus1095_invlog_nodem_topn_polity_2$color))

tilecolor <- as.character(sort(unique(rankorder_surplus1095_invlog_nodem_topn_polity_2$color)))

g2_1095 <- ggplot(rankorder_surplus1095_invlog_nodem_topn_polity_2,
                  aes(x = decade, 
                      y = ranking,
                      alpha = factor(medianindicator),
                      color = factor(medianindicator),
                      fill = countryname,
                      label = countryname)) +
  geom_tile(color = "white") +
  geom_text(fontface = "bold", size = 2, alpha = 1) +
  theme(legend.position = "none") +
  scale_fill_manual(values = tilecolor) +
  scale_color_manual(values = c("darkgrey", "white")) +
  scale_alpha_discrete(range = c(0.3, 0.9)) +
  scale_y_reverse(breaks = seq(1, 10),
                  expand = c(0, 0)) +
  scale_x_continuous(breaks = seq(1800, 2010, 10),
                     expand = c(0, 0)) +
  theme_classic() +
  theme(legend.position = "none",
        panel.grid = element_blank(),
        panel.border = element_blank(),
        plot.title = element_text(size = 10),
        plot.subtitle = element_text(size = 8),
        axis.line = element_blank(),
        axis.ticks = element_blank(),
        axis.text = element_text(size = 8)) +
  labs(x = "",
       y = "",
       title = "SDP ($3 subsistence level) rankorder graph for the United States",
       subtitle = "Preference compatibility measured via Polity2.") 
g2_1095

# greyscale version
g2_1095_grey <- ggplot(rankorder_surplus1095_invlog_nodem_topn_polity_2,
                  aes(x = decade, 
                      y = ranking,
                      alpha = factor(medianindicator),
                      color = factor(medianindicator),
                      label = countryname)) +
  geom_tile(color = "white") +
  geom_text(fontface = "bold", size = 2, alpha = 1) +
  theme(legend.position = "none") +
  scale_color_manual(values = c("darkgrey", "white")) +
  scale_alpha_discrete(range = c(0.2, 0.9)) +
  scale_y_reverse(breaks = seq(1, 10),
                  expand = c(0, 0)) +
  scale_x_continuous(breaks = seq(1800, 2010, 10),
                     expand = c(0, 0)) +
  theme_classic() +
  theme(legend.position = "none",
        panel.grid = element_blank(),
        panel.border = element_blank(),
        plot.title = element_text(size = 10),
        plot.subtitle = element_text(size = 8),
        axis.line = element_blank(),
        axis.ticks = element_blank(),
        axis.text = element_text(size = 8)) +
  labs(x = "",
       y = "",
       title = "SDP ($3 subsistence level) rankorder graph for the United States",
       subtitle = "Preference compatibility measured via Polity2.") 
g2_1095_grey


### GDP ###
rankorder_gdp_invlog_nodem_topn_polity_2 <- rankorder_gdp_invlog_nodem_topn_polity[[1]]
rankorder_gdp_invlog_nodem_topn_polity_2 <- filter(rankorder_gdp_invlog_nodem_topn_polity_2, decade != 2010)
names(rankorder_gdp_invlog_nodem_topn_polity_2)[2] <- "ccode"
rankorder_gdp_invlog_nodem_topn_polity_2$ccode <- as.character(rankorder_gdp_invlog_nodem_topn_polity_2$ccode)

tilecolor_df_gdp_2 <- tilecolor_df %>%
  filter(targetcountry == 2, variant == "gdp") %>%
  select(ccode, color) %>%
  arrange(color) %>%
  dplyr::mutate(ccode = as.character(ccode))

rankorder_gdp_invlog_nodem_topn_polity_2 <- left_join(rankorder_gdp_invlog_nodem_topn_polity_2, tilecolor_df_gdp_2)
rankorder_gdp_invlog_nodem_topn_polity_2$countryname <- factor(rankorder_gdp_invlog_nodem_topn_polity_2$countryname,
                                                               levels = unique(rankorder_gdp_invlog_nodem_topn_polity_2$countryname))
rankorder_gdp_invlog_nodem_topn_polity_2$color <- factor(rankorder_gdp_invlog_nodem_topn_polity_2$color,
                                                         levels = unique(rankorder_gdp_invlog_nodem_topn_polity_2$color))

tilecolor <- as.character(sort(unique(rankorder_gdp_invlog_nodem_topn_polity_2$color)))

g2_gdp <- ggplot(rankorder_gdp_invlog_nodem_topn_polity_2,
                 aes(x = decade, 
                     y = ranking,
                     alpha = factor(medianindicator),
                     color = factor(medianindicator),
                     fill = factor(countryname),
                     label = factor(countryname))) +
  geom_tile(color = "white") +
  geom_text(fontface = "bold", size = 2, alpha = 1) +
  theme(legend.position = "none") +
  scale_fill_manual(values = tilecolor) +
  scale_color_manual(values = c("darkgrey", "white")) +
  scale_alpha_discrete(range = c(0.3, 0.9)) +
  scale_y_reverse(breaks = seq(1, 10),
                  expand = c(0, 0)) +
  scale_x_continuous(breaks = seq(1800, 2010, 10),
                     expand = c(0, 0)) +
  theme_classic() +
  theme(legend.position = "none",
        panel.grid = element_blank(),
        panel.border = element_blank(),
        plot.title = element_text(size = 10),
        plot.subtitle = element_text(size = 8),
        axis.line = element_blank(),
        axis.ticks = element_blank(),
        axis.text = element_text(size = 8)) +
  labs(x = "",
       y = "",
       title = "GDP rankorder graph for the United States",
       subtitle = "Preference compatibility measured via Polity2.") 
g2_gdp

# greyscale version
g2_gdp_grey <- ggplot(rankorder_gdp_invlog_nodem_topn_polity_2,
                 aes(x = decade, 
                     y = ranking,
                     alpha = factor(medianindicator),
                     color = factor(medianindicator),
                     label = factor(countryname))) +
  geom_tile(color = "white") +
  geom_text(fontface = "bold", size = 2, alpha = 1) +
  theme(legend.position = "none") +
  scale_color_manual(values = c("darkgrey", "white")) +
  scale_alpha_discrete(range = c(0.2, 0.9)) +
  scale_y_reverse(breaks = seq(1, 10),
                  expand = c(0, 0)) +
  scale_x_continuous(breaks = seq(1800, 2010, 10),
                     expand = c(0, 0)) +
  theme_classic() +
  theme(legend.position = "none",
        panel.grid = element_blank(),
        panel.border = element_blank(),
        plot.title = element_text(size = 10),
        plot.subtitle = element_text(size = 8),
        axis.line = element_blank(),
        axis.ticks = element_blank(),
        axis.text = element_text(size = 8)) +
  labs(x = "",
       y = "",
       title = "GDP rankorder graph for the United States",
       subtitle = "Preference compatibility measured via Polity2.") 
g2_gdp_grey

### POP ###
rankorder_pop_invlog_nodem_topn_polity_2 <- rankorder_pop_invlog_nodem_topn_polity[[1]]
rankorder_pop_invlog_nodem_topn_polity_2 <- filter(rankorder_pop_invlog_nodem_topn_polity_2, decade != 2010)
names(rankorder_pop_invlog_nodem_topn_polity_2)[2] <- "ccode"
rankorder_pop_invlog_nodem_topn_polity_2$ccode <- as.character(rankorder_pop_invlog_nodem_topn_polity_2$ccode)

tilecolor_df_pop_2 <- tilecolor_df %>%
  filter(targetcountry == 2, variant == "pop") %>%
  select(ccode, color) %>%
  arrange(color) %>%
  dplyr::mutate(ccode = as.character(ccode))

rankorder_pop_invlog_nodem_topn_polity_2 <- left_join(rankorder_pop_invlog_nodem_topn_polity_2, tilecolor_df_pop_2)
rankorder_pop_invlog_nodem_topn_polity_2$countryname <- factor(rankorder_pop_invlog_nodem_topn_polity_2$countryname,
                                                               levels = unique(rankorder_pop_invlog_nodem_topn_polity_2$countryname))
rankorder_pop_invlog_nodem_topn_polity_2$color <- factor(rankorder_pop_invlog_nodem_topn_polity_2$color,
                                                         levels = unique(rankorder_pop_invlog_nodem_topn_polity_2$color))

tilecolor <- as.character(sort(unique(rankorder_pop_invlog_nodem_topn_polity_2$color)))

g2_pop <- ggplot(rankorder_pop_invlog_nodem_topn_polity_2,
                 aes(x = decade, 
                     y = ranking,
                     alpha = factor(medianindicator),
                     color = factor(medianindicator),
                     fill = factor(countryname),
                     label = factor(countryname))) +
  geom_tile(color = "white") +
  geom_text(fontface = "bold", size = 2, alpha = 1) +
  theme(legend.position = "none") +
  scale_fill_manual(values = tilecolor) +
  scale_color_manual(values = c("darkgrey", "white")) +
  scale_alpha_discrete(range = c(0.3, 0.9)) +
  scale_y_reverse(breaks = seq(1, 10),
                  expand = c(0, 0)) +
  scale_x_continuous(breaks = seq(1800, 2010, 10),
                     expand = c(0, 0)) +
  theme_classic() +
  theme(legend.position = "none",
        panel.grid = element_blank(),
        panel.border = element_blank(),
        plot.title = element_text(size = 10),
        plot.subtitle = element_text(size = 8),
        axis.line = element_blank(),
        axis.ticks = element_blank(),
        axis.text = element_text(size = 8)) +
  labs(x = "",
       y = "",
       title = "Population rankorder graph for the United States",
       subtitle = "Preference compatibility measured via Polity2.") 

g2_pop_grey <- ggplot(rankorder_pop_invlog_nodem_topn_polity_2,
                 aes(x = decade, 
                     y = ranking,
                     alpha = factor(medianindicator),
                     color = factor(medianindicator),
                     label = factor(countryname))) +
  geom_tile(color = "white") +
  geom_text(fontface = "bold", size = 2, alpha = 1) +
  theme(legend.position = "none") +
  scale_color_manual(values = c("darkgrey", "white")) +
  scale_alpha_discrete(range = c(0.2, 0.9)) +
  scale_y_reverse(breaks = seq(1, 10),
                  expand = c(0, 0)) +
  scale_x_continuous(breaks = seq(1800, 2010, 10),
                     expand = c(0, 0)) +
  theme_classic() +
  theme(legend.position = "none",
        panel.grid = element_blank(),
        panel.border = element_blank(),
        plot.title = element_text(size = 10),
        plot.subtitle = element_text(size = 8),
        axis.line = element_blank(),
        axis.ticks = element_blank(),
        axis.text = element_text(size = 8)) +
  labs(x = "",
       y = "",
       title = "Population rankorder graph for the United States",
       subtitle = "Preference compatibility measured via Polity2.") 
g2_pop_grey

g2 <- arrangeGrob(g2_1095, 
                  g2_gdp,
                  g2_pop,
                  ncol = 1)

### greyscale version for printing
g2_gs <- arrangeGrob(g2_1095_grey, 
                  g2_gdp_grey,
                  g2_pop_grey,
                  ncol = 1)

# Figure 5: potential threat faced by the United States 

us_sub <- us_dyadic %>%
  dplyr::select(ccode, year, opponent,
                boix_distinterestweight_dyadratio_gdp_invlog,
                boix_distinterestweight_dyadratio_sdp1095_invlog,
                polity_distinterestweight_dyadratio_gdp_invlog,
                polity_distinterestweight_dyadratio_sdp1095_invlog) %>%
  mutate(new_gdp = ifelse(is.na(polity_distinterestweight_dyadratio_gdp_invlog), boix_distinterestweight_dyadratio_gdp_invlog, polity_distinterestweight_dyadratio_gdp_invlog),
         new_sdp1095 = ifelse(is.na(polity_distinterestweight_dyadratio_sdp1095_invlog), boix_distinterestweight_dyadratio_sdp1095_invlog, polity_distinterestweight_dyadratio_sdp1095_invlog)) %>%
  dplyr::select(ccode, year, opponent, new_gdp, new_sdp1095) %>%
  gather(indicator, value, -ccode, -year, -opponent) %>%
  mutate(color = ifelse(opponent == 710, "China", "Total"),
         color = replace(color, opponent == 365, "Russia")) %>%
  filter(!is.na(value)) %>%
  group_by(ccode, year, indicator, color) %>%
  summarise(value = sum(value))

us_sub$color <- factor(us_sub$color,
                       levels = rev(c("China", "Russia", "Total")))

us_sub$indicator <- factor(us_sub$indicator,
                           levels = c("new_gdp",
                                      "new_sdp1095"),
                           labels = c("GDP",
                                      "SDP ($3 subsistence)"))
# color version
ggplot(us_sub,
       aes(x = year,
           y = value,
           fill = factor(color),
           color = factor(color))) +
  geom_bar(stat = "identity", position = "stack", color = NA, width = 1.15) +
  #geom_line(y=0, color = 'grey90') +
  scale_fill_manual(values = c("Total" = "grey85", 
                               "China" = "#e66101",
                               "Russia" = "#5e3c99"),
                    name = "") +
  scale_color_manual(values = c("Total" = "grey85", 
                                "China" = "#e66101",
                                "Russia" = "#5e3c99"),
                     name = "") +
  facet_wrap(~indicator) +
  theme_bw() +
  scale_x_continuous(breaks = seq(1800, 2000, 25)) +
  labs(title = "Potential threat faced by the United States",
       y = "Potential threat (Polity)",
       x = "") +
  theme(panel.grid.minor = element_blank(),
        legend.position = "top",
        strip.text = element_text(size = 6),
        plot.title = element_text(size = 9, margin = margin(0,0,0,0)),
        legend.text = element_text(size = 7),
        legend.margin=margin(3,0,0,0),
        axis.text = element_text(size = 6),
        axis.title = element_text(size = 8)) + 
  guides(color = guide_legend(override.aes = list(size = 2)))

# b&w version
ggplot(us_sub,
       aes(x = year,
           y = value,
           fill = factor(color),
           color = factor(color))) +
  geom_bar(stat = "identity", position = "stack", color = NA, width = 1.15) +
  scale_fill_manual(values = c("Total" = "grey85", 
                               "China" = "grey45",
                               "Russia" = "grey25"),
                    name = "") +
  scale_color_manual(values = c("Total" = "grey85", 
                                "China" = "grey45",
                                "Russia" = "grey25"),
                     name = "") +
  facet_wrap(~indicator) +
  theme_bw() +
  scale_x_continuous(breaks = seq(1800, 2000, 25)) +
  labs(title = "Potential threat faced by the United States",
       y = "Potential threat (Polity)",
       x = "") +
  theme(panel.grid.minor = element_blank(),
        legend.position = "top",
        strip.text = element_text(size = 6),
        plot.title = element_text(size = 9, margin = margin(0,0,0,0)),
        legend.text = element_text(size = 7),
        legend.margin=margin(3,0,0,0),
        axis.text = element_text(size = 6),
        axis.title = element_text(size = 8)) + 
  guides(color = guide_legend(override.aes = list(size = 2)))


# Figure 6
# code for figure 6 (coefficient plot) can be found in replication_models_tables.R

# Figure 7: military expenditure as percentage

master_milburden_world <- master %>%
  dplyr::select(ccode,
                year,
                milexsurplus1095_estimate,
                milexgdp_estimate) %>%
  gather(indicator, value, -ccode, -year) %>%
  dplyr::mutate(region = "World")

master_milburden_region  <- master %>%
  dplyr::select(ccode,
                year,
                milexsurplus1095_estimate,
                milexgdp_estimate) %>%
  gather(indicator, value, -ccode, -year) %>%
  dplyr::mutate(region = ifelse(ccode < 200, "Americas", NA),
                region = replace(region, ccode >= 200 & ccode < 400, "Europe"),
                region = replace(region, ccode >= 400 & ccode < 600, "Sub-Saharan Africa"),
                region = replace(region, ccode >= 600 & ccode < 700, "Middle East and N. Africa"),
                region = replace(region, ccode >= 700 & ccode < 900, "Asia"),
                region = replace(region, ccode >= 900, "Australia and Ozeania")) %>%
  filter(ccode < 900) %>%
  bind_rows(master_milburden_world) %>%
  dplyr::mutate(time = ifelse(year %in% seq(1801,1900), "nineteeth", NA),
                time = replace(time, year %in% seq(1901,2000), "twenteeth"),
                time = replace(time, year > 2000, "twentyfirst"))

master_milburden_region_summary <- master_milburden_region %>%
  group_by(indicator, time, region) %>%
  summarise(av = mean(value, na.rm = T)) %>%
  filter(!is.na(time))

master_milburden_region_summary2 <- master_milburden_region %>%
  filter(year >= 2012) %>%
  group_by(region, indicator) %>%
  summarise(av = mean(value, na.rm = T))

master_milburden_region$indicator <- factor(master_milburden_region$indicator,
                                            levels = c("milexsurplus1095_estimate",
                                                       "milexgdp_estimate"),
                                            labels = c("Military expenditure/SDP",
                                                       "Military expenditure/GDP"))

ggplot(master_milburden_region,
       aes(x = year, y = value, color = indicator, linetype = indicator)) +
  #geom_point(alpha = 0.03, size = 0.25) +
  geom_smooth(se = F, size = 0.5) +
  theme_bw() +
  scale_color_manual(name = "Military burden",
                     values = c("#da7c30", #orange
                                "gray60")) +
  scale_linetype_manual(name = "Military burden",
                        values = c("solid", 
                                   "21")) +
  scale_y_continuous(labels = scales::percent) +
  facet_wrap(~region, nrow = 1) +
  theme(legend.position = "top",
        strip.text = element_text(size = 5),
        plot.title = element_text(size = 7, margin = margin(0,0,0,0)),
        legend.text = element_text(size = 6),
        legend.title = element_text(size = 6),
        legend.margin=margin(0,0,0,0),
        axis.text = element_text(size = 5),
        axis.title = element_text(size = 7)) +
  labs(title = "Military expenditure as a percentage of GDP versus SDP",
       x = "Year",
       y = "Military expenditure/economic income") +
  scale_y_continuous(labels = scales::percent)


# Figure 8: evolution of military burden over time

master_milburden <- master %>%
  dplyr::select(ccode,
                year,
                milexsurplus1095_estimate,
                milexgdp_estimate) %>%
  #gather(indicator, value, -ccode, -year) %>%
  pivot_longer(c(-ccode, -year), names_to = "indicator", values_to = "value") %>%
  filter(ccode %in% c(710, 365, 750, 732, 816, 140)) 


master_milburden$country <- factor(master_milburden$ccode,
                                   levels = c(710, 365, 750, 732, 816, 140),
                                   labels = c("China",
                                              "Russia",
                                              "India",
                                              "South Korea",
                                              "Vietnam",
                                              "Brazil"))

master_milburden$indicator <- factor(master_milburden$indicator,
                                     levels = c("milexsurplus1095_estimate",
                                                "milexgdp_estimate"),
                                     labels = c("Military expenditure/SDP",
                                                "Military expenditure/GDP"))


p_milburden <- ggplot(master_milburden,
                      aes(x = year, y = value, color = indicator, linetype = indicator)) +
  geom_line(alpha = 1) +
  facet_wrap(~country, nrow = 2) +
  theme_bw() +
  scale_color_manual(name = "Military burden",
                     values = c("#da7c30", #orange
                                 "gray40")) +
  scale_linetype_manual(name = "Military burden",
                        values = c("solid", 
                                   "21")) +
  theme(legend.position = "top",
        axis.text = element_text(size = 6),
        plot.margin = unit(c(t = 0.8, r = 0.2, b = 0.2, l = 0.2), "cm"),
        strip.text = element_text(size = 8),
        legend.text = element_text(size = 8),
        legend.title = element_text(size = 8),
        axis.title = element_text(size = 8),
        plot.title = element_text(hjust = 0.5, size = 10, face = "bold")) +
  labs(title = "Evolution of military burden over time",
       x = "Year",
       y = "Military expenditure/economic power-resources") +
  scale_y_continuous(labels = scales::percent) 
p_milburden

###################
###################
### Appendix 
###################
###################

# Figure 2

set_cors <- list()

years <- unique(master$year)
for(y in 1:length(years)){
  sub <- master %>%
    filter(year == years[y])
  
  # surplus1095
  cor_gdp_surplus1095 <- tryCatch({
    cor <- cor.test(sub$surplus1095, sub$WorldBank_gdp_2011_ppp_estimate, conf.level = 0.95)
    val_surplus1095 <- as.numeric(cor$estimate)
    cilo_surplus1095 <- as.numeric(cor$conf.int[1])
    cihi_surplus1095 <- as.numeric(cor$conf.int[2])
  }, error = function(e) {
    print(paste("Error on year ", years[y]))
    val_surplus1095 <- NA
    cilo_surplus1095 <- NA
    cihi_surplus1095 <- NA      
  })
  
  # surplus730
  cor_gdp_surplus730 <- tryCatch({
    cor <- cor.test(sub$surplus730, sub$WorldBank_gdp_2011_ppp_estimate, conf.level = 0.95)
    val_surplus730 <- as.numeric(cor$estimate)
    cilo_surplus730 <- as.numeric(cor$conf.int[1])
    cihi_surplus730 <- as.numeric(cor$conf.int[2])
  }, error = function(e) {
    print(paste("Error on year ", years[y]))
    val_surplus730 <- NA
    cilo_surplus730 <- NA
    cihi_surplus730 <- NA      
  })
  
  # surplus365
  cor_gdp_surplus365 <- tryCatch({
    cor <- cor.test(sub$surplus365, sub$WorldBank_gdp_2011_ppp_estimate, conf.level = 0.95)
    val_surplus365 <- as.numeric(cor$estimate)
    cilo_surplus365 <- as.numeric(cor$conf.int[1])
    cihi_surplus365 <- as.numeric(cor$conf.int[2])
  }, error = function(e) {
    print(paste("Error on year ", years[y]))
    val_surplus365 <- NA
    cilo_surplus365 <- NA
    cihi_surplus365 <- NA      
  })
  
  # sub1095
  cor_gdp_sub1095 <- tryCatch({
    cor <- cor.test(sub$subsistence1095, sub$WorldBank_gdp_2011_ppp_estimate, conf.level = 0.95)
    val_sub1095 <- as.numeric(cor$estimate)
    cilo_sub1095 <- as.numeric(cor$conf.int[1])
    cihi_sub1095 <- as.numeric(cor$conf.int[2])
  }, error = function(e) {
    print(paste("Error on year ", years[y]))
    val_sub1095 <- NA
    cilo_sub1095 <- NA
    cihi_sub1095 <- NA      
  })
  
  # sub730
  cor_gdp_sub730 <- tryCatch({
    cor <- cor.test(sub$subsistence730, sub$WorldBank_gdp_2011_ppp_estimate, conf.level = 0.95)
    val_sub730 <- as.numeric(cor$estimate)
    cilo_sub730 <- as.numeric(cor$conf.int[1])
    cihi_sub730 <- as.numeric(cor$conf.int[2])
  }, error = function(e) {
    print(paste("Error on year ", years[y]))
    val_sub730 <- NA
    cilo_sub730 <- NA
    cihi_sub730 <- NA      
  })
  
  # sub365
  cor_gdp_sub365 <- tryCatch({
    cor <- cor.test(sub$subsistence365, sub$WorldBank_gdp_2011_ppp_estimate, conf.level = 0.95)
    val_sub365 <- as.numeric(cor$estimate)
    cilo_sub365 <- as.numeric(cor$conf.int[1])
    cihi_sub365 <- as.numeric(cor$conf.int[2])
  }, error = function(e) {
    print(paste("Error on year ", years[y]))
    val_sub365 <- NA
    cilo_sub365 <- NA
    cihi_sub365 <- NA      
  })
  
  set_cors[[y]] <- c(years[y], 
                     
                     # SDP correlations
                     val_surplus1095, cilo_surplus1095, cihi_surplus1095,
                     val_surplus730, cilo_surplus730, cihi_surplus730,
                     val_surplus365, cilo_surplus365, cihi_surplus365,
                     
                     # Subsisistence correlations
                     val_sub1095, cilo_sub1095, cihi_sub1095,
                     val_sub730, cilo_sub730, cihi_sub730,
                     val_sub365, cilo_sub365, cihi_sub365)
}

vec_names <- c("year", 
               "val_surplus:1095", "cilo_surplus:1095", "cihi_surplus:1095",
               "val_surplus:730", "cilo_surplus:730", "cihi_surplus:730",
               "val_surplus:365", "cilo_surplus:365", "cihi_surplus:365",
               "val_sub:1095", "cilo_sub:1095", "cihi_sub:1095",
               "val_sub:730", "cilo_sub:730", "cihi_sub:730",
               "val_sub:365", "cilo_sub:365", "cihi_sub:365")

df_cors_gdpsdp <- matrix(unlist(set_cors), nrow = length(years), byrow = T) %>%
  as.data.frame() %>%
  rename_all(., function(x) vec_names) %>%
  gather(indicator, value, -year) %>%
  separate(indicator, c("type", "sdp"), sep = "_") %>%
  spread(type, value) %>%
  separate(sdp, c("type", "lev"))

df_cors_gdpsdp$type <- factor(df_cors_gdpsdp$type,
                              levels = c("surplus",
                                         "sub"),
                              labels = c("Surplus",
                                         "Subsistence"))

df_cors_gdpsdp$lev <- factor(df_cors_gdpsdp$lev,
                             levels = c("365",
                                        "730",
                                        "1095"),
                             labels = c("$1",
                                        "$2",
                                        "$3"))

p_cor <- ggplot(df_cors_gdpsdp,
                aes(x = year,
                    y = val,
                    ymin = cilo,
                    ymax = cihi)) +
  geom_hline(yintercept = c(0,1), linetype = "dashed") +
  geom_ribbon(alpha = 0.3,
              size = 0.25) +
  geom_line() +
  facet_grid(type~lev) + 
  theme_light() +
  labs(title = "Correlation between GDP and surplus or subsistence",
       x = "Year",
       y = "Pearson correlation coefficient with 95% CI")


df_thresh <- master %>%
  dplyr::select(ccode,
                year,
                WorldBank_gdp_2011_ppp_estimate,
                v365,
                v730,
                v1095) %>%
  filter(!is.na(WorldBank_gdp_2011_ppp_estimate)) %>%
  gather(lev, value, v365:v1095) %>%
  mutate(abovethres = ifelse(value < WorldBank_gdp_2011_ppp_estimate, "above", "below")) %>%
  mutate(lev = str_replace(lev, "v", "")) %>%
  group_by(year, 
           lev,
           abovethres) %>%
  summarise(total = n()) %>%
  ungroup() %>%
  spread(abovethres, total) %>%
  mutate(above = replace(above, is.na(above), 0),
         below = replace(below, is.na(below), 0)) %>%
  mutate(prop = below/(above + below))


df_thresh$lev <- factor(df_thresh$lev,
                        levels = c("365",
                                   "730",
                                   "1095"),
                        labels = c("$1",
                                   "$2",
                                   "$3"))

p_thresh <- ggplot(df_thresh,
                   aes(x = year,
                       y = prop)) +
  geom_hline(yintercept = c(0,1), linetype = "dashed") +
  geom_bar(stat = "identity",
           color = NA,
           width = 1,
           alpha = 0.4) +
  facet_wrap(~lev) +
  theme_light() +
  labs(title = "States below subsistence threshold",
       x = "Year",
       y = "Proportion of states")

p_thresh_alt <- ggplot(df_thresh,
                       aes(x = year,
                           y = prop)) +
  geom_hline(yintercept = c(0,1), linetype = "dashed") +
  geom_line() +
  # geom_bar(stat = "identity",
  #          color = NA,
  #          width = 1,
  #          alpha = 0.5) +
  facet_wrap(~lev) +
  theme_light()

t <- grid.arrange(p_cor, p_thresh, heights = c(2.5,1), ncol = 1)

# Figure 3

rankorder_surplus1095_invlog_nodem_topn_polity_740 <- rankorder_surplus1095_invlog_nodem_topn_polity[[3]]
rankorder_surplus1095_invlog_nodem_topn_polity_740 <- filter(rankorder_surplus1095_invlog_nodem_topn_polity_740, decade != 2010)

names(rankorder_surplus1095_invlog_nodem_topn_polity_740)[2] <- "ccode"
rankorder_surplus1095_invlog_nodem_topn_polity_740$ccode <- as.character(rankorder_surplus1095_invlog_nodem_topn_polity_740$ccode)


tilecolor_df_surplus1095_740 <- tilecolor_df %>%
  filter(targetcountry == 740, variant == "surplus1095") %>%
  select(ccode, color) %>%
  arrange(color) %>%
  dplyr::mutate(ccode = as.character(ccode))

rankorder_surplus1095_invlog_nodem_topn_polity_740 <- left_join(rankorder_surplus1095_invlog_nodem_topn_polity_740, tilecolor_df_surplus1095_740)
rankorder_surplus1095_invlog_nodem_topn_polity_740$countryname <- factor(rankorder_surplus1095_invlog_nodem_topn_polity_740$countryname,
                                                                         levels = unique(rankorder_surplus1095_invlog_nodem_topn_polity_740$countryname))
rankorder_surplus1095_invlog_nodem_topn_polity_740$color <- factor(rankorder_surplus1095_invlog_nodem_topn_polity_740$color,
                                                                   levels = unique(rankorder_surplus1095_invlog_nodem_topn_polity_740$color))

tilecolor <- as.character(sort(unique(rankorder_surplus1095_invlog_nodem_topn_polity_740$color)))


g740_1095 <- ggplot(rankorder_surplus1095_invlog_nodem_topn_polity_740,
                    aes(x = decade, 
                        y = ranking,
                        alpha = factor(medianindicator),
                        color = factor(medianindicator),
                        fill = factor(countryname),
                        label = factor(countryname))) +
  geom_tile(color = "white") +
  geom_text(fontface = "bold", size = 4, alpha = 1) +
  theme(legend.position = "none") +
  scale_fill_manual(values = tilecolor) +
  scale_color_manual(values = c("darkgrey", "white")) +
  scale_alpha_discrete(range = c(0.3, 0.9)) +
  scale_y_reverse(breaks = seq(1, 10),
                  expand = c(0, 0)) +
  scale_x_continuous(breaks = seq(1800, 2010, 10),
                     expand = c(0, 0)) +
  theme_classic() +
  theme(legend.position = "none",
        panel.grid = element_blank(),
        panel.border = element_blank(),
        axis.line = element_blank(),
        axis.ticks = element_blank(),
        axis.text = element_text(size = 12)) +
  labs(x = "",
       y = "",
       title = "SDP ($3 subsistence level) rankorder graph for Japan",
       subtitle = "Preference compatibility measured via Polity2.") 
g740_1095

### GDP ###
rankorder_gdp_invlog_nodem_topn_polity_740 <- rankorder_gdp_invlog_nodem_topn_polity[[3]]
rankorder_gdp_invlog_nodem_topn_polity_740 <- filter(rankorder_gdp_invlog_nodem_topn_polity_740, decade != 2010)
names(rankorder_gdp_invlog_nodem_topn_polity_740)[2] <- "ccode"
rankorder_gdp_invlog_nodem_topn_polity_740$ccode <- as.character(rankorder_gdp_invlog_nodem_topn_polity_740$ccode)


tilecolor_df_gdp_740 <- tilecolor_df %>%
  filter(targetcountry == 740, variant == "gdp") %>%
  select(ccode, color) %>%
  arrange(color) %>%
  dplyr::mutate(ccode = as.character(ccode))

rankorder_gdp_invlog_nodem_topn_polity_740 <- left_join(rankorder_gdp_invlog_nodem_topn_polity_740, tilecolor_df_gdp_740)
rankorder_gdp_invlog_nodem_topn_polity_740$countryname <- factor(rankorder_gdp_invlog_nodem_topn_polity_740$countryname,
                                                                 levels = unique(rankorder_gdp_invlog_nodem_topn_polity_740$countryname))
rankorder_gdp_invlog_nodem_topn_polity_740$color <- factor(rankorder_gdp_invlog_nodem_topn_polity_740$color,
                                                           levels = unique(rankorder_gdp_invlog_nodem_topn_polity_740$color))

tilecolor <- as.character(sort(unique(rankorder_gdp_invlog_nodem_topn_polity_740$color)))

g740_gdp <- ggplot(rankorder_gdp_invlog_nodem_topn_polity_740,
                   aes(x = decade, 
                       y = ranking,
                       alpha = factor(medianindicator),
                       color = factor(medianindicator),
                       fill = factor(countryname),
                       label = factor(countryname))) +
  geom_tile(color = "white") +
  geom_text(fontface = "bold", size = 4, alpha = 1) +
  theme(legend.position = "none") +
  scale_fill_manual(values = tilecolor) +
  scale_color_manual(values = c("darkgrey", "white")) +
  scale_alpha_discrete(range = c(0.3, 0.9)) +
  scale_y_reverse(breaks = seq(1, 10),
                  expand = c(0, 0)) +
  scale_x_continuous(breaks = seq(1800, 2010, 10),
                     expand = c(0, 0)) +
  theme_classic() +
  theme(legend.position = "none",
        panel.grid = element_blank(),
        panel.border = element_blank(),
        axis.line = element_blank(),
        axis.ticks = element_blank(),
        axis.text = element_text(size = 12)) +
  labs(x = "",
       y = "",
       title = "GDP rankorder graph for Japan",
       subtitle = "Preference compatibility measured via Polity2.")
g740_gdp

### POp ###
rankorder_pop_invlog_nodem_topn_polity_740 <- rankorder_pop_invlog_nodem_topn_polity[[3]]
rankorder_pop_invlog_nodem_topn_polity_740 <- filter(rankorder_pop_invlog_nodem_topn_polity_740, decade != 2010)
names(rankorder_pop_invlog_nodem_topn_polity_740)[2] <- "ccode"
rankorder_pop_invlog_nodem_topn_polity_740$ccode <- as.character(rankorder_pop_invlog_nodem_topn_polity_740$ccode)


tilecolor_df_pop_740 <- tilecolor_df %>%
  filter(targetcountry == 740, variant == "pop") %>%
  select(ccode, color) %>%
  arrange(color) %>%
  dplyr::mutate(ccode = as.character(ccode))

rankorder_pop_invlog_nodem_topn_polity_740 <- left_join(rankorder_pop_invlog_nodem_topn_polity_740, tilecolor_df_pop_740)
rankorder_pop_invlog_nodem_topn_polity_740$countryname <- factor(rankorder_pop_invlog_nodem_topn_polity_740$countryname,
                                                                 levels = unique(rankorder_pop_invlog_nodem_topn_polity_740$countryname))
rankorder_pop_invlog_nodem_topn_polity_740$color <- factor(rankorder_pop_invlog_nodem_topn_polity_740$color,
                                                           levels = unique(rankorder_pop_invlog_nodem_topn_polity_740$color))

tilecolor <- as.character(sort(unique(rankorder_pop_invlog_nodem_topn_polity_740$color)))

g740_pop <- ggplot(rankorder_pop_invlog_nodem_topn_polity_740,
                   aes(x = decade, 
                       y = ranking,
                       alpha = factor(medianindicator),
                       color = factor(medianindicator),
                       fill = factor(countryname),
                       label = factor(countryname))) +
  geom_tile(color = "white") +
  geom_text(fontface = "bold", size = 4, alpha = 1) +
  theme(legend.position = "none") +
  scale_fill_manual(values = tilecolor) +
  scale_color_manual(values = c("darkgrey", "white")) +
  scale_alpha_discrete(range = c(0.3, 0.9)) +
  scale_y_reverse(breaks = seq(1, 10),
                  expand = c(0, 0)) +
  scale_x_continuous(breaks = seq(1800, 2010, 10),
                     expand = c(0, 0)) +
  theme_classic() +
  theme(legend.position = "none",
        panel.grid = element_blank(),
        panel.border = element_blank(),
        axis.line = element_blank(),
        axis.ticks = element_blank(),
        axis.text = element_text(size = 12)) +
  labs(x = "",
       y = "",
       title = "Population rankorder graph for Japan",
       subtitle = "Preference compatibility measured via Polity2.")


g740 <- arrangeGrob(g740_1095,
                    g740_gdp, 
                    g740_pop, 
                    ncol = 1)

# Figure 4

### 1095 ###
rankorder_surplus1095_invlog_nodem_topn_polity_200 <- rankorder_surplus1095_invlog_nodem_topn_polity[[10]]
rankorder_surplus1095_invlog_nodem_topn_polity_200 <- filter(rankorder_surplus1095_invlog_nodem_topn_polity_200, decade != 2010)
names(rankorder_surplus1095_invlog_nodem_topn_polity_200)[2] <- "ccode"
rankorder_surplus1095_invlog_nodem_topn_polity_200$ccode <- as.character(rankorder_surplus1095_invlog_nodem_topn_polity_200$ccode)

tilecolor_df_surplus1095_200 <- tilecolor_df %>%
  filter(targetcountry == 200, variant == "surplus1095") %>%
  select(ccode, color) %>%
  arrange(color) %>%
  dplyr::mutate(ccode = as.character(ccode))

rankorder_surplus1095_invlog_nodem_topn_polity_200 <- left_join(rankorder_surplus1095_invlog_nodem_topn_polity_200, tilecolor_df_surplus1095_200)
rankorder_surplus1095_invlog_nodem_topn_polity_200$countryname <- factor(rankorder_surplus1095_invlog_nodem_topn_polity_200$countryname,
                                                                         levels = unique(rankorder_surplus1095_invlog_nodem_topn_polity_200$countryname))
rankorder_surplus1095_invlog_nodem_topn_polity_200$color <- factor(rankorder_surplus1095_invlog_nodem_topn_polity_200$color,
                                                                   levels = unique(rankorder_surplus1095_invlog_nodem_topn_polity_200$color))

tilecolor <- as.character(sort(unique(rankorder_surplus1095_invlog_nodem_topn_polity_200$color)))


g200_1095 <- ggplot(rankorder_surplus1095_invlog_nodem_topn_polity_200,
                    aes(x = decade, 
                        y = ranking,
                        alpha = factor(medianindicator),
                        color = factor(medianindicator),
                        fill = factor(countryname),
                        label = factor(countryname))) +
  geom_tile(color = "white") +
  geom_text(fontface = "bold", size = 4, alpha = 1) +
  theme(legend.position = "none") +
  scale_fill_manual(values = tilecolor) +
  scale_color_manual(values = c("darkgrey", "white")) +
  scale_alpha_discrete(range = c(0.3, 0.9)) +
  scale_y_reverse(breaks = seq(1, 10),
                  expand = c(0, 0)) +
  scale_x_continuous(breaks = seq(1800, 2010, 10),
                     expand = c(0, 0)) +
  theme_classic() +
  theme(legend.position = "none",
        panel.grid = element_blank(),
        panel.border = element_blank(),
        axis.line = element_blank(),
        axis.ticks = element_blank(),
        axis.text = element_text(size = 12)) +
  labs(x = "",
       y = "",
       title = "SDP ($3 subsistence level) rankorder graph for the United Kingdom",
       subtitle = "Preference compatibility measured via Polity2.") 
g200_1095

### GDP ###
rankorder_gdp_invlog_nodem_topn_polity_200 <- rankorder_gdp_invlog_nodem_topn_polity[[10]]
rankorder_gdp_invlog_nodem_topn_polity_200 <- filter(rankorder_gdp_invlog_nodem_topn_polity_200, decade != 2010)
names(rankorder_gdp_invlog_nodem_topn_polity_200)[2] <- "ccode"
rankorder_gdp_invlog_nodem_topn_polity_200$ccode <- as.character(rankorder_gdp_invlog_nodem_topn_polity_200$ccode)


tilecolor_df_gdp_200 <- tilecolor_df %>%
  filter(targetcountry == 200, variant == "gdp") %>%
  select(ccode, color) %>%
  arrange(color) %>%
  dplyr::mutate(ccode = as.character(ccode))

rankorder_gdp_invlog_nodem_topn_polity_200 <- left_join(rankorder_gdp_invlog_nodem_topn_polity_200, tilecolor_df_gdp_200)
rankorder_gdp_invlog_nodem_topn_polity_200$countryname <- factor(rankorder_gdp_invlog_nodem_topn_polity_200$countryname,
                                                                 levels = unique(rankorder_gdp_invlog_nodem_topn_polity_200$countryname))
rankorder_gdp_invlog_nodem_topn_polity_200$color <- factor(rankorder_gdp_invlog_nodem_topn_polity_200$color,
                                                           levels = unique(rankorder_gdp_invlog_nodem_topn_polity_200$color))

tilecolor <- as.character(sort(unique(rankorder_gdp_invlog_nodem_topn_polity_200$color)))

g200_gdp <- ggplot(rankorder_gdp_invlog_nodem_topn_polity_200,
                   aes(x = decade, 
                       y = ranking,
                       alpha = factor(medianindicator),
                       color = factor(medianindicator),
                       fill = factor(countryname),
                       label = factor(countryname))) +
  geom_tile(color = "white") +
  geom_text(fontface = "bold", size = 4, alpha = 1) +
  theme(legend.position = "none") +
  scale_fill_manual(values = tilecolor) +
  scale_color_manual(values = c("darkgrey", "white")) +
  scale_alpha_discrete(range = c(0.3, 0.9)) +
  scale_y_reverse(breaks = seq(1, 10),
                  expand = c(0, 0)) +
  scale_x_continuous(breaks = seq(1800, 2010, 10),
                     expand = c(0, 0)) +
  theme_classic() +
  theme(legend.position = "none",
        panel.grid = element_blank(),
        panel.border = element_blank(),
        axis.line = element_blank(),
        axis.ticks = element_blank(),
        axis.text = element_text(size = 12)) +
  labs(x = "",
       y = "",
       title = "GDP rankorder graph for the United Kingdom",
       subtitle = "Preference compatibility measured via Polity2.")
g200_gdp


### Pop ###
rankorder_pop_invlog_nodem_topn_polity_200 <- rankorder_pop_invlog_nodem_topn_polity[[10]]
rankorder_pop_invlog_nodem_topn_polity_200 <- filter(rankorder_pop_invlog_nodem_topn_polity_200, decade != 2010)
names(rankorder_pop_invlog_nodem_topn_polity_200)[2] <- "ccode"
rankorder_pop_invlog_nodem_topn_polity_200$ccode <- as.character(rankorder_pop_invlog_nodem_topn_polity_200$ccode)


tilecolor_df_pop_200 <- tilecolor_df %>%
  filter(targetcountry == 200, variant == "pop") %>%
  select(ccode, color) %>%
  arrange(color) %>%
  dplyr::mutate(ccode = as.character(ccode))

rankorder_pop_invlog_nodem_topn_polity_200 <- left_join(rankorder_pop_invlog_nodem_topn_polity_200, tilecolor_df_pop_200)
rankorder_pop_invlog_nodem_topn_polity_200$countryname <- factor(rankorder_pop_invlog_nodem_topn_polity_200$countryname,
                                                                 levels = unique(rankorder_pop_invlog_nodem_topn_polity_200$countryname))
rankorder_pop_invlog_nodem_topn_polity_200$color <- factor(rankorder_pop_invlog_nodem_topn_polity_200$color,
                                                           levels = unique(rankorder_pop_invlog_nodem_topn_polity_200$color))

tilecolor <- as.character(sort(unique(rankorder_pop_invlog_nodem_topn_polity_200$color)))

g200_pop <- ggplot(rankorder_pop_invlog_nodem_topn_polity_200,
                   aes(x = decade, 
                       y = ranking,
                       alpha = factor(medianindicator),
                       color = factor(medianindicator),
                       fill = factor(countryname),
                       label = factor(countryname))) +
  geom_tile(color = "white") +
  geom_text(fontface = "bold", size = 4, alpha = 1) +
  theme(legend.position = "none") +
  scale_fill_manual(values = tilecolor) +
  scale_color_manual(values = c("darkgrey", "white")) +
  scale_alpha_discrete(range = c(0.3, 0.9)) +
  scale_y_reverse(breaks = seq(1, 10),
                  expand = c(0, 0)) +
  scale_x_continuous(breaks = seq(1800, 2010, 10),
                     expand = c(0, 0)) +
  theme_classic() +
  theme(legend.position = "none",
        panel.grid = element_blank(),
        panel.border = element_blank(),
        axis.line = element_blank(),
        axis.ticks = element_blank(),
        axis.text = element_text(size = 12)) +
  labs(x = "",
       y = "",
       title = "Population rankorder graph for the United Kingdom",
       subtitle = "Preference compatibility measured via Polity2.")


g200 <- arrangeGrob(g200_1095,
                    g200_gdp, 
                    g200_pop, 
                    ncol = 1)

# Figure 5

### Computing CINC without China
cinc_annualtotal_nochina <- cincdat %>%
  dplyr::select(ccode,
                year,
                irst,
                pec,
                tpop,
                upop,
                milex,
                milper,
                country) %>%
  filter(ccode != 710) %>%
  group_by(year) %>%
  dplyr::summarise(irst_total = sum(irst, na.rm = T),
                   pec_total = sum(pec, na.rm = T),
                   tpop_total = sum(tpop, na.rm = T),
                   upop_total = sum(upop, na.rm = T),
                   milex_total = sum(milex, na.rm = T),
                   milper_total = sum(milper, na.rm = T))

# Annual total of GDP and SDP without China
cinc_new_nochina <- cincdat %>%
  left_join(cinc_annualtotal_nochina) %>%
  filter(ccode != 710) %>%
  dplyr::mutate(irst_sumirst_nochina = irst/irst_total,
                pec_sumpec_nochina = pec/pec_total,
                tpop_sumtpop_nochina = tpop/tpop_total,
                upop_sumupop_nochina = upop/upop_total,
                milex_summilex_nochina = milex/milex_total,
                milper_summilper_nochina = milper/milper_total) %>%
  dplyr::mutate(cinc_recomputed_nochina = rowMeans(data.frame(irst_sumirst_nochina,
                                                              pec_sumpec_nochina,
                                                              tpop_sumtpop_nochina,
                                                              upop_sumupop_nochina,
                                                              milex_summilex_nochina,
                                                              milper_summilper_nochina))) %>%
  dplyr::select(ccode,
                year,
                contains("nochina"))

# Cinc without china (to make observations match)
cinc_new_drop <- cinc_new %>%
  filter(ccode != 710)

master_annual_nochina <- master %>%
  filter(ccode != 710) %>%
  dplyr::select(ccode,
                year,
                surplus1095,
                WorldBank_gdp_2011_ppp_estimate,
                WorldBank_pop_estimate) %>%
  dplyr::mutate(surplus1095_estimate_truncatedzero = surplus1095) %>%
  group_by(year) %>%
  dplyr::summarize(sumgdp_nochina = sum(WorldBank_gdp_2011_ppp_estimate, na.rm = T),
                   sumsurplus_nochina = sum(surplus1095_estimate_truncatedzero, na.rm = T),
                   sumpop_nochina = sum(WorldBank_pop_estimate, na.rm = T))

years_cinc <- unique(cinc_new_drop$year)
cors_china <- list()
for(y in 1:length(years_cinc)){
  sub1 <- cinc_new_drop %>%
    filter(year == years_cinc[y])
  sub2 <- cinc_new_nochina %>%
    filter(year == years_cinc[y])
  
  result <- tryCatch({
    cor <- cor.test(sub1$cinc_recomputed, sub2$cinc_recomputed, conf.level = 0.95)
    val <- as.numeric(cor$estimate)
    val_cilo <- as.numeric(cor$conf.int[1])
    val_cihi <- as.numeric(cor$conf.int[2])
  }, error = function(e) {
    print(paste("Error on year ", years_cinc[y]))
    val <- NA
    val_cilo <- NA
    val_cihi <- NA
  })
  
  cors_china[[y]] <- c(years_cinc[y], val, val_cilo, val_cihi)
}

df_cors_china <- matrix(unlist(cors_china), nrow = length(years_cinc), byrow = T) %>%
  as.data.frame() %>%
  dplyr::select(year = V1,
                estimate = V2,
                cilo = V3, 
                cihi = V4)

ggplot(df_cors_china,
       aes(x = year,
           y = estimate,
           ymin = cilo,
           ymax = cihi)) +
  geom_point(size = 0.5) +
  geom_linerange(alpha = 0.3) +
  labs(title = "Influence of China on CINC score",
       subtitle = "China observations are excluded from both series", 
       x = "Year",
       y = "Correlation between CINC with and without China") +
  theme_light() +
  coord_cartesian(ylim = c(0.992, 1)) +
  scale_x_continuous(breaks = seq(1800, 2010, 20))

# Figure 6 

master_relsurplus_all_nochina <- master %>%
  # filter(ccode != 710) %>%
  dplyr::select(ccode,
                year,
                surplus1095,
                WorldBank_gdp_2011_ppp_estimate,
                WorldBank_pop_estimate) %>%
  dplyr::mutate(surplus1095_estimate_truncatedzero = surplus1095) %>%
  left_join(master_annual_nochina, by = "year") %>%
  left_join(master_annual, by = "year") %>%
  dplyr::mutate(relsurplus = 1/sumsurplus*surplus1095_estimate_truncatedzero,
                relgdp = 1/sumgdp*WorldBank_gdp_2011_ppp_estimate,
                relpop = 1/sumpop*WorldBank_pop_estimate,
                relsurplus_nochina = 1/sumsurplus_nochina*surplus1095_estimate_truncatedzero,
                relgdp_nochina = 1/sumgdp_nochina*WorldBank_gdp_2011_ppp_estimate,
                relpop_nochina = 1/sumpop_nochina*WorldBank_pop_estimate) %>%
  filter(year >= 1816) %>%
  left_join(cinc_new_nochina) %>%
  dplyr::select(ccode,
                year,
                contains("rel"),
                contains("nochina"))

cors_new_nochina <- list()
for(y in 1:length(years2)){
  
  # Subsetting data fram
  sub <- master_relsurplus_all_nochina %>%
    filter(year == years2[y]) 
  
  ##### SDP #####
  # irst
  result_sdp_irst <- tryCatch({sdp_irst <- cor.test(sub$relsurplus_nochina, sub$irst_sumirst_nochina, conf.level = 0.95)
  val_sdp_irst <- as.numeric(sdp_irst$estimate)
  val_sdp_irst_cilo <- as.numeric(sdp_irst$conf.int[1])
  val_sdp_irst_cihi <- as.numeric(sdp_irst$conf.int[2])
  }, error = function(e) {
    print(paste("Error on year ", years2[y]))
    val_sdp_irst <- NA
    val_sdp_irst_cilo <- NA
    val_sdp_irst_cihi <- NA      
  })
  
  # pec
  result_sdp_pec <- tryCatch({sdp_pec <- cor.test(sub$relsurplus_nochina, sub$pec_sumpec_nochina, conf.level = 0.95)
  val_sdp_pec <- as.numeric(sdp_pec$estimate)
  val_sdp_pec_cilo <- as.numeric(sdp_pec$conf.int[1])
  val_sdp_pec_cihi <- as.numeric(sdp_pec$conf.int[2])
  }, error = function(e) {
    print(paste("Error on year ", years2[y]))
    val_sdp_pec <- NA
    val_sdp_pec_cilo <- NA
    val_sdp_pec_cihi <- NA      
  })
  
  # upop
  result_sdp_upop <- tryCatch({sdp_upop <- cor.test(sub$relsurplus_nochina, sub$upop_sumupop_nochina, conf.level = 0.95)
  val_sdp_upop <- as.numeric(sdp_upop$estimate)
  val_sdp_upop_cilo <- as.numeric(sdp_upop$conf.int[1])
  val_sdp_upop_cihi <- as.numeric(sdp_upop$conf.int[2])
  }, error = function(e) {
    print(paste("Error on year ", years2[y]))
    val_sdp_upop <- NA
    val_sdp_upop_cilo <- NA
    val_sdp_upop_cihi <- NA      
  })
  
  # tpop
  result_sdp_tpop <- tryCatch({sdp_tpop <- cor.test(sub$relsurplus_nochina, sub$tpop_sumtpop_nochina, conf.level = 0.95)
  val_sdp_tpop <- as.numeric(sdp_tpop$estimate)
  val_sdp_tpop_cilo <- as.numeric(sdp_tpop$conf.int[1])
  val_sdp_tpop_cihi <- as.numeric(sdp_tpop$conf.int[2])
  }, error = function(e) {
    print(paste("Error on year ", years2[y]))
    val_sdp_tpop <- NA
    val_sdp_tpop_cilo <- NA
    val_sdp_tpop_cihi <- NA      
  })
  
  # relpop
  result_sdp_relpop <- tryCatch({sdp_relpop <- cor.test(sub$relsurplus_nochina, sub$relpop_nochina, conf.level = 0.95)
  val_sdp_relpop <- as.numeric(sdp_relpop$estimate)
  val_sdp_relpop_cilo <- as.numeric(sdp_relpop$conf.int[1])
  val_sdp_relpop_cihi <- as.numeric(sdp_relpop$conf.int[2])
  }, error = function(e) {
    print(paste("Error on year ", years2[y]))
    val_sdp_relpop <- NA
    val_sdp_relpop_cilo <- NA
    val_sdp_relpop_cihi <- NA      
  })
  
  # milex
  result_sdp_milex <- tryCatch({sdp_milex <- cor.test(sub$relsurplus_nochina, sub$milex_summilex_nochina, conf.level = 0.95)
  val_sdp_milex <- as.numeric(sdp_milex$estimate)
  val_sdp_milex_cilo <- as.numeric(sdp_milex$conf.int[1])
  val_sdp_milex_cihi <- as.numeric(sdp_milex$conf.int[2])
  }, error = function(e) {
    print(paste("Error on year ", years2[y]))
    val_sdp_milex <- NA
    val_sdp_milex_cilo <- NA
    val_sdp_milex_cihi <- NA      
  })
  
  # milper
  result_sdp_milper <- tryCatch({sdp_milper <- cor.test(sub$relsurplus_nochina, sub$milper_summilper_nochina, conf.level = 0.95)
  val_sdp_milper <- as.numeric(sdp_milper$estimate)
  val_sdp_milper_cilo <- as.numeric(sdp_milper$conf.int[1])
  val_sdp_milper_cihi <- as.numeric(sdp_milper$conf.int[2])
  }, error = function(e) {
    print(paste("Error on year ", years2[y]))
    val_sdp_milper <- NA
    val_sdp_milper_cilo <- NA
    val_sdp_milper_cihi <- NA      
  })
  
  
  
  ##### GDP #####
  # irst
  result_gdp_irst <- tryCatch({gdp_irst <- cor.test(sub$relgdp_nochina, sub$irst_sumirst_nochina, conf.level = 0.95)
  val_gdp_irst <- as.numeric(gdp_irst$estimate)
  val_gdp_irst_cilo <- as.numeric(gdp_irst$conf.int[1])
  val_gdp_irst_cihi <- as.numeric(gdp_irst$conf.int[2])
  }, error = function(e) {
    print(paste("Error on year ", years2[y]))
    val_gdp_irst <- NA
    val_gdp_irst_cilo <- NA
    val_gdp_irst_cihi <- NA      
  })
  
  # pec
  result_gdp_pec <- tryCatch({gdp_pec <- cor.test(sub$relgdp_nochina, sub$pec_sumpec_nochina, conf.level = 0.95)
  val_gdp_pec <- as.numeric(gdp_pec$estimate)
  val_gdp_pec_cilo <- as.numeric(gdp_pec$conf.int[1])
  val_gdp_pec_cihi <- as.numeric(gdp_pec$conf.int[2])
  }, error = function(e) {
    print(paste("Error on year ", years2[y]))
    val_gdp_pec <- NA
    val_gdp_pec_cilo <- NA
    val_gdp_pec_cihi <- NA      
  })
  
  # upop
  result_gdp_upop <- tryCatch({gdp_upop <- cor.test(sub$relgdp_nochina, sub$upop_sumupop_nochina, conf.level = 0.95)
  val_gdp_upop <- as.numeric(gdp_upop$estimate)
  val_gdp_upop_cilo <- as.numeric(gdp_upop$conf.int[1])
  val_gdp_upop_cihi <- as.numeric(gdp_upop$conf.int[2])
  }, error = function(e) {
    print(paste("Error on year ", years2[y]))
    val_gdp_upop <- NA
    val_gdp_upop_cilo <- NA
    val_gdp_upop_cihi <- NA      
  })
  
  # tpop
  result_gdp_tpop <- tryCatch({gdp_tpop <- cor.test(sub$relgdp_nochina, sub$tpop_sumtpop_nochina, conf.level = 0.95)
  val_gdp_tpop <- as.numeric(gdp_tpop$estimate)
  val_gdp_tpop_cilo <- as.numeric(gdp_tpop$conf.int[1])
  val_gdp_tpop_cihi <- as.numeric(gdp_tpop$conf.int[2])
  }, error = function(e) {
    print(paste("Error on year ", years2[y]))
    val_gdp_tpop <- NA
    val_gdp_tpop_cilo <- NA
    val_gdp_tpop_cihi <- NA      
  })
  
  # relpop
  result_gdp_relpop <- tryCatch({gdp_relpop <- cor.test(sub$relgdp_nochina, sub$relpop_nochina, conf.level = 0.95)
  val_gdp_relpop <- as.numeric(gdp_relpop$estimate)
  val_gdp_relpop_cilo <- as.numeric(gdp_relpop$conf.int[1])
  val_gdp_relpop_cihi <- as.numeric(gdp_relpop$conf.int[2])
  }, error = function(e) {
    print(paste("Error on year ", years2[y]))
    val_gdp_relpop <- NA
    val_gdp_relpop_cilo <- NA
    val_gdp_relpop_cihi <- NA      
  })
  
  # milex
  result_gdp_milex <- tryCatch({gdp_milex <- cor.test(sub$relgdp_nochina, sub$milex_summilex_nochina, conf.level = 0.95)
  val_gdp_milex <- as.numeric(gdp_milex$estimate)
  val_gdp_milex_cilo <- as.numeric(gdp_milex$conf.int[1])
  val_gdp_milex_cihi <- as.numeric(gdp_milex$conf.int[2])
  }, error = function(e) {
    print(paste("Error on year ", years2[y]))
    val_gdp_milex <- NA
    val_gdp_milex_cilo <- NA
    val_gdp_milex_cihi <- NA      
  })
  
  # milper
  result_gdp_milper <- tryCatch({gdp_milper <- cor.test(sub$relgdp_nochina, sub$milper_summilper_nochina, conf.level = 0.95)
  val_gdp_milper <- as.numeric(gdp_milper$estimate)
  val_gdp_milper_cilo <- as.numeric(gdp_milper$conf.int[1])
  val_gdp_milper_cihi <- as.numeric(gdp_milper$conf.int[2])
  }, error = function(e) {
    print(paste("Error on year ", years2[y]))
    val_gdp_milper <- NA
    val_gdp_milper_cilo <- NA
    val_gdp_milper_cihi <- NA      
  })
  
  
  
  cors_new_nochina[[y]] <- c(years2[y], 
                             
                             # SDP correlations
                             val_sdp_irst, val_sdp_irst_cilo, val_sdp_irst_cihi,
                             val_sdp_pec, val_sdp_pec_cilo, val_sdp_pec_cihi,
                             val_sdp_upop, val_sdp_upop_cilo, val_sdp_upop_cihi,
                             val_sdp_tpop, val_sdp_tpop_cilo, val_sdp_tpop_cihi,
                             val_sdp_relpop, val_sdp_relpop_cilo, val_sdp_relpop_cihi,
                             val_sdp_milex, val_sdp_milex_cilo, val_sdp_milex_cihi,
                             val_sdp_milper, val_sdp_milper_cilo, val_sdp_milper_cihi,
                             
                             # GDP correlations
                             val_gdp_irst, val_gdp_irst_cilo, val_gdp_irst_cihi,
                             val_gdp_pec, val_gdp_pec_cilo, val_gdp_pec_cihi,
                             val_gdp_upop, val_gdp_upop_cilo, val_gdp_upop_cihi,
                             val_gdp_tpop, val_gdp_tpop_cilo, val_gdp_tpop_cihi,
                             val_gdp_relpop, val_gdp_relpop_cilo, val_gdp_relpop_cihi,
                             val_gdp_milex, val_gdp_milex_cilo, val_gdp_milex_cihi,
                             val_gdp_milper, val_gdp_milper_cilo, val_gdp_milper_cihi)
  
}


df_cors_new_nochina <- matrix(unlist(cors_new_nochina), nrow = length(years2), byrow = T) %>%
  as.data.frame() 

names(df_cors_new_nochina) <- c("year", 
                                # SDP correlations
                                "sdp_irst", "sdp_irst_cilo", "sdp_irst_cihi",
                                "sdp_pec", "sdp_pec_cilo", "sdp_pec_cihi",
                                "sdp_upop", "sdp_upop_cilo", "sdp_upop_cihi",
                                "sdp_tpop", "sdp_tpop_cilo", "sdp_tpop_cihi",
                                "sdp_relpop", "sdp_relpop_cilo", "sdp_relpop_cihi",
                                "val_sdp_milex", "val_sdp_milex_cilo", "val_sdp_milex_cihi",
                                "val_sdp_milper", "val_sdp_milper_cilo", "val_sdp_milper_cihi",
                                
                                # GDP correlations
                                "gdp_irst", "gdp_irst_cilo", "gdp_irst_cihi",
                                "gdp_pec", "gdp_pec_cilo", "gdp_pec_cihi",
                                "gdp_upop", "gdp_upop_cilo", "gdp_upop_cihi",
                                "gdp_tpop", "gdp_tpop_cilo", "gdp_tpop_cihi",
                                "gdp_relpop", "gdp_relpop_cilo", "gdp_relpop_cihi",
                                "val_gdp_milex", "val_gdp_milex_cilo", "val_gdp_milex_cihi",
                                "val_gdp_milper", "val_gdp_milper_cilo", "val_gdp_milper_cihi")

df_cors_new_nochina_long <- df_cors_new_nochina %>%
  gather(indicator, value, -year) %>%
  dplyr::mutate(type = ifelse(str_detect(indicator, "cilo"), "cilo", "estimate"),
                type = replace(type, str_detect(indicator, "cihi"), "cihi")) %>%
  dplyr::mutate(comp = ifelse(str_detect(indicator, "sdp"), "sdp", "gdp")) %>%
  dplyr::mutate(variable = ifelse(str_detect(indicator, "irst"), "irst", NA),
                variable = replace(variable, str_detect(indicator, "pec"), "pec"),
                variable = replace(variable, str_detect(indicator, "tpop"), "tpop"),
                variable = replace(variable, str_detect(indicator, "upop"), "upop"),
                variable = replace(variable, str_detect(indicator, "relpop"), "relpop"),
                variable = replace(variable, str_detect(indicator, "milex"), "milex"),
                variable = replace(variable, str_detect(indicator, "milper"), "milper")) %>%
  dplyr::select(-indicator) %>% 
  spread(type, value)# currently not working

df_cors_new_nochina_long$comp <- factor(df_cors_new_nochina_long$comp,
                                        levels = c("sdp", "gdp"),
                                        labels = c("Share of global SDP",
                                                   "Share of global GDP"))

df_cors_new_nochina_long$variable <- factor(df_cors_new_nochina_long$variable,
                                            levels = c("irst",
                                                       "pec",
                                                       "upop",
                                                       "relpop", #these are our new estimates
                                                       "tpop",
                                                       "milex",
                                                       "milper"),
                                            labels = c("Share of global iron and steel production",
                                                       "Share of global primary energy consumption",
                                                       "Share of global urban population",
                                                       "Share of global population",
                                                       "Share of global population ",
                                                       "Share of global military expenditure",
                                                       "Share of global military personnel"))



ggplot(subset(df_cors_new_nochina_long, !is.na(estimate) & variable %in% c("Share of global iron and steel production",
                                                                           "Share of global primary energy consumption")),
       aes(x = year, y = estimate, color = comp, shape = comp)) +
  geom_linerange(aes(ymin = cilo, ymax = cihi), size = 0.2, alpha = 0.3, show.legend = F) +
  geom_point(size = 0.9) +
  theme_bw() +
  labs(x = "Year",
       y = "Pearson's product-moment correlation coefficient",
       title = "Correlation between CINC components and global shares of SDP vs. GDP",
       subtitle = "China is excluded from global totals of CINC, global SDP, and global GDP") +
  scale_color_manual(values = c("darkorange3", #orange
                                "gray50"), #green
                     name = "Share of global economic power-resources") +
  scale_shape_discrete(name = "Share of global economic power-resources") +
  theme(legend.position = "top",
        panel.grid.minor = element_blank(),
        strip.text = element_text(size = 10),
        legend.text = element_text(size = 10)) +
  guides(color = guide_legend(override.aes = list(size = 5, alpha = 1))) +
  coord_cartesian(xlim = c(1816,2012),
                  ylim = c(0,1)) +
  facet_wrap(~variable, ncol = 1) +
  scale_x_continuous(breaks = seq(1800, 2000, 25))

### Version 2: drop China from both CINC but not SDP/GDP
cors_new_nochina_v2 <- list()
for(y in 1:length(years2)){
  
  # Subsetting data fram
  sub <- master_relsurplus_all_nochina %>%
    filter(year == years2[y]) 
  
  ##### SDP #####
  # irst
  result_sdp_irst <- tryCatch({sdp_irst <- cor.test(sub$relsurplus, sub$irst_sumirst_nochina, conf.level = 0.95)
  val_sdp_irst <- as.numeric(sdp_irst$estimate)
  val_sdp_irst_cilo <- as.numeric(sdp_irst$conf.int[1])
  val_sdp_irst_cihi <- as.numeric(sdp_irst$conf.int[2])
  }, error = function(e) {
    print(paste("Error on year ", years2[y]))
    val_sdp_irst <- NA
    val_sdp_irst_cilo <- NA
    val_sdp_irst_cihi <- NA      
  })
  
  # pec
  result_sdp_pec <- tryCatch({sdp_pec <- cor.test(sub$relsurplus, sub$pec_sumpec_nochina, conf.level = 0.95)
  val_sdp_pec <- as.numeric(sdp_pec$estimate)
  val_sdp_pec_cilo <- as.numeric(sdp_pec$conf.int[1])
  val_sdp_pec_cihi <- as.numeric(sdp_pec$conf.int[2])
  }, error = function(e) {
    print(paste("Error on year ", years2[y]))
    val_sdp_pec <- NA
    val_sdp_pec_cilo <- NA
    val_sdp_pec_cihi <- NA      
  })
  
  # upop
  result_sdp_upop <- tryCatch({sdp_upop <- cor.test(sub$relsurplus, sub$upop_sumupop_nochina, conf.level = 0.95)
  val_sdp_upop <- as.numeric(sdp_upop$estimate)
  val_sdp_upop_cilo <- as.numeric(sdp_upop$conf.int[1])
  val_sdp_upop_cihi <- as.numeric(sdp_upop$conf.int[2])
  }, error = function(e) {
    print(paste("Error on year ", years2[y]))
    val_sdp_upop <- NA
    val_sdp_upop_cilo <- NA
    val_sdp_upop_cihi <- NA      
  })
  
  # tpop
  result_sdp_tpop <- tryCatch({sdp_tpop <- cor.test(sub$relsurplus, sub$tpop_sumtpop_nochina, conf.level = 0.95)
  val_sdp_tpop <- as.numeric(sdp_tpop$estimate)
  val_sdp_tpop_cilo <- as.numeric(sdp_tpop$conf.int[1])
  val_sdp_tpop_cihi <- as.numeric(sdp_tpop$conf.int[2])
  }, error = function(e) {
    print(paste("Error on year ", years2[y]))
    val_sdp_tpop <- NA
    val_sdp_tpop_cilo <- NA
    val_sdp_tpop_cihi <- NA      
  })
  
  # relpop
  result_sdp_relpop <- tryCatch({sdp_relpop <- cor.test(sub$relsurplus, sub$relpop_nochina, conf.level = 0.95)
  val_sdp_relpop <- as.numeric(sdp_relpop$estimate)
  val_sdp_relpop_cilo <- as.numeric(sdp_relpop$conf.int[1])
  val_sdp_relpop_cihi <- as.numeric(sdp_relpop$conf.int[2])
  }, error = function(e) {
    print(paste("Error on year ", years2[y]))
    val_sdp_relpop <- NA
    val_sdp_relpop_cilo <- NA
    val_sdp_relpop_cihi <- NA      
  })
  
  # milex
  result_sdp_milex <- tryCatch({sdp_milex <- cor.test(sub$relsurplus, sub$milex_summilex_nochina, conf.level = 0.95)
  val_sdp_milex <- as.numeric(sdp_milex$estimate)
  val_sdp_milex_cilo <- as.numeric(sdp_milex$conf.int[1])
  val_sdp_milex_cihi <- as.numeric(sdp_milex$conf.int[2])
  }, error = function(e) {
    print(paste("Error on year ", years2[y]))
    val_sdp_milex <- NA
    val_sdp_milex_cilo <- NA
    val_sdp_milex_cihi <- NA      
  })
  
  # milper
  result_sdp_milper <- tryCatch({sdp_milper <- cor.test(sub$relsurplus, sub$milper_summilper_nochina, conf.level = 0.95)
  val_sdp_milper <- as.numeric(sdp_milper$estimate)
  val_sdp_milper_cilo <- as.numeric(sdp_milper$conf.int[1])
  val_sdp_milper_cihi <- as.numeric(sdp_milper$conf.int[2])
  }, error = function(e) {
    print(paste("Error on year ", years2[y]))
    val_sdp_milper <- NA
    val_sdp_milper_cilo <- NA
    val_sdp_milper_cihi <- NA      
  })
  
  
  
  ##### GDP #####
  # irst
  result_gdp_irst <- tryCatch({gdp_irst <- cor.test(sub$relgdp, sub$irst_sumirst_nochina, conf.level = 0.95)
  val_gdp_irst <- as.numeric(gdp_irst$estimate)
  val_gdp_irst_cilo <- as.numeric(gdp_irst$conf.int[1])
  val_gdp_irst_cihi <- as.numeric(gdp_irst$conf.int[2])
  }, error = function(e) {
    print(paste("Error on year ", years2[y]))
    val_gdp_irst <- NA
    val_gdp_irst_cilo <- NA
    val_gdp_irst_cihi <- NA      
  })
  
  # pec
  result_gdp_pec <- tryCatch({gdp_pec <- cor.test(sub$relgdp, sub$pec_sumpec_nochina, conf.level = 0.95)
  val_gdp_pec <- as.numeric(gdp_pec$estimate)
  val_gdp_pec_cilo <- as.numeric(gdp_pec$conf.int[1])
  val_gdp_pec_cihi <- as.numeric(gdp_pec$conf.int[2])
  }, error = function(e) {
    print(paste("Error on year ", years2[y]))
    val_gdp_pec <- NA
    val_gdp_pec_cilo <- NA
    val_gdp_pec_cihi <- NA      
  })
  
  # upop
  result_gdp_upop <- tryCatch({gdp_upop <- cor.test(sub$relgdp, sub$upop_sumupop_nochina, conf.level = 0.95)
  val_gdp_upop <- as.numeric(gdp_upop$estimate)
  val_gdp_upop_cilo <- as.numeric(gdp_upop$conf.int[1])
  val_gdp_upop_cihi <- as.numeric(gdp_upop$conf.int[2])
  }, error = function(e) {
    print(paste("Error on year ", years2[y]))
    val_gdp_upop <- NA
    val_gdp_upop_cilo <- NA
    val_gdp_upop_cihi <- NA      
  })
  
  # tpop
  result_gdp_tpop <- tryCatch({gdp_tpop <- cor.test(sub$relgdp, sub$tpop_sumtpop_nochina, conf.level = 0.95)
  val_gdp_tpop <- as.numeric(gdp_tpop$estimate)
  val_gdp_tpop_cilo <- as.numeric(gdp_tpop$conf.int[1])
  val_gdp_tpop_cihi <- as.numeric(gdp_tpop$conf.int[2])
  }, error = function(e) {
    print(paste("Error on year ", years2[y]))
    val_gdp_tpop <- NA
    val_gdp_tpop_cilo <- NA
    val_gdp_tpop_cihi <- NA      
  })
  
  # relpop
  result_gdp_relpop <- tryCatch({gdp_relpop <- cor.test(sub$relgdp, sub$relpop_nochina, conf.level = 0.95)
  val_gdp_relpop <- as.numeric(gdp_relpop$estimate)
  val_gdp_relpop_cilo <- as.numeric(gdp_relpop$conf.int[1])
  val_gdp_relpop_cihi <- as.numeric(gdp_relpop$conf.int[2])
  }, error = function(e) {
    print(paste("Error on year ", years2[y]))
    val_gdp_relpop <- NA
    val_gdp_relpop_cilo <- NA
    val_gdp_relpop_cihi <- NA      
  })
  
  # milex
  result_gdp_milex <- tryCatch({gdp_milex <- cor.test(sub$relgdp, sub$milex_summilex_nochina, conf.level = 0.95)
  val_gdp_milex <- as.numeric(gdp_milex$estimate)
  val_gdp_milex_cilo <- as.numeric(gdp_milex$conf.int[1])
  val_gdp_milex_cihi <- as.numeric(gdp_milex$conf.int[2])
  }, error = function(e) {
    print(paste("Error on year ", years2[y]))
    val_gdp_milex <- NA
    val_gdp_milex_cilo <- NA
    val_gdp_milex_cihi <- NA      
  })
  
  # milper
  result_gdp_milper <- tryCatch({gdp_milper <- cor.test(sub$relgdp, sub$milper_summilper_nochina, conf.level = 0.95)
  val_gdp_milper <- as.numeric(gdp_milper$estimate)
  val_gdp_milper_cilo <- as.numeric(gdp_milper$conf.int[1])
  val_gdp_milper_cihi <- as.numeric(gdp_milper$conf.int[2])
  }, error = function(e) {
    print(paste("Error on year ", years2[y]))
    val_gdp_milper <- NA
    val_gdp_milper_cilo <- NA
    val_gdp_milper_cihi <- NA      
  })
  
  
  
  cors_new_nochina_v2[[y]] <- c(years2[y], 
                                
                                # SDP correlations
                                val_sdp_irst, val_sdp_irst_cilo, val_sdp_irst_cihi,
                                val_sdp_pec, val_sdp_pec_cilo, val_sdp_pec_cihi,
                                val_sdp_upop, val_sdp_upop_cilo, val_sdp_upop_cihi,
                                val_sdp_tpop, val_sdp_tpop_cilo, val_sdp_tpop_cihi,
                                val_sdp_relpop, val_sdp_relpop_cilo, val_sdp_relpop_cihi,
                                val_sdp_milex, val_sdp_milex_cilo, val_sdp_milex_cihi,
                                val_sdp_milper, val_sdp_milper_cilo, val_sdp_milper_cihi,
                                
                                # GDP correlations
                                val_gdp_irst, val_gdp_irst_cilo, val_gdp_irst_cihi,
                                val_gdp_pec, val_gdp_pec_cilo, val_gdp_pec_cihi,
                                val_gdp_upop, val_gdp_upop_cilo, val_gdp_upop_cihi,
                                val_gdp_tpop, val_gdp_tpop_cilo, val_gdp_tpop_cihi,
                                val_gdp_relpop, val_gdp_relpop_cilo, val_gdp_relpop_cihi,
                                val_gdp_milex, val_gdp_milex_cilo, val_gdp_milex_cihi,
                                val_gdp_milper, val_gdp_milper_cilo, val_gdp_milper_cihi)
  
}


df_cors_new_nochina_v2 <- matrix(unlist(cors_new_nochina_v2), nrow = length(years2), byrow = T) %>%
  as.data.frame() 

names(df_cors_new_nochina_v2) <- c("year", 
                                   # SDP correlations
                                   "sdp_irst", "sdp_irst_cilo", "sdp_irst_cihi",
                                   "sdp_pec", "sdp_pec_cilo", "sdp_pec_cihi",
                                   "sdp_upop", "sdp_upop_cilo", "sdp_upop_cihi",
                                   "sdp_tpop", "sdp_tpop_cilo", "sdp_tpop_cihi",
                                   "sdp_relpop", "sdp_relpop_cilo", "sdp_relpop_cihi",
                                   "val_sdp_milex", "val_sdp_milex_cilo", "val_sdp_milex_cihi",
                                   "val_sdp_milper", "val_sdp_milper_cilo", "val_sdp_milper_cihi",
                                   
                                   # GDP correlations
                                   "gdp_irst", "gdp_irst_cilo", "gdp_irst_cihi",
                                   "gdp_pec", "gdp_pec_cilo", "gdp_pec_cihi",
                                   "gdp_upop", "gdp_upop_cilo", "gdp_upop_cihi",
                                   "gdp_tpop", "gdp_tpop_cilo", "gdp_tpop_cihi",
                                   "gdp_relpop", "gdp_relpop_cilo", "gdp_relpop_cihi",
                                   "val_gdp_milex", "val_gdp_milex_cilo", "val_gdp_milex_cihi",
                                   "val_gdp_milper", "val_gdp_milper_cilo", "val_gdp_milper_cihi")

df_cors_new_nochina_v2_long <- df_cors_new_nochina_v2 %>%
  gather(indicator, value, -year) %>%
  dplyr::mutate(type = ifelse(str_detect(indicator, "cilo"), "cilo", "estimate"),
                type = replace(type, str_detect(indicator, "cihi"), "cihi")) %>%
  dplyr::mutate(comp = ifelse(str_detect(indicator, "sdp"), "sdp", "gdp")) %>%
  dplyr::mutate(variable = ifelse(str_detect(indicator, "irst"), "irst", NA),
                variable = replace(variable, str_detect(indicator, "pec"), "pec"),
                variable = replace(variable, str_detect(indicator, "tpop"), "tpop"),
                variable = replace(variable, str_detect(indicator, "upop"), "upop"),
                variable = replace(variable, str_detect(indicator, "relpop"), "relpop"),
                variable = replace(variable, str_detect(indicator, "milex"), "milex"),
                variable = replace(variable, str_detect(indicator, "milper"), "milper")) %>%
  dplyr::select(-indicator) %>% 
  spread(type, value)

df_cors_new_nochina_v2_long$comp <- factor(df_cors_new_nochina_v2_long$comp,
                                           levels = c("sdp", "gdp"),
                                           labels = c("Share of global SDP",
                                                      "Share of global GDP"))

df_cors_new_nochina_v2_long$variable <- factor(df_cors_new_nochina_v2_long$variable,
                                               levels = c("irst",
                                                          "pec",
                                                          "upop",
                                                          "relpop", #these are our new estimates
                                                          "tpop",
                                                          "milex",
                                                          "milper"),
                                               labels = c("Share of global iron and steel production",
                                                          "Share of global primary energy consumption",
                                                          "Share of global urban population",
                                                          "Share of global population",
                                                          "Share of global population ",
                                                          "Share of global military expenditure",
                                                          "Share of global military personnel"))



ggplot(subset(df_cors_new_nochina_v2_long, !is.na(estimate) & variable %in% c("Share of global iron and steel production",
                                                                              "Share of global primary energy consumption")),
       aes(x = year, y = estimate, color = comp, shape = comp)) +
  geom_linerange(aes(ymin = cilo, ymax = cihi), size = 0.2, alpha = 0.3, show.legend = F) +
  geom_point(size = 0.9) +
  theme_bw() +
  labs(x = "Year",
       y = "Pearson's product-moment correlation coefficient",
       title = "Correlation between CINC components and global shares of SDP vs. GDP",
       subtitle = "China is excluded from CINC variables, but included in global SDP and global GDP") +
  scale_color_manual(values = c("darkorange", #orange
                                "gray50"), #green
                     name = "Share of global economic power-resources") +
  scale_shape_discrete(name = "Share of global economic power-resources") +
  theme(legend.position = "top",
        panel.grid.minor = element_blank(),
        strip.text = element_text(size = 10),
        legend.text = element_text(size = 10)) +
  guides(color = guide_legend(override.aes = list(size = 5, alpha = 1))) +
  coord_cartesian(xlim = c(1816,2012),
                  ylim = c(0,1)) +
  facet_wrap(~variable, ncol = 1) +
  scale_x_continuous(breaks = seq(1800, 2000, 25))

# Figure 7

#gwlist <- readRDS("./data/gwlist.rds")

sub_years <- master %>%
  dplyr::select(ccode, 
                year,
                WorldBank_gdp_2011_ppp_estimate_ln,
                ln_gdp_surplus1095_truncatedone) %>%
  left_join(gwlist, by = c("ccode" = "statenum")) %>%
  dplyr::mutate(region = ifelse(ccode < 200, "Americas", NA),
                region = replace(region, ccode >= 200 & ccode < 400, "Europe"),
                region = replace(region, ccode >= 400 & ccode < 600, "Sub-Saharan Africa"),
                region = replace(region, ccode >= 600 & ccode < 700, "Middle East and N. Africa"),
                region = replace(region, ccode >= 700 & ccode < 900, "Asia"),
                region = replace(region, ccode >= 900, "Australia and Ozeania")) %>%
  filter(ccode < 900) %>%
  filter(year %in% c(1816, 1910, 1935, 1965, 1990, 2010))

sub_years_select <- sub_years %>%
  mutate(stateabb_select = stateabb,
         stateabb_select = replace(stateabb_select, !(stateabb %in% c("USA",
                                                                      "UKG",
                                                                      "CHN",
                                                                      "JPN",
                                                                      "RUS",
                                                                      "GMY",
                                                                      "IND",
                                                                      "RWA")), NA))
ggplot(sub_years_select,
       aes(x = WorldBank_gdp_2011_ppp_estimate_ln, 
           y = ln_gdp_surplus1095_truncatedone, 
           color = region)) +
  geom_point(alpha = 0.8) +
  coord_cartesian(xlim = c(0,31),
                  ylim = c(0,31)) +
  facet_wrap(~year) +
  theme_bw() + 
  labs(title = "GDP versus SDP",
       x = "ln GDP", 
       y = "ln SDP") +
  theme(legend.position = "top") +
  scale_color_brewer(palette = "Set1",
                     name = "Region") +
  geom_text_repel(aes(label = stateabb_select), 
                  box.padding = unit(0.8, "lines"),
                  show.legend = F) +
  guides(colour = guide_legend(override.aes = list(alpha = 1)))

# Figure 8

sub_surplus_scatter <- master %>%
  mutate(gdppc = WorldBank_gdp_2011_ppp_estimate/WorldBank_pop_estimate) %>%
  dplyr::select(ccode, 
                year,
                gdppc,
                surplus1095 = gdp_surplus1095,
                surplus730 = gdp_surplus730,
                surplus365 = gdp_surplus365,
                surplus1095_truncone = gdp_surplus1095_truncatedone,
                surplus730_truncone = gdp_surplus730_truncatedone,
                surplus365_truncone = gdp_surplus365_truncatedone) %>% 
  left_join(gwlist, by = c("ccode" = "statenum")) %>%
  dplyr::mutate(region = ifelse(ccode < 200, "Americas", NA),
                region = replace(region, ccode >= 200 & ccode < 400, "Europe"),
                region = replace(region, ccode >= 400 & ccode < 600, "Sub-Saharan Africa"),
                region = replace(region, ccode >= 600 & ccode < 700, "Middle East and N. Africa"),
                region = replace(region, ccode >= 700 & ccode < 900, "Asia"),
                region = replace(region, ccode >= 900, "Australia and Ozeania")) %>%
  filter(ccode < 900) %>%
  mutate(stateabb_select = stateabb,
         stateabb_select = replace(stateabb_select, !(stateabb_select %in% c("USA",
                                                                             "UKG",
                                                                             "CHN",
                                                                             "JPN",
                                                                             "RUS",
                                                                             "GMY",
                                                                             "IND",
                                                                             "RWA") & year == 1990), NA))

sub_surplus_scatter_long <- sub_surplus_scatter %>%
  gather(indicator, value, surplus1095:surplus365)


ggplot(subset(sub_surplus_scatter_long, indicator == "surplus1095"),
       aes(x = gdppc, y = value, color = region)) +
  geom_point(alpha = 0.1, position = position_jitter()) +
  geom_smooth() +
  labs(title = "Relationship between SDP and per capita GDP",
       subtitle = "The y-axis is truncated to 10e+12 for presentation",
       y = "SDP",
       x = "GDP per capita") +
  theme_bw() + 
  theme(legend.position = "top") +
  #scale_x_log10() +
  coord_cartesian(ylim = c(0, 10e+12)) +
  geom_vline(xintercept = 1095) +
  scale_color_brewer(palette = "Set1",
                     name = "Region") +
  geom_text_repel(aes(label = stateabb_select), 
                  box.padding = unit(0.8, "lines"),
                  show.legend = F)

# Figure 9

#distance
#distmat_polity <- readRDS("./data/dist_mat_politysub.rds")

maxdist <- max(distmat_polity[,3:ncol(distmat_polity)])


distmat_polity_sub <- distmat_polity %>%
  gather(ccodeb, dist, -ccode, - year) %>%
  mutate(logdist = log(dist),
         logdist = replace(logdist, dist == 0, 0),
         invdist = 1/dist,
         invlog = 1/logdist,
         invlog = replace(invlog, dist == 0, 0),
         linear1 = (max(maxdist)-dist)/(maxdist),
         label = ifelse(ccodeb == 200, "Great Britain", NA),
         label = replace(label, ccodeb == 365, "Russia"),
         label = replace(label, ccodeb == 2, "USA"),
         label = replace(label, ccodeb == 750, "India"),
         label = replace(label, ccodeb == 20, "Canada"),
         label = replace(label, ccodeb == 740, "Japan"),
         label = replace(label, ccodeb == 731, "North Korea"),
         label = replace(label, ccodeb == 732, "South Korea"),
         label = replace(label, ccodeb == 710, "China"),
         label = replace(label, ccodeb == 220, "France"),
         label = replace(label, ccodeb == 255, "Germany"),
         label = replace(label, ccodeb == 265, "Germany"),
         printlabel = ifelse(!is.na(label), 1, 0)) %>%
  filter(ccode %in% c(2, 710, 220),
         dist != 0) %>%
  dplyr::mutate(title = ifelse(ccode == 2, "USA", NA),
                title = replace(title, ccode == 710, "China"))

# population
#popmat_polity <- readRDS("./data/pop_mat_politysub.rds")
popmat_polity_sub <- popmat_polity %>%
  filter(ccode %in% c(2, 710)) %>%
  gather(ccodeb, pop, -ccode, - year)

# polity 
#indicatormat_polity <- readRDS("./data/indicator_mat_politysub.rds")

indicatormat_polity_sub <- indicatormat_polity %>%
  filter(ccode %in% c(2, 710)) %>%
  gather(ccodeb, indicator, -ccode, - year)

# GDP and merging
#gdpdat <- readRDS("./data/gdp_mat_politysub.rds")
gdpmat_polity <- gdpdat

t2 <- as.numeric(names(gdpmat_polity)[3:length(names(gdpmat_polity))])

gdpi <- c()

for(i in 1:nrow(gdpmat_polity)){
  t <- gdpmat_polity$ccode[i]
  gdpi[i] <- gdpmat_polity[i, which(t2 == t) + 2]
  gdpmat_polity[i, which(t2 == t)+2] <- NA
}

gdpi[gdpi == 0] <- 1

gdp_ratio <- cbind(gdpmat_polity[,1:2], gdpmat_polity[,3:ncol(gdpmat_polity)]/(gdpi + gdpmat_polity[,3:ncol(gdpmat_polity)]))

gdp_long <- gdp_ratio %>%
  filter(ccode %in% c(2, 710, 220)) %>%
  gather(ccodeb, gdp, -ccode, - year)

# Surplus version
surplus1095mat_polity <- cbind(gdpdat[,1:2], 
                               gdpdat[,3:ncol(gdpdat)] - (1095*popmat_polity[,3:ncol(popmat_polity)]))


surplus1095i <- c()

for(i in 1:nrow(surplus1095mat_polity)){
  t <- surplus1095mat_polity$ccode[i]
  surplus1095i[i] <- surplus1095mat_polity[i, which(t2 == t) + 2]
  surplus1095mat_polity[i, which(t2 == t)+2] <- NA
}

surplus1095i[surplus1095i == 0] <- 1

surplus1095_ratio <- cbind(surplus1095mat_polity[,1:2], surplus1095mat_polity[,3:ncol(surplus1095mat_polity)]/(surplus1095i + surplus1095mat_polity[,3:ncol(surplus1095mat_polity)]))


surplus1095_long <- surplus1095_ratio %>%
  filter(ccode %in% c(2, 710, 220)) %>%
  gather(ccodeb, surplus1095, -ccode, - year)

merged_polity_sub <- full_join(gdp_long, surplus1095_long) %>%
  left_join(popmat_polity_sub, by = c("ccode", "ccodeb", "year")) %>%
  left_join(distmat_polity_sub, by = c("ccode", "ccodeb", "year")) %>%
  left_join(indicatormat_polity_sub, by = c("ccode", "ccodeb", "year")) %>%
  group_by(ccode, year) %>%
  mutate(sumgdp = sum(gdp, na.rm = T),
         sumsurplus = sum(surplus1095, na.rm = T)) %>%
  ungroup() %>%
  mutate(threat_gdp_invlog = gdp*invlog,
         threat_surplus1095_invlog = surplus1095*invlog) %>%
  mutate(ccodeb = as.numeric(ccodeb)) %>%
  left_join(gwlist, by = c("ccodeb" = "statenum"))

new <- merged_polity_sub %>%
  dplyr::select(ccode, 
                ccodeb, 
                statenme,
                year,
                everything(),
                -title, -printlabel, -label, -sumgdp, -sumsurplus) %>%
  gather(measure, value, gdp:threat_surplus1095_invlog) %>%
  filter(!is.na(value)) %>%
  mutate(measure = as.factor(measure))

test <- new %>%
  filter(ccode != ccodeb,
         ccode %in% c(2, 220, 710),
         year == 1996,
         measure %in% c("dist", "linear1", "invlog")) %>% 
  spread(measure, value) %>%
  dplyr::mutate(group = ifelse(dist < 1000, "group1", NA),
                group = replace(group, dist >= 1000 & dist < 2000, "group2"),
                group = replace(group, dist >= 2000, "group3")) %>%
  gather(measure, value, dist:linear1)

test$group <- factor(test$group,
                     levels = c("group1",
                                "group2",
                                "group3"),
                     labels = c("Below 1000km",
                                "Between 1000km and 2000km",
                                "Above 2000km"))


test$ccode <- factor(test$ccode,
                     levels = c(2, 220, 710),
                     labels = c("United States of America", "France", "China"))

p_dist <- ggplot(subset(test, measure == "dist"),
                 aes(x = value, fill = factor(group))) +
  geom_density() +
  facet_wrap(~ ccode, scales = "free_y") +
  theme_bw() +
  theme(panel.grid = element_blank()) +
  labs(title = "Density of distance") +
  scale_fill_brewer(type = "div", name = "Distance") 

p_linear1 <- ggplot(subset(test, measure == "linear1"),
                    aes(x = value, fill = factor(group))) +
  geom_density() +
  facet_wrap(~ ccode, scales = "free_y") +
  theme_bw() +
  theme(panel.grid = element_blank()) +
  labs(title = "Density of linearly decreasing distance measure") +
  scale_fill_brewer(type = "div", name = "Distance")


p_invlog <- ggplot(subset(test, measure == "invlog"),
                   aes(x = value, fill = factor(group))) +
  geom_density() +
  facet_wrap(~ ccode, scales = "free_y") +
  theme_bw() +
  theme(panel.grid = element_blank()) +
  labs(title = "Density of inverse of logged distance") +
  scale_fill_brewer(type = "div", name = "Distance")

p_all <- grid.arrange(p_dist, p_linear1, p_invlog, ncol = 1)

# Figure 10

new <- merged_polity_sub %>%
  dplyr::select(ccode, 
                ccodeb, 
                statenme,
                year,
                everything(),
                -title, -printlabel, -label, -sumgdp, -sumsurplus) %>%
  gather(measure, value, gdp:threat_surplus1095_invlog) %>%
  filter(!is.na(value)) %>%
  mutate(measure = as.factor(measure))

table(new$measure)

tab_top10_gdp_surplus <- new %>%
  filter(ccode != ccodeb,
         ccode == 2,
         year == 1935,
         measure %in% c("gdp", 
                        "surplus1095",
                        "indicator")) %>%
  spread(measure, value) %>%
  filter(!is.na(indicator)) %>%
  gather(measure, value, gdp, surplus1095) %>%
  group_by(measure) %>%
  arrange(desc(value)) %>%
  slice(1:10) %>%
  mutate(topnorder = seq(1, 10))

tab_top10_gdp_surplus$measure <- factor(tab_top10_gdp_surplus$measure,
                                        levels = c("gdp", 
                                                   "surplus1095"),
                                        labels = c("GDP ratio",
                                                   "SDP ratio"))

p_top10_gdp_surplus <- ggplot(tab_top10_gdp_surplus,
                              aes(x = reorder(topnorder, value), y = value, shape = factor(indicator))) +
  geom_point() +
  geom_text(aes(label = statenme), hjust = -0.1, size = 2.5) + 
  facet_wrap(~ measure) +
  theme_bw() +
  labs(x = "",
       y = "Relative power-resources") +
  scale_shape_manual(values = c(1, 16)) +
  theme(legend.position = "none",
        panel.grid.minor = element_blank()) +
  coord_flip(ylim = c(0,0.55))


tab_top10_invlog <- new %>%
  filter(ccode != ccodeb,
         ccode == 2,
         year == 1935,
         measure %in% c("invlog",
                        "indicator")) %>%
  spread(measure, value) %>%
  filter(!is.na(indicator)) %>%
  gather(measure, value, invlog) %>%
  group_by(measure) %>%
  arrange(desc(value)) %>%
  slice(1:10) %>%
  mutate(topnorder = seq(1, 10)) %>%
  ungroup()

tab_top10_invlog2 <- tab_top10_invlog %>%
  dplyr:: mutate(measure = as.character(measure),
                 measure = replace(measure, measure == "invlog", "invlog2")) %>%
  bind_rows(tab_top10_invlog)

tab_top10_invlog2$measure <- factor(tab_top10_invlog2$measure,
                                    levels = c("invlog", "invlog2"),
                                    labels = c("Inverse logged distance", "Inverse logged distance "))


p_top10_invlog <- ggplot(tab_top10_invlog2,
                         aes(x = reorder(topnorder, value), y = value, shape = factor(indicator))) +
  geom_point() +
  geom_text(aes(label = statenme), hjust = -0.1, size = 2.5) + 
  facet_wrap(~ measure) +
  theme_bw() +
  labs(x = "",
       y = "Inverse logged distance") +
  scale_shape_manual(values = c(1, 16)) +
  theme(legend.position = "none",
        panel.grid.minor = element_blank()) +
  coord_flip(ylim = c(0, .3))

tab_top10_threatgdp_threatsurplus <- new %>%
  filter(ccode != ccodeb,
         ccode == 2,
         year == 1935,
         measure %in% c("threat_gdp_invlog", 
                        "threat_surplus1095_invlog",
                        "indicator")) %>%
  spread(measure, value) %>%
  filter(!is.na(indicator)) %>%
  gather(measure, value, threat_gdp_invlog:threat_surplus1095_invlog) %>%
  group_by(measure) %>%
  arrange(desc(value)) %>%
  slice(1:10) %>%
  mutate(topnorder = seq(1, 10))

tab_top10_threatgdp_threatsurplus$measure <- factor(tab_top10_threatgdp_threatsurplus$measure,
                                                    levels = c("threat_gdp_invlog", 
                                                               "threat_surplus1095_invlog"),
                                                    labels = c("Distance-weighted GDP ratio",
                                                               "Distance-weighted SDP ratio"))

p_top10_threatgdp_threatsurplus <- ggplot(tab_top10_threatgdp_threatsurplus,
                                          aes(x = reorder(topnorder, value), y = value, shape = factor(indicator))) +
  geom_point() +
  geom_text(aes(label = statenme), hjust = -0.1, size = 2.5) + 
  facet_wrap(~ measure) +
  theme_bw() +
  labs(x = "",
       y = "Distance weighted relative power-resources") +
  scale_shape_manual(values = c(1, 16)) +
  theme(legend.position = "none",
        panel.grid.minor = element_blank()) +
  coord_flip(ylim = c(0, 0.2))

tab_top10_threatgdp_threatsurplus_polity <- new %>%
  filter(ccode != ccodeb,
         ccode == 2,
         year == 1935,
         measure %in% c("threat_gdp_invlog", 
                        "threat_surplus1095_invlog",
                        "indicator")) %>%
  spread(measure, value) %>%
  filter(!is.na(indicator)) %>%
  filter(indicator == 1) %>%
  gather(measure, value, threat_gdp_invlog, threat_surplus1095_invlog) %>%
  group_by(measure) %>%
  arrange(desc(value)) %>%
  slice(1:10) %>%
  mutate(topnorder = seq(1, 10))

tab_top10_threatgdp_threatsurplus_polity$measure <- factor(tab_top10_threatgdp_threatsurplus_polity$measure,
                                                           levels = c("threat_gdp_invlog", 
                                                                      "threat_surplus1095_invlog"),
                                                           labels = c("Distance-weighted GDP ratio",
                                                                      "Distance-weighted SDP ratio"))

p_top10_threatgdp_threatsurplus_polity <- ggplot(tab_top10_threatgdp_threatsurplus_polity,
                                                 aes(x = reorder(topnorder, value), y = value, shape = factor(indicator))) +
  geom_point() +
  geom_text(aes(label = statenme), hjust = -0.1, size = 2.5) + 
  facet_wrap(~ measure) +
  theme_bw() +
  labs(x = "",
       y = "Distance weighted relative power-resources for non-democratic dyads") +
  scale_shape_manual(values = c(16)) +
  theme(legend.position = "none",
        panel.grid.minor = element_blank()) +
  coord_flip(ylim = c(0, 0.2))


# Putting it all into one plot
t_economicpower <- paste("Step 1: Economic power-resources",
                         "",
                         "The graph shows the top 10 most economically powerful",
                         "states (excluding the US) as measured by their ratio",
                         "of a) the joint GDP between the state and the US and", 
                         "b) the joint SDP in 1935. More generally, the measure",
                         "is computed as the power-resources of state j divided",
                         "by the sum of the power-resources of states j and i (here the US).",
                         "SDP is computed by subtracting from GDP the amount",
                         "of economic income a country needs to meet the subsistence",
                         "needs of the population at a $3 per diem value.",
                         sep = "\n")
tg_economicpower <- text_grob(t_economicpower, just = "center", size = 8)
t_invlog <- paste("Step 2: Geographic proximity",
                  "",
                  "The graph shows the top 10 most proximate states to the US as", 
                  "measured by the inverse of the logged distance between",
                  "capital cities. Due to the Loss of Strength Gradient, closer",
                  "states are more threatening than states that are farther away.",
                  sep = "\n")
tg_invlog <- text_grob(t_invlog, just = "center", size = 8)
t_combined <- paste("Step 3: Combining power-resources and proximity",
                    "",
                    "The graph shows the top 10 states ranking highest",
                    "in terms of their distance-weighted proportion of ",
                    "of a) joint GDP and b) joint SDP from the perspective",
                    "of the US in 1935. The measure is computed",
                    "by multiplying each countries' value on relative",
                    "power-resources by the distance measure.",
                    "Without accounting for preference compatibility, the UK",
                    "would be the most threatening country toward the US based on SDP.",
                    "China would be among the top five most threatening states",
                    "if power-resources are measured via GDP without taking",
                    "the subsistence needs of the population into account.", sep = "\n")
tg_combined <- text_grob(t_combined, just = "center", size = 8)
t_combinedpolity <- paste("Step 4: Accounting for preference compatibility",
                          "",
                          "The graph shows the top 10 contributors to the",
                          "potential threat within the US' strategic environment",
                          "after accounting for preference compatibility. We drop",
                          "those countries that are coded as jointly democratic",
                          "with the US in 1935 based on the Polity2 score (min 6).",
                          "Using the binary Polity measure, only the distance-",
                          "weighted relative power-resources of non-jointly",
                          "democratic dyads will contribute toward the total",
                          "potential threat score of the US' strategic environment.",
                          sep = "\n")
tg_combinedpolity <- text_grob(t_combinedpolity, just = "center", size = 8)


c_joint <- grid.arrange(p_top10_gdp_surplus, 
                        tg_economicpower,
                        p_top10_invlog,
                        tg_invlog,
                        p_top10_threatgdp_threatsurplus,
                        tg_combined,
                        p_top10_threatgdp_threatsurplus_polity,
                        tg_combinedpolity,
                        ncol = 2,
                        widths = 3:2,
                        top = "Construction of the potential threat measure for the US in 1935")

# Figure 11

### Setting up groups of variables for interest compatibility

# 1) Main competition variable
vars_comp <- c("comp_surplus1095_polity_invlog_nodenom_all",
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
               "comp_surplus1095_nopreference_invlog_nodenom_all")

# Raw averages of interest compatibility measures
vars_rawav <- c("polity_rawav",
                "boix_rawav",
                "uds_rawav",
                "riv_rawav",
                "defense_rawav",
                "sscores_rawav",
                "absidealdiff_rawav",
                "igo_rawav",
                "diplo_rawav",
                "bitrade_rawav",
                "pecpc_rawav",
                "atopkappa_rawav",
                "atopbin_rawav")

## Labels
vars_labels <- c("PT Joint democracy (Polity)",
                 "PT Joint democracy (Boix et al.)",
                 "PT Joint democracy (UDS)",
                 "PT Rivalry",
                 "PT Defense pacts",
                 "PT S-scores",
                 "PT Absolute ideal difference",
                 "PT Joint IGO membership",
                 "PT Diplomatic exchange",
                 "PT Bilateral trade",
                 "PT Energy consumption",
                 "PT Alliances (continuous)",
                 "PT Alliances (binary)",
                 "PT No preference variable")

sub_corrplot <- master %>%
  dplyr::select(one_of(vars_comp))
names(sub_corrplot) <- vars_labels

res2 <- Hmisc::rcorr(as.matrix(sub_corrplot))

#png(height=1800, width=1800, pointsize=25, file="figA11.png")
col <- colorRampPalette(c("#6666ff","#ff6666"))
corrplot(res2$r, 
         type = "lower", 
         method = "color",
         addCoef.col = "black",
         p.mat = res2$P, 
         sig.level = 0.05,
         tl.col = "black",
         insig = "blank",
         tl.srt = 45,
         col = col(200),
         diag = T,
         tl.cex = 1,
         tl.offset = .5)
#dev.off()

# Figure 12

### A) Competition
comp_long <- master %>%
  dplyr::select(ccode, year, one_of(vars_comp), styear) %>%
  gather(indicator, value, -ccode, -year, -styear) %>%
  dplyr::mutate(entrybefore1900 = ifelse(styear < 1900, "yes", "no"),
                entrybefore1945 = ifelse(styear < 1945, "yes", "no"),
                entry = ifelse(styear < 1900, 1, NA),
                entry = replace(entry, styear >= 1900 & styear <= 1945, 2),
                entry = replace(entry, styear > 1945, 3),
                entrynolabel = entry) %>%
  filter(year >= styear)

comp_long$entry <- factor(comp_long$entry,
                          levels = c(1, 2, 3),
                          labels = c("Pre 1900",
                                     "Between 1900 and 1945",
                                     "Post 1945"))

comp_long$indicator <- factor(comp_long$indicator,
                              levels = vars_comp,
                              labels = vars_labels)

ggplot(comp_long, 
       aes(x = year, 
           y = value)) +
  # geom_line(aes(group = ccode),
  #           size = 0.1,
  #           alpha = 0.5) +
  geom_point(size = 0.1, alpha = 0.05) +
  geom_smooth(method = "loess", se = F) +
  theme_light() +
  facet_wrap(~ indicator, scales = "free_y") +
  labs(x = "Year",
       y = "Potential threat",
       title = "Evolution of alternative potential threat variables over time") +
  coord_cartesian(xlim = c(1816, 2012)) +
  # scale_color_manual(name = "Entry into state system",
  #                    values = c("#b8860b", "#0bb886","#860bb8")) +
  theme(legend.position = "top",
        panel.grid.minor = element_blank())

# Figure 13

ggplot(comp_long, 
       aes(x = year, 
           y = value, 
           color = factor(entry))) +
  # geom_line(aes(group = ccode),
  #           size = 0.1,
  #           alpha = 0.5) +
  geom_point(size = 0.1, alpha = 0.05) +
  geom_smooth(method = "loess", se = F) +
  theme_light() +
  facet_wrap(~ indicator, scales = "free_y") +
  labs(x = "Year",
       y = "Potential threat",
       title = "Evolution of alternative potential threat variables over time") +
  coord_cartesian(xlim = c(1816, 2012)) +
  scale_color_manual(name = "Entry into state system",
                     values = c("#b8860b", "#0bb886","#860bb8")) +
  theme(legend.position = "top",
        panel.grid.minor = element_blank())

# Figure 14 
master_new <- master %>%
  dplyr::mutate(surplus1095pc = (surplus1095+0.00001)/WorldBank_pop_estimate)

# GDP quartiles 
years <- seq(1816,2012)

master_ls <- list()
for(y in 1:length(years)){
  master_sub <- master_new %>%
    filter(year == years[y],
           !is.na(surplus1095pc)) 
  breaks = quantile(master_sub$surplus1095pc)
  
  possibleError <- tryCatch({master_sub$quartile = cut(master_sub$surplus1095pc, 
                                                       breaks = breaks,
                                                       labels = c("q1", "q2", "q3", "q4"))},
                            error = function(e) e)
  
  if(!inherits(possibleError, "error")){
    #REAL WORK
    master_sub$quartile = cut(master_sub$surplus1095pc, 
                              breaks = breaks,
                              labels = c("First", "Second", "Third", "Fourth"))
  } else {
    master_sub$quartile = "notpossible"
  }
  
  master_ls[[y]] <- master_sub
}


master_new2 <- do.call(rbind, master_ls)

comp_new_long <- master_new2 %>%
  dplyr::select(ccode, year, one_of(vars_comp), quartile) %>%
  gather(indicator, value, - ccode, -year, - quartile) %>%
  filter(!is.na(quartile), !is.na(value))

comp_new_long$indicator <- factor(comp_new_long$indicator,
                                  levels = vars_comp,
                                  labels = vars_labels)

ggplot(comp_new_long, aes(x = year, y = value, color = factor(quartile))) +
  geom_point(size = 0.2, alpha = 0.05) +
  geom_smooth(method = "loess", se = F) +
  theme_light() +
  facet_wrap(~ indicator, scales = "free_y") +
  labs(x = "Year",
       y = "Potential threat",
       title = "Evolution of alternative potential threat variables over time by quartile of SDP") +
  coord_cartesian(xlim = c(1816, 2012)) +
  scale_color_brewer(name = "Quartile of per capita SDP (3USD)", type = "div") +
  theme(panel.grid.minor = element_blank(), legend.position = "top")

# Figure 15

# Loading developer version of cshapes package (uses sf to read data)
# devtools::install_github("thereseanders/cshapes")
library(cshapes)

vars_comp <- c("comp_surplus1095_polity_invlog_nodenom_all",
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
               "comp_surplus1095_nopreference_invlog_nodenom_all")

# Scaling the data
vars_comp_scaled <- c()
for(i in 1:length(vars_comp)){
  vars_comp_scaled[i] <- paste("scaled", vars_comp[i], sep = "_")
  master[[vars_comp_scaled[i]]] <- c(scale(master[,vars_comp[i]]))
}

vars_comp_scaled <- c("scaled_comp_surplus1095_polity_invlog_nodenom_all",
                      "scaled_comp_surplus1095_boix_invlog_nodenom_all",
                      "scaled_comp_surplus1095_uds_invlog_nodenom_all",
                      "scaled_comp_surplus1095_riv_invlog_nodenom_all",
                      "scaled_comp_surplus1095_defense_invlog_nodenom_all",
                      "scaled_comp_surplus1095_sscores_invlog_nodenom_all",
                      "scaled_comp_surplus1095_absidealdiff_invlog_nodenom_all",
                      "scaled_comp_surplus1095_igo_invlog_nodenom_all",
                      "scaled_comp_surplus1095_diplo_invlog_nodenom_all",
                      "scaled_comp_surplus1095_bitrade_invlog_nodenom_all",
                      "scaled_comp_surplus1095_pecpc_invlog_nodenom_all",
                      "scaled_comp_surplus1095_atopkappa_invlog_nodenom_all",
                      "scaled_comp_surplus1095_atopbin_invlog_nodenom_all",
                      "scaled_comp_surplus1095_nopreference_invlog_nodenom_all")

#########################
# Using c-shapes library
#########################

## Reading 1965 data
cshp1965 <- cshp(date = as.Date("1965-1-1"), useGW = T, simplify = T, tolerance = 0.1) %>%
  mutate(year = 1965)
class(cshp1965)

## Reading 2000 data
cshp2000 <- cshp(date = as.Date("2000-1-1"), useGW = T, simplify = T, tolerance = 0.1) %>%
  mutate(year = 2000)

# Binding 1965 and 2000 maps together
world_map <- rbind(cshp1965, cshp2000) %>%
  dplyr::mutate(ccode = GWCODE)
class(world_map)

#world_map$ccode <- as.numeric(world_map$ccode)
#subsetting master
merged <- master %>%
  filter(year %in% c(1965, 2000)) %>%
  dplyr::select(ccode, year,
                one_of(vars_comp),
                one_of(vars_comp_scaled)) %>%
  pivot_longer(-c(year, ccode), 
               names_to = "indicator", values_to = "value") %>%
  dplyr::mutate(indicator = str_replace_all(indicator, "comp_", "")) %>%
  dplyr::mutate(indicator = str_replace_all(indicator, "_invlog_nodenom_all", "")) %>%
  mutate(scaled = ifelse(str_detect(indicator, "scaled"), 1, 0),
         indicator = str_replace_all(indicator, "scaled_", "")) %>%
  separate(indicator, c("version", "indicator"), sep = "_") %>%
  left_join(world_map) %>%
  st_as_sf()

varlabels_comp <- c("Joint democracy\n(Polity)",
                    "Joint democracy\n(Boix et al.)",
                    "Joint democracy\n(UDS)",
                    "Rivalry",
                    "Defense\npacts",
                    "S-dcores",
                    "Absolut ideal\ndifference",
                    "Joint IGO\nmembership",
                    "Diplomatic\nexchange",
                    "Bilateral\ntrade",
                    "Energy consumption",
                    "Alliances (continuous)",
                    "Alliances (binary)",
                    "No interest variable")

merged$indicatornames <- factor(merged$indicator,
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
                                labels = varlabels_comp)

######################################
#
# Individual maps for each indicator
#
#
######################################

##### Color Scheme
# getting a color scheme
pals <- c("YlGnBu",#set1 #check
          "BuPu", #check
          "OrRd", #check
          "GnBu",#set2 #check
          "PuRd", #check
          "YlOrBr", #check
          "PuBu",#set3 #check
          "RdPu", #check
          "YlGn", #check
          "BuGn",#set4
          "YlOrRd",
          "RdYlGn",
          "RdYlBu",
          "PuBuGn")

# Polity
merged_polity <- merged %>%
  filter(indicator == "polity")

p_polity <- ggplot(subset(merged_polity, scaled == 0)) +
  geom_sf(aes(fill = value),
          color = "black", size = 0.02) +
  scale_fill_gradientn(colors = brewer.pal(3,pals[1]),
                       na.value = "lightgrey",
                       name = "ln potential\nthreat",
                       trans = "log") +
  theme_minimal() +
  theme(axis.line=element_blank(),
        axis.text.x=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks=element_blank(),
        axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        panel.background=element_blank(),
        panel.border=element_blank(),
        panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),
        plot.background=element_blank(),
        legend.position = "right",
        legend.key.width = unit(0.5,"cm"),
        strip.text = element_text(size=11, face="bold")) +
  facet_wrap(~ year, ncol = 2) +
  labs(title = "Joint democracy (Polity)")

# Boix
merged_boix <- merged %>%
  filter(indicator == "boix")

p_boix <- ggplot(subset(merged_boix, scaled == 0)) +
  geom_sf(aes(fill = value),
          color = "black", size = 0.02) +
  scale_fill_gradientn(colors = brewer.pal(3,pals[2]),
                       na.value = "lightgrey",
                       name = "ln potential\nthreat",
                       trans = "log") +
  theme_minimal() +
  theme(axis.line=element_blank(),
        axis.text.x=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks=element_blank(),
        axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        panel.background=element_blank(),
        panel.border=element_blank(),
        panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),
        plot.background=element_blank(),
        legend.position = "right",
        legend.key.width = unit(0.5,"cm"),
        strip.text = element_text(size=11, face="bold")) +
  facet_wrap(~ year, ncol = 2) +
  labs(title = "Joint democracy (Boix et al.)")


# UDS
merged_uds <- merged %>%
  filter(indicator == "uds")

p_uds <- ggplot(subset(merged_uds, scaled == 0)) +
  geom_sf(aes(fill = value),
          color = "black", size = 0.02) +
  scale_fill_gradientn(colors = brewer.pal(3,pals[3]),
                       na.value = "lightgrey",
                       name = "ln potential\nthreat",
                       trans = "log") +
  theme_minimal() +
  theme(axis.line=element_blank(),
        axis.text.x=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks=element_blank(),
        axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        panel.background=element_blank(),
        panel.border=element_blank(),
        panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),
        plot.background=element_blank(),
        legend.position = "right",
        legend.key.width = unit(0.5,"cm"),
        strip.text = element_text(size=11, face="bold")) +
  facet_wrap(~ year, ncol = 2) +
  labs(title = "Joint democracy (UDS)")

p_dem <- arrangeGrob(p_polity,
                     p_boix,
                     p_uds,
                     ncol = 1)

# Figure 16

cytest_polity <- master %>%
  dplyr::select(ccode, 
                year,
                scaled_comp_surplus1095_polity_invlog_nodenom_all)


cytest_boix <- master %>%
  dplyr::select(ccode, 
                year,
                scaled_comp_surplus1095_boix_invlog_nodenom_all)

cytest_uds <- master %>%
  dplyr::select(ccode, 
                year,
                scaled_comp_surplus1095_uds_invlog_nodenom_all)

cytest <- master %>%
  dplyr::select(ccode, 
                year,
                one_of(vars_comp_scaled)) %>%
  gather(indicator, value, -ccode, -year) %>%
  filter(!is.na(value)) %>%
  group_by(ccode, year) %>%
  dplyr::summarise(comp_av = mean(value, rm.na = T),
                   comp_sd = sd(value)) %>%
  ungroup() %>%
  dplyr::filter(!is.na(comp_av), !is.na(comp_sd)) %>%
  dplyr::mutate(cilo = comp_av - (1.96*comp_sd),
                cihi = comp_av + (1.96*comp_sd)) %>%
  left_join(cytest_polity, by = c("year", "ccode")) %>%
  left_join(cytest_boix, by = c("year", "ccode")) %>%
  left_join(cytest_uds, by = c("year", "ccode"))

topcomp <- cytest %>%
  filter(year >= 1816) %>%
  arrange(year, desc(scaled_comp_surplus1095_polity_invlog_nodenom_all)) %>%
  group_by(year) %>%
  slice(1:20) %>%
  dplyr::mutate(topn = seq(1:20)) %>%
  left_join(gwlist, by = c("ccode" = "statenum")) %>%
  gather(indicator, 
         value, 
         scaled_comp_surplus1095_polity_invlog_nodenom_all, 
         scaled_comp_surplus1095_boix_invlog_nodenom_all, 
         scaled_comp_surplus1095_uds_invlog_nodenom_all,
         comp_av) %>%
  filter(!is.na(value)) %>%
  dplyr::mutate(statenme = replace(statenme, ccode == 403, "Sao Tome and Principe"),
                statenme = replace(statenme, ccode == 57, "Saint Vincent and the Grenadines")) %>%
  dplyr::mutate(cilo = replace(cilo, indicator %in% c("comp_av",
                                                      "scaled_comp_surplus1095_boix_invlog_nodenom_all",
                                                      "scaled_comp_surplus1095_uds_invlog_nodenom_all"), NA),
                cihi = replace(cihi, indicator %in% c("comp_av",
                                                      "scaled_comp_surplus1095_boix_invlog_nodenom_all",
                                                      "scaled_comp_surplus1095_uds_invlog_nodenom_all"), NA),
                statenme = replace(statenme, indicator %in% c("comp_av",
                                                              "scaled_comp_surplus1095_boix_invlog_nodenom_all",
                                                              "scaled_comp_surplus1095_uds_invlog_nodenom_all"), NA)) %>%
  filter(indicator != "comp_av")

topcomp$indicator <- factor(topcomp$indicator,
                            levels = c("scaled_comp_surplus1095_polity_invlog_nodenom_all",
                                       "scaled_comp_surplus1095_boix_invlog_nodenom_all",
                                       "scaled_comp_surplus1095_uds_invlog_nodenom_all"),
                            labels = c("Polity potential threat measure",
                                       "Boix et al. potential threat measure",
                                       "UDS potential threat measure"))

ggplot(subset(topcomp, year %in% c(1816, 1910, 1935, 1965, 1990, 2010)),
       aes(x = topn, y = value, color = indicator, shape = indicator)) +
  geom_hline(yintercept = 0, color = "lightgrey", linetype = "dashed", alpha = 0.8) +
  geom_linerange(aes(ymin = cilo, ymax = cihi), size = .4, color = "black") +
  geom_point(alpha = 0.8) +
  geom_text(aes(label = statenme), vjust = -0.5, size = 4, show.legend = FALSE, color = "black") +
  facet_wrap(~year, nrow = 2) +
  coord_flip(ylim = c(-5, 7)) +
  theme_bw() +
  theme(panel.grid = element_blank(),
        legend.position = "top") +
  scale_x_reverse(breaks = seq(1:20)) +
  labs(title = "Top 20 states facing most threatening geopolitical environment",
       subtitle = "Variables ordered by the potential threat scores using the Polity data",
       x = "",
       y = "Standardized potential threat measure") +
  scale_color_manual(values = c("red", "darkgreen", "darkblue"),
                     name = "") +
  scale_shape_discrete(name = "") +
  guides(color = guide_legend(override.aes = list(size = 3)))

# Figure 17

df_comp <- master %>%
  dplyr::select(ccode,
                year,
                comp_gdp_polity_invlog_nodenom_all,
                comp_surplus365_polity_invlog_nodenom_all,
                comp_surplus730_polity_invlog_nodenom_all,
                comp_surplus1095_polity_invlog_nodenom_all,
                comp_pop_polity_invlog_nodenom_all) %>%
  gather(indicator, value, -year, -ccode, -comp_pop_polity_invlog_nodenom_all) %>%
  mutate(sel = ifelse(year < 1900, "19th", NA),
         sel = replace(sel, year >= 1900, "20th"),
         sel = replace(sel, year >= 2000, "21st"))

df_comp$indicator <- factor(df_comp$indicator,
                            levels = c("comp_surplus1095_polity_invlog_nodenom_all",
                                       "comp_surplus730_polity_invlog_nodenom_all",
                                       "comp_surplus365_polity_invlog_nodenom_all",
                                       "comp_gdp_polity_invlog_nodenom_all"),
                            labels = c("Potential threat SDP ($3 subsistence)",
                                       "Potential threat SDP ($2 subsistence)",
                                       "Potential threat SDP ($1 subsistence)",
                                       "Potential threat GDP"))

ggplot(df_comp,
       aes(x = value, y = comp_pop_polity_invlog_nodenom_all)) +
  geom_point(alpha = 0.1, size = 0.4) +
  facet_wrap(~indicator) +
  geom_smooth() +
  theme_bw() +
  labs(title = "Relationship between potential threat measures",
       subtitle = "Period of observation: 1816-2012. Preference compatibility measured using Polity.",
       x = "Potential threat (economic resources)",
       y = "Potential threat (population)")

# Figure 18

corfun_gdp_pop <- function(df) cor.test(df$gdp, df$pop) %>% broom::tidy()
corfun_gdp_surplus1095 <- function(df) cor.test(df$gdp, df$surplus1095) %>% broom::tidy()
corfun_pop_surplus1095 <- function(df) cor.test(df$pop, df$surplus1095) %>% broom::tidy()

vars_comp_gdp <- c("comp_gdp_polity_invlog_nodenom_all",
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
                   "comp_gdp_nopreference_invlog_nodenom_all")

vars_comp_pop <- c("comp_pop_polity_invlog_nodenom_all",
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
                   "comp_pop_nopreference_invlog_nodenom_all")

pref_names <- c("polity",
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
                "atopbin")

sub_threat <- master %>%
  dplyr::select(ccode,
                year,
                vars_comp,
                one_of(vars_comp_pop),
                one_of(vars_comp_gdp)) %>%
  gather(indicator, value, -ccode, -year) %>%
  separate(indicator, c("trash", "type", "pref", "trash2", "trash3", "trash4")) %>%
  dplyr::select(-contains("trash")) %>%
  filter(!is.na(value)) %>%
  spread(type, value) %>%
  group_by(year, pref) %>%
  dplyr::summarise(`gdp_surplus1095:est` = cor.test(gdp, surplus1095)$estimate,
            `gdp_surplus1095:cilo` = cor.test(gdp, surplus1095)$conf.int[1],
            `gdp_surplus1095:cihi` = cor.test(gdp, surplus1095)$conf.int[2],
            `gdp_pop:est` = cor.test(gdp, pop)$estimate,
            `gdp_pop:cilo` = cor.test(gdp, pop)$conf.int[1],
            `gdp_pop:cihi` = cor.test(gdp, pop)$conf.int[2],
            `pop_surplus1095:est` = cor.test(pop, surplus1095)$estimate,
            `pop_surplus1095:cilo` = cor.test(pop, surplus1095)$conf.int[1],
            `pop_surplus1095:cihi` = cor.test(pop, surplus1095)$conf.int[2]) %>%
  ungroup() %>%
  gather(indicator, value, -year, -pref) %>%
  separate(indicator, c("indicator", "est"), sep = ":") %>%
  spread(est, value)


sub_threat$indicator <- factor(sub_threat$indicator,
                               levels = c("pop_surplus1095",
                                          "gdp_surplus1095",
                                          "gdp_pop"),
                               labels = c("PT SDP and PT Population",
                                          "PT SDP and PT GDP",
                                          "PT GDP and PT Population"))

sub_threat$pref <- factor(sub_threat$pref,
                          levels = c(pref_names, "nopreference"),
                          labels = str_remove(vars_labels, "PT "))

ggplot(subset(sub_threat, str_detect(pref, "Polity")),
       aes(x = year,
           y = est,
           ymin = cilo,
           ymax = cihi)) +
  geom_hline(yintercept = c(0,1), 
             linetype = "dashed",
             alpha = 0.5) +
  geom_ribbon(alpha = 0.3) +
  geom_line(size = 0.5) +
  facet_grid(pref ~ indicator) +
  theme_bw() +
  labs(title = "Correlation between alternative potential threat variables",
       x = "Year",
       y = "Pearson correlation coefficient with 95% CI") +
  coord_cartesian(ylim = c(0,1))


# Figure 19

master_dv_rel <- master %>%
  dplyr::select(ccode, year,
                milexsurplus1095_estimate,
                milperpop_estimate,
                tonnageimputedsurplus1095_estimate) %>%
  dplyr::mutate(milexsurplus1095_estimate_rescaled = c(scale(milexsurplus1095_estimate)),
                milperpop_estimate_rescaled = c(scale(milperpop_estimate)),
                tonnageimputedsurplus1095_estimate_rescaled = c(scale(tonnageimputedsurplus1095_estimate))) %>%
  gather(indicator, value, milexsurplus1095_estimate:tonnageimputedsurplus1095_estimate_rescaled) %>%
  dplyr::mutate(type = ifelse(str_detect(indicator, "rescaled"), "rescaled", "original"),
                indicator = str_replace(indicator, "\\_rescaled", ""),
                type_detailed = type,
                type_detailed = replace(type_detailed, str_detect(indicator, "milex"), "original_milex"),
                type_detailed = replace(type_detailed, str_detect(indicator, "milper"), "original_milper"),
                type_detailed = replace(type_detailed, str_detect(indicator, "tonnage"), "original_tonnage"),
                type_detailed = replace(type_detailed, type == "rescaled", "rescaled")) %>%
  filter(!is.na(value))

master_dv_rel$indicator <- factor(master_dv_rel$indicator,
                                  levels = c("milexsurplus1095_estimate",
                                             "milperpop_estimate",
                                             "tonnageimputedsurplus1095_estimate"),
                                  labels = c("Military expenditure/SDP",
                                             "Military personnel/population",
                                             "Naval tonnage/SDP"))

master_dv_rel$type_detailed <- factor(master_dv_rel$type_detailed,
                                      levels = c("original_milex",
                                                 "original_milper",
                                                 "original_tonnage",
                                                 "rescaled"),
                                      labels = c("Military expenditure/SDP",
                                                 "Military personnel/population",
                                                 "Naval tonnage/SDP",
                                                 "Rescaled version"))

p_milex <- ggplot(subset(master_dv_rel,
                         indicator == "Military expenditure/SDP" & type == "original"),
                  aes(x = year, y = value, color = indicator, linetype = indicator)) +
  geom_point(alpha = 0.1, size = 0.1) +
  stat_smooth(size = 1.5, alpha = 1, se = F) +
  theme_bw() +
  scale_color_manual(values = c("#006687"),
                     name = "") + 
  scale_linetype_manual(name = "",
                        values = c("solid")) +
  labs(title = "Military expenditure index",
       x = "Year",
       y = "Military expenditure/SDP") +
  theme(legend.position = "none")

p_tonnage <- ggplot(subset(master_dv_rel,
                           indicator == "Naval tonnage/SDP" & type == "original"),
                    aes(x = year, y = value, color = indicator, linetype = indicator)) +
  geom_point(alpha = 0.1, size = 0.1) +
  stat_smooth(size = 1.5, alpha = 1, se = F) +
  theme_bw() +
  scale_color_manual(values = c("#870023"),
                     name = "") + 
  scale_linetype_manual(name = "",
                        values = c("dashed")) +
  coord_cartesian(ylim = c(0, 3000),
                  xlim = c(1816, 2012)) +
  labs(title = "Naval tonnage index",
       x = "Year",
       y = "Naval tonnage/SDP") +
  theme(legend.position = "none")

p_rescaled <- ggplot(subset(master_dv_rel,
                            type == "rescaled" & indicator != "Military personnel/population"),
                     aes(x = year, y = value, color = indicator, linetype = indicator)) +
  geom_point(alpha = 0.1, size = 0.1) +
  stat_smooth(size = 1.5, alpha = 1, se = F) +
  theme_bw() +
  scale_color_manual(values = c("Military expenditure/SDP" = "#006687",
                                "Naval tonnage/SDP" = "#870023"),
                     name = "") + 
  scale_linetype_manual(name = "",
                        values = c("solid", "dashed")) +
  coord_cartesian(ylim = c(-0.75, 1.5),
                  xlim = c(1816, 2012)) +
  labs(title = "Rescaled dependent variables",
       x = "Year",
       y = "Rescaled dependent variables") +
  theme(legend.position = "bottom")

all <- grid.arrange(p_milex,
                    # p_milper,
                    p_tonnage,
                    p_rescaled, ncol = 1)

# Figure 20

master_sub <- master %>%
  filter(year >= 1865, year <= 2007)

years <- unique(master_sub$year)

### Proportions
# milexgdpsurplus1095 milperpop
cor_milexgdpsurplus1095_milperpop <- c()
yr_milexgdpsurplus1095_milperpop <- c()
for(i in 1:length(years)){
  cor_milexgdpsurplus1095_milperpop[i] <- cor.test(master_sub$milexsurplus1095_estimate[master_sub$year == years[i]],
                                                   master_sub$milperpop_estimate[master_sub$year == years[i]])$estimate
  yr_milexgdpsurplus1095_milperpop[i] <- years[i]
}

df_milexgdpsurplus1095_milperpop <- data.frame(year = yr_milexgdpsurplus1095_milperpop, milex_milper = cor_milexgdpsurplus1095_milperpop)

# milexgdpsurplus1095 tonnagegdpsurplus1095
cor_milexgdpsurplus1095_tonnageimputedgdpsurplus1095 <- c()
yr_milexgdpsurplus1095_tonnageimputedgdpsurplus1095 <- c()
for(i in 1:length(years)){
  cor_milexgdpsurplus1095_tonnageimputedgdpsurplus1095[i] <- cor.test(master_sub$milexsurplus1095_estimate[master_sub$year == years[i]],
                                                                      master_sub$tonnageimputedsurplus1095_estimate[master_sub$year == years[i]])$estimate
  yr_milexgdpsurplus1095_tonnageimputedgdpsurplus1095[i] <- years[i]
}

df_milexgdpsurplus1095_tonnageimputedgdpsurplus1095 <- data.frame(year = yr_milexgdpsurplus1095_tonnageimputedgdpsurplus1095, milex_tonnageimputed = cor_milexgdpsurplus1095_tonnageimputedgdpsurplus1095)

# milperpop tonnagegdpsurplus1095
cor_milperpop_tonnageimputedgdpsurplus1095 <- c()
yr_milperpop_tonnageimputedgdpsurplus1095 <- c()
for(i in 1:length(years)){
  cor_milperpop_tonnageimputedgdpsurplus1095[i] <- cor.test(master_sub$milperpop_estimate[master_sub$year == years[i]],
                                                            master_sub$tonnageimputedsurplus1095_estimate[master_sub$year == years[i]])$estimate
  yr_milperpop_tonnageimputedgdpsurplus1095[i] <- years[i]
}

df_milperpop_tonnageimputedgdpsurplus1095 <- data.frame(year = yr_milperpop_tonnageimputedgdpsurplus1095, milper_tonnageimputed = cor_milperpop_tonnageimputedgdpsurplus1095)

# Merging
merged <- full_join(df_milexgdpsurplus1095_milperpop, df_milexgdpsurplus1095_tonnageimputedgdpsurplus1095) %>%
  full_join(df_milperpop_tonnageimputedgdpsurplus1095) %>%
  dplyr::select(year, everything()) %>%
  gather(key, value, 2:4)

merged$key <- factor(merged$key,
                     levels = c("milex_milper", "milex_tonnageimputed", "milper_tonnageimputed"),
                     labels = c("Military expenditure index and military personnel index",
                                "Military expenditure index and naval tonnage index", 
                                "Military personnel index and naval tonnage index"))

ggplot(subset(merged,
              key == "Military expenditure index and naval tonnage index"), 
       aes(x = year, y = value)) +
  geom_point(alpha = 0.3) +
  stat_smooth(alpha = 0.7) +
  theme_bw() +
  facet_wrap(~key) +
  labs(title = "Year-by-year correlations for dependent variables",
       x = "Year", 
       y = "Pearson's correlation coefficient") +
  theme(legend.position = "bottom") +
  scale_x_continuous(breaks = seq(1850,2000, 25)) +
  coord_cartesian(ylim = c(0,1))

# Figure 21

df_individ <- master %>%
  dplyr::select(ccode,
                year,
                ln_milex_surplus1095,
                # ln_milper_pop,
                ln_tonnageimputed_surplus1095,
                one_of(vars_comp)) %>%
  filter(ccode %in% c(2, 200, 220, 365, 710, 740, 140),
         year >= 1812,
         year <= 2012) %>%
  gather(indicator, value, -ccode, -year) %>%
  na.omit() %>%
  dplyr::mutate(value_new = ifelse(indicator %in% vars_comp,
                                   value, exp(value)),
                value_log10 = log10(value_new),
                indicator_category = as.character(ifelse(indicator %in% c("comp_surplus1095_polity_invlog_nodenom_all",
                                                                          "comp_surplus1095_boix_invlog_nodenom_all",
                                                                          "comp_surplus1095_uds_invlog_nodenom_all"),
                                                         "Regime type potential threat", 
                                                         NA))) %>%
  dplyr::mutate(indicator_category = replace(indicator_category, !(indicator %in% c("comp_surplus1095_polity_invlog_nodenom_all",
                                                                                    "comp_surplus1095_boix_invlog_nodenom_all",
                                                                                    "comp_surplus1095_uds_invlog_nodenom_all")),
                                             "Alternative potential threat"),
                indicator_category = replace(indicator_category, indicator == "ln_milex_surplus1095", "Military expenditure index"),
                #indicator_category = replace(indicator_category, indicator == "ln_milper_pop", "Military personnel index"),
                indicator_category = replace(indicator_category, indicator == "ln_tonnageimputed_surplus1095", "Naval tonnage index"))

df_individ$ccode <- factor(df_individ$ccode, levels = c(2, 200, 220, 365, 710, 740, 140),
                           labels = c("United States", "United Kingdom", "France", "Russia","China", "Japan", "Brazil"))

df_individ$indicator_category <- factor(df_individ$indicator_category,
                                        levels = c("Regime type potential threat",
                                                   "Alternative potential threat",
                                                   "Military expenditure index",
                                                   # "Military personnel index",
                                                   "Naval tonnage index"),
                                        labels = c("Regime type potential threat",
                                                   "Alternative potential threat",
                                                   "Military expenditure/SDP",
                                                   # "Military personnel/population",
                                                   "Naval tonnage/SDP"))

ggplot(filter(df_individ, year >= 1900 & indicator_category != "Alternative potential threat" & ccode != "Russia"), 
       aes(x = year, 
           y = value_log10, 
           color = indicator_category)) +
  geom_point(alpha = 0.2, size = 0.7) +
  geom_smooth(se = F, size = 1) +
  facet_grid(indicator_category ~ ccode, 
             scales = "free_y") +
  theme_bw() +
  scale_color_manual(values = c("Regime type potential threat" = "black", 
                                #"Alternative potential threat" = "black",
                                "Military expenditure/SDP" = "#006687", 
                                "Naval tonnage/SDP" = "#870023")) +
  theme(legend.position = "none",
        strip.text = element_text(size = 12, face = "bold")) + 
  labs(x = "Year",
       y = "Value of Variable (logarithm base 10)",
       title = "Potential threat and military capabilities for select countries (1900-1999)") +
  scale_x_continuous(breaks = seq(1900, 2000, 50))# +

# Figure 22

master_comp_world <- master %>%
  dplyr::select(ccode,
                year,
                comp_gdp_polity_invlog_nodenom_all,
                comp_gdp_boix_invlog_nodenom_all,
                comp_gdp_uds_invlog_nodenom_all,
                comp_surplus1095_polity_invlog_nodenom_all,
                comp_surplus1095_boix_invlog_nodenom_all,
                comp_surplus1095_uds_invlog_nodenom_all) %>%
  gather(indicator, value, -ccode, -year) %>%
  dplyr::mutate(econ = ifelse(str_detect(indicator, "surplus"), "SDP", "GDP"),
                var = ifelse(str_detect(indicator, "polity"), "polity", NA),
                var = replace(var, str_detect(indicator, "boix"), "boix"),
                var = replace(var, str_detect(indicator, "uds"), "uds")) %>%
  dplyr::mutate(region = "World")

master_comp_region  <- master %>%
  dplyr::select(ccode,
                year,
                comp_gdp_polity_invlog_nodenom_all,
                comp_gdp_boix_invlog_nodenom_all,
                comp_gdp_uds_invlog_nodenom_all,
                comp_surplus1095_polity_invlog_nodenom_all,
                comp_surplus1095_boix_invlog_nodenom_all,
                comp_surplus1095_uds_invlog_nodenom_all) %>%
  gather(indicator, value, -ccode, -year) %>%
  dplyr::mutate(region = ifelse(ccode < 200, "Americas", NA),
                region = replace(region, ccode >= 200 & ccode < 400, "Europe"),
                region = replace(region, ccode >= 400 & ccode < 600, "Sub-Saharan Africa"),
                region = replace(region, ccode >= 600 & ccode < 700, "Middle East and N. Africa"),
                region = replace(region, ccode >= 700 & ccode < 900, "Asia"),
                region = replace(region, ccode >= 900, "Australia and Ozeania")) %>%
  dplyr::mutate(econ = ifelse(str_detect(indicator, "surplus"), "SDP", "GDP"),
                var = ifelse(str_detect(indicator, "polity"), "polity", NA),
                var = replace(var, str_detect(indicator, "boix"), "boix"),
                var = replace(var, str_detect(indicator, "uds"), "uds")) %>%
  filter(ccode < 900) %>%
  bind_rows(master_comp_world)

master_comp_region$var <- factor(master_comp_region$var,
                                 levels = c("polity",
                                            "boix",
                                            "uds"),
                                 labels = c("Potential threat Polity",
                                            "Potential threat Boix et al.",
                                            "Potential threat UDS"))

master_comp_region$econ <- factor(master_comp_region$econ, 
                                  levels = c("SDP",
                                             "GDP"))

ggplot(master_comp_region,
       aes(x = year, y = value, color = econ, linetype = econ)) +
  geom_point(alpha = 0.025, size = 0.5) +
  geom_smooth(se = F, size = 0.75, alpha = 0.9) +
  theme_bw() +
  facet_grid(var~region) +
  theme(legend.position = "top",
        legend.key.width = unit(1, "cm")) +
  labs(title = "Potential threat over time by world region",
       x = "Year",
       y = "Standardized value of the potential threat measure") +
  scale_color_manual(name = "Power-resources",
                     values = c("#da7c30", #orange
                                "gray50")) +
  scale_linetype_manual(name = "Power-resources",
                        values = c("solid", 
                                   "21")) +
  coord_cartesian(ylim = c(0,21))

#  Figures 23 - 27
# code for figures 23 - 27 (coefficient plots) can be found in replication_models_tables.R