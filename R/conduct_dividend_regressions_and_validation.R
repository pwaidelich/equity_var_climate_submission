# clean up the environment
rm(list = ls())

# load packages
library(plm)
library(tidyverse)
library(readxl)
library(janitor)
library(fixest)
library(ggpubr)

# source helper functions
source("R/utils/clean_value_labels.R")

# set the ggplot theme
theme_set(theme_classic())

# load the 6th release of the JST database
df_jst <- read_excel("data/240115 JSTdatasetR6.xlsx")

# read in the Calibration.xlsx info on actual index dividends and market caps and clean column names
df_calib_div <- read_excel("data/Calibration.xlsx", sheet = "Index size") %>%
  clean_names()

# read in the same information at the country level, clean column names, and discard non-country rows (= missing ISO)
df_calib_marketcap <- read_excel("data/Calibration.xlsx", sheet = "Portfolio") %>%
  clean_names() %>%
  filter(!is.na(iso3)) %>% select(iso3, name, market_cap_in_acwi, msci_div_yield)

# load the country-specific present value weights for each year and by SSP
pvtimeseries_2024_2100 <- readRDS("data/weights/pvtimeseries_2024_2100.rds")

# merge market cap into df_jst
df_jst <- df_jst %>% left_join(df_calib_marketcap %>% select(iso3, market_cap_in_acwi), by = c("iso" = "iso3"))

# confirm that no years are skipped
if(!all(df_jst %>% group_by(iso) %>%
        # create a dummy indicating if the row above has a year value that is 1 lower
        mutate(year_past = dplyr::lag(year) == year -1) %>%
        # drop years that are the minimum year (= have no row above them)
        filter(year != min(year)) %>%
        # take the dummy values and check if this is equal to TRUE (as it should) - if not, throw error
        pull(year_past))) {
  stop("Some years are skipped")
}

# calculate inflation (CPI growth, see JST paper's main text) and nominal GDP growth
df_jst <- df_jst %>% group_by(iso) %>%
  
  mutate(gdp_growth_nominal = gdp/dplyr::lag(gdp, 1) - 1,
         inflation = cpi/dplyr::lag(cpi, 1) - 1) %>% ungroup()

# inspect the data
df_jst %>% select(starts_with("eq_"), contains("gdp")) %>% summary()


#### variable definitions in the JST database (from R6 documentation, pp.6-7)

# gdp: GDP (nominal, local currency)
# eq_tr: Equity total return, nominal. r[t] = [[p[t] + d[t]] / p[t-1] ] - 1
# eq_capgain: Equity capital gain, nominal. cg[t] = [ p[t] / p[t-1] ] - 1
# eq_dp: Equity dividend yield. dp[t] = dividend[t]/p[t]
# eq_div_rtn: Equity dividend return. dp rtn[t] = dividend[t]/p[t-1]
# rgdpmad: Real GDP per capita (PPP, 1990 Int$, Maddison)
# rgdpmad: Real GDP per capita (PPP, 1990 Int$, Maddison)
# NOTE: pop for Germany in 2020 is 84491, so this is in 1000s

# NOTE: MSCI data features div. yield (eq_dp) and gross return (which includes capgains and reinvested divs, hence eq_tr)

# NOTE: we do not know dividends but we know that
# dividend_t = dp_t x p_t --> dividend_t-1 = dp_t-1 x p_t-1
# dividing by each other yields:
# dividend_t / dividend_t-1 = (dp_t/dp_t-1) x (p_t/p_t-1)
# capgain = p_t/p_t-1 - 1, so last term equals 1 + capgain

###

# calculate dividends, real GDP (PPP), growth in real GDP
df_jst <- df_jst %>% group_by(iso) %>%
  
  # calculate nominal dividend growth as per the formula derived above
  mutate(div_growth_nominal = (eq_dp/dplyr::lag(eq_dp, 1)) * (1 + eq_capgain) - 1) %>%
  
  # calculate growth rates in real GDPpc and population
  mutate(gdppc_mad_growth_real = rgdpmad/dplyr::lag(rgdpmad, 1),
         gdppc_barro_growth_real = rgdpbarro/dplyr::lag(rgdpbarro, 1),
         pop_growth = pop/dplyr::lag(pop, 1)) %>%
  
  # calculate real GDP growth based on the other growth rates (GDPpc growth x POP growth)
  mutate(gdp_mad_growth_real = gdppc_mad_growth_real*pop_growth - 1,
         gdp_barro_growth_real = gdppc_barro_growth_real*pop_growth - 1) %>%
  
  # if growth is infinite (because previous value was 0), set to NA
  mutate(div_growth_nominal = if_else(is.infinite(div_growth_nominal), NA_real_, div_growth_nominal)) %>%
  
  # inflation-adjust all nominal growth rates/returns by using the CPI inflation rate
  mutate(div_growth_real = (1+div_growth_nominal)/(1 + inflation) - 1,
         gdp_growth_real = (1 + gdp_growth_nominal)/(1 + inflation) - 1,
         eq_tr_real = (1 + eq_tr)/(1 + inflation) - 1,
         eq_dp_real = (1 + eq_dp)/(1 + inflation) - 1,
         eq_div_rtn_real = (1 + eq_div_rtn)/(1 + inflation) - 1,
         eq_capgain_real = (1 + eq_capgain)/(1 + inflation) - 1
         ) %>%
  
  # ungroup
  ungroup()

# compare gdp growth
cor(df_jst$gdp_growth_real, df_jst$gdp_mad_growth_real, use = "complete.obs")
cor(df_jst$gdp_growth_real, df_jst$gdp_barro_growth_real, use = "complete.obs")
cor(df_jst$gdp_mad_growth_real, df_jst$gdp_barro_growth_real, use = "complete.obs")
# -> Maddison and Barro are similar but not identical, growth rate based on local currency is quite different (as expected)

# compare dividend growth and nominal GDP growth for the US
df_jst %>%
  
  # subset to US and from 1950 (= exclude War and pre-War years as well as immediate post-war period)
  filter(iso %in% c("USA"), year > 1950) %>%
  
  # draw lines
  ggplot(aes(year)) +
  geom_hline(aes(yintercept = 0), linetype = "dashed", color = "grey") +
  geom_line(aes(y = div_growth_real, colour = "Dividend growth (real)")) +
  geom_line(aes(y = gdp_mad_growth_real, colour = "GDP growth (real)")) +
  
  # facet by country (= effectively adds a country label above the chart)
  facet_wrap(~ country) +
  
  # set chart elements
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  labs(colour = NULL, y= NULL, x = NULL) +
  theme_classic() +
  theme(legend.position = c(0.4, 0.15),
        legend.background = element_blank())

# save out
ggsave(file.path("graphs", paste0(Sys.Date(), " SI_USgdpversusdividendgrowth.pdf")),
       width = 4, height = 4)


################################################################################
########################## REGRESSIONS #########################################
################################################################################

### a) test for unit roots using the plm package

# create a pdata.frame
df_jst_plm <- plm::pdata.frame(df_jst %>% filter(!is.na(div_growth_real) & !is.na(gdp_mad_growth_real)) %>%
                                 filter(year > 1950), index = c("iso", "year"))

# IPS test for dividend growth
div_test_ips <- plm::purtest(df_jst_plm$div_growth_real, exo = "intercept", test = "ips")
summary(div_test_ips)

# MADWU test for dividend growth
div_test_madwu <- plm::purtest(df_jst_plm$div_growth_real, exo = "intercept", test = "madwu")
summary(div_test_madwu)

# repeat for GDP growth
gdp_test_ips <- plm::purtest(df_jst_plm$gdp_mad_growth_real, exo = "intercept", test = "ips")
summary(gdp_test_ips)

gdp_test_madwu <- plm::purtest(df_jst_plm$gdp_mad_growth_real, exo = "intercept", test = "madwu")
summary(gdp_test_madwu)
# -> unit root hypothesis can be rejected at 5% significance level for full sample and all countries (except Norway dividends, 10% level)
#    and Japan GDP growth (p > 10%)

# clean up
rm(div_test_ips, div_test_madwu, gdp_test_ips, gdp_test_madwu, df_jst_plm)


### b) estimate regressions

# setting overall table style for the fixest package
my_style_tex = style.tex(main = "base",
                         fixef.title = '\\midrule',
                         fixef.suffix = " FEs",
                         stats.title = '\\midrule',
                         depvar.title = "",
                         model.title = "",
                         var.title = "\\midrule",
                         adjustbox = "width = 1\\textwidth, center",
                         fontsize = "small",
                         signif.code = c("***"=0.001, "**"=0.01, "*"=0.05, "\\dagger"=0.10))

# setting table style for fixest
setFixest_etable(
  fitstat = c("n", "r2", "wr2", "bic"),
  style.tex = my_style_tex,
  digits = 3,
  digits.stats = 3
)

# define and set dictionary for clean labels
fixest_dict <- c(
  gdp_growth_for_regression = "Real GDP PPP growth",
  alternative_gdp = "Barro GDP growth",
  is_crisis  = "I(Crisis year)",
  is_recession = "I(Negative GDP growth)",
  is_above_break = "I(Year > 1985)",
  div_growth_real = "Real dividend growth"
)
setFixest_dict(fixest_dict)

# make a function to quickly produce the entire regression table
make_regression_table <- function(year_threshold = 1950,
                                  div_growth_lowerbound = -Inf,
                                  div_growth_upperbound = Inf,
                                  gdp_growth_used = "gdp_mad_growth_real",
                                  return_fixest_object = F,
                                  time_break_used = 1985,
                                  vcov_used = as.formula("~ iso + year"),
                                  weighting_variable_used = "market_cap_in_acwi") {
  
  # print out the function inputs
  print(paste("year_threshold: ", year_threshold))
  print(paste("div_growth_lowerbound: ", div_growth_lowerbound))
  print(paste("div_growth_upperbound: ", div_growth_upperbound))
  print(paste("gdp_growth_used: ", gdp_growth_used))

  # create a separate tibble for modifications
  df_reg <- df_jst 
  
  # put the selected GDP growth variable into a new column
  df_reg <- df_reg %>% mutate(gdp_growth_for_regression = !!as.name(gdp_growth_used))

  # set weights to one for all obs if no weighting variable is used, otherwise put it into a new column
  if(is.null(weighting_variable_used)) {
    df_reg <- df_reg %>% mutate(weighting_variable = 1)
  } else {
    df_reg <- df_reg %>% mutate(weighting_variable = !!as.name(weighting_variable_used))
  }
    
  # convert growth from decimals to %
  df_reg <- df_reg %>% mutate_at(vars(contains("growth")), ~ . * 100)
  
  # put the alternative GDP measure into a new column
  if(gdp_growth_used == "gdp_barro_growth_real")   df_reg <- df_reg %>% mutate(alternative_gdp = !!as.name("gdp_mad_growth_real"))
  if(gdp_growth_used == "gdp_mad_growth_real")   df_reg <- df_reg %>% mutate(alternative_gdp = !!as.name("gdp_barro_growth_real"))
  
  # calculate the quantiles used for trimming the dividend growth rate
  thresholds <- quantile(df_reg %>% filter(!is.na(div_growth_real)) %>% pull(div_growth_real) , probs = c(0.025, 0.975))
  
  # create a list where fixest objects will be stored
  regr_realgrowth <- list()
  
  # estimate the different specifications of interest
  regr_realgrowth[["No FEs"]] <- feols(div_growth_real ~ gdp_growth_for_regression, data = df_reg %>% filter(year > year_threshold,
                                                                                                         div_growth_real > div_growth_lowerbound,
                                                                                                         div_growth_real < div_growth_upperbound),
                                       vcov = ~ iso + year,
                                       panel.id = c("iso", "year"))
  
  regr_realgrowth[["Year FE"]] <- feols(div_growth_real ~ gdp_growth_for_regression | year,
                                        data = df_reg %>% filter(year > year_threshold),
                                        vcov = ~ iso + year)
  
  regr_realgrowth[["Main"]] <- feols(div_growth_real ~ gdp_growth_for_regression | year + iso[year],
                                           data = df_reg %>% filter(year > year_threshold),
                                           vcov = ~ iso + year,
                                           panel.id = c("iso", "year"))
  
  regr_realgrowth[["+ Quadratic"]] <- feols(div_growth_real ~ gdp_growth_for_regression + I(gdp_growth_for_regression^2) | year + iso[year],
                                                      data = df_reg %>% filter(year > year_threshold),
                                                      vcov = ~ iso + year)
  
  regr_realgrowth[["Other GDP measure"]] <- feols(div_growth_real ~ alternative_gdp | year + iso[year],
                                         data = df_reg %>% filter(year > year_threshold) %>%
                                           mutate(is_crisis = crisisJST == 1),
                                         vcov = ~ iso + year,
                                         panel.id = c("iso", "year"))
  
  regr_realgrowth[["+ Crises"]] <- feols(div_growth_real ~ gdp_growth_for_regression + i(is_crisis, gdp_growth_for_regression, ref = "FALSE") + i(is_crisis) | year + iso[year],
                                           data = df_reg %>% filter(year > year_threshold) %>%
                                         mutate(is_crisis = crisisJST == 1),
                                           vcov = ~ iso + year,
                                           panel.id = c("iso", "year"))
  
  regr_realgrowth[["+ Recessions"]] <- feols(div_growth_real ~ gdp_growth_for_regression + i(is_recession) + i(is_recession, gdp_growth_for_regression, ref = "FALSE")  | year + iso[year],
                                                data = df_reg %>% filter(year > year_threshold) %>%
                                             mutate(is_recession = gdp_growth_for_regression < 0),
                                                vcov = ~ iso + year,
                                                panel.id = c("iso", "year"))
  
  regr_realgrowth[["Market cap-weighted"]] <- feols(div_growth_real ~ gdp_growth_for_regression | year + iso[year],
                                           data = df_reg %>% filter(year > year_threshold),
                                           vcov = ~ iso + year,
                                           panel.id = c("iso", "year"), weights = ~ weighting_variable)
  
  regr_realgrowth[["Trim upp/low 2.5%"]] <- feols(div_growth_real ~ gdp_growth_for_regression | year + iso[year],
                                             data = df_reg %>% filter(year > year_threshold,
                                                                      div_growth_real > thresholds[1],
                                                                      div_growth_real < thresholds[2]),
                                             vcov = ~ iso + year,
                                             panel.id = c("iso", "year"))
  
  regr_realgrowth[["Break"]] <- feols(div_growth_real ~ gdp_growth_for_regression + i(above_break, gdp_growth_for_regression, ref = "FALSE") | year + iso[year],
                                            data = df_reg %>% filter(year > year_threshold) %>%
                                             mutate(above_break = year > time_break_used),
                                            vcov = ~ iso + year,
                                            panel.id = c("iso", "year"))
  
  # print out the regression table
  etable(regr_realgrowth, vcov = vcov_used) %>% print()
  
  # return the list of fixest objects if requested
  if(return_fixest_object) {
    return(regr_realgrowth)
  } else {
    return(NULL)
  }
}

# # test the function (COMMENTED OUT)
# make_regression_table()

# make the main table
fit_feols <- make_regression_table(gdp_growth_used = "gdp_mad_growth_real", year_threshold = 1950, time_break_used = 1985,
                                   return_fixest_object = T)

# export it as .tex file
fit_feols %>% etable(file = file.path("tables", paste0(Sys.Date(), " div_growth_regression.tex")), replace = T,
                     label = "tab:regress_div_growth",
                     headers = names(fit_feols),
                     se.row = T,
                     title = "Regressing dividend growth on GDP growth using the Jorda-Schularick-Taylor Macrohistory Database",
                     adjustbox = "width = 1\\textwidth, height = 0.23\\textheight, center")

# which countries are effectively included?
countries_included <- fixef(fit_feols$Main)$iso %>% names()
df_jst %>% filter(!iso %in% countries_included) %>% pull(iso) %>% unique()
df_jst %>% filter(iso %in% c("CAN", "IRL")) %>% summary()
# -> required financial data is missing for Canada and Irland, so effective sample size is 16

# test if the sum of the coefficients is significantly different from 1
marginaleffects::hypotheses(fit_feols$`+ Recessions`, hypothesis = "b1 + b3 = 0", vcov = fit_feols$`+ Recessions`$cov.scaled)


### c) make additional scatter plots and other charts

# plot dividend growth vs gdp growth over time
df_jst %>%
  filter(year > 1950) %>%
  ggplot(aes(year)) +
  geom_line(aes(y = div_growth_real, color = "Div growth")) +
  geom_line(aes(y = gdp_mad_growth_real, color = "GDP growth")) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  labs(title = "Dividend vs GDP growth",
       x = "Year") +
  facet_wrap(~ iso) +
  coord_cartesian(ylim = c(-0.5, 1)) +
  scale_x_continuous(breaks = c(1975, 2000)) +
  theme(legend.position = "bottom")


################################################################################
######################### VALIDATION ###########################################
################################################################################

# calculate the implied market cap based on the dividends 
df_validation <- pvtimeseries_2024_2100 %>% filter(portfolio == "msciacwi") %>%
  
  # for each country and SSP, sum up the present-value of equity income (= divs) and extract the initial share
  # NOTE: initialequityshare is normed to 1 for a given portfolio and represents the dividends in the base year
  #       since each portfolio has multiple countries, initialequityshare does not equal 1 for a given country
  group_by(iso3, ssp) %>%
  summarise(total_presentequity = sum(presentequity_damodaran),
            initialequityshare = first(initialequityshare), .groups = "drop") %>%
  
  # calculate how much the initial dividends account for in the total present value of all dividends
  mutate(inverse_ratio = initialequityshare/total_presentequity,
         ratio = 1/inverse_ratio) %>%
  
  # discard countries that do not feature in the portfolio (= initialequityshare of zero)
  filter(initialequityshare > 0) %>%
  
  # merge in actual market cap and dividend yield data
  left_join(df_calib_marketcap, by = "iso3") %>%
  
  # calculate the implied market cap based on the dividends
  arrange(desc(initialequityshare)) %>%
  mutate(marketcap_implied = market_cap_in_acwi*msci_div_yield*ratio,
         estimation_error = round(marketcap_implied/market_cap_in_acwi - 1, 2))

# calculate the same at the index level
df_validation_index <- pvtimeseries_2024_2100 %>%
  group_by(ssp, portfolio) %>% 
  summarise(total_presentequity = sum(presentequity_damodaran), .groups = "drop") %>%
  # NOTE: at the index level, initialequityshare sums up to 1 and hence we can simply take the inverse of the total PV
  mutate(inverse_ratio = 1/total_presentequity)

# plot index-level validation results
p1 <- df_validation_index %>%
  
  # subset to indices of interest
  filter(portfolio %in% c("msciworld", "msciacwi", "msciem", "mscifm")) %>%

  # merge in information on actual dividend yield
  left_join(df_calib_div, by = c("portfolio")) %>%
  mutate(marketcap_implied = total_presentequity*div_actual_udsbn,
         index_size = index_size * 10^(-9),
         estimation_error = round(marketcap_implied/index_size - 1, 2)) %>%
  
  # add the US (which due to its importance for global portfolios is displayed separately)
  bind_rows(df_validation %>% filter(iso3 == "USA") %>% rename(portfolio = "iso3",
                                                               index_size = "market_cap_in_acwi") %>%
              # convert monetary variables from USD to billion
              mutate(marketcap_implied = marketcap_implied/10^9,
                     index_size = index_size/10^9)) %>%
  
  # clean up the index labels
  mutate(label_clean = factor(case_when(portfolio == "msciworld" ~ "MSCI World",
                                 portfolio == "msciacwi" ~ "MSCI ACWI",
                                 portfolio == "msciem" ~ "MSCI EM",
                                 portfolio == "mscifm" ~ "MSCI FEM",
                                 portfolio == "USA" ~ "USA",
                                 T ~ NA_character_
                                 ),
                              levels = c("MSCI ACWI", "MSCI World", "USA", "MSCI EM", "MSCI FEM"))) %>%
  
  # drop the MSCI ACWI (which we do not use in the paper)
  filter(portfolio != "msciacwi") %>%
  
  # create the chart
  ggplot(aes(ssp)) + 
  
  # NOTE: we divide by 10^3 to convert from USD bn to USD tn
  geom_point(aes(y = index_size/10^3, color = "Actual")) +
  geom_point(aes(y = marketcap_implied/10^3, color = "Implied by our model\n& current dividend yields")) +

  # set labels, axis limits and facet by portfolio using the clean labels
  scale_y_continuous(limits = c(0, NA)) +
  labs(y = "Market cap in USD trillion", x = NULL, color = NULL) +
  facet_wrap(~ label_clean, scales = "free") +
  theme(legend.position = "bottom")


# plot country-level validation results
# NOTE: we divide values by 10^12 to convert from USD to USD tn
p2 <- df_validation %>%
  
  # display the actual market cap (converted to USD tn) on x axis
  ggplot(aes(market_cap_in_acwi/10^12, color = iso3)) +
  
  # draw the 45deg line
  geom_abline(slope = 1, intercept = 0, linetype = "dashed") +
  
  # add dots for the implied market cap, with different symbols for the respective SSP used
  geom_point(aes(y = marketcap_implied/10^12,  shape = ssp)) +
  
  # draw an error bar between the two SSP points for each country
  geom_errorbar(data = df_validation %>%
                  pivot_wider(names_from = ssp, values_from = c(market_cap_in_acwi, marketcap_implied)) %>%
                  group_by(iso3) %>% summarise(market_cap_in_acwi = mean(market_cap_in_acwi_SSP2, na.rm = TRUE),
                                               marketcap_implied_SSP2 = mean(marketcap_implied_SSP2, na.rm = TRUE),
                                               marketcap_implied_SSP5 = mean(marketcap_implied_SSP5, na.rm = TRUE), .groups = "drop"),
                aes(ymin = marketcap_implied_SSP2/10^12, ymax = marketcap_implied_SSP5/10^12)) +
  
  # add country labels
  ggrepel::geom_label_repel(aes(y = ifelse(ssp == "SSP2" & iso3 != "USA", marketcap_implied/10^12, NA), label = iso3),
                            min.segment.length = unit(1000, 'lines'), size = 2) +
  
  # add chart labels, axis limits, omit the color legend, and position the legend
  labs(y = "Implied market cap (USD trillion)", x = "Actual market cap (USD trillion)", shape = NULL, color = NULL) +
  coord_cartesian(xlim = c(0, 4), ylim = c(0, 4)) +
  guides(color = "none") +
  theme(legend.position = "bottom")

# combine the two into a 2x1 grid chart
ggarrange(p1, p2, nrow = 1, align = "hv", widths = c(0.8, 1))

# save out
ggsave(file.path("graphs", paste0(Sys.Date(), " SI_marketcap_validation.pdf")), width = 7, height = 4)
