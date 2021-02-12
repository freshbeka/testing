## ----setup, include=FALSE------------------------------------------------------------------
knitr::opts_chunk$set(echo = TRUE, 
                      fig.align = "center", out.width = '70%')


## ---- echo = FALSE, fig.align = "center", out.width = "40%"--------------------------------
knitr::include_graphics("images/seastars.png")


## ----equatiomatic, results = "hold"--------------------------------------------------------
library(equatiomatic)

## Fit a simple linear regression model
model_fit <- lm(mpg ~ cyl + disp, mtcars)

## Give the results to extract_eq
extract_eq(model_fit)

## raw result
## $$
## \operatorname{mpg} = \alpha + \beta_{1}(\operatorname{cyl}) + \beta_{2}(\operatorname{disp}) + \epsilon
## $$


## ----load_pkgs, message=FALSE--------------------------------------------------------------
## for reading data
library(readxl)
library(readr)
## for data munging
library(dplyr)
library(tidyr)
## for plotting
library(ggplot2)
library(patchwork)
## for model fitting
library(R2jags)


## ---- get_author_data----------------------------------------------------------------------
## data directory
data_dir <- here::here("lectures", "week_06", "data")
## original data file
orig_data <- file.path(data_dir, "Schultz_data_ver2.xlsx")
## worksheets in notebook
sht_names <- excel_sheets(orig_data)
## convert worksheets to csv
if(length(list.files(data_dir, "csv")) == 0) {
  for(i in sht_names) {
    orig_data %>% read_xlsx(sheet = i) %>%
      write_csv(file = file.path(data_dir, i))
  }
}


## ----read_count_data, message=FALSE--------------------------------------------------------
## read count data
counts <- read_csv(file.path(data_dir, "transectcounts.csv"))
colnames(counts)
## split out before/after counts of sunflower stars
## wide format
stars_wide <- counts %>%
  select(ssws, transect, sunflower.star) %>%
  spread(ssws, value = sunflower.star)  %>%
  select(-transect)
## tidy format
stars_tidy <- stars_wide %>%
  gather(key = time, value = count)


## ----plot_data, fig.height=5, fig.width=8--------------------------------------------------
## base plot
pp <- ggplot(stars_tidy, aes(x=time, y=count, color=time))
## jittered dotplot
p1 <- pp + geom_jitter(shape=16, position=position_jitter(0.3)) + theme_bw()
## violin plot
p2 <- pp + geom_violin() + theme_bw()
## combine plots
p1 + p2 &
  theme(legend.position="none") &
  scale_x_discrete(limits=c("before","after")) &
  labs(x = "", y = "Count")


## ----glm_1---------------------------------------------------------------------------------
## total survey area in m^2
area <- 3.75


## ----pois_fit_jags, message=FALSE, cache=TRUE----------------------------------------------
## define Poisson model in JAGS
cat("

data {
  N <- dim(stars_wide);
}

model {
  ## PRIORS
  ln_lambda_bef ~ dnorm(0,0.01);
  ln_lambda_aft ~ dnorm(0,0.01);
  ## DERIVED PARAMS
  lambda_bef <- exp(ln_lambda_bef);
  lambda_aft <- exp(ln_lambda_aft);
  ## LIKELIHOOD
  for(i in 1:N[1]) {
    stars_wide[i,1] ~ dpois(lambda_aft * area);
    stars_wide[i,2] ~ dpois(lambda_bef * area);
  }
}

", file="poisson_glm.txt") ## end model description

## data to pass to JAGS
dat_jags <- c("stars_wide", "area") 
## model params for JAGS to return
par_jags <- c("lambda_bef","lambda_aft")
## MCMC control params
mcmc_chains <- 4
mcmc_length <- 2e4
mcmc_burn <- 1e4
mcmc_thin <- 20
## total number of MCMC samples
mcmc_samp <- (mcmc_length-mcmc_burn)*mcmc_chains/mcmc_thin
## list of model info for JAGS
mod_jags <- list(data = dat_jags,
                 model.file = "poisson_glm.txt",
                 parameters.to.save = par_jags,
                 n.chains = as.integer(mcmc_chains),
                 n.iter = as.integer(mcmc_length),
                 n.burnin = as.integer(mcmc_burn),
                 n.thin = as.integer(mcmc_thin))
## fit model
po_fit_jags <- do.call(jags.parallel, mod_jags)


## ----print_pois_fit_jags-------------------------------------------------------------------
print(po_fit_jags)


## ----plot_pois_fit_jags, fig.height=5, fig.width=8, message=FALSE--------------------------
## gather posteriors samples
pdat <- data.frame(Time = rep(c("before","after"), ea=mcmc_samp),
                   samples = c(po_fit_jags$BUGSoutput$sims.list$lambda_bef,
                               po_fit_jags$BUGSoutput$sims.list$lambda_aft))
## summary of posteriors
pdat2 <- pdat %>%
  group_by(Time) %>%
  summarize(lo = quantile(samples, 0.025),
            med = quantile(samples, 0.5),
            hi = quantile(samples, 0.975))
## histogram of posteriors
p1 <- ggplot(pdat, aes(samples, fill = Time)) +
  theme_bw() +
  geom_histogram(bins = 100) +
  guides(fill = guide_legend(reverse=TRUE)) +
  labs(x = expression(Density~(m^-2)), y = "Count")
p2 <- ggplot(pdat2, aes(x=Time, y=med, color=Time)) +
  theme_bw() +
  geom_point(aes(size=1.5)) +
  geom_errorbar(aes(ymin=lo, ymax=hi), width=0) +
  scale_x_discrete(limits=c("before","after")) +
  xlab("") +
  ylab(expression(Density~(m^-2))) +
  theme(legend.position="none")
p1 + p2


## ----nb_fit_jags, message=FALSE, cache=TRUE------------------------------------------------
## define negative binomial model in JAGS
cat("

data {
  N <- dim(stars_wide);
}

model {
  ## PRIORS
  r_bef ~ dnorm(0,0.01) T(0,);
  r_aft ~ dnorm(0,0.01) T(0,);
  lambda_bef ~ dnorm(0,0.1) T(0,);
  lambda_aft ~ dnorm(0,0.1) T(0,);
  ## DERIVED PARAMS
  mu_bef <- lambda_bef * area; 
  mu_aft <- lambda_aft * area; 
  p_aft <- r_aft / (r_aft + mu_aft);
  p_bef <- r_bef / (r_bef + mu_bef);
  mean_bef <- r_bef * (1 - p_bef) / p_bef;
  mean_aft <- r_aft * (1 - p_aft) / p_aft;
  var_bef <- mean_bef / p_bef;
  var_aft <- mean_aft / p_aft;
  ## LIKELIHOOD
  for(i in 1:N[1]) {
    stars_wide[i,1] ~ dnegbin(p_aft, r_aft);
    stars_wide[i,2] ~ dnegbin(p_bef, r_bef);
  }
}

", file="negbin_glm.txt") ## end model description

## update model params for JAGS to return
par_jags <- c("lambda_bef","mean_bef","var_bef","lambda_aft","mean_aft","var_aft")

## update list of model info for JAGS
mod_jags$model.file <-"negbin_glm.txt"
mod_jags$parameters.to.save <- par_jags

## fit model
nb_fit_jags <- do.call(jags.parallel, mod_jags)


## ------------------------------------------------------------------------------------------
print(nb_fit_jags)


## ----plot_nb_fit_jags, fig.height=5, fig.width=8, message=FALSE----------------------------
## gather posteriors samples
pdat <- data.frame(Time = rep(c("before","after"), ea=mcmc_samp),
                   samples = c(nb_fit_jags$BUGSoutput$sims.list$lambda_bef,
                               nb_fit_jags$BUGSoutput$sims.list$lambda_aft))
## summary of posteriors
pdat2 <- pdat %>%
  group_by(Time) %>%
  summarize(lo = quantile(samples, 0.025),
            med = quantile(samples, 0.5),
            hi = quantile(samples, 0.975))
## trim away big values for histogram
pdat <- pdat %>%
  filter(samples <= 2)
## histogram of posteriors
p1 <- ggplot(pdat, aes(samples, color = Time)) +
  theme_bw() +
  geom_freqpoly(bins = 50) +
  guides(color = guide_legend(reverse=TRUE)) +
  labs(x = expression(Density~(m^-2)), y = "Count")
p2 <- ggplot(pdat2, aes(x=Time, y=med, color=Time)) +
  theme_bw() +
  geom_point(aes(size=1.5)) +
  geom_errorbar(aes(ymin=lo, ymax=hi), width=0) +
  scale_x_discrete(limits=c("before","after")) +
  labs(x = "", y = expression(Density~(m^-2))) +
  theme(legend.position="none")
p1 + p2


## ----ex_waffle_ghg, fig.height = 8, fig.width = 5------------------------------------------
library(hrbrthemes)
library(waffle)

## data frame of GHG data
ghg <- tibble(
  parts = as.factor(c("CO2 - fossil fuels", "CO2 - land use", "CO2 - chemicals",
                      "Methane", "Nitrous oxide", "Flourinated gases",
                      "Electricity", "Food & land use", "Transportation",
                      "Industry", "Buildings", "Other energy",
                      "Increase in atmosphere", "Land-based sink",
                      "Ocean-based sink")),
  values = c(62, 11, 3, 16, 6, 2, 25, 24, 14, 21, 6, 10, 45, 32, 23),
  category = c(rep("Greenhouse gas emissions", 6),
               rep("Greenhouse gas sources", 6),
               rep("Fate of CO2 emissions", 3))
)

## Greenhouse gas emissions
ghg_1 <- filter(ghg, category == "Greenhouse gas emissions")
ghg_p1 <- ggplot(ghg_1,
                 aes(fill=parts, values=values)) +
  geom_waffle(color = "white", size = 0.5, n_rows = 10) +
  scale_x_discrete(expand=c(0,0)) +
  scale_y_discrete(expand=c(0,0)) +
  ggthemes::scale_fill_tableau(name=NULL,
                               palette = "Miller Stone",
                               type = "regular",
                               direction = 1,
                               breaks = ghg_1$parts) +
  coord_equal() +
  labs(
    title = unique(ghg_1$category)
  ) +
  theme_ipsum_rc(grid="") +
  theme_enhance_waffle()

## Greenhouse gas sources
ghg_2 <- filter(ghg, category == "Greenhouse gas sources")
ghg_p2 <- ggplot(ghg_2,
                 aes(fill=parts, values=values)) +
  geom_waffle(color = "white", size = 0.5, n_rows = 10) +
  scale_x_discrete(expand=c(0,0)) +
  scale_y_discrete(expand=c(0,0)) +
  ggthemes::scale_fill_tableau(name=NULL,
                               palette = "Miller Stone",
                               type = "regular",
                               direction = 1,
                               breaks = ghg_2$parts) +
  coord_equal() +
  labs(
    title = unique(ghg_2$category)
  ) +
  theme_ipsum_rc(grid="") +
  theme_enhance_waffle()

## Fate of CO2 emissions
ghg_3 <- filter(ghg, category == "Fate of CO2 emissions")
ghg_p3 <- ggplot(ghg_3,
                 aes(fill=parts, values=values)) +
  geom_waffle(color = "white", size = 0.5, n_rows = 10) +
  scale_x_discrete(expand=c(0,0)) +
  scale_y_discrete(expand=c(0,0)) +
  ggthemes::scale_fill_tableau(name = NULL,
                               palette = "Color Blind",
                               type = "regular",
                               direction = -1,
                               breaks = ghg_3$parts) +
  coord_equal() +
  labs(
    title = unique(ghg_3$category)
  ) +
  theme_ipsum_rc(grid="") +
  theme_enhance_waffle()

## stick the plots together
iron(ghg_p1, ghg_p2, ghg_p3)

