library(equatiomatic)

## Fit a simple linear regression model
model_fit <- lm(mpg ~ cyl + disp, mtcars)

## Give the results to extract_eq
extract_eq(model_fit)

## raw result
## $$
## \operatorname{mpg} = \alpha + \beta_{1}(\operatorname{cyl}) + \beta_{2}(\operatorname{disp}) + \epsilon
## $$

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


## data directory
data_dir <- here::here("data")
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

## read count data
counts <- read_csv(file.path(data_dir, "transectcounts.csv"))
colnames(counts)



##Waffle plot example ####
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
