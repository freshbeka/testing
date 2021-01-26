## ----setup, include=FALSE------------------------------------------------
knitr::opts_chunk$set(echo = TRUE, 
                      fig.align = "center", out.width = '90%')


## ----setwd_ex, eval=FALSE------------------------------------------------
## setwd("~/Users/Mark/Dcouments/folder/that/only/Mark/has")


## ----setwd_relative, eval=FALSE------------------------------------------
## setwd("~/data/")


## ----chk_wd, eval=FALSE--------------------------------------------------
## getwd()


## ----chk_dir_files-------------------------------------------------------
## what is the working directory?
getwd()

## what files exist in the working directory?
list.files()
dir()


## ----here_ex-------------------------------------------------------------
## load here package
library(here)

## show files in base directory
list.files(here())

## show files in references directory
list.files(here("references"))


## ------------------------------------------------------------------------
here("data", "palmer_penguins.csv")


## ----here_ex_save, eval = FALSE------------------------------------------
## ## set analysis directory
## analysis_dir <- here("analysis")
## 
## ## check if the `/analysis/cache` folder exists
## ## if not, create it
## if(!file.exists(here("analysis_dir", "cache"))) {
##   dir.create(here("analysis_dir", "cache"))
## }
## 
## ## check for saved file named `fit_ipm_JAGS.rds`
## ## if it doesn't exists, fit the model and save it
## if(!file.exists(here("analysis_dir", "cache", "fit_ipm_JAGS.rds"))) {
##   ## fit the model
##   fit_ipm_JAGS <- rjags(...)
##   ## save the model fit
##   saveRDS(fit_ipm_JAGS, here("analysis_dir", "cache", "fit_ipm_JAGS.rds"))
## }


## ----get_flow_user_inputs------------------------------------------------
## first & last years of flow data
yr_frst <- 2001
yr_last <- 2020

## flow gauge ID
flow_site <- 12178000


## ----get_flow_url--------------------------------------------------------
## get URL for flow data from USGS
flow_url <- paste0("https://waterdata.usgs.gov/nwis/dv",
                   "?cb_00060=on",
                   "&format=rdb",
                   "&site_no=",flow_site,
                   "&begin_date=",yr_frst,"-01-01",
                   "&end_date=",yr_last,"-12-31")


## ----get_flow_metadata---------------------------------------------------
## load `readr` package
library("readr")

## raw flow data from USGS
flow_raw <- read_lines(flow_url)

## lines with metadata
hdr_flow <- which(lapply(flow_raw, grep, pattern = "\\#")==1, arr.ind = TRUE)

## print flow metadata
print(flow_raw[hdr_flow], quote = FALSE)


## ----get_flows, cache=TRUE-----------------------------------------------
## flow data for years of interest
dat_flow <-  read_tsv(flow_url,
                      col_names = FALSE,
                      col_types = "ciDdc",
                      skip = max(hdr_flow)+2)

colnames(dat_flow) <- unlist(strsplit(tolower(flow_raw[max(hdr_flow)+1]),
                                      split = "\\s+"))
head(dat_flow)


## ----trim_dat_flow, cache=TRUE-------------------------------------------
## keep only relevant columns
dat_flow <- dat_flow[c("datetime",
                       grep("[0-9]$", colnames(dat_flow), value = TRUE))]

## nicer column names
colnames(dat_flow) <- c("date","flow")

## convert cubic feet to cubic meters
dat_flow$flow <- dat_flow$flow / 35.3147

## flow by year & month
dat_flow$year <- as.integer(format(dat_flow$date,"%Y"))
dat_flow$month <- as.integer(format(dat_flow$date,"%m"))
dat_flow <- dat_flow[,c("year","month","flow")]

## inspect the file
head(dat_flow)


## ----ex_readtable, eval = FALSE------------------------------------------
## raw_data <- read.table("flat_file.txt")


## ----here_ex_read_base---------------------------------------------------
## read penguin data
data_raw <- read.csv(here("lectures", "week_04", "data", "palmer_penguins.csv"))

## inspect its content
head(data_raw)


## ----here_ex_readr-------------------------------------------------------
## read penguin data
data_raw <- read_csv(here("lectures", "week_04", "data", "palmer_penguins.csv"))

## inspect its content
head(data_raw)


## ----examine_wb_file, warning = FALSE------------------------------------
## load readxl
library(readxl)

## inpect the sheet names
excel_sheets("data/world_bank_climate_change.xls")


## ----read_wb_sheet, warning = FALSE--------------------------------------
## read in the "Data" worksheet
cc_data <- read_excel("data/world_bank_climate_change.xls", sheet = "Data")

## inspect it
head(cc_data)


## ------------------------------------------------------------------------
cc_data <- read_excel("data/world_bank_climate_change.xls",
                      sheet     = "Data",
                      range     = "G481:L498",
                      col_names = as.character(c(1990:1995)),
                      na        = "..")
head(cc_data)


## ----googlesheet_setup---------------------------------------------------
## load the packge
library(googlesheets4)

## authorize for reading public-only sheets
gs4_deauth()


## ----googlesheet_read----------------------------------------------------
## specify URL
gs_url <- "https://docs.google.com/spreadsheets/d/1ct3MMMzEX82BeqJcUw3uYD5b4CJrKUmRYCK-wVA3yig/edit#gid=0"

## read from URL
read_sheet(gs_url)

## assign it to an object
gs_raw <- read_sheet(gs_url)

## specify ID
gs_id <- as_sheets_id(gs_url)

## read from ID
read_sheet(gs_id)

## assign it to an object
gs_raw <- read_sheet(gs_id)


## ----gs_browse-----------------------------------------------------------
## browse sheet contents
gs4_get(gs_id)


## ------------------------------------------------------------------------
read_sheet(gs_id, range = "A1:B10")


## var mark = {

##   "city" : "Seattle",

##   "state" : "WA",

##   "hobbies" : {

##     "hobby1" : "cycling",

##     "hobby2" : "skiing"

##   }

##   "bikes" : {

##     "bike1" : "Ridley Helium",

##     "bike2" : "Niner RLT",

##     "bike3" : "Santa Cruz Tallboy"

##   }

##   "skis" : {

##     "skis1" : "K2 Hardside",

##     "skis2" : "DPS Wailer"

##   }

## }


## ----json_github_ex------------------------------------------------------
## load the pckage
library(jsonlite)

## set the URL
github_url = "https://api.github.com/users/mdscheuerell/repos"

## get the JSON data
data_json <- fromJSON(github_url)


## ----json_names----------------------------------------------------------
## all of the JSON names
names(data_json)


## ----gh_count_forks------------------------------------------------------
## table of the number of forks
table(data_json$forks)


## ---- eval = FALSE-------------------------------------------------------
## ## table number of the different languages
## table(data_json$language)


## ----gh_open_issues------------------------------------------------------
## how many repos have open issues? 
table(data_json$open_issues_count)

