knitr::purl(“lec_10_getting_data.Rmd”)

## first & last years of flow data
yr_frst <- 2001
yr_last <- 2020

## flow gauge ID
flow_site <- 12178000

## get URL for flow data from USGS
flow_url <- paste0("https://waterdata.usgs.gov/nwis/dv",
                   "?cb_00060=on",
                   "&format=rdb",
                   "&site_no=",flow_site,
                   "&begin_date=",yr_frst,"-01-01",
                   "&end_date=",yr_last,"-12-31")

## load `readr` package
library("readr")

## raw flow data from USGS
flow_raw <- read_lines(flow_url)

## lines with metadata
hdr_flow <- which(lapply(flow_raw, grep, pattern = "\\#")==1, arr.ind = TRUE)

## print flow metadata
print(flow_raw[hdr_flow], quote = FALSE)

## flow data for years of interest
dat_flow <-  read_tsv(flow_url,
                      col_names = FALSE,
                      col_types = "ciDdc",
                      skip = max(hdr_flow)+2) ##skip the first few lines, plus 2

colnames(dat_flow) <- unlist(strsplit(tolower(flow_raw[max(hdr_flow)+1]),
                                      split = "\\s+"))
head(dat_flow)


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
