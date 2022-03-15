library(fusionData)
library(haven)
library(data.table)
library(ggplot2)
library(naniar)
library(mice)
library(survey)
library(labelled)
library(tidyverse)
source("R/utils.R")
source("R/createDictionary.R")
source("R/imputeMissing.R")
source("R/detectDependence.R")
#getSurveyProcessed(survey = "all")

in_dir <- 'C:/Users/ncgra/Dropbox/Penn/repos/acs_wrapper/rhfs/'
out_dir <- 'C:/Users/ncgra/Dropbox/Penn/repos/acs_wrapper/rhfs/'

## Function to apply codebook and keep only those clean extracted variables.
pull_main_wave <- function(y, full_cb, id_vars) {
  message(paste0('Extracting ', y, '...'))
  full_cb <- full_cb[wave==y, ]
  setnames(full_cb, 'wave', 'year')
  missing_vars <- full_cb[!(var %in% names(d)), var_rename]
  message(paste('Missing var:', missing_vars, collapse = '\n'))
  full_cb <- full_cb[var %in% names(d), ]
  ## Recode any raw variables that need recoding.
  for(v in full_cb[, var]) {
    if(!(v %in% names(d))) message(paste0('...', v, ' is missing from main file.'))
    if(v %in% names(d)) {
      v <- gsub(' ','',v)
      ## Keep codebook where we are using this raw variable.
      raw_var_cb <- full_cb[var==v, ]
      ## In case this raw variable is being used to construct multiple clean variables.
      raw_var_cb[, n := 1:.N] 
      ## Loop over and process each clean variable constructed using this raw variable.
      for(i in raw_var_cb[, n]) {
        cb <- raw_var_cb[n==i, ]
        all_vars <- v
        for(sv in all_vars) {
          this_rename <- cb[var==v, var_rename]
          message(paste0('Recoding ', v, ' to ', this_rename,'...'))
          d[, (this_rename) := as.character(get(v))]
          all_vals <- as.numeric(gsub('value_','',names(full_cb)[grepl('value_',names(full_cb))]))
          for(val in all_vals) {
            this_val <- cb[var==v, get(paste0('value_',val))]
            ## Split up multiple values if provided (",") or numeric range ("_").
            if(grepl(',',this_val)) this_val <- unlist(strsplit(this_val, ','))
            handle_seq <- function(x) {
              if(grepl('_',x)) {
                x <- unlist(strsplit(x, '_'))
                x <- as.character(x[1]:x[2])
              }
              return(as.character(x))
            }
            this_val <- unlist(lapply(this_val, handle_seq))
            ## Grab recode value.
            this_recode <- cb[var==v, get(paste0('recode_',val))]
            ## Do the recode.
            if(!is.na(this_val[1])) {
              d[get(sv) %in% this_val, (this_rename) := this_recode] 
            }
          }
          ## After recoding, make numeric if numeric specified.
          if(!is.na(cb[var==v, numeric])) d[, (this_rename) := as.numeric(as.character(get(this_rename)))]
        }
        ## Sum over multiple columns if needed (scales variables).
        if(!is.na(cb[var==v, sum_values])) d[, (this_rename) := rowSums(.SD, na.rm = TRUE), .SDcols = all_vars] 
      }
    }
  }
  ## Return cleaned dataset.
  d[, year := y]
  d <- d[, c(id_vars,'year',full_cb[, var_rename]), with=FALSE]
  return(d)
}

## Load raw data
d <- fread(paste0(in_dir,'rhfspuf2012.csv'))

## Manually add design variables
##  id = CONTROLPUF
##  rep weights = REPWGT1-100
d[, rhfs_2012_hid := CONTROLPUF]
d[, weight := WEIGHT]
#setnames(d, paste0('REPWGT',1:100), paste0('rep_',1:100))

## Load codebook
full_cb <- fread(paste0(in_dir,"rhfs_2018_codebook.csv"))
full_cb[, var := gsub(' ','',var)]

## Extract all variables by wave
all <- rbindlist(lapply(2012, pull_main_wave, full_cb=full_cb, id_vars=c('rhfs_2012_hid','weight')), fill=T)

## Drop commercial properties
all <- all[commerical_use_space!='Yes']

## Replace monthly rent per housing unit with monthly rent per single unit where missing
#all[monthly_rent_housing_unit==0 & !is.na(monthly_rent_single_unit), monthly_rent_housing_unit := monthly_rent_single_unit]

## Section 8 missing means no Section 8

## No capital expenses means 0
all[is.na(total_capital_expenses), total_capital_expenses := 0]

## Multiple imputation
# all <- all[, -c('record_type','commerical_use_space','total_rents')]
# for(v in names(sapply(all, class)[sapply(all, class)=='character'])) all[, (v) := as.factor(get(v))]
# imp <- imputeMissing(data = as.tibble(all),
#                      N = 10,
#                      weight = "weight",
#                      x_exclude = c("rhfs_2012_hid",'expenses_response'))
# imp <- as.data.table(imp)
# for(v in names(imp)) {
#   message(v)
#   all[, (v) := imp[,get(v)]]
# }

## Drop 161 observations where monthly rent per housing unit is 0.
#all <- all[monthly_rent_housing_unit!=0,]

## Rent exploitation = rent receipts / market value (per housing unit)
#all[, rent_exploitation := monthly_rent_housing_unit / market_value_unit]
all[total_rent_collected!=0, rent_exploitation := total_rent_collected / market_value / units_total_topcode]

## Profit margin = (rent receipts - expenses) / rent receipts (per housing unit)
all[, (c('profit','profit_margin')) := NULL]
all[property_taxes!=0 & property_taxes!='300000+', property_taxes_num1 := tstrsplit(property_taxes,'-',keep=1)]
all[, property_taxes_num1 := as.numeric(property_taxes_num1)]
all[property_taxes!=0 & property_taxes!='300000+', property_taxes_num2 := tstrsplit(property_taxes,'-',keep=2)]
all[, property_taxes_num2 := as.numeric(property_taxes_num2)]
all[property_taxes!=0 & property_taxes!='300000+', property_taxes_num := rowMeans(.SD), .SDcols=c('property_taxes_num1','property_taxes_num2')]
all[property_taxes==0, property_taxes_num := 0]
all[property_taxes=='300000+', property_taxes_num := 300000]
all[, total_annual_expenses := utility_expenses + property_taxes_num + total_capital_expenses + management_expenses]
all[total_annual_expenses!=0, profit := total_rent_collected - total_annual_expenses]
all[total_annual_expenses!=0, profit_margin := profit / total_rent_collected]
summary(all[,profit_margin])
# ggplot() + 
#   geom_density(data=all,
#                aes(x=profit_margin)) + 
#   lims(x=c(-2,1)) + 
#   theme_minimal()
# ggplot() + 
#   geom_histogram(data=all,
#                  aes(x=rent_exploitation)) + 
#   lims(x=c(0,0.4)) + 
#   theme_minimal()

## Check weighted medians
des <- svydesign(id=~rhfs_2012_hid, weights=~weight, data=all[!is.na(rent_exploitation),])
svyquantile(~rent_exploitation, des, 0.5)
des <- svydesign(id=~rhfs_2012_hid, weights=~weight, data=all[!is.na(profit),])
svyquantile(~profit_margin, des, 0.5)

## Apply variable labels (optional but needed for fusionData)

## Save processed survey data by wave
saveRDS(all, paste0(out_dir,'clean_RHFS_2012.RDS'))
