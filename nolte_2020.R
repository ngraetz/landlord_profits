library(data.table)
library(ggplot2)
library(sf)
library(raster)
library(tigris)
library(tidycensus)
source('C:/Users/ncgra/Dropbox/Penn/repos/covid/map_template.R')
Sys.setenv(CENSUS_API_KEY='ecae108da10a8741591e6bdda9aee2c1cb438538')

################################################################################################################################################################################
## Load land values from Nolte 2020
################################################################################################################################################################################

values <- raster("C:/Users/ncgra/Downloads/doi_10.5061_dryad.np5hqbzq9__v4/places_fmv_pnas_dryad/1 estimates/places_fmv_all.tif")
all_counties <- counties(class='sf')
values <- projectRaster(values, crs = crs(all_counties))
# philly_values_dt <- as.data.frame(philly_values, xy=TRUE)
# ggplot() +
#   geom_raster(data = philly_values_dt,
#               aes(x = x, y = y,
#                   fill = places_fmv_all)) +
#   scale_fill_viridis_c(name = "Value/ha",option='plasma') + 
#   coord_quickmap()

################################################################################################################################################################################
## At tract level, estimate (1) total market value of all land in tract and (2) total rents collected across the whole tract (renting households * median rent). 
################################################################################################################################################################################

process_county <- function(county) {
  
  message(county)
  
  ## First, get average value per ha
  this_state <- substr(county,1,2)
  this_county <- substr(county,3,5)
  philly_tracts <- tracts(class='sf', state=this_state, county=this_county)
  philly_values <- crop(values, philly_tracts)
  vals <- extract(philly_values, philly_tracts)
  vals <- unlist(lapply(vals, mean, na.rm=T))
  
  ## Second, multiply by total ha in each tract to get total market value of all land
  philly_tracts$value_ha <- exp(vals) ## Holte 2020 provides estimates in logged sales price per ha
  philly_tracts$area <- st_area(philly_tracts) ## Area in meters^2 by default
  philly_tracts$value_total <- as.numeric((philly_tracts$value_ha * (philly_tracts$area/10000)) / 1000000) ## Total land value in tract (millions)

  ## Third, query total renters and median rent from tidycensus (also grab percent Black to look at correlations)
  renters <- as.data.table(get_acs(geography='tract',year=2019,table='B07013',state=this_state,county=this_county,cache_table = T))
  renters <- dcast(renters, GEOID ~ variable, value.var='estimate')
  renters <- renters[,c('GEOID','B07013_003')]
  setnames(renters, 'B07013_003', 'renters')
  rent <- as.data.table(get_acs(geography='tract',year=2019,table='DP04',state=this_state,county=this_county,cache_table = T))
  rent <- dcast(rent, GEOID ~ variable, value.var='estimate')
  rent <- rent[,c('GEOID','DP04_0134')]
  setnames(rent, 'DP04_0134', 'median_rent')
  black <- as.data.table(get_acs(geography='tract',year=2019,table='B02001',state=this_state,county=this_county,cache_table = T))
  black <- dcast(black, GEOID ~ variable, value.var='estimate')
  black[, percent_black := B02001_003 / B02001_001]
  black <- black[,c('GEOID','percent_black')]
  pov <- as.data.table(get_acs(geography='tract',year=2019,table='S1701',state=this_state,county=this_county,cache_table = T))
  pov <- dcast(pov, GEOID ~ variable, value.var='estimate')
  pov[, percent_poverty := S1701_C03_001]
  pov <- pov[,c('GEOID','percent_poverty')]
  philly_tracts <- merge(philly_tracts, renters, by='GEOID')
  philly_tracts <- merge(philly_tracts, rent, by='GEOID')
  philly_tracts <- merge(philly_tracts, black, by='GEOID')
  philly_tracts <- merge(philly_tracts, pov, by='GEOID')
  
  ## Last, estimate rent exploitation
  philly_tracts$total_rents <- (philly_tracts$renters * philly_tracts$median_rent) / 1000000
  philly_tracts$exploitation <- philly_tracts$total_rents / philly_tracts$value_total
  this_name <- all_counties[all_counties$STATEFP==this_state & all_counties$COUNTYFP==this_county,]$NAME
  philly_tracts$county_name <- this_name 
  
  return(philly_tracts) 
  
}
#all <- lapply(c('42101','55079','39035','36081','06075','01069'), process_county)
all <- lapply(c('39035','55079','39061','37119','12031','48439'), process_county)
all <- Reduce(rbind, all)
all <- all[!is.na(all$percent_black),]
plot_data <- as.data.table(all)

pdf('C:/Users/ncgra/Dropbox/Penn/repos/acs_wrapper/rhfs/exploitation_plots/rent_exploitation.pdf',width=11,height=6)
ggplot(data=plot_data,
       aes(x=percent_poverty,
           y=exploitation)) + 
  geom_point(aes(size=renters),shape=21) + 
  geom_smooth(method='lm') + 
  facet_wrap(~county_name) + 
  #lims(y=c(0,0.2)) + 
  labs(x='Percent poverty',y='Rent exploitation (total rents / total land value)',size='Renters') + 
  theme_bw()
ggplot(data=plot_data,
       aes(x=percent_black,
           y=exploitation)) + 
  geom_point(aes(size=renters),shape=21) + 
  geom_smooth(method='lm') + 
  facet_wrap(~county_name,ncol=3) + 
  #lims(y=c(0,0.2)) + 
  labs(x='Percent Black',y='Rent exploitation (total rents / total land value)',size='Renters') + 
  theme_bw()
dev.off()

map <- make_map(all[all$COUNTYFP=='079',], vars='exploitation')
#, custom_breaks=list(exploitation=c(0,0.2)
#cor(philly_tracts$percent_black, philly_tracts$exploitation, use='complete.obs')