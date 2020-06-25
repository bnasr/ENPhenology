# this scripts read the original flux_sites.csv file and add the additional meteorological data

library(data.table)
library(lubridate)
library(daymetr)

fluxsites <- setDT(read.csv('data/flux_sites.csv'))
fluxsites[,.(Lat, Long)]

for(i in 1:nrow(fluxsites)){
  dm <- download_daymet(site = fluxsites$FLUXNET.site.code[i],
                        lat = fluxsites$Lat[i],
                        lon = fluxsites$Long[i], start = 1981, end = 2010)
  dt <- setDT(dm$data)
  dt[, date := as.Date(yday - 1, origin = paste0(year, "-01-01"))]
  dt[, month := month(date)]
  fluxsites$Mean.January.temperature[i] <- dt[month==1, mean((tmin..deg.c. + tmax..deg.c.) / 2)]
  fluxsites$Mean.July.temperature[i] <- dt[month==7, mean((tmin..deg.c. + tmax..deg.c.) / 2)]
}

fluxsites[, Lat := round(Lat, digits = 4)]
fluxsites[, Long := round(Long, digits = 4)]
fluxsites[, Elev := floor(Elev)]
fluxsites[, Mean.annual.precipitation := floor(Mean.annual.precipitation)]
fluxsites[, Mean.annual.temperature := round(Mean.annual.temperature, digits = 1)]
fluxsites[, Mean.January.temperature := round(Mean.January.temperature, digits = 1)]
fluxsites[, Mean.July.temperature := round(Mean.July.temperature, digits = 1)]


fluxsites$snowy.days = 0

for(i in 1:nrow(fluxsites)){
  snow_flags <- paste0('data/snow_flags/snow_flag_', fluxsites$FLUXNET.site.code[i], '.csv')
  snow <- setDT(read.csv(snow_flags, skip = 1))
  snow[is.nan(snow), snow:=NA]
  snow[, date := sprintf('%04d-%02d-%02d', Year, Month, Day)]
  snow[, doy := yday(date)]
  fluxsites$snowy.days[i] = snow[,mean(snow, na.rm = T),doy][,floor(sum(V1))]
}

write.csv(fluxsites[,.(
                       'FLUXNET Code' = FLUXNET.site.code, 
                       'PhenoCam' = PhenoCam.site.name,
                       'Site Description' = Site.description..location, 
                       'Latitude (°)' = Lat, 
                       'Longitude (°)' = Long, 
                       'Altitude (m)' = Elev, 
                       'MAT (°C)' = Mean.annual.temperature,
                       'MAP (mm)' = Mean.annual.precipitation, 
                       'Mean January Temperature (°C)' = Mean.January.temperature, 
                       'Mean July Temperature (°C)' = Mean.July.temperature, 
                       'Snow Days' = snowy.days,
                       'Dominant Species' = Dominant.species,
                       DOI = doi)], 
          file = 'data/flux_sites_updated.csv', row.names = F)
