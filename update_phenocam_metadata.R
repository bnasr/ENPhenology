# this script update phenocam sites metadata

library(phenocamapi)

phenos <- phenocamapi::get_phenos()

study_sites <- readLines('data/modeling_study_sites.csv')    

write.csv(row.names = F, file = 'data/phenocams.csv',
          phenos[site%in%study_sites, .('PhenoCam Site Name' = site, 
                                            'Flux Site Name' = flux_sitenames, 
                                            'Latitude (°)' = round(lat, digits = 4),
                                            'Longitude (°)' = round(lon, digits = 4),
                                            'Altitude (m)' = round(elev, digits = 4),
                                            'MAT (°C)' = MAT_daymet, 
                                            'MAP (mm)' = MAP_daymet,
                                            Acknowledgements = gsub(pattern = '\n|\r|  ', 
                                                                    replacement = '', 
                                                                    site_acknowledgements))])

