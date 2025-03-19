#Use latest codepoint open postcode geodata to geocode to UK local authority boundaries
#Get codepoint open from here:
#https://osdatahub.os.uk/downloads/open/CodePointOpen

#Local authorities boundaries from here:
#https://geoportal.statistics.gov.uk/datasets/ons::local-authority-districts-may-2024-boundaries-uk-bfc-2/about
#("full resolution clipped to coastline")
library(tidyverse)
library(sf)
library(pryr)


#Local authorities
la <- st_read("../../../MapPolygons/UK/2024/Local_Authority_Districts_May_2024_Boundaries_UK_BFC/LAD_MAY_2024_UK_BFC.shp")

#itl2 zones
itl2 <- st_read("../RegionalEcons_web/data/ITL_geographies/International_Territorial_Level_2_January_2021_UK_BFE_V2_2022_-4735199360818908762/ITL2_JAN_2021_UK_BFE_V2.shp")



#Make LA / ITL2 lookup from those two
la.itl2 <- intersect_makelookup(larger_zone = itl2, smaller_zone = la, vartogroupby_fromsmallerzone = LAD24CD)

#Reduce columns
la.itl2 <- la.itl2 %>% 
  select(ITL221CD,ITL221NM,LAD24CD,LAD24NM)



#Latest codepoint open (Nov 2024) stored locally
#Postcode areas all in separate files - combine into one
filenames <- list.files("local/codepo_gb/Data/CSV", '*.csv', full.names = T)

pc <- map(filenames, read_csv, col_names = F) %>% bind_rows()

#Get column names
cn <- read_csv("local/codepo_gb/Doc/Code-Point_Open_Column_Headers.csv") %>% slice(1) %>% as.character()

colnames(pc) <- cn

#It already has local authorities in - "Admin_district_code")
#But one not matching... check
table(unique(pc$Admin_district_code) %in% la$LAD24CD)

table(la$LAD24CD %in% pc$Admin_district_code)

#Which differ? Oh it's just the NA...?
unique(pc$Admin_district_code)[!unique(pc$Admin_district_code) %in% la$LAD24CD]

#Others in the LA file will be different countries right? Yep all NI
la$LAD24NM[!la$LAD24CD %in% pc$Admin_district_code]

#Add local authority names in
chk <- pc %>%
  rename(localauthority_code = Admin_district_code) %>% 
  left_join(
    la.itl2 %>% st_set_geometry(NULL) %>% select(localauthority_code = LAD24CD,localauthority_name = LAD24NM, ITL221CD,ITL221NM),
    by = c('localauthority_code')
  ) %>% 
  relocate(localauthority_name, .after = localauthority_code) %>% 
  relocate(Admin_ward_code, .after = Northings) %>% 
  select(-c(NHS_regional_HA_code,Admin_county_code))
  
#chk %>% filter(is.na(localauthority_code)) %>% View

#Any NAs were already in the orig data...
table(is.na(chk$localauthority_name))
table(is.na(chk$NHS_HA_code))

#A few postcodes with eastings and northings we could in theory intersect...
#But only 58 postcodes
table(chk$Eastings != 0, is.na(chk$NHS_HA_code))

#drop those
chk <- chk %>% filter(!is.na(ITL221CD))

#Save locally - a tad too big for github
#p.s. instructions here for removing large files from commit history
#https://docs.github.com/en/repositories/working-with-files/managing-large-files/about-large-files-on-github
#Dropbox download:
#https://www.dropbox.com/scl/fi/iaa3vli7zxl7b93aekyyh/postcode_localauthority_itl2_lookup_2025-03-19.csv?rlkey=jp5v00hm1erfsi20ft0ggp4jk&dl=1
write_csv(chk, paste0('local/postcode_localauthority_itl2_lookup_',Sys.Date(),'.csv'))










