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
    la %>% st_set_geometry(NULL) %>% select(localauthority_code = LAD24CD,localauthority_name = LAD24NM),
    by = c('localauthority_code')
  ) %>% 
  relocate(localauthority_name, .after = localauthority_code)


#Save locally - a tad too big for github
#p.s. instructions here for removing large files from commit history
#https://docs.github.com/en/repositories/working-with-files/managing-large-files/about-large-files-on-github
#Dropbox download:
#https://www.dropbox.com/scl/fi/0873bsx2ka1d3clhs8e07/postcode_localauthority_lookup_2024-11-20.csv?rlkey=rtda1srm2nlzmqxc2ms6fllno&dl=1
write_csv(chk, paste0('local/postcode_localauthority_lookup_',Sys.Date(),'.csv'))










