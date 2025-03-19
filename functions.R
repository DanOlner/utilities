library(tidyverse)
library(sf)


#Function for intersecting two geographies, finding which has largest area overlap from first
#And using that largest area one as the lookup label
intersect_makelookup <- function(larger_zone, smaller_zone, vartogroupby_fromsmallerzone){
  
  vartogroupby_fromsmallerzone <- enquo(vartogroupby_fromsmallerzone)
  
  cat('Beginning intersect (timing)...\n')  
  x <- Sys.time()
  
  intersect_result <- sf::st_intersection(larger_zone, smaller_zone)
  
  cat('Time: ',Sys.time() - x,'\n')
  
  #Add area to all zones
  #Some smaller zones will be entire within larger zones
  #Those will only have one row
  #Others will have more - the largest area within that group of rows 
  #will be the zone overlap that's the largest
  #We'll keep this one (and report back on the scale of difference)
  intersect_result <- intersect_result %>% 
    mutate(
      overlap_area_m2 = st_area(.) %>% as.numeric
    ) %>% 
    group_by(!!vartogroupby_fromsmallerzone) %>% 
    mutate(
      area_percent = (overlap_area_m2/sum(overlap_area_m2))*100,
      group_count = n()
    ) %>% 
    ungroup() %>% 
    arrange(-group_count)#to get a good view of most overlapping zones
  
  
  #Keep only largest % from each group to larger overlap as label
  keeps <- intersect_result %>% 
    group_by(!!vartogroupby_fromsmallerzone) %>% 
    filter(area_percent == max(area_percent)) %>% 
    ungroup()
  
  #Can drop some smaller zones if not at all inside larger zones...
  # smaller_zone %>% filter(!zone_code %in% keeps$zone_code) %>% View
  
  #Should now have unique zone codes
  # length(unique(keeps$zone_code)) == nrow(keeps)
  
  cat('Lookup single zone picked - percent that are fully inside larger zones:',mean(keeps$area_percent == 100) * 100,'%\n')
  
  return(keeps)
  
}