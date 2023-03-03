

full_set <- readRDS("temp/geocoded_shootings.rds") |> 
  rename(longitude = lon,
         latitude = lat,
         id2 = id) |> 
  ungroup()

## create function to find closest killing on any given day
find_closest <- function(bg_data_f, centroids_f, d){
  d = as.Date(d)
  
  ## keep killings occuring on that day
  sites <- filter(full_set,
                  date == d) %>%
    mutate(id = row_number())
  
  if(nrow(sites) > 0){ #if there were any killings on that day, do the following:
    ## create tree of those killings
    tree <- createTree(coordinates(dplyr::select(sites, x = longitude, y = latitude)))
    
    ## find closest killing to each point in centroids_f
    inds <- knnLookup(tree , newdat = coordinates(centroids_f), k = 1)
    
    ## combine killing info and BG info
    bg_data_f <- left_join(cbind(bg_data_f, inds),
                           dplyr::select(sites, id, longitude, latitude, date, id2),
                           by = c("inds" = "id"))
    
    ## calculate distance between BG and killing, keep those within 20 miles
    dist <- data.table(dist = pointDistance(dplyr::select(bg_data_f, INTPTLON, INTPTLAT),
                                            dplyr::select(bg_data_f, longitude, latitude), lonlat = T) * 0.000621371,
                       date = bg_data_f$date,
                       id = bg_data_f$id2,
                       GEOID = bg_data_f$GEOID,
                       share_dem = bg_data_f$G20PREDBID / (bg_data_f$G20PREDBID + bg_data_f$G20PRERTRU)) %>% 
      filter(dist <= 20)
  }else{
    dist <- data.table(dist = double(),
                       date = as.Date(character()),
                       id = integer(),
                       GEOID = character(),
                       share_dem = double())
  }
  
  return(dist)
}

## this will keep one observation for every block group for every shooting within 20 miles
## over the 2 year-long periods
## loop over every state

assess_2020 <- function(s){
  # if(!(file.exists(paste0("temp/bgs_dists_new_", s, ".rds")))){
  library(tigris)
  library(rgdal)
  library(sf)
  library(sp)
  library(rgeos)
  library(SearchTrees)
  library(raster)
  library(data.table)
  library(tidyverse)
  
  state <- substring(list.files(s)[1], 1, 2)
  print(state)
  ## pull BG shapefiles using tigris package
  bgs <- readOGR(dsn = s, layer = substring(list.files(s)[1], 1, nchar(list.files(s)[1]) - 4))
  bgs <- spTransform(bgs, CRS("+proj=longlat +datum=NAD83 +no_defs"))
  
  centroids <- SpatialPoints(
    gCentroid(bgs, byid = TRUE)@coords
  )
  
  
  bgs <- cbind(bgs@data,
               gCentroid(bgs, byid = TRUE)@coords) |> 
    rename(INTPTLON = x,
           INTPTLAT = y) |> 
    mutate(GEOID = paste0(state, row_number()),
           across(c(G20PREDBID, G20PRERTRU), as.numeric))
  
  #########################################
  ## loop over every day between Jan 1, 2020, and Election day
  tot <- rbindlist(lapply(seq(as.Date("2020-05-03"), as.Date("2021-05-02"), by="days"), function(d){
    l <- find_closest(bgs, centroids, d)
  }))
  
  saveRDS(tot, paste0("temp/precincts_dists_new_", state, ".rds"))
  # }
}

assess_2016 <- function(s){
  
  library(tigris)
  library(rgdal)
  library(sf)
  library(sp)
  library(rgeos)
  library(SearchTrees)
  library(raster)
  library(data.table)
  library(tidyverse)
  
  state <- substring(list.files(s)[1], 1, 2)
  #if(!(file.exists(paste0("temp/precincts_16_dists_new_", state, ".rds")))){
    
  print(state)
  ## pull BG shapefiles using tigris package
  bgs <- readOGR(dsn = s, layer = substring(list.files(s)[1], 1, nchar(list.files(s)[1]) - 4))
  bgs <- spTransform(bgs, CRS("+proj=longlat +datum=NAD83 +no_defs"))
  
  centroids <- SpatialPoints(
    gCentroid(bgs, byid = TRUE)@coords
  )
  
  colnames(bgs@data) <- toupper(colnames(bgs@data))
  
  bgs <- cbind(bgs@data,
               gCentroid(bgs, byid = TRUE)@coords) |> 
    rename(INTPTLON = x,
           INTPTLAT = y) |> 
    mutate(GEOID = paste0(state, row_number()),
           across(c(G16PREDCLI, G16PRERTRU), as.numeric))
  
  #########################################
  ## loop over every day between Jan 1, 2020, and Election day
  tot <- rbindlist(lapply(seq(as.Date("2016-05-08"), as.Date("2017-05-07"), by="days"), function(d){
    l <- find_closest(bgs, centroids, d)
  }))
  
  saveRDS(tot, paste0("temp/precincts_16_dists_new_", state, ".rds"))
  #}
}

cl <- makeCluster(8)  
registerDoParallel(cl)

clusterExport(cl, list("find_closest", "assess_2020", "assess_2016", "full_set"))

runs <- list.dirs("../regular_data/vest/vest_2020")[2:length(list.dirs("../regular_data/vest/vest_2020"))][c(1:32, 34:52)]

c(parLapply(cl, runs,
            fun = assess_2020))

runs <- list.dirs("../regular_data/vest/vest_2016/2016")[2:length(list.dirs("../regular_data/vest/vest_2016/2016"))]
c(parLapply(cl, runs,
            fun = assess_2016))

files <- c(list.files(path = "temp/", pattern = "^precincts_dists_new_*", full.names = T),
           list.files(path = "temp/", pattern = "^precincts_16_dists_new_*", full.names = T))

all_bgs <- rbindlist(lapply(files, readRDS))

saveRDS(all_bgs, "temp/precinct_demshare.rds")

############################################################

b2p <- function(state){
  
  library(tigris)
  library(rgdal)
  library(sf)
  library(sp)
  library(rgeos)
  library(SearchTrees)
  library(raster)
  library(data.table)
  library(tidyverse)
  
  #if(!(file.exists(paste0("temp/p2b_", state, ".rds")))){
    bgs <- tigris::blocks(state = state, class = "sp", year = 2019)
    
    bgs@data <- bgs@data |> 
      mutate(across(c('INTPTLON10','INTPTLAT10'), as.numeric))
    
    
    pct <- readOGR(paste0("../regular_data/vest/vest_2016/2016/", tolower(state), "_2016"),
                   paste0(tolower(state), "_2016"))
    pct <- spTransform(pct, CRS("+proj=longlat +datum=NAD83 +no_defs"))
    
    pct@data$GEOID <- paste0(state, c(1:nrow(pct@data)))
    
    pings  <- SpatialPoints(bgs@data[,c('INTPTLON10','INTPTLAT10')], proj4string = pct@proj4string)
    bgs$precinct <- over(pings, pct)$GEOID
    
    h16 <- bgs@data |> 
      mutate(state = state,
             year = 2016)
    
    if(state == "KY"){
      
      pct <- readOGR(paste0("../regular_data/vest/vest_2020/", tolower(state), "_2020_vtd_estimates"),
                     paste0(tolower(state), "_2020_vtd_estimates"))
    }else{
      pct <- readOGR(paste0("../regular_data/vest/vest_2020/", tolower(state), "_2020"),
                     paste0(tolower(state), "_2020"))
    }
    pct <- spTransform(pct, CRS("+proj=longlat +datum=NAD83 +no_defs"))
    
    pct@data$GEOID <- paste0(state, c(1:nrow(pct@data)))
    pings  <- SpatialPoints(bgs@data[,c('INTPTLON10','INTPTLAT10')], proj4string = pct@proj4string)
    bgs$precinct <- over(pings, pct)$GEOID
    
    h20 <- bgs@data |> 
      mutate(state = state,
             year = 2020)
    
    saveRDS(bind_rows(h16, h20),
            paste0("temp/p2b_", state, ".rds"))
  #}
}

cl <- makeCluster(8)  
registerDoParallel(cl)

clusterExport(cl, list("b2p"))

states <- unique(filter(fips_codes, state_code <= "56")$state)

c(parLapply(cl, states,
            fun = b2p))


files <- list.files(path = "temp/", pattern = "^p2b*", full.names = T)

all_p2b <- rbindlist(lapply(files, readRDS)) |> 
  select(GEOID10, precinct, state, year)

saveRDS(all_p2b, "temp/precinct_block.rds")
