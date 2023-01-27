
shootings <- rbind(fread("raw_data/export-08b3b8db-71df-47c0-8e18-05b91d3b1729.csv"),
                   fread("raw_data/2018.csv"),
                   fread("raw_data/2019.csv"),
                   fread("raw_data/2016.csv")) |> 
  group_by(`Incident ID`) |> 
  filter(row_number() == 1) |> 
  mutate(date = as.Date(`Incident Date`, "%B %d, %Y")) |> 
  select(id = `Incident ID`,
         state = State,
         city = `City Or County`,
         street = Address,
         deaths = `# Killed`,
         injured = `# Injured`,
         date) |> 
  mutate(street = gsub("block of ", "", street),
         street = gsub(" and ", " ", street))

if(file.exists("temp/geocoded_shootings.rds")){
  l <- readRDS("temp/geocoded_shootings.rds")
  
  shootings <- left_join(shootings, l)
  
  to_geo <- filter(shootings, is.na(lon))[,c(1:7)]
  
  if(nrow(to_geo) > 0){
    to_geo <- cbind(to_geo, geocode(with(to_geo, paste(street, city, state)), output = "more"))[,c(1:12)]
    
    shootings <- rbind(filter(shootings, !is.na(lon)), to_geo)
  }
  
  saveRDS(shootings, "temp/geocoded_shootings.rds")
}else{
  to_geo <- shootings
  
  to_geo <- cbind(to_geo, geocode(with(to_geo, paste(street, city, state)), output = "more"))
  
  saveRDS(to_geo, "temp/geocoded_shootings.rds")
}

##########################
# read coded killings keep only well-coded killings
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
                       GEOID = bg_data_f$GEOID) %>% 
      filter(dist <= 20)
  }else{
    dist <- data.table(dist = double(),
                       date = as.Date(character()),
                       id = integer(),
                       GEOID = character())
  }
  
  return(dist)
}

## this will keep one observation for every block group for every shooting within 20 miles
## over the 2 year-long periods
## loop over every state


assess <- function(s){
  # if(!(file.exists(paste0("temp/bgs_dists_new_", s, ".rds")))){
  library(tigris)
  library(sp)
  library(SearchTrees)
  library(raster)
  library(data.table)
  library(tidyverse)
  ## pull BG shapefiles using tigris package
  bgs <- block_groups(state = s, class = "sp", year = 2019)
  
  centroids <- SpatialPoints(
    data.table(x = as.numeric(bgs@data$INTPTLON), y = as.numeric(bgs@data$INTPTLAT))
  )
  
  
  bg_data <- bgs@data %>%
    mutate_at(vars(INTPTLON, INTPTLAT), as.numeric)
  
  #########################################
  ## loop over every day between Jan 1, 2020, and Election day
  tot <- rbindlist(lapply(seq(as.Date("2020-05-03"), as.Date("2021-05-02"), by="days"), function(d){
    l <- find_closest(bg_data, centroids, d)
  }))
  
  tot2 <- rbindlist(lapply(seq(as.Date("2016-05-08"), as.Date("2017-05-07"), by="days"), function(d){
    l <- find_closest(bg_data, centroids, d)
  }))
  
  tot <- bind_rows(tot, tot2)
  
  saveRDS(tot, paste0("temp/bgs_dists_new_", s, ".rds"))
  # }
}

cl <- makeCluster(8)  
registerDoParallel(cl)

clusterExport(cl, list("find_closest", "assess", "full_set"))

c(parLapply(cl, unique(filter(fips_codes, state_code <= 56)$state_code), fun = assess))

files <- list.files(path = "temp/", pattern = "^bgs_dists_new_*", full.names = T)

all_bgs <- rbindlist(lapply(files, readRDS))

saveRDS(all_bgs, "temp/dists_long_new.rds")

####################
####################
####################
####################
####################


full_set <- readRDS("temp/geocoded_shootings.rds") %>% 
  ungroup() %>% 
  mutate(score = ifelse(is.na(score), 100, as.numeric(score))) %>% 
  filter(score > 95,
         (date >= "2016-05-08" & date <= "2017-05-07") |
           (date >= "2020-05-03" & date <= "2021-05-02")) %>% 
  select(id2, latitude, longitude)


cents <- rbindlist(lapply(unique(filter(fips_codes, state_code <= 56)$state_code), function(s){
  print(s)

  bgs <- block_groups(state = s, class = "sp", year = 2019)
  
  return(data.table(GEOID = bgs@data$GEOID,
                    long = as.numeric(bgs@data$INTPTLON),
                    lat = as.numeric(bgs@data$INTPTLAT)))
}))


ds <- rbindlist(lapply(c(1:nrow(full_set)), function(k){
  
  cents$dist <- pointDistance(select(cents, long, lat),
                              cbind(full_set$longitude[k], full_set$latitude[k]), lonlat = T) * 0.000621371
  
  return(filter(cents, dist <= 20) %>% 
           mutate(k_id = full_set$id2[k]))
  
}))

cvap20 <- fread("../regular_data/CVAP_2015-2019_ACS_csv_files/BlockGr.csv") %>%
  filter(lntitle == "Total") %>%
  mutate(GEOID = substring(geoid, 8)) %>%
  select(GEOID, cvap = cvap_est)

ds <- left_join(ds, cvap20)

ll <- ds %>% 
  group_by(k_id) %>% 
  summarize(count_5 = sum(dist <= 5),
            pop_5 = sum((cvap * (dist <= 5))),
            count_1 = sum(dist <= 1),
            pop_1 = sum((cvap * (dist <= 1))),
            count_half = sum(dist <= .5),
            pop_half = sum((cvap * (dist <= .5))))

mean(ll$count_1)

l2 <- ds %>% 
  filter(dist <= 1) %>% 
  group_by(GEOID) %>% 
  filter(row_number() == 1)

########################

j <- cvap20 %>% 
  mutate(t = GEOID %in% filter(ds, dist < 10)$GEOID)
weighted.mean(j$t, j$cvap)

j <- cvap20 %>% 
  mutate(t = GEOID %in% filter(ds, dist < 3)$GEOID)
weighted.mean(j$t, j$cvap)

j <- cvap20 %>% 
  mutate(t = GEOID %in% filter(ds, dist < 1)$GEOID)
weighted.mean(j$t, j$cvap)

#######################

counties <- counties(class = "sp", year = 2019)

pings  <- SpatialPoints(full_set[,c('longitude','latitude')], proj4string = counties@proj4string)
full_set$county <- over(pings, counties)$GEOID

cvap20 <- fread("../regular_data/CVAP_2015-2019_ACS_csv_files/County.csv") %>%
  filter(lntitle == "Total") %>%
  mutate(GEOID = substring(geoid, 8)) %>%
  select(GEOID, cvap = cvap_est)

j <- cvap20 %>% 
  mutate(t = GEOID %in% full_set$county)
weighted.mean(j$t, j$cvap)
