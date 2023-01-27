## these are helper functions to grab commonly desired census data
## at the bottom is a function that calls all the others


pop_dens <-  function(geo, year, state = NULL, county = NULL){
  library(tigris)
  library(tidyverse)
  library(tidycensus)
  if(is.null(state)){
    pd <- rbindlist(lapply(unique(fips_codes[as.numeric(fips_codes$state_code) < 60, ]$state), function(state){
      if(geo == "block group"){
        area <- dplyr::select(tigris::block_groups(state = state, county = county), GEOID, area = ALAND) %>%
          mutate(area = as.numeric(area) * 0.00000038610)
      }else if(geo == "tract"){
        area <- dplyr::select(tigris::tracts(state = state, county = county), GEOID, area = ALAND) %>%
          mutate(area = as.numeric(area) * 0.00000038610)
      }else if(geo == "county"){
        area <- dplyr::select(tigris::tracts(state = state, county = county), GEOID, area = ALAND) %>%
          mutate(area = as.numeric(area) * 0.00000038610,
                 GEOID = substring(GEOID, 1, 5)) %>%
          group_by(GEOID) %>%
          summarize(area = sum(area))
      }else if(geo == "county subdivision"){
        area <- dplyr::select(tigris::county_subdivisions(state = state, county = county), GEOID, area = ALAND) %>%
          mutate(area = as.numeric(area) * 0.00000038610) %>%
          group_by(GEOID) %>%
          summarize(area = sum(area))
      }else if(geo == "place"){
        area <- dplyr::select(tigris::places(state = state), GEOID, area = ALAND) %>%
          mutate(area = as.numeric(area) * 0.00000038610) %>%
          group_by(GEOID) %>%
          summarize(area = sum(area))
      }else if(geo == "state"){
        area <- dplyr::select(tigris::tracts(state = state, county = county), GEOID, area = ALAND) %>%
          mutate(area = as.numeric(area) * 0.00000038610,
                 GEOID = substring(GEOID, 1, 2)) %>%
          group_by(GEOID) %>%
          summarize(area = sum(area))
      }
      
      pop <- get_acs(geography = geo,
                     variables = c(population = "B01003_001"),
                     state = state, county = county, year = year) %>%
        dplyr::select(-variable, -moe) %>%
        dplyr::rename(population = estimate)
      
      pd <- inner_join(pop, area) %>%
        mutate(pop_dens = population / area) %>%
        select(GEOID, pop_dens)
      return(pd)
    }))
  }else{
    if(geo == "block group"){
      area <- dplyr::select(tigris::block_groups(state = state, county = county), GEOID, area = ALAND) %>%
        mutate(area = as.numeric(area) * 0.00000038610)
    }else if(geo == "tract"){
      area <- dplyr::select(tigris::tracts(state = state, county = county), GEOID, area = ALAND) %>%
        mutate(area = as.numeric(area) * 0.00000038610)
    }else if(geo == "county"){
      area <- dplyr::select(tigris::tracts(state = state, county = county), GEOID, area = ALAND) %>%
        mutate(area = as.numeric(area) * 0.00000038610,
               GEOID = substring(GEOID, 1, 5)) %>%
        group_by(GEOID) %>%
        summarize(area = sum(area))
    }else if(geo == "county subdivision"){
      area <- dplyr::select(tigris::county_subdivisions(state = state, county = county), GEOID, area = ALAND) %>%
        mutate(area = as.numeric(area) * 0.00000038610) %>%
        group_by(GEOID) %>%
        summarize(area = sum(area))
    }else if(geo == "place"){
      area <- dplyr::select(tigris::places(state = state), GEOID, area = ALAND) %>%
        mutate(area = as.numeric(area) * 0.00000038610) %>%
        group_by(GEOID) %>%
        summarize(area = sum(area))
    }else if(geo == "state"){
      area <- dplyr::select(tigris::tracts(state = state, county = county), GEOID, area = ALAND) %>%
        mutate(area = as.numeric(area) * 0.00000038610,
               GEOID = substring(GEOID, 1, 2)) %>%
        group_by(GEOID) %>%
        summarize(area = sum(area))
    }
    
    pop <- get_acs(geography = geo,
                   variables = c(population = "B01003_001"),
                   state = state, county = county, year = year) %>%
      dplyr::select(-variable, -moe) %>%
      dplyr::rename(population = estimate)
    
    pd <- inner_join(pop, area) %>%
      mutate(pop_dens = population / area) %>%
      select(GEOID, pop_dens)
    return(pd)
  }
  return(pd)
}



census_vap <- function(geo, year, state = NULL, county = NULL){
  library(tidycensus)
  library(tidyverse)
  
  vap <- get_acs(geography = geo,
                 variables = c(ym1 = "B01001_003",
                               ym2 = "B01001_004",
                               ym3 = "B01001_005",
                               ym4 = "B01001_006",
                               yw1 = "B01001_027",
                               yw2 = "B01001_028",
                               yw3 = "B01001_029",
                               yw4 = "B01001_030"),
                 summary_var = "B01001_001", state = state, county = county, year = year) %>%
    dplyr::group_by(GEOID) %>%
    dplyr::summarize(under18 = sum(estimate),
                     pop = mean(summary_est)) %>%
    dplyr::mutate(vap = pop - under18) %>%
    dplyr::select(-under18, -pop) %>%
    dplyr::ungroup()
}

census_race_ethnicity <- function(geo, year, state = NULL, county = NULL){
  library(tidycensus)
  library(tidyverse)
  race_ethnicity <- get_acs(geography = geo,
                            variables = c(nh_white = "B03002_003",
                                          nh_black = "B03002_004",
                                          latino = "B03002_012",
                                          latino_black = "B03002_014",
                                          native_american = "B03002_005",
                                          hawaiian_pac_island = "B03002_007",
                                          asian = "B03002_006"),
                            summary_var = c(population = "B03002_001"),
                            state = state, county = county, year = year) %>%
    dplyr::mutate(estimate = estimate / summary_est) %>%
    dplyr::select(-ends_with("moe")) %>%
    dplyr::rename(population = summary_est) %>%
    spread(variable, estimate)
}

census_income <- function(geo, year, state = NULL, county = NULL){
  library(tidycensus)
  library(tidyverse)
  income <- get_acs(geography = geo,
                    variables = c(medincome = "B19013_001"),
                    state = state, county = county, year = year) %>%
    dplyr::select(-variable, -moe) %>%
    dplyr::rename(median_income = estimate)
}

census_over64 <- function(geo, year, state = NULL, county = NULL){
  library(tidycensus)
  library(tidyverse)
  o64 <- get_acs(geography = geo,
                 variables = c("B01001_020",
                               "B01001_021",
                               "B01001_022",
                               "B01001_023",
                               "B01001_024",
                               "B01001_025",
                               "B01001_044",
                               "B01001_045",
                               "B01001_046",
                               "B01001_047",
                               "B01001_048",
                               "B01001_049"),
                 summary_var = "B01001_001",
                 state = state, county = county, year = year) %>%
    dplyr::group_by(GEOID, NAME) %>%
    dplyr::summarize(share_over_64 = sum(estimate / summary_est))
}

census_under40 <- function(geo, year, state = NULL, county = NULL){
  library(tidycensus)
  library(tidyverse)
  o64 <- get_acs(geography = geo,
                 variables = c("B01001_003",
                               "B01001_004",
                               "B01001_005",
                               "B01001_006",
                               "B01001_007",
                               "B01001_008",
                               "B01001_009",
                               "B01001_010",
                               "B01001_011",
                               "B01001_012",
                               "B01001_013",
                               "B01001_027",
                               "B01001_028",
                               "B01001_029",
                               "B01001_030",
                               "B01001_031",
                               "B01001_032",
                               "B01001_033",
                               "B01001_034",
                               "B01001_035",
                               "B01001_036",
                               "B01001_037"),
                 summary_var = "B01001_001",
                 state = state, county = county, year = year) %>%
    dplyr::group_by(GEOID, NAME) %>%
    dplyr::summarize(share_under40 = sum(estimate / summary_est))
}

census_education <- function(geo, year, state = NULL, county = NULL){
  library(tidycensus)
  library(tidyverse)
  education <- get_acs(geography = geo,
                       variables = c("B15002_012",
                                     "B15002_013",
                                     "B15002_014",
                                     "B15002_015",
                                     "B15002_016",
                                     "B15002_017",
                                     "B15002_018",
                                     "B15002_029",
                                     "B15002_030",
                                     "B15002_031",
                                     "B15002_032",
                                     "B15002_033",
                                     "B15002_034",
                                     "B15002_035"),
                       summary_var = "B15002_001",
                       state = state, county = county, year = year) %>%
    dplyr::group_by(GEOID, NAME) %>%
    dplyr::summarize(some_college = sum(estimate / summary_est))
}

census_unemployment <- function(geo, year, state = NULL, county = NULL){
  library(tidycensus)
  library(tidyverse)
  unemployment <- get_acs(geography = geo,
                          variables = c(lf = "B23025_003",
                                        unem = "B23025_005"),
                          output = "wide",
                          state = state, county = county, year = year) %>%
    dplyr::mutate(unem = unemE / lfE) %>%
    dplyr::select(GEOID, NAME, unem)
}

census_median_age <- function(geo, year, state = NULL, county = NULL){
  library(tidycensus)
  library(tidyverse)
  median_age <- get_acs(geography = geo,
                        variables = c(median_age = "B01002_001"),
                        state = state, county = county, year = year, output = "wide") %>%
    dplyr::select(GEOID, median_age = median_ageE)
}

census_non_citizen <- function(geo, year, state = NULL, county = NULL){
  library(tidycensus)
  library(tidyverse)
  
  non_citizen <- get_acs(geography = geo,
                         variables = c(non_citizen = "B05001_006"),
                         summary_var = "B05001_001",
                         state = state, county = county, year = year) %>%
    dplyr::mutate(share_non_citizen = estimate / summary_est) %>%
    dplyr::select(GEOID, share_non_citizen)
}

census_movers <- function(geo, year, state = NULL, county = NULL){
  library(tidycensus)
  library(tidyverse)
  movers <- get_acs(geography = geo,
                    variables = c(tot = "B07001_001",
                                  not_moved = "B07001_017"),
                    state = state, county = county, year = year, output = "wide") %>%
    dplyr::mutate(share_moved = 1 - (not_movedE / totE)) %>%
    dplyr::select(GEOID, share_moved)
}

census_no_car <- function(geo, year, state = NULL, county = NULL){
  library(tidycensus)
  library(tidyverse)
  no_car <- get_acs(geography = geo,
                    variables = c(tot = "B08201_001",
                                  no_car = "B08201_002"),
                    state = state, county = county, year = year, output = "wide") %>%
    dplyr::mutate(share_no_car = no_carE / totE) %>%
    dplyr::select(GEOID, share_no_car)
}

get_basic_census_stats <- function(geo, year, state = NULL, county = NULL){
  
  vap <- census_vap(geo = geo, state = state, county = county, year = year)
  
  race_ethnicity <- census_race_ethnicity(geo = geo, state = state, county = county, year = year)
  
  income <- census_income(geo = geo, state = state, county = county, year = year)
  
  education <- census_education(geo = geo, state = state, county = county, year = year)
  
  unemployment <- census_unemployment(geo = geo, state = state, county = county, year = year)
  
  median_age <- census_median_age(geo = geo, state = state, county = county, year = year)
  
  non_citizen <- census_non_citizen(geo = geo, state = state, county = county, year = year)
  
  movers <- census_movers(geo = geo, state = state, county = county, year = year)
  
  no_car <- census_no_car(geo = geo, state = state, county = county, year = year)
  
  o64 <- census_over64(geo = geo, state = state, county = county, year = year)
  
  u40 <- census_under40(geo = geo, state = state, county = county, year = year)
  
  pd <- pop_dens(geo = geo, state = state, county = county, year = year)
  
  census <- list(race_ethnicity, income, education, vap,
                 unemployment, median_age, non_citizen,
                 movers, no_car, o64, u40, pd) %>%
    reduce(left_join)
  
  return(census)
}

library(tidycensus)
library(tidyverse)
library(data.table)


for(year in c(2014, 2016, 2018, 2019)){
  sy = as.character(year - 2000)
  cd <- rbindlist(lapply(unique(filter(fips_codes, as.integer(state_code) < 59)$state), function(s){
    jj <- get_basic_census_stats("block group", year, state = s)
  }))
  saveRDS(cd, paste0("temp/census_bgs_", sy, ".rds"))
}

