#### the 2014 files from L2 are modified slightly before use
## this is because earlier files were not geocoded as well as later ones
## here we present the code that creates the 2014 SQL database, which merges addresses
## missing block group info to addresses from a later file (2018) to get the correct 
## block group information. below, we also show how this sql db is used to construct the number of
## ballots cast by block group that mirrors the other years


files <- list.files("../voter_file_data/national/post_2014", full.names = T, pattern = "*.zip")

db14 <- dbConnect(SQLite(), "../voter_file_data/national_file_post14.db")
db18 <- dbConnect(SQLite(), "../voter_file_data/national_file_post18.db")

for(f in files){
  f2 <- substring(f, 1, nchar(f) - 4)
  state <- substring(f, 37, 38)
  print(state)
  if(!dir.exists(f2)){
    unzip(f, exdir = f2)
  }else{
    print("Already Unzipped")
  }
  
  l <- list.files(f2, full.names = T, pattern = "*.tab")
  k <- fread(l,
             select = c("LALVOTERID", "Voters_FIPS",
                        "Residence_Addresses_AddressLine", 
                        "Residence_Addresses_ExtraAddressLine", 
                        "Residence_Addresses_City", 
                        "Residence_Addresses_State",
                        "Residence_Addresses_CensusTract",
                        "Residence_Addresses_CensusBlockGroup",
                        "Residence_Addresses_CensusBlock",
                        "EthnicGroups_EthnicGroup1Desc",
                        "General_2014-11-04")) %>%
    mutate(state = state,
           across(c(Residence_Addresses_CensusTract,
                    Residence_Addresses_CensusBlockGroup,
                    Residence_Addresses_CensusBlock), as.character))
  
  miss <- filter(k,
                 Residence_Addresses_CensusTract == "" |
                   is.na(Residence_Addresses_CensusTract) | 
                   Residence_Addresses_CensusTract == 0) %>% 
    select(-Residence_Addresses_CensusTract,
                        -Residence_Addresses_CensusBlockGroup,
                        -Residence_Addresses_CensusBlock)
  fine <- filter(k,
                 Residence_Addresses_CensusTract != "" &
                   !is.na(Residence_Addresses_CensusTract) & 
                   Residence_Addresses_CensusTract != 0)
  
  
  new_ads <- dbGetQuery(db18, paste0("select Residence_Addresses_AddressLine, 
                        Residence_Addresses_City, 
                        Residence_Addresses_State,
                        Residence_Addresses_CensusTract,
                        Residence_Addresses_CensusBlockGroup,
                        Residence_Addresses_CensusBlock from [", state, "]")) %>% 
    group_by(Residence_Addresses_AddressLine, 
             Residence_Addresses_City, 
             Residence_Addresses_State) %>% 
    filter(row_number() == 1) %>% 
    mutate(across(c(Residence_Addresses_CensusTract,
                    Residence_Addresses_CensusBlockGroup,
                    Residence_Addresses_CensusBlock), as.character))
  
  miss <- left_join(miss, new_ads)
  
  k <- bind_rows(miss, fine)
  print(state)
  print(mean(is.na(k$Residence_Addresses_CensusTract)))
  
  dbWriteTable(db14, name = state, value = k, overwrite = T, append = F)
}

########################################################
tabs <- dbListTables(db14)

bg_ballots <- lapply(tabs, function(s){
  if(file.exists(paste0("temp/14_ballots_by_bg_", s, ".rds"))){
    print(paste0(s, " Already Processed"))
  }else{
    print(paste0("Processing ", s))
    code_good <- unique(filter(fips_codes, state == s)$state_code)
    
    ## pull data, clean names, etc
    tt <- dbGetQuery(db14, paste0("select Residence_Addresses_CensusTract,
                                        Residence_Addresses_CensusBlockGroup,
                                        Voters_FIPS,
                                         [General_2014-11-04]
                                from [", s, "]"))
    colnames(tt) <- gsub("-", "_", colnames(tt))
    colnames(tt) <- gsub("'", "", colnames(tt))
    
    tt <- tt %>%
      ## take separate county, tract, and block group codes and combine to single GEOID
      mutate(GEOID = paste0(code_good, str_pad(Voters_FIPS, width = 3, side = "left", pad = "0"),
                            str_pad(Residence_Addresses_CensusTract, width = 6, side = "left", pad = "0"),
                            Residence_Addresses_CensusBlockGroup)) %>%
      ## if turnout flag is missing or empty, make it equal to "N" for "NO"
      mutate(across(starts_with("General"), ~ ifelse(is.na(.) | . == "", "N", .))) %>% 
      group_by(GEOID) %>%
      ## sum ballots cast by each BG
      summarize(ballots = sum(General_2014_11_04 != "N"),
                voters = n())
    
    
    saveRDS(tt, paste0("temp/14_ballots_by_bg_", s, ".rds"))
  }
})

##################

dates <- rbindlist(lapply(files, function(f){
  data.table(state = substring(f, 53, 54),
             `2014` = substring(f, 66, 75))
}))

dates <- full_join(dates, full_join(readRDS("temp/dated_files_2016.rds"),
                                    full_join(readRDS("temp/dated_files_2018.rds"),
                                              readRDS("temp/dated_files_2020.rds")))) %>% 
  rename(State = state)

knitr::kable(dates, booktabs = T, caption = "\\label{tab:dates} Snapshot Dates", linesep = "",
             longtable = T, align=rep('c', ncol(dates)), format = "latex") %>%
  kable_styling(font_size = 10,
                latex_options = c("HOLD_position", "repeat_header")) %>% 
  save_kable(paste0("temp/tabs/snapshot_dates.tex"))
