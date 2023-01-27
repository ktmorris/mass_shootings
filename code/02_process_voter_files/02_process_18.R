db <- dbConnect(SQLite(), "../voter_file_data/national_file_post18.db")
dbh <- dbConnect(SQLite(), "D:/national_file_post18_history.db")

tabs <- dbListTables(db)
tabs <- tabs[tabs != "CA_032020"]

for(f in tabs){
  print(f)
  scode <- filter(fips_codes, state == f)[1, "state_code"]
  # if(!file.exists(paste0("temp/", f, "_bgs_ballots_18.rds"))){
    k <- dbGetQuery(db, paste0("select LALVOTERID,
                                         Residence_Addresses_CensusTract,
                                         Residence_Addresses_CensusBlockGroup,
                                         Voters_FIPS from [", f, "]")) %>%
      ## take separate county, tract, and block group codes and combine to single GEOID
      mutate(GEOID = paste0(scode, str_pad(Voters_FIPS, width = 3, side = "left", pad = "0"),
                            str_pad(Residence_Addresses_CensusTract, width = 6, side = "left", pad = "0"),
                            Residence_Addresses_CensusBlockGroup)) %>%
      select(LALVOTERID, GEOID)
    
    h <- dbGetQuery(dbh, paste0("select LALVOTERID, General_2018_11_06 from ", f, "_history_18")) %>% 
      mutate(across(starts_with("General"), ~ ifelse(is.na(.) | . == "", "N", .)))
    
    
    k <- left_join(k, h) %>% 
      group_by(GEOID) %>% 
      summarize(voter_count = n(),
                ballots = sum(General_2018_11_06 != "N"))
    
    saveRDS(k, paste0("temp/18_ballots_by_bg_", f, ".rds"))
  # }
}

##################

files <- list.dirs("../voter_file_data/national/post_2018/voter-files-by-state", recursive = F)

dates <- rbindlist(lapply(files, function(f){
  data.table(state = substring(f, 65, 66),
             `2018` = substring(f, 69, 78))
}))

saveRDS(dates, "temp/dated_files_2018.rds")
