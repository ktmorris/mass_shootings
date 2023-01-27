## these functions assume that the raw L2 post-2016 voter files are in a SQL database
## dc's file is funky so needs to be done separately

db <- dbConnect(SQLite(), "D:/national_file_post16.db")
tabs <- dbListTables(db)

bg_ballots <- lapply(tabs, function(s){
  if(file.exists(paste0("temp/16_ballots_by_bg_", s, ".rds"))){
    print(paste0(s, " Already Processed"))
  }else{
    print(paste0("Processing ", s))
    code_good <- unique(filter(fips_codes, state == s)$state_code)
    if(s %in% c("DC")){
      ## pull data, clean names, etc
      tt <- dbGetQuery(db, paste0("select LALVOTERID,
                                         Residence_Addresses_CensusTract,
                                         Residence_Addresses_CensusBlockGroup,
                                         Voters_FIPS,
                                [General_2016-11-08],
                                [General_2012-11-06],
                                [General_2014-11-04],
                                [General_2008-11-04]
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
        summarize(ballots = sum(General_2016_11_08 != "N"),
                  voters = n())
    }else{
      tt <- dbGetQuery(db, paste0("select LALVOTERID,
                                         Residence_Addresses_CensusTract,
                                         Residence_Addresses_CensusBlockGroup,
                                         Voters_FIPS,
                                [General_2016-11-08],
                                [General_2012-11-06],
                                [General_2014-11-04],
                                [General_2008-11-04],
                                [General_2010-11-02]
                                from [", s, "]"))
      colnames(tt) <- gsub("-", "_", colnames(tt))
      colnames(tt) <- gsub("'", "", colnames(tt))
      tt <- tt %>%
        mutate(GEOID = paste0(code_good, str_pad(Voters_FIPS, width = 3, side = "left", pad = "0"),
                              str_pad(Residence_Addresses_CensusTract, width = 6, side = "left", pad = "0"),
                              Residence_Addresses_CensusBlockGroup)) %>%
        mutate(across(starts_with("General"), ~ ifelse(is.na(.) | . == "", "N", .)),
               new = General_2016_11_08 != "N" &
                 General_2008_11_04 == "N" & General_2014_11_04 == "N" &
                 General_2012_11_06 == "N" & General_2010_11_02 == "N") %>% 
        group_by(GEOID) %>%
        summarize(ballots = sum(General_2016_11_08 != "N"),
                  voters = n(),
                  new = sum(new))
    }
    
    saveRDS(tt, paste0("temp/16_ballots_by_bg_", s, ".rds"))
  }
})


##################

files <- list.dirs("../voter_file_data/national/post_2016", recursive = F)

dates <- rbindlist(lapply(files, function(f){
  data.table(state = substring(f, 53, 54),
             `2016` = substring(f, 66, 75))
}))

saveRDS(dates, "temp/dated_files_2016.rds")
