

j <- read.fwf("C:/Users/morrisk/OneDrive - Brennan Center for Justice/Desktop/output.txt",
              99999) |> 
  filter(!is.na(V1))

location <- filter(j, grepl("Geolocation", V1)) |> 
  pull()

j$v2 <- NA
for(i in c(2:nrow(j))){
  h <- i - 1
  j$v2[i] <- ifelse(grepl("Type: Victim", j$V1[h]), 1, NA)
}

victims <- filter(j, !is.na(v2))

for(i in c(2:nrow(j))){
  h <- i - 1
  j$v2[i] <- ifelse(grepl("Type: Subject-Suspect", j$V1[h]), 1, NA)
}

subj <- filter(j, !is.na(v2))


for(i in c(2:nrow(j))){
  g <- i - 2
  h <- i - 1
  j$v2[i] <- ifelse(grepl("Guns Involved", paste0(j$V1[g], j$V1[h])), 1, NA)
  
}

gun_info <- filter(j, !is.na(v2))
