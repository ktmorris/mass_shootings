library(rvest)
library(RSelenium)
library(data.table)

rD <- rsDriver(browser = "firefox",
               chromever = NULL)

remDr <- rD[["client"]]



shootings <- unique(rbind(fread("raw_data/export-08b3b8db-71df-47c0-8e18-05b91d3b1729.csv"),
                   fread("raw_data/2018.csv"),
                   fread("raw_data/2019.csv"),
                   fread("raw_data/2016.csv"))$`Incident ID`)

hold <- c()

for(i in shootings){
  remDr$navigate("https://web.archive.org/")
  
  
  input <- remDr$findElement(using = 'xpath', value = '/html/body/div[4]/div[1]/div[3]/form/div/div/input[1]')
  input$sendKeysToElement(list(paste0("https://www.gunviolencearchive.org/incident/", i), key = "enter"))
  Sys.sleep(2)
  
  pa <- remDr$findElements(using = 'xpath', value = '/html/body/div[4]/div[2]/span/a[1]')
  
  if(length(pa) == 0){
    pa <- remDr$findElement(using = 'xpath', value = '/html/body/div[4]/div[2]/a')
  }else{
    pa <- remDr$findElement(using = 'xpath', value = '/html/body/div[4]/div[2]/span/a[1]')
  }
  
  pa$clickElement()
  
  hold <- c(hold, remDr$getCurrentUrl())
}


