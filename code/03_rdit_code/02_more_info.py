import time
import random
import os
import pandas as pd
from bs4 import BeautifulSoup
from datetime import datetime


from selenium.common.exceptions import WebDriverException
from selenium.webdriver.remote.webdriver import By
import selenium.webdriver.support.expected_conditions as EC  # noqa
from selenium.webdriver.support.wait import WebDriverWait

import undetected_chromedriver as uc


driver = uc.Chrome(version_main=109)

df1 = pd.read_csv('raw_data/2016.csv')
df2 = pd.read_csv('raw_data/2018.csv')
df3 = pd.read_csv('raw_data/2019.csv')
df4 = pd.read_csv('raw_data/export-08b3b8db-71df-47c0-8e18-05b91d3b1729.csv')

df5 = pd.concat([df1, df2, df3, df4])
df5.columns = [c.replace(' ', '_') for c in df5.columns]

df5['Incident_Date'] = pd.to_datetime(df5['Incident_Date'], format= "%B %d, %Y")

df5 = df5[((df5['Incident_Date'] >= '2020-05-03') & (df5['Incident_Date'] <= '2021-05-02')) |
  ((df5['Incident_Date'] >= '2016-05-08') & (df5['Incident_Date'] <= '2017-05-07'))]

j = df5.Incident_ID.unique()

b = 0

l = os.listdir("raw_data/extra_info/") + os.listdir("C:/Users/morrisk/OneDrive - Brennan Center for Justice/Desktop/extra_info/")

for i in j:
  
  
  if "inc_" + str(i) + ".html" not in l:
    
    b = b + 1
    
    print(i)
    
    driver.get("https://www.gunviolencearchive.org/incident/" + str(i))
  
    time.sleep(10 + (0 * random.random()))
    
    if b % 10 == 0:
      time.sleep(60)
    
    page = driver.page_source
    
    file_ = open('C:/Users/morrisk/OneDrive - Brennan Center for Justice/Desktop/extra_info/inc_' + str(i) + ".html", 'w')
    
    file_.write(page)
    
    file_.close()
    
  else:
    print(i)



print("done")


url = "C:/Users/morrisk/OneDrive - Brennan Center for Justice/Desktop/extra_info/inc_1806368.html"
with open(url) as fp:
    soup = BeautifulSoup(fp, 'html.parser')

text = soup.get_text()

with open("C:/Users/morrisk/OneDrive - Brennan Center for Justice/Desktop/output.txt", "a") as f:
  print(text, file = f)
  
print("d")
