#%%
import requests
from bs4 import BeautifulSoup
import csv
import pandas as pd
import pyarrow.feather as feather

# Making a GET request
r = requests.get('https://www.tabechoku.com/products/')

# Check status code for response recieved
# > Success code: 200
print(r.status_code)

# Parsing the HTML
soup = BeautifulSoup(r.content, 'html.parser')

s = soup.find('div', class_='p-productList')

names = s.find_all('h4')
lst_names = []

count = 1
for name in names:
    d = {}
    d['No'] = f'Name { count }'
    d['Name'] = name.text
    count += 1
    lst_names.append(d)

df_names = pd.DataFrame(lst_names, columns=['No', 'Name'])

# Export as feather
path = "test.feather"
feather.write_feather(df_names, path)

f_df = feather.read_feather('test.feather')

# filename = 'names.csv'
# with open(filename, 'w', newline='') as f:
#     w  = csv.DictWriter(f, ['No', 'Name'])
#     w.writeheader()
# 
#     w.writerows(lst_names)
