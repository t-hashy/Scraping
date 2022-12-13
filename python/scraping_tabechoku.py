#%% Import the data
# Load the packages
import pandas as pd # Hnadling data frames
import feather # For feather files

# Get the data
df_ctg = pd.read_feather("../data/tabechoku/productsCtg_20221123-173118.feather")

# Extract ids
ids = df_ctg.id_product

#%% Get HTML in each product page
# Load the packages
import requests #Get data from url
import time

# Set basics for scraping
url_base = "https://www.tabechoku.com/products/"
lst_ids = []
lst_res = []
count = 0

# Get HTML in each product page.
for id in ids:
    
    # Convert id from float to string
    id = str(round(id))

    # Set URL to get
    url = url_base + str(id)

    # Get HTML
    res = requests.get(url)

    # Sleep 1 sec in every 10 requests
    count += 1
    if count % 10 == 0:
        time.sleep(1)
    
    # If GET request goes with error, skip to next id.
    status_code = res.status_code
    if status_code == 404:
        continue

    # Push results into lists
    lst_res.append(res)
    lst_ids.append(id)
    
    ## FOR SAFETY, remove if all the conditions are settled and checked.
    print(count)
    if count > 20:
        break

#%% Extract nodes from HTML list
# Load the packages
from bs4 import BeautifulSoup as bs # Inspect the url

# Set basic variables
df_items = pd.DataFrame(columns=['id', 'item_name', 'reviews', 'price', "volume"])
count = 0

# Extract nodes from HTML
for res in lst_res:
    
    # Get the contents
    soup = bs(res.content, 'html.parser')

    # Extract nodes
    item_name = soup.find('h1', class_='item-name').text
    reviews = soup.find('a', class_='product-review-link').span.text
    reviews = int( reviews.replace('件のレビュー',''))
    price = soup.find('p', class_="price").text
    price = int(price.replace('¥','').replace(',', ''))

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
### ~~~~~ HERE's THE PONT ~~~~~
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

    categories = soup.find_all(class_='product-categories_link-wrap')
        

    # Push into df
    df_items = df_items.append({
        'id': lst_ids[count],
        'item_name': item_name,
        'reviews': reviews,
        'price': price
    },ignore_index=True)
    
    # Count up
    count += 1