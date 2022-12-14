#%% ==== Import product ids 
# Load the packages
import pandas as pd # Hnadling data frames
import feather # For feather files

# Get the data
df_ctg = pd.read_feather("../data/tabechoku/productsCtg_20221123-173118.feather")

# Extract ids
ids = df_ctg.id_product

#%% ==== Set basics for scraping items page
import requests #Get data from url
import time

url_base = "https://www.tabechoku.com/products/"

sleep_period = 5

lst_ids = []
lst_res = []

#%% ==== Crawl items pages
count = 0
for id in ids:

    # Count up
    count += 1

     # Convert id from float to string
    id = str(round(id))
    
    # If id is already in the list, skip to next
    if id in lst_ids:
        print("skip" + str(count))
        continue

    # Set URL to get
    url = url_base + id

    # Get HTML
    res = requests.get(url)

    # Sleep 1 sec in every 10 requests
    if count % sleep_period == 0:
        time.sleep(1)
    
    # If GET request goes with error, skip to next id.
    status_code = res.status_code
    if status_code == 404:
        print("error")
        continue

    # Push results into lists
    lst_res.append(res)
    lst_ids.append(id)

    # Count up
    print(count)
    
   """  # FOR TEST
    if count > 10:
        break """

#%% ==== Set basics for saving items results

from datetime import datetime 
def datafile_path( file_title:str, folder_name: str = "tabechoku", extention: str = ".feather"):
    # Set directory to save
    dir = "../data/" + folder_name + "/"

    # Set time stamp in a file name
    now = datetime.now()
    year = str(now.year)
    month = str(now.month)
    day = str(now.day)
    hour = str(now.hour)
    minute = str(now.minute)

    # Create file name
    filename = dir + file_title + "_" + year + month + day + "-" + hour + minute + extention

    # Return
    return filename

from bs4 import BeautifulSoup as bs # Inspect the url

df_soup_items = pd.DataFrame(columns=["product_id", "soup"])

#%% ==== Export items soup df as a feather file
count = 0
for res in lst_res:
    
    # Get the id
    product_id = lst_ids[count]

    # Count up
    count += 1

    # If already exists, skip for next.
    if product_id in list(df_soup_items.product_id):
        print("skip|" + str(count))
        continue
    
    # Get the contents
    soup = bs(res.content, 'html.parser')
    df_soup_items.loc[len(df_soup_items)] = [product_id, soup]

    # Count
    print(count)

# Save df as a feather file
df_soup_items.to_feather(datafile_path("soup_items"))

#%% ===|= Set basics for extracting items info

"""df_items = pd.DataFrame(columns=['product_id', 'item_name', 'reviews', 'price', "volume", "product_pros", "producing_pros", "lrg_category", "sml_category", "pref", "description"])"""
df_items = pd.DataFrame(columns=['product_id', 'item_name', 'reviews', 'price'])
df_tags = pd.DataFrame(columns=["product_id", "tag"])

#%% ==|== Extract items info from soup
count = 0
for soup in list(df_soup_items.soup):

    # Get id
    product_id = df_soup_items.product_id[count]

    # Count up
    count += 1

    # Check existense
    if product_id in list(df_items.product_id):
        print("skip|" + str(count))
        continue
    
    # Extract items info
    try:
        item_name = soup.find('h1', class_='item-name').text
    except:
        item_name = "Not Found"
    
    try:
        reviews = soup.find('a', class_='product-review-link').span.text
    except:
        reviews = 0
    else:
        reviews = int( reviews.replace('件のレビュー','').replace(",","").replace(",",""))
    
    try:
        price = soup.find('p', class_="price").text
    except:
        price = 0
    else:
        price = int(price.replace('¥','').replace(',', ''))

    """ volume = 
    product_pros = 
    producing_pros =
    lrg_category = 
    sml_category =
    pref =
    description = 

    # Extract tags
    tags =
    for tag in tags:
        df_tags.append({
            "product_id": product_id,
            "tag": tag
        }, ignore_index=True)
     """      

    # Push into df
    df_items.loc[len(df_items)] = [product_id, item_name, reviews, price]

    # Count
    print(count)

    """ # FOR TEST
    if count > 20:
        break """

# Export items df as a feather file
df_items.to_feather(datafile_path("items"))
""" df_tags.to_feather(datafile_path("tags")) """

#%% ==== Set basics for scraping reviews page
lst_reviews = []
df_pageinfo = pd.DataFrame(columns=["product_id", "page_num"])

#%% ==|== Crawl reviews pages
count = 0
for product_id in list(df_items.product_id):
    
    # If id is already in the list, skip to next
    if product_id in df_pageinfo.product_id:
        print("skip|" + str(count))
        continue

    # Get numbers of reviews
    num_reviews = int(df_items[df_items["product_id"] == product_id].reviews)
    num_pages = round((num_reviews+9) / 20)

    # Get HTML on every page
    for num_page in range(1,num_pages):

        # Count up
        count += 1
        
        # Set url to get
        url = url_base +  product_id + "/reviews?page=" + str(num_page)

        # Get HTML
        res = requests.get(url)

       # Sleep 1 sec respecitively
        if count % sleep_period == 0:
            time.sleep(1)
    
        # If GET request goes with error, skip to next id.
        status_code = res.status_code
        if status_code == 404:
            print("error")
            continue

        # Push results into lists
        lst_reviews.append(res)
        df_pageinfo.loc[len(df_pageinfo)] = [product_id, num_page]

        # Count up
        print(count)
    
        """ # FOR TEST
        if count > 5:
            break """

#%% ==== Set basics for saving reviews results
df_soup_reviews = pd.DataFrame(columns=["product_id", "page_num", "soup"])

#%% ==== Export reviews soup df as a feather file
count = 0
for reviews in lst_reviews:
    
    # Get the id
    product_id = str(df_pageinfo.product_id[count])
    page_num = int(df_pageinfo.page_num[count])

    # Count up
    count += 1

    # If already exists, skip for next.
    if product_id in list(df_soup_reviews.product_id):
        if page_num in list(df_soup_reviews.page_num):
            print("skip|" + str(count))
            continue
    
    # Get the contents
    soup = bs(reviews.content, 'html.parser')
    df_soup_reviews.loc[len(df_soup_reviews)] = [product_id, page_num, soup]

    # Count
    print(count)

# Save df as a feather file
df_soup_reviews.to_feather(datafile_path("soup_reviews"))


#%% ==== Set basics for extracting reviews info

df_reviews = pd.DataFrame(columns=["product_id", "rating", "comment"])

#%% ==|== Extract reviews info from soup
count = 0
for soup in list(df_soup_reviews.soup):

    # Get id and page number
    product_id = str(df_soup_reviews.product_id[count])

    # Count up
    count += 1
    
    # Extract items info
    try:
        rating = soup.find('', class_='').text
    except:
        rating = 0
    
    try:
        comment = soup.find('', class_='').span.text
    except:
        comment = "Not Found"

    # Push into df
    df_reviews.loc[len(df_reviews)] = [product_id, rating, comment]

    # Count
    print(count)

    """ # FOR TEST
    if count > 20:
        break """

# Export items df as a feather file
df_reviews.to_feather(datafile_path("reviews"))
# %%
