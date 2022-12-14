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
    if count % 5 == 0:
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
    
    ## FOR TEST
    #if count > 5:
    #    break

#%% ==== Set basics for extracting items info
from bs4 import BeautifulSoup as bs # Inspect the url

df_htmls = pd.DataFrame(columns=["product_id", "html"])
df_items = pd.DataFrame(columns=['product_id', 'item_name', 'reviews', 'price', "volume", "product_pros", "producing_pros", "lrg_category", "sml_category", "pref", "description"])
df_tags = pd.DataFrame(columns=["product_id", "tag"])

#%% ===|= Extract items info from HTML
count = 0
for res in lst_res:

    # Get id
    product_id = lst_ids[count]

    # Count up
    count += 1

    # If already exists, skip for next.
    if product_id in df_items.product_id:
        print("skip|" + str(count))
        continue
    
    # Get the contents
    soup = bs(res.content, 'html.parser')
    df_htmls.append({
        "product_id": product_id,
        "htmls": soup.text
    }, ignore_index = True)

    # Extract basics
    item_name = soup.find('h1', class_='item-name').text
    reviews = soup.find('a', class_='product-review-link').span.text
    reviews = int( reviews.replace('件のレビュー','').replace(",","").replace(",",""))
    price = soup.find('p', class_="price").text
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
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
### ~~~~~ HERE's THE PONT ~~~~~
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~       

    # Push into df
    df_items = df_items.append({
        'product_id': product_id,
        'item_name': item_name,
        'reviews': reviews,
        'price': price
    },ignore_index=True)

    # For more columns
    """'volume': volume,
    "product_pros": product_pros,
    "producing_pros": producing_pros,
    "lrg_category": lrg_category,
    "sml_category": sml_category,
    "pref": pref,
    "description": description  """

    # Count
    print(count)

    # FOR TEST
    """ if count > 5:
        break """
#%% ==== Eport items info as a feather file

# Set datetime for export file name
from datetime import datetime 
now = datetime.now()
year = str(now.year)
month = str(now.month)
day = str(now.day)
hour = str(now.hour)
minute = str(now.minute)

# Set file name
data_dir = "../data/tabechoku/"
filename_tail = year + month + day + "-" + hour + minute + ".feather"

# Export
# df_htmls.to_feather(data_dir + "htmls_" + filename_tail)
df_items.to_feather( data_dir +  'items_' +  filename_tail)
# df_tags.to_feather( data_dir + 'tags_' +  filename_tail)

#%% ==== Set basics for scraping reviews page
lst_reviews = []
df_pageinfo = pd.DataFrame(columns=["product_id", "page_num"])

#%% ==|== Crawl reviews pages
count = 0
for id in lst_ids:
    
    # If id is already in the list, skip to next
    if id in df_pageinfo.product_id:
        print("skip|" + str(count))
        continue

    # Get numbers of reviews
    num_reviews = df_items[product_id==id].reviews
    num_pages = round(num_reviews + 19)

    # Get HTML on every page
    for num_page in range(num_pages):

        # Count up
        count += 1
        
        # Set url to get
        url = url_base +  id + "/reviews?page=" + num_page

        # Get HTML
        res = requests.get(url)

       # Sleep 1 sec in every 10 requests
        if count % 5 == 0:
            time.sleep(1)
    
        # If GET request goes with error, skip to next id.
        status_code = res.status_code
        if status_code == 404:
            print("error")
            continue

        # Push results into lists
        lst_reviews.append(res)
        df_pageinfo.append({
            "product_id": product_id,
            "page_num": num_page
        }, ignore_index = True)

        # Count up
        print(count)
    
        # FOR TEST
        if count > 5:
            break
#%% ==== Set basics for extracting reviews info
df_reviews = pd.DataFrame(columns=["product_id", "rating", "comment"])
df_html_reviews = pd.DataFrame(columns=["product_id", "page_num","html"])

#%% ==|== Ectract reviews info from HTML
count = 0
for review in lst_reviews:

    # Get id and pagenumber
    product_id = df_pageinfo[count].product_id
    page_num = df_pageinfo[count].page_num
    
    # Count up
    count +=1

    # Check existence
    if page_num in df_reviews[product_id == product_id].page_num:
        print("skip|" + str(count))
        continue

    # Get the contents
    soup = bs(review.content, "html.parser")
    df_html_reviews.append({
        "product_id": product_id,
        "page_num": page_num,
        "html": soup.text
    }, ignore_index=True)

    # Extract the data
    rating = 
    comment = 

    # Push into df
    df_reviews.append({
        "product_id": product_id,
        "rating": rating,
        "comment": comment
    }, ignore_index=True)

    # Count 
    print(count)
#%% ==== Export reviews info as a feather file.
df_reviews.to_feather( data_dir + 'reviews_' + filename_tail)
