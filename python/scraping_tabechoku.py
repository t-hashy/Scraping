#%% == Set conditions
print("Set conditions")

# Base conditions
test = True
initialization = True

# Files importing: Describe file path or "False" value.
imp_id = "../data/tabechoku/productsCtg_20221123-173118.feather" # Must be a file path
imp_cnt_items = False
imp_items = False
imp_tags = False
imp_cnt_reviews = False
imp_reviews = False

#%% == Set basics
print("Set basics")

if initialization:
    # Import product ids 
    import pandas as pd # Hnadling data frames
    import feather # For feather files
    df_ctg = pd.read_feather(imp_id)
    lst_ids = list(df_ctg.id_product)

    # Set basics for crawlling
    import requests #Get data from url
    import time
    url_base = "https://www.tabechoku.com/products/"
    sleep_period = 4

    # Set basics for exporting
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

    # Set basics for beautifulsoup
    from bs4 import BeautifulSoup as bs # Inspect the url

    # Set data frames
    ## For items
    df_cnt_items = pd.DataFrame(columns=["product_id", "content"])
    """df_items = pd.DataFrame(columns=['product_id', 'item_name', 'reviews', 'price', "volume", "product_pros", "producing_pros", "lrg_category", "sml_category", "pref", "description"])"""
    df_items = pd.DataFrame(columns=['product_id', 'item_name', 'reviews', 'price'])
    df_tags = pd.DataFrame(columns=["product_id", "tag"])
    ## For reviews
    df_cnt_reviews = pd.DataFrame(columns=["product_id", "num_page", "content"])
    df_reviews = pd.DataFrame(columns=["product_id", "rating", "comment"])


#%% ==== Get contents of items pages
print("Get contents of items pages")

if imp_cnt_items:
    df_cnt_items = pd.read_feather(imp_cnt_items)
count = 0
i = 0
for product_id in lst_ids:

    # Count up
    count += 1

    # Convert id from float to string
    product_id = str(round(product_id))
    
    # Check existence
    if product_id in list(df_cnt_items.product_id):
        print("skip" + str(count))
        continue

    # Set URL to get
    url = url_base + product_id

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
    
    # Get the content
    content = res.content

    # Push results into lists
    df_cnt_items.loc[len(df_cnt_items)] = [product_id, content]

    # Iterater check: Take a break and load data in every 5000 times
    i += 1
    if i > 5000:
        break

    # Count up
    print(count)
    
    # FOR TEST
    if test & (count > 10):
        break

# Export the data
df_cnt_items.to_feather(datafile_path("cnt_items"))

#%% ==== Extract items info
print("Extract items info")

if imp_items & imp_tags:
    df_items = pd.read_feather(imp_items)
    df_tags = pd.read_feather(imp_tags)
for i in range(len(df_cnt_items)):

    # Get item info of the row
    product_id = df_cnt_items.product_id[i]
    content = df_cnt_items.content[i]

    # Check existence
    if product_id in list(df_items.product_id):
        print("skip|" + str(i))
        continue
    
    # Parse with beautifulsoup
    soup = bs(content, 'html.parser')
    
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
    """ df_tags.loc[len(df_tags)] = [product_id, tag] """

    # Show progress
    print(i)

# Export items df as a feather file
df_items.to_feather(datafile_path("items"))
""" df_tags.to_feather(datafile_path("tags")) """

#%% ==|== Get contents of reviews pages
print("Get contents of reviews pages")

if imp_cnt_reviews:
    df_cnt_reviews = pd.read_feather(imp_cnt_reviews)
for i in range(len(df_items)):

    # Get the items info
    product_id = df_items.product_id[i]
    num_reviews = df_items.reviews[i]
    num_pages = round((num_reviews+9) / 20)

    # Check existence
    if product_id in list(df_cnt_reviews.product_id):
        if num_pages in list(df_cnt_reviews[df_cnt_reviews["product_id"] == product_id].num_page):
            print("skip|" + str(count))
            continue

    # Get HTML on every page
    for num_page in range(1,num_pages):
        
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

        # Get content
        content = res.content

        # Push results into lists
        df_cnt_reviews.loc[len(df_cnt_reviews)] = [product_id, num_page, content]

        # Show progress
        print('{index}-{page}'.format(index=i, page=num_page))

    # Iterater check: take a load break in every 5,000 times
    if i > 5000:
        break

# Export the data
df_cnt_reviews.to_feather(datafile_path("cnt_reviews"))

#%% ==== Extract reviews

print("Extract reviews")

if imp_reviews:
    df_reviews = pd.read_feather(imp_reviews)
for i in range(len(df_cnt_reviews)):
    
    # Get the reviews info
    product_id = df_cnt_reviews.product_id[i]
    num_page = df_cnt_reviews.num_page[i]
    content = df_cnt_reviews.content[i]
    
    # Get the contents
    soup = bs(content, 'html.parser')
    
    # Extract items info
    try:
        rating = soup.find('', class_='').text
    except:
        rating = 0
    
    try:
        comment = soup.find('', class_='').span.text
    except:
        comment = "Not Found"

    # Check existence
    if product_id in list(df_reviews.product_id):
        if rating in list(df_reviews[df_reviews["product_id"] == product_id].rating):
            if comment in list(df_reviews[df_reviews["product_id"] == product_id].comment):
                print("skip|" + str(i))
                continue

    # Push into df
    df_reviews.loc[len(df_reviews)] = [product_id, rating, comment]

    # Show progress
    print(i)

# Export items df as a feather file
df_reviews.to_feather(datafile_path("reviews"))

#%% FIN
print("DONE")