#%% == Set conditions
print("Set conditions")

# Base conditions
<<<<<<< HEAD
test = False
initialization = True # Just for basics not for data frames.
sleep_period = 5

# Files importing: Describe file path or "False" value.
imp_ids = "../data/tabechoku/cnt_items_20221215-2348.feather" # Must be a path of a file with a column named "produc_id"
imp_cnt_items = "../data/tabechoku/cnt_items_20221215-2348.feather"
imp_items = "../data/tabechoku/items_20221217-5h1m.feather"
=======
test = True
initialization = True

# Files importing: Describe file path or "False" value.
imp_id = "../data/tabechoku/productsCtg_20221123-173118.feather" # Must be a file path
imp_cnt_items = False
imp_items = False
>>>>>>> parent of 282070c (fix some)
imp_tags = False
imp_cnt_reviews = "../data/tabechoku/cnt_reviews_20221218-11h35m.feather"
imp_reviews = False

# Set basics
if initialization:
    
    # Set product ids 
    import pandas as pd # Hnadling data frames
    import feather # For feather files

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
        filename = dir + file_title + "_" + year + month + day + "-" + hour + "h" + minute + "m" + extention

        # Return
        return filename

    # Set basics for beautifulsoup
    from bs4 import BeautifulSoup as bs # Inspect the url

<<<<<<< HEAD
=======
    # Set data frames
    ## For items
    df_cnt_items = pd.DataFrame(columns=["product_id", "content"])
    """df_items = pd.DataFrame(columns=['product_id', 'item_name', 'reviews', 'price', "volume", "product_pros", "producing_pros", "lrg_category", "sml_category", "pref", "description"])"""
    df_items = pd.DataFrame(columns=['product_id', 'item_name', 'reviews', 'price'])
    df_tags = pd.DataFrame(columns=["product_id", "tag"])
    ## For reviews
    df_cnt_reviews = pd.DataFrame(columns=["product_id", "num_page", "content"])
    df_reviews = pd.DataFrame(columns=["product_id", "rating", "comment"])


>>>>>>> parent of 282070c (fix some)
#%% ==== Get contents of items pages
print("Get contents of items pages")

if imp_cnt_items:
    print("Create df_cnt_items")
    df_cnt_items = pd.read_feather(imp_cnt_items)
<<<<<<< HEAD
else:
    df_cnt_items = pd.DataFrame(columns=["product_id", "content"])
    
print("Create df_ids")
df_ids = pd.read_feather(imp_ids).product_id

=======
>>>>>>> parent of 282070c (fix some)
count = 0
i = 0
for product_id in list(df_ids):

    # Count up
    count += 1
    
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

    # Count up
    print(count)
    
    # FOR TEST
    if test & (count > 10):
        break

    # Save in every 5,000 times
    if count % 5000 == 0:
        print("Save cnt_items")
        df_cnt_items.to_feather(datafile_path("cnt_items"))

# Export the data
print("Save cnt_items file")
df_cnt_items.to_feather(datafile_path("cnt_items"))

#%% ==== Extract items info
print("Extract items info")

if imp_items:
    print("Get df_items data")
    df_items = pd.read_feather(imp_items)
else:
    df_items = pd.DataFrame(columns=['product_id', 'item_name', 'reviews', 'price', "volume", "product_pros", "producing_pros", "lrg_category", "sml_category", "pref", "description"])

if imp_tags:
    print("Get df_tag")
    df_tags = pd.read_feather(imp_tags)
<<<<<<< HEAD
else:
    df_tags = pd.DataFrame(columns=["product_id", "tag"])


=======
>>>>>>> parent of 282070c (fix some)
for i in range(len(df_cnt_items)):

    # Get item info of the row
    product_id = df_cnt_items.product_id[i]
    content = df_cnt_items.content[i]

    # Check existence
    if product_id in list(df_items.product_id):
        print("skip|" + str(i))
        continue
    
    # Parse with beautifulsoup
    soup = bs(content, 'html.parser').find('div', class_='info-area container')
    
    # Extract items info
    try:
        item_name = soup.find('h1', class_='item-name').text
    except:
        item_name = ""
    
    try:
        reviews_txt = soup.find('a', class_='product-review-link').span.text
        reviews = int( reviews_txt.replace('件のレビュー','').replace(",","").replace(",",""))
    except:
        reviews = ""
        
    try:
        price_txt = soup.find('p', class_="price").text
        price = int(price_txt.replace('¥','').replace(',', ''))
    except:
        price = ""

    soup_dl = soup.find('dl', class_='product-detail-list')
    soup_dts = soup_dl.find_all('dt')
    soup_dds = soup_dl.find_all('dd')
    lst_dl = []
    i = 0
    for soup_dt in soup_dts:
        dt = soup_dt.text
        dd = soup_dds[i].text
        lst_dl.append([dt, dd])        

    volume = ""

    product_pros = ""
    
    producing_pros = ""
    
    lrg_category = ""
        
    sml_category = ""
        
    pref = ""
        
    description = ""
    
    # Push into df
    df_items.loc[len(df_items)] = [product_id, item_name, reviews, price, volume, product_pros, producing_pros, lrg_category, sml_category, pref, description]

    # Extract tags
    """ try:
        tags = soup.find_all()
        for tag in tags:
            df_tags.loc[len(df_tags)] = [product_id, tag]
    except:
        df_tags.loc[len(df_tags)] = [product_id, "" ] """
    
    # Show progress
    print(i)

    # Test
    if test & (i > 5):
        break

    # Save every 1,000 times
    if (i % 1000 == 0) & (i > 0):
        print("Save df_items")
        df_items.to_feather(datafile_path("items"))
        # print("Save df_tags")
        # df_tags.to_feather(datafile_path("tags"))

# Export items df as a feather file
print("Save df_items")
df_items.to_feather(datafile_path("items"))
# print("Save df_tags")
#df_tags.to_feather(datafile_path("tags"))

#%% ==== Get contents of reviews pages
print("Get contents of reviews pages")

if imp_cnt_reviews:
    print("Get cnt_reviews")
    df_cnt_reviews = pd.read_feather(imp_cnt_reviews)
<<<<<<< HEAD
else:
    df_cnt_reviews = pd.DataFrame(columns=["product_id", "num_page", "content"])

count = 0
=======
>>>>>>> parent of 282070c (fix some)
for i in range(len(df_items)):

    # Get item info
    product_id = df_items.product_id[i]
    reviews = df_items.reviews[i]
    num_pages = int(round((reviews+10)/20))

    # Get HTML on every page
    for num_page in range(1, num_pages):

        """ # Check existence
        if (product_id in list(df_cnt_reviews.product_id)) and (num_page in list(df_cnt_reviews.num_page)):
            print("skip|{index}-{page}".format(index=i, page=num_page))
            continue """
        
        # Set url to get
        url = url_base +  product_id + "/reviews?page=" + str(num_page)

        # Get HTML
        res = requests.get(url)

        # Sleep 1 sec respecitively
        count += 1
        if count % sleep_period == 0:
            time.sleep(1)
    
        # If GET request goes with error, skip to next id.
        status_code = res.status_code
        if status_code == 404:
            print("error")
            break

        # Get content
        content = res.content

        # Push results into lists
        df_cnt_reviews.loc[len(df_cnt_reviews)] = [product_id, num_page, content]

        # Show progress
        print('{index}-{page}|{c}'.format(index=i, page=num_page, c=count))
    
        # Save in every 1,000 times
        if (count % 1000 == 0) & (count > 0):
            print("Save temp df_cnt_reviews")
            df_cnt_reviews.to_feather(datafile_path("cnt_reviews"))

    # TEST
    if test:
        if count > 5:
            break

# Export the data
print("Save df_cnt_reviews")
df_cnt_reviews.to_feather(datafile_path("cnt_reviews"))

#%% ==== Extract reviews

print("Extract reviews")

if imp_reviews:
    print("Get reviews")
    df_reviews = pd.read_feather(imp_reviews)
for i in range(len(df_cnt_reviews)):
    
    # Get the reviews info
    product_id = df_cnt_reviews.product_id[i]
    num_page = df_cnt_reviews.num_page[i]
    content = df_cnt_reviews.content[i]
    
    # Get the contents
    soup = bs(content, 'html.parser')
    
    # Extract posts
    posts = soup.find_all('div', class_='p-post')

    # Reviews in each post
    for post in posts:

        try:
            rating_elm = post.find('i', clas_='c-rating')
            rating = int(rating_elm['title'].text.replace('評価：', ''))
        except:
            rating = ""
        
        try:
            comment = post.find('p', class_='p-post__contentBody').text
        except:
            comment = ""
        
        # Push into df
        df_reviews.loc[len(df_reviews)] = [product_id, rating, comment]
            

    # Check existence
    if product_id in list(df_reviews.product_id):
        if rating in list(df_reviews[df_reviews["product_id"] == product_id].rating):
            if comment in list(df_reviews[df_reviews["product_id"] == product_id].comment):
                print("skip|" + str(i))
                continue
 
    # Show progress
    print(i)

    # Save in every 5,000 times
    if (i % 5000 == 0) & (i > 0):
        print("Save df_reviews")
        df_reviews.to_feather(datafile_path("reviews"))

# Export items df as a feather file
print("Save df_reviews")
df_reviews.to_feather(datafile_path("reviews"))

#%% FIN
print("DONE")