import numpy as np
from scraper import Scraper, DataScraper
import pandas as pd

nan = np.nan

for i in range(50):
    print(f"Page Number: {1 + i}") # start from 50
    linkscaper = Scraper(f"https://www.lankapropertyweb.com/sale/index.php?page={i + 1}&property-type=House")
    linkscaper.scrape_link()
    linkscaper.scrape_location()
    linkscaper.scrape_price()

ds = DataScraper(list_of_links=linkscaper.LINKS)
data = pd.DataFrame(ds.scrape_data())

price_ser = pd.Series(linkscaper.PRICES["Price:"], name="Price")
location_ser = pd.Series(linkscaper.LOCATION["Location:"], name="Location")
data_frame = pd.concat([data, price_ser, location_ser], axis=1)
print(data_frame.head())

# To ensure a safe scraping, I decided to scrape chunks by chunk
data_frame.to_csv("Output.csv", index=False)  # First 50 pages
# data_frame.to_csv("Output1.csv", index=False)  # Last 50 or so pages


# To get the combined form of the chunks
# first_chunk = pd.read_csv("Output.csv")
# second_chunk = pd.read_csv("Output1.csv")
#
# pd.concat([first_chunk, second_chunk], axis=0).to_csv("FullDataFrame.csv")