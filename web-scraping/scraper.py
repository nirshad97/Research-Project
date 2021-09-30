from bs4 import BeautifulSoup
import requests
import numpy as np


class Scraper:
    """Scrapper objects look into HTML elements and finds the necessary information we need.
    We are in need of the links of the each house. These links contain the information about
    the house themselves. We need to get the location and prices of the house seperately."""

    LINKS = []

    LOCATION = {"Location:": []}

    PRICES = {"Price:": []}

    def __init__(self, link):
        self.link = link
        response = requests.get(self.link)
        webpage = response.text
        self.soup = BeautifulSoup(webpage, "html.parser")

    def scrape_link(self):
        """This scrapes the href from the anchor tag from the div with the class having caption1.
        Scraped links are then appended to the main LINKS object. This LINK object is then iterated
        through using a separate class (DataScraper). """

        div_tags = self.soup.find_all("div", {"class": "caption1"})
        for tags in div_tags:
            href_link = tags.find("a", href=True)["href"]
            new_link = "https://www.lankapropertyweb.com" + href_link
            self.LINKS.append(new_link)
        print(f"So far {len(self.LINKS)} links have been scraped.....")
        return self.LINKS

    def scrape_location(self):
        """Location and prices are scraped independently for convenience. We maintain
        global LOCATION dictionary to store the location and the prices of the houses."""

        elems = self.soup.find_all("li", id="og-thum-city")
        for elem in elems:
            self.LOCATION["Location:"].append(elem.text)

        return self.LOCATION  # returns a dictionary
    # Change this to a pd.Series

    def scrape_price(self):
        elems = self.soup.find_all("span", class_="priceList")
        for elem in elems:
            self.PRICES["Price:"].append(elem.text)

        return self.PRICES # Returns a dictionary
    # Change this to a pd.Series


class DataScraper:

    HOUSE_INFO = {

        "Property Type:": [],
        "Bedrooms:": [],
        "Bathrooms/WCs:": [],
        "Floor area:": [],
        "No. of floors:": [],
        "Car parking spaces:": [],
        "Area of land:": [],
        "Availability:": [],
        "Nearest bus stop:": [],
        "Nearest train station:": []

    }

    def __init__(self, list_of_links):
        self.list_of_links = list_of_links

    def scrape_data(self):
        """The global LINKS object consists of the individual links of different houses. Within
        these links have the information we need i.e. number of bedrooms, square ft. of the house etc...
        This class is responsible for going into this link and scraping the necessary information.
        Fields necessary are already in the global HOUSE_INFO object"""

        for link in self.list_of_links:
            print(f"Scraping information from: {link}")
            response = requests.get(link)
            webpage = response.text
            soup = BeautifulSoup(webpage, "html.parser")
            all_tabs = soup.find_all('tr')
            keys = list(self.HOUSE_INFO.keys())  # Getting all the keys from the dictionary

            for row in all_tabs:

                try:
                    value_key = row.find("td", class_="left").text
                    info_value = row.find("td", class_="right").text

                    self.HOUSE_INFO[value_key].append(info_value)
                    keys.remove(value_key)  # Some fields aren't provided in the webpage
                    # To tackle this problem, we remove the fields used
                    # The we can fill in nan values for the remaining fields.
                except AttributeError:
                    None
                # Whenever a field that is not on our house_info variable appears
                # it would throw an error
                # This is error handled here
                except KeyError:
                    None

            for missing_key in keys:
                self.HOUSE_INFO[missing_key].append(np.nan)

        return self.HOUSE_INFO

