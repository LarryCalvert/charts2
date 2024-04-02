import sys
import json

import requests
from urllib.parse import unquote

#geturl = rf'https://www.barchart.com/futures/quotes/{symbol}/price-history/historical'
#geturl = 'https://www.barchart.com/futures/quotes/' + symbol + '/price-history/historical'


def scrape(symbol):
    geturl = 'https://www.barchart.com/futures/quotes/' + symbol + '/price-history/historical'    
    apiurl=r'https://www.barchart.com/proxies/core-api/v1/historical/get'
    getheaders={
        'accept': 'text/html,application/xhtml+xml,application/xml;q=0.9,image/webp,image/apng,*/*;q=0.8',
        'accept-encoding': 'gzip, deflate, br',
        'accept-language': 'en-US,en;q=0.9',
        'cache-control': 'max-age=0',
        'upgrade-insecure-requests': '1',
        'referer': geturl,
        'user-agent': 'Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/72.0.3626.119 Safari/537.36'
    }
    s=requests.Session()
    r=s.get(geturl, headers=getheaders)
    headers={
        'accept': 'application/json',
        'accept-encoding': 'gzip, deflate, br',
        'accept-language': 'en-US,en;q=0.9',
        'user-agent': "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/90.0.4430.85 Safari/537.36",
        'x-xsrf-token': unquote(unquote(s.cookies.get_dict()['XSRF-TOKEN']))
    }
    payload={
        #"symbol":"ZWH24",
        "symbol":symbol,
        "fields":"tradeTime.format(m\/d\/Y),openPrice,highPrice,lowPrice,lastPrice,volume,openInterest",
        "type":"eod",
        "orderBy":"tradeTime",
        "orderDir":"asc",
        "limit":300,
        #"meta":"field.shortName,field.type,field.description",
        'raw': '1'
    }
    r=s.get(apiurl,params=payload,headers=headers)
    j=r.json()
    #print(j)
    return j
    
     


def main():
    # Check if a command line argument is provided
    if len(sys.argv) != 2:
        print("Usage: python script.py <symbol>")
        sys.exit(1)
    # Get the symbol from the command line argument
    symbol = sys.argv[1]
    # Call the scrape function
    scraped_text = scrape(symbol)
    # Write the scraped text to a file in JSON format
    filename = f"{symbol}.json"
    with open(filename, 'w') as file:
        json.dump({"symbol": symbol, "scraped_text": scraped_text}, file)
    print(f"Scraped text has been written to {filename}")


if __name__ == "__main__":
    main()

