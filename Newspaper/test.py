import requests
import os
from dotenv import load_dotenv

load_dotenv()

API_KEY = os.getenv("NEWS_API")

url = ('https://newsapi.org/v2/everything?'
       'q=Apple&'
       'from=2024-07-16&'
       'sortBy=popularity&'
       'apiKey=f412a23579584b8ab98de1674633acc6')

response = requests.get(url)

print(response.json)
