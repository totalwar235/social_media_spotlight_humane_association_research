from email import header
from oauthlib.oauth2 import BackendApplicationClient
from requests_oauthlib import OAuth2Session
import requests
import pandas as pd

api_key = 'FIX5FVnmleO1OH9C1gTXn0kQqnX0i73DeVrmcJND6Q7zJpqSxQ'
api_secret = 'ySlZNKGk5UtmvLJWeYuXXdKO4F86tecpYYsczG6W'
url = 'https://api.petfinder.com/v2/oauth2/token'

token_url = url+"/grant_type=client_credentials&client_id="+api_key+"&client_secret="+api_secret
client = BackendApplicationClient(client_id=api_key)
oauth = OAuth2Session(client=client)
token = oauth.fetch_token(token_url=url, client_id=api_key, client_secret=api_secret)

headers = {'Authorization': "Bearer " + token["access_token"]}
animals_url = 'https://api.petfinder.com/v2/animals'
orginizations = [
    'WI325', #jackson county AS
    'WI335', #Taylor county HS
    'WI03', #rusk county
    'WI07', #chippewa county
    'WI46', #dunn county
    'WI25', #washburn county
    'WI61', #douglas county
    'WI469', #st croix county
    'WI96', #buffalo county
    'WI246', #trempealeau county
    'wi128', #sawyer county
    'wi09', #peppin county
    'wi22' #baron county
    ]

org_string = ''
for i in orginizations:
    org_string += ','+i
org_string = org_string[1:]

page_count=animals_url+"?organization="+org_string+'&limit=100&status=adoptable,adopted'

pages = requests.get(page_count, headers=headers).json()

all_animals = []

for i in range(pages.get('pagination').get('total_pages')):
    page_animals=animals_url+"?organization="+org_string+'&limit=100&status=adoptable,adopted&page='+str(i+1)
    next_animals = requests.get(page_animals, headers=headers).json()
    all_animals = next_animals.get('animals') + all_animals

print('done')

animals_df = pd.DataFrame(all_animals)

animals_df.drop(['breeds', 'colors','size', 'coat', 'attributes', 'environment', 'tags','organization_animal_id', 'photos',
       'primary_photo_cropped', 'videos','distance', 'contact', '_links'], axis=1, inplace=True)

animals_df.to_csv(r'petfinder_results.csv')