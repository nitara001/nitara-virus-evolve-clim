!pip install opencage
import pandas as pd
from opencage.geocoder import OpenCageGeocode

key = 'ddda7e0f1a7843048c205b5f9afef55d'
  # get api key from:  https://opencagedata.com



geocoder = OpenCageGeocode(key)
#import data
list_lat = []   # create empty lists

list_long = []

for index, row in data.iterrows(): # iterate over rows in dataframe



    City = row['City']
    Country = row['Country']
    query = str(City)+','+str(Country)

    results = geocoder.geocode(query)
    lat = results[0]['geometry']['lat']
    long = results[0]['geometry']['lng']

    list_lat.append(lat)
    list_long.append(long)


# create new columns from lists

data['lat'] = list_lat

data['lon'] = list_long
#export data