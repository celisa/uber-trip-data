import pandas as pd
import numpy as np
from datasets import load_dataset

data_apr_14 = pd.read_csv(
    "https://raw.githubusercontent.com/fivethirtyeight/uber-tlc-foil-response/master/uber-trip-data/uber-raw-data-apr14.csv"
)
data_may_14 = pd.read_csv(
    "https://raw.githubusercontent.com/fivethirtyeight/uber-tlc-foil-response/master/uber-trip-data/uber-raw-data-may14.csv"
)
data_jun_14 = pd.read_csv(
    "https://raw.githubusercontent.com/fivethirtyeight/uber-tlc-foil-response/master/uber-trip-data/uber-raw-data-jun14.csv"
)
data_jul_14 = pd.read_csv(
    "https://raw.githubusercontent.com/fivethirtyeight/uber-tlc-foil-response/master/uber-trip-data/uber-raw-data-jul14.csv"
)
data_aug_14 = pd.read_csv(
    "https://raw.githubusercontent.com/fivethirtyeight/uber-tlc-foil-response/master/uber-trip-data/uber-raw-data-aug14.csv"
)
data_sep_14 = pd.read_csv(
    "https://raw.githubusercontent.com/fivethirtyeight/uber-tlc-foil-response/master/uber-trip-data/uber-raw-data-sep14.csv"
)

data14 = pd.concat(
    [data_apr_14, data_may_14, data_jun_14, data_jul_14, data_aug_14, data_sep_14],
    ignore_index=True,
)

weather_data = pd.read_csv(
    "https://raw.githubusercontent.com/celisa/uber-trip-data/main/3115153.csv"
)

weather_data = weather_data[weather_data["STATION"] == "USW00094789"]

# cast 'DATE' column as datetime
weather_data["DATE"] = pd.to_datetime(weather_data["DATE"]).dt.date
data14["DATE"] = pd.to_datetime(data14["Date/Time"]).dt.date

# report # of uber trips per day
uber_trips = data14.groupby("DATE").size().reset_index(name="count")

# join uber trips and weather data
uber_weather_df = uber_trips.merge(weather_data, on="DATE", how="left")

# rename 'count' column to 'num_uber_trips'
uber_weather_df.rename(columns={"count": "num_uber_trips"}, inplace=True)

# read in uber 2015 data

data15 = pd.read_csv(
    "https://raw.githubusercontent.com/celisa/uber-trip-data/main/uber-data-2015.csv"
)

uber_trips_15 = data15.groupby("Pickup_date").size().reset_index(name="count")

# convert 'Pickup_date' column to datetime
uber_trips_15["Pickup_date"] = pd.to_datetime(uber_trips_15["Pickup_date"]).dt.date

uber_trips_15.rename(columns={"count": "num_uber_trips"}, inplace=True)
uber_trips_15.rename(columns={"Pickup_date": "DATE"}, inplace=True)


# join with weather data
uber_trips_15 = uber_trips_15.merge(weather_data, on="DATE", how="left")

uber_weather_df = pd.concat([uber_weather_df, uber_trips_15], ignore_index=True)

uber_weather_df.to_csv("uber-data-cleaned.csv", index=False)
print(len(uber_weather_df))
