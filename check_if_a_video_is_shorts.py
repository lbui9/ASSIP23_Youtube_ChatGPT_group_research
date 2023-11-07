import requests
import pandas as pd

def check_if_shorts(video_id):
    url = f"https://www.youtube.com/shorts/{video_id}"
    response = requests.head(url)
    return response.status_code == 200

dataset_path = "Whole_Sample.csv"
df = pd.read_csv(dataset_path)

# create an empty list to store the results
shorts_list = []

# loop through each video ID, check if it's a Shorts, and update the result list
for video_id in df['Video ID']:
    is_shorts = check_if_shorts(video_id)
    shorts_list.append(is_shorts)

# add the results to a new column
df['Shorts?'] = shorts_list
df.to_csv(dataset_path, index=False)