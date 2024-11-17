#!/usr/local/bin/python

import pandas as pd
import subprocess
import os

# Choose job
job = "dl"  # "dl" for downloading, "rename" for renaming files

# URL configuration
base_url = "https://bimsa.net/bimsavideo.php?id="
temp_name_prefix = "bimsavideo-bimsavideo.phpid="

# Items
dl_list = "list.csv"
rename_prefix = 'sml-II-'

# Read the CSV file
data = pd.read_csv(dl_list, header=None)
ids = data[0].tolist()
names = data[1].tolist()
max_index = len(ids)
print(f"Total #items: {max_index}")

print(f"Task [{job}] starts ...")

if job == "dl":
    # -------------------------------------------
    # Downloading
    # -------------------------------------------
    for index in range(max_index):
        id = ids[index]
        url = f"{base_url}{id}.mp4"
        print(url)

        # Downloading
        print(f">>> [{index + 1}/{max_index}]")
        subprocess.run(["youtube-dl", "-f", "best", url])
        print(f"{id}.mp4: done<<<\n")

elif job == "rename":
    # -------------------------------------------
    # Update file names
    # -------------------------------------------
    print("List of videos to be renamed: [OLD >>> NEW]")
    print("--------------------------------------")
    for index in range(max_index):
        id = ids[index]
        name = names[index]
        print(f"id: {id}    >>>    p{index + 1}.{rename_prefix}{name}.mp4")

    # Confirm to perform
    confirm = input("Are you sure? (y/n): ")
    if confirm.lower() == 'y':
        for index in range(max_index):
            id = ids[index]
            name = names[index]

            # Renaming
            old_name = f"{temp_name_prefix}{id}.php"
            new_name = f"p{index + 1}.{rename_prefix}{name}.mp4"
            os.rename(old_name, new_name)

        print("Video renaming [done]")

