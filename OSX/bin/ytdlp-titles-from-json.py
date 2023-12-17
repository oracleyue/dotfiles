#!/usr/bin/env python

import os
import json
import re

jsondir = "./"
flist = os.listdir(jsondir)
files = sorted(flist)

prefix_str = input("Type the prefix string to be removed:")

outfile = open("titles.txt", "w")
for f in files:
    if f.endswith(".json"):
        infile = open(os.path.join(jsondir, f))
        data = json.load(infile)
        title = data['title']+'.mp4'
        title_str = re.sub(r'^{0}'.format(prefix_str), '', title)
        outfile.write(title_str+'\n')
    else:
        continue

outfile.close()


