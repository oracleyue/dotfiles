#!/usr/bin/env python3
# -*- coding: utf-8 -*-

# Copyright [2017] <oracleyue>
# Last modified on 25 May 2018

"""
This script is to convert the exported settings of SpeedDial on Firefox to the settings in json for Speed Dial 2 on Chrome.
"""


import re
import json
import urllib.parse
# import urllib.request
# import lxml.html


# ====================================================
# Meta Information
# ====================================================

spFile = './SpeedDial-Firefox.CurrentSettings'
jsonFile = './SpeedDial2-Chrome.json'

# Meta information from SpeedDial in Firefox
# note: 'Home' is not needed.
groupList = ['Linux', 'Acad', 'Wiki', 'Courses', 'Linux_pl',
			 'BBS_BT', "Tools", 'CHINA', 'Cheatsheets']
numColumn = 5;
numPerPage = 20;


# ====================================================
# member functions
# ====================================================

def extractURL(fileName):
	"To extract data (titles, urls) from the exported settings of SpeedDial on Firefox."

	# group info
	fid = open(fileName)
	matchG = re.search('numGroups=([0-9]+)', fid.read())
	numGroups = int(matchG.group(1))
	fid.close()
	groupList = ['']*numGroups

	with open(fileName) as fid:
		lines = fid.readlines()

	for line in lines:
		matchT = re.match('group-([0-9]+)-title=', line)
		matchC = re.match('group-1-columns=', line)
		matchR = re.match('group-1-rows=', line)
		matchG = re.match('numGroups=', line)

		if matchT:
			index = int(matchT.group(1)) - 1
			groupList[index] = line.replace(matchT.group(0),'').replace('\n','')
		elif matchC:
			numColumns = int(line.replace(matchC.group(0), ''))
		elif matchR:
			numRows = int(line.replace(matchR.group(0), ''))
		elif matchG:
			break
	del groupList[0]

	# dials info
	urlList = [''] * (numGroups * numColumns * numRows)
	labelList = [''] * (numGroups * numColumns * numRows)
	for line in lines:
		matchU = re.match('thumbnail-([0-9]+)-url=', line)
		if matchU:
			strID = matchU.group(1)
			strUrl = line.replace(matchU.group(0), '').replace('\n','')
			urlList[int(strID)-1] = strUrl
		# use titles from SpeedDial.CurrentSettings
		matchL = re.match('thumbnail-([0-9]+)-label=', line)
		if matchL:
			strID = matchL.group(1)
			strLabel = line.replace(matchL.group(0), '').replace('\n','')
			strLabel = urllib.parse.unquote(strLabel)
			labelList[int(strID)-1] = strLabel

	# output data in dict
	data = {}
	data['numRows'] = numRows
	data['numColumns'] = numColumns
	data['numGroups'] = numGroups
	data['groupList'] = groupList
	data['labelList'] = labelList
	data['urlList'] = urlList

	return data


def init_entry():
	"This is to initialize an entry of /dials/ for SpeedDial2 in Chrome."

	entry = {}
	entry['id'] = 0
	entry['idgroup'] = 0
	entry['position'] = 999
	entry['thumbnail'] = ''
	entry['title'] = 'xxxxxx'
	entry['ts_created'] = 1500000000
	entry['url'] = 'xxxxxx'
	entry['visits'] = 0
	entry['visits_afternoon'] = 0
	entry['visits_evening'] = 0
	entry['visits_morning'] = 0
	entry['visits_night'] = 0
	return entry


# ====================================================
# Main
# ====================================================

# Load data
rawdata = extractURL(spFile)

# Reformat data in dict for json
data = {}

# groups
data['groups'] = {}
groupList = rawdata['groupList']
for index in range(len(groupList)):
	entry = {}
	entry['color'] = ''
	entry['id'] = index + 1
	entry['position'] = index + 1
	entry['title'] = groupList[index]
	data['groups'][str(index)] = entry
	index = index + 1

# dials
data['dials'] = {}
labelList = rawdata['labelList']
urlList = rawdata['urlList']
for index in range(len(urlList)):
	entry = init_entry()
	entry['id'] = index + 1
	entry['idgroup'] = index // numPerPage
	entry['ts_created'] += index
	entry['url'] = urlList[index]
	# # retrieve webpage title online
	# page = urllib.request.urlopen(urlList[index])
	# html = lxml.html.parse(page)
	# entry['title'] = html.find(".//title").text
	# print(entry['title'])
	# # use webpage names from SpeedDial (firefox)
	if labelList[index]:
		entry['title'] = labelList[index]
	else:
		# Speeddial(firefox) use empty labels to remove dials
		entry['url'] = 'xxxxxx'
	data['dials'][str(index)] = entry

# settings
# with open(jsonDemo, 'r') as fdemo:
# 	dataDemo = json.load(fdemo)
# 	data['settings'] = dataDemo['settings']
data['settings'] = {
    "firstTime": "false",
    "options.alwaysNewTab": "0",
    "options.appOrder": "[\"pjkljhegncpnkpknbcohdijeoejaedia\",\"ejjicmeblgpmajnghnpcppodonldlgfn\",\"laohedokgllahcfkeilpoahlplcpfbhh\",\"apdfllckaahabafndbhieahigkjlhalf\",\"bfmbadcfdhiklafcdohpfphhhakmiakk\",\"lneaknkopdijkpnocmklfnjbeapigfbh\",\"pjjhlfkghdhmijklfnahfkpgmhcmfgcm\",\"blpcfgokakmgnkcojhhkbfbldkacnbeo\",\"coobgpohoikkiipiblmjeljniedjpjpf\",\"ahfgeienlihckogmohjhadlkjgocpleb\"]",
    "options.apps.show": "0",
    "options.apps.theme": "dark",
    "options.background": "images/themes/light/background_top.jpg",
    "options.backgroundPattern": "images/themes/light/background_strip.jpg",
    "options.backgroundPosition": "right top",
    "options.backgroundSize": "contain",
    "options.centerThumbnailsVertically": "1",
    "options.centerVertically": "1",
    "options.colors.bg": "fff",
    "options.colors.border": "CCCCCC",
    "options.colors.borderover": "999999",
    "options.colors.dialbg": "FFFFFF",
    "options.colors.dialbginner": "FFFFFF",
    "options.colors.dialbginnerover": "FFFFFF",
    "options.colors.dialbgover": "FFFFFF",
    "options.colors.title": "8C7E7E",
    "options.colors.titleover": "333333",
    "options.columns": "5",
    "options.darkTheme": "0",
    "options.defaultGroupColor": "#FFF",
    "options.defaultGroupName": "Home",
    "options.dialSpace": "72",
    "options.dialspacing": "12",
    "options.dialstyle.corners": "4",
    "options.dialstyle.shadow": "glow",
    "options.fontface": "Helvetica,\"Helvetica Nueue\";arial,sans-serif",
    "options.fontsize": "11",
    "options.fontstyle": "font-weight:normal;font-style:normal;",
    "options.highlight": "0",
    "options.installation_time": "1435074624",
	"options.keepActiveGroup": "1",
    "options.order": "position",
    "options.padding": "4",
    "options.refreshThumbnails": "0",
    "options.repeatbackground": "repeat-x",
    "options.scrollLayout": "0",
    "options.showAddButton": "1",
    "options.showContextMenu": "1",
    "options.showOptionsButton": "1",
    "options.showSearch": "1",
    "options.showTitle": "1",
    "options.showVisits": "1",
    "options.sidebar": "1",
    "options.sidebar.showApps": "1",
    "options.sidebar.showBookmarks": "1",
    "options.sidebar.showHistory": "0",
    "options.thumbnailQuality": "medium",
    "options.thumbnailRatio": "1",
    "options.titleAlign": "center",
    "refreshThumbnail": "",
    "refresh_create": "false",
    "refresh_id": "",
    "refresh_url": "",
    "requestThumbnail": "5|||https://sites.google.com/site/teachingmeng/Home/math-4223",
    "sys.cellspacing": "12",
    "sys.cols": "5",
    "sys.containerwidth": "828px",
    "sys.dialheight": "136.5",
    "sys.dialwidth": "198",
    "sys.groups": "1",
    "sys.rows": "4",
    "sys.rowspacing": "12"
}

# Export
with open(jsonFile, 'w') as fout:
	json.dump(data, fout)
