#!/usr/bin/python
import csv
import time
import datetime

#keys = ["warty","hoary","breezy","dapper","edgy","feisty","gutsy","hardy","intrepid","jaunty","karmic","lucid","maverick","natty","oneiric","precise","quantal"]
keys = ["lucid","maverick","natty","oneiric","precise","quantal"]

def conv(s):
	return str(time.mktime(datetime.datetime.strptime(s, "%Y-%m-%d").timetuple()) * 1000)

all_series = []
for k in keys:
	csv_file = csv.DictReader(open('./ubuntu-bugs.csv', 'rb'), delimiter=',', quotechar='"')
	totals = { "key": k, "values": [] }
	for line in csv_file:
                if (int(line[k]) > 0):
			totals["values"].append([conv(line["date"]), int(line[k])])
	all_series.append(totals)

print all_series
