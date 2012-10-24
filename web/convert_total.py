#!/usr/bin/python
import csv
import time
import datetime

def conv(s):
	return str(time.mktime(datetime.datetime.strptime(s, "%Y-%m-%d").timetuple()) * 1000)

csv_file = csv.DictReader(open('./ubuntu-bugs.csv', 'rb'), delimiter=',', quotechar='"')

totals = { "key": "All Open Bugs", "values": [] }
closed = { "key": "All Closed Bugs", "values": [] }
fix_closed = { "key":"All Fix Released Bugs", "values": [] }

for line in csv_file:
    totals["values"].append([conv(line["date"]), int(line["value"])])
    closed["values"].append([conv(line["date"]), int(line["closed"])])
    fix_closed["values"].append([conv(line["date"]), int(line["closed_fixed"])])

all_series = []
all_series.append(totals)
all_series.append(closed)
all_series.append(fix_closed)

print all_series
