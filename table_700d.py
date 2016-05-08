import csv
import os
import ast,sys
import datetime


#updated_by_yi

file_path = '../input/' + sys.argv[1] + '/dict_'+sys.argv[1]+'.csv'

def get_top2(x):
	return x[:2]


now = datetime.datetime.now()
date = now.strftime("%Y%m%d")
file_path = "../tmp/" + sys.argv[1] + "/gen_700_750_output" + "_" + date + ".csv"

with open(file_path) as csvfile:
	csv_opened = csv.DictReader(csvfile)
	findallone = []
	findalltwo = []
	for row in csv_opened:
		findallone += ast.literal_eval(row["only_in_one"])
		findalltwo += ast.literal_eval(row["only_in_other"])

file_path = "../tmp/" + sys.argv[1] + "/gen_700_750_output" + "_" + date + ".csv"
with open(file_path) as csvfile:
	csv_opened = csv.DictReader(csvfile)
	findall_withindex_first = []
	for row in csv_opened:
		for d in ast.literal_eval(row["only_in_one"]):
			findall_withindex_first += [(row["two_bundle_index"] ,d ,row["ItemID(LIsting_id)"], row["camera_model"], row["deltaN"], 1)]

file_path = "../tmp/" + sys.argv[1] + "/gen_700_750_output" + "_" + date + ".csv"
with open(file_path) as csvfile:
	csv_opened = csv.DictReader(csvfile)
	findall_withindex_second = []
	for row in csv_opened:
		for d in ast.literal_eval(row["only_in_other"]):
			findall_withindex_second += [(row["two_bundle_index"] ,d ,row["ItemID(LIsting_id)"], row["camera_model"], row["deltaN"], 2)]

# pair = []
# for index in indexID_all:
# 	count = 0
# 	for i in findall:
# 		if index == i:
# 			count += 1
# 	pair += [(index, count)]




fn = ["ItemID", "two_bundle_index", "camera_model", "deltaN", "dic_index", "dictIdx_top2", "first"]

output = "../tmp/" + sys.argv[1] + "/dictIdx_table" + "_" + date + ".csv"
with open(output, 'w') as csvfile:
		writer = csv.DictWriter(csvfile, fieldnames = fn)
		writer.writeheader()
		for i in findall_withindex_first:
			writer.writerow({'ItemID': i[2] ,'two_bundle_index': i[0], 
				"camera_model": i[3], 'deltaN': i[4], 'dic_index': i[1], 
				'dictIdx_top2': get_top2(i[1]), 'first': i[5]})
		for i in findall_withindex_second:
			writer.writerow({'ItemID': i[2] ,'two_bundle_index': i[0], 
				"camera_model": i[3], 'deltaN': i[4], 'dic_index': i[1], 
				'dictIdx_top2': get_top2(i[1]), 'first': i[5]})




# fn = ["index"]

# with open('../tmp/forgraph.csv', 'w') as csvfile:
# 		writer = csv.DictWriter(csvfile, fieldnames = fn)
# 		writer.writeheader()
# 		for i in findall:
# 			writer.writerow({'index': i})

# fn = ["bundle_index", "dic_index"]
# with open('../tmp/for_individual_graph.csv', 'w') as csvfile:
# 		writer = csv.DictWriter(csvfile, fieldnames = fn)
# 		writer.writeheader()
# 		for i in findall_withindex:
# 			writer.writerow({'bundle_index': i[0], 'dic_index': i[1]})


