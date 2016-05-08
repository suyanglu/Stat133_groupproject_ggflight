import csv
import datetime
import os
import sys


#yi_has_deleted_all_things_about_750d


#updated_by_yi
filepath_700d = '../input/' + sys.argv[1] + '/lst_' + sys.argv[1] + '.csv'

# Compare two array and return the difference
def comp_array(one, other):
	count = 0
	only_in_one = []
	only_in_other = []
	for i in other:
		if i not in one:
			count += 1
			only_in_other += [i]

	for i in one:
		if i not in other:
			count += 1
			only_in_one += [i]

	in_both = only_in_one + only_in_other

	return (count, only_in_one, only_in_other, in_both)



def get_top2(x):
	result = []
	for i in x:
		result += [i[:2]]
	return result


######################################################
######################################################


# Get unique ItemId for 700d
with open(filepath_700d) as csv700d:
	csv_opened = csv.DictReader(csv700d)
	itemID700d_all = []
	for row in csv_opened:
		if row["ItemID(LIsting_id)"] not in itemID700d_all and row["ItemID(LIsting_id)"] != "":
			itemID700d_all += [row["ItemID(LIsting_id)"]]
l_700d = len(itemID700d_all)
#print('l_700d:', l_700d)
#print(itemID700d_all)


######################################################
######################################################

# get each id's dict_yi_index when bundle_index = 1 in 700d
with open(filepath_700d) as csvfile:
	csv_opened = csv.DictReader(csvfile)
	dict_index700d_1 = []
	for itemID in itemID700d_all:
		csvfile.seek(0)
		temp_dict_index = []
		for row in csv_opened:
			if row["ItemID(LIsting_id)"] == itemID and row["bundle_index"] == "1" and row["dict_yi_index"] not in temp_dict_index:
				temp_dict_index += [row["dict_yi_index"]]
		dict_index700d_1 += [temp_dict_index]
#print('should be', l_700d, len(dict_index700d_1))
# for i in dict_index700d_1:
# 	print(i)

# get each id's dict_yi_index when bundle_index = 2 in 700d
with open(filepath_700d) as csvfile:
	csv_opened = csv.DictReader(csvfile)
	dict_index700d_2 = []
	for itemID in itemID700d_all:
		csvfile.seek(0)
		temp_dict_index = []
		for row in csv_opened:
			if row["ItemID(LIsting_id)"] == itemID and row["bundle_index"] == "2" and row["dict_yi_index"] not in temp_dict_index:
				temp_dict_index += [row["dict_yi_index"]]
		dict_index700d_2 += [temp_dict_index]
#print('should be', l_700d, len(dict_index700d_2))


# get each id's dict_yi_index when bundle_index = 3 in 700d
with open(filepath_700d) as csvfile:
	csv_opened = csv.DictReader(csvfile)
	dict_index700d_3 = []
	for itemID in itemID700d_all:
		csvfile.seek(0)
		temp_dict_index = []
		for row in csv_opened:
			if row["ItemID(LIsting_id)"] == itemID and row["bundle_index"] == "3" and row["dict_yi_index"] not in temp_dict_index:
				temp_dict_index += [row["dict_yi_index"]]
		dict_index700d_3 += [temp_dict_index]
#print('should be', l_700d, len(dict_index700d_3))


# get each id's dict_yi_index when bundle_index = 4 in 700d
with open(filepath_700d) as csvfile:
	csv_opened = csv.DictReader(csvfile)
	dict_index700d_4 = []
	for itemID in itemID700d_all:
		csvfile.seek(0)
		temp_dict_index = []
		for row in csv_opened:
			if row["ItemID(LIsting_id)"] == itemID and row["bundle_index"] == "4" and row["dict_yi_index"] not in temp_dict_index:
				temp_dict_index += [row["dict_yi_index"]]
		dict_index700d_4 += [temp_dict_index]
#print('should be', l_700d, len(dict_index700d_4))


# get each id's dict_yi_index when bundle_index = 5 in 700d
with open(filepath_700d) as csvfile:
	csv_opened = csv.DictReader(csvfile)
	dict_index700d_5 = []
	for itemID in itemID700d_all:
		csvfile.seek(0)
		temp_dict_index = []
		for row in csv_opened:
			if row["ItemID(LIsting_id)"] == itemID and row["bundle_index"] == "5" and row["dict_yi_index"] not in temp_dict_index:
				temp_dict_index += [row["dict_yi_index"]]
		dict_index700d_5 += [temp_dict_index]
#print('should be', l_700d, len(dict_index700d_5))


# get each id's dict_yi_index when bundle_index = 6 in 700d
with open(filepath_700d) as csvfile:
	csv_opened = csv.DictReader(csvfile)
	dict_index700d_6 = []
	for itemID in itemID700d_all:
		csvfile.seek(0)
		temp_dict_index = []
		for row in csv_opened:
			if row["ItemID(LIsting_id)"] == itemID and row["bundle_index"] == "6" and row["dict_yi_index"] not in temp_dict_index:
				temp_dict_index += [row["dict_yi_index"]]
		dict_index700d_6 += [temp_dict_index]
#print('should be', l_700d, len(dict_index700d_6))


# get each id's dict_yi_index when bundle_index = 7 in 700d
with open(filepath_700d) as csvfile:
	csv_opened = csv.DictReader(csvfile)
	dict_index700d_7 = []
	for itemID in itemID700d_all:
		csvfile.seek(0)
		temp_dict_index = []
		for row in csv_opened:
			if row["ItemID(LIsting_id)"] == itemID and row["bundle_index"] == "7" and row["dict_yi_index"] not in temp_dict_index:
				temp_dict_index += [row["dict_yi_index"]]
		dict_index700d_7 += [temp_dict_index]
#print('should be', l_700d, len(dict_index700d_7))


# get each id's dict_yi_index when bundle_index = 8 in 700d
with open(filepath_700d) as csvfile:
	csv_opened = csv.DictReader(csvfile)
	dict_index700d_8 = []
	for itemID in itemID700d_all:
		csvfile.seek(0)
		temp_dict_index = []
		for row in csv_opened:
			if row["ItemID(LIsting_id)"] == itemID and row["bundle_index"] == "8" and row["dict_yi_index"] not in temp_dict_index:
				temp_dict_index += [row["dict_yi_index"]]
		dict_index700d_8 += [temp_dict_index]
#print('should be', l_700d, len(dict_index700d_8))



##############################################################################################
##############################################################################################

now = datetime.datetime.now()
date = now.strftime("%Y%m%d")
output = '../tmp/' + sys.argv[1] + '/gen_700_750_output' + '_' + date + '.csv'
##############################################################################################
##############################################################################################

fn = ['Author', 'ItemID(LIsting_id)', 'camera_model', 'two_bundle_index', 'deltaN', 'only_in_one', 'only_in_other', 'in_both']
an = "Jiahao"
with open(output, 'w') as csvfile:
		writer = csv.DictWriter(csvfile, fieldnames = fn)
		writer.writeheader()
		for i in range(0,7):
			for j in range(i + 1, 8):
				for idx in range(0, l_700d):

					array = [dict_index700d_1, dict_index700d_2, dict_index700d_3, 
					dict_index700d_4, dict_index700d_5, dict_index700d_6, dict_index700d_7, dict_index700d_8]

					one = array[i][idx]
					other = array[j][idx]

					if one != [] or other != []:
						o = comp_array(one, other)
						writer.writerow({'Author': an, 
							'ItemID(LIsting_id)': str(itemID700d_all[idx]), 
							'camera_model': "700d",
							'two_bundle_index': [i + 1, j + 1], 
							'deltaN': o[0], 
							'only_in_one': o[1], 
							'only_in_other': o[2], 
							'in_both': o[3]})


	
##############################################################################################
##############################################################################################















