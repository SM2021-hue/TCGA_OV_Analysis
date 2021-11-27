
#################################	REMOVING	##################################
#																			          #											
# THIS IS THE SCRIPT FOR REMOVING UNNACESSARY IMAGES FROM SPLITTED IMAGES      #
#																				 #											
#################################################################################

import os

path_fol_in="/home/arraygen/Desktop/Image_Processing_PART_3/A"



def fun(path_fol,out_fol_path):
	#print(path_fol)
	small_size=0
	for root, dirs, files in os.walk(path_fol):
		d = {} # intialize dict
		for fn in files:
			path = os.path.join(root, fn)
			size = os.stat(path).st_size
			# capture file name and size for files in root
			d[fn] = size
		# some folders may be empty
		if d:
			# get the file name of the file with the smallest size
			smallestfile = min(d, key=d.get)
			#print(root, smallestfile, d[smallestfile])
			small_size=d[smallestfile]

	for k,v in d.items():
		if(small_size==v):	
			#print(k,v)
			path_file_del=path_fol+"/"+k
			cmd="mv "+path_file_del+" "+out_fol_path
			os.system(cmd)






fol_list=os.listdir(path_fol_in)


small_cnt=0
fol_i=0
while(fol_i<len(fol_list)):
	name_of_fol=fol_list[fol_i]
	print(name_of_fol)
	small_cnt=int(input("\n\nplz Enter a No How many times it'll search for small size file in Folder Or 0 to do nothing : "))

	#name_of_fol="TCGA-23-2647-01Z-00-DX1.21E5D0D8-6BA8-4D49-BA70-EC228CF10387"

	input_fol_path=path_fol_in+"/"+name_of_fol

	out_fol_path=path_fol_in.strip(path_fol_in.split("/")[-1])+"OUTPUT_"+name_of_fol
	print(out_fol_path)

	try:
		os.mkdir(out_fol_path)
	except:
		print("Folder Already exists")

	for i in range(0,small_cnt):
		fun(input_fol_path,out_fol_path)
	
	if(small_cnt==0):
		fol_i=fol_i+1




















