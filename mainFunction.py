
def mainFunctionPy_upto4(pathGlobal,alpha1,alpha2,alpha3,alpha4,pc):
	import re
	import pandas
	import os
	import copy
	import numpy
	

	files  = os.listdir(pathGlobal)
	if '.DS_Store' in files:
		files.remove('.DS_Store')

	count1 = {el:0 for el in alpha1}
	count2 = {el:0 for el in alpha2}
	count3 = {el:0 for el in alpha3}
	count4 = {el:0 for el in alpha4}

	entropy1 = {el:0 for el in alpha1}
	entropy2 = {el:0 for el in alpha2}
	entropy3 = {el:0 for el in alpha3}
	entropy4 = {el:0 for el in alpha4}

	Count = {el:[copy.deepcopy(count1),copy.deepcopy(count2),copy.deepcopy(count3),copy.deepcopy(count4)] for el in files}
	C2 = {el:copy.deepcopy(entropy2) for el in files}
	F1 = {el:copy.deepcopy(count1) for el in files}
	F2 = {el:copy.deepcopy(count2) for el in files}
	F3 = {el:copy.deepcopy(count3) for el in files}
	F4 = {el:copy.deepcopy(count4) for el in files}

	H = {el:[copy.deepcopy(entropy1),copy.deepcopy(entropy2),copy.deepcopy(entropy3),copy.deepcopy(entropy4)] for el in files}
	entropylevels = {el:{"H0":0,"H1":0, "H2":0, "H3":0,  "H4":0} for el in files}


	for j in range(len(files)):
		with open(str(pathGlobal)+"/"+str(files[j])) as f:
			for line in f:
				for i in range(len(Count[files[j]])):
					for call in Count[files[j]][i].keys():
						if i == 1:
							Count[files[j]][i][call]  += line.count(call)
							C2[files[j]][call] += line.count(call)
						Count[files[j]][i][call]  += line.count(call)
		entropylevels[files[j]]["H0"] = numpy.log2(numpy.count_nonzero(Count[files[j]][0].values()))

	
	Count2_DF = pandas.DataFrame(C2)
	Count2_DF.to_csv(pathGlobal+"Counts.csv")
	
	probability_1 = {el:copy.deepcopy(count1) for el in files}

	for k in probability_1.keys():
		for call in probability_1[k].keys():
			if (float(pc) == 0 and sum(Count[k][0].values()) == 0):
				prob = 0
			else:
				prob = (float(Count[k][0][call])+float(pc))/(sum(Count[k][0].values())+float(pc)*len(alpha1))

			probability_1[k][call] =  prob
			F1[k][call] = prob
	Freq1_DF = pandas.DataFrame(F1)
	

	probability_2 = {el:copy.deepcopy(count2) for el in files}

	for k in probability_2.keys():
		for call in probability_2[k].keys():
			getfirst = re.split("\t", call)[0]
			allwithFirst = []
			for element in alpha1:
				allwithFirst.append(str(getfirst+"\t"+element))
			subset = dict((r, Count[k][1][r]) for r in allwithFirst if r in Count[k][1])
			if (float(pc) ==0 and  sum(subset.values()) == 0):
				prob = 0
			else:
				prob = (float(Count[k][1][call])+float(pc))/(sum(subset.values())+float(pc)*len(alpha2))
			probability_2[k][call] =  prob
			F2[k][call] = prob
	Freq2_DF = pandas.DataFrame(F2)
	

	probability_3 = {el:copy.deepcopy(count3) for el in files}

	for k in probability_3.keys():
		for call in probability_3[k].keys():
			gettwo = re.split("\t", call)
			gettwo = gettwo[0]+"\t"+gettwo[1]
			allwithFirstTwo = []
			for element in alpha1:
				allwithFirstTwo.append(str(gettwo+"\t"+element))
			subset = dict((r, Count[k][2][r]) for r in allwithFirstTwo if r in Count[k][2])
			if (float(pc) ==0 and sum(subset.values()) == 0):
				prob = 0
			else:
				prob = (float(Count[k][2][call])+float(pc))/(sum(subset.values())+float(pc)*len(alpha3))
			probability_3[k][call] =  prob
			F3[k][call] = prob
	Freq3_DF = pandas.DataFrame(F3)
	

	probability_4 = {el:copy.deepcopy(count4) for el in files}

	for k in probability_4.keys():
		for call in probability_4[k].keys():
			getthree = re.split("\t", call)
			getthree = getthree[0]+"\t"+getthree[1]+"\t"+getthree[2]

			allwithFirstThree = []
			for element in alpha1:
				allwithFirstThree.append(str(getthree+"\t"+element))
			subset = dict((r, Count[k][3][r]) for r in allwithFirstThree if r in Count[k][3])
			if (float(pc) ==0 and sum(subset.values()) == 0):
				prob = 0
			else:
				prob = (float(Count[k][3][call])+float(pc))/(sum(subset.values())+float(pc)*len(alpha4))
			probability_4[k][call] =  prob
			F4[k][call] = prob
	Freq4_DF = pandas.DataFrame(F4)


	entropy_1 = {el:copy.deepcopy(entropy1) for el in files}
	print "Entropy H1"
	for k in entropy_1.keys():
		for call in entropy_1[k].keys():
			if numpy.log2(probability_1[k][call]) != float("-inf"):
				entropy_1[k][call] = probability_1[k][call] * numpy.log2(probability_1[k][call])
			else:
				entropy_1[k][call] = 0
		entropylevels[k]["H1"] = (-1*sum(entropy_1[k].values()))
			 


	entropy_2 = {el:copy.deepcopy(entropy2) for el in files}
	print "Entropy H2"
	for k in entropy_2.keys():

		for call in entropy_2[k].keys():

			getfirst = re.split("\t", call)[0]
			if numpy.log2(probability_2[k][call]) != float("-inf"):
				entropy_2[k][call] = probability_1[k][getfirst]*probability_2[k][call] * numpy.log2(probability_2[k][call])
			else:
				entropy_2[k][call] = 0
		entropylevels[k]["H2"] = (-1*sum(entropy_2[k].values()))


	entropy_3 = {el:copy.deepcopy(entropy3) for el in files}
	print "Entropy H3"
	for k in entropy_3.keys():
		
		for call in entropy_3[k].keys():
		
			getfirst = re.split("\t", call)[0]
			gettwo = re.split("\t", call)
			gettwo = gettwo[0]+"\t"+gettwo[1]
			if numpy.log2(probability_3[k][call]) != float("-inf"):
				print numpy.log2(probability_3[k][call])
				entropy_3[k][call] = probability_1[k][getfirst]*probability_2[k][gettwo]* probability_3[k][call]* numpy.log2(probability_3[k][call])
			else:
				entropy_3[k][call] = 0
		entropylevels[k]["H3"] = (-1*sum(entropy_3[k].values()))

	entropy_4 = {el:copy.deepcopy(entropy4) for el in files}
	print "Entropy H4"
	for k in entropy_4.keys():
		
		for call in entropy_4[k].keys():
			
			getfirst = re.split("\t", call)[0]
			getSplit = re.split("\t", call)
			gettwo = getSplit[0]+"\t"+getSplit[1]
			getthree = getSplit[0]+"\t"+getSplit[1]+"\t"+getSplit[2]
			if numpy.log2(probability_4[k][call]) != float("-inf"):
				entropy_4[k][call] = probability_1[k][getfirst]*probability_2[k][gettwo]* probability_3[k][getthree]*probability_4[k][call]* numpy.log2(probability_4[k][call])
			else:
				entropy_4[k][call] = 0
		entropylevels[k]["H4"] = (-1*sum(entropy_4[k].values()))


	Entropy_DF = pandas.DataFrame(entropylevels)

	tEntropy_DF = Entropy_DF.transpose()

	meanCounts = []	
	observations = []

	for call in Count2_DF.transpose():

		meanCounts.append([call,numpy.mean(Count2_DF.transpose()[call])])
		observations.extend(call.split("\t")* int(numpy.mean(Count2_DF.transpose()[call])))
	observations = filter(None, observations)
	meanCounts2 = pandas.DataFrame(meanCounts,columns = ["Call","Mean"]) 
	Freq1_DF = Freq1_DF.transpose()
	Freq1_DF.columns = alpha1
	Freq1_DF = Freq1_DF.astype(float)


	Freq2_DF = Freq2_DF.transpose()
	Freq2_DF.columns = alpha2
	Freq2_DF = Freq2_DF.astype(float)

	Freq3_DF = Freq3_DF.transpose()
	Freq3_DF.columns = alpha3
	Freq3_DF = Freq3_DF.astype(float)


	Freq4_DF = Freq4_DF.transpose()
	Freq4_DF.columns = alpha4
	Freq4_DF = Freq4_DF.astype(float)
	


	return [Entropy_DF, meanCounts2, Freq1_DF, Freq2_DF,Freq3_DF,Freq4_DF, tEntropy_DF, observations]


def mainFunctionPy_upto3(pathGlobal,alpha1,alpha2,alpha3,pc):
	import re
	import pandas
	import os
	import copy
	import numpy
	

	files  = os.listdir(pathGlobal)
	if '.DS_Store' in files:
		files.remove('.DS_Store')

	count1 = {el:0 for el in alpha1}
	count2 = {el:0 for el in alpha2}
	count3 = {el:0 for el in alpha3}
	

	entropy1 = {el:0 for el in alpha1}
	entropy2 = {el:0 for el in alpha2}
	entropy3 = {el:0 for el in alpha3}
	

	Count = {el:[copy.deepcopy(count1),copy.deepcopy(count2),copy.deepcopy(count3)] for el in files}
	C2 = {el:copy.deepcopy(entropy2) for el in files}
	F1 = {el:copy.deepcopy(count1) for el in files}
	F2 = {el:copy.deepcopy(count2) for el in files}
	F3 = {el:copy.deepcopy(count3) for el in files}

	H = {el:[copy.deepcopy(entropy1),copy.deepcopy(entropy2),copy.deepcopy(entropy3)] for el in files}
	entropylevels = {el:{"H0":0,"H1":0, "H2":0, "H3":0} for el in files}


	for j in range(len(files)):
		with open(str(pathGlobal)+"/"+str(files[j])) as f:
			for line in f:
				for i in range(len(Count[files[j]])):
					for call in Count[files[j]][i].keys():
						if i == 1:
							Count[files[j]][i][call]  += line.count(call)
							C2[files[j]][call] += line.count(call)
						Count[files[j]][i][call]  += line.count(call)
		entropylevels[files[j]]["H0"] = numpy.log2(numpy.count_nonzero(Count[files[j]][0].values()))

	
	Count2_DF = pandas.DataFrame(C2)
	
	probability_1 = {el:copy.deepcopy(count1) for el in files}

	for k in probability_1.keys():
		for call in probability_1[k].keys():
			if (float(pc) ==0 and sum(Count[k][0].values()) == 0):
				prob = 0
			else:
				prob = (float(Count[k][0][call])+float(pc))/(sum(Count[k][0].values())+float(pc)*len(alpha1))

			probability_1[k][call] =  prob
			F1[k][call] = prob
	Freq1_DF = pandas.DataFrame(F1)
	

	probability_2 = {el:copy.deepcopy(count2) for el in files}

	for k in probability_2.keys():
		for call in probability_2[k].keys():
			getfirst = re.split("\t", call)[0]
			allwithFirst = []
			for element in alpha1:
				allwithFirst.append(str(getfirst+"\t"+element))
			subset = dict((r, Count[k][1][r]) for r in allwithFirst if r in Count[k][1])
			if (float(pc) == 0 and sum(subset.values()) == 0):
				prob = 0
			else:
				prob = (float(Count[k][1][call])+float(pc))/(sum(subset.values())+float(pc)*len(alpha2))
			probability_2[k][call] =  prob
			F2[k][call] = prob
	Freq2_DF = pandas.DataFrame(F2)
	

	probability_3 = {el:copy.deepcopy(count3) for el in files}

	for k in probability_3.keys():
		for call in probability_3[k].keys():
			gettwo = re.split("\t", call)
			gettwo = gettwo[0]+"\t"+gettwo[1]
			allwithFirstTwo = []
			for element in alpha1:
				allwithFirstTwo.append(str(gettwo+"\t"+element))
			subset = dict((r, Count[k][2][r]) for r in allwithFirstTwo if r in Count[k][2])
			if (float(pc) ==0 and sum(subset.values()) == 0):
				prob = 0
			else:
				prob = (float(Count[k][2][call])+float(pc))/(sum(subset.values())+float(pc)*len(alpha3))
			probability_3[k][call] =  prob
			F3[k][call] = prob
	Freq3_DF = pandas.DataFrame(F3)
	


	entropy_1 = {el:copy.deepcopy(entropy1) for el in files}
	print "Entropy H1"
	for k in entropy_1.keys():
		for call in entropy_1[k].keys():
			if numpy.log2(probability_1[k][call]) != float("-inf"):
				entropy_1[k][call] = probability_1[k][call] * numpy.log2(probability_1[k][call])
			else:
				entropy_1[k][call] = 0
		entropylevels[k]["H1"] = (-1*sum(entropy_1[k].values()))
			 


	entropy_2 = {el:copy.deepcopy(entropy2) for el in files}
	print "Entropy H2"
	for k in entropy_2.keys():

		for call in entropy_2[k].keys():

			getfirst = re.split("\t", call)[0]
			if numpy.log2(probability_2[k][call]) != float("-inf"):
				entropy_2[k][call] = probability_1[k][getfirst]*probability_2[k][call] * numpy.log2(probability_2[k][call])
			else:
				entropy_2[k][call] = 0
		entropylevels[k]["H2"] = (-1*sum(entropy_2[k].values()))


	entropy_3 = {el:copy.deepcopy(entropy3) for el in files}
	print "Entropy H3"
	for k in entropy_3.keys():
		
		for call in entropy_3[k].keys():
		
			getfirst = re.split("\t", call)[0]
			gettwo = re.split("\t", call)
			gettwo = gettwo[0]+"\t"+gettwo[1]
			if numpy.log2(probability_3[k][call]) != float("-inf"):
			
				entropy_3[k][call] = probability_1[k][getfirst]*probability_2[k][gettwo]* probability_3[k][call]* numpy.log2(probability_3[k][call])
			else:
				entropy_3[k][call] = 0
		entropylevels[k]["H3"] = (-1*sum(entropy_3[k].values()))


	Entropy_DF = pandas.DataFrame(entropylevels)

	tEntropy_DF = Entropy_DF.transpose()

	meanCounts = []	
	observations = []

	for call in Count2_DF.transpose():

		meanCounts.append([call,numpy.mean(Count2_DF.transpose()[call])])
		observations.extend(call.split("\t")* int(numpy.mean(Count2_DF.transpose()[call])))
	observations = filter(None, observations)
	meanCounts2 = pandas.DataFrame(meanCounts,columns = ["Call","Mean"]) 
	Freq1_DF = Freq1_DF.transpose()
	Freq1_DF.columns = alpha1
	Freq1_DF = Freq1_DF.astype(float)
	

	Freq2_DF = Freq2_DF.transpose()
	Freq2_DF.columns = alpha2
	Freq2_DF = Freq2_DF.astype(float)

	Freq3_DF = Freq3_DF.transpose()
	Freq3_DF.columns = alpha3
	Freq3_DF = Freq3_DF.astype(float)
	


	return [Entropy_DF, meanCounts2, Freq1_DF, Freq2_DF,Freq3_DF, tEntropy_DF, observations]

def mainFunctionPy_upto2(pathGlobal,alpha1,alpha2,pc):
	import re
	import pandas
	import os
	import copy
	import numpy

	files  = os.listdir(pathGlobal)
	if '.DS_Store' in files:
		files.remove('.DS_Store')

	count1 = {el:0 for el in alpha1}
	count2 = {el:0 for el in alpha2}
	
	

	entropy1 = {el:0 for el in alpha1}
	entropy2 = {el:0 for el in alpha2}
	
	

	Count = {el:[copy.deepcopy(count1),copy.deepcopy(count2)] for el in files}
	C2 = {el:copy.deepcopy(entropy2) for el in files}
	F1 = {el:copy.deepcopy(count1) for el in files}
	F2 = {el:copy.deepcopy(count2) for el in files}
	

	H = {el:[copy.deepcopy(entropy1),copy.deepcopy(entropy2)] for el in files}
	entropylevels = {el:{"H0":0,"H1":0, "H2":0} for el in files}


	for j in range(len(files)):
		with open(str(pathGlobal)+"/"+str(files[j])) as f:
			for line in f:
				for i in range(len(Count[files[j]])):
					for call in Count[files[j]][i].keys():
						if i == 1:
							Count[files[j]][i][call]  += line.count(call)
							C2[files[j]][call] += line.count(call)
						Count[files[j]][i][call]  += line.count(call)
		entropylevels[files[j]]["H0"] = numpy.log2(numpy.count_nonzero(Count[files[j]][0].values()))

	
	Count2_DF = pandas.DataFrame(C2)
	
	probability_1 = {el:copy.deepcopy(count1) for el in files}

	for k in probability_1.keys():
		for call in probability_1[k].keys():
			if (pc ==0 and sum(Count[k][0].values()) == 0):
				prob = 0
			else:
				prob = (float(Count[k][0][call])+pc)/(sum(Count[k][0].values())+pc*len(alpha1))

			probability_1[k][call] =  prob
			F1[k][call] = prob
	Freq1_DF = pandas.DataFrame(F1)
	

	probability_2 = {el:copy.deepcopy(count2) for el in files}

	for k in probability_2.keys():
		for call in probability_2[k].keys():
			getfirst = re.split("\t", call)[0]
			allwithFirst = []
			for element in alpha1:
				allwithFirst.append(str(getfirst+"\t"+element))
			subset = dict((r, Count[k][1][r]) for r in allwithFirst if r in Count[k][1])
			if (pc ==0 and sum(subset.values()) == 0):
				prob = 0
			else:
				prob = (float(Count[k][1][call])+pc)/(sum(subset.values())+pc*len(alpha2))
			probability_2[k][call] =  prob
			F2[k][call] = prob
	Freq2_DF = pandas.DataFrame(F2)
	

	entropy_1 = {el:copy.deepcopy(entropy1) for el in files}
	print "Entropy H1"
	for k in entropy_1.keys():
		for call in entropy_1[k].keys():
			if numpy.log2(probability_1[k][call]) != float("-inf"):
				entropy_1[k][call] = probability_1[k][call] * numpy.log2(probability_1[k][call])
			else:
				entropy_1[k][call] = 0
		entropylevels[k]["H1"] = (-1*sum(entropy_1[k].values()))
			 


	entropy_2 = {el:copy.deepcopy(entropy2) for el in files}
	print "Entropy H2"
	for k in entropy_2.keys():

		for call in entropy_2[k].keys():

			getfirst = re.split("\t", call)[0]
			if numpy.log2(probability_2[k][call]) != float("-inf"):
				entropy_2[k][call] = probability_1[k][getfirst]*probability_2[k][call] * numpy.log2(probability_2[k][call])
			else:
				entropy_2[k][call] = 0
		entropylevels[k]["H2"] = (-1*sum(entropy_2[k].values()))


	

	Entropy_DF = pandas.DataFrame(entropylevels)

	tEntropy_DF = Entropy_DF.transpose()

	meanCounts = []	
	observations = []

	for call in Count2_DF.transpose():

		meanCounts.append([call,numpy.mean(Count2_DF.transpose()[call])])
		observations.extend(call.split("\t")* int(numpy.mean(Count2_DF.transpose()[call])))
	observations = filter(None, observations)
	meanCounts2 = pandas.DataFrame(meanCounts,columns = ["Call","Mean"]) 
	Freq1_DF = Freq1_DF.transpose()
	Freq1_DF.columns = alpha1
	Freq1_DF = Freq1_DF.astype(float)

	Freq2_DF = Freq2_DF.transpose()
	Freq2_DF.columns = alpha2
	Freq2_DF = Freq2_DF.astype(float)


	return [Entropy_DF, meanCounts2, Freq1_DF, Freq2_DF, tEntropy_DF, observations]







