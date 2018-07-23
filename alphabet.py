
def generateAlphabetsPy(alphafromfile):
	alphabetFile = open(alphafromfile,"r")
	getAlphabet = alphabetFile.readline().split(",")
	alpha2 = []
	for letter in getAlphabet:
		for letter2 in getAlphabet:
			new = str(letter+"\t"+letter2)
			alpha2.append(new)
			
	alpha3 = []
	for letter in alpha2:
		for letter2 in getAlphabet:
			new = str(letter+"\t"+letter2)
			alpha3.append(new)

	alpha4 = []
	for letter in alpha3:
		for letter2 in getAlphabet:
			new = str(letter+"\t"+letter2)
			alpha4.append(new)

	alpha2_withoutTab = [s.replace('\t', '_') for s in alpha2]
	alpha3_withoutTab = [s.replace('\t', '_') for s in alpha3]
	alpha4_withoutTab = [s.replace('\t', '_') for s in alpha4]
	return([getAlphabet, alpha2, alpha3, alpha4,alpha2_withoutTab,alpha3_withoutTab,alpha4_withoutTab])