
def linearModelPy(levels, Entropy_DF, Group):
	import numpy
	import pandas
	LM = []
	for line in Entropy_DF:
		for level in levels:
			LM.append([line,float(Entropy_DF[line][level]),level,Group])
	return pandas.DataFrame(LM,columns = ["Mouse","Entropy","Level","Genotype"]) 