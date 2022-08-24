MTcinderellaALPODSClassify = function(Data,Header){
  return(ALPODSclassifier(Data,Header,Type="V2"))
# # [RuleCls,PopulationNumber] = MTcinderellaALPODSClassify(Data,Header)
# # rule based classifier and subpopulations corresponding to RuleCls
# # INPUT
# # Data(1:d,1:n)        data array of d cases with n variables
# # Header(1:n,:)        array of variable names
# # OUTPUT
# # RuleCls[1:d)             vector of classes, d integer numbers, number k indicates class k
# # PopulationNumber[1:d)    vector of classes, d integer numbers, number i indicates subpopulation  i
# #
# # V 1.4
# #
# V=dim(Data); n = V[1]; d = V[2]; 
# RuleCls = zeros(n,1)
# PopulationNumber =zeros(n,1)
# CD13=findAttrCol('CD13',Header,Data)
# CD33=findAttrCol('CD33',Header,Data)
# CD34=findAttrCol('CD34',Header,Data)
# CD45=findAttrCol('CD45',Header,Data)
# CD56=findAttrCol('CD56',Header,Data)
# CD7=findAttrCol('CD7',Header,Data)
# FS=findAttrCol('FS',Header,Data)
# HLA_DR=findAttrCol('HLA_DR',Header,Data)
# SS=findAttrCol('SS',Header,Data)
# Ind1001 =which((CD33 >= 3.7545) &(CD34 < 2.4475))
#  RuleCls[Ind1001] = 1 
#  PopulationNumber[Ind1001] = 5
# Ind1003 =which((HLA_DR < 2.3605) &(CD45 >= 1.8695) &(CD33 < 3.7545) &(CD34 < 2.4475))
#  RuleCls[Ind1003] = 1 
#  PopulationNumber[Ind1003] = 1
# Ind1004 =which((FS < 5.5215) &(CD13 < 3.8915) &(FS < 5.8145) &(CD34 >= 2.4475))
#  RuleCls[Ind1004] = 1 
#  PopulationNumber[Ind1004] = 4
# Ind1005 =which((HLA_DR < 2.9215) &(CD13 >= 3.8915) &(FS < 5.8145) &(CD34 >= 2.4475))
#  RuleCls[Ind1005] = 1 
#  PopulationNumber[Ind1005] = 8
# Ind1007 =which((CD45 < 3.2545) &(FS >= 5.5215) &(CD13 < 3.8915) &(FS < 5.8145) &(CD34 >= 2.4475))
#  RuleCls[Ind1007] = 1 
#  PopulationNumber[Ind1007] = 38
# Ind1008 =which((CD33 >= 3.9505) &(HLA_DR < 2.6345) &(CD13 < 3.8025) &(FS >= 5.8145) &(CD34 >= 2.4475))
#  RuleCls[Ind1008] = 1 
#  PopulationNumber[Ind1008] = 34
# Ind1011 =which((CD45 < 3.6005) &(CD33 < 3.3545) &(HLA_DR >= 2.3605) &(CD45 >= 1.8695) &(CD33 < 3.7545) &(CD34 < 2.4475))
#  RuleCls[Ind1011] = 1 
#  PopulationNumber[Ind1011] = 12
# Ind1012 =which((CD45 >= 3.7485) &(CD33 >= 3.3545) &(HLA_DR >= 2.3605) &(CD45 >= 1.8695) &(CD33 < 3.7545) &(CD34 < 2.4475))
#  RuleCls[Ind1012] = 1 
#  PopulationNumber[Ind1012] = 20
# Ind1014 =which((CD45 < 3.2855) &(CD33 < 3.9505) &(HLA_DR < 2.6345) &(CD13 < 3.8025) &(FS >= 5.8145) &(CD34 >= 2.4475))
#  RuleCls[Ind1014] = 1 
#  PopulationNumber[Ind1014] = 16
# Ind1015 =which((SS >= 5.9965) &(CD33 < 3.6495) &(HLA_DR >= 2.6345) &(CD13 < 3.8025) &(FS >= 5.8145) &(CD34 >= 2.4475))
#  RuleCls[Ind1015] = 1 
#  PopulationNumber[Ind1015] = 30
# Ind1016 =which((CD33 >= 4.0425) &(CD33 >= 3.6495) &(HLA_DR >= 2.6345) &(CD13 < 3.8025) &(FS >= 5.8145) &(CD34 >= 2.4475))
#  RuleCls[Ind1016] = 1 
#  PopulationNumber[Ind1016] = 23
# Ind1017 =which((SS < 5.7785) &(CD33 >= 3.5295) &(CD45 < 3.9585) &(CD13 >= 3.8025) &(FS >= 5.8145) &(CD34 >= 2.4475))
#  RuleCls[Ind1017] = 1 
#  PopulationNumber[Ind1017] = 25
# Ind1019 =which((CD33 >= 3.5405) &(HLA_DR < 3.0905) &(CD45 >= 3.9585) &(CD13 >= 3.8025) &(FS >= 5.8145) &(CD34 >= 2.4475))
#  RuleCls[Ind1019] = 1 
#  PopulationNumber[Ind1019] = 15
# Ind1021 =which((CD34 < 2.3145) &(CD45 < 3.7485) &(CD33 >= 3.3545) &(HLA_DR >= 2.3605) &(CD45 >= 1.8695) &(CD33 < 3.7545) &(CD34 < 2.4475))
#  RuleCls[Ind1021] = 1 
#  PopulationNumber[Ind1021] = 28
# Ind1022 =which((SS >= 5.2675) &(CD45 >= 3.5155) &(CD45 >= 3.2545) &(FS >= 5.5215) &(CD13 < 3.8915) &(FS < 5.8145) &(CD34 >= 2.4475))
#  RuleCls[Ind1022] = 1 
#  PopulationNumber[Ind1022] = 19
# Ind1024 =which((CD45 >= 3.8345) &(CD45 >= 3.2855) &(CD33 < 3.9505) &(HLA_DR < 2.6345) &(CD13 < 3.8025) &(FS >= 5.8145) &(CD34 >= 2.4475))
#  RuleCls[Ind1024] = 1 
#  PopulationNumber[Ind1024] = 35
# Ind1025 =which((CD56 < 2.3325) &(SS < 5.9965) &(CD33 < 3.6495) &(HLA_DR >= 2.6345) &(CD13 < 3.8025) &(FS >= 5.8145) &(CD34 >= 2.4475))
#  RuleCls[Ind1025] = 1 
#  PopulationNumber[Ind1025] = 7
# Ind1026 =which((CD45 >= 3.7085) &(CD33 < 4.0425) &(CD33 >= 3.6495) &(HLA_DR >= 2.6345) &(CD13 < 3.8025) &(FS >= 5.8145) &(CD34 >= 2.4475))
#  RuleCls[Ind1026] = 1 
#  PopulationNumber[Ind1026] = 29
# Ind1028 =which((CD45 >= 4.0285) &(CD33 < 3.5405) &(HLA_DR < 3.0905) &(CD45 >= 3.9585) &(CD13 >= 3.8025) &(FS >= 5.8145) &(CD34 >= 2.4475))
#  RuleCls[Ind1028] = 1 
#  PopulationNumber[Ind1028] = 17
# Ind1029 =which((CD7 >= 3.9395) &(CD56 < 3.0565) &(CD45 >= 3.6005) &(CD33 < 3.3545) &(HLA_DR >= 2.3605) &(CD45 >= 1.8695) &(CD33 < 3.7545) &(CD34 < 2.4475))
#  RuleCls[Ind1029] = 1 
#  PopulationNumber[Ind1029] = 40
# Ind1030 =which((CD45 < 3.2435) &(CD34 >= 2.3145) &(CD45 < 3.7485) &(CD33 >= 3.3545) &(HLA_DR >= 2.3605) &(CD45 >= 1.8695) &(CD33 < 3.7545) &(CD34 < 2.4475))
#  RuleCls[Ind1030] = 1 
#  PopulationNumber[Ind1030] = 43
# Ind1032 =which((HLA_DR >= 3.181) &(SS < 5.2675) &(CD45 >= 3.5155) &(CD45 >= 3.2545) &(FS >= 5.5215) &(CD13 < 3.8915) &(FS < 5.8145) &(CD34 >= 2.4475))
#  RuleCls[Ind1032] = 1 
#  PopulationNumber[Ind1032] = 39
# Ind1033 =which((CD45 < 3.6405) &(CD56 >= 2.3325) &(SS < 5.9965) &(CD33 < 3.6495) &(HLA_DR >= 2.6345) &(CD13 < 3.8025) &(FS >= 5.8145) &(CD34 >= 2.4475))
#  RuleCls[Ind1033] = 1 
#  PopulationNumber[Ind1033] = 26
# Ind1034 =which((CD45 < 3.3865) &(CD45 < 3.7085) &(CD33 < 4.0425) &(CD33 >= 3.6495) &(HLA_DR >= 2.6345) &(CD13 < 3.8025) &(FS >= 5.8145) &(CD34 >= 2.4475))
#  RuleCls[Ind1034] = 1 
#  PopulationNumber[Ind1034] = 41
# Ind1036 =which((FS < 5.3645) &(CD7 < 3.9395) &(CD56 < 3.0565) &(CD45 >= 3.6005) &(CD33 < 3.3545) &(HLA_DR >= 2.3605) &(CD45 >= 1.8695) &(CD33 < 3.7545) &(CD34 < 2.4475))
#  RuleCls[Ind1036] = 1 
#  PopulationNumber[Ind1036] = 42
# Ind1038 =which((CD45 >= 3.5615) &(CD45 >= 3.2435) &(CD34 >= 2.3145) &(CD45 < 3.7485) &(CD33 >= 3.3545) &(HLA_DR >= 2.3605) &(CD45 >= 1.8695) &(CD33 < 3.7545) &(CD34 < 2.4475))
#  RuleCls[Ind1038] = 1 
#  PopulationNumber[Ind1038] = 22
# Ind1040 =which((CD34 >= 2.6985) &(CD45 >= 3.6405) &(CD56 >= 2.3325) &(SS < 5.9965) &(CD33 < 3.6495) &(HLA_DR >= 2.6345) &(CD13 < 3.8025) &(FS >= 5.8145) &(CD34 >= 2.4475))
#  RuleCls[Ind1040] = 1 
#  PopulationNumber[Ind1040] = 37
# Ind1043 =which((CD45 >= 3.8705) &(CD7 < 2.8195) &(FS >= 5.3645) &(CD7 < 3.9395) &(CD56 < 3.0565) &(CD45 >= 3.6005) &(CD33 < 3.3545) &(HLA_DR >= 2.3605) &(CD45 >= 1.8695) &(CD33 < 3.7545) &(CD34 < 2.4475))
#  RuleCls[Ind1043] = 1 
#  PopulationNumber[Ind1043] = 33
# Ind2002 =which((CD45 < 1.8695) &(CD33 < 3.7545) &(CD34 < 2.4475))
#  RuleCls[Ind2002] = 2 
#  PopulationNumber[Ind2002] = 13
# Ind2006 =which((HLA_DR >= 2.9215) &(CD13 >= 3.8915) &(FS < 5.8145) &(CD34 >= 2.4475))
#  RuleCls[Ind2006] = 2 
#  PopulationNumber[Ind2006] = 36
# Ind2009 =which((CD33 < 3.5295) &(CD45 < 3.9585) &(CD13 >= 3.8025) &(FS >= 5.8145) &(CD34 >= 2.4475))
#  RuleCls[Ind2009] = 2 
#  PopulationNumber[Ind2009] = 3
# Ind2010 =which((HLA_DR >= 3.0905) &(CD45 >= 3.9585) &(CD13 >= 3.8025) &(FS >= 5.8145) &(CD34 >= 2.4475))
#  RuleCls[Ind2010] = 2 
#  PopulationNumber[Ind2010] = 6
# Ind2013 =which((CD45 < 3.5155) &(CD45 >= 3.2545) &(FS >= 5.5215) &(CD13 < 3.8915) &(FS < 5.8145) &(CD34 >= 2.4475))
#  RuleCls[Ind2013] = 2 
#  PopulationNumber[Ind2013] = 31
# Ind2018 =which((SS >= 5.7785) &(CD33 >= 3.5295) &(CD45 < 3.9585) &(CD13 >= 3.8025) &(FS >= 5.8145) &(CD34 >= 2.4475))
#  RuleCls[Ind2018] = 2 
#  PopulationNumber[Ind2018] = 18
# Ind2020 =which((CD56 >= 3.0565) &(CD45 >= 3.6005) &(CD33 < 3.3545) &(HLA_DR >= 2.3605) &(CD45 >= 1.8695) &(CD33 < 3.7545) &(CD34 < 2.4475))
#  RuleCls[Ind2020] = 2 
#  PopulationNumber[Ind2020] = 32
# Ind2023 =which((CD45 < 3.8345) &(CD45 >= 3.2855) &(CD33 < 3.9505) &(HLA_DR < 2.6345) &(CD13 < 3.8025) &(FS >= 5.8145) &(CD34 >= 2.4475))
#  RuleCls[Ind2023] = 2 
#  PopulationNumber[Ind2023] = 2
# Ind2027 =which((CD45 < 4.0285) &(CD33 < 3.5405) &(HLA_DR < 3.0905) &(CD45 >= 3.9585) &(CD13 >= 3.8025) &(FS >= 5.8145) &(CD34 >= 2.4475))
#  RuleCls[Ind2027] = 2 
#  PopulationNumber[Ind2027] = 14
# Ind2031 =which((HLA_DR < 3.181) &(SS < 5.2675) &(CD45 >= 3.5155) &(CD45 >= 3.2545) &(FS >= 5.5215) &(CD13 < 3.8915) &(FS < 5.8145) &(CD34 >= 2.4475))
#  RuleCls[Ind2031] = 2 
#  PopulationNumber[Ind2031] = 24
# Ind2035 =which((CD45 >= 3.3865) &(CD45 < 3.7085) &(CD33 < 4.0425) &(CD33 >= 3.6495) &(HLA_DR >= 2.6345) &(CD13 < 3.8025) &(FS >= 5.8145) &(CD34 >= 2.4475))
#  RuleCls[Ind2035] = 2 
#  PopulationNumber[Ind2035] = 11
# Ind2037 =which((CD45 < 3.5615) &(CD45 >= 3.2435) &(CD34 >= 2.3145) &(CD45 < 3.7485) &(CD33 >= 3.3545) &(HLA_DR >= 2.3605) &(CD45 >= 1.8695) &(CD33 < 3.7545) &(CD34 < 2.4475))
#  RuleCls[Ind2037] = 2 
#  PopulationNumber[Ind2037] = 21
# Ind2039 =which((CD34 < 2.6985) &(CD45 >= 3.6405) &(CD56 >= 2.3325) &(SS < 5.9965) &(CD33 < 3.6495) &(HLA_DR >= 2.6345) &(CD13 < 3.8025) &(FS >= 5.8145) &(CD34 >= 2.4475))
#  RuleCls[Ind2039] = 2 
#  PopulationNumber[Ind2039] = 27
# Ind2041 =which((CD7 >= 2.8195) &(FS >= 5.3645) &(CD7 < 3.9395) &(CD56 < 3.0565) &(CD45 >= 3.6005) &(CD33 < 3.3545) &(HLA_DR >= 2.3605) &(CD45 >= 1.8695) &(CD33 < 3.7545) &(CD34 < 2.4475))
#  RuleCls[Ind2041] = 2 
#  PopulationNumber[Ind2041] = 9
# Ind2042 =which((CD45 < 3.8705) &(CD7 < 2.8195) &(FS >= 5.3645) &(CD7 < 3.9395) &(CD56 < 3.0565) &(CD45 >= 3.6005) &(CD33 < 3.3545) &(HLA_DR >= 2.3605) &(CD45 >= 1.8695) &(CD33 < 3.7545) &(CD34 < 2.4475))
#  RuleCls[Ind2042] = 2 
#  PopulationNumber[Ind2042] = 10
# # end Classifier: MTcinderellaALPODSClassify()
# V=list(RuleCls,PopulationNumber)
# names(V) = c('RuleCls','PopulationNumber')
# return(V)
}