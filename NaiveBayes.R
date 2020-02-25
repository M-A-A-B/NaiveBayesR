learnProbabilities<-function(trainX,labels){
  oneClass = labels==1				#this will indicate true for all values for class = 1
  zeroClass = labels==0			#this will indicate true for all values for class = 0
  
  oneDat = trainX[oneClass,]			#this will give you the data matrix for class = 1
  zeroDat = trainX[zeroClass,]
  
  prior1=mean(labels)
  prior0= 1-prior1
  
  probOf1Class1 = colMeans(oneDat)		#this gives the mean vector of columns where attribute 1 and c 1
  
  probOf0Class1 = 1 - probOf1Class1		#this gives the mean vector of columns where attribute 0 and c 1
  
  probOf1Class0 = colMeans(zeroDat)		#this gives the mean vector of columns where attribute 1 and c 0
  
  probOf0Class0 = 1 - probOf1Class0		#this gives the mean vector of columns where attribute 0 and c 0
  
  probabilitiesList<-list(
                          "probOf0Class0" = probOf0Class0,
                          "probOf1Class0" = probOf1Class0,
                          "probOf0Class1" = probOf0Class1,
                          "probOf1Class1" = probOf1Class1,
                          "prior0" = prior0,
                          "prior1" = prior1
                          )
  return(probabilitiesList)
}

calculateLikelihood<-function(testRow,probOf0Class0,probOf1Class0,
                              probOf0Class1,probOf1Class1){
  ind1 = testRow == 1				#ind has indices of all values of Row1 equal to one
  ind0 = testRow == 0				#ind has indices of all values of Row1 equal to zero
  
  class1 = testRow
  class1[ind1] = probOf1Class1[ind1]		#this replaces all values at ind1 with corresponding probability 
  class1[ind0] = probOf0Class1[ind0]		#this replaces all values at ind0 with corresponding probability 
  likelihood1=prod(class1)					#will return product of all elements of Row1
  
  class0 = testRow				#gives first row of testX
  class0[ind1] = probOf1Class0[ind1]		#this replaces all values at ind1 with corresponding probability 
  class0[ind0] = probOf0Class0[ind0]		#this replaces all values at ind0 with corresponding probability 
  likelihood0=prod(class0)					#will return product of all elements of Row1
  
  likelihoods<-list("c0" = likelihood0,"c1" = likelihood1)
  return(likelihoods)
}

testMAP<-function(testRow,probOf0Class0,probOf1Class0,
                  probOf0Class1,probOf1Class1,prior0,prior1){
  likelihoods=calculateLikelihood(testRow,probOf0Class0,probOf1Class0,
                                  probOf0Class1,probOf1Class1)  
  map0 = likelihoods$c0*prior0
  map1 = likelihoods$c1*prior1
  mapevidence = map0+map1
  
  mapclass0 = map0/mapevidence
  mapclass1 = map1/mapevidence
  
  if (mapclass1 > mapclass0)
    predictedLabel = 1
  else
    predictedLabel = 0
  
  return(c("MAPClass0" = mapclass0,
                      "MAPClass1" = mapclass1,
                      "predictedLabel" = predictedLabel))
}

testML<-function(testRow,probOf0Class0,probOf1Class0,
                 probOf0Class1,probOf1Class1){
  likelihoods=calculateLikelihood(testRow,probOf0Class0,probOf1Class0,
                      probOf0Class1,probOf1Class1)
  
  if (likelihoods$c1 > likelihoods$c0)
    predictedLabel = 1
  else
    predictedLabel = 0
  
  return(c("MLClass0"=likelihoods$c0,"MLClass1"= likelihoods$c1,
              "predictedLabel"=predictedLabel))
}

driver<-function(){
  datAll=read.table("trainData.txt")  #Training data
  labels = datAll[,ncol(datAll)]  	#this will store last column of datAll in labels
  trainX = datAll[,-ncol(datAll)]		#this will store all features except label in dat
  training=learnProbabilities(trainX, labels)
  
  print(training)

  testX=read.table("testData.txt")    #Test data
  testRow = testX

  probabilitiesMAP <- apply(testRow,1,testMAP,training$probOf0Class0,training$probOf1Class0,
          training$probOf0Class1,training$probOf1Class1,
          training$prior0,training$prior1)
  
  probabilitiesML <- apply(testRow,1,testML,training$probOf0Class0,training$probOf1Class0,
          training$probOf0Class1,training$probOf1Class1)
  
  print(cbind(testX,t(as.data.frame(probabilitiesMAP)),t(as.data.frame(probabilitiesML))))
}

driver()

