movie_data<-read.csv("u_data_tabular.csv")
filter_data<-function(movie,user){
  filtered_data<<-movie_data[c(1:user),c(1:movie)]
  #print(filtered_data)
}
transpose<-function(){
  transposed<<-as.data.frame(t(filtered_data[,2:ncol(filtered_data)]))
  colnames(transposed)<<-filtered_data[,1]
}

CorrelateAndWeight<-function(uname){
  #finding the correlation between current user and other users
  correlated<<-cor(transposed[,uname],transposed[,!names(transposed) %in% c(uname)],use="pairwise.complete.obs")
  #remvoing NA if any
  correlated<<-correlated[1,!is.na(correlated)]
  #ignoring <0's
  correlated<<-correlated[correlated>=0]
  #correlated users
 # print(names(correlated))
  #calcuating weighted mean
  weightedmeanvalues<-apply(transposed[,names(correlated)],1,function(x) weighted.mean(x,correlated,na.rm = TRUE))
  #print(weightedmeanvalues)
  weightedmeanvalues<<-weightedmeanvalues[!is.na(weightedmeanvalues[])]
}

findRecommendation<-function(user,num){
  suppressWarnings(CorrelateAndWeight(user))
  #find the names of movies not seen from the transposed 
  notseen<-row.names(transposed[is.na(transposed[,user]),])
  weightedmeanvalues<-weightedmeanvalues[notseen]
 print("Recommendations ")
  print(head(sort(weightedmeanvalues,decreasing = TRUE),num))
 suppressWarnings(confusionMat('u28',4))
}

confusionMat<-function(target,threshold=3){
  testdata_num<-transposed[!is.na(transposed[,target]),target]
  testdata<-transposed[!is.na(transposed[,target]),]
  testdata=as.data.frame(x = testdata_num,row.names=rownames(testdata)) 
  #print(testdata)
  traindata<-transposed[rownames(testdata),!colnames(transposed) %in% c(target)]
  error=0
  count=0
  #to set the digits
  TruePos=FalsePos=TrueNeg=FalseNeg=0;
  options(digits=8)
  #print(rownames(testdata))
  for (movie in rownames(testdata)){
    #print(movie)
    targetvalue=testdata[movie,]
    testdata[movie,]='NA'
    correlatedvalue<-cor(testdata_num,traindata[,],use="pairwise.complete.obs")
    correlatedvalue<-correlatedvalue[,!is.na(correlatedvalue)]
    predictedvalue=weighted.mean(traindata[movie,names(correlatedvalue)],correlatedvalue,na.rm=TRUE)
   # print(predictedvalue)
    predictedvalue=as.double(predictedvalue)
    targetvalue=as.double(targetvalue)
    if(!is.na(predictedvalue)){
      count=count+1;
      #print(predictedvalue)
      #print(targetvalue)
      error =error+ abs(targetvalue-predictedvalue)
      if(predictedvalue>=threshold){
        if(targetvalue>=threshold){
          TruePos=TruePos+1;
        }
        else{
          FalsePos=FalsePos+1;
        }
      }
      else{
        if(targetvalue>=threshold){
          FalseNeg=FalseNeg+1;
        }
        else{
          TrueNeg=TrueNeg+1;
        }
        
      }
      
    }
  }
  print("error rate")
  print(error/count)
  print("          True False")
  print(paste("Positive ",TruePos,FalsePos))
  print(paste("Negative ",TrueNeg,FalseNeg))
  
 

}

filter_data(1683,900)
transpose()
suppressWarnings(findRecommendation('u28',10))


