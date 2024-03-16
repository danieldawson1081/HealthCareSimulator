#name of repo
library(dplyr)
healthhistory=function(majinjprob=0.001, mininjprob=0.01, numcheckups=5){
major=minor=checkups=majorfollowup=majorfollowupstart=majorfollowupend=minorfollowupstart=minorfollowupend=minorfollowup=NULL
majorfollowupperiod=60
minorfollowupperiod=30
majorfollowupstart=0
minorfollowupstart=0
majorfollowupend=0
minorfollowupend=0

for(i in 1:365){
major=c(major, rbinom(1,1, majinjprob))
if(major[i]==1){
majorfollowupstart=i
majorfollowupend=i+majorfollowupperiod
}
if(i>majorfollowupstart & i < majorfollowupend){
  majorfollowup=c(majorfollowup, rbinom(1,1,0.9))} else {
    majorfollowup=c(majorfollowup, 0)
  }
minor=c(minor, rbinom(1,1, mininjprob))
if(minor[i]==1){
  minorfolloupstart=i
  minorfollowupend=i+minorfollowupperiod
}
if(i>minorfollowupstart & i < minorfollowupend){
  minorfollowup=c(minorfollowup, rbinom(1,1,0.01))} else {
    minorfollowup=c(minorfollowup, 0)
  }

checkups=c(checkups, rbinom(1,1, numcheckups/365))
}

healthhist=data.frame(major, majorfollowup,minor, minorfollowup, checkups)

return(healthhist)

}

