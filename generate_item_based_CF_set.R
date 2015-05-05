library(RMySQL)
library(RPostgreSQL)
library(plyr)
library(fields)
library(Matrix)
library(reshape2)
###################################################################
# set up connection 
###################################################################
# connect to redshift instance
drv = dbDriver('PostgreSQL')
host='beatport.sql.segment.com'
conseg = dbConnect(drv,host=host, user='readonly',pass='4da28a8db6c5415296d8d3a27be05df6A', dbname="events", port=5439)
# set working directory
setwd("~/Desktop/OtherAnalysis/recommendation_engine")
###################################################################
# pull relevant data from segment
###################################################################

query="select coalesce(user_id, anonymous_id) user_id, label
from beatport_web_production.heart
where category='sounds'"
heart=dbGetQuery(conseg,query)

query="select coalesce(user_id, anonymous_id) user_id, label
from beatport_web_production.play"
play=dbGetQuery(conseg,query)

eventdf=rbind(heart,play)
eventdf$label=as.character(eventdf$label)

###################################################################
# subset to only those that have been interacted with by at least x unique users
###################################################################

x=1000
labs_count=ddply(eventdf,"label", summarise, unique_user=length(unique(user_id)))
labs_with_atleastx=labs_count$label[labs_count$unique_user>=x]
atleastx=subset(eventdf, label %in% labs_with_atleastx)  

###################################################################
# make SPARSE label - user matrix
###################################################################
users=unique(atleastx$user_id)
labels=unique(atleastx$label)

usermap=data.frame(colnumber=1:length(users),user_id=users)
labelmap=data.frame(rownumber=1:length(labels), label=labels)
nalx=join(join(atleastx,usermap),labelmap)
nalx$xsp=1

sparsmat=sparseMatrix(i=nalx$rownumber,j=nalx$colnumber, x=nalx$xsp)

###################################################################
# make pseudo track - track distance matrix
# for each track, only saving indices of top k most similar tracks
# calculate pairwise distances 500 labels at a time
###################################################################

top_k_indices=function(labelusermat,row,k=5){
  x1=matrix(labelusermat[row,],nrow=1)
  x2=labelusermat
  d1=rdist(x1,x2)
}
tracktrackmat=rdist(sparsmat)
diag(tracktrackmat)=NA

###################################################################
# make recommender df
###################################################################

label_recommender=data.frame(current_label=rep(NA,ncol(tracktrackmat)),
                             recommended_label_1=rep(NA,ncol(tracktrackmat)),
                             recommended_label_2=rep(NA,ncol(tracktrackmat)),
                             recommended_label_3=rep(NA,ncol(tracktrackmat)),
                             recommended_label_4=rep(NA,ncol(tracktrackmat)),
                             recommended_label_5=rep(NA,ncol(tracktrackmat)))
for(i in 1:length(labels)){
  label_recommender$current_label[i]=labels[i]
  currentrow=tracktrackmat[i,]
  top5scores=sort(currentrow)[1:5]
  inds=which(currentrow%in%top5scores)
  scoreinds=currentrow[inds]
  ind_df=data.frame(inds, scoreinds)
  ind_df=ind_df[order(ind_df$scoreinds),]
  label_recommender$recommended_label_1[i]=labels[ind_df$inds[1]]
  label_recommender$recommended_label_2[i]=labels[ind_df$inds[2]]
  label_recommender$recommended_label_3[i]=labels[ind_df$inds[3]]
  label_recommender$recommended_label_4[i]=labels[ind_df$inds[4]]
  label_recommender$recommended_label_5[i]=labels[ind_df$inds[5]]
  if(i %%100==0) print(i)
}

write.csv(label_recommender, "label_recommender_df_20150504.csv",row.names=F)


min.k.dists <- function(x,k=5) {
  apply(x,2,function(r) {
    b <- colSums((x - r)^2)
    o <- order(b)
    o[1:k]
  })
}

