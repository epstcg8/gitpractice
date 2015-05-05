# function to find recommendations

get_recommendations=function(x, y=NA){
  x=tolower(x)
  sub=subset(label_recommender, grepl(x,tolower(current_label))==T)
  if(!is.na(y)){
    y=tolower(y)
    sub=subset(sub, grepl(y,tolower(current_label))==T)
  } 
  if(nrow(sub)==0) return("No matches")
  else{
    df=data.frame(Rank=c("Current Track",1:5),Recommendations=as.character(sub[1,]))
    return(df)
  }
}
