# change to where folder is saved
dir="~/Desktop/general_item_based_CF"

# set working directory and load function
setwd(dir)
source(file=paste(dir,"get_track_recommendations_fcn.R",sep="/"))

# read in data
label_recommender=read.csv("label_recommender_df_20150505.csv",colClasses="character")

# Get recommendations with get_recommendations(x, y) function
# Inputs x and y are any two parts of a sound label -- for example, x can be part of the track name, and y could be part of the list of artists
# Only one input is required, and neither x nor y is case sensitive. Examples:

get_recommendations("woman", "mstrkrft")
get_recommendations("the hum")
get_recommendations("California", "Kaleena")
get_recommendations("Cloudlight")
