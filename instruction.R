#check for the str() of the dataframe
###check for number of NAs and potentially ** empty values

##ask yourself why/how they are missing 

#MCAR: missing completely at random 
##you can test it with naniar package 
###little's MCAR test
### H0 is that MCAR is valid


#MAR: missing at random 
#for example by building a lm() or glm()


#MNAR: missing not at random 


## we can fill in missing numerical values by using:
##different approaches:
# 1)predictive mean matching (package "mice")
# 2) it is a simple lm()
# 3) KNN
# Finally we going to combine all of them (ensemble approach)


##if few missing values, fill with mean or median
#depending on distribution (and potentially conditional on other informations)

#when it comes to categorical missing values 
##if you have few of them, take the most frequent level of the variable 
