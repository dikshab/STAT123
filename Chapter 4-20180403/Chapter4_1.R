
custdata <- read.table('Z:/STAT123/data/Custdata/custdata.tsv',header=T,sep='\t')

# Managing data : Cleaning data : Treating missing values (NAs) 
# Title: Checking locations of missing data 

summary(custdata[is.na(custdata$housing.type), 	# Note: 1 
                   c("recent.move","num.vehicles")]) 	# Note: 2 

custdata[18,]
recent.move     num.vehicles   	# Note: 3 
Mode:logical   Min.   : NA
NA's:56        1st Qu.: NA
Median : NA
Mean   :NaN
3rd Qu.: NA
Max.   : NA
NA's   :56

# Note 1: 
#   Restrict to the rows where housing.type is 
#   NA. 

# Note 2: 
#   Look only at the columns recent.move and 
#   num.vehicles. 

# Note 3: 
#   The output: all NAs. All the missing data 
#   comes from the same rows. 

###############################################################################
#Managing data : Cleaning data : Treating missing values (NAs) 
# Title: Remapping NA to a level 
custdata$is.employed.fix <- ifelse(is.na(custdata$is.employed),  	# Note: 1 
                                     "missing",                    	# Note: 2 
                                     ifelse(custdata$is.employed==T, 	# Note: 3 
                                            "employed",
                                            "not employed"))  	# Note: 4 

 summary(as.factor(custdata$is.employed.fix)) 	# Note: 5 

employed      missing not employed
599          328           73

# Note 1: 
#   If is.employed value is missing... 

# Note 2: 
#   ...assign the value "missing". 
#   Otherwise... 

# Note 3: 
#   ...if is.employed==TRUE, assign the value 
#   "employed"... 

# Note 4: 
#   ...or the value "not employed". 

# Note 5: 
#   The transformation has turned the variable 
#   type from factor to string. You can change it back 
#   with the as.factor() function. 
####################################################################################################

# Managing data : Cleaning data : Treating missing values (NAs) 

custdata$is.employed.fix <- ifelse(is.na(custdata$is.employed),
                                   "not in active workforce",
                                   ifelse(custdata$is.employed==T,
                                          "employed",
                                          "not employed"))

#########################################################################################################



