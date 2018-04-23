
custdata <- read.table('Z:/STAT123/data/Custdata/custdata.tsv',header=T,sep='\t')

custdata<-subset(custdata, (custdata$income >= 0))
summary(custdata$income)


Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
30   19000   38350   57684   70163  615000 


###############################################################################'s

#  Managing data : Cleaning data : Treating missing values (NAs) 
# Title: Converting missing numeric data to a level 

 breaks <-c(0, 10000, 50000, 100000, 250000, 1000000)           	# Note: 1 

Income.groups <- cut(custdata$income,
                       breaks=breaks, include.lowest=T)  	# Note: 2 

summary(Income.groups)                                        	# Note: 3 

[0,1e+04]   (1e+04,5e+04]   (5e+04,1e+05] (1e+05,2.5e+05] (2.5e+05,1e+06] 
106             463             214             102              25 

 Income.groups <- as.character(Income.groups)                   	# Note: 4 

 Income.groups <- ifelse(is.na(Income.groups),                  	# Note: 5 
"no income", Income.groups)

summary(as.factor(Income.groups))

(1e+04,5e+04] (1e+05,2.5e+05] (2.5e+05,1e+06]   (5e+04,1e+05]       [0,1e+04] 
463             102              25             214             106 

# Note 1: 
#   Select some income ranges of interest. To 
#   use the cut() function, the upper and lower bounds 
#   should encompass the full income range of the 
#   data. 

# Note 2: 
#   Cut the data into income ranges. The 
#   include.lowest=T argument makes sure that zero 
#   income data is included in the lowest income range 
#   category. By default it would be excluded. 

# Note 3: 
#   The cut() function produces factor 
#   variables. Note the NAs are preserved. 

# Note 4: 
#   To preserve the category names before adding 
#   a new category, convert the variables to strings. 

# Note 5: 
#   Add the "no income" category to replace the 
#   NAs. 

#######################################################################################'
# Title: Tracking original NAs with an extra categorical variable 

missingIncome <- is.na(custdata$Income)  	# Note: 1 
Income.fix <- ifelse(is.na(custdata$Income), 0, custdata$Income) 	# Note: 2

# Note 1: 
#   The missingIncome variable lets you 
#   differentiate the two kinds of zeros in the data: 
#   the ones that you are about to add, and the ones 
#   that were already there. 

# Note 2: 
#   Replace the NAs with zeros. 


#########################################################################################

#Title: Cleaning data : Data transformations 

 custdata$income.lt.20K <- custdata$income < 20000
  summary(custdata$income.lt.20K)
  Mode   FALSE    TRUE 
  logical     668     242 


###########################################################################################'

# Title: Converting age into ranges 

brks <- c(0, 25, 65, Inf)  	# Note: 1 
custdata$age.range <- cut(custdata$age,
                          breaks=brks, include.lowest=T) 	# Note: 2 
summary(custdata$age.range) 	# Note: 3 
[0,25]  (25,65] (65,Inf] 
45      665      200 

# Note 1: 
#   Select the age ranges of interest. The upper 
#   and lower bounds should encompass the full range 
#   of the data. 

# Note 2: 
#   Cut the data into age ranges. The 
#   include.lowest=T argument makes sure that zero age 
#   data is included in the lowest age range category. 
#   By default it would be excluded. 

# Note 3: 
#   The output of cut() is a factor variable. 


###############################################################################################


# Title: Summarizing age 

summary(custdata$age)
Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
18.00   39.00   50.00   51.85   64.00   93.00 
meanage <- mean(custdata$age)  	# Note: 1 
stdage <- sd(custdata$age)     	# Note: 2 
> meanage
[1] 51.85165
> stdage
[1] 17.46028
custdata$age.normalized <- (custdata$age-meanage)/stdage 	# Note: 3 
summary(custdata$age.normalized)
Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
-1.9388 -0.7361 -0.1060  0.0000  0.6958  2.3567 

# Note 1: 
#   Take the mean. 

# Note 2: 
#   Take the standard deviation. 

# Note 3: 
#   Use the mean value as the origin (or 
#   reference point) and rescale the distance from the 
#   mean by the standard deviation. 
####################################################################################################

# Managing data : Cleaning data : Data transformations 

signedlog10 <- function(x) {
  ifelse(abs(x) <= 1, 0, sign(x)*log10(abs(x)))
}

sampledata<-c(0.4,0.5,1.2,3.4,1,3)

signedlog10(sampledata)
