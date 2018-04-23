Exploring data : Using summary statistics to spot problems 
# Title: The summary() command 

custdata <- read.table('H:/STAT123/data/Custdata/custdata.tsv',header=T,sep='\t')
custdata <- read.table('H:/STAT123/data/Custdata/custdata.tsv',header=T,sep='\t')
print(custdata[1:3,])

custdata2 <- read.table('custdata2.tsv',header=T,sep='\t')


summary(custdata)


#custid        sex
#Min.   :   2068   F:440
#1st Qu.: 345667   M:560
#Median : 693403
#Mean   : 698500
#3rd Qu.:1044606
#Max.   :1414286

is.employed         income     	# Note: 1 
Mode :logical   Min.   : -8700
FALSE:73        1st Qu.: 14600
TRUE :599       Median : 35000
NA's :328       Mean   : 53505
3rd Qu.: 67000
Max.   :615000

marital.stat
Divorced/Separated:155
Married           :516
Never Married     :233
Widowed           : 96

health.ins                    	# Note: 2 
Mode :logical
FALSE:159
TRUE :841
NA's :0

housing.type                        	# Note: 3 
Homeowner free and clear    :157
Homeowner with mortgage/loan:412
Occupied with no rent       : 11
Rented                      :364
NA's                        : 56

recent.move      num.vehicles
Mode :logical   Min.   :0.000
FALSE:820       1st Qu.:1.000
TRUE :124       Median :2.000
NA's :56        Mean   :1.916
3rd Qu.:2.000
Max.   :6.000
NA's   :56

age              state.of.res       	# Note: 4 
Min.   :  0.0   California  :100
1st Qu.: 38.0   New York    : 71
Median : 50.0   Pennsylvania: 70
Mean   : 51.7   Texas       : 56
3rd Qu.: 64.0   Michigan    : 52
Max.   :146.7   Ohio        : 51
(Other)     :600

# Note 1: 
#   The variable is.employed is missing for 
#   about a third of the data. The variable income has negative values, which are 
#   potentially invalid. 

# Note 2: 
#   About 84% of the customers have health 
#   insurance. 

# Note 3: 
#   The variables housing.type, recent.move, and 
#   num.vehicles are each missing 56 values. 

# Note 4: 
#   The average value of the variable age seems 
#   plausible, but the minimum and maximum values seem unlikely. The variable 
#   state.of.res is a categorical variable; summary() reports how many customers are in 
#   each state (for the first few states). 

#####################################################################################################################
# Title: Examples of invalid values and outliers 

summary(custdata$income)
   Min. 1st Qu.  Median    Mean 3rd Qu.
  -8700   14600   35000   53500   67000   	# Note: 1 
   Max.
 615000

summary(custdata$age)
   Min. 1st Qu.  Median    Mean 3rd Qu.
    0.0    38.0    50.0    51.7    64.0   	# Note: 2 
   Max.
  146.7

# Note 1: 
#   Negative values for income could indicate 
#   bad data. They might also have a special meaning, like amount of 
#   debt. Either way, you should check how prevalent the issue is, 
#   and decide what to do: Do you drop the data with negative income? Do you 
#   convert negative values to zero? 

# Note 2: 
#   Customers of age zero, or customers of an 
#   age greater than about 110 are outliers. They fall out of the range of 
#   expected customer values. Outliers could be data input errors. 
#   They could be special sentinel values: zero might mean age unknown or 
#   a refuse to state. And some of your customers might be especially 
#   long-lived. 

##################################################################################################################
# Title: Plotting a histogram 

### First method
hist(custdata$age, main = paste("Histogram of age"))


#### Second Method
library(ggplot2)     	# Note: 1 

ggplot(custdata) + geom_histogram(aes(x=age), binwidth=5, fill="gray") 	# Note: 2

# Note 1: 
#   Load the ggplot2 library, if you havenot 
#   already done so. 

# Note 2: 
#   binwidth parameterThe binwidth parameter tells the 
#   geom_histogram call how to make bins of five-year intervals (default is 
#   datarange/30). The fill parameter specifies the color of the histogram 
#   bars (default: black). 
##############################################################################################################

# Title: Producing a density plot 

library(scales) 	# Note: 1 

ggplot(custdata) + geom_density(aes(x=income)) +
scale_x_continuous(labels=dollar) 	# Note: 2

# Note 1: 
#   The scales package brings in the dollar 
#   scale notation. 

# Note 2: 
#   Set the x-axis labels to 
#   dollars. 
#################################################################################################################
# informalexample 3.2 of section 3.2.1 
# (informalexample 3.2 of section 3.2.1)  : Exploring data : Spotting problems using graphics and visualization : Visually checking distributions for a single variable 


## First method
counts <- table(custdata$marital.stat)
barplot(counts)
#########

## Second method
ggplot(custdata) + geom_bar(aes(x=marital.stat), fill="gray")



###################################################################################################

# Title: Producing a horizontal bar chart 

ggplot(custdata) +
   geom_bar(aes(x=state.of.res), fill="gray") +  	# Note: 1 
   coord_flip() + 	# Note: 2 
   theme(axis.text.y=element_text(size=rel(0.8)))  	# Note: 3

# Note 1: 
#   Plot bar chart as before: state.of.res 
#   is on x axis, count is on y-axis. 

# Note 2: 
#   Flip the x and y axes: state.of.res is 
#   now on the y-axis. 

# Note 3: 
#   Reduce the size of the y-axis tick 
#   labels to 80% of default size for legibility. 



################################################################################################################

# Title: Producing a line plot 

x <- runif(100)   	# Note: 1 
y <- x^2 + 0.2*x   	# Note: 2 
ggplot(data.frame(x=x,y=y), aes(x=x,y=y)) + geom_line()  	# Note: 3

# Note 1: 
#   First, generate the data for this example. 
#   The x variable is uniformly randomly distributed 
#   between 0 and 1. 

# Note 2: 
#   The y variable is a 
#   quadratic function of x. 

# Note 3: 
#   Plot the line plot. 


#################################################################################################################
 
# Title: Examining the correlation between age and income 

custdata2 <- subset(custdata, (custdata$age > 0 & custdata$age < 100 & custdata$income > 0))   	# Note: 1 

cor(custdata2$age, custdata2$income) 	# Note: 2 

[1] -0.02240845 	# Note: 3

# Note 1: 
#   Only consider a subset of data with 
#   reasonable age and income values. 

# Note 2: 
#   Get correlation of age and income. 

# Note 3: 
#   Resulting correlation. 

#########################################################################################################

##Title: Scatter plot

#First method

plot(custdata$age, custdata$income, ylim=c(0,200000), xlim=c(20,100))

#Second method
ggplot(custdata2, aes(x=age, y=income)) + geom_point() + ylim(0, 200000)




################################################################################
 Exploring data : Spotting problems using graphics and visualization : Visually checking relationships between two variables 
# Title: Specifying different styles of bar chart 

ggplot(custdata) + geom_bar(aes(x=marital.stat,
   fill=health.ins)) 	# Note: 1 

ggplot(custdata) + geom_bar(aes(x=marital.stat,
   fill=health.ins),
   position="dodge")      	# Note: 2 

ggplot(custdata) + geom_bar(aes(x=marital.stat,
   fill=health.ins),
   position="fill")        	# Note: 3

# Note 1: 
#   Stacked bar chart, the 
#   default 

# Note 2: 
#   Side-by-side bar chart 

# Note 3: 
#   Filled bar chart 
###########################################################


