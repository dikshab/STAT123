##################################################################################
# Title: Plotting the relation between disposable income and loan outcome 

tab1 <- as.table(matrix(data=c(50,6,0,44),nrow=2,ncol=2))
dimnames(tab1) <- list('loan.as.pct.disposable.income'=c('LT.15pct','GT.15pct'),
                       'loan.quality.pop1'=c('goodloan','badloan'))
tab2 <- as.table(matrix(data=c(34,18,16,32),nrow=2,ncol=2))
dimnames(tab2) <- list('loan.as.pct.disposable.income'= c('LT.15pct','GT.15pct'),'loan.quality.pop2'= c('goodloan','badloan'))
tab1
#loan.quality.pop1 	# Note: 1 
#loan.as.pct.disposable.income goodloan badloan
#LT.15pct       50       0
#GT.15pct        6      44
 sum(diag(tab1))/sum(tab1)                  	# Note: 2 

tab2
#loan.quality.pop2  	# Note: 3 
#loan.as.pct.disposable.income goodloan badloan
#LT.15pct       34      16
#GT.15pct       18      32
 sum(diag(tab2))/sum(tab2)
#[1] 0.66                                                        	# Note: 4

# Note 1: 
#   The count of correct predictions is on the 
#   diagonal of tab1. In this first population, all 
#   the loans that were less than 15% of disposable 
#   income were good loans, and all but six of the 
#   loans that were greater than 15% of disposable 
#   income defaulted. So you know that 
#   loan.as.pct.disposable.income models loan quality 
#   well in this population. Or as statisticians might 
#   say, loan.as.pct.disposable.income the 
#   output (loan quality). 

# Note 2: 
#   In fact, itis 94% accurate. 

# Note 3: 
#   In the second population, about a third of 
#   the loans that were less than 15% of disposable 
#   income defaulted, and over half of the loans that 
#   were greater than 15% of disposable income were 
#   good. So you know that 
#   loan.as.pct.disposable.income model loan 
#   quality well in this population. 

# Note 4: 
#   The rule of thumb is only 66% 
#   accurate. 


##############################################################################################################

# Title: Reading the UCI car data 

uciCar <- read.table(  	# Note: 1 
  'http://www.win-vector.com/dfiles/car.data.csv', 	# Note: 2 
  sep=',', 	# Note: 3 
  header=T 	# Note: 4 
)

# Note 1: 
#   Command to read from a file or URL and store the result in a new data frame object 
#   called 
#   uciCar. 

# Note 2: 
#   Filename or URL to get the data from. 

# Note 3: 
#   Specify the column or field separator as a 
#   comma. 

# Note 4: 
#   Tell R to expect a header line that defines 
#   the data column names. 

uciCar2 <- read.table( 'H:/STAT123/Chapter 2 R code/car.data.csv', sep=',', header=T)


###########################################################################################################


# Title: Exploring the car data 

 class(uciCar)
#[1] "data.frame" 	# Note: 1 
 summary(uciCar)


 dim(uciCar)
#[1] 1728    7   	# Note: 2

# Note 1: 
#   The loaded object uciCar is of type data.frame. 

# Note 2: 
#   The [1] is just an output sequence 
#   marker. The actual information is this: uciCar has 
#   1728 rows and 7 columns. Always try to confirm you 
#   got a good parse by at least checking that the 
#   number of rows is exactly one fewer than the 
#   number of lines of text in the original file. The 
#   difference of one is because the column header 
#   counts as a line, but not as a data row. 

#########################################################################################################

# Title: Loading the credit dataset 

d <- read.table(paste('http://archive.ics.uci.edu/ml/',
                      'machine-learning-databases/statlog/german/german.data',sep=''),
                stringsAsFactors=F,header=F)
print(d[1:3,])



d2 <- read.table("H:/STAT123/Chapter 2 R code/german.data",sep='', stringsAsFactors=F,header=F)
print(d2[1:3,])

#########################################################################################################

# Title: Setting column names 

colnames(d) <- c('Status.of.existing.checking.account',
                 'Duration.in.month',  'Credit.history', 'Purpose',
                 'Credit.amount', 'Savings account/bonds',
                 'Present.employment.since',
                 'Installment.rate.in.percentage.of.disposable.income',
                 'Personal.status.and.sex', 'Other.debtors/guarantors',
                 'Present.residence.since', 'Property', 'Age.in.years',
                 'Other.installment.plans', 'Housing',
                 'Number.of.existing.credits.at.this.bank', 'Job',
                 'Number.of.people.being.liable.to.provide.maintenance.for',
                 'Telephone', 'foreign.worker', 'Good.Loan')

d$Good.Loan <- as.factor(ifelse(d$Good.Loan==1,'GoodLoan','BadLoan'))
print(d[1:3,])

summary(d)


############################################################################################
# Title: Plotting the confusion matrix 

creditdata <- d
resultframe <- data.frame(Good.Loan=creditdata$Good.Loan,
                          pred=predict(model, type="class"))
rtab <- table(resultframe) 	# Note: 1 
rtab
#          pred
#Good.Loan  BadLoan GoodLoan
#  BadLoan       41      259
#  GoodLoan      13      687

sum(diag(rtab))/sum(rtab)  	# Note: 2 
#[1] 0.728
sum(rtab[1,1])/sum(rtab[,1]) 	# Note: 3 
#[1] 0.7592593
sum(rtab[1,1])/sum(rtab[1,]) 	# Note: 4 
#[1] 0.1366667
sum(rtab[2,1])/sum(rtab[2,]) 	# Note: 5 
#[1] 0.01857143

# Note 1: 
#   Create the confusion matrix. Rows represent 
#   actual loan status; columns represent predicted 
#   loan status. The diagonal entries represent 
#   correct predictions. 

# Note 2: 
#   accuracyconfusion matrixOverall model accuracy: 73% of the predictions 
#   were correct. 

# Note 3: 
#   precisionconfusion matrixModel precision: 76% of the applicants 
#   predicted as bad really did default. 

# Note 4: 
#   recallconfusion matrixModel recall: the model found 14% of the 
#   defaulting loans. 

# Note 5: 
#   false positive rateconfusion matrixFalse positive rate: 2% of the good applicants 
#   were mistakenly identified as bad. 


########################################################################################################

# Title: Building a map to interpret loan use codes 

mapping <- list(
  'A40'='car (new)',
  'A41'='car (used)',
  'A42'='furniture/equipment',
  'A43'='radio/television',
  'A44'='domestic appliances'
)

########################################################################################################


# Title: Transforming the car data 

for(i in 1:(dim(d))[2]) {             	# Note: 1 
  if(class(d[,i])=='character') {
    d[,i] <- as.factor(as.character(mapping[d[,i]]))  	# Note: 2 
  }
}

# Note 1: 
#   (dim(d))[2] is the number of columns 
#   in the data frame d. 

# Note 2: 
#   Note that the indexing operator [] is vectorized. Each step in the for loop remaps an 
#   entire column of data through our list. 

########################################################################################################


# Title: Summary of Good.Loan and Purpose 

table(d$Purpose,d$Good.Loan) 

########################################################################################################################



