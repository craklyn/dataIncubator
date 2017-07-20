setwd('~/dataIncubator/Q2')

if(!file.exists("Data")) {
  dir.create("Data")
}

setwd('~/dataIncubator/Q2/Data')

if(!file.exists("LoanStats3c.csv.zip")) {
  download.file('https://resources.lendingclub.com/LoanStats3c.csv.zip', dest='LoanStats3c.csv.zip')
  unzip('LoanStats3c.csv.zip')
}

if(!file.exists("LoanStats3d.csv.zip")) {
  download.file('https://resources.lendingclub.com/LoanStats3d.csv.zip', dest='LoanStats3d.csv.zip')
  unzip('LoanStats3d.csv.zip')    
}
            

data2014 <- read.csv("LoanStats3c.csv", skip=1)
# The last two rows are not useful for this analysis.
data2014 <- data2014[-nrow(data2014),]
data2014 <- data2014[-nrow(data2014),]

data2015 <- read.csv("LoanStats3d.csv", skip=1)
data2015 <- data2015[-nrow(data2015),]
data2015 <- data2015[-nrow(data2015),]

loanData <- rbind(data2014, data2015)
rm(data2014, data2015)

## Q1  ##
print("What is the median loan amount?")
median(loanData$loan_amnt)

## Q2  ##
print("Each loan is categorized into a single purpose. What fraction of 
      all loans are for the most common purpose?")

# Find which is most common: names(which.max(table(loanData$purpose)))
max(table(loanData$purpose)) / nrow(loanData)

## Q3  ##
print("Calculate the average interest rate across loans for each purpose. 
      What is the ratio of minimum average rate to the maximum average rate? 
      (The ratio should be less than 1.)")

# Remove the % and change int_rate from factor/character to numeric.
loanData$int_rate <- as.numeric(sub("%","",as.character(loanData$int_rate)))/100

minIntRate <- tbl_df(loanData) %>% 
  group_by(purpose) %>% 
  summarize(meanInterestRate = mean(int_rate)) %>%
  slice(which.min(meanInterestRate))

maxIntRate <- tbl_df(loanData) %>% 
  group_by(purpose) %>% 
  summarize(meanInterestRate = mean(int_rate)) %>%
  slice(which.max(meanInterestRate))

print(minIntRate$meanInterestRate / maxIntRate$meanInterestRate)


## Q4  ##
print("What is the difference in the fraction of the loans with a 36-month 
      term between 2014 and 2015?")

loanData$issue_year <- sub(".*-", "", as.character(loanData$issue_d))
loanData$issue_month <- sub("-.*", "", as.character(loanData$issue_d))


minIntRate <- tbl_df(loanData) %>% 
  group_by(issue_year) %>% 
  summarize(meanInterestRate = count()) %>%
  
trimws(as.character(loanData$term)) == "36 months"



## Q5  ##
print("We will consider all loans that are not in the 'Fully Paid', 
      'Current', 'In Grace Period' statuses to be in default. Calculate 
      the ratio of the time spent paying the loan, defined as the difference 
      between the last payment date and the issue date, divided by the term 
      of loan. What is the standard deviation of this ratio for all the 
      loans in default?")

## Q6  ##
print("What is the Pearson correlation coefficient between the total rate 
of return, as figured from the total payments and the loan amount, and 
      the interest rate? Consider only loans that have reached the end of 
      their term.")

## Q7  ##
print("Let's find a loan purpose that shows up abnormally often in one 
      state. Call AA the probability of a loan going to a specific purpose 
      nationwide. Call BB the probability of a loan going to a specific 
      purpose for each state. Out of all (state, purpose) pairs with at 
      least 10 loans, what is the highest ratio of B / A (i.e. the most 
      surprising)?")

## Q8  ##
print("Group all loans by their sub-grade and calculate their average 
      inerest rate and average default rate. Build a linear model to predict 
      the (average) default rate from the (average) interest rate. Find that 
      sub-grade with the largest absolute deviation from this model. What is 
      the deviation of the actual default rate from the predicted value?")

## Q9  ##
print("Please provide the script used to generate this result (max 10000 characters).")
cat(paste(readLines("../main.R"), sep='\n'))
