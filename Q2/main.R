options(digits=10)
library(dplyr)
setwd('~/dataIncubator/Q2')

if(!file.exists("Data")) {
  dir.create("Data")
}

setwd('~/dataIncubator/Q2/Data')

if(!file.exists("LCDataDictionary.xlsx")) {
  download.file('https://resources.lendingclub.com/LCDataDictionary.xlsx', dest='LCDataDictionary.xlsx')
}

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
cat("What is the difference in the fraction of the loans with a 36-month 
term between 2014 and 2015?")

loanData$issue_year <- sub(".*-", "", as.character(loanData$issue_d))
loanData$issue_month <- sub("-.*", "", as.character(loanData$issue_d))

tbl_df(loanData) %>% 
  group_by(issue_year, term) %>% 
  summarise (n = n()) %>%
  mutate(year_freq = n / sum(n))
# issue_year       term      n    year_freq
# <chr>     <fctr>  <int>        <dbl>
#  1       2014  36 months 162570 0.6899405421
# 2       2014  60 months  73059 0.3100594579
# 3       2015  36 months 283173 0.6724682079
# 4       2015  60 months 137922 0.3275317921

print(abs(0.6899405421 - 0.6724682079))


## Q5  ##
cat("We will consider all loans that are not in the 'Fully Paid', 
'Current', 'In Grace Period' statuses to be in default. Calculate 
the ratio of the time spent paying the loan, defined as the difference 
between the last payment date and the issue date, divided by the term 
of loan. What is the standard deviation of this ratio for all the 
loans in default?")

monthsIntoPeriod <-
  c("Jan-2014"=1, "Feb-2014"=2, "Mar-2014"=3, "Apr-2014"=4, "May-2014"=5, "Jun-2014"=6, 
    "Jul-2014"=7, "Aug-2014"=8, "Sep-2014"=9, "Oct-2014"=10, "Nov-2014"=11, "Dec-2014"=12,
    "Jan-2015"=13, "Feb-2015"=14, "Mar-2015"=15, "Apr-2015"=16, "May-2015"=17, "Jun-2015"=18, 
    "Jul-2015"=19, "Aug-2015"=20, "Sep-2015"=21, "Oct-2015"=22, "Nov-2015"=23, "Dec-2015"=24,
    "Jan-2016"=25, "Feb-2016"=26, "Mar-2016"=27, "Apr-2016"=28, "May-2016"=29, "Jun-2016"=30, 
    "Jul-2016"=31, "Aug-2016"=32, "Sep-2016"=33, "Oct-2016"=34, "Nov-2016"=35, "Dec-2016"=36,
    "Jan-2017"=37, "Feb-2017"=38, "Mar-2017"=39, "Apr-2017"=40, "May-2017"=41, "Jun-2017"=42, 
    "Jul-2017"=43, "Aug-2017"=44, "Sep-2017"=45, "Oct-2017"=46, "Nov-2017"=47, "Dec-2017"=48)
loanPeriod <- c(" 36 months"=36, " 60 months"=60)

loanData$issue_d_monthsInto2014 <- monthsIntoPeriod[as.character(loanData$issue_d)]
loanData$last_pymnt_d_monthsInto2014 <- monthsIntoPeriod[as.character(loanData$last_pymnt_d)]
loanData$term_inMonths <- loanPeriod[as.character(loanData$term)]

inDefaultLoanData <- subset(loanData, 
                          !(loan_status %in% c("Fully Paid", 
                                             "Current", 
                                             "In Grace Period")))

#TODO: Understand if I should be removing NA's
sd((inDefaultLoanData$last_pymnt_d_monthsInto2014 
    - inDefaultLoanData$issue_d_monthsInto2014) / inDefaultLoanData$term_inMonths,
   na.rm=TRUE)


## Q6  ##
cat("What is the Pearson correlation coefficient between the total rate 
of return, as figured from the total payments and the loan amount, and 
the interest rate? Consider only loans that have reached the end of 
their term.")

endTermLoanData <- subset(loanData, loan_status == "Fully Paid")

cor(x = (endTermLoanData$total_pymnt - endTermLoanData$loan_amnt),
    y= endTermLoanData$int_rate, 
    method="pearson")

## Q7  ##
print("Let's find a loan purpose that shows up abnormally often in one 
      state. Call A the probability of a loan going to a specific purpose 
      nationwide. Call B the probability of a loan going to a specific 
      purpose for each state. Out of all (state, purpose) pairs with at 
      least 10 loans, what is the highest ratio of B / A (i.e. the most 
      surprising)?")

A <- tbl_df(loanData) %>% 
  group_by(purpose) %>% 
  summarise (n = n()) %>%
  mutate(purpose_freq = n / sum(n))

B <- tbl_df(loanData) %>% 
  group_by(addr_state, purpose) %>% 
  summarise (n = n()) %>%
  mutate(bystate_purpose_freq = n / sum(n))
A[['n']] <- NULL
B[['n']] <- NULL
AB <- merge(A,B)
AB <- AB %>%
  mutate(surprisingness = bystate_purpose_freq/purpose_freq) %>%
  arrange(-surprisingness)

head(AB)
AB$surprisingness[1]


## Q8  ##
print("Group all loans by their sub-grade and calculate their average 
      inerest rate and average default rate. Build a linear model to predict 
      the (average) default rate from the (average) interest rate. Find that 
      sub-grade with the largest absolute deviation from this model. What is 
      the deviation of the actual default rate from the predicted value?")

loanData$inDefault <- !(loanData$loan_status %in% c("Fully Paid", 
                                                    "Current", 
                                                    "In Grace Period"))

xy <- tbl_df(loanData) %>% 
  group_by(sub_grade) %>% 
  summarize(meanInterestRate = mean(int_rate), fractionInDefault = mean(inDefault))
lmfit <- lm(fractionInDefault~meanInterestRate, xy)
max(abs(residuals(lmfit)))

plot(fractionInDefault~meanInterestRate, xy)
abline(lmfit, col='red')


## Q9  ##
print("Please provide the script used to generate this result (max 10000 characters).")
cat(paste(readLines("../main.R"), sep='\n'))
