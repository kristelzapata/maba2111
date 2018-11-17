# KRISTEL JOYCE ZAPATA
# 184138

# MACHINE PROBLEM

# Define an R function that removes NA values from a vector.

remove.na = function(x){x[!is.na(x)]}


# Define an R function that computes the factorial of given an integer argument. The output should be a vector of length 1.

fact <- function(x) {
  if(x <= 0) {
    return(1)
  } else { 
    return(x*fact(x-1))
  }
}

# Define an R function that computes the determinant of a given matrix. The output should be a vector of length 1.
# Define an R function that sorts a given vector in decreasing order. The output should be a vector of the same length. It should accept both numeric or character vectors. 
# Define an R function that accepts a POSIXct as argument and outputs the day of the week as characters. 

# Create a function to compute for your net pay at work

# monthly tax table from: https://www.pinoymoneytalk.com/new-withholding-tax-table-bir-circular-rmc-1-2018/?fbclid=IwAR16YoyRHLaCixhOHZBF_XkL6t3Qg22hOaTZGCAPrKPkhbTTKplcjd7MNv0
# OTI - other taxable income (Overtime, premium, other allowances, etc)
# MD - mandatory deductions (SSS, PhilHealth, Pag-ibig)

monthly.net.pay <- function(monthly.basic.pay, OTI = 0, MD = 0) {
  
  taxable.income = monthly.basic.pay + OTI - MD
  
  if (taxable.income <= 20833) {
    monthly.net.pay = taxable.income
  } else if (taxable.income <= 33332) {
    monthly.net.pay = (taxable.income - 20833) * 0.2
  } else if (taxable.income <= 66666) {
    monthly.net.pay = ((taxable.income - 33333) * 0.25) + 2500
  } else if (taxable.income <= 166666) {
    monthly.net.pay = ((taxable.income - 66667) * 0.3) + 10833.33
  } else if (taxable.income <= 666666) {
    monthly.net.pay = ((taxable.income - 166667) * 0.32) + 40833.33
  } else {
    monthly.net.pay = ((taxable.income - 666667) * 0.35) + 200833.33
  }
  print(monthly.net.pay)
}


# Create a function that accepts a vector and and integer n and returns nth highest number



# Create a function that computes the compound interest of an investment given the rate, time, and initial amount or principal. 

# compound.interest = the future value (principal + interest)
# principal = the principal amount (the initial loan amount)
# r = the annual interest rate (decimal)
# n = the number of times that interest is compounded per year
# t = the number of years the money is invested or borrowed for

compound.interest <- function(principal, r = 0.1, n = 1, t = 1){
  rate = (1+(r/n))
  time = n*t
  y <- (principal*(rate^time))
  print(y)
}


# Create a function isPrime(n) that accepts an integer and outputs a Boolean value (TRUE or FALSE) depending whether the integer is a prime 

isPrime <- function(x){
  if(sum(x/1:x==x%/%1:x)==2) {
    print("TRUE")
  }
  else {
    print("FALSE")
  }  
}