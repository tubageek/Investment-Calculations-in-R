# Pull in any configuration information
source("C:/R Packages/Config.R")

pv <- function (cashflows, rate)
{
  discounts <- cashflows$Amount * (1 + rate)^(cashflows$Days / 365)
  return (sum(discounts))
}

pvDX <- function(cashflows, rate)
{
  discounts <- cashflows$Amount * (cashflows$Days / 365) * (1 + rate) ^ ((cashflows$Days - 365) / 365)
  return (sum(discounts))
}

recSolve <- function(cashflows, rate, iteration, target) 
{
  if (iteration >= 100) {
    return("The maximum number of iterations has been reached")
  }
  
  presentVal <- pv(cashflows, rate)
  presentValDX <- pvDX(cashflows, rate)
  
  if (abs (target - presentVal) <= 0.001) {
    return (rate)
  }
  else {
    return (recSolve (cashflows, (rate - (presentVal - target) / presentValDX), (iteration + 1), target))
  }
}

getCurrentPrice <- function(fundSymbol)
{
  library(rjson)
  rawResponse <- readLines(paste("http://finance.google.com/finance/info?client=ig&q=", fundSymbol, sep = ""))
  cleanedResponse <- c(rawResponse[3:20])
  responseAsString <- paste(cleanedResponse, collapse = "")
  parsedJson = fromJSON(responseAsString, method = "C")
  return (as.numeric(parsedJson[[4]]))
}

solveIRR <- function(allTransactions) {
  mutualFund <- allTransactions$MutualFundSymbol[1]
  currentPrice <- getCurrentPrice(mutualFund)
  numberOfShares <- sum(allTransactions$NumberOfShares)
  investments <- subset(allTransactions, allTransactions$ReturnType == "Investment")
  return (recSolve(investments, 0.0, 0, currentPrice * numberOfShares))
}

calculateIRR <- function()
{
  library(RODBC)
  
  dbhandle <- odbcDriverConnect(DBConnectionString)
  allTransactions <- sqlQuery(dbhandle, "select * from lta.MutualFundView WHERE HasBalance = 1 AND IsRetirementFund = 0")
  
  allTransactions$TransactionDate <- as.Date(allTransactions$TransactionDate)
  allTransactions$Days <- as.integer(Sys.Date() - allTransactions$TransactionDate)
  
  fundFactor <- allTransactions$MutualFundSymbol

  by(allTransactions, fundFactor, solveIRR)
}
