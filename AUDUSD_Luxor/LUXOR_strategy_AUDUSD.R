#rm.strat(qs.strategy) #clear the strategy before the re-run
rm(list = ls()) #Clear all data 

#Setup library----------------------------------------------------------------
library("quantstrat")

# read in data -----------------------------------------------------------------
sym1 <- "AUDUSD"
syms <- sym1

#Set the parameters
initDate <- "2012-12-31" #Date of initiation 
from <- "2013-01-01" #Start date of the data set
to <- "2013-01-30" #End date of the data set
initEq <- 1000 #Initial equity

#-------------------------Function to import data------------------------------------------------
getFXSymbols <- function(Symbols,
                         from = getOption("getSymbols.FI.from", "2010-01-01"),
                         to = getOption("getSymbols.FI.to", Sys.Date()),
                         dir = get_data_storage_directory(),
                         days_to_omit = "Saturday",
                         prepend_sym_name_to_cols = TRUE,
                         ...) {
  getSymbols.FI(Symbols = Symbols,
                from = from,
                to = to,
                dir = "Location where the data is stored",
                days_to_omit = days_to_omit,
                prepend_sym_name_to_cols = prepend_sym_name_to_cols,
                ... = ...)
  
  # Now for each symbol in the global environment, update the column names:
  for (sym in Symbols) {
    tmp <- get(sym, envir = .GlobalEnv)
    colnames(tmp) <- paste0(sym, ".", colnames(tmp))
    assign(sym, value = tmp, envir = .GlobalEnv)
  }
  return()
}



#-------------------------Generate data----------------------------------------------------------
lapply(X = syms, FUN = getFXSymbols, from = from, to = to) #Import data
mkData <- Cl(AUDUSD) #Store only the close price of the data
colnames(mkData) <- "Close" #Naming the column

#-------------------------Initiate portfolio and account-------------------------------------------
qs.strategy <- "qsFaber" #Name the strategy

initPortf(qs.strategy, "mkData", initDate = initDate) #Initiate portfolio
initAcct(qs.strategy, portfolios = qs.strategy, initDate = initDate, initEq = initEq) #Initiate account
initOrders(portfolio = qs.strategy, initDate = initDate) #Initiate account

strategy(qs.strategy, store = TRUE) #Store all the events in the strategy

#-------------------------Add indicator-------------------
#Calculate the moving average for 30 periods
add.indicator(strategy = qs.strategy,
              name = "SMA",
              arguments = list (x = mkData$Close,
                                n = 30),
              label = "SMA30"
              )

#-------------------------Add signals ---------------------
add.signal(qs.strategy, 
           name = "sigCrossover",
           arguments = list(columns = c("Close", "SMA30"),
                            relationship = "gt"),
           label = "Cl.gt.SMA")

add.signal(qs.strategy, 
           name = "sigCrossover",
           arguments = list(columns = c("Close", "SMA30"),
                            relationship = "lt"),
           label = "Cl.lt.SMA")
#-------------------------Add rules to buy and sell ---------
#Buy 100 when the enter signal is triggered and sell all when the exit signal is triggered
add.rule(qs.strategy, name = "ruleSignal",
         arguments = list(sigcol = "Cl.gt.SMA",
                          sigval = TRUE,
                          orderqty = 100,
                          ordertype = "market",
                          orderside = "long"),
         type = "enter")

add.rule(qs.strategy, name = "ruleSignal",
         arguments = list (sigcol = "Cl.lt.SMA",
                           sigval = TRUE,
                           orderqty = "all",
                           ordertype = "market",
                           orderside = "long"),
         type = "exit")
#-------------------------Apply Strategy ------------
applyStrategy(strategy = qs.strategy, portfolios = qs.strategy)
getTxns(Portfolio = qs.strategy, Symbol = "mkData")

#-------------------------Update portfolio, account and equity------------
updatePortf(qs.strategy)
updateAcct(qs.strategy)
updateEndEq(qs.strategy)

#-------------------------Plot performance-------------------
myTheme <- chart_theme()
myTheme$col$dn.col<-'lightblue'
myTheme$col$dn.border <- 'lightgray'
myTheme$col$up.border <- 'lightgray'

chart.Posn(qs.strategy, Symbol = 'mkData', Dates = '2013::',theme=myTheme,
           TA='add_SMA(n=10,col=4, on=1, lwd=2)')
