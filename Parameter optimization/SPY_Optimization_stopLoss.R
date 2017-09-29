rm.strat(qs.strategy) #clear the strategy before the re-run
rm(list = ls()) #Clear all data

#Setup library----------------------------------------------------------------
library("quantstrat")

# read in data -----------------------------------------------------------------
SPY <-get(getSymbols("SPY"))

#Set the parameters
initDate <- "2010-12-31" #Date of initiation

nSlow <- 75 #Slow moving average sample size
nFast <- 5 #Fast moving average sample size
syms <- "SPY" #The stock that we are interested in
initEq <- 1000 #Initial equity
stopLossRange <- seq(0.05, 10, length.out = 50)/100
threshold <- 0.0005 #Threshold
orderqty <- 100 #Qty order

#-------------------------Initiate portfolio and account-------------------------------------------
qs.strategy <- "qsFaber" #Name the strategy

initPortf(qs.strategy, syms, initDate = initDate) #Initiate portfolio
initAcct(qs.strategy, portfolios = qs.strategy, initDate = initDate, initEq = initEq) #Initiate account
initOrders(portfolio = qs.strategy, initDate = initDate) #Initiate account

strategy(qs.strategy, store = TRUE) #Store all the events in the strategy

#----------Add indicators-----------------------
add.indicator(qs.strategy,
              name = "SMA",
              arguments = list (
                x = quote(Cl(mktdata)[,1]),
                n  = nFast
              ),
              label = "nFast"
)

add.indicator(qs.strategy,
              name = "SMA",
              arguments = list(
                x = quote(Cl(mktdata)[,1]),
                n = nSlow
              ),
              label = "nSlow"
)

#----------Add signals-----------------------
add.signal(qs.strategy,
           name = "sigCrossover",
           arguments = list(
             columns = c("nFast", "nSlow"),
             relationship = "gte"
           ),
           label = "long"
)

add.signal(qs.strategy,
           name = "sigCrossover",
           arguments = list(
             columns = c("nFast", "nSlow"),
             relationship = "lt"
           ),
           label = "short")

#----------Add rule-----------------------
#EnterLONG
add.rule(qs.strategy,
         name = "ruleSignal",
         arguments = list(sigcol = "long",
                          sigval = TRUE,
                          orderside = "long",
                          ordertype = "stoplimit",
                          prefer = "High",
                          threshold = threshold,
                          orderqty = +orderqty,
                          replace = FALSE,
                          orderset = "ocolong"),
         type = "enter",
         label = "EnterLONG")

#Exit2SHORT
add.rule(qs.strategy,
         name = "ruleSignal",
         arguments = list (sigcol = "short",
                           sigval = TRUE,
                           orderside = "long",
                           ordertype = "market",
                           orderqty = "all",
                           replace = TRUE,
                           orderset = "ocolong"),
         type = "exit",
         label = "Exit2SHORT"
)

#Stop Loss
add.rule(qs.strategy,
         name = "ruleSignal",
         arguments = list (sigcol = "long",
                           sigval = TRUE,
                           replace = FALSE,
                           orderside = "long",
                           ordertype = "stoplimit",
                           tmult = TRUE,
                           treshold = quote(stopLoss),
                           orderqty = "all",
                           orderset = "ocolong"),
         type = "chain",
         parent = "EnterLONG",
         label = "StopLossLONG",
         enabled = TRUE)

add.distribution(qs.strategy,
                 paramset.label = "stopLossLONG",
                 component.type = "chain",
                 component.label = "StopLossLONG",
                 variable = list(threshold = stopLossRange),
                 label = "StopLossLONG")


#----------Apply strategy-----------------------
out <- apply.paramset(qs.strategy,
                      paramset.label = "stopLossLONG",
                      portfolio.st = qs.strategy,
                      account.st = qs.strategy,
                      nsamples = 0,
                      verbose = TRUE)

updatePortf(qs.strategy)
updateAcct(qs.strategy)
updateEndEq(qs.strategy)

tradeStats <- out$tradeStats
graphData <- cbind(tradeStats$StopLossLONG, tradeStats$Net.Trading.PL)
colnames(graphData) <- c("Stop Loss %", "Net PL")
graphData[,1] <- graphData[,1]*100
plot(graphData, main = "SPY - PL Vs stop loss %")
View(graphData)
View(tradeStats)



