rm.strat(qs.strategy) #clear the strategy before the re-run
#rm(list = ls()) #Clear all data

#Setup library----------------------------------------------------------------
library("quantstrat")
library("rgl")

# read in data -----------------------------------------------------------------
SPY <-get(getSymbols("SPY"))["2011"]

#Set the parameters
initDate <- "2010-12-31" #Date of initiation

nSlow <- 30 #Slow moving average sample size
nFast <- 10 #Fast moving average sample size
syms <- "SPY" #The stock that we are interested in
initEq <- 1000 #Initial equity
FastSMA <- 1:10 #Range of moving average
SlowSMA <- seq(20,80,5) #Range of moving average
threshold <- 0.0005 #Threshold
orderqty <- 100 #Qty order

#-------------------------Initiate portfolio and account-------------------------------------------
qs.strategy <- "qsFaber" #Name the strategy

initPortf(qs.strategy, syms, initDate = initDate) #Initiate portfolio
initAcct(qs.strategy, portfolios = qs.strategy, initDate = initDate, initEq = initEq) #Initiate account
initOrders(portfolio = qs.strategy, initDate = initDate) #Initiate account

strategy(qs.strategy, store = TRUE) #Store all the events in the strategy

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

#--------------------------------------------------------------------
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

add.rule(qs.strategy,
         name = "ruleSignal",
         arguments = list(sigcol = "long",
                          sigval = TRUE,
                          orderside = "long",
                          ordertype = "stoplimit",
                          prefer = "High",
                          threshold = threshold,
                          orderqty = +orderqty,
                          replace = FALSE
                          ),
         type = "enter",
         label = "EnterLONG")


add.rule(qs.strategy,
         name = "ruleSignal",
         arguments = list (sigcol = "short",
                           sigval = TRUE,
                           orderside = "long",
                           ordertype = "market",
                           orderqty = "all",
                           replace = TRUE
                           ),
         type = "exit",
         label = "Exit2SHORT")

add.distribution(qs.strategy,
                 paramset.label = "SMA",
                 component.type = "indicator",
                 component.label = "nFast",
                 variable = list(n = FastSMA),
                 label = "nFAST")

add.distribution(qs.strategy,
                 paramset.label = "SMA",
                 component.type = "indicator",
                 component.label = "nSlow",
                 variable = list(n = SlowSMA),
                 label = "nSLOW")

add.distribution.constraint(qs.strategy,
                            paramset.label = "SMA",
                            distribution.label.1 = "nFAST",
                            distribution.label.2 = "nSLOW",
                            operator = "<",
                            label = "SMA")


out <- apply.paramset(qs.strategy, paramset.label = "SMA", portfolio.st = qs.strategy, account.st = qs.strategy, nsamples = 0)

updatePortf(qs.strategy)
updateAcct(qs.strategy)
updateEndEq(qs.strategy)

tS <- out$tradeStats
idx <- order(tS[,1],tS[,2])
tS <- tS[idx,]

tradeGraphs(stats = tS, free.params = c("nFAST", "nSLOW"),
            statistics = c("Net.Trading.PL", ""))


