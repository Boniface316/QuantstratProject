#Setup library----------------------------------------------------------------
library("quantstrat")

#Set the currency and stock
currency("USD")
stock("SPY",currency="USD",multiplier=1)

initDate <- "2006-01-02" #Date of initiation
from <- "2007-01-03" #Start date of the data set
to <- "2018-03-18" #End date of the data set
initEq <- 1000 #Initial equity
nFast <- 10
nSlow <- 30
orderqty <- 100
# read in data -----------------------------------------------------------------
getSymbols("SPY", from = from, to = to)

#Set the parameters


#-------------------------Initiate portfolio and account-------------------------------------------
strategy.st <- "luxor" #Name the strategy
account.st <- "luxor"
portfolio.st <- "luxor"


initPortf(strategy.st,symbols = "SPY", initDate = initDate) #Initiate portfolio
initAcct(account.st, portfolios = portfolio.st, initDate = initDate, initEq = initEq) #Initiate account
initOrders(portfolio = portfolio.st, initDate = initDate) #Initiate account

strategy(strategy.st, store = TRUE) #Store all the events in the strategy

### indicators


add.indicator(strategy.st, name = "SMA",
              arguments = list(
                x = quote(Cl(mktdata)[,1]),
                n = nFast
              ),
              label="nFast"
)

add.indicator(strategy.st, name="SMA",
              arguments = list(
                x = quote(Cl(mktdata)[,1]),
                n = nSlow
              ),
              label="nSlow"
)

### signals

add.signal(strategy.st, name='sigCrossover',
           arguments = list(
             columns=c("nFast","nSlow"),
             relationship="gte"
           ),
           label='long'
)

add.signal(strategy.st, name='sigCrossover',
           arguments = list(
             columns=c("nFast","nSlow"),
             relationship="lt"
           ),
           label='short'
)

### rules


add.rule(strategy.st, name='ruleSignal',
         arguments=list(sigcol='short', sigval=TRUE,
                        orderside='long' ,
                        ordertype='market',
                        orderqty='all',
                        replace=TRUE
         ),
         type='exit',
         label='Exit2SHORT'
)

add.rule(strategy.st, name='ruleSignal',
         arguments=list(sigcol='long' , sigval=TRUE,
                        orderside='long' ,
                        ordertype='market', 
                        orderqty=orderqty,
                        replace=FALSE
         ),
         type='enter',
         label='EnterLONG'
)


###############################################################################

applyStrategy(strategy.st, portfolio.st)

#update
updatePortf(strategy.st)
updateAcct(strategy.st)
updateEndEq(strategy.st)

#Plot chart
chart.Posn(portfolio.st, "SPY")
