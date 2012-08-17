import Pyro.core
from Pyro.EventService.Clients import Subscriber

from ib.ext.Contract import Contract
from ib.ext.Order import Order
from ib.opt import ibConnection, message
from ib.ext.ScannerSubscription import ScannerSubscription

from time import *
from datetime import *


# create an object corresponding to the server
o = Pyro.core.getProxyForURI("PYRONAME://serverInterface")

#############################################
# Test 1: global information
#############################################

# create an Account information listener class
class AccountSubscriber(Subscriber):
    def __init__(self):
        Subscriber.__init__(self)
        self.subscribe("UpdateAccountValue")
        self.subscribe("UpdatePortfolio")
        self.subscribe("UpdateAccountTime")

    def event(self, event):
        print "receive a data: " + str(event)

# and one instance
sub = AccountSubscriber()

# ask the server to send account information
o.accountStatus(True)

# listening 
# TODO: this is blocking ...
sub.listen()

# cancel request
o.accountStatus(False)

#############################################
# Test2: orders
#############################################

# make a contract (an Ibpy one)
c = Contract()
c.m_symbol = "GS"
c.m_secType = 'STK'
c.m_exchange = "SMART"
c.m_currency = "USD"

# create a market order (with no chance to execute)
order1 = Order()
order1.m_action = "BUY"
order1.m_tif = "DAY"
order1.m_orderType = 'LMT'
order1.m_totalQuantity = 100
order1.m_openClose = "O"
order1.m_lmtPrice = float(round(0,2))

# execute it without time out
oid1 = o.placeOrder(c, order2, None)
print "orderId1: " + str(oid1)

# create a lmt order (with no chance to execute)
order2 = Order()
order2.m_action = "BUY"
order2.m_tif = "DAY"
order2.m_orderType = 'LMT'
order2.m_totalQuantity = 100
order2.m_openClose = "O"
order2.m_lmtPrice = float(round(0,2))

# execute the order with a timeout of 3 seconds
oid2 = o.placeOrder(c, order1, timedelta(seconds=3))
print "orderId2: " + str(oid2)

# waiting for some time
sleep(5)

# displaying the orders status
print o.orderStatus(oid1)
print o.orderStatus(oid2)

# we cancel order 1
o.cancelOrder(oid1)
print o.orderStatus(oid1)

#############################################
# Test3: Market scanning
#############################################

# we create a scanner
subscript = ScannerSubscription() 
subscript.numberOfRows(200) 
subscript.m_instrument = 'STK' 
subscript.m_locationCode = 'STK.AMEX'
subscript.m_scanCode = "MOST_ACTIVE"
subscript.m_stockTypeFilter = 'ALL' 
subscript.m_m_abovePrice = 0.0
subscript.m_aboveVolume = 0
subscript.m_marketCapAbove = 0.0

# and execute it on the server
print str(o.scanMkt(subscript))

#############################################
# Test4: Market Data
#############################################

# build a contract
c = Contract()
c.m_symbol = "GS"
c.m_secType = 'STK'
c.m_exchange = "SMART"
c.m_currency = "USD"

# request the data
dataid = o.reqMktData(c, False)
print dataid

# read current data
for i in range(0, 10):
    print str(o.getMktData(dataid))
    sleep(1)

# cancel data request
o.cancelMktData(dataid)

#############################################
# Test5: Market Depth
#############################################

# build a contract
c = Contract()
c.m_symbol = "GS"
c.m_secType = 'STK'
c.m_exchange = "SMART"
c.m_currency = "USD"

# request market depth
dataid = o.reqMktDepth(c, 5)
print dataid

# read the depth
for i in range(0, 10):
    print str(o.getMktDepth(dataid))
    sleep(1)

# cancel request
o.cancelMktDepth(dataid)

#############################################
# Test6: Contract Details
#############################################

# build a contract (pattern)
c = Contract()
c.m_symbol = "GS"
c.m_secType = 'STK'
c.m_currency = "USD"

# ask for the details of contract with similar fields
l = o.reqContractDetails(c)

# print details
for i in l:
    print "MarketName: " + i.m_marketName + ", secType:" + i.m_summary.m_secType + ", exchange: " + i.m_summary.m_exchange

#############################################
# Test7: new bulletins
#############################################

# create a suscriber object and an instance
class NewsSubscriber(Subscriber):
    def __init__(self):
        Subscriber.__init__(self)
        self.subscribe("NewBulletins")

    def event(self, event):
        print "receive a news: " + str(event)

sub = NewsSubscriber()

# request the news
o.reqNewsBulletins(True)

# listening: TODO non-blocking one
sub.listen()

# canceling request
o.cancelNewsBulletins()


#############################################
# Test8: historical data
#############################################

# create a contract
c = Contract()
c.m_symbol = "DELL"
c.m_secType = 'STK'
c.m_exchange = "SMART"
c.m_currency = "USD"

# create ending time 
mnow = datetime.now()

# create starting time
delta = timedelta(hours=2)
delta2 = timedelta(days=1)
mstr = (mnow - delta2 + delta).strftime("%Y%m%d %H:%M:%S")

# ask for historical data
dataid = o.reqHistData(c, mstr, "1 D", "5 mins", "MIDPOINT", 0, 1)
print dataid

# wait a bit
sleep(5)

# grab the historical data
hist = o.getHistData(dataid)
for i in hist:
    print i

# cancel the request
o.cancelHistData(dataid)

#############################################
# Test9: Real Time Bars
#############################################

# create a contract
c = Contract()
c.m_symbol = "DELL"
c.m_secType = 'STK'
c.m_exchange = "SMART"
c.m_currency = "USD"

# request realtime bars
dataid = o.reqRTData(c, "5 secs", "MIDPOINT", 0)
print dataid

# read the bars
for i in range(0, 10):
    print str(o.getRTData(dataid))
    sleep(1)

# cancel request
o.cancelRTData(dataid)

#############################################
# Test 10: Executions information
#############################################

# request information
execs = o.reqExecutions()

# printing them
for i in execs:
    print str(i)

#############################################
# Test 11: Fundamental Data
#############################################

# create a contract
c = Contract()
c.m_symbol = "DELL"
c.m_secType = 'STK'
c.m_exchange = "SMART"
c.m_currency = "USD"

# request the data
dataid = o.reqFundamentalData(c, "Summary")
print dataid

# wait a bit
sleep(5)

# print the data
print o.getFundamentalData(dataid)

# cancel request
o.cancelFundamentalData(dataid)


