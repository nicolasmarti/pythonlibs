from storegraph import Storegraph
from gspeech import speack
from googlesheet import GoogleSheet
from datetime import *

from pickle import *

import yahoojap
import sys
import time

import matplotlib.pyplot as plt
import matplotlib.font_manager as font_manager

import random

class Strat():

    def __init__(self, store = None, name = None):

        # we initialize the store
        if store == None:
            self.store = Storegraph(_globals = globals())
        else:
            self.store = store

        #we initialize the name of the strat
        if name == None:
            self.name = "strat" + str(random.randint(0,1000000))
        else:
            self.name = name

        #the state of the automaton
        # 0 := closed
        # 1 := opening
        # 2 := cancelling opening
        # 3 := opened
        # 4 := closing
        # 5 := cancelling closing
        self.store[self.name]["state"] = 0

        # the opening/closing time out (default: None)
        self.store[self.name]["openingtimeout"] = None
        self.store[self.name]["closingtimeout"] = None

        # the number of bar so far
        self.store[self.name]["nb_bars"] = 0

    #######################################################
    # the function of the strategy 

    # the entry/exit signal emitting functions
    # return a pair of size(>0 -> buy | < 0 -> sell)/price
    def entry(self):
        return None

    # return a price
    def exit(self):
        return None

    # the update function
    def update(self):
        return None

    #######################################################
    # the brokers functions

    # the order function
    def order(self, size, price):
        return None

    # the order close function
    def close(self):
        return None

    # the cancel function
    def cancel(self):
        return None

    #######################################################

    # compute the pnl (in price) and upnl (in size)
    def pnl_upnl(self):
        pnl = 0
        upnl = 0
        for i in self.store[self.name]["order"].keys():
            order = self.store[self.name]["order"][i]
            pnl += -order[0] * order[1]
            upnl += order[0]

        #self.store.show_graph()
        upnl2 = upnl * self.store[self.name]["bars"][self.store[self.name]["nb_bars"] - 1]["ajust. close"]

        return (pnl, upnl, upnl2, pnl + upnl2)


    #######################################################
    
    # draw stock price / pnl
    def draw(self):
        
        fig = plt.figure()
        ax = fig.add_subplot(111)
        l = map (lambda x: self.store[self.name]["bars"][x]["ajust. close"], range(0, self.store[self.name]["nb_bars"]))
        ax.plot(range(0, self.store[self.name]["nb_bars"]), l)
        ax2 = ax.twinx()
        l = map (lambda x: self.store[self.name]["pnl"][x][3], range(0, self.store[self.name]["nb_bars"]))
        ax2.plot(range(0, self.store[self.name]["nb_bars"]), l)
        return fig



    #######################################################

    # a step function
    def step(self):
        
        # first we update
        self.update()
        
        # now a case analysis on the state
        st = self.store[self.name]["state"]


        # we are closed, look for an entry signal
        if self.store[self.name]["state"] == 0:
            entry_sig = self.entry()
            # we have one signal
            if entry_sig <> None:
                # we are now in opening state
                self.store[self.name]["state"] = 1
                self.store[self.name]["openingtime"] = self.store[self.name]["nb_bars"]
                # we create the order
                self.order(entry_sig[0], entry_sig[1])

                
        # we are opening, do we reach a time out?
        # (if the order is filled, a different thread should put us in opened state
        if self.store[self.name]["state"] == 1:
            # if we have a time out, we check it does not expire
            if self.store[self.name]["openingtimeout"] <> None:
                if self.store[self.name]["openingtimeout"] + self.store[self.name]["openingtime"] < self.store[self.name]["nb_bars"]:
                    # put ourselves in cancelling opening state and cancel the order
                    self.store[self.name]["state"] = 2
                    self.cancel()
                    
        # we are cancelling opening, nothing to do
        if self.store[self.name]["state"] == 2:
            pass

        # we are opened, look for an exit signal
        if self.store[self.name]["state"] == 3:
            exit_sig = self.exit()
            # we have one signal
            if exit_sig <> None:
                # we are now in closing state
                self.store[self.name]["state"] = 4
                self.store[self.name]["closingtime"] = datetime.now()

                # we create the order
                self.close()

            
        # we are in closing, do we reach a time out?
        if self.store[self.name]["state"] == 4:
            # if we have a time out, we check it does not expire
            if self.store[self.name]["closingtimeout"] <> None:
                if self.store[self.name]["closingtimeout"] + self.store[self.name]["closingtime"] < self.store[self.name]["nb_bars"]:
                    # put ourselves in cancelling closing state and cancel the order
                    self.store[self.name]["state"] = 5
                    self.cancel()

        # we are cancelling closing, nothing to do
        if self.store[self.name]["state"] == 5:
            pass

        # just debug output
        if st <> self.store[self.name]["state"] and False:
            print str(st) + " --> " + str(self.store[self.name]["state"])

        # we are recording the pnl
        self.store[self.name]["pnl"][self.store[self.name]["nb_bars"] - 1] = self.pnl_upnl()
        
        #print str(self.store["pnl"][self.store["nb_bars"] - 1])


# a first derivation: a backtest with pickled data
class BackTest(Strat):
    
    def __init__(self, store = None):
        # init the Strat
        Strat.__init__(self, store = store)

    # the order function
    def order(self, size, price):
        if price == None:
            price = self.store[self.name]["bars"][self.store[self.name]["nb_bars"] - 1]["ajust. close"]
        self.store[self.name]["order"][self.store[self.name]["nb_bars"] - 1] = (size, price)

        if False:
            print "order: " + str((size, price)) + "@ " + str(self.store[self.name]["bars"][self.store[self.name]["nb_bars"] - 1]["date"]) + " | " + str(self.store[self.name]["nb_bars"] - 1) + " ==> " + str(self.pnl_upnl())

        if self.store[self.name]["state"] == 1:
            self.store[self.name]["state"] = 3

        if self.store[self.name]["state"] == 4:
            self.store[self.name]["state"] = 0

        return None

    # the order close function
    def close(self):
        pos = self.pnl_upnl()[1]
        self.order(-pos, None) 
        return None

    # the cancel function
    def cancel(self):
        return None

    # run the backtest
    def run(self):
        #print self.bars
        for i in self.bars:
            #print i

            # add the bar
            self.store[self.name]["bars"][self.store[self.name]["nb_bars"]] = i

            # increment the number of bar
            self.store[self.name]["nb_bars"] += 1

            # call the step
            self.step()

        self.store[self.name]["final pnl"] = self.pnl_upnl()

    def load(self, filename):
        # open the bars file and reverse it
        self.bars = load(open(filename, "rb"))
        self.bars.reverse()

    def getyahoojpan(self, ticker, startdate, enddate = date.today()):
        self.bars = yahoojap.get_historical(ticker, startdate, enddate, filename = None)
        self.bars.reverse()


def is_increasing(l):

    if len(l) <= 1:
        return False

    for i in range(0, len(l)):
        if l.count(l[i]) > 1:
            return False
        
    for i in range(0, len(l)-1):
        if l[i] >= l[i+1]:
            return False
    
    return True

def is_decreasing(l):

    if len(l) <= 1:
        return False

    for i in range(0, len(l)):
        if l.count(l[i]) > 1:
            return False
        
    for i in range(0, len(l)-1):
        if l[i] <= l[i+1]:
            return False

    return True



# a second strat: look for an index in the ema such that the first segment is increasing and the other decreasing
class Strat2(BackTest):
    
    def __init__(self, store = None):
        BackTest.__init__(self, store = store)

        self.store[self.name]["nbema"] = 6
        
        for i in range(0, self.store[self.name]["nbema"]):            
            if i == 0:
                self.store[self.name]["ema"][i]["period"] = 5
            else:
                self.store[self.name]["ema"][i]["period"] = self.store[self.name]["ema"][i-1]["period"] * 2
        
        if False:
            print self.store[self.name]["ema"]

        self.store[self.name]["index"] = []

    def entry(self):

        if self.store[self.name]["nb_bars"] < self.store[self.name]["ema"][self.store[self.name]["nbema"] - 1]["period"]:
            return None

        index = self.store[self.name]["nb_bars"] - 1

        indexV = self.store[self.name]["data"][index]["indexV"]
        indexA = self.store[self.name]["data"][index]["indexA"]
        
        # we go long if decreasing 
        if (indexV <> None and indexV <= 2):
            return (100, None)


        return None

    # return a price
    def exit(self):

        if self.store[self.name]["nb_bars"] < self.store[self.name]["ema"][self.store[self.name]["nbema"] - 1]["period"]:
            return None

        index = self.store[self.name]["nb_bars"] - 1

        indexV = self.store[self.name]["data"][index]["indexV"]
        indexA = self.store[self.name]["data"][index]["indexA"]

        if indexV <> None and indexV >= 3:
            return True

        if indexA <> None and indexA <= 3:
            return True


        return None


    # 
    # the update function
    def update(self):
        index = self.store[self.name]["nb_bars"] - 1
        price = self.store[self.name]["bars"][index]["ajust. close"]

        for i in range(0, self.store[self.name]["nbema"]):
            period = self.store[self.name]["ema"][i]["period"]            
            try:
                lastema = self.store[self.name]["ema"][i]["value"][index - 1]
                alpha = 2.0/(float(period)+1.0)
                newema = lastema * (1-alpha) + price * alpha
                self.store[self.name]["ema"][i]["value"][index] = newema
            except Exception as e:
                #print e
                self.store[self.name]["ema"][i]["value"][index] = price


        self.store[self.name]["data"][index]["lema"] = []

        if self.store[self.name]["nb_bars"] < self.store[self.name]["ema"][self.store[self.name]["nbema"] - 1]["period"]:            
            return None

        lema = []
        for i in range(0, self.store[self.name]["nbema"]):
            lema.append(self.store[self.name]["ema"][i]["value"][self.store[self.name]["nb_bars"] - 1])

        self.store[self.name]["data"][index]["lema"] = lema

        self.store[self.name]["data"][index]["increasing"] = is_increasing(lema)

        self.store[self.name]["data"][index]["decreasing"] = is_decreasing(lema)

        indexA = None
        # look for an index of /\ shape
        for i in range(0, self.store[self.name]["nbema"]+1):

            if is_increasing(lema[0:i]) and is_decreasing(lema[i:len(lema)]):            
                if indexA == None:
                    indexA = i
                else:
                    indexA = max(indexA, i)
                    
        self.store[self.name]["data"][index]["indexA"] = indexA


        indexV = None
        # look for an index of \/ shape
        for i in range(0, self.store[self.name]["nbema"]+1):

            if is_decreasing(lema[0:i]) and is_increasing(lema[i:len(lema)]):            
                if indexV == None:
                    indexV = i
                else:
                    indexV = max(indexV, i)

        self.store[self.name]["data"][index]["indexV"] = indexV

        if False:
            if indexA <> None:
                print "AShape (" + str(indexA) + "): " + str((lema[0:indexA], lema[indexA:len(lema)]))

            if indexV <> None:
                print "VShape (" + str(indexV) + ": " + str((lema[0:indexV], lema[indexV:len(lema)]))

            if self.store[self.name]["data"][index]["increasing"]:
                print "increasing: " + str(lema)

            if self.store[self.name]["data"][index]["decreasing"]:
                print "decreasing: " + str(lema)

            if indexA == None and indexV == None and not self.store[self.name]["data"][index]["increasing"] and not self.store[self.name]["data"][index]["decreasing"]:
                print "index == None in " + str(lema)

            print ""


       

        return None

    # draw stock price / pnl
    def draw(self):
        
        fig = plt.figure()
        ax = fig.add_subplot(111)
        l = map (lambda x: self.store[self.name]["bars"][x]["ajust. close"], range(0, self.store[self.name]["nb_bars"]))
        ax.plot(range(0, self.store[self.name]["nb_bars"]), l, label='spot')

        for i in range(0, self.store[self.name]["nbema"]):
            l = map (lambda x: self.store[self.name]["ema"][i]["value"][x], range(0, self.store[self.name]["nb_bars"]))
            ax.plot(range(0, self.store[self.name]["nb_bars"]), l, label='ema ' + str(i))
        
        props = font_manager.FontProperties(size=10)
        ax.legend(loc='best', shadow=True, fancybox=True, prop=props)

        ax2 = ax.twinx()
        l = map (lambda x: self.store[self.name]["pnl"][x][3], range(0, self.store[self.name]["nb_bars"]))
        ax2.plot(range(0, self.store[self.name]["nb_bars"]), l, label='pnl')
        return fig


if __name__ == "__main__":
    
    store = Storegraph(_globals = globals())

    if (len(sys.argv) > 1):
        tickers = sys.argv[1:]
    else:

        # all tickers of nikkei
        tickers = yahoojap.get_tickers_nikkei225()

        tickers = map(lambda x: str(x), tickers)

    tot_pnl = 0

    for ticker in tickers:

        bt = Strat2(store = store)

        print "ticker = " + str(ticker)
        
        try:
            open(ticker + ".quotes", "rb")
        except:
            print "downloading quotes ..."
            yahoojap.get_historical(ticker, date(2010,1,1), filename = ticker + ".quotes")

        bt.load(ticker + ".quotes")

        print "running backtest ..."
        bt.run()


        #bt.store.save(open(ticker + ".log", "wb"))

        pnl = bt.store[bt.name]["final pnl"]
        tot_pnl += pnl[3]
        print "pnl = " + str(pnl[0]) + ", unrealized pnl = " + str(pnl[2]) + ", total pnl = "  + str(pnl[3]) + "\n"
        

        fig = bt.draw()
        fig.savefig(ticker + ".png")
        del fig
        #fig.show()
        #raw_input()

    store.save(open("strat_nikkei225" + ".log", "wb"))
    print "total pnl = " + str(tot_pnl)
