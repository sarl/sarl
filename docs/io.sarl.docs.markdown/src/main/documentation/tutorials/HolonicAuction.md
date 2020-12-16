# English Auction with Holons

[:Outline:]

This document describes the basics of the creation and design of holons in SARL. This tutorial is applied
on a simple English auction application. Before reading this document, it is recommended reading
the [General Syntax Reference](../reference/GeneralSyntax.md).
The architecture presented in this tutorial may be used for designing a system in which a decision must be
taken after arbitration among components. 

<div class="bt-download">
<a href="[:githublnk](https://github.com/sarl/sarl/tree/master/contribs/io.sarl.examples/io.sarl.examples.plugin/projects/io-sarl-tutorials-holonicauction/src/main/sarl/io/sarl/examples/holonicauction)"><img alt="See the code" src="[:sarlUrl!]/images/download-icon.png"/></a>
</div>
According to the vocabulary used in the SARL project, all the agents are holons. In the rest of this tutorial,
the terms "agent" and "holon" are synonymous. 

The elements that are explained in this tutorial are:

* the definition of an event;
* the definition of a super-agent;
* the definition of a sub-agent;
* the emit of events from the super-agent to its sub-agent;
* the emit of events from the sub-agent to its super-agent.

> **_Note:_** The communication between the sub-agents is out of the scope of this tutorial. For interested
> readers, the [Agent Reference](../reference/Agent.md) may be read.

The source code related to this tutorial may be found in the
[Github of the SARL demos]([:githublnk!]).


## Principle of the English Auction

This type of auction is arguably the most common form of auction in use today. 
Participants bid openly against one another, with each subsequent bid required to
be higher than the previous bid. An auctioneer may announce prices, bidders
may call out their bids themselves.
The auction ends when no participant is willing to bid further, at which
point the highest bidder pays their bid.
Alternatively, if the seller has set a minimum sale price in advance
(the 'reserve' price) and the final bid does not reach that price the item
remains unsold.
Sometimes the auctioneer sets a minimum amount by which the next bid must
exceed the current highest bid. The most significant distinguishing factor of
this auction type is that the current highest bid is always available to
potential bidders.

Source: [Wikipedia](https://en.wikipedia.org/wiki/Auction)


## Principle of the Application

The principle of the application is the following:

* The application is set-up with one auctioneer agent and three bidders agents.
* The auctioneer is announcing the starting price.
* The bidders are offering their bids back.
* When a bid is received, the auctioneer is announcing the new price.
* If there is no received bid, the auctioneer is closing the auction, and the winner is the bidder with the higher bid.

__By a design choice, the bidders are sub-agents of the auctioneer agent.__

![Holonic Auction](./holonic_auction.png)


## Why are the sub-agents in the inner context?

According to the [Built-in Capacity Reference](../reference/BIC.md),
a sub-agent is living in the __inner context__ of a super-agent.
Each agent defines its own context, called the inner context, where other agents can live.
Therefore, every agent can be seen as a part of a larger
[holon](https://en.wikipedia.org/wiki/Holon_(philosophy)) _and_ at the same time be composed 
by other agents that exist in its _inner context_. 

> **_Note:_** According to the SARL specifications, all the agents in a context belong to
> the default space of this context. This property is important
> for designing the communication links between two adjacent levels
> in the hierarchy of agents. The default space of the inner context
> becomes the natural place where the super-agent and
> its sub-agents are interacting.


## Definitions of the events

In the application, two events are needed: the event from the auctioneer for notifying the
bidders of the new price; and the event that is sent by a bidder to the auctioneer with a
bid inside.


### Price event

The [:priceevent:] event is the event sent by the auctioneer for notifying a bidder that
the price has changed. This event contains the new price.

[:Success:][:On]
	package io.sarl.docs.tutorials.holonicauction
	event [:priceevent](Price) {
		val price : float
		new(price : float) {
			this.price = price
		}
	}
[:End:]


### Playing event

The [:bidevent:] event is the event sent by a bidder to the auctioneer.
This event contains the value of the bid.

[:Success:]
	package io.sarl.docs.tutorials.holonicauction
	[:On]event [:bidevent](Bid) {
		val value : float
		new(value : float) {
			this.value = value
		}
	}
[:End:]


## Definition of the bidder

The bidder agent is reacting to new price notifications, and could offer a new bid if the
new price is not  exceeding its maximal price.


### Initial definition

The initial definition of the bidder is below. The [:random:] attribute contains an instance of
a random number generator (from the Java library). The [:maxprice:] attribute is the maximum value
of the price that the bidder will consider for bidding.
The bidder selects the maximum price between 100 and 1000 randomly.

[:Success:]
	package io.sarl.docs.tutorials.holonicauction
	import java.util.Random
	import io.sarl.core.Initialize
	[:On]agent Bidder {
		val random = new Random
		var [:maxprice](maxPrice) : float
		
		on Initialize {
			maxPrice = [:random](random).nextFloat() * 900f + 100f
		}
	}
[:End:]


### Bidding

The definition of the bidder agent is extended by the bidding behavior.
The bidding must occur when the auctioneer is notifying a new price, i.e. when the
[:priceevent:] event is received.
The bidder computes the new price. If this last is not exceeding the maximal
price, then the bidder is sending its bid in a [:bidevent:] event.

> **_Interaction Principle:_**
> For sending data to its super-agent, a sub-agent must
> fire an event in the default space of the inner context
> of the super-agent. The [:emitfct:] function is supporting this interaction.

> **_Caution:_** The [:bidevent:] event is sent in the default space. But there is no
> restriction on the event's recipient. It means that the super-agent __and__ the
> other sub-agents will receive this event.

[:Success:]
	package io.sarl.docs.tutorials.holonicauction
	import java.util.Random
	import io.sarl.core.Initialize
	import io.sarl.core.DefaultContextInteractions
	import io.sarl.core.Logging
	event Price {
		val price : float
		new(price : float) {
			this.price = price
		}
	}
	event Bid {
		val value : float
		new(value : float) {
			this.value = value
		}
	}
	[:On]agent Bidder {
		val random = new Random
		var maxPrice : float
		var myLastBid : float
		
		on Initialize {
			maxPrice = random.nextFloat() * 900f + 100f
		}
		
		uses DefaultContextInteractions, Logging
	
		on Price {
			if(occurrence.price == myLastBid) {
				println("I do not bet, I am the winner with :" + myLastBid)
			} else {
				if(occurrence.price < maxPrice) {
					var priceIncrease = random.nextFloat() * 50f
					if (priceIncrease > 0) {
						var newPrice = occurrence.price + priceIncrease
						if (newPrice <= maxPrice) {
							[:emitfct](emit)(new Bid(newPrice))
							myLastBid = newPrice
						} else {
							println(" I give up, this is beyond my resources : " + myLastBid)
						}
					}
				} else {
					println("I dropped to " + myLastBid)
				}
			}
		}
	}
[:End:]


### Restrict the bid to the auctioneer

For restricting the recipients of the [:bidevent:] event to the auctioneer, it is mandatory to specify a
scope for the event.
For supporting the holonic communication from the sub-agent to the super-agent, the scope
of the event corresponds to the address of the super-agent in the default space.

> **_Note:_** The ID of the super-agent, and the ID of the inner context of this super-agent are always the same.

Below, we update the bidding behavior by creating a scope, and providing it to the [:emitfct:] function.

[:Success:]
	package io.sarl.docs.tutorials.holonicauction
	import java.util.Random
	import io.sarl.core.Initialize
	import io.sarl.core.DefaultContextInteractions
	import io.sarl.core.Logging
	event Price {
		val price : float
		new(price : float) {
			this.price = price
		}
	}
	event Bid {
		val value : float
		new(value : float) {
			this.value = value
		}
	}
	[:On]agent Bidder {
		val random = new Random
		var maxPrice : float
		var myLastBid : float
		
		on Initialize {
			maxPrice = random.nextFloat() * 900f + 100f
		}
	
		uses DefaultContextInteractions, Logging
	
		on Price {
			if(occurrence.price == myLastBid) {
				println("I do not bet, I am the winner with :" + myLastBid)
			} else {
				if(occurrence.price < maxPrice) {
					var priceIncrease = random.nextFloat() * 50f
					if (priceIncrease > 0) {
						var newPrice = occurrence.price + priceIncrease
						if (newPrice <= maxPrice) {
							emit(new Bid(newPrice)) [ it.ID == defaultContext.ID]
							myLastBid = newPrice
						} else {
							println(" I give up, this is beyond my resources : " + myLastBid)
						}
					}
				} else {
					println("I dropped to " + myLastBid)
				}
			}
		}
	}
[:End:]


### Definition of the auctioneer

The third step of this tutorial is the definition of the auctioneer that is initiating
the auction, waiting for bids, and selecting the winner.


#### Initial definition

The initial definition of the auctioneer is defined below. The auctioneer is starting the
auction with a price of 50. It is notifying the bidders with a [:priceevent:] event.
Because the bidders are sub-agents, they are living in the inner context of the auctioneer.
For sending the [:priceevent:] event to the bidders, the auctioneer must put it in the
default space of its inner context.
This particular type of emit is supported by the [:wakefct:] function, which is provided by
the [:behcap:] capacity. This function does the same as:

innerContext.defaultSpace.emit(new Price(50))

> **_Interaction Principle:_** 
> For sending data to its sub-agents, a super-agent must fire an event in the default space
> of its inner context. The [:wakefct:] function is supporting this interaction.

[:Success:]
	package io.sarl.docs.tutorials.holonicauction
	import java.util.Random
	import io.sarl.core.Behaviors
	import io.sarl.core.Initialize
	import io.sarl.lang.core.Address
	event Price {
		val price : float
		new(price : float) {
			this.price = price
		}
	}
	[:On]agent Auctioneer {
						
		uses [:behcap](Behaviors)
		
		var maxBid = 0f
		var winner : Address
		var hasBid = false 
		var isAuctionOpened = true
		
		on Initialize {
			[:wakefct](wake)(new Price(50))
		}
	}
[:End:]


#### Create the bidders

The creation of the sub-agents in the auctioneer needs to spawn agents in the inner context.
The [:lifecyclecap:] capacity gives the [:sicfct:] function.
This function permits creating an agent in a particular context.
For obtaining the inner context, we need to use the [:innercap:] capacity,
which provides the [:getinner:s] function. Below, we create the three bidders. 

[:Success:]
	package io.sarl.docs.tutorials.holonicauction
	import java.util.Random
	import io.sarl.lang.core.Address
	import io.sarl.core.Behaviors
	import io.sarl.core.InnerContextAccess
	import io.sarl.core.Lifecycle
	import io.sarl.core.Initialize
	event Price {
		val price : float
		new(price : float) {
			this.price = price
		}
	}
	agent Bidder {}
	[:On]agent Auctioneer {
						
		uses Behaviors, [:lifecyclecap](Lifecycle), [:innercap](InnerContextAccess)
		
		var maxBid = 0f
		var winner : Address
		var hasBid = false 
		var isAuctionOpened = true
		
		on Initialize {
			for(i : 1..3) {
				[:sicfct](spawnInContext)(typeof(Bidder), [:getinner](getInnerContext))
			}
			
			wake(new Price(50))
		}
	}
[:End:]


#### Receive the bids

The auctioneer is waiting for bids. This behavior is coded inside the behavior
unit dedicated to the [:bidevent:] event. We add a guard on the [:isopened:] attribute
to execute the behavior only if the auction is still opened. We will see later when
the auction is closed. If the value of the received bid is greater than the current
price, the source of the [:bidevent:] event becomes the new potential winner. 

[:Success:]
	package io.sarl.docs.tutorials.holonicauction
	import java.util.Random
	import io.sarl.lang.core.Address
	import io.sarl.core.Behaviors
	import io.sarl.core.InnerContextAccess
	import io.sarl.core.Lifecycle
	import io.sarl.core.Initialize
	event Price {
		val price : float
		new(price : float) {
			this.price = price
		}
	}
	event Bid {
		val value : float
		new(value : float) {
			this.value = value
		}
	}
	agent Bidder {}
	[:On]agent Auctioneer {
						
		uses Behaviors, Lifecycle, InnerContextAccess
		
		var maxBid = 0f
		var winner : Address
		var hasBid = false 
		var [:isopened](isAuctionOpened) = true
		
		on Initialize {
			for(i : 1..3) {
				spawnInContext(Bidder, innerContext)
			}
			
			wake(new Price(50))
		}
		
		on Bid [ isAuctionOpened ] {
			if (occurrence.value > maxBid) {
				maxBid = occurrence.value
				winner = occurrence.source
			}
		}
	}
[:End:]


#### Stop the auction

The auctioneer must wait some time before it is closing the auction due to lake of bid.
To reproduce this behavior, we introduce a periodic task, which is executed every 10
seconds for checking if a bid was provided during the last 10 seconds. This periodic task
is started after the first 10 seconds.

Coding the periodic task in SARL is done with the [:schedcap:] capacity.
It provides the [:everyfct:] function that is executing at a fixed delay its second argument,
given by the first argument.
In the task's code, we test if a bid was received. If not, the auctioneer closes the auction,
and outputs the appropriate message. To delay the task executor about the first ten seconds,
we use the [:infct:] function provided by the capacity.

[:Success:]
	package io.sarl.docs.tutorials.holonicauction
	import java.util.Random
	import io.sarl.lang.core.Address
	import io.sarl.core.Behaviors
	import io.sarl.core.InnerContextAccess
	import io.sarl.core.Lifecycle
	import io.sarl.core.Schedules
	import io.sarl.core.Logging
	import io.sarl.core.Initialize
	event Price {
		val price : float
		new(price : float) {
			this.price = price
		}
	}
	event Bid {
		val value : float
		new(value : float) {
			this.value = value
		}
	}
	agent Bidder {}
	[:On]agent Auctioneer {
						
		uses Behaviors, Lifecycle, InnerContextAccess, [:schedcap](Schedules), Logging
		
		var maxBid = 0f
		var winner : Address
		var hasBid = false 
		var isAuctionOpened = true
		
		on Initialize {
			for(i : 1..3) {
				spawnInContext(Bidder, innerContext)
			}
			
			wake(new Price(50))

			[:infct](in)(10000) [
				val waitTask = task("wait-task")
				waitTask.[:everyfct](every)(10000) [
					if (!hasBid) {
						isAuctionOpened = false
						if (winner === null) {
							println("No winner")
						} else {
							println("The winner is " + winner
								+ " with the bid of " + maxBid)
						}
					}
					hasBid = false
				]
			]
		}
		
		on Bid [ isAuctionOpened ] {
			hasBid = true
			if (occurrence.value > maxBid) {
				maxBid = occurrence.value
				winner = occurrence.source
			}
		}
	}
[:End:]


#### Synchronize the operations

Because the periodic task and the event handlers may be executed in parallel, we are facing
a classical problem in concurrent programming: how to ensure that two
blocks of code are not executed at the same time for avoiding any conflicting access
on the same data.

It is recommended to "synchronize" the blocks of code. We use the [:sync:] operator
(which has the same meaning as in the Java language). This operator ensures
that two blocks of code, which are synchronized on the
same Object (the argument of the operator) cannot be
executed in parallel by different threads.

[:Success:]
	package io.sarl.docs.tutorials.holonicauction
	import java.util.Random
	import io.sarl.lang.core.Address
	import io.sarl.core.Behaviors
	import io.sarl.core.InnerContextAccess
	import io.sarl.core.Lifecycle
	import io.sarl.core.Schedules
	import io.sarl.core.Logging
	import io.sarl.core.Initialize
	event Price {
		val price : float
		new(price : float) {
			this.price = price
		}
	}
	event Bid {
		val value : float
		new(value : float) {
			this.value = value
		}
	}
	agent Bidder {}
	[:On]agent Auctioneer {
						
		uses Behaviors, Lifecycle, InnerContextAccess, Schedules, Logging
		
		var maxBid = 0f
		var winner : Address
		var hasBid = false 
		var isAuctionOpened = true
		
		on Initialize {
			for(i : 1..3) {
				spawnInContext(Bidder, innerContext)
			}
			
			wake(new Price(50))

			in(10000) [
				val waitTask = task("wait-task")
				waitTask.every(10000) [
					synchronized(this) {
						if (!hasBid) {
							isAuctionOpened = false
							if (winner === null) {
								println("No winner")
							} else {
								println("The winner is " + winner
									+ " with the bid of " + maxBid)
							}
						}
						hasBid = false
					}
				]
			]
		}
		
		on Bid [ isAuctionOpened ] {
			[:sync](synchronized)(this) {
				hasBid = true
				if (occurrence.value > maxBid) {
					maxBid = occurrence.value
					winner = occurrence.source
				}
			}
		}
	}
[:End:]


### Stop the agents

The previous code works well at one exception. When the auction is closed, the system does not stop.
Indeed, the auctioneer does not send any more the [:priceevent:] event. This
cause all the agents waiting something that will never append.

__We need to stop the agents.__

> **_Important Note:_** In the specification of SARL, a super-agent cannot be killed
> if there is some other agent belonging to its inner context.
> Consequently, for stopping the agents, we need to stop the
> sub-agents before the super-agent.


#### StopAuction event

Because the determination of the end of the agent's life is made by the auctioneer,
this last must notify its sub-agents that it is time to commit a suicide.
We introduce the [:stopauctionevent:] event that is used for this particular notification task.

[:Success:]
	package io.sarl.docs.tutorials.holonicauction
	[:On]event [:stopauctionevent](StopAuction)
[:End:]


#### Kill the bidder

The code of the bidder must be updated for reacting on the receiving of the [:stopauctionevent:]
event.
When it is received, the bidder agent is killing itself by calling the [:killme:] function.
This function is provided by the [:lifecyclecap:] capacity.

[:Success:]
	package io.sarl.docs.tutorials.holonicauction
	import java.util.Random
	import io.sarl.core.Initialize
	import io.sarl.core.DefaultContextInteractions
	import io.sarl.core.Lifecycle
	event Price {
		val price : float
		new(price : float) {
			this.price = price
		}
	}
	event Bid {
		val value : float
		new(value : float) {
			this.value = value
		}
	}
	event StopAuction
	[:On]agent Bidder {
		val random = new Random()
		var maxPrice : float
		
		on Initialize {
			maxPrice = random.nextFloat() * 900f + 100f
		}
	
		uses DefaultContextInteractions
	
		on Price {
			var priceIncrease = random.nextFloat() * 50f
			if (priceIncrease > 0) {
				var newPrice = occurrence.price + priceIncrease
				if (newPrice <= maxPrice) {
					emit(new Bid(newPrice)) [ it.ID == defaultContext.ID]
				}
			}
		}
		
		uses Lifecycle
		
		on StopAuction {
			[:killme](killMe)
		}
	}
[:End:]


#### Kill the auctioneer

The code of the auctioneer must be updated for firing the [:stopauctionevent:] event, and for killing
itself when there is no more sub-agent.
Firstly, the periodic task is updated with a [:wakefct:] call
that permits notifying the sub-agents of the end of the auction.
Secondly, in this periodic task, if the auction is closed, then the auctioneer is killing itself if the
[:hmafct:] function replies false.
This function is provided by the [:innercap:] capacity.
The periodic task must also be stopped. The [:cancelfct:] function is invoked on the periodic task
to stop its execution.

[:Success:]
	package io.sarl.docs.tutorials.holonicauction
	import java.util.Random
	import io.sarl.lang.core.Address
	import io.sarl.core.Behaviors
	import io.sarl.core.InnerContextAccess
	import io.sarl.core.Lifecycle
	import io.sarl.core.Schedules
	import io.sarl.core.Logging
	import io.sarl.core.Initialize
	event Price {
		val price : float
		new(price : float) {
			this.price = price
		}
	}
	event Bid {
		val value : float
		new(value : float) {
			this.value = value
		}
	}
	event StopAuction
	agent Bidder {}
	[:On]agent Auctioneer {
						
		uses Behaviors, Lifecycle, InnerContextAccess, Schedules, Logging
		
		var maxBid = 0f
		var winner : Address
		var hasBid = false 
		var isAuctionOpened = true
		
		on Initialize {
			for(i : 1..3) {
				spawnInContext(Bidder, innerContext)
			}
			
			wake(new Price(50))

			in(10000) [
				val waitTask = task("wait-task")
				waitTask.every(10000) [
					synchronized(this) {
						if (!isAuctionOpened) {
							if (![:hmafct](hasMemberAgent)) {
								waitTask.[:cancelfct](cancel)
								killMe
							}
						} else {
							if (!hasBid) {
								isAuctionOpened = false
								if (winner === null) {
									println("No winner")
								} else {
									println("The winner is " + winner
										+ " with the bid of " + maxBid)
								}
								wake(new StopAuction)
							}
							hasBid = false
						}
					}
				]
			]
		}
		
		on Bid [ isAuctionOpened ] {
			synchronized(this) {
				hasBid = true
				if (occurrence.value > maxBid) {
					maxBid = occurrence.value
					winner = occurrence.source
				}
			}
		}
	}
[:End:]


[:Include:](../legal.inc)
