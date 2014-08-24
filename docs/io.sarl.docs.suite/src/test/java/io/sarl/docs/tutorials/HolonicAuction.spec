/*
 * $Id$
 *
 * SARL is an general-purpose agent programming language.
 * More details on http://www.sarl.io
 *
 * Copyright (C) 2014 Sebastian RODRIGUEZ, Nicolas GAUD, Stéphane GALLAND.
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *      http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */
package io.sarl.docs.tutorials

import com.google.inject.Inject
import io.sarl.docs.utils.SARLParser
import io.sarl.docs.utils.SARLSpecCreator
import org.jnario.runner.CreateWith

import static extension io.sarl.docs.utils.SpecificationTools.*

/* <!-- OUTPUT OUTLINE -->
 *
 * This document describes the basics of the creation
 * and design of holons in SARL. This tutorial is applied
 * on a simple English auction application.
 * Before reading this document, it is recommended reading
 * the [General Syntax Reference](../reference/GeneralSyntaxReferenceSpec.html).
 * The architecture presented in this tutorial may be
 * used for designing a system in which a decision must be
 * taken after arbitration among components. 
 * 
 * <div class="bt-download">
 * <a href="%sarlmavenrepository%/last-demos-release.jar"><img alt="Download the Binary JAR file" src="%website%/images/download-icon.png"/></a>
 * </div>
 * According to the vocabulary used in the SARL project,
 * all the agents are holons. In the rest of this tutorial,
 * the terms "agent" and "holon" are synonymous. 
 * 
 * The elements that are explained in this tutorial are:
 * 
 *  * the definition of an event;
 *  * the definition of a super-agent;
 *  * the definition of a sub-agent;
 *  * the emit of events from the super-agent to its sub-agent;
 *  * the emit of events from the sub-agent to its super-agent.
 *
 * <span class="label label-info">Note</span> The communication between
 * the sub-agents is out of the scope of this tutorial. For interested
 * readers, the [Agent Reference](../reference/AgentReferenceSpec.html)
 * may be read.
 * 
 * The source code related to this tutorial may be found
 * in the [SARL demos](https://github.com/sarl/sarl-demos/tree/master/src/main/sarl/io/sarl/docs/tutorials/holonicauction).
 */
@CreateWith(SARLSpecCreator)
describe "English Auction with Holons"{

		@Inject extension SARLParser

		/*
		 * This type of auction is arguably the most common form of auction in use today.
		 * Participants bid openly against one another, with each subsequent bid required to 
		 * be higher than the previous bid. An auctioneer may announce prices, bidders
		 * may call out their bids themselves.
		 * The auction ends when no participant is willing to bid further, at which
		 * point the highest bidder pays their bid.
		 * Alternatively, if the seller has set a minimum sale price in advance 
		 * (the 'reserve' price) and the final bid does not reach that price the item 
		 * remains unsold.
		 * Sometimes the auctioneer sets a minimum amount by which the next bid must
		 * exceed the current highest bid. The most significant distinguishing factor of
		 * this auction type is that the current highest bid is always available to 
		 * potential bidders.
		 * 
		 * 
		 * Source: [Wikipedia](http://en.wikipedia.org/wiki/Auction)
		 * 
		 * @filter(.*)
		 */
		fact "Principle of the English Auction" {
			// Test the URLs in the introduction of this page.
			"../reference/GeneralSyntaxReferenceSpec.html".mustBeJnarioLink
			"../reference/AgentReferenceSpec.html".mustBeJnarioLink
			"%sarlmavenrepository%".mustBeHttpLink
			"%website%".mustBeHttpLink
		}

		/*
		 * The principle of the application is the following:
		 * 
		 *  * The application is set-up with one auctioneer agent and
		 *    three bidders agents.
		 *  * The auctioneer is announcing the starting price.
		 *  * The bidders are offering their bids back.
		 *  * When a bid is received, the auctioneer is announcing
		 *    the new price.
		 *  * If there is no received bid, the auctioneer is closing
		 *    the auction, and the winner is the bidder with the higher
		 *    bid.
		 *
		 * __By a design choice, the bidders are sub-agents of
		 * the auctioneer agent.__
		 * 
		 * <center><img src="./holonic_auction.png"/></center>
		 * 
		 */
		fact "Principle of the Application" {
			"./holonic_auction.png".mustBePicture
		}

		/*
		 * According to the
		 * [Built-in Capacity Reference](../reference/BuiltInCapacityReferenceSpec.html),
		 * a sub-agent is living in the __inner context__ of
		 * a super-agent.
		 * Each agent defines its own context, called the inner context,
		 * where other agents can live. Therefore, every agent can be seen 
		 * as a part of a larger
		 * <a href="http://en.wikipedia.org/wiki/Holon_(philosophy)">holon</a>,
		 * __and__ at the same time be composed 
		 * by other agents that exist in its _inner context_. 
		 * 
		 * <span class="label label-info">Note</span> According to the
		 * SARL specifications, all the agents in a context belong to
		 * the default space of this context. This property is important
		 * for designing the communication links between two adjacent levels
		 * in the hierarchy of agents. The default space of the inner context
		 * becomes the natural place where the super-agent and
		 * its sub-agents are interacting.
		 * 
		 * @filter(.*)
		 */
		fact "Why are the sub-agents in the inner context?" {
			"../reference/BuiltInCapacityReferenceSpec.html".mustBeJnarioLink
		}

		/* In the application, two events are needed: the event from
		 * the auctioneer for notifying the bidders of the new price; and
		 * the event that is sent by a bidder to the auctioneer with a 
		 * bid inside.
		 */
		describe "Definitions of the events" {

			/* The `Price` event is the event sent
			 * by the auctioneer for notifying a
			 * bidder that the price has changed.
			 * This event contains the new price.
			 * 
			 * @filter(.* = '''|'''|.parsesSuccessfully.*)
			 */			
			fact "Price event" {
				'''
				package io.sarl.docs.tutorials.holonicauction
				event Price {
					val price : float
					new(price : float) {
						this.price = price
					}
				}
				'''.parsesSuccessfully
			}

			/* The `Bid` event is the event sent
			 * by a bidder to the auctioneer.
			 * This event contains the value of the bid.
			 * 
			 * @filter(.* = '''|'''|.parsesSuccessfully.*)
			 */			
			fact "Playing event" {
				'''
				event Bid {
					val value : float
					new(value : float) {
						this.value = value
					}
				}
				'''.parsesSuccessfully(
					"package io.sarl.docs.tutorials.holonicauction",
					// TEXT
					""
				)
			}

		}
		
		/* The bidder agent is reacting to new price notifications,
		 * and could offer a new bid if the new price is not 
		 * exceeding its maximal price.
		 */
		describe "Definition of the bidder" {

			/* The initial definition of the bidder is below.
			 * The `random` attribute contains an instance of
			 * a random number generator (from the Java library).
			 * The `maxPrice` attribute is the maximum value
			 * of the price that the bidder will consider for
			 * bidding.
			 * The bidder selects the maximum price
			 * between 100 and 1000 randomly.
			 * 
			 * @filter(.* = '''|'''|.parsesSuccessfully.*)
			 */
			fact "Initial definition" {
				'''
				agent Bidder {
					val random = new Random()
					var maxPrice : float
					
					on Initialize {
						maxPrice = random.nextFloat() * 900f + 100f
					}
				}
				'''.parsesSuccessfully(
					"package io.sarl.docs.tutorials.basicholon
					import java.util.Random
					import io.sarl.core.Initialize",
					// TEXT
					""
				)
			}

			/* The definition of the bidder agent is extended
			 * by the bidding behavior.
			 * The bidding must occur when the auctioneer is
			 * notifying a new price, i.e. when the `Price` event
			 * is received.
			 * The bidder computes the new price. If this last is not
			 * exceeding the maximal
			 * price, then the bidder is sending its bid in a `Bid` event.
			 * 
			 * <span class="label label-warning">Interaction Principle</span>
			 * For sending data to its super-agent, a sub-agent must
			 * fire an event in the default space of the inner context
			 * of the super-agent.
			 * The `emit` function is supporting this interaction.
			 * 
			 * 
			 * <span class="label label-warning">Caution</span>
			 * The `Bid` event is sent in the default space.
			 * But there is no restriction on the event's recipient.
			 * It means that the super-agent __and__ the
			 * other sub-agents will receive this event. 
			 * 
			 * @filter(.* = '''|'''|.parsesSuccessfully.*)
			 */
			fact "Bidding" {
				'''
				agent Bidder {
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
								emit(new Bid(newPrice))
							}
						}
					}
				}
				'''.parsesSuccessfully(
					"package io.sarl.docs.tutorials.basicholon
					import java.util.Random
					import io.sarl.core.Initialize
					import io.sarl.core.DefaultContextInteractions
					import io.sarl.util.Scopes
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
					}",
					// TEXT
					""
				)
			}

			/* For restricting the recipients of the `Bid` event
			 * to the auctioneer, it is mandatory to specify a
			 * scope for the event.
			 * For supporting the holonic communication from
			 * the sub-agent to the super-agent, the scope
			 * of the event corresponds to the address of the
			 * super-agent in the default space.
			 * 
			 * <span class="label label-info">Note</span>
			 * The ID of the super-agent, and the ID of the inner
			 * context of this super-agent are always the same. 
			 * 
			 * Below, we update the bidding behavior by creating
			 * a scope, and providing it to the `emit` function.
			 * 
			 * @filter(.* = '''|'''|.parsesSuccessfully.*)
			 */
			fact "Restrict the bid to the auctioneer" {
				'''
				agent Bidder {
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
								var superScope = Scopes.addresses(
									defaultSpace.getAddress(defaultContext.ID))
								emit(new Bid(newPrice), superScope)
							}
						}
					}
				}
				'''.parsesSuccessfully(
					"package io.sarl.docs.tutorials.basicholon
					import java.util.Random
					import io.sarl.core.Initialize
					import io.sarl.core.DefaultContextInteractions
					import io.sarl.util.Scopes
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
					}",
					// TEXT
					""
				)
			}

		}

		/* The third step of this tutorial is the definition of the
		 * auctioneer that is initiating the auction, waiting for
		 * bids, and selecting the winner.
		 */
		context "Definition of the auctioneer" {

			/* The initial definition of the auctioneer is defined below.
			 * The auctioneer is starting the auction with a price of 50.
			 * It is notifying the bidders with a `Price` event.
			 * Because the bidders are sub-agents, they are living
			 * in the inner context of the auctioneer.
			 * For sending the `Price` event to the bidders, the
			 * auctioneer must put it in the default space of
			 * its inner context.
			 * This particular type of emit is supported by
			 * the `wake` function, which is provided by
			 * the `Behaviors` capacity. This function does
			 * the same as:
			 * ```
			 * innerContext.defaultSpace.emit(new Price(50))
			 * ```
			 * 
			 * <span class="label label-warning">Interaction Principle</span>
			 * For sending data to its sub-agents, a super-agent must
			 * fire an event in the default space of its inner context.
			 * The `wake` function is supporting this interaction.
			 * 
			 * @filter(.* = '''|'''|.parsesSuccessfully.*)
			 */
			fact "Initial definition" {
				'''
				agent Auctioneer {
									
					uses Behaviors
					
					var maxBid = 0f
					var winner : Address
					var hasBid = false 
					var isAuctionOpened = true
					
					on Initialize {
						wake(new Price(50))
					}
				}
				'''.parsesSuccessfully(
					"package io.sarl.docs.tutorials.holonicauction
					import java.util.Random
					import io.sarl.core.Behaviors
					import io.sarl.core.Initialize
					import io.sarl.lang.core.Address
					event Price {
						val price : float
						new(price : float) {
							this.price = price
						}
					}",
					// TEXT
					""
				)
			}

			/* The creation of the sub-agents in the auctioneer
			 * needs to spawn agents in the inner context.
			 * The `Lifecycle` capacity gives the
			 * `spawnInContext` function.
			 * This function permits creating an agent in
			 * a particular context.
			 * For obtaining the inner context,
			 * we need to use the `InnerContextAccess` capacity,
			 * which provides the `getInnerContext` function.
			 * Below, we create the three bidders. 
			 * 
			 * @filter(.* = '''|'''|.parsesSuccessfully.*)
			 */
			fact "Create the bidders" {
				'''
				agent Auctioneer {
									
					uses Behaviors, Lifecycle, InnerContextAccess
					
					var maxBid = 0f
					var winner : Address
					var hasBid = false 
					var isAuctionOpened = true
					
					on Initialize {
						for(i : 1..3) {
							spawnInContext(typeof(Bidder), innerContext)
						}
						
						wake(new Price(50))
					}
				}
				'''.parsesSuccessfully(
					"package io.sarl.docs.tutorials.holonicauction
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
					agent Bidder {}",
					// TEXT
					""
				)
			}

			/* The auctioneer is waiting for bids.
			 * This behavior is coded inside the behavior
			 * unit dedicated to the `Bid` event.
			 * We add a guard on the `isAuctionOpened` attribute
			 * to execute the behavior only if the auction is still
			 * opened. We will see later when the auction is closed.
			 * If the value of the received bid is greater than
			 * the current price, the source of the `Bid` event
			 * becomes the new potential winner. 
			 * 
			 * @filter(.* = '''|'''|.parsesSuccessfully.*)
			 */
			fact "Receive the bids" {
				'''
				agent Auctioneer {
									
					uses Behaviors, Lifecycle, InnerContextAccess
					
					var maxBid = 0f
					var winner : Address
					var hasBid = false 
					var isAuctionOpened = true
					
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
				'''.parsesSuccessfully(
					"package io.sarl.docs.tutorials.holonicauction
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
					agent Bidder {}",
					// TEXT
					""
				)
			}

			/* The auctioneer must wait some time before it
			 * is closing the auction due to lake of bid.
			 * To reproduce this behavior, we introduce a
			 * periodic task, which is executed every 10
			 * seconds for checking if a bid was provided
			 * during the last 10 seconds. This periodic task
			 * is started after the first 10 seconds.
			 * 
			 * Coding the periodic task in SARL is done with
			 * the `Schedules` capacity.
			 * It provides the `every` function that is
			 * executing at a fixed delay its second argument,
			 * given by the first argument.
			 * In the task's code, we test if a bid was received.
			 * If not, the auctioneer closes the auction,
			 * and outputs the appropriate message.
			 * To delay the task executor about the first ten seconds,
			 * we use the `in` function provided by the capacity.
			 * 
			 * @filter(.* = '''|'''|.parsesSuccessfully.*)
			 */
			fact "Stop the auction" {
				'''
				agent Auctioneer {
									
					uses Behaviors, Lifecycle, InnerContextAccess, Schedules
					
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
				'''.parsesSuccessfully(
					"package io.sarl.docs.tutorials.holonicauction
					import java.util.Random
					import io.sarl.lang.core.Address
					import io.sarl.core.Behaviors
					import io.sarl.core.InnerContextAccess
					import io.sarl.core.Lifecycle
					import io.sarl.core.Schedules
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
					agent Bidder {}",
					// TEXT
					""
				)
			}

			/* Because the periodic task and the event handlers may be
			 * executed in parallel, we are facing a classical problem
			 * in concurrent programming: how to ensure that two
			 * blocks of code are not executed at the same time for
			 * avoiding any conflicting access on the same data.
			 * 
			 * It is recommended to "synchronize" the blocks of code.
			 * We use the `synchronize` operator (which has the same
			 * meaning as in the Java language). This operator ensures
			 * that two blocks of code, which are synchronized on the
			 * same Object (the argument of the operator) cannot be
			 * executed in parallel by different threads.
			 * 
			 * @filter(.* = '''|'''|.parsesSuccessfully.*)
			 */
			fact "Synchronize the operations" {
				'''
				agent Auctioneer {
									
					uses Behaviors, Lifecycle, InnerContextAccess, Schedules
					
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
						synchronized(this) {
							hasBid = true
							if (occurrence.value > maxBid) {
								maxBid = occurrence.value
								winner = occurrence.source
							}
						}
					}
				}
				'''.parsesSuccessfully(
					"package io.sarl.docs.tutorials.holonicauction
					import java.util.Random
					import io.sarl.lang.core.Address
					import io.sarl.core.Behaviors
					import io.sarl.core.InnerContextAccess
					import io.sarl.core.Lifecycle
					import io.sarl.core.Schedules
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
					agent Bidder {}",
					// TEXT
					""
				)
			}

		}

		/* The previous code works well at one exception.
		 * When the auction is closed, the system does not stop.
		 * Indeed, the auctioneer
		 * does not send any more the `Price` event. This
		 * cause all the agents waiting something that will
		 * never append.
		 * 
		 * __We need to stop the agents.__
		 * 
		 * <span class="label label-warning">Important</span> In the
		 * specification of SARL, a super-agent cannot be killed
		 * if there is some other agent belonging to its inner context.
		 * Consequently, for stopping the agents, we need to stop the
		 * sub-agents before the super-agent. 
		 */
		describe "Stop the agents" {
			
			/* Because the determination of the end of
			 * the agent's life is made by the auctioneer,
			 * this last must notify its sub-agents that
			 * it is time to commit a suicide.
			 * We introduce the `StopAuction` event that
			 * is used for this particular notification task.
			 *  
			 * @filter(.* = '''|'''|.parsesSuccessfully.*)
			 */			
			fact "StopAuction event" {
				'''
				event StopAuction
				'''.parsesSuccessfully(
					"package io.sarl.docs.tutorials.holonicauction",
					// TEXT
					""
				)
			}
			
			/* The code of the bidder must be updated for
			 * reacting on the receiving of the `StopAuction`
			 * event.
			 * When it is received, the bidder agent
			 * is killing itself by calling the `killMe` function.
			 * This function is provided by the `Lifecycle`
			 * capacity.
			 *  
			 * @filter(.* = '''|'''|.parsesSuccessfully.*)
			 */			
			fact "Kill the bidder" {
				'''
				agent Bidder {
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
								var superScope = Scopes.addresses(
									defaultSpace.getAddress(defaultContext.ID))
								emit(new Bid(newPrice), superScope)
							}
						}
					}
					
					uses Lifecycle
					
					on StopAuction {
						killMe
					}
				}
				'''.parsesSuccessfully(
					"package io.sarl.docs.tutorials.basicholon
					import java.util.Random
					import io.sarl.core.Initialize
					import io.sarl.core.DefaultContextInteractions
					import io.sarl.core.Lifecycle
					import io.sarl.util.Scopes
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
					event StopAuction",
					// TEXT
					""
				)
			}

			/* The code of the auctioneer must be updated for
			 * firing the `StopAuction` event, and for killing
			 * itself when there is no more sub-agent.
			 * Firstly, the periodic task is updated with a `wake` call
			 * that permits notifying the sub-agents of the end
			 * of the auction.
			 * Secondly, in this periodic task, if the auction is closed,
			 * then the auctioneer is killing itself if the
			 * `hasMemberAgent` function replies false.
			 * This function is provided by the `InnerContextAccess`
			 * capacity.
			 * The periodic task must also be stopped. The
			 * `cancel` function is invoked on the periodic task
			 * to stop its execution.
			 *  
			 * @filter(.* = '''|'''|.parsesSuccessfully.*)
			 */			
			fact "Kill the auctioneer" {
				'''
				agent Auctioneer {
									
					uses Behaviors, Lifecycle, InnerContextAccess, Schedules
					
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
										if (!hasMemberAgent) {
											killMe
											waitTask.cancel
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
				'''.parsesSuccessfully(
					"package io.sarl.docs.tutorials.holonicauction
					import java.util.Random
					import io.sarl.lang.core.Address
					import io.sarl.core.Behaviors
					import io.sarl.core.InnerContextAccess
					import io.sarl.core.Lifecycle
					import io.sarl.core.Schedules
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
					agent Bidder {}",
					// TEXT
					""
				)
			}

		}

		/* The last step of this tutorial is the definition
		 * of the launching process.
		 * In the rest of this section, we discuss the use
		 * of the [Janus runtime environment](http://www.janusproject.io)
		 * for running the agents.
		 *
		 *
		 * The Janus platform is designed to launch a single agent at start-up.
		 * Then, this launched agent must spawn the other agents in the system.
		 * This is typically the case in the auction application.
		 * 
		 * <span class="label label-warning">Important</span> In this section,
		 * we explain how to launch the agents from the command line interface.
		 * For launching the agents from the Eclipse IDE, please read
		 * ["Run SARL Agent in the Eclipse IDE"](../gettingstarted/RunSARLAgentInTheEclipseIDESpec.html).
		 */
		describe "Compile and Launch the agents" {
			
			/* You must have a file that contains
			 * the compiled files of the tutorial, the Janus platform,
			 * and all the needed libraries by SARL and Janus.
			 *
			 * You could directly download this file by clicking on
			 * the download icon at the top of this page; or by compiling
			 * the source code yourself.
			 *  
			 * If you download the source code of the
			 * [SARL demos](https://github.com/sarl/sarl-demos/), and
			 * compile them with [Maven](http://maven.apache.org),
			 * you will obtain a JAR file with all the mandatory elements
			 * inside. This file is located in the `target` folder,
			 * and it has a name similar to
			 * `sarl-demos-0.1.0-with-dependencies.jar`.
			 * 
			 * @filter(.*)
			 */
			fact "Compile the code" {
				// Test the URL in the introduction of this section
				"../gettingstarted/RunSARLAgentInTheEclipseIDESpec.html".mustBeJnarioLink
			}

			/* Here, there is two assumptions:<ol>
			 * <li>The file `sarl-demos-0.1.0-with-dependencies.jar`
			 *     is execuable, i.e. it can be directly launched by the Java 
			 *     Virtual Machine.</li>
			 * <li>From this file, the JVM is launching the Janus bootstrap automatically, i.e.
			 *     it has a Main-Class set to `io.janusproject.Boot`.</li>
			 * </ol>
			 * On the command line, you must launch Janus with:
			 * 
			 *     java -jar sarl-demos-0.1.0-with-dependencies.jar
			 *          io.sarl.docs.tutorials.holonicauction.Auctioneer
			 *  
			 * The file `sarl-demos-0.1.0-with-dependencies.jar` is explained above.
			 * The third argument is the qualified name of the agent to launch.
			 *  
			 * @filter(.*)
			 */
			fact "Execute with a runnable JAR" {
				true
			}
			
			/* In opposite to the previous section, we assume that
			 * the file `sarl-demos-0.1.0-with-dependencies.jar`
			 * is not executable.
			 * On the command line, you must launch Janus with:
			 * 
			 *     java -cp sarl-demos-0.1.0-with-dependencies.jar
			 *          io.janusproject.Boot
			 *          io.sarl.docs.tutorials.holonicauction.Auctioneer
			 *  
			 * The file `sarl-demos-0.1.0-with-dependencies.jar` is explained above.
			 * The string `io.janusproject.Boot` specifies the Java class to launch: the Janus bootstrap.
			 * The first argument after the bootstrap is the qualified name of the 
			 * agent to launch.
			 *  
			 * @filter(.*)
			 */
			fact "Execute without a runnable JAR" {
				true
			}

		}

}
