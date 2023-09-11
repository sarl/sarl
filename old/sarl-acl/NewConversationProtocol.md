# Creating a communication protocol for agents

Agent Communication Language (ACL), proposed by the [Foundation for Intelligent Physical Agents](http://www.fipa.org/) (FIPA), is a
proposed standard language for agent communications.

The most popular ACLs are:

* FIPA-ACL (by the FIPA, a standardization consortium)
* [Knowledge Query and Manipulation Language](https://fr.wikipedia.org/wiki/Knowledge_Query_and_Manipulation_Language) (KQML)


Both rely on [speech act theory](https://en.wikipedia.org/wiki/Speech_act). A set of performatives is defined, also called
Communicative Acts, and their meaning (e.g. `ask-one`). The content of the performative is not standardized, but varies
from system to system.

To make agents understand each other they have to not only speak the same language, but also have a common ontology. An ontology
is a part of the agent's knowledge base that describes what kind of things an agent can deal with and how they are related to each other.

SARL framework provides the support of the ACL (mostly according to the FIPA specifications).
This part of the SARL API provides:

* Implementation of protocols, from the initiator and participant points of view.
* A conversation manager that is connected to the SARL agent.
* Several encoding tools of messages in order to enable communication with other ACL-compliant frameworks, e.g. FIPA-OS or Jade.


This tutorial explains briefly the method for creating a new protocol that could be used by the agents.
The example that is taken is the Ping-Pong protocol.
The principle of the protocol is the following:

* The `Ping` agent is sending a `sarl-ping` message to another agent, named the `PongAgent` agent. 
* The `PongAgent` agent is receiving the `sarl-ping` message, and replies with a `sarl-pong` message to the sender of the `sarl-ping` message.
* The `Ping` agent is receiving a `sarl-pong` message and replies to the sender of the `sarl-pong` with a new `sarl-ping` message.

## Definition of the protocol sequence

The first task to be done is to define the communication protocol, usually represented with a sequence diagram. 
The following diagram represents the Ping-Pong protocol:


![Sequence diagram of the Ping-Pong communication protocol](pingpong.png)


The two life-lines in the sequence diagram represent the two actors in the communication protocol:

* **Initiator:** the agent that initiates of the communication.
* **Participant:** the agent that is the participant to the initiated communication.

The *initiator* sends the `sarl-ping` message to the target *participant*.
After some treatment, the *participant* replies with a `sarl-pong` message.


## Implementation of the protocol step-by-step

### States of the Initiator and Participant

The first step is to define the states of the agents when they are involved into the communication.
Basically, the states could be extracted from the previously sequence diagram:

![States of the Initiator and Participant](pingpong-states.png)

In each life-line, a change of state corresponds to the sending or the receiving of a message.
Each activation box corresponds to a specific state.

For the *initiator* point of view, the three following states are defined:

* `NOT_STARTED`: the communication has not yet started.
* `WAITING_ANSWER`: the *initiator* has sent a `sarl-ping` message, and is waiting for the *participant*'s reply.
* `PONG_RECEIVED`: the answer from the *participant* has been received.

For the *participant* point of view, the three following states are defined:

* `NOT_STARTED`: the communication has not yet started.
* `BUILDING_ANSWER`: the initiator has sent a `sarl-ping` message, and it is received by the *participant*. The *initiator* is waiting for the *participant* reply.
* `PONG_SENT`: the *participant* has sent the `sarl-pong`.

The implemenation of the states are done inside an enumeration. The following code is the definition of this enumeration using the Java syntax:

```sarl
public enum PingPongProtocolState implements ProtocolState {

	NOT_STARTED,
	CANCELED,
	BROKEN_PROTOCOL,

	WAITING_ANSWER,
	PONG_RECEIVED,
	BUILDING_ANSWER,
	PONG_SENT;

	public String getName() {
		return name();
	}
	
	public boolean isStarted() {
		return this != NOT_STARTED;
	}

	public boolean isFinished() {
		return this == PONG_RECEIVED
				|| this == PONG_SENT
				|| isCancelled() || isErrorneous();
	}

	public boolean isCancelled() {
		return this == CANCELED;
	}

	public boolean isErrorneous() {
		return isBrokenProtocol();
	}

	public boolean isBrokenProtocol() {
		return this == BROKEN_PROTOCOL;
	}

}
```

First the states are defined. Two more states are added in order to represent the facts that a communication protocol is cancelled by the initiator `CANCELED` and the protocol is broken `BROKEN_PROTOCOL` because one of the participants didn't follows the protocol's sequence.

Since an enumeration in Java is a specific type of class, it is possible to implement functions inside an enumeration. Please note that our state enumeration implements the `ProtocolState` type. This state represents a state into an ACL protocol. It is an interface that provides all the necessary functions that are used by the rest of the ACL API.

Based on this interface implementation, several functions must be implemented:

* `getName`: replies the name of the state, i.e. the name of the enumeration constant.
* `isStarted`: replies if the state indicates that the communication has started.
* `isFinished`: replies if the state corresponds to a final state.
* `isCancelled`: replies if the state indicates that the communication was cancelled.
* `isErrorneous`: replies if the state indicates a protocol error.
* `isBrokenProtocol`: replies if the state represents the broken protocol.


### Events that are received by the agent

An agent could instances a conversation with a `ConversationManager`.
This tool manages the conversations and links the agent to any change of state in the communication protocol. This link may take two forms:

1. Object-oriented event listeners/observers.
2. SARL events that are fired into the inner context of the agent.

The first method does not need a specific action. The second approach needs a specific implementation for the Ping-Pong communication protocol.
Consequently, in order to enable the SARL agents to react to the changes of state in the protocol,
specific events are defined and fired for each receiving of message.

![Events fired into the agents](pingpong-events.png)

Regarding the PingPong protocol, two events are defined:

* `PingReceived` is fired into the *participant* when a `sarl-ping` is received.
* `PongReceived` is fired into the *initiator* when a `sarl-pong` is received.

In the handler for the first event, the *participant* is able to react and reply.
In the handler for the second event, the *initiator* is able to react to the completion of the
protocol.

In addition to the defined events, an shared abstract event must be defined, namely `AbstractSarlPingPongProtocolEvent`. This abstract implementation permits to
share the reference to the instance of `SarlPingPongProtocol` (that will be defind later)
that is representing the instance of the communication protocol for the agent.

```sarl
event AbstractSarlPingPongProtocolEvent extends AbstractAclProtocolEvent {
	val protocol : SarlPingPongProtocol
	new (protocol : SarlPingPongProtocol) {
		this.protocol = protocol
	}
}
```

Then, all the defined events must be implemented:
```sarl
event PingReceived extends AbstractSarlPingPongProtocolEvent
event PongReceived extends AbstractSarlPingPongProtocolEvent
```

### Definition of the PingPong Protocol

For defining the Ping-Pong protocol, a specific class must be defined for each
initiator/participant.
But, before defining these classes, a common super type must be defined.

#### Definition of the Abstract PingPong Protocol

The abstract implementation of the protocol contains all the definitions that
are shared by the different participants' protocols. The `SarlPingPongProtocol`
is the abstract implementation of the Ping-Pong protocol.

```sarl
abstract class SarlPingPongProtocol extends OneToOneAclProtocol {

	new (owner : Agent) {
		super(owner)
		PingPongProtocolState::NOT_STARTED.change(null)
	}

	final override getAclProtocolId : AclProtocolId {
		AclProtocolId::NONE
	}

	final def getProtocolId : String {
		"sarl-pingpong"
	}

	final override isStarted : boolean {
		getState != PingPongProtocolState::NOT_STARTED
	}

	protected final override getCancelledState : ProtocolState {
		PingPongProtocolState::CANCELED
	}

	
	protected final override getErrorState : ProtocolState {
		PingPongProtocolState::BROKEN_PROTOCOL
	}

}
```

First the class extends the `OneToOneAclProtocol` type, which is one of the super types
helping to create specific protocols. The `OneToOneAclProtocol` type represents a protocol
involving an initiator and a single participant.

The constructor forces the state of the protocol to be set to `NO_STARTED`. This constructor
takes a single argument that is the agent owning the protocol behavior, i.e. an ACL protocol
is implemented as a SARL behavior owned by an agent.

The `getAclProtocolId` function replies the identifier of the protocol (from the FIPA
specifications for several of them). Because the Ping-Pong protocol is not part of the FIPA specifications,
the `NONE` value is replied.

The `getProtocolId` function replies the identifier of the protocol. By default, this
function returns the name of the protocol from the FIPA specifications. Because the Ping-Pong protocol is not part of
the FIPA specifications, we define the function in order to reply
the name of the protocol.

The functions `isStarted`, `getCancelledState` and `getErrorState` reply if the conversation
has been started, the "cancel" state and the "error" state, respectively.


#### Definition of the Initiator-side of the Protocol


The definition of the *initiator*-side of the communication protocol is based on the following class that is extending the super type defined in the previous section:

```sarl
behavior InitiatorSarlPingPongProtocol extends SarlPingPongProtocol {
	
	final override isInitiatorSide : boolean {
		true
	}

	final override isParticipantSide : boolean {
		false
	}

	def cancel(reason : Object = null) {
		cancelProtocol(reason, null)
	}
```

The two functions `isInitiatorSide` and `isParticipantSide` specify if the protocol is
for the *initiator* or the *participant* (the *initiator* here).
The `cancel` function is defined in order to enable the *initiator* to cancel the
communication.
This function calls the cancelling function from the API that is defined into the super type.
This function takes the reason of the cancellation as first argument, and the SARL event
to be fired into the agent for notifying the cancellation (none/`null` in our example).


Now, each function that is provided to the *initiator* for starting the conversation are
implemented. In the context of the Ping-Pong protocol, only the `ping` function is provided
in order to send the `acl-ping` message.

The expected arguents of this function are the identifier of the participant (it may be unknown, i.e. `null`), and the application-dependent data to be put into the ping message.

```sarl
def ping(participant : UUID = null, pingDescription : Object) {
```


The first action to do is to initialize the conversation as initiator.
If the participant cannot be determined, the conversation must not be stared.

```sarl
if (participant !== null) {
	participant.initiateAsInitiator
}
val part = this.participant
if (part === null) {
	reportError("Unspecified participant identifier")
} else {
```


Since the conversation is used in a parallel execution context, it is important to
ensure that all the actions within the protocol are "synchronized".
In the following code, we use the `lock` attribute that is defined into the ACL protocol
super type in order to synchronize the protocol's code.

The following code tests if the `ping` function is invoked when the state of the protocol
is `NOT_STARTED`.


```sarl
val rlock = this.lock.readLock
rlock.lock
var st : ProtocolState
try {
	st = this.state
} finally {
	rlock.unlock
}
if (st == PingPongProtocolState::NOT_STARTED) {
	val wlock = this.lock.writeLock
	wlock.lock
	try {
		st = this.state
		if (st == PingPongProtocolState::NOT_STARTED) {
```

Then, the conversation could start. The following actions are done:

* Reset any internal data that is used for storing the result of the conversation.
* Start a parallel timer that is generating a conversation time-out.
* Change to the expected starting state `WAITING_ANSWER`.
* Send the `sarl-ping` message, with the `PROPOSE` performative (this performative is selected within the set of performatives defined by FIPA).


```sarl
this.resultData = null
startTimeoutNotifier
PingPongProtocolState::WAITING_ANSWER.change(null)
sendMessage(proposeDescription, Performative::PROPOSE, part)
```

The rest of the `ping` function's code finalize the implementation of the functions with some unlocking mechanisms and error reporting.

```sarl
				} else {
					reportError(MessageFormat::format("Cannot send a sarl-ping message in state {0}", state.name))
				}
			} finally {
				wlock.unlock
			}
		} else {
			reportError(MessageFormat::format("Cannot send a sarl-ping message in state {0}", state.name))
		}
	}
}
```


As briefly explained before, you may want to store the result of the conversation for further use.
The most used practise is to defined an attribute of type `ProtocolResult` that is reset when
the conversation is canceled.
The value of this attribute is canceled into the `ping` function and is changed when the
`acl-pong` message is received from the *participant*.


```sarl
	var resultData : ProtocolResult

	protected override onCancelled {
		this.resultData = null
	}
```

The *initiator* must react to any message sent by the *participant*, if and only if the
conversation has started and has not failed:


```sarl
on AclMessage [isStarted && !hasFailed] {
```

In this SARL event handler, the first thing to do is to test if the state of the protocol
is valid when receiving the `sarl-pong` message. If not, an error is reported.


```sarl
	val st = getState
	if (st != PingPongProtocolState::WAITING_ANSWER) {
		reportError(MessageFormat::format("Cannot handle a participant message {0} in state {1}",
			occurrence.performative.name,
			st.name))
	} else {
```


The *initiator* checks if the expected performative is received (here `AGREE`).
The result of the conversation is built.
And, the `finish` function is called in order to mark the conversation as finished.
The two arguments of this function are the final state of the conversation, and
the instance of the event to be fired into the associated agent, (here, `PongReceived`).

```sarl
switch (occurrence.performative) {
	case AGREE: {
		val wlock = this.lock.writeLock
		wlock.lock
		try {
			this.resultData = new ProtocolResult(occurrence.sender, occurrence.performative, occurrence.content.content)
			finish(PingPongProtocolState::PONG_RECEIVED, new PongReceived(this))
		} finally {
			wlock.unlock
		}
	}
	default: {
		occurrence.performative.reportUnpexectedPerformativeError
	}
}
```

Of course, if the recieved performative is not expected, an error is reported.

Finally, the function `getAnswer` is implemented in order to enable the *initiator*
to get the result of the conversation. In order to be safe, the synchronization lock
is used, and the conversation state is checked, before returned the conversation
result.

```sarl
def getAnswer : ProtocolResult {
	val rlock = this.lock.readLock
	rlock.lock
	try {
		if (this.state == PingPongProtocolState::PONG_RECEIVED) {
			return this.resultData
		}
	} finally {
		rlock.unlock
	}
	return null
}
```



#### Definition of the Participant-side of the Protocol


The *participant* side of the conversation is defined into the `ParticipantSarlPingPongProtocol`
type. This type extends the shared `SarlPingPongProtocol` type.
Additionnally, the utility functions are defined, a well as the attribute for storing
the data stored into the received `sarl-ping` message.

```sarl
behavior ParticipantSarlPingPongProtocol extends SarlPingPongProtocol {

	var pingData : ProtocolResult

	final override isInitiatorSide : boolean {
		false
	}

	final override isParticipantSide : boolean {
		true
	}
```

At the *participant* side, the `notUnderstood` function is usually defined in order to
give the opportunity to the *participant* to indicate that it is not understanding the
query.
This function takes the reason of the mis-understanding as argument.
It invokes the `notUnderstoodProtocol` function that is defined into the super-type API.

```sarl
def notUnderstood(reason : Object = null) {
	notUnderstoodProtocol(reason)
}
```


The *participant* may need to get the data that is stored into the `sarl-ping` message.

```sarl
def getPing : ProtocolResult {
	val rlock = this.lock.readLock
	rlock.lock
	try {
		if (this.numberOfErrors === 0) {
			return this.pingData
		}
	} finally {
		rlock.unlock
	}
	reportError("Cannot proceed two ping-pong queries in the same conversation")
	return null
}
```

Then, the `pong` function is provided to the *participant* is order to give to it the
ability to reply. The implementation follows the same guidelines as the ones used
for implementing the `ping` function above.

```sarl
def pong(pongInformation : Object = null) {
	val rlock = this.lock.readLock
	rlock.lock
	var st : ProtocolState
	try {
		st = this.state
	} finally {
		rlock.unlock
	}
	if (st == PingPongProtocolState::BUILDING_ANSWER) {
		val wlock = this.lock.writeLock
		wlock.lock
		try {
			st = this.state
			if (st == PingPongProtocolState::BUILDING_ANSWER) {
				PingPongProtocolState::PONG_SENT.finish(null)
				sendMessage(pongInformation, Performative::AGREE, this.initiator)
			} else {
				reportError(MessageFormat::format("Pong cannot be sent in the state {0}", state.name))			
			}
		} finally {
			wlock.unlock
		}
	} else {
		reportError(MessageFormat::format("Pong cannot be sent in the state {0}", state.name))			
	}
}
```


The *participant* has to react to the receiving of the `sarl-ping` message.
The following code is the SARL event handler that is dedicated to the implementation
of the pong-reply by the *participant*.
The implementation follows the same guidelines as for the *initiator*'s event handler. 

```sarl
on AclMessage [!hasFailed] {
	switch (occurrence.performative) {
		case Performative::PROPOSE: {
			val data = getPing
			if (data === null) {
				val wlock = this.lock.writeLock
				wlock.lock
				try {
					this.pingData = new ProtocolResult(occurrence.sender, Performative::PROPOSE, occurrence.content.content)
					this.conversationId = occurrence.conversationId
					this.initiator = occurrence.sender
					PingPongProtocolState::BUILDING_ANSWER.change(new PingReceived(this))
					startTimeoutNotifier
				} finally {
					wlock.unlock
				}
			} else {
				reportError("Cannot proceed two ping-pong queries in the same conversation")
			}
		}
		default: {
			reportUnpexectedPerformativeError(occurrence.performative)
		}
	}
}
```

