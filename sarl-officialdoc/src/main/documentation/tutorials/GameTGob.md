# Goal-oriented Action Selection from Video Game Field

Goal-oriented behavior (GOB) is a general term that covers any technique taking into account goals of the agents.
GOB is not supported by a single technique.
This tutorial does a focus on GOB that are used in video games, and more specifically on a simple action selection
mechanism that is including time constraints.

This tutorial is related to the reference documentation on [GOB](../reference/gob/TGob.md).
Please refer to this documentation for understanding the concepts of goal, action, and action selection mechanism.

It is now possible to write an agent that is behaving as it is expected in GOB.

The SARL standard development kit provides an API that is implementing the algorithms from the previous section.
The rest of this tutorial describes how to use this API for creating an agent that is selecting an action to reach its goal, and based on the previous algorithms.

## Defining goals and actions in the agent knowledge

The first step is to define the goals and actions that are available to the agent.
The following code defines the function [:initsystemfunction:] that is declaring two goals ([:goaleat!] and [:goalrest!]) and three actions ([:actionsnack!], [:actionmeal!] and [:actionbathroom!]).

[:Success:]
	package io.sarl.docs.tutorials.gametgob
	import io.sarl.api.core.Initialize
	import io.sarl.api.game.tgob.BaseAction
	import io.sarl.api.game.tgob.BaseGoal
	[:On]
	agent Person {

		val goals = <BaseGoal>newArrayList
	
		val actions = <BaseAction>newArrayList

		on Initialize {
			initializeGoalSystem
		}

		def [:initsystemfunction](initializeGoalSystem) : void {
			val g0 = new [:basegoaltype](BaseGoal)("[:goaleat](Eat)", Math::floor(Math::random * 10.0), 0.6)
			val g1 = new BaseGoal("[:goalrest](Rest)", Math::floor(Math::random * 10.0), 0.4)
			this.goals += g0
			this.goals += g1
	
			val a0 = new [:baseactiontype](BaseAction)("[:actionsnack](Eat-Snack)", [:actionsnackduration](2)) => [
				setInsistenceChangeFor(g0, [:actionsnackeatinsistence](-2.0))
			]
			this.actions += a0
			val a1 = new BaseAction("[:actionmeal](Eat-Main-Meal)", 4) => [
				setInsistenceChangeFor(g0, -4.0)
			]
			this.actions += a1
			val a2 = new BaseAction("[:actionbathroom](Visit-Bathroom)", 1) => [
				setInsistenceChangeFor(g1, -4.0)
			]
			this.actions += a2
		}
	}
[:End:]

All the defined goals and actions are stored into local variables to the agent.
The SARL API provides the type [:basegoaltype:] that is representing a goal. For constructing a goal, its name, initial insistence, and insistence change per time unit must be provided to the constructor.
The type [:baseactiontype:] represents an action, with its name and duration as arguments to its constructor.
In the code, the operator `object => initialization code` is used for setting the insistences per goal that are associated to the action.
For example, the action [:actionsnack!] has a duration of [:actionsnackduration!] units. It also applies a change of insistence about [:actionsnackeatinsistence!] to the goal [:goaleat!].

## Calling the GOB action selector

As soon as the goals and actions are defined, it is possible to invoke the GOB action selector for choosing the best action.
The code of the agent is updated for obtaining:

[:Success:]
	package io.sarl.docs.tutorials.gametgob
	import io.sarl.api.core.Initialize
	import io.sarl.api.core.Logging
	import io.sarl.api.game.tgob.Action
	import io.sarl.api.game.tgob.BaseAction
	import io.sarl.api.game.tgob.BaseGoal
	import io.sarl.api.game.tgob.SelectAction
	import io.sarl.api.game.tgob.StandardActionSelectionSkill
	[:On]
	agent Person {

		uses [:selectactioncapacity](SelectAction)
		uses Logging

		val goals = <BaseGoal>newArrayList
	
		val actions = <BaseAction>newArrayList

		on Initialize {
			initializeGoalSystem

			[:setskillfunction](setSkill)(new [:selectactionskill](StandardActionSelectionSkill))

			var selectedAction = [:selectionactionfunction](selectAction)(this.actions, this.goals)
			
			info("Selected action: " + selectedAction)
		}

		def initializeGoalSystem : void {
			val g0 = new BaseGoal("Eat", Math.floor(Math.random * 10.0), 0.6)
			val g1 = new BaseGoal("Rest", Math.floor(Math.random * 10.0), 0.4)
			this.goals += g0
			this.goals += g1
	
			val a0 = new BaseAction("Eat-Snack", 2) => [
				setInsistenceChangeFor(g0, -2.0)
			]
			this.actions += a0
			val a1 = new BaseAction("Eat-Main-Meal", 4) => [
				setInsistenceChangeFor(g0, -4.0)
			]
			this.actions += a1
			val a2 = new BaseAction("Visit-Bathroom", 1) => [
				setInsistenceChangeFor(g1, -4.0)
			]
			this.actions += a2
		}
	}
[:End:]

In this code, the capacity [:selectactioncapacity:] is used. It exhibits to the agent the capability to select the best action according to the GOB implementation.
Because this capacity has not an implementation skill that is automatically provided by the SARL run-time environment, it is necessary to equip the agent with
the proper skill, i.e., [:selectactionskill:], by calling the function [:setskillfunction:].

It is now possible to invoke the function [:selectionactionfunction:] that gets the actions and goals as arguments, and replies the selected action.



[:Include:](../legal.inc)
