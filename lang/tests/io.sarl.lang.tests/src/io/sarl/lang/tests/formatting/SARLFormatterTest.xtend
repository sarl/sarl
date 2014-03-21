/*
 * Copyright 2014 Sebastian RODRIGUEZ, Nicolas GAUD, Stéphane GALLAND.
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */
package io.sarl.lang.tests.formatting

import com.google.inject.Inject
import io.sarl.lang.SARLInjectorProvider
import io.sarl.lang.sarl.Model
import org.eclipse.xtext.formatting.INodeModelFormatter
import org.eclipse.xtext.junit4.InjectWith
import org.eclipse.xtext.junit4.XtextRunner
import org.eclipse.xtext.junit4.util.ParseHelper
import org.eclipse.xtext.resource.XtextResource
import org.junit.Test
import org.junit.runner.RunWith

import static extension org.junit.Assert.*

/**
 * @author $Author: Sebastian Rodriguez$
 * @version $Name$ $Revision$ $Date$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 */
@RunWith(XtextRunner)
@InjectWith(SARLInjectorProvider)
class SARLFormatterTest {
	@Inject extension ParseHelper<Model>
	@Inject extension INodeModelFormatter

	@Test
	def void testAgents() {
		'''agent A1 { } agent A2 {}
			'''.assertFormattedAs(
			'''
			agent A1 {
			}
			
			agent A2 {
			}'''
		)
	}

	@Test
	def void testBehaviors() {
		'''behavior B1 { } behavior B2 {}
			'''.assertFormattedAs(
			'''
			
			
			behavior B1 {
			}
			
			behavior B2 {
			}'''
		)
	}

	@Test
	def void testBehaviors2() {
		'''
			behavior B1 { 
				requires Cap1, Cap2 
				uses Cap1
				uses Cap2
				
				def a1 {
					//something
				}
				
				on E [] {
					if(true == true) { println("Nice")}
				}
				}
				 behavior B2 {}
		'''.assertFormattedAs(
			'''
			
			
			behavior B1 {
				requires Cap1, Cap2
				uses Cap1
				uses Cap2
			
				def a1 {
				//something
			
				}
			
				on E [] {
					if(true == true) {
						println("Nice")
					}
				}
			
			}
			
			behavior B2 {
			}'''
		)
	}

	@Test
	def void testAgentsSpaces() {
		'''
			agent A1 { }
			 
			 
			 
			 agent A2 {}
		'''.assertFormattedAs(
			'''
			agent A1 {
			}
			
			agent A2 {
			}'''
		)
	}

	@Test
	def void testAgentActions() {
		'''
			agent A {
			uses Cap1, Cap
			
			on E [occurrence.name == "Hello"] {
				println("event is hello") 
				action2
				
				
								action2
				
				
				
												action2 				action2
			}
			
			def action2 {
				println("action2")
				println("action2..") println("action2..")
			}
			}
		'''.assertFormattedAs(
			'''
			agent A {
				uses Cap1, Cap
			
				on E [occurrence.name == "Hello"] {
					println("event is hello")
					action2
			
					action2
			
					action2 action2
				}
			
				def action2 {
					println("action2")
					println("action2..") println("action2..")
				}
			}'''
		)
	}

	//}
	@Test
	def void testSkill() {
		'''
			skill S implements C{
			new(ag:Agent){super(ag)}
			def action(a:String){
				println(a)
			}
			}
		'''.assertFormattedAs(
			'''
			
			skill S implements C {
			
				new(ag : Agent) {
					super(ag)
				}
			
				def action(a : String) {
					println(a)
				}
			}'''
		)
	}

	@Test
	def void testAttributes() {
		'''
			agent A1 { var cmdAddr : Address var cmdAddr2 : Address }
		'''.assertFormattedAs(
			'''
			agent A1 {
				var cmdAddr : Address
				var cmdAddr2 : Address
			}'''
		)
	}

	@Test
	def void testVarDeclaration() {
		'''
			agent A1 { var cmdAddr : Address var cmdAddr2 : Address 
			on E {
					val cmd :Address
					val cmd2 : Address
					
					
				}}
		'''.assertFormattedAs(
			'''
			agent A1 {
				var cmdAddr : Address
				var cmdAddr2 : Address
			
				on E {
					val cmd : Address
					val cmd2 : Address
			
				}
			}'''
		)
	}

	@Test
	def void testVarAssigement() {
		'''
			agent A1 { var cmdAddr : Address var cmdAddr2 : Address 
			on E {
					val cmd :Address
					val cmd2 : Address
					cmd = null 
					cmd2 = null
				}}
		'''.assertFormattedAs(
			'''
			agent A1 {
				var cmdAddr : Address
				var cmdAddr2 : Address
			
				on E {
					val cmd : Address
					val cmd2 : Address
					cmd = null
					cmd2 = null
				}
			}'''
		)
	}

	@Test
	def void testUses() {
		'''
			agent A1 { uses Cap1 , Cap2, Cap3 }
		'''.assertFormattedAs(
			'''
			agent A1 {
				uses Cap1, Cap2, Cap3
			}'''
		)
	}

	@Test
	def void testBehaviorUnitOnAgent() {
		'''
			agent A1 { uses Cap1
			on Initialize {} }
		'''.assertFormattedAs(
			'''
			agent A1 {
				uses Cap1
			
				on Initialize {
				}
			}'''
		)
	}

	@Test
	def void testBehaviorUnit() {
		'''
			agent A {
				on Initialize { if(true) {println("hello")} }
				}
		'''.assertFormattedAs(
			'''
			agent A {
			
				on Initialize {
					if(true) {
						println("hello")
					}
				}
			}'''
		)
	}

	@Test
	def void testXbase() {
		'''
			agent A {
				on Initialize {
								if(!dir.exists){
				throw new IllegalArgumentException("Path ["+myPath+"] is not a Directory")
				}
				
						for(f : dir.listFiles()){
				if(f.directory){
					if(!f.name.equals(".")&& !f.name.equals("..")) {
						val targetID = FileSearchAgent.spawnInContext(innerContext,#[f.absolutePath,false.toString]);
						innerContext.defaultSpace.emit(new Search =>[source=innerContext.defaultSpace.getAddress(ID); date=maxDate;fileMatch = fMatch], AddressScope.getScope(innerContext.defaultSpace.getAddress(targetID)));
					}
				}else{
					if((maxDate.time >= f.lastModified) && !(f.name.startsWith(".")) && (f.name.endsWith(fMatch))){
						emit(new FoundFile =>[file = f.absolutePath],AddressScope.getScope(cmdAddr))
					}
				}
			}
			if (true == true){prinln ("cool!")}else{}
						
				}
				}
		'''.assertFormattedAs(
			'''
			agent A {
			
				on Initialize {
					if(!dir.exists) {
						throw new IllegalArgumentException("Path [" + myPath + "] is not a Directory")
					}
			
					for(f:dir.listFiles()){
						if(f.directory) {
							if(!f.name.equals(".") && !f.name.equals("..")) {
								val targetID = FileSearchAgent.spawnInContext(innerContext, # [ f.absolutePath , false.toString ]);
								innerContext.defaultSpace.emit(new Search => [source = innerContext.defaultSpace.getAddress(ID) ; date = maxDate ; fileMatch = fMatch], AddressScope.getScope(innerContext.defaultSpace.getAddress(targetID)));
							}
						} else {
							if((maxDate.time >= f.lastModified) && !(f.name.startsWith(".")) && (f.name.endsWith(fMatch))) {
								emit(new FoundFile => [file = f.absolutePath], AddressScope.getScope(cmdAddr))
							}
						}
					}
					if(true == true) {
						prinln("cool!")
					} else {
					}
				}
			}'''
		)
	}

	@Test
	def void testBehaviorUnitIfNoBrackets() {
		'''
			agent A {
				on Initialize { if(true) println("hello") else println("goodbye") }
				}
		'''.assertFormattedAs(
			'''
			agent A {
			
				on Initialize {
					if(true) println("hello") else println("goodbye")
				}
			}'''
		)
	}

	@Test
	def void testImports() {
		'''
			import io.sarl.core.Agent import io.sarl.core.Event
			
			import io.sarl.core.Agent 
			import io.sarl.core.Event
			
			import static java.lang.String . *
		'''.assertFormattedAs(
			'''
			import io.sarl.core.Agent
			import io.sarl.core.Event
			
			import io.sarl.core.Agent
			import io.sarl.core.Event
			
			import static java.lang.String.*'''
		)
	}

	@Test
	def void testImports2() {
		'''
			import io.sarl.core.Agent import io.sarl.core.Event
			
			import io.sarl.core.Agent 
			import io.sarl.core.Event
			
			agent A {}
		'''.assertFormattedAs(
			'''
			import io.sarl.core.Agent
			import io.sarl.core.Event
			
			import io.sarl.core.Agent
			import io.sarl.core.Event
			
			agent A {
			}'''
		)
	}

	@Test
	def void testEvents() {
		'''
			event E1 {} event E2 {}
		'''.assertFormattedAs(
			'''
			event E1 {
			}
			
			event E2 {
			}'''
		)
	}

	@Test
	def void testEventsWithAgent() {
		'''
			event E1 {} agent A {}
		'''.assertFormattedAs(
			'''
			event E1 {
			}
			
			agent A {
			}'''
		)
	}

	@Test
	def void testEventsWithAgentAndImport() {
		'''
			import io.sarl.core.Schedules
			
			event E1 {} agent A {}
		'''.assertFormattedAs(
			'''
			import io.sarl.core.Schedules
			
			event E1 {
			}
			
			agent A {
			}'''
		)
	}

	@Test
	def testActions() {
		'''
				import io.sarl.core.Schedules
				
				event E1 {} agent A {
				
				on Destroy {
				println("I'm about to die, bye :-)")
			} def isInternal ( e : Event ) : boolean {
				return e.source.spaceId.equals(innerContext.defaultSpace.ID);
			} def hasMembers : boolean {
				return innerContext.defaultSpace.participants.size > 1
			}
				}
		'''.assertFormattedAs(
			'''
			import io.sarl.core.Schedules
			
			event E1 {
			}
			
			agent A {
			
				on Destroy {
					println("I'm about to die, bye :-)")
				}
			
				def isInternal(e : Event) : boolean {
					return e.source.spaceId.equals(innerContext.defaultSpace.ID);
				}
			
				def hasMembers : boolean {
					return innerContext.defaultSpace.participants.size > 1
				}
			}'''
		)
	}

	@Test
	def testCapacity() {
		'''
capacity ExternalContextAccess {
	/**
	 * Replies all contexts this agent is a member of, including the default context
	 */	 
	def getAllContexts : Collection<AgentContext>

	/**
	 * Replies the AgentContext for the given ID.
	 * The agent must have joined the context before calling this action or use its parentContextID
	 * @see{Agent#getParentID}
	 * @see{#join}
	 * 
	 * @throws UnknownContextException if the context specified context is not known by the agent.
	 * @param contextID the ID of the context to get.
	 */
	def getContext(contextID : UUID): AgentContext

    /**
	 * Joins a new parent context (a new super holon).
	 * This actions registers the agent in the default Space of the parent Context.
	 * . 
	 * @fires ContextJoined in its inner Context default space (Behaviors#wake).
	 * @fires MemberJoined in its parent Context default Space 
	 */
	def join(futureContext : UUID, futureContextDefaultSpaceID : UUID) fires ContextJoined, MemberJoined

	/**
	 * Leaves the parent's context. 
	 * @fires ContextLeft in its inner Context default space (Behaviors#wake).
	 * @fires MemberLeft in its parent Context default Space 
	 */
	def leave(contextID : UUID) fires ContextLeft, MemberLeft

}

		'''.
		assertFormattedAs(
			'''
			
			capacity ExternalContextAccess {
			/**
				 * Replies all contexts this agent is a member of, including the default context
				 */
				def getAllContexts : Collection <AgentContext>
			
				/**
				 * Replies the AgentContext for the given ID.
				 * The agent must have joined the context before calling this action or use its parentContextID
				 * @see{Agent#getParentID}
				 * @see{#join}
				 * 
				 * @throws UnknownContextException if the context specified context is not known by the agent.
				 * @param contextID the ID of the context to get.
				 */
				def getContext(contextID : UUID) : AgentContext
			
				/**
				 * Joins a new parent context (a new super holon).
				 * This actions registers the agent in the default Space of the parent Context.
				 * . 
				 * @fires ContextJoined in its inner Context default space (Behaviors#wake).
				 * @fires MemberJoined in its parent Context default Space 
				 */
				def join(futureContext : UUID, futureContextDefaultSpaceID : UUID) fires ContextJoined, MemberJoined
			
				/**
				 * Leaves the parent's context. 
				 * @fires ContextLeft in its inner Context default space (Behaviors#wake).
				 * @fires MemberLeft in its parent Context default Space 
				 */
				def leave(contextID : UUID) fires ContextLeft, MemberLeft
			}'''
		)
	}

	@Test
	def testPackage() {

		'''
			package test
				import io.sarl.core.Agent import io.sarl.core.Event
				
				import io.sarl.core.Agent 
				import io.sarl.core.Event
				
				import static java.lang.String . *
				
				event E {}
		'''.assertFormattedAs(
			'''
				package test
				
				import io.sarl.core.Agent
				import io.sarl.core.Event
				
				import io.sarl.core.Agent
				import io.sarl.core.Event
				
				import static java.lang.String.*
				
				event E {
				}'''
		)

	}

	def void assertFormattedAs(CharSequence input, CharSequence expected) {
		expected.toString.assertEquals(
			(input.parse.eResource as XtextResource).parseResult.rootNode.format(0, input.length).formattedText)
	}
}
