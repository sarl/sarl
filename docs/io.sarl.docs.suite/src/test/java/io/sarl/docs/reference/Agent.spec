/*
 * Copyright 2014 Sebastian RODRIGUEZ, Nicolas GAUD, Stéphane GALLAND
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
package io.sarl.docs.reference

import com.google.inject.Inject
import io.sarl.docs.utils.SARLParser
import io.sarl.docs.utils.SARLSpecCreator
import io.sarl.lang.sarl.Attribute
import io.sarl.lang.sarl.Event
import org.jnario.runner.CreateWith

import static org.junit.Assert.*
import org.eclipse.xtext.naming.IQualifiedNameProvider

/**
 * This document describes the features related to the definition of an agent in SARL.
 */
@CreateWith(SARLSpecCreator)
describe "Agent Reference"{
	
		@Inject extension SARLParser
		@Inject extension IQualifiedNameProvider
		
		/*
		 * @filter(.* = '''|'''|.parsesSuccessfully.*) 
		 */
		fact "Define an agent"{
			val model = '''
			package events
			event MyEvent {
			}
			'''.parsesSuccessfully
			val evt = model.elements.filter(Event).head
			assertEquals("events.MyEvent", evt.fullyQualifiedName.toString)
		}
	
		/*
		 * @filter(.* = '''|'''|.parsesSuccessfully.*) 
		 */
		fact "Agent Attributes"{
			val model = '''
			package events
			event MyEvent {
			}
			'''.parsesSuccessfully
			val evt = model.elements.filter(Event).head
			assertEquals("events.MyEvent", evt.fullyQualifiedName.toString)
		}

		/*
		 * @filter(.* = '''|'''|.parsesSuccessfully.*) 
		 */
		fact "Define behavioral units"{
			val model = '''
			package events
			event MyEvent {
			}
			'''.parsesSuccessfully
			val evt = model.elements.filter(Event).head
			assertEquals("events.MyEvent", evt.fullyQualifiedName.toString)
		}

		/*
		 * @filter(.* = '''|'''|.parsesSuccessfully.*) 
		 */
		fact "Declare Skills for an Agent"{
			val model = '''
			package events
			event MyEvent {
			}
			'''.parsesSuccessfully
			val evt = model.elements.filter(Event).head
			assertEquals("events.MyEvent", evt.fullyQualifiedName.toString)
		}

		/*
		 * @filter(.* = '''|'''|.parsesSuccessfully.*) 
		 */
		fact "Use the Capacities"{
			val model = '''
			package events
			event MyEvent {
			}
			'''.parsesSuccessfully
			val evt = model.elements.filter(Event).head
			assertEquals("events.MyEvent", evt.fullyQualifiedName.toString)
		}

}
