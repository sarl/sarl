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
 * This document defines how to declare events in SARL.
 */
@CreateWith(SARLSpecCreator)
describe "Event Reference"{
	
		@Inject extension SARLParser
		@Inject extension IQualifiedNameProvider
		/*
		 * @filter(.* = '''|'''|.parsesSuccessfully.*) 
		 */
		fact "Define an event"{
			val model = '''
			package events
			event MyEvent {
			}
			'''.parsesSuccessfully
			val evt = model.elements.filter(Event).head
			assertEquals("events.MyEvent", evt.fullyQualifiedName.toString)
		}
	
		/**
		 * Events in SARL can carry information
		 * 
		 * @filter(.* = '''|'''|.parsesSuccessfully.*) 
		 */
		fact "Declare event with attributes"{
			val model = '''
			package myapp.demo
			event MyEvent {
				var number : Integer
			}
			'''.parsesSuccessfully
			val evt = model.elements.filter(Event).head
			assertEquals(1,evt.features.size)
			val att = evt.features.filter(Attribute).head
			assertEquals("number",att.name)
			assertEquals("java.lang.Integer", att.type.identifier)
		}
		
		/**
		 * Events in SARL can carry information that is unmodifiable using `val` d
		 * 
		 * @filter(.* = '''|'''|.parsesSuccessfully.*) 
		 */
		fact "Declare event with value attributes"{
			val model = '''
			package myapp.demo
			import io.sarl.lang.core.Agent
			event MyEvent {
				val number : Integer
				
				new(nb:Integer){
					number = nb
				}
			}
			'''.parsesSuccessfully
			val evt = model.elements.filter(Event).head
			assertEquals(2,evt.features.size)//attribute and constructor
			val att = evt.features.filter(Attribute).head
			assertEquals("number",att.name)
			assertEquals("java.lang.Integer", att.type.identifier)
		}
}
