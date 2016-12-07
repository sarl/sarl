/*
 * $Id$
 *
 * SARL is an general-purpose agent programming language.
 * More details on http://www.sarl.io
 *
 * Copyright (C) 2014-2016 the original authors or authors.
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
package io.sarl.docs.bestpractices

import com.google.inject.Inject
import io.sarl.docs.utils.SARLParser
import io.sarl.docs.utils.SARLSpecCreator
import org.jnario.runner.CreateWith

import static extension io.sarl.docs.utils.SpecificationTools.*
import static extension org.junit.Assume.*;

/* @outline
 *
 * <p>This document describes the basics of the creation
 * of events with a builder.
 * In some cases, each event must have a unique identifier.
 * But because static fields are not allowed in the event definitions,
 * it is impossible to store the next available event ID in a static field.
 * The best way for creating events with unique identifiers is to apply the
 * [builder design pattern](https://en.wikipedia.org/wiki/Software_design_pattern).  
 * 
 * <p>The elements that are explained in this document are:
 * 
 *  * the definition of an event;
 *  * the definition of an event builder;
 *  * the use of the event builder.
 */
@CreateWith(SARLSpecCreator)
describe "Event Creation with a Builder"{

	@Inject extension SARLParser

	/* The purpose of this document is to create an event, which has a unique identifier.
	 * This identifier is an integer number that should be incremented each time an
	 * event instance is created.
	 *
	 * <p>The definition of the event should define the event with a read-only identifier (`val`).
	 * Because the identifier is a value, it must be initialize in the constructor of the event.
	 * Consequently, a constructor is defined with the identifier value as parameter.
	 * 
	 * @filter(.* = '''|'''|.parseSuccessfully.*)
	 */
	fact "Definition of the event" {
		'''
		package io.sarl.docs.bestpractices.eventbuilder
		event MyEvent {
			val id : long
			new(id : long) {
				this.id = id
			}
		}
		'''.parseSuccessfully
	}

	/* For creating the event instances, we apply the [builder design pattern](https://en.wikipedia.org/wiki/Software_design_pattern)
	 *
	 * <p>A builder is a class that is able to create an instance of the event when it is invoked.
	 * The next available unique identifier for the events is stored into a field of the builder (`id`).
	 * 
	 * <p>The function `newInstance` is defined for creating an instance of the event. This function
	 * gets the next available global identifier to give it to the event instance, and it increments
	 * the next available global identifier for the next call to `newInstance`.
	 * Finally, the `newInstance` function create the event instance with the appropriate identifier. 
	 * 
	 * @filter(.* = '''|'''|.parseSuccessfully.*)
	 */
	fact "Definition of the event builder" {
		'''
		class MyEventBuilder {
			private var id : long = 0
			def newInstance : MyEvent {
				val eventId = id
				id++
				return new MyEvent(eventId)
			}
		}
		'''.parseSuccessfully(
			'''
			package io.sarl.docs.bestpractices.eventbuilder
			event MyEvent {
				val id : long
				new(id : long) {
					this.id = id
				}
			}
			''',
			//TEXT
			'''
			''')
		"https://en.wikipedia.org/wiki/Software_design_pattern" should beURL "!file"
	}

	/* For using the event builder, you have simply to create an instance of the `MyEventBuilder`
	 * class, and use it as follow (two events are created in the example):
	 * 
	 * @filter(.* = '''|'''|.parseSuccessfully.*)
	 */
	fact "Use of the event builder" {
		'''
		val builder = new MyEventBuilder
		var event1 = builder.newInstance
		var event2 = builder.newInstance
		'''.parseSuccessfully(
			'''
			package io.sarl.docs.bestpractices.eventbuilder
			event MyEvent
			class MyEventBuilder {
				private var id : long = 0
				def newInstance : MyEvent {
					null
				}
			}
			class MyEventBuilderUser {
				def use {
			''',
			//TEXT
			'''
				}
			}
			''')
		"https://en.wikipedia.org/wiki/Software_design_pattern" should beURL "!file"
	}

	/* Specification: SARL General-purpose Agent-Oriented Programming Language ("Specification")<br/>
	 * Version: %sarlspecversion%<br/>
	 * Status: %sarlspecreleasestatus%<br/>
	 * Release: %sarlspecreleasedate%
	 * 
	 * 
	 * <p>Copyright &copy; %copyrightdate% %copyrighters%.
	 * 
	 * <p>Licensed under the Apache License, Version 2.0;
	 * you may not use this file except in compliance with the License.
	 * You may obtain a copy of the [License](http://www.apache.org/licenses/LICENSE-2.0).
	 *
	 * @filter(.*) 
	 */
	fact "Legal Notice" {
		// The checks are valid only if the macro replacements were done.
		// The replacements are done by Maven.
		// So, Eclipse Junit tools do not make the replacements.
		System.getProperty("sun.java.command", "").startsWith("org.eclipse.jdt.internal.junit.").assumeFalse
		//
		"%sarlversion%" should startWith "%sarlspecversion%"
		("%sarlspecreleasestatus%" == "Stable Release"
			|| "%sarlspecreleasestatus%" == "Draft Release") should be true
		"%sarlspecreleasedate%" should beDate "YYYY-mm-dd"
		"%copyrightdate%" should beNumber "0000";
		("%copyrighters%".empty || "%copyrighters%".startsWith("%")) should be false
	}

}
