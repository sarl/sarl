/*
 * $Id$
 *
 * SARL is an general-purpose agent programming language.
 * More details on http://www.sarl.io
 *
 * Copyright (C) 2014-2017 the original authors or authors.
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
 * <p>This document describes the basics of the overriding of the event handlers.
 * The key feature that is considered in this document is the overriding of the behavior associated to an event handler.
 * Let an event occurrence of `MyEvent`. In the definition of the abstract agent named `AbstractAgent`, the message
 * "hello world" is printed out when an occurrence of `MyEvent` is received.
 * Let consider that you want to avoid this message printing in a sub-type of `AbstractAgent`, but do something other.
 * 
 * <p>In the SARL language, it is impossible to override the event handlers, i.e. the `on` statements.
 * So, <strong>how to override the behavior of an inherited `on` statement?</strong>
 *
 * <p>In this document, the operational semantic of the event handlers is reminded. Then, the best practice for overriding
 * the event handler behavior is explained.
 */
@CreateWith(SARLSpecCreator)
describe "Event Handler Overriding"{

	@Inject extension SARLParser

	/* The operational semantics of the event handler is the following:
	 * when an event is received by an agent (or a behavior), all the event handlers matching the
	 * event type and with a valid guard on the event occurrence are run in parallel.
	 *
	 * <p>The event handlers from the current agent type, and from all the super agent types are considered.
	 * Let the example below. When the agent `MyAgent` receives an occurrence of `MyEvent`, the two
	 * event handlers are runs. It means that the CODE1 is run and the message "hello world" is printed out.
	 * These two event handlers are run in parallel.
	 * 
	 * @filter(.* = '''|'''|.parseSuccessfully.*)
	 */
	fact "Operational semantic of the event handlers" {
		'''
		event MyEvent
		
		agent MySuperAgent {
		  uses Logging
		  on MyEvent {
		    info("hello world")
		  }
		}
		
		agent MyAgent extends MySuperAgent {
		  on MyEvent {
		    // CODE1
		  }
		}
		'''.parseSuccessfully(
			"package io.sarl.docs.bestpractices.eventbuilder
			capacity Logging {
				def info(s : String)
			}",
			"")
	}

	/* For overriding the behavior associated to an event handler, it is mandatory to use the function overriding mechanism
	 * that is provided by the SARL language.
	 *
	 * <p>A function must be defined in the super type for each event handler that could be overridden.
	 * In the following example, the function is named `myEventBehavior`. The code of this function is the code
	 * of the event handler from the previous section. The event handler invokes the defined function.
	 * 
	 * @filter(.* = '''|'''|.parseSuccessfully.*)
	 */
	fact "Defining the super behavior in a function" {
		'''
		event MyEvent
		
		agent MySuperAgent {
		  uses Logging
		  on MyEvent {
		    myEventBehavior
		  }
		  def myEventBehavior {
		    info("hello world")
		  }
		}
		'''.parseSuccessfully(
			"package io.sarl.docs.bestpractices.eventbuilder
			capacity Logging {
				def info(s : String)
			}",
			"")
	}

	/* For overriding the behavior, the function `myEventBehavior` could be overridden in the sub-type.
	 * With the example below, the message "hello world" is no more printed out because of the function overriding.
	 *
	 * @filter(.* = '''|'''|.parseSuccessfully.*)
	 */
	fact "Overriding the super function" {
		'''
		agent MyAgent extends MySuperAgent {
		  def myEventBehavior {
		    // CODE1
		  }
		}
		'''.parseSuccessfully(
			"package io.sarl.docs.bestpractices.eventbuilder
			event MyEvent
			agent MySuperAgent {
			  def myEventBehavior {
			  }
			}",
			"")
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
