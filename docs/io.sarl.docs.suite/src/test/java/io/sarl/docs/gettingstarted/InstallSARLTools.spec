/*
 * $Id$
 *
 * SARL is an general-purpose agent programming language.
 * More details on http://www.sarl.io
 *
 * Copyright (C) 2014-2015 the original authors or authors.
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
package io.sarl.docs.gettingstarted

import io.sarl.docs.utils.SARLSpecCreator
import org.jnario.runner.CreateWith

import static extension io.sarl.docs.utils.SpecificationTools.*
import static extension org.junit.Assume.*

/*
 * There are two easy ways to get SARL up and running.
 * A pre-configured Eclipse distribution is available which has already 
 * all the necessary plug-ins installed. 
 * Alternatively, you can install SARL SDK into your existing 
 * Eclipse using the Eclipse update mechanism.
 */
@CreateWith(SARLSpecCreator)
describe "Install SARL Tools" {
	
	/* You must follow the instructions given on
	 * the [download](%website%/download/index.html) page for installing the
	 * SARL tools.
	 * 
	 * @filter(.*) 
	 */
	fact "Installation Instructions" {
		// The checks are valid only if the macro replacements were done.
		// The replacements are done by Maven.
		// So, Eclipse Junit tools do not make the replacements.
		System.getProperty("sun.java.command", "").startsWith("org.eclipse.jdt.internal.junit.").assumeFalse
		// URLs should not end with a slash
		"%website%" should beURL "!file"
	} 
	
	/*
	 * In the next section, we will learn how to create a SARL project.
	 * 
	 * [Next>](CreateFirstProjectSpec.html)
	 * 
	 * @filter(.*) 
	 */
	fact "What's next?" {
		"CreateFirstProjectSpec.html" should beAccessibleFrom this
	}

}
