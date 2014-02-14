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
import org.jnario.runner.CreateWith

/*
 * This document describes the general syntax of the SARL Language.
 * While we will use the agent definition, it is also valid for other concepts.
 * Please see the specific Reference documentation for details.
 * 
 */
@CreateWith(SARLSpecCreator)
describe "General Syntax Reference"{

	@Inject extension SARLParser
	/*
	 * @filter(.* = '''|'''|.parsesSuccessfully.*) 
	 */
	fact "Variable definition"{
		'''
		agent A {
			//Variable defnition
			var name : String
			
			//final variable definition - It can not be reassigned
			val string : String = "string value"
			
			//Infered type to String
			var lastname = "Lastname"
		}
		'''.parsesSuccessfully
	}
	
	/*
	 * @filter(.* = '''|'''|.parsesSuccessfully.*) 
	 */
	fact "Java compatibility" {
		'''
		agent A {
			//Variable defnition
			var date : java.util.Date = new java.util.Date()
			
			//final variable definition - It can not be reassigned
			val otherDate : java.util.Date = new java.util.Date()
			
			//Infered type to java.lang.String
			var lastname = "Lastname"
		}
		'''.parsesSuccessfully
	}
	
	/*
	 * @filter(.* = '''|'''|.parsesSuccessfully.*) 
	 */
	fact "Action definition"
	
}