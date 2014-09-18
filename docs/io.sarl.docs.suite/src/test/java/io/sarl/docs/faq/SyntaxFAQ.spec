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
package io.sarl.docs.faq

import com.google.inject.Inject
import io.sarl.docs.utils.SARLParser
import io.sarl.docs.utils.SARLSpecCreator
import org.jnario.runner.CreateWith

/*
 * @outline
 */
@CreateWith(SARLSpecCreator)
describe "SARL Syntax FAQ" {

		@Inject extension SARLParser

		/*  
		 */
		context "General Syntax" {

			/* __No__.
			 * When a decimal point is written in the literal,
			 * the fractional part and the mantissa part must
			 * be specify also, even if these parts are equal
			 * to zero.
			 * 
			 * @filter(.* = '''|'''|.parsesWithError.*) 
			 */
			fact "Can I use the same syntax as in Java for number literals?" {
				'''
					124.0 // Correct syntax
					124.  // Incorrect syntax
					0.123 // Correct syntax
					.123  // Incorrect syntax
				'''.parseWithError(
						"package io.sarl.docs.faq.syntax
						agent A {
							def action : double {",
						// TEXT
						"} }"
				)
			}
			
			/* It is not allowed to put a SARL keyword, such as
			 * `agent`, in the name of a package.
			 * 
			 * But, if you prefix with the `^` character the string
			 * that corresponds to a keyword, then it is possible
			 * to obtain a package name with one of its components
			 * equals to a SARL keyword:
			 * 
			 * @filter(.* = '''|'''|.parsesSuccessfully.*) 
			 */
			fact "Why can I put a string in the package name?" {
				'''
					package io.sarl.docs.faq.syntax.^agent
				'''.parseSuccessfully
			}

			/* __Yes and No__.
			 * Indeed, the `val` keyword defines a name
			 * that it could be initialized only once time.
			 * It is similar to the `final` modifier of
			 * the Java language.
			 * 
			 * Consider the example below: two values are
			 * defined, `a` and `b`.
			 * The `a` variable is a real constant because it
			 * has a raw type and it is initialized.
			 * The `b` variable is not a real constant
			 * because it is a reference to an object.
			 * The reference is constant, *but* the
			 * referred object is not. Consequently, it is still
			 * possible to call the setters of `b`. 
			 * 
			 * @filter(.* = '''|'''|.parsesSuccessfully.*) 
			 */
			fact "Is the 'val' keyword defining a constant?" {
				'''
					val a : int = 4
					val b : Object = new Object
				'''.parseSuccessfully(
						"package io.sarl.docs.faq.syntax
						agent A {",
						// TEXT
						"}"
				)
			}

			/* In SARL, the array type may be written
			 * with the classic array syntax, such as
			 * `int[]`, or the object-oriented syntax,
			 * such as `List<Integer>`.
			 * 
			 * SARL considers that the
			 * each array is a list of something.
			 * Consequently, retrieving the values of the array must
			 * be done with `get(int)`.
			 * 
			 * @filter(.* = '''|'''|.parsesSuccessfully.*) 
			 */
			fact "Why cannot use the syntax 'a[0]' for arrays?" {
				'''
					var a : Integer[] = #[1, 2, 3]
					var b : List<Integer> = newArrayList(1, 2, 3)
					
					a.get(0) == b.get(0)
				'''.parseSuccessfully(
						"package io.sarl.docs.faq.syntax
						import java.util.List
						agent A {
							def action : boolean {",
						// TEXT
						"} }"
				)
			}

		}
		
}
