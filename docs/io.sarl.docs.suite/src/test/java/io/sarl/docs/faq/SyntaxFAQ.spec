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
package io.sarl.docs.faq

import com.google.inject.Inject
import io.sarl.docs.utils.SARLParser
import io.sarl.docs.utils.SARLSpecCreator
import org.jnario.runner.CreateWith

import static extension io.sarl.docs.utils.SpecificationTools.*
import static extension org.junit.Assume.assumeFalse

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
			 * to zero.  Consequently: <ul>
			 * <li>`123.0` is correct; </li>
			 * <li>`123.` is incorrect; </li>
			 * <li>`0.123` is correct; </li>
			 * <li>`.123` is incorrect; </li>
			 * </ul>
			 * 
			 * @filter(.*) 
			 */
			fact "Can I use the same syntax as in Java for number literals?" {
				'''		package io.sarl.docs.faq.syntax
						agent A {
							def action : double {
								var a = 123.0
								return a
							}
						}
				'''.parseSuccessfully

				'''		package io.sarl.docs.faq.syntax
						agent A {
							def action : double {
								var a = 0.123
								return a
							}
						}
				'''.parseSuccessfully

				'''		package io.sarl.docs.faq.syntax
						agent A {
							def action : double {
								var a = 123.
								return a
							}
						}
				'''.parseWithError

				'''		package io.sarl.docs.faq.syntax
						agent A {
							def action : double {
								var a = .123
								return a
							}
						}
				'''.parseWithError
			}
			
			/* It is not allowed to put a SARL keyword, such as
			 * `agent`, in the name of a package.
			 * 
			 * <p>But, if you prefix with the `^` character the string
			 * that corresponds to a keyword, then it is possible
			 * to obtain a package name with one of its components
			 * equals to a SARL keyword:
			 * 
			 * @filter(.* = '''|'''|.parseSuccessfully.*) 
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
			 * <p>Consider the example below: two values are
			 * defined, `a` and `b`.
			 * The `a` variable is a real constant because it
			 * has a raw type and it is initialized.
			 * The `b` variable is not a real constant
			 * because it is a reference to an object.
			 * The reference is constant, *but* the
			 * referred object is not. Consequently, it is still
			 * possible to call the setters of `b`. 
			 * 
			 * @filter(.* = '''|'''|.parseSuccessfully.*) 
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
			 * <p>SARL considers that the
			 * each array is a list of something.
			 * Consequently, retrieving the values of the array must
			 * be done with `get(int)`.
			 * 
			 * @filter(.* = '''|'''|.parseSuccessfully.*) 
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

			/* In SARL, the empty generic parameter list, written <code>&lt,&gt;</code> is
			 * not supported: a generic type expression must be written between them.
			 * 
			 * <p>For solving this problem, two choices: i) add a type expression between
			 * <code>&lt;</code> and <code>&gt;</code>; ii) remove the generic parameter
			 * list.
			 *
			 * <pre><code>
			 * var firstSolution : List<Integer> = new ArrayList<Integer>()
			 * var secondSolution : List<Integer> = new ArrayList()
			 * </code></pre>
			 * 
			 * @filter(.*) 
			 */
			fact "Why can I not use the '&lt;&gt;' notation for generic parameters?" {
				'''
					package io.sarl.docs.faq.syntax
					import java.util.List
					import java.util.ArrayList
					agent A {
						var a : List<Integer> = new ArrayList<>()
					}
				'''.parseWithError
			}

			/* In SARL, the creation of anonymous classes (interface implementation, etc.)
			 * must be done with a closure.
			 *
			 * <p>Consider the definition of the following interface:
			 * 
			 *      interface MyInterface {
			 *           def myfunction(parameter : Object) : void
			 *      }
			 * 
			 * The on-the-fly definition and instantiation of an instance of this interface,
			 * a.k.a. anonymous class definition in the Java community, could be written is SARL
			 * with the following closure:
			 * 
			 * @filter(.* = '''|'''|.parseSuccessfully.* 
			 */
			fact "How can I create instances of anonymous classes?" {
				'''
					var instance : MyInterface
					instance = [ parameter | /* The code of myfunction() */ ]
				'''.parseSuccessfully(
						"package io.sarl.docs.faq.syntax
						import java.util.List
			 			interface MyInterface {
			 				def myfunction(parameter : Object) : void
			 			}
						agent A {
							def action : void {",
						// TEXT
						"} }"
				)
			}

			/* In SARL, the creation of anonymous classes (interface implementation, etc.)
			 * must be done with a closure (see previous question).
			 *
			 * <p>The Java-based syntax for defining an anonymous class's instance if totally forbidden
			 * in the SARL language. It means that the following code generates a syntax error:
			 * 
			 * @filter(.* = '''|'''|.parseWithError.* 
			 */
			fact "Java syntax for anonymous classes is forbidden" {
				'''
					var instance = new MyInterface() {
							def myfunction(parameter : Object) {
								/* The code of myfunction() */
							}
					}
				'''.parseWithError(
						"package io.sarl.docs.faq.syntax
						import java.util.List
			 			interface MyInterface {
			 				def myfunction(parameter : Object)
			 			}
						agent A {
							def action : boolean {",
						// TEXT
						"} }"
				)
			}

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
