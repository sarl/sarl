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
package io.sarl.lang.tests.parsing

import com.google.inject.Inject
import io.sarl.lang.SARLInjectorProvider
import io.sarl.lang.sarl.SarlScript
import org.eclipse.xtext.diagnostics.Diagnostic
import org.eclipse.xtext.junit4.InjectWith
import org.eclipse.xtext.junit4.XtextRunner
import org.eclipse.xtext.junit4.util.ParseHelper
import org.eclipse.xtext.junit4.validation.ValidationTestHelper
import org.eclipse.xtext.xbase.XbasePackage
import org.eclipse.xtext.xbase.validation.IssueCodes
import org.junit.Test
import org.junit.runner.RunWith

/**
 * @author $Author: srodriguez$
 * @author $Author: sgalland$
 * @version $Name$ $Revision$ $Date$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 */
@RunWith(XtextRunner)
@InjectWith(SARLInjectorProvider)
class SARLMapExtensionsTest {
	
	@Inject
	extension ParseHelper<SarlScript>
	@Inject
	extension ValidationTestHelper

	@Test
	def void operator_addMapPair_0() {
		val mas = '''
			package test
			import java.util.Map
			agent A1 {
				var map : Map<String, Integer>
				var str = "a"
				var num = 4
				
				def myaction0 : Object {
					map += str -> num
				}
			}
		'''.parse
		mas.assertNoErrors
	}

	@Test
	def void operator_addMapPair_1() {
		val mas = '''
			package test
			import java.util.Map
			agent A1 {
				var map : Map<String, Integer>
				var str = "a"
				var num = 4
				
				def myaction1 : Object {
					var p = str -> num
					map += p
				}
			}
		'''.parse
		mas.assertNoErrors
	}

	@Test
	def void operator_addMapMap_0() {
		val mas = '''
			package test
			import java.util.Map
			agent A1 {
				var map1 : Map<String, Number>
				var map2 : Map<String, Integer>
				
				def myaction0 : Object {
					map1 += map2
				}
			}
		'''.parse
		mas.assertNoErrors
	}

	@Test
	def void operator_addMapMap_1() {
		val mas = '''
			package test
			import java.util.Map
			agent A1 {
				var map1 : Map<String, Number>
				var map2 : Map<String, Integer>
				
				def myaction0 : Object {
					map2 += map1
				}
			}
		'''.parse
		mas.assertError(
			XbasePackage::eINSTANCE.XFeatureCall,
			IssueCodes::INCOMPATIBLE_TYPES,
			"Type mismatch: cannot convert from Map<String, Number> to Pair<? extends String, ? extends Integer>"
		)
	}

	@Test
	def void operator_plusMapPair_0() {
		val mas = '''
			package test
			import java.util.Map
			agent A1 {
				var map : Map<String, Integer>
				var r : Map<String, Integer>
				var str = "a"
				var num = 4
				
				def myaction0 : Object {
					r = map + (str -> num)
				}
			}
		'''.parse
		mas.assertNoErrors
	}

	@Test
	def void operator_plusMapPair_1() {
		val mas = '''
			package test
			import java.util.Map
			agent A1 {
				var map : Map<String, Integer>
				var r : Map<String, Integer>
				var str = "a"
				var num = 4
				
				def myaction0 : Object {
					var p = str -> num
					r = map + p
				}
			}
		'''.parse
		mas.assertNoErrors
	}

	@Test
	def void operator_plusPairMap_0() {
		val mas = '''
			package test
			import java.util.Map
			agent A1 {
				var map : Map<String, Integer>
				var r : Map<String, Integer>
				var str = "a"
				var num = 4
				
				def myaction0 : Object {
					r = (str -> num) + map
				}
			}
		'''.parse
		mas.assertNoErrors
	}

	@Test
	def void operator_plusPairMap_1() {
		val mas = '''
			package test
			import java.util.Map
			agent A1 {
				var map : Map<String, Integer>
				var r : Map<String, Integer>
				var str = "a"
				var num = 4
				
				def myaction0 : Object {
					var p = str -> num
					r = p + map
				}
			}
		'''.parse
		mas.assertNoErrors
	}

	@Test
	def void operator_plusMapMap_0() {
		val mas = '''
			package test
			import java.util.Map
			agent A1 {
				var map1 : Map<String, Integer>
				var map2 : Map<String, Integer>
				var r : Map<String, Integer>
				
				def myaction0 : Object {
					r = map1 + map2
				}
			}
		'''.parse
		mas.assertNoErrors
	}

	@Test
	def void operator_plusMapMap_1() {
		val mas = '''
			package test
			import java.util.Map
			agent A1 {
				var map1 : Map<String, Integer>
				var map2 : Map<String, Integer>
				var r : Map<String, Integer>
				
				def myaction0 : Object {
					r = map2 + map1
				}
			}
		'''.parse
		mas.assertNoErrors
	}

	@Test
	def void operator_plusMapMap_2() {
		val mas = '''
			package test
			import java.util.Map
			agent A1 {
				var map1 : Map<String, Integer>
				var map2 : Map<String, Number>
				var r : Map<String, Integer>
				
				def myaction0 : Object {
					r = map1 + map2
				}
			}
		'''.parse
		mas.assertError(
			XbasePackage::eINSTANCE.XFeatureCall,
			IssueCodes::INCOMPATIBLE_TYPES,
			"Type mismatch: cannot convert from Map<String, Number> to Pair<? extends String, ? extends Integer>"
		)
	}

	@Test
	def void operator_plusMapMap_3() {
		val mas = '''
			package test
			import java.util.Map
			agent A1 {
				var map1 : Map<String, Integer>
				var map2 : Map<String, Number>
				var r : Map<String, Number>
				
				def myaction0 : Object {
					r = map2 + map1
				}
			}
		'''.parse
		mas.assertNoErrors
	}

	@Test
	def void operator_plusMapMap_4() {
		val mas = '''
			package test
			import java.util.Map
			agent A1 {
				var map1 : Map<String, Integer>
				var map2 : Map<String, Number>
				var r : Map<String, Integer>
				
				def myaction0 : Object {
					r = map2 + map1
				}
			}
		'''.parse
		mas.assertError(
			XbasePackage::eINSTANCE.XBinaryOperation,
			IssueCodes::INCOMPATIBLE_TYPES,
			"Type mismatch: cannot convert from String to Map<String, Integer>"
		)
	}

	@Test
	def void operator_removeMapK_0() {
		val mas = '''
			package test
			import java.util.Map
			agent A1 {
				var map : Map<String, Integer>
				var r : Map<String, Integer>
				var str = "a"
				var num = 4
				
				def myaction0 : Object {
					map -= str
				}
			}
		'''.parse
		mas.assertNoErrors
	}

	@Test
	def void operator_removeMapK_1() {
		val mas = '''
			package test
			import java.util.Map
			agent A1 {
				var map : Map<String, Integer>
				var r : Map<String, Integer>
				var str = "a"
				var num = 4
				
				def myaction0 : Object {
					map -= num
				}
			}
		'''.parse
		mas.assertError(
			XbasePackage::eINSTANCE.XFeatureCall,
			IssueCodes::INCOMPATIBLE_TYPES,
			"Type mismatch: cannot convert from int to String"
		)
	}

	@Test
	def void operator_minusMapK_0() {
		val mas = '''
			package test
			import java.util.Map
			agent A1 {
				var map : Map<String, Integer>
				var r : Map<String, Integer>
				var str = "a"
				var num = 4
				
				def myaction0 : Object {
					r = map - str
				}
			}
		'''.parse
		mas.assertNoErrors
	}

	@Test
	def void operator_minusMapK_1() {
		val mas = '''
			package test
			import java.util.Map
			agent A1 {
				var map : Map<String, Integer>
				var r : Map<String, Integer>
				var str = "a"
				var num = 4
				
				def myaction0 : Object {
					r = str - map
				}
			}
		'''.parse
		mas.assertError(
			XbasePackage::eINSTANCE.XBinaryOperation,
			Diagnostic::LINKING_DIAGNOSTIC,
			"The method -(Map<String, Integer>) is undefined"
		)
	}

	@Test
	def void operator_minusMapK_2() {
		val mas = '''
			package test
			import java.util.Map
			agent A1 {
				var map : Map<String, Integer>
				var r : Map<String, Integer>
				var str = "a"
				var num = 4
				
				def myaction0 : Object {
					r = map - num
				}
			}
		'''.parse
		mas.assertError(
			XbasePackage::eINSTANCE.XFeatureCall,
			IssueCodes::INCOMPATIBLE_TYPES,
			"Type mismatch: cannot convert from int to String"
		)
	}

	@Test
	def void operator_minusMapK_3() {
		val mas = '''
			package test
			import java.util.Map
			agent A1 {
				var map : Map<String, Integer>
				var r : Map<String, Integer>
				var str = "a"
				var num = 4
				
				def myaction0 : Object {
					r = num - map
				}
			}
		'''.parse
		mas.assertError(
			XbasePackage::eINSTANCE.XBinaryOperation,
			IssueCodes::INCOMPATIBLE_TYPES,
			"Type mismatch: cannot convert from int to Map<String, Integer>"
		)
	}

	@Test
	def void operator_mappedToMapK_0() {
		val mas = '''
			package test
			import java.util.Map
			agent A1 {
				var map : Map<String, Integer>
				var r : Integer
				var str = "a"
				var num = 4
				
				def myaction0 : Object {
					r = map->str
				}
			}
		'''.parse
		mas.assertNoErrors
	}

	@Test
	def void operator_mappedToMapK_1() {
		val mas = '''
			package test
			import java.util.Map
			agent A1 {
				var map : Map<String, Integer>
				var r : Integer
				var str = "a"
				var num = 4
				
				def myaction0 : Object {
					r = map->num
				}
			}
		'''.parse
		mas.assertError(
			XbasePackage::eINSTANCE.XBinaryOperation,
			IssueCodes::INCOMPATIBLE_TYPES,
			"Type mismatch: cannot convert from Pair<Map<String, Integer>, Integer> to Integer"
		)
	}

}
