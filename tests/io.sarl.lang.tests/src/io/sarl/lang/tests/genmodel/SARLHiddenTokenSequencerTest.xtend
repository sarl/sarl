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
package io.sarl.lang.tests.genmodel

import com.google.inject.Inject
import io.sarl.lang.SARLInjectorProvider
import io.sarl.lang.genmodel.SARLCodeGenerator
import io.sarl.tests.api.AbstractSarlTest
import org.eclipse.emf.ecore.resource.Resource
import org.eclipse.xtext.junit4.InjectWith
import org.eclipse.xtext.junit4.XtextRunner
import org.eclipse.xtext.serializer.ISerializer
import org.junit.Test
import org.junit.runner.RunWith

import static org.junit.Assert.*
import static org.mockito.Mockito.*
import org.eclipse.emf.common.util.BasicEList
import org.eclipse.xtext.xbase.XbaseFactory

/**
 * @author $Author: sgalland$
 * @version $Name$ $Revision$ $Date$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 */
@RunWith(XtextRunner)
@InjectWith(SARLInjectorProvider)
class SARLHiddenTokenSequencerTest extends AbstractSarlTest {
	
	@Inject
	private var ISerializer serializer

	@Inject
	private var SARLCodeGenerator generator

	@Test
	def void prefixComment() {
		var resource = mock(typeof(Resource))
		when(resource.contents).thenReturn(new BasicEList)
		var code = generator.createScript(resource, "io.sarl.lang.tests")
		var agent = generator.createAgent(code, "MyAgent", null)
		//
		generator.attachComment(code, agent, "/* my comment\n*/")
		//
		var text = serializer.serialize(code.sarlScript)
		assertEquals(
			"package io.sarl.lang.tests\n\n/* my comment\n*/\nagent MyAgent {\n}",
			text)
	}

	@Test
	def void postfixComment() {
		var resource = mock(typeof(Resource))
		when(resource.contents).thenReturn(new BasicEList)
		var code = generator.createScript(resource, "io.sarl.lang.tests")
		var agent = generator.createAgent(code, "MyAgent", null)
		//
		generator.attachPostComment(code, agent, "/* my comment\n*/")
		//
		var text = serializer.serialize(code.sarlScript)
		assertEquals(
			"package io.sarl.lang.tests\n\nagent MyAgent {\n}\n\n/* my comment\n*/",
			text)
	}

	@Test
	def void innerBlockComment() {
		var resource = mock(typeof(Resource))
		when(resource.contents).thenReturn(new BasicEList)
		var code = generator.createScript(resource, "io.sarl.lang.tests")
		var agent = generator.createAgent(code, "MyAgent", null)
		var block = XbaseFactory::eINSTANCE.createXBlockExpression
		generator.createAction(code, agent, "myFct", null, block)
		//
		generator.attachInnerComment(code, block, "/* my comment\n*/")
		//
		var text = serializer.serialize(code.sarlScript)
		assertEquals(
			"package io.sarl.lang.tests\n\nagent MyAgent {\n\n	def myFct {\n	\n/* my comment\n*/\n	}\n}",
			text)
	}

}
