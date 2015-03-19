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
package io.sarl.lang.tests.genmodel;

import static org.junit.Assert.assertEquals;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;
import io.sarl.lang.SARLInjectorProvider;
import io.sarl.lang.genmodel.GeneratedCode;
import io.sarl.lang.genmodel.SARLCodeGenerator;
import io.sarl.lang.sarl.Agent;
import io.sarl.tests.api.AbstractSarlTest;

import org.eclipse.emf.common.util.BasicEList;
import org.eclipse.emf.ecore.resource.Resource;
import org.eclipse.xtext.junit4.InjectWith;
import org.eclipse.xtext.junit4.XtextRunner;
import org.eclipse.xtext.serializer.ISerializer;
import org.eclipse.xtext.xbase.XBlockExpression;
import org.eclipse.xtext.xbase.XbaseFactory;
import org.junit.Test;
import org.junit.runner.RunWith;

import com.google.inject.Inject;

/**
 * @author $Author: sgalland$
 * @version $Name$ $Revision$ $Date$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 */
@SuppressWarnings("all")
public class SARLHiddenTokenSequencerTest extends AbstractSarlTest {
	
	@Inject
	private ISerializer serializer;

	@Inject
	private SARLCodeGenerator generator;

	@Test
	public void prefixComment() {
		Resource resource = mock(Resource.class);
		when(resource.getContents()).thenReturn(new BasicEList());
		GeneratedCode code = generator.createScript(resource, "io.sarl.lang.tests");
		Agent agent = generator.createAgent(code, "MyAgent", null);
		//
		generator.attachComment(code, agent, "/* my comment\n*/");
		//
		String text = serializer.serialize(code.getSarlScript());
		assertEquals(
			"package io.sarl.lang.tests\n\n/* my comment\n*/\nagent MyAgent {\n}",
			text);
	}

	@Test
	public void postfixComment() {
		Resource resource = mock(Resource.class);
		when(resource.getContents()).thenReturn(new BasicEList());
		GeneratedCode code = generator.createScript(resource, "io.sarl.lang.tests");
		Agent agent = generator.createAgent(code, "MyAgent", null);
		//
		generator.attachPostComment(code, agent, "/* my comment\n*/");
		//
		String text = serializer.serialize(code.getSarlScript());
		assertEquals(
			"package io.sarl.lang.tests\n\nagent MyAgent {\n}\n\n/* my comment\n*/",
			text);
	}

	@Test
	public void innerBlockComment() {
		Resource resource = mock(Resource.class);
		when(resource.getContents()).thenReturn(new BasicEList());
		GeneratedCode code = generator.createScript(resource, "io.sarl.lang.tests");
		Agent agent = generator.createAgent(code, "MyAgent", null);
		XBlockExpression block = XbaseFactory.eINSTANCE.createXBlockExpression();
		generator.createAction(code, agent, "myFct", null, block);
		//
		generator.attachInnerComment(code, block, "/* my comment\n*/");
		//
		String text = serializer.serialize(code.getSarlScript());
		assertEquals(
			"package io.sarl.lang.tests\n\nagent MyAgent {\n\n	def myFct {\n	\n/* my comment\n*/\n	}\n}",
			text);
	}

}
