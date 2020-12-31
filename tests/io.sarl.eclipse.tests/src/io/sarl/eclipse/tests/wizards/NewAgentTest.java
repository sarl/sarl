/*
 * $Id$
 *
 * SARL is an general-purpose agent programming language.
 * More details on http://www.sarl.io
 *
 * Copyright (C) 2014-2021 the original authors or authors.
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
package io.sarl.eclipse.tests.wizards;

import java.io.ByteArrayOutputStream;
import java.util.Collections;
import java.util.Map;
import java.util.UUID;
import javax.inject.Inject;

import org.eclipse.core.resources.IFile;
import org.eclipse.emf.ecore.resource.Resource;
import org.eclipse.jdt.core.IMethod;
import org.eclipse.xtext.util.EmfFormatter;
import org.junit.Before;
import org.junit.Ignore;
import org.junit.Test;

import io.sarl.eclipse.util.Jdt2Ecore;
import io.sarl.lang.actionprototype.ActionPrototype;
import io.sarl.lang.codebuilder.CodeBuilderFactory;
import io.sarl.lang.codebuilder.builders.ISarlAgentBuilder;
import io.sarl.lang.codebuilder.builders.IScriptBuilder;
import io.sarl.tests.api.AbstractSarlUiTest;

/**
 * @author $Author: ngaud$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 */
@SuppressWarnings("all")
public class NewAgentTest extends AbstractSarlUiTest {

	@Inject
	private CodeBuilderFactory codeBuilderFactory;
	
	@Inject
	protected Jdt2Ecore jdt2sarl;

	private Resource resource;
	
	@Before
	public void setUp() throws Exception {
		IFile file = helper().createFile("test_" + UUID.randomUUID() + ".sarl", "");
		this.resource = helper().getResourceFor(file);
		
	}

	@Test
	@Ignore("only for debugging purpose")
	public void xtextParsing() throws Exception {
		IFile file = helper().createFile("test_" + UUID.randomUUID() + ".sarl", multilineString(
				"package io.sarl.eclipse.tests.wizards",
				"agent FooAgent0 { }"));
		Resource resource = helper().getResourceFor(file);
		resource.load(null);
		System.out.println(EmfFormatter.objToStr(resource.getContents().get(0)));
	}

	@Test
	public void newAgentSerialization() throws Exception {
		IScriptBuilder scriptBuilder = this.codeBuilderFactory.createScript(
				"io.sarl.eclipse.tests.wizards", this.resource);
		ISarlAgentBuilder agent = scriptBuilder.addSarlAgent("FooAgent");
		agent.setExtends("io.sarl.lang.core.Agent");
		agent.setDocumentation("Foo Agent");

		final Map<ActionPrototype, IMethod> operationsToImplement;

		this.jdt2sarl.populateInheritanceContext(
				this.jdt2sarl.toTypeFinder(helper().getJavaProject()),
				// Discarding final operation.
				null,
				// Discarding overridable operation.
				null,
				// Discarding inherited fields,
				null,
				// Discarding inherited operation.
				null,
				// Discarding super constructors,
				null,
				"io.sarl.lang.core.Agent",
				Collections.<String>emptyList());

		scriptBuilder.finalizeScript();
		
		//System.out.println(EmfFormatter.objToStr(scriptBuilder.getScript()));
		
		String content;
		try (ByteArrayOutputStream baos = new ByteArrayOutputStream()) {
			this.resource.save(baos, null);
			content = baos.toString();
		}
		
		assertEquals(multilineString(
				"package io.sarl.eclipse.tests.wizards",
				"/* Foo Agent",
				" */",
				"agent FooAgent {",
				"}",
				""), content);
	}

}
