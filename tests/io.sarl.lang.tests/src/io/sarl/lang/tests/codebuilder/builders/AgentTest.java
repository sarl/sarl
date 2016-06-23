/*
 * $Id$
 *
 * SARL is an general-purpose agent programming language.
 * More details on http://www.sarl.io
 *
 * Copyright (C) 2015 Sebastian RODRIGUEZ, Nicolas GAUD, Stéphane GALLAND.
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
package io.sarl.lang.tests.codebuilder.builders;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNotNull;

import com.google.inject.Inject;
import foo.ecore.SubAgent;
import org.eclipse.emf.ecore.EObject;
import org.eclipse.emf.ecore.resource.Resource;
import org.eclipse.xtext.common.types.util.TypeReferences;
import org.eclipse.xtext.junit4.util.ResourceHelper;
import org.eclipse.xtext.resource.XtextResource;
import org.eclipse.xtext.xbase.typesystem.util.CommonTypeComputationServices;
import org.junit.Before;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.junit.runners.Suite;
import org.junit.runners.Suite.SuiteClasses;

import io.sarl.lang.codebuilder.CodeBuilderFactory;
import io.sarl.lang.codebuilder.builders.IAgentBuilder;
import io.sarl.lang.codebuilder.builders.IFieldBuilder;
import io.sarl.lang.core.Agent;
import io.sarl.lang.formatting2.FormatterFacade;
import io.sarl.lang.sarl.SarlAgent;
import io.sarl.lang.sarl.SarlScript;
import io.sarl.tests.api.AbstractSarlTest;
import io.sarl.tests.api.AbstractSarlUiTest;

/**
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 */
@RunWith(Suite.class)
@SuiteClasses({
	AgentTest.TypeTest.class,
	AgentTest.FieldTest.class,
})
@SuppressWarnings("all")
public class AgentTest {

	public static class TypeTest extends AbstractCodeBuilderTest {

		@Test
		public void documentationReferenceTest_0() throws Exception {
			String input = "/*Top comment*/agent Myagent {}";
			String expected = multilineString(
					"/* Top comment",
					" */",
					"agent Myagent {",
					"}",
					"");
			SarlScript script = file(input, true);
			assertFormatted(script, expected);
		}

		@Test
		public void documentationReferenceTest_1() throws Exception {
			String input = "/*Top comment*/agent Myagent {}";
			String expected = multilineString(
					"/* Top comment",
					" */",
					"agent Myagent {",
					"}");
			SarlAgent agent = agent(input, true);
			assertFormatted(agent, expected);
		}

		@Test
		public void addSLDocumentation() throws Exception {
			String expected = multilineString(
					"/* Top comment",
					" */",
					"agent Myagent {",
					"}");
			IAgentBuilder builder = factory.createAgent("Myagent", this.resourceHelper.resource(""));
			assertNotNull(builder.getSarlAgent());
			builder.setDocumentation("Top comment");
			assertFormatted(builder.getSarlAgent(), expected);
		}

		@Test
		public void addMLDocumentation() throws Exception {
			String expected = multilineString(
					"/* Top comment.",
					" * Second line.",
					" * Third line.",
					" */",
					"agent Myagent {",
					"}");
			IAgentBuilder builder = factory.createAgent("Myagent", this.resourceHelper.resource(""));
			assertNotNull(builder.getSarlAgent());
			builder.setDocumentation("Top comment.\nSecond line.\n    Third line.");
			assertFormatted(builder.getSarlAgent(), expected);
		}

		@Test
		public void extension_noAgentType() throws Exception {
			String expected = multilineString(
					"agent Myagent {",
					"}");
			IAgentBuilder builder = factory.createAgent("Myagent", this.resourceHelper.resource(""));
			assertNotNull(builder.getSarlAgent());
			builder.setExtends(Object.class.getName());
			assertFormatted(builder.getSarlAgent(), expected);
		}

		@Test
		public void extension_implicitType() throws Exception {
			String expected = multilineString(
					"agent Myagent {",
					"}");
			IAgentBuilder builder = factory.createAgent("Myagent", this.resourceHelper.resource(""));
			assertNotNull(builder.getSarlAgent());
			builder.setExtends(this.typeReferences.findDeclaredType(Agent.class, builder.eResource()).getQualifiedName());
			assertFormatted(builder.getSarlAgent(), expected);
		}

		@Test
		public void extension_explicitType() throws Exception {
			String expected = multilineString(
					"agent Myagent extends SubAgent {",
					"}");
			Resource resource1 = this.resourceHelper.resource("agent SubAgent { }");
			Resource resource2 = this.resourceHelper.resource("", resource1.getResourceSet());
			IAgentBuilder builder = factory.createAgent("Myagent", resource2);
			assertNotNull(builder.getSarlAgent());
			builder.setExtends("SubAgent");
			assertFormatted(builder.getSarlAgent(), expected);
		}

	}

	public static class FieldTest extends AbstractCodeBuilderTest {

		private IAgentBuilder agentBuilder;
		
		private IFieldBuilder fieldBuilder;

		@Before
		public void setUp() throws Exception {
			this.agentBuilder = this.factory.createAgent("Myagent", this.resourceHelper.resource(""));
			assertNotNull(this.agentBuilder.getSarlAgent());
			this.fieldBuilder = this.agentBuilder.addField("myField");
		}
		
		@Test
		public void setPrimitiveType() throws Exception {
			String expected = multilineString(
					"agent Myagent {",
					"\tvar myField : int",
					"}");
			this.fieldBuilder.setType("int");
			assertFormatted(this.agentBuilder.getSarlAgent(), expected);
		}

		@Test
		public void setObjectType() throws Exception {
			String expected = multilineString(
					"agent Myagent {",
					"\tvar myField : java.io.IOException",
					"}");
			this.fieldBuilder.setType("java.io.IOException");
			assertFormatted(this.agentBuilder.getSarlAgent(), expected);
		}

		@Test
		public void setInitialValueFromString() throws Exception {
			String expected = multilineString(
					"agent Myagent {",
					"\tvar myField = 1+2",
					"}");
			this.fieldBuilder.getInitialValue().setExpression("1+2");
			assertFormatted(this.agentBuilder.getSarlAgent(), expected);
		}

		@Test
		public void addSLDocumentation() throws Exception {
			String expected = multilineString(
					"agent Myagent {",
					"\t/* Top comment",
					"\t */",
					"\tvar myField : int",
					"}");
			this.fieldBuilder.setType("int");
			this.fieldBuilder.setDocumentation("Top comment");
			assertFormatted(this.agentBuilder.getSarlAgent(), expected);
		}

		@Test
		public void addMLDocumentation() throws Exception {
			String expected = multilineString(
					"agent Myagent {",
					"\t/* Top comment.",
					"\t * Second line.",
					"\t * Third line.",
					"\t */",
					"\tvar myField : int",
					"}");
			this.fieldBuilder.setType("int");
			this.fieldBuilder.setDocumentation("Top comment.\nSecond line.\n    Third line.");
			assertFormatted(this.agentBuilder.getSarlAgent(), expected);
		}
	}

}