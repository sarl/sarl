/*
 * Copyright (C) 2014-2015 Sebastian RODRIGUEZ, Nicolas GAUD, Stéphane GALLAND.
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
package io.sarl.eclipse.tests.bugs;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNotNull;
import io.sarl.eclipse.util.Jdt2Ecore;
import io.sarl.lang.actionprototype.ActionParameterTypes;
import io.sarl.lang.actionprototype.ActionPrototype;
import io.sarl.lang.bugfixes.SARLContextPDAProvider;
import io.sarl.lang.generator.helper.ECoreGeneratorHelper;
import io.sarl.lang.generator.helper.SarlEcoreCode;
import io.sarl.lang.sarl.SarlSkill;
import io.sarl.tests.api.AbstractSarlUiTest;
import io.sarl.tests.api.TestClasspath;

import java.util.Arrays;
import java.util.Map;

import org.eclipse.core.resources.IFile;
import org.eclipse.emf.ecore.resource.Resource;
import org.eclipse.jdt.core.IMethod;
import org.eclipse.xtend.core.xtend.XtendFile;
import org.eclipse.xtext.serializer.ISerializer;
import org.junit.Test;

import com.google.common.collect.Maps;
import com.google.inject.Inject;

/**
 * @author $Author: sgalland$
 * @version $Name$ $Revision$ $Date$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 */
@SuppressWarnings("all")
public class Bug277 extends AbstractSarlUiTest {

	@Inject
	private SARLContextPDAProvider pdaProvider;

	@Inject
	private ISerializer serializer;

	@Inject
	private ECoreGeneratorHelper sarlGenerator;

	@Test
	public void testBugFixExist() {
		assertNotNull(pdaProvider);
	}

	/**
	 * @throws Exception
	 * @see <a href="https://github.com/sarl/sarl/issues/277">issue #277</a>
	 */
	@Test
	public void multipleCapacityImplementation_0() throws Exception {
		XtendFile script = helper().sarlFile("SARLContextPDAProviderTest0",
				"package io.sarl.lang.tests.genmodel.serializer\n"
						+"capacity C1 {}\n"
						+"capacity C2 {}\n"
						+"skill S1 implements C1, C2 {}");
		//
		String text = serializer.serialize(script);
		//
		assertEquals(
				"package io.sarl.lang.tests.genmodel.serializer\n"
						+"capacity C1 {}\n"
						+"capacity C2 {}\n"
						+"skill S1 implements C1, C2 {}",
						text);
	}

	/**
	 * @throws Exception
	 * @see <a href="https://github.com/sarl/sarl/issues/277">issue #277</a>
	 */
	@Test
	@TestClasspath("io.sarl.core")
	public void multipleCapacityImplementation_1() throws Exception {
		XtendFile script = helper().sarlFile("SARLContextPDAProviderTest1",
				"package io.sarl.lang.tests.genmodel.serializer\n"
						+"import io.sarl.core.Lifecycle\n"
						+"import io.sarl.core.Schedules\n"
						+"skill S1 implements Lifecycle, Schedules {}");
		//
		String text = serializer.serialize(script);
		//
		assertEquals(
				"package io.sarl.lang.tests.genmodel.serializer\n"
						+"import io.sarl.core.Lifecycle\n"
						+"import io.sarl.core.Schedules\n"
						+"skill S1 implements Lifecycle, Schedules {}",
						text);
	}

	@Test
	@TestClasspath("io.sarl.core")
	public void similarToWizardCreation() throws Exception {
		helper().sarlFile(
				"Bug277_0", "package io.sarl.lang.tests.bug277\ncapacity MyCapacity { }");
		helper().awaitAutoBuild();

		IFile file = helper().createFile("Bug277_1", "package io.sarl.lang.tests.bug277");
		Resource resource = helper().getResourceFor(file);
		SarlEcoreCode code = this.sarlGenerator.createScript(resource, "io.sarl.lang.tests.bug277");
		SarlSkill skill = this.sarlGenerator.createSkill(code, "MyS", null,
				Arrays.asList("io.sarl.core.Lifecycle", "io.sarl.lang.tests.bug277.MyCapacity"));
		this.sarlGenerator.attachComment(code, skill, "My Test");

		Map<ActionPrototype, IMethod> operationsToImplement = Maps.newHashMap();
		Map<ActionParameterTypes, IMethod> constructors = Maps.newHashMap();

		Jdt2Ecore.populateInheritanceContext(
				Jdt2Ecore.toTypeFinder(helper().getJavaProject()),
				// Discarding final operation
				null,
				// Discarding overridable operation
				null,
				// Discarding inherited fields,
				null,
				operationsToImplement,
				constructors,
				code.getCodeGenerator().getActionSignatureProvider(),
				null,
				Arrays.asList("io.sarl.core.Lifecycle", "io.sarl.lang.tests.bug277.MyCapacity"));

		Jdt2Ecore.createStandardConstructors(code, constructors.values(), skill);
		Jdt2Ecore.createActions(code, operationsToImplement.values(), skill);

		code.finalizeScript();

		//
		resource.save(null);
		String text = helper().getContents(file);
		//
		assertEquals(
				"package io.sarl.lang.tests.bug277\n"
				+ "\n"
				+ "import io.sarl.core.Lifecycle\n"
				+ "import io.sarl.lang.core.AgentContext\n"
				+ "import io.sarl.lang.tests.bug277.MyCapacity\n"
				+ "import java.util.UUID\n"
				+ "// My Test\n"
				+ "skill MyS implements Lifecycle , MyCapacity {\n"
				+ "\n"
				+ "	def spawnInContextWithID(arg0 : Object, arg1 : UUID, arg2 : AgentContext, arg3 : Object *) : UUID {\n"
				+ "	// TODO Auto-generated action.\n"
				+ "		null\n"
				+ "	}\n"
				+ "\n"
				+ "	def killMe {\n"
				+ "	// TODO Auto-generated action.\n"
				+ "\n"
				+ "	}\n"
				+ "\n"
				+ "	def spawnInContext(arg0 : Object, arg1 : AgentContext, arg2 : Object *) : UUID {\n"
				+ "	// TODO Auto-generated action.\n"
				+ "		null\n"
				+ "	}\n"
				+ "}",
				text);
	}

}
