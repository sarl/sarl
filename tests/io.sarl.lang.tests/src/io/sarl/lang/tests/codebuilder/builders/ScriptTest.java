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
import org.eclipse.emf.ecore.resource.Resource;
import org.eclipse.xtext.junit4.util.ResourceHelper;
import org.eclipse.xtext.resource.FileExtensionProvider;
import org.eclipse.xtext.serializer.ISerializer;
import org.junit.Test;

import io.sarl.lang.codebuilder.CodeBuilderFactory;
import io.sarl.lang.codebuilder.builders.IActionBuilder;
import io.sarl.lang.codebuilder.builders.IAgentBuilder;
import io.sarl.lang.codebuilder.builders.IFieldBuilder;
import io.sarl.lang.codebuilder.builders.IScriptBuilder;
import io.sarl.tests.api.AbstractSarlTest;
import io.sarl.tests.api.AbstractSarlUiTest;

/**
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 */
@SuppressWarnings("all")
public class ScriptTest extends AbstractCodeBuilderTest {

	@Test
	public void field() throws Exception {
		String expected = multilineString(
				"package io.sarl.foo",
				"",
				"import java.io.IOException",
				"",
				"agent Myagent {",
				"	var myField : IOException",
				"}",
				"");
		IScriptBuilder builder = factory.createScript("io.sarl.foo", this.resourceHelper.resource(""));
		assertNotNull(builder.getScript());
		IAgentBuilder abuilder = builder.addAgent("Myagent");
		IFieldBuilder fbuilder = abuilder.addField("myField");
		fbuilder.setType("java.io.IOException");
		builder.finalizeScript();
		assertFormatted(builder.getScript(), expected);
	}

	@Test
	public void action_nobody() throws Exception {
		String expected = multilineString(
				"package io.sarl.foo",
				"",
				"import java.io.IOException",
				"",
				"agent Myagent {",
				"	def myAction : IOException",
				"}",
				"");
		IScriptBuilder builder = factory.createScript("io.sarl.foo", this.resourceHelper.resource(""));
		assertNotNull(builder.getScript());
		IAgentBuilder abuilder = builder.addAgent("Myagent");
		IActionBuilder tbuilder = abuilder.addAction("myAction");
		tbuilder.setReturnType("java.io.IOException");
		builder.finalizeScript();
		assertFormatted(builder.getScript(), expected);
	}

}