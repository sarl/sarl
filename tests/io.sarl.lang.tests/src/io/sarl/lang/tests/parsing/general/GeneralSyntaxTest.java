/*
 * Copyright (C) 2014-2016 the original authors or authors.
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
package io.sarl.lang.tests.parsing.general;

import io.sarl.lang.sarl.SarlScript;
import io.sarl.tests.api.AbstractSarlTest;

import org.eclipse.xtext.xbase.validation.IssueCodes;
import org.eclipse.xtext.xtype.XtypePackage;
import org.junit.Test;


/**
 * @author $Author: sgalland$
 * @version $Name$ $Revision$ $Date$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 */
@SuppressWarnings("all")
public class GeneralSyntaxTest extends AbstractSarlTest {

	@Test
	public void wildCardImports() throws Exception {
		SarlScript mas = file(multilineString(
				"package io.sarl.docs.reference.gsr",
				"import org.eclipse.xtext.xbase.lib.Procedures.*",
				"agent A {",
				"}",
				""));
		validate(mas).assertWarning(
				XtypePackage.eINSTANCE.getXImportDeclaration(),
				IssueCodes.IMPORT_WILDCARD_DEPRECATED,
				35, 47,
				"The use of wildcard imports is deprecated");
	}

	@Test
	public void noParamNoReturnActionInClass() throws Exception {
		SarlScript mas = file(multilineString(
				"abstract class Light {",
				"	abstract def turnOn",
				"	abstract def turnOff",
				"}",
				""));
		validate(mas).assertNoIssues();
	}

	@Test
	public void noParamNoReturnActionInInterface() throws Exception {
		SarlScript mas = file(multilineString(
				"interface Light {",
				"	abstract def turnOn",
				"	abstract def turnOff",
				"}",
				""));
		validate(mas).assertNoIssues();
	}
	
}
