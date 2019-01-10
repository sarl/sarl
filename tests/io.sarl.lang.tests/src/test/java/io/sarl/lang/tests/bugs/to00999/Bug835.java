/*
 * $Id$
 *
 * SARL is an general-purpose agent programming language.
 * More details on http://www.sarl.io
 *
 * Copyright (C) 2014-2019 the original authors or authors.
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

package io.sarl.lang.tests.bugs.to00999;

import static org.junit.Assert.*;

import java.util.ArrayList;

import com.google.inject.Inject;
import org.eclipse.emf.ecore.EObject;
import org.eclipse.jdt.core.dom.Type;
import org.eclipse.xtend.core.xtend.XtendClass;
import org.eclipse.xtext.common.types.JvmTypeReference;
import org.eclipse.xtext.common.types.TypesPackage;
import org.eclipse.xtext.testing.util.ParseHelper;
import org.eclipse.xtext.xbase.XbasePackage;
import org.eclipse.xtext.xbase.testing.CompilationTestHelper;
import org.eclipse.xtext.xbase.typesystem.util.CommonTypeComputationServices;
import org.eclipse.xtext.xbase.validation.UIStrings;
import org.eclipse.xtext.xtype.XtypePackage;
import org.junit.Test;

import io.sarl.lang.SARLVersion;
import io.sarl.lang.annotation.SarlSpecification;
import io.sarl.lang.sarl.SarlField;
import io.sarl.lang.sarl.SarlPackage;
import io.sarl.lang.sarl.SarlScript;
import io.sarl.lang.util.Utils;
import io.sarl.lang.validation.IssueCodes;
import io.sarl.tests.api.AbstractSarlTest;
import io.sarl.tests.api.AbstractSarlTest.Validator;

/** Testing class for issue: Compiler accepts "uses <SkillName>" the same as "uses <CapacityName>".
 *
 * <p>https://github.com/sarl/sarl/issues/835
 *
 * @author $Author: sgalland$
 * @version $Name$ $Revision$ $Date$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 * @see "https://github.com/sarl/sarl/issues/835"
 */
@SuppressWarnings("all")
public class Bug835 extends AbstractSarlTest {

	private static final String SNIPSET01 = multilineString(
			"package ^skill.^capacity.bug",
			"import io.sarl.core.Initialize",
			"capacity SomeCapacity{",
			"	def someFunction",
			"}",
			"skill SomeSkill implements SomeCapacity{",
			"	def someFunction{",
			"	}",
			"}",
			"agent SomeAgent{",
			"	uses SomeSkill",
			"   on Initialize {",
			"       someFunction",
			"   }",
			"}");

	private static final String SNIPSET02 = multilineString(
			"package ^skill.^capacity.bug",
			"import io.sarl.core.Initialize",
			"capacity SomeCapacity{",
			"	def someFunction",
			"}",
			"skill SomeSkill implements SomeCapacity{",
			"	def someFunction{",
			"	}",
			"}",
			"agent SomeAgent{",
			"	uses SomeCapacity",	
			"   on Initialize {",
			"       someFunction",
			"   }",
			"}");

	@Test
	public void parsing_01() throws Exception {
		SarlScript mas = file(SNIPSET01);
		final Validator validator = validate(mas);
		validator.assertError(
				TypesPackage.eINSTANCE.getJvmParameterizedTypeReference(),
				IssueCodes.INVALID_CAPACITY_TYPE,
				"Only capacities can be used after the keyword 'uses'");
	}

	@Test
	public void parsing_02() throws Exception {
		SarlScript mas = file(SNIPSET02);
		final Validator validator = validate(mas);
		validator.assertNoIssues();
	}

}

