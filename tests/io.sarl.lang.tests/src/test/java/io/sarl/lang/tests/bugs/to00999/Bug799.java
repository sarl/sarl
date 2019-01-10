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
import org.eclipse.xtext.testing.util.ParseHelper;
import org.eclipse.xtext.xbase.XbasePackage;
import org.eclipse.xtext.xbase.testing.CompilationTestHelper;
import org.eclipse.xtext.xbase.typesystem.util.CommonTypeComputationServices;
import org.eclipse.xtext.xbase.validation.IssueCodes;
import org.eclipse.xtext.xbase.validation.UIStrings;
import org.junit.Test;

import io.sarl.lang.SARLVersion;
import io.sarl.lang.sarl.SarlField;
import io.sarl.lang.sarl.SarlPackage;
import io.sarl.lang.sarl.SarlScript;
import io.sarl.lang.util.Utils;
import io.sarl.tests.api.AbstractSarlTest;
import io.sarl.tests.api.AbstractSarlTest.Validator;

/** Testing class for issue: Invalid return type with the Maven compiler
 *
 * <p>https://github.com/sarl/sarl/issues/799
 *
 * @author $Author: sgalland$
 * @version $Name$ $Revision$ $Date$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 * @see "https://github.com/sarl/sarl/issues/799"
 */
@SuppressWarnings("all")
public class Bug799 extends AbstractSarlTest {


	private static final String SNIPSET01 = multilineString(
			"package foo.bug799",
			"skill A implements Cap {",
			"  def act_connectToSimulator(address : String, port : int)  {",
			"    while(true) {}",
			"  }",
			"}",
			"skill B implements Cap {",
			"  def act_connectToSimulator(address : String, port : int)  {",
			"    while(true) {}",
			"  }",
			"}");
	
	private static final String EXPECTED01a = multilineString(
			"package foo.bug799;",
			"",
			"import foo.bug799.Cap;",
			"import io.sarl.lang.annotation.SarlElementType;",
			"import io.sarl.lang.annotation.SarlSpecification;",
			"import io.sarl.lang.annotation.SyntheticMember;",
			"import io.sarl.lang.core.Agent;",
			"import io.sarl.lang.core.Skill;",
			"",
			"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
			"@SarlElementType(" + SarlPackage.SARL_SKILL + ")",
			"@SuppressWarnings(\"all\")",
			"public class A extends Skill implements Cap {",
			"  public void act_connectToSimulator(final String address, final int port) {",
			"    while (true) {",
			"    }",
			"  }",
			"  ",
			"  @SyntheticMember",
			"  public A() {",
			"    super();",
			"  }",
			"  ",
			"  @SyntheticMember",
			"  public A(final Agent arg0) {",
			"    super(arg0);",
			"  }",
			"}",
			"");

	private static final String EXPECTED01b = multilineString(
			"package foo.bug799;",
			"",
			"import foo.bug799.Cap;",
			"import io.sarl.lang.annotation.SarlElementType;",
			"import io.sarl.lang.annotation.SarlSpecification;",
			"import io.sarl.lang.annotation.SyntheticMember;",
			"import io.sarl.lang.core.Agent;",
			"import io.sarl.lang.core.Skill;",
			"",
			"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
			"@SarlElementType(" + SarlPackage.SARL_SKILL + ")",
			"@SuppressWarnings(\"all\")",
			"public class B extends Skill implements Cap {",
			"  public void act_connectToSimulator(final String address, final int port) {",
			"    while (true) {",
			"    }",
			"  }",
			"  ",
			"  @SyntheticMember",
			"  public B() {",
			"    super();",
			"  }",
			"  ",
			"  @SyntheticMember",
			"  public B(final Agent arg0) {",
			"    super(arg0);",
			"  }",
			"}",
			"");

	@Test
	public void parsing_01() throws Exception {
		SarlScript mas = file(SNIPSET01);
		final Validator validator = validate(mas);
		validator.assertNoErrors();
	}

	@Test
	public void compiling_01() throws Exception {
		getCompileHelper().compile(SNIPSET01, (it) -> {
			String actual = it.getGeneratedCode("foo.bug799.A");
			assertEquals(EXPECTED01a, actual);
			actual = it.getGeneratedCode("foo.bug799.B");
			assertEquals(EXPECTED01b, actual);
		});
	}

}

