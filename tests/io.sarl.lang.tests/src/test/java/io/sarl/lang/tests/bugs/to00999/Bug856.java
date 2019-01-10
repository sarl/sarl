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

import com.google.inject.Inject;
import org.eclipse.xtext.xbase.XbasePackage;
import org.eclipse.xtext.xbase.testing.CompilationTestHelper;
import org.junit.Test;

import io.sarl.lang.SARLVersion;
import io.sarl.lang.sarl.SarlPackage;
import io.sarl.lang.sarl.SarlScript;
import io.sarl.tests.api.AbstractSarlTest;

/** Testing class for issue: Twice rooting error when declared a pure function..
 *
 * <p>https://github.com/sarl/sarl/issues/856
 *
 * @author $Author: sgalland$
 * @version $Name$ $Revision$ $Date$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 * @see "https://github.com/sarl/sarl/issues/856"
 */
@SuppressWarnings("all")
public class Bug856 extends AbstractSarlTest {

	private static final String SNIPSET01 = multilineString(
			"package io.sarl.lang.tests.bug856",
			"import org.eclipse.xtext.xbase.lib.Pure",
			"import java.util.List",
			"import java.util.UUID",
			"class SomeClass {",
			"  @Pure",
			"  protected def getResults(agentId : UUID) : List<Object> {",
			"       newArrayList",
			"  }",
			"}");

	private static final String EXPECTED01 = multilineString(
			"package io.sarl.lang.tests.bug856;",
			"",
			"import io.sarl.lang.annotation.SarlElementType;",
			"import io.sarl.lang.annotation.SarlSpecification;",
			"import io.sarl.lang.annotation.SyntheticMember;",
			"import java.util.List;",
			"import java.util.UUID;",
			"import org.eclipse.xtext.xbase.lib.CollectionLiterals;",
			"import org.eclipse.xtext.xbase.lib.Pure;",
			"",
			"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING+ "\")",
			"@SarlElementType(" + SarlPackage.SARL_CLASS + ")",
			"@SuppressWarnings(\"all\")",
			"public class SomeClass {",
			"  @Pure",
			"  protected List<Object> getResults(final UUID agentId) {",
			"    return CollectionLiterals.<Object>newArrayList();",
			"  }",
			"  ",
			"  @SyntheticMember",
			"  public SomeClass() {",
			"    super();",
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
			final String actual = it.getGeneratedCode("io.sarl.lang.tests.bug856.SomeClass");
			assertEquals(EXPECTED01, actual);
		});
	}

	private static final String SNIPSET02 = multilineString(
			"package io.sarl.lang.tests.bug856",
			"import org.eclipse.xtext.xbase.lib.Pure",
			"import java.util.List",
			"import java.util.UUID",
			"class SomeClass {",
			"  @Pure",
			"  protected def getResults(agentId : UUID = null) : List<Object> {",
			"       newArrayList",
			"  }",
			"}");

	private static final String EXPECTED02 = multilineString(
			"package io.sarl.lang.tests.bug856;",
			"",
			"import io.sarl.lang.annotation.DefaultValue;",
			"import io.sarl.lang.annotation.DefaultValueSource;",
			"import io.sarl.lang.annotation.DefaultValueUse;",
			"import io.sarl.lang.annotation.SarlElementType;",
			"import io.sarl.lang.annotation.SarlSourceCode;",
			"import io.sarl.lang.annotation.SarlSpecification;",
			"import io.sarl.lang.annotation.SyntheticMember;",
			"import java.util.List;",
			"import java.util.UUID;",
			"import org.eclipse.xtext.xbase.lib.CollectionLiterals;",
			"import org.eclipse.xtext.xbase.lib.Pure;",
			"",
			"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING+ "\")",
			"@SarlElementType(" + SarlPackage.SARL_CLASS + ")",
			"@SuppressWarnings(\"all\")",
			"public class SomeClass {",
			"  @DefaultValueSource",
			"  @Pure",
			"  protected List<Object> getResults(@DefaultValue(\"io.sarl.lang.tests.bug856.SomeClass#GETRESULTS_0\") final UUID agentId) {",
			"    return CollectionLiterals.<Object>newArrayList();",
			"  }",
			"  ",
			"  /**",
			"   * Default value for the parameter agentId",
			"   */",
			"  @SyntheticMember",
			"  @SarlSourceCode(\"null\")",
			"  private final static UUID $DEFAULT_VALUE$GETRESULTS_0 = null;",
			"  ",
			"  @DefaultValueUse(\"java.util.UUID\")",
			"  @SyntheticMember",
			"  @Pure",
			"  protected final List<Object> getResults() {",
			"    return getResults($DEFAULT_VALUE$GETRESULTS_0);",
			"  }",
			"  ",
			"  @SyntheticMember",
			"  public SomeClass() {",
			"    super();",
			"  }",
			"}",
			"");

	@Test
	public void parsing_02() throws Exception {
		SarlScript mas = file(SNIPSET02);
		final Validator validator = validate(mas);
		validator.assertNoErrors();
	}

	@Test
	public void compiling_02() throws Exception {
		getCompileHelper().compile(SNIPSET02, (it) -> {
			final String actual = it.getGeneratedCode("io.sarl.lang.tests.bug856.SomeClass");
			assertEquals(EXPECTED02, actual);
		});
	}

}

