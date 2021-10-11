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
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package io.sarl.lang.tests.bugs.to00699;

import static io.sarl.tests.api.tools.TestEObjects.file;
import static io.sarl.tests.api.tools.TestUtils.multilineString;
import static io.sarl.tests.api.tools.TestValidator.validate;
import static org.junit.jupiter.api.Assertions.assertEquals;

import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Tag;
import org.junit.jupiter.api.Test;

import io.sarl.lang.SARLVersion;
import io.sarl.lang.sarl.SarlPackage;
import io.sarl.lang.sarl.SarlScript;
import io.sarl.tests.api.AbstractSarlTest;
import io.sarl.tests.api.tools.TestValidator.Validator;

/** Testing class for issue: Invalid default value attribute with generic type
 *
 * <p>https://github.com/sarl/sarl/issues/643
 *
 * @author $Author: sgalland$
 * @version $Name$ $Revision$ $Date$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 */
@DisplayName("Bug #643")
@SuppressWarnings("all")
@Tag("core")
public class Bug643Test extends AbstractSarlTest {

	private static final String SNIPSET1 = multilineString(
			"package io.sarl.lang.tests.bug643",
			"import java.util.Stack",
			"import java.util.List",
			"",
			"class ShapedObject { }",
			"",
			"class Shape2f<T> {",
			"}",
			"",
			"class TreeNode<T extends ShapedObject> {",
			"  new (parent : TreeNode<T> = null, box : Shape2f) { }",
			"}");

	private final String EXPECTED = multilineString(
			"package io.sarl.lang.tests.bug643;",
			"",
			"import io.sarl.lang.annotation.DefaultValue;",
			"import io.sarl.lang.annotation.DefaultValueSource;",
			"import io.sarl.lang.annotation.DefaultValueUse;",
			"import io.sarl.lang.annotation.SarlElementType;",
			"import io.sarl.lang.annotation.SarlSourceCode;",
			"import io.sarl.lang.annotation.SarlSpecification;",
			"import io.sarl.lang.annotation.SyntheticMember;",
			"import org.eclipse.xtext.xbase.lib.Pure;",
			"",
			"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
			"@SarlElementType(" + SarlPackage.SARL_CLASS + ")",
			"@SuppressWarnings(\"all\")",
			"public class TreeNode<T extends ShapedObject> {",
			"  @DefaultValueSource",
			"  public TreeNode(@DefaultValue(\"io.sarl.lang.tests.bug643.TreeNode#NEW_0\") final TreeNode<T> parent, final Shape2f box) {",
			"  }",
			"  ",
			"  /**",
			"   * Default value for the parameter parent",
			"   */",
			"  @Pure",
			"  @SyntheticMember",
			"  @SarlSourceCode(\"null\")",
			"  private static TreeNode $DEFAULT_VALUE$NEW_0() {",
			"    return null;",
			"  }",
			"  ",
			"  @DefaultValueUse(\"io.sarl.lang.tests.bug643.TreeNode,io.sarl.lang.tests.bug643.Shape2f\")",
			"  @SyntheticMember",
			"  public TreeNode(final Shape2f box) {",
			"    this($DEFAULT_VALUE$NEW_0(), box);",
			"  }",
			"}",
			"");

	@Test
	@Tag("sarlValidation")
	public void parsing_01() throws Exception {
		SarlScript mas = file(getParseHelper(), SNIPSET1);
		final Validator validator = validate(getValidationHelper(), getInjector(), mas);
		validator.assertNoErrors();
	}

	@Test
	@Tag("compileToJava")
	public void compiling_01() throws Exception {
		getCompileHelper().compile(SNIPSET1, (it) -> {
			final String actual = it.getGeneratedCode("io.sarl.lang.tests.bug643.TreeNode");
			assertEquals(EXPECTED, actual);
		});
	}

}
