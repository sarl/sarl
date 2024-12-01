/*
 * $Id$
 *
 * SARL is an general-purpose agent programming language.
 * More details on http://www.sarl.io
 *
 * Copyright (C) 2014-2024 SARL.io, the Original Authors and Main Authors
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

package io.sarl.lang.tests.bugs.to01399;

import static io.sarl.tests.api.tools.TestEObjects.file;
import static io.sarl.tests.api.tools.TestUtils.multilineString;
import static io.sarl.tests.api.tools.TestValidator.validate;
import static org.junit.jupiter.api.Assertions.assertEquals;

import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Tag;
import org.junit.jupiter.api.Test;

import io.sarl.lang.core.SARLVersion;
import io.sarl.lang.sarl.SarlPackage;
import io.sarl.lang.sarl.SarlScript;
import io.sarl.lang.tests.api.AbstractSarlTest;
import io.sarl.tests.api.tools.TestValidator.Validator;

/** Testing class for issue: Invalid bound matching.
 *
 * <p>https://github.com/sarl/sarl/issues/1154
 *
 * @author $Author: sgalland$
 * @version $Name$ $Revision$ $Date$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 * @see "https://github.com/sarl/sarl/issues/1154"
 */
@DisplayName("Bug #1154")
@SuppressWarnings("all")
@Tag("core")
public class Bug1154Test extends AbstractSarlTest {

	private static final String SARL_CODE_1 = multilineString(
			"package io.sarl.lang.tests.bug1154",
			"import java.io.Serializable",
			"interface ShapedObject {",
			"}",
			"interface SpatialTreeNode<N extends SpatialTreeNode<N, D>, D extends ShapedObject> extends Iterable<N>, Serializable {",
			"}",
			"abstract class AbstractSpatialTreeNode<N extends SpatialTreeNode<N, D>, D extends ShapedObject> implements SpatialTreeNode<N, D> {",
			"}");

	private static final String JAVA_CODE_1 = multilineString(
			"package io.sarl.lang.tests.bug1154;",
			"",
			"import io.sarl.lang.core.annotation.SarlElementType;",
			"import io.sarl.lang.core.annotation.SarlSpecification;",
			"import java.io.Serializable;",
			"import org.eclipse.xtext.xbase.lib.XbaseGenerated;",
			"",
			"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
			"@SarlElementType(" + SarlPackage.SARL_INTERFACE + ")",
			"@XbaseGenerated",
			"@SuppressWarnings(\"all\")",
			"public interface SpatialTreeNode<N extends SpatialTreeNode<N, D>, D extends ShapedObject> extends Iterable<N>, Serializable {",
			"}",
			"");

	private static final String JAVA_CODE_2 = multilineString(
			"package io.sarl.lang.tests.bug1154;",
			"",
			"import io.sarl.lang.core.annotation.SarlElementType;",
			"import io.sarl.lang.core.annotation.SarlSpecification;",
			"import io.sarl.lang.core.annotation.SyntheticMember;",
			"import org.eclipse.xtext.xbase.lib.XbaseGenerated;",
			"",
			"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
			"@SarlElementType(" + SarlPackage.SARL_CLASS + ")",
			"@XbaseGenerated",
			"@SuppressWarnings(\"all\")",
			"public abstract class AbstractSpatialTreeNode<N extends SpatialTreeNode<N, D>, D extends ShapedObject> implements SpatialTreeNode<N, D> {",
			"  @SyntheticMember",
			"  public AbstractSpatialTreeNode() {",
			"    super();",
			"  }",
			"",
			"  @SyntheticMember",
			"  private static final long serialVersionUID = 1L;",
			"}",
			"");

	@Test
	@Tag("sarlValidation")
	@DisplayName("parse: no error #1")
	public void parsing_0() throws Exception {
		SarlScript mas = file(getParseHelper(), SARL_CODE_1);
		final Validator validator = validate(getValidationHelper(), getInjector(), mas);
		validator.assertNoErrors();
	}

	@Test
	@Tag("compileToJava")
	@DisplayName("compile: interface #1")
	public void compiling_interface_0() throws Exception {
		getCompileHelper().compile(SARL_CODE_1, (it) -> {
			String actual;
			actual = it.getGeneratedCode("io.sarl.lang.tests.bug1154.SpatialTreeNode");
			assertEquals(JAVA_CODE_1, actual);
		});
	}

	@Test
	@Tag("compileToJava")
	@DisplayName("compile: abstract class #1")
	public void compiling_abstract_0() throws Exception {
		getCompileHelper().compile(SARL_CODE_1, (it) -> {
			String actual;
			actual = it.getGeneratedCode("io.sarl.lang.tests.bug1154.AbstractSpatialTreeNode");
			assertEquals(JAVA_CODE_2, actual);
		});
	}

	private static final String SARL_CODE_2 = multilineString(
			"package io.sarl.lang.tests.bug1154",
			"import java.io.Serializable",
			"interface SpatialTreeNode<N extends SpatialTreeNode<N>> {",
			"}",
			"abstract class AbstractSpatialTreeNode<N extends SpatialTreeNode<N>> implements SpatialTreeNode<N> {",
			"}");

	private static final String JAVA_CODE_3 = multilineString(
			"package io.sarl.lang.tests.bug1154;",
			"",
			"import io.sarl.lang.core.annotation.SarlElementType;",
			"import io.sarl.lang.core.annotation.SarlSpecification;",
			"import org.eclipse.xtext.xbase.lib.XbaseGenerated;",
			"",
			"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
			"@SarlElementType(" + SarlPackage.SARL_INTERFACE + ")",
			"@XbaseGenerated",
			"@SuppressWarnings(\"all\")",
			"public interface SpatialTreeNode<N extends SpatialTreeNode<N>> {",
			"}",
			"");

	private static final String JAVA_CODE_4 = multilineString(
			"package io.sarl.lang.tests.bug1154;",
			"",
			"import io.sarl.lang.core.annotation.SarlElementType;",
			"import io.sarl.lang.core.annotation.SarlSpecification;",
			"import io.sarl.lang.core.annotation.SyntheticMember;",
			"import org.eclipse.xtext.xbase.lib.XbaseGenerated;",
			"",
			"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
			"@SarlElementType(" + SarlPackage.SARL_CLASS + ")",
			"@XbaseGenerated",
			"@SuppressWarnings(\"all\")",
			"public abstract class AbstractSpatialTreeNode<N extends SpatialTreeNode<N>> implements SpatialTreeNode<N> {",
			"  @SyntheticMember",
			"  public AbstractSpatialTreeNode() {",
			"    super();",
			"  }",
			"}",
			"");

	@Test
	@Tag("sarlValidation")
	@DisplayName("parse: no error #2")
	public void parsing_1() throws Exception {
		SarlScript mas = file(getParseHelper(), SARL_CODE_2);
		final Validator validator = validate(getValidationHelper(), getInjector(), mas);
		validator.assertNoErrors();
	}

	@Test
	@Tag("compileToJava")
	@DisplayName("compile: interface #2")
	public void compiling_interface_1() throws Exception {
		getCompileHelper().compile(SARL_CODE_2, (it) -> {
			String actual;
			actual = it.getGeneratedCode("io.sarl.lang.tests.bug1154.SpatialTreeNode");
			assertEquals(JAVA_CODE_3, actual);
		});
	}

	@Test
	@Tag("compileToJava")
	@DisplayName("compile: abstract class #2")
	public void compiling_abstract_1() throws Exception {
		getCompileHelper().compile(SARL_CODE_2, (it) -> {
			String actual;
			actual = it.getGeneratedCode("io.sarl.lang.tests.bug1154.AbstractSpatialTreeNode");
			assertEquals(JAVA_CODE_4, actual);
		});
	}

}
