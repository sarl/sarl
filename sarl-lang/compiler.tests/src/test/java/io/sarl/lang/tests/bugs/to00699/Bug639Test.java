/*
 * $Id$
 *
 * SARL is an general-purpose agent programming language.
 * More details on http://www.sarl.io
 *
 * Copyright (C) 2014-2025 SARL.io, the Original Authors and Main Authors.
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

import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Tag;
import org.junit.jupiter.api.Test;

import io.sarl.lang.core.SARLVersion;
import io.sarl.lang.sarl.SarlPackage;
import io.sarl.lang.sarl.SarlScript;
import io.sarl.lang.tests.api.AbstractSarlTest;
import io.sarl.tests.api.tools.TestValidator.Validator;

/** Testing class for issue: Scopes of emit could be on UUID.
 *
 * <p>https://github.com/sarl/sarl/issues/639
 *
 * @author <a href="http://www.ciad-lab.fr/stephane_galland">St&eacute;phane Galland</a>
 * @version $Name$ $Revision$ $Date$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 */
@DisplayName("Bug #639")
@SuppressWarnings("all")
@Tag("core")
public class Bug639Test extends AbstractSarlTest {

	private static final String SNIPSET1 = multilineString(
			"package io.sarl.lang.tests.bug639",
			"import java.util.UUID",
			"import io.sarl.lang.core.Address",
			"class XXX {",
			"  static def cond(param : (Address) => boolean) : void {",
			"  }",
			"  static def getID : UUID {",
			"    null",
			"  }",
			"  static def test01 : void {",
			"    cond [it == ID]",
			"  }",
			"  static def test02 : void {",
			"    cond [ID == it]",
			"  }",
			"}");

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
		getCompileHelper().assertCompilesTo(SNIPSET1, multilineString(
				"package io.sarl.lang.tests.bug639;",
				"",
				"import io.sarl.lang.core.Address;",
				"import io.sarl.lang.core.annotation.SarlElementType;",
				"import io.sarl.lang.core.annotation.SarlSpecification;",
				"import io.sarl.lang.core.annotation.SyntheticMember;",
				"import java.util.UUID;",
				"import org.eclipse.xtext.xbase.lib.Functions.Function1;",
				"import org.eclipse.xtext.xbase.lib.Pure;",
				"import org.eclipse.xtext.xbase.lib.XbaseGenerated;",
				"",
				"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
				"@SarlElementType(" + SarlPackage.SARL_CLASS + ")",
				"@XbaseGenerated",
				"@SuppressWarnings(\"all\")",
				"public class XXX {",
				"  public static void cond(final Function1<? super Address, ? extends Boolean> param) {",
				"  }",
				"",
				"  @Pure",
				"  public static UUID getID() {",
				"    return null;",
				"  }",
				"",
				"  public static void test01() {",
				"    final Function1<Address, Boolean> _function = (Address it) -> {",
				"      UUID _iD = it.getID();",
				"      return Boolean.valueOf(((_iD) != null && (_iD).equals(it.getID())));",
				"    };",
				"    XXX.cond(_function);",
				"  }",
				"",
				"  public static void test02() {",
				"    final Function1<Address, Boolean> _function = (Address it) -> {",
				"      UUID _iD = it.getID();",
				"      return Boolean.valueOf((((_iD) == null && (it) == null) || ((_iD) != null && (_iD).equals((it).getID()))));",
				"    };",
				"    XXX.cond(_function);",
				"  }",
				"",
				"  @SyntheticMember",
				"  public XXX() {",
				"    super();",
				"  }",
				"}",
				""));
	}

	private static final String SNIPSET2 = multilineString(
			"package io.sarl.lang.tests.bug639",
			"import io.sarl.lang.core.Address",
			"import io.sarl.lang.core.SpaceID",
			"class YYY {",
			"  static def cond(param : (Address) => boolean) : void {",
			"  }",
			"  static def getID : SpaceID {",
			"    null",
			"  }",
			"  static def test01 : void {",
			"    cond [it == spaceID]",
			"  }",
			"  static def test02 : void {",
			"    cond [spaceID == it]",
			"  }",
			"}");

	@Test
	@Tag("sarlValidation")
	public void parsing_02() throws Exception {
		SarlScript mas = file(getParseHelper(), SNIPSET2);
		final Validator validator = validate(getValidationHelper(), getInjector(), mas);
		validator.assertNoErrors();
	}

	@Test
	@Tag("compileToJava")
	public void compiling_02() throws Exception {
		getCompileHelper().assertCompilesTo(SNIPSET2, multilineString(
				"package io.sarl.lang.tests.bug639;",
				"",
				"import io.sarl.lang.core.Address;",
				"import io.sarl.lang.core.SpaceID;",
				"import io.sarl.lang.core.annotation.SarlElementType;",
				"import io.sarl.lang.core.annotation.SarlSpecification;",
				"import io.sarl.lang.core.annotation.SyntheticMember;",
				"import org.eclipse.xtext.xbase.lib.Functions.Function1;",
				"import org.eclipse.xtext.xbase.lib.Pure;",
				"import org.eclipse.xtext.xbase.lib.XbaseGenerated;",
				"",
				"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
				"@SarlElementType(" + SarlPackage.SARL_CLASS + ")",
				"@XbaseGenerated",
				"@SuppressWarnings(\"all\")",
				"public class YYY {",
				"  public static void cond(final Function1<? super Address, ? extends Boolean> param) {",
				"  }",
				"",
				"  @Pure",
				"  public static SpaceID getID() {",
				"    return null;",
				"  }",
				"",
				"  public static void test01() {",
				"    final Function1<Address, Boolean> _function = (Address it) -> {",
				"      SpaceID _spaceID = it.getSpaceID();",
				"      return Boolean.valueOf(((_spaceID) != null && (_spaceID).equals(it.getSpaceID())));",
				"    };",
				"    YYY.cond(_function);",
				"  }",
				"",
				"  public static void test02() {",
				"    final Function1<Address, Boolean> _function = (Address it) -> {",
				"      SpaceID _spaceID = it.getSpaceID();",
				"      return Boolean.valueOf(((it) != null && _spaceID != null && (_spaceID.equals((it).getSpaceID()))));",
				"    };",
				"    YYY.cond(_function);",
				"  }",
				"",
				"  @SyntheticMember",
				"  public YYY() {",
				"    super();",
				"  }",
				"}",
				""));
	}

	private static final String SNIPSET3 = multilineString(
			"package io.sarl.lang.tests.bug639",
			"import java.util.UUID",
			"import io.sarl.lang.core.Address",
			"class XXX {",
			"  static def cond(param : (Address) => boolean) : void {",
			"  }",
			"  static def getID : UUID {",
			"    null",
			"  }",
			"  static def test01 : void {",
			"    cond [it != ID]",
			"  }",
			"  static def test02 : void {",
			"    cond [ID != it]",
			"  }",
			"}");

	@Test
	@Tag("sarlValidation")
	public void parsing_03() throws Exception {
		SarlScript mas = file(getParseHelper(), SNIPSET3);
		final Validator validator = validate(getValidationHelper(), getInjector(), mas);
		validator.assertNoErrors();
	}

	@Test
	@Tag("compileToJava")
	public void compiling_03() throws Exception {
		getCompileHelper().assertCompilesTo(SNIPSET3, multilineString(
				"package io.sarl.lang.tests.bug639;",
				"",
				"import io.sarl.lang.core.Address;",
				"import io.sarl.lang.core.annotation.SarlElementType;",
				"import io.sarl.lang.core.annotation.SarlSpecification;",
				"import io.sarl.lang.core.annotation.SyntheticMember;",
				"import java.util.UUID;",
				"import org.eclipse.xtext.xbase.lib.Functions.Function1;",
				"import org.eclipse.xtext.xbase.lib.Pure;",
				"import org.eclipse.xtext.xbase.lib.XbaseGenerated;",
				"",
				"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
				"@SarlElementType(" + SarlPackage.SARL_CLASS + ")",
				"@XbaseGenerated",
				"@SuppressWarnings(\"all\")",
				"public class XXX {",
				"  public static void cond(final Function1<? super Address, ? extends Boolean> param) {",
				"  }",
				"",
				"  @Pure",
				"  public static UUID getID() {",
				"    return null;",
				"  }",
				"",
				"  public static void test01() {",
				"    final Function1<Address, Boolean> _function = (Address it) -> {",
				"      UUID _iD = it.getID();",
				"      return Boolean.valueOf(((_iD) == null || !(_iD).equals(it.getID())));",
				"    };",
				"    XXX.cond(_function);",
				"  }",
				"",
				"  public static void test02() {",
				"    final Function1<Address, Boolean> _function = (Address it) -> {",
				"      UUID _iD = it.getID();",
				"      return Boolean.valueOf((((_iD) != null || (it) != null) && ((_iD) == null || !(_iD).equals((it).getID()))));",
				"    };",
				"    XXX.cond(_function);",
				"  }",
				"",
				"  @SyntheticMember",
				"  public XXX() {",
				"    super();",
				"  }",
				"}",
				""));
	}

	private static final String SNIPSET4 = multilineString(
			"package io.sarl.lang.tests.bug639",
			"import io.sarl.lang.core.Address",
			"import io.sarl.lang.core.SpaceID",
			"class YYY {",
			"  static def cond(param : (Address) => boolean) : void {",
			"  }",
			"  static def getID : SpaceID {",
			"    null",
			"  }",
			"  static def test01 : void {",
			"    cond [it != spaceID]",
			"  }",
			"  static def test02 : void {",
			"    cond [spaceID != it]",
			"  }",
			"}");

	@Test
	@Tag("sarlValidation")
	public void parsing_04() throws Exception {
		SarlScript mas = file(getParseHelper(), SNIPSET4);
		final Validator validator = validate(getValidationHelper(), getInjector(), mas);
		validator.assertNoErrors();
	}

	@Test
	@Tag("compileToJava")
	public void compiling_04() throws Exception {
		getCompileHelper().assertCompilesTo(SNIPSET4, multilineString(
				"package io.sarl.lang.tests.bug639;",
				"",
				"import io.sarl.lang.core.Address;",
				"import io.sarl.lang.core.SpaceID;",
				"import io.sarl.lang.core.annotation.SarlElementType;",
				"import io.sarl.lang.core.annotation.SarlSpecification;",
				"import io.sarl.lang.core.annotation.SyntheticMember;",
				"import org.eclipse.xtext.xbase.lib.Functions.Function1;",
				"import org.eclipse.xtext.xbase.lib.Pure;",
				"import org.eclipse.xtext.xbase.lib.XbaseGenerated;",
				"",
				"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
				"@SarlElementType(" + SarlPackage.SARL_CLASS + ")",
				"@XbaseGenerated",
				"@SuppressWarnings(\"all\")",
				"public class YYY {",
				"  public static void cond(final Function1<? super Address, ? extends Boolean> param) {",
				"  }",
				"",
				"  @Pure",
				"  public static SpaceID getID() {",
				"    return null;",
				"  }",
				"",
				"  public static void test01() {",
				"    final Function1<Address, Boolean> _function = (Address it) -> {",
				"      SpaceID _spaceID = it.getSpaceID();",
				"      return Boolean.valueOf(((_spaceID) == null || !(_spaceID).equals(it.getSpaceID())));",
				"    };",
				"    YYY.cond(_function);",
				"  }",
				"",
				"  public static void test02() {",
				"    final Function1<Address, Boolean> _function = (Address it) -> {",
				"      SpaceID _spaceID = it.getSpaceID();",
				"      return Boolean.valueOf(((it) == null || _spaceID == null || !(_spaceID.equals((it).getSpaceID()))));",
				"    };",
				"    YYY.cond(_function);",
				"  }",
				"",
				"  @SyntheticMember",
				"  public YYY() {",
				"    super();",
				"  }",
				"}",
				""));
	}

}
