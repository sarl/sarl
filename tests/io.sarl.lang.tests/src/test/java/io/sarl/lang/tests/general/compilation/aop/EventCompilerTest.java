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

package io.sarl.lang.tests.general.compilation.aop;

import static io.sarl.tests.api.tools.TestUtils.multilineString;
import static org.junit.jupiter.api.Assertions.assertEquals;

import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Tag;
import org.junit.jupiter.api.Test;

import io.sarl.lang.SARLVersion;
import io.sarl.lang.sarl.SarlPackage;
import io.sarl.tests.api.AbstractSarlTest;

/**
 * @author $Author: srodriguez$
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 */
@SuppressWarnings("all")
@DisplayName("Compilation: Event")
@Tag("core")
@Tag("compileToJava")
public class EventCompilerTest extends AbstractSarlTest {

	@Test
	public void basicCompile_withBlock() throws Exception {
		String source = "event E1 { }";
		String expected = multilineString(
				"import io.sarl.lang.annotation.SarlElementType;",
				"import io.sarl.lang.annotation.SarlSpecification;",
				"import io.sarl.lang.annotation.SyntheticMember;",
				"import io.sarl.lang.core.Address;",
				"import io.sarl.lang.core.Event;",
				"",
				"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
				"@SarlElementType(" + SarlPackage.SARL_EVENT + ")",
				"@SuppressWarnings(\"all\")",
				"public class E1 extends Event {",
				"  @SyntheticMember",
				"  public E1() {",
				"    super();",
				"  }",
				"  ",
				"  @SyntheticMember",
				"  public E1(final Address arg0) {",
				"    super(arg0);",
				"  }",
				"  ",
				"  @SyntheticMember",
				"  private static final long serialVersionUID = 588368462L;",
				"}",
				""
				);
		getCompileHelper().assertCompilesTo(source, expected);
	}

	@Test
	public void basicCompile_withoutBlock() throws Exception {
		String source = "event E1";
		String expected = multilineString(
				"import io.sarl.lang.annotation.SarlElementType;",
				"import io.sarl.lang.annotation.SarlSpecification;",
				"import io.sarl.lang.annotation.SyntheticMember;",
				"import io.sarl.lang.core.Address;",
				"import io.sarl.lang.core.Event;",
				"",
				"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
				"@SarlElementType(" + SarlPackage.SARL_EVENT + ")",
				"@SuppressWarnings(\"all\")",
				"public class E1 extends Event {",
				"  @SyntheticMember",
				"  public E1() {",
				"    super();",
				"  }",
				"  ",
				"  @SyntheticMember",
				"  public E1(final Address arg0) {",
				"    super(arg0);",
				"  }",
				"  ",
				"  @SyntheticMember",
				"  private static final long serialVersionUID = 588368462L;",
				"}",
				""
				);
		getCompileHelper().assertCompilesTo(source, expected);
	}

	@Test
	public void withVarAttributesCompile() throws Exception {
		String source = multilineString(
				"event E1 {",
				"  var name : String",
				"}"
				);
		String expected = multilineString(
				"import io.sarl.lang.annotation.SarlElementType;",
				"import io.sarl.lang.annotation.SarlSpecification;",
				"import io.sarl.lang.annotation.SyntheticMember;",
				"import io.sarl.lang.core.Address;",
				"import io.sarl.lang.core.Event;",
				"import java.util.Objects;",
				"import org.eclipse.xtext.xbase.lib.Pure;",
				"import org.eclipse.xtext.xbase.lib.util.ToStringBuilder;",
				"",
				"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
				"@SarlElementType(" + SarlPackage.SARL_EVENT + ")",
				"@SuppressWarnings(\"all\")",
				"public class E1 extends Event {",
				"  public String name;",
				"  ",
				"  @SyntheticMember",
				"  public E1() {",
				"    super();",
				"  }",
				"  ",
				"  @SyntheticMember",
				"  public E1(final Address arg0) {",
				"    super(arg0);",
				"  }",
				"  ",
				"  @Override",
				"  @Pure",
				"  @SyntheticMember",
				"  public boolean equals(final Object obj) {",
				"    if (this == obj)",
				"      return true;",
				"    if (obj == null)",
				"      return false;",
				"    if (getClass() != obj.getClass())",
				"      return false;",
				"    E1 other = (E1) obj;",
				"    if (!Objects.equals(this.name, other.name))",
				"      return false;",
				"    return super.equals(obj);",
				"  }",
				"  ",
				"  @Override",
				"  @Pure",
				"  @SyntheticMember",
				"  public int hashCode() {",
				"    int result = super.hashCode();",
				"    final int prime = 31;",
				"    result = prime * result + Objects.hashCode(this.name);",
				"    return result;",
				"  }",
				"  ",
				"  /**",
				"   * Returns a String representation of the E1 event's attributes only.",
				"   */",
				"  @SyntheticMember",
				"  @Pure",
				"  protected void toString(final ToStringBuilder builder) {",
				"    super.toString(builder);",
				"    builder.add(\"name\", this.name);",
				"  }",
				"  ",
				"  @SyntheticMember",
				"  private static final long serialVersionUID = 1787001662L;",
				"}",
				""
				);
		getCompileHelper().assertCompilesTo(source, expected);
	}

	@Test
	public void inheritanceCompile() throws Exception {
		String source = multilineString(
				"event E1 {",
				"	var name : String",
				"}",
				"",
				"event E2 extends E1{",
				"}"
				);
		final String expectedE2 = multilineString(
				"import io.sarl.lang.annotation.SarlElementType;",
				"import io.sarl.lang.annotation.SarlSpecification;",
				"import io.sarl.lang.annotation.SyntheticMember;",
				"import io.sarl.lang.core.Address;",
				"",
				"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
				"@SarlElementType(" + SarlPackage.SARL_EVENT + ")",
				"@SuppressWarnings(\"all\")",
				"public class E2 extends E1 {",
				"  @SyntheticMember",
				"  public E2() {",
				"    super();",
				"  }",
				"  ",
				"  @SyntheticMember",
				"  public E2(final Address arg0) {",
				"    super(arg0);",
				"  }",
				"  ",
				"  @SyntheticMember",
				"  private static final long serialVersionUID = 2189L;",
				"}",
				""
				);
		getCompileHelper().compile(source, (r) -> assertEquals(expectedE2,r.getGeneratedCode("E2")));
	}

	@Test
	public void noStaticField() throws Exception {
		String source = multilineString(
				"event E1 {",
				"	val titi : int = 4",
				"	val toto : int",
				"	new(a : int) {",
				"		this.toto = a",
				"	}",
				"}"
				);
		String expected = multilineString(
				"import io.sarl.lang.annotation.SarlElementType;",
				"import io.sarl.lang.annotation.SarlSpecification;",
				"import io.sarl.lang.annotation.SyntheticMember;",
				"import io.sarl.lang.core.Event;",
				"import org.eclipse.xtext.xbase.lib.Pure;",
				"import org.eclipse.xtext.xbase.lib.util.ToStringBuilder;",
				"",
				"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
				"@SarlElementType(" + SarlPackage.SARL_EVENT + ")",
				"@SuppressWarnings(\"all\")",
				"public class E1 extends Event {",
				"  public final int titi = 4;",
				"  ",
				"  public final int toto;",
				"  ",
				"  public E1(final int a) {",
				"    this.toto = a;",
				"  }",
				"  ",
				"  @Override",
				"  @Pure",
				"  @SyntheticMember",
				"  public boolean equals(final Object obj) {",
				"    if (this == obj)",
				"      return true;",
				"    if (obj == null)",
				"      return false;",
				"    if (getClass() != obj.getClass())",
				"      return false;",
				"    E1 other = (E1) obj;",
				"    if (other.titi != this.titi)",
				"      return false;",
				"    if (other.toto != this.toto)",
				"      return false;",
				"    return super.equals(obj);",
				"  }",
				"  ",
				"  @Override",
				"  @Pure",
				"  @SyntheticMember",
				"  public int hashCode() {",
				"    int result = super.hashCode();",
				"    final int prime = 31;",
				"    result = prime * result + Integer.hashCode(this.titi);",
				"    result = prime * result + Integer.hashCode(this.toto);",
				"    return result;",
				"  }",
				"  ",
				"  /**",
				"   * Returns a String representation of the E1 event's attributes only.",
				"   */",
				"  @SyntheticMember",
				"  @Pure",
				"  protected void toString(final ToStringBuilder builder) {",
				"    super.toString(builder);",
				"    builder.add(\"titi\", this.titi);", 
				"    builder.add(\"toto\", this.toto);",
				"  }",
				"  ",
				"  @SyntheticMember",
				"  private static final long serialVersionUID = 598944340L;",
				"}",
				""
				);
		getCompileHelper().assertCompilesTo(source, expected);
	}

	@Test
	public void eventmodifier_none() throws Exception {
		getCompileHelper().assertCompilesTo(
			multilineString(
				"event E1"
			),
			multilineString(
				"import io.sarl.lang.annotation.SarlElementType;",
				"import io.sarl.lang.annotation.SarlSpecification;",
				"import io.sarl.lang.annotation.SyntheticMember;",
				"import io.sarl.lang.core.Address;",
				"import io.sarl.lang.core.Event;",
				"",
				"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
				"@SarlElementType(" + SarlPackage.SARL_EVENT + ")",
				"@SuppressWarnings(\"all\")",
				"public class E1 extends Event {",
				"  @SyntheticMember",
				"  public E1() {",
				"    super();",
				"  }",
				"  ",
				"  @SyntheticMember",
				"  public E1(final Address arg0) {",
				"    super(arg0);",
				"  }",
				"  ",
				"  @SyntheticMember",
				"  private static final long serialVersionUID = 588368462L;",
				"}",
				""));
	}

	@Test
	public void eventmodifier_public() throws Exception {
		getCompileHelper().assertCompilesTo(
			multilineString(
				"public event E1"
			),
			multilineString(
				"import io.sarl.lang.annotation.SarlElementType;",
				"import io.sarl.lang.annotation.SarlSpecification;",
				"import io.sarl.lang.annotation.SyntheticMember;",
				"import io.sarl.lang.core.Address;",
				"import io.sarl.lang.core.Event;",
				"",
				"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
				"@SarlElementType(" + SarlPackage.SARL_EVENT + ")",
				"@SuppressWarnings(\"all\")",
				"public class E1 extends Event {",
				"  @SyntheticMember",
				"  public E1() {",
				"    super();",
				"  }",
				"  ",
				"  @SyntheticMember",
				"  public E1(final Address arg0) {",
				"    super(arg0);",
				"  }",
				"  ",
				"  @SyntheticMember",
				"  private static final long serialVersionUID = 588368462L;",
				"}",
				""));
	}

	@Test
	public void eventmodifier_abstract() throws Exception {
		getCompileHelper().assertCompilesTo(
			multilineString(
				"abstract event E1"
			),
			multilineString(
				"import io.sarl.lang.annotation.SarlElementType;",
				"import io.sarl.lang.annotation.SarlSpecification;",
				"import io.sarl.lang.annotation.SyntheticMember;",
				"import io.sarl.lang.core.Address;",
				"import io.sarl.lang.core.Event;",
				"",
				"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
				"@SarlElementType(" + SarlPackage.SARL_EVENT + ")",
				"@SuppressWarnings(\"all\")",
				"public abstract class E1 extends Event {",
				"  @SyntheticMember",
				"  public E1() {",
				"    super();",
				"  }",
				"  ",
				"  @SyntheticMember",
				"  public E1(final Address arg0) {",
				"    super(arg0);",
				"  }",
				"  ",
				"  @SyntheticMember",
				"  private static final long serialVersionUID = 588368462L;",
				"}",
				""));
	}

	@Test
	public void eventmodifier_package() throws Exception {
		getCompileHelper().assertCompilesTo(
			multilineString(
				"package event E1"
			),
			multilineString(
				"import io.sarl.lang.annotation.SarlElementType;",
				"import io.sarl.lang.annotation.SarlSpecification;",
				"import io.sarl.lang.annotation.SyntheticMember;",
				"import io.sarl.lang.core.Address;",
				"import io.sarl.lang.core.Event;",
				"",
				"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
				"@SarlElementType(" + SarlPackage.SARL_EVENT + ")",
				"@SuppressWarnings(\"all\")",
				"class E1 extends Event {",
				"  @SyntheticMember",
				"  public E1() {",
				"    super();",
				"  }",
				"  ",
				"  @SyntheticMember",
				"  public E1(final Address arg0) {",
				"    super(arg0);",
				"  }",
				"  ",
				"  @SyntheticMember",
				"  private static final long serialVersionUID = 588368462L;",
				"}",
				""));
	}

	@Test
	public void eventmodifier_final() throws Exception {
		getCompileHelper().assertCompilesTo(
			multilineString(
				"final event E1"
			),
			multilineString(
				"import io.sarl.lang.annotation.SarlElementType;",
				"import io.sarl.lang.annotation.SarlSpecification;",
				"import io.sarl.lang.annotation.SyntheticMember;",
				"import io.sarl.lang.core.Address;",
				"import io.sarl.lang.core.Event;",
				"",
				"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
				"@SarlElementType(" + SarlPackage.SARL_EVENT + ")",
				"@SuppressWarnings(\"all\")",
				"public final class E1 extends Event {",
				"  @SyntheticMember",
				"  public E1() {",
				"    super();",
				"  }",
				"  ",
				"  @SyntheticMember",
				"  public E1(final Address arg0) {",
				"    super(arg0);",
				"  }",
				"  ",
				"  @SyntheticMember",
				"  private static final long serialVersionUID = 588368462L;",
				"}",
				""));
	}

	@Test
	public void fieldmodifier_none() throws Exception {
		getCompileHelper().assertCompilesTo(
			multilineString(
				"event E1 {",
				"	var field : int",
				"}"
			),
			multilineString(
				"import io.sarl.lang.annotation.SarlElementType;",
				"import io.sarl.lang.annotation.SarlSpecification;",
				"import io.sarl.lang.annotation.SyntheticMember;",
				"import io.sarl.lang.core.Address;",
				"import io.sarl.lang.core.Event;",
				"import org.eclipse.xtext.xbase.lib.Pure;",
				"import org.eclipse.xtext.xbase.lib.util.ToStringBuilder;",
				"",
				"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
				"@SarlElementType(" + SarlPackage.SARL_EVENT + ")",
				"@SuppressWarnings(\"all\")",
				"public class E1 extends Event {",
				"  public int field;",
				"  ",
				"  @SyntheticMember",
				"  public E1() {",
				"    super();",
				"  }",
				"  ",
				"  @SyntheticMember",
				"  public E1(final Address arg0) {",
				"    super(arg0);",
				"  }",
				"  ",
				"  @Override",
				"  @Pure",
				"  @SyntheticMember",
				"  public boolean equals(final Object obj) {",
				"    if (this == obj)",
				"      return true;",
				"    if (obj == null)",
				"      return false;",
				"    if (getClass() != obj.getClass())",
				"      return false;",
				"    E1 other = (E1) obj;",
				"    if (other.field != this.field)",
				"      return false;",
				"    return super.equals(obj);",
				"  }",
				"  ",
				"  @Override",
				"  @Pure",
				"  @SyntheticMember",
				"  public int hashCode() {",
				"    int result = super.hashCode();",
				"    final int prime = 31;",
				"    result = prime * result + Integer.hashCode(this.field);",
				"    return result;",
				"  }",
				"  ",
				"  /**",
				"   * Returns a String representation of the E1 event's attributes only.",
				"   */",
				"  @SyntheticMember",
				"  @Pure",
				"  protected void toString(final ToStringBuilder builder) {",
				"    super.toString(builder);",
				"    builder.add(\"field\", this.field);",
				"  }",
				"  ",
				"  @SyntheticMember",
				"  private static final long serialVersionUID = 685900599L;",
				"}",
				""));
	}

	@Test
	public void fieldmodifier_public() throws Exception {
		getCompileHelper().assertCompilesTo(
			multilineString(
				"event E1 {",
				"	public var field : int",
				"}"
			),
			multilineString(
				"import io.sarl.lang.annotation.SarlElementType;",
				"import io.sarl.lang.annotation.SarlSpecification;",
				"import io.sarl.lang.annotation.SyntheticMember;",
				"import io.sarl.lang.core.Address;",
				"import io.sarl.lang.core.Event;",
				"import org.eclipse.xtext.xbase.lib.Pure;",
				"import org.eclipse.xtext.xbase.lib.util.ToStringBuilder;",
				"",
				"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
				"@SarlElementType(" + SarlPackage.SARL_EVENT + ")",
				"@SuppressWarnings(\"all\")",
				"public class E1 extends Event {",
				"  public int field;",
				"  ",
				"  @SyntheticMember",
				"  public E1() {",
				"    super();",
				"  }",
				"  ",
				"  @SyntheticMember",
				"  public E1(final Address arg0) {",
				"    super(arg0);",
				"  }",
				"  ",
				"  @Override",
				"  @Pure",
				"  @SyntheticMember",
				"  public boolean equals(final Object obj) {",
				"    if (this == obj)",
				"      return true;",
				"    if (obj == null)",
				"      return false;",
				"    if (getClass() != obj.getClass())",
				"      return false;",
				"    E1 other = (E1) obj;",
				"    if (other.field != this.field)",
				"      return false;",
				"    return super.equals(obj);",
				"  }",
				"  ",
				"  @Override",
				"  @Pure",
				"  @SyntheticMember",
				"  public int hashCode() {",
				"    int result = super.hashCode();",
				"    final int prime = 31;",
				"    result = prime * result + Integer.hashCode(this.field);",
				"    return result;",
				"  }",
				"  ",
				"  /**",
				"   * Returns a String representation of the E1 event's attributes only.",
				"   */",
				"  @SyntheticMember",
				"  @Pure",
				"  protected void toString(final ToStringBuilder builder) {",
				"    super.toString(builder);",
				"    builder.add(\"field\", this.field);",
				"  }",
				"  ",
				"  @SyntheticMember",
				"  private static final long serialVersionUID = 685900599L;",
				"}",
				""));
	}

	@Test
	public void constructormodifier_none() throws Exception {
		getCompileHelper().assertCompilesTo(
			multilineString(
				"event E1 {",
				"	new { super(null) }",
				"}"
			),
			multilineString(
				"import io.sarl.lang.annotation.SarlElementType;",
				"import io.sarl.lang.annotation.SarlSpecification;",
				"import io.sarl.lang.annotation.SyntheticMember;",
				"import io.sarl.lang.core.Event;",
				"",
				"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
				"@SarlElementType(" + SarlPackage.SARL_EVENT + ")",
				"@SuppressWarnings(\"all\")",
				"public class E1 extends Event {",
				"  public E1() {",
				"    super(null);",
				"  }",
				"  ",
				"  @SyntheticMember",
				"  private static final long serialVersionUID = 588370691L;",
				"}",
				""));
	}

	@Test
	public void constructormodifier_public() throws Exception {
		getCompileHelper().assertCompilesTo(
			multilineString(
				"event E1 {",
				"	public new { super(null) }",
				"}"
			),
			multilineString(
				"import io.sarl.lang.annotation.SarlElementType;",
				"import io.sarl.lang.annotation.SarlSpecification;",
				"import io.sarl.lang.annotation.SyntheticMember;",
				"import io.sarl.lang.core.Event;",
				"",
				"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
				"@SarlElementType(" + SarlPackage.SARL_EVENT + ")",
				"@SuppressWarnings(\"all\")",
				"public class E1 extends Event {",
				"  public E1() {",
				"    super(null);",
				"  }",
				"  ",
				"  @SyntheticMember",
				"  private static final long serialVersionUID = 588370691L;",
				"}",
				""));
	}

	@Test
	public void constructormodifier_private() throws Exception {
		getCompileHelper().assertCompilesTo(
			multilineString(
				"event E1 {",
				"	private new { super(null) }",
				"}"
			),
			multilineString(
				"import io.sarl.lang.annotation.SarlElementType;",
				"import io.sarl.lang.annotation.SarlSpecification;",
				"import io.sarl.lang.annotation.SyntheticMember;",
				"import io.sarl.lang.core.Event;",
				"",
				"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
				"@SarlElementType(" + SarlPackage.SARL_EVENT + ")",
				"@SuppressWarnings(\"all\")",
				"public class E1 extends Event {",
				"  private E1() {",
				"    super(null);",
				"  }",
				"  ",
				"  @SyntheticMember",
				"  private static final long serialVersionUID = 588370691L;",
				"}",
				""));
	}

	@Test
	public void constructormodifier_package() throws Exception {
		getCompileHelper().assertCompilesTo(
			multilineString(
				"event E1 {",
				"	package new { super(null) }",
				"}"
			),
			multilineString(
				"import io.sarl.lang.annotation.SarlElementType;",
				"import io.sarl.lang.annotation.SarlSpecification;",
				"import io.sarl.lang.annotation.SyntheticMember;",
				"import io.sarl.lang.core.Event;",
				"",
				"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
				"@SarlElementType(" + SarlPackage.SARL_EVENT + ")",
				"@SuppressWarnings(\"all\")",
				"public class E1 extends Event {",
				"  E1() {",
				"    super(null);",
				"  }",
				"  ",
				"  @SyntheticMember",
				"  private static final long serialVersionUID = 588370691L;",
				"}",
				""));
	}

	@Test
	public void constructormodifier_protected() throws Exception {
		getCompileHelper().assertCompilesTo(
			multilineString(
				"event E1 {",
				"	protected new { super(null) }",
				"}"
			),
			multilineString(
				"import io.sarl.lang.annotation.SarlElementType;",
				"import io.sarl.lang.annotation.SarlSpecification;",
				"import io.sarl.lang.annotation.SyntheticMember;",
				"import io.sarl.lang.core.Event;",
				"",
				"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
				"@SarlElementType(" + SarlPackage.SARL_EVENT + ")",
				"@SuppressWarnings(\"all\")",
				"public class E1 extends Event {",
				"  protected E1() {",
				"    super(null);",
				"  }",
				"  ",
				"  @SyntheticMember",
				"  private static final long serialVersionUID = 588370691L;",
				"}",
				""));
	}

}
