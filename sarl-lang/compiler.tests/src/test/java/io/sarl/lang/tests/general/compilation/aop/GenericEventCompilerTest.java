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

package io.sarl.lang.tests.general.compilation.aop;

import static io.sarl.tests.api.tools.TestUtils.multilineString;
import static org.junit.jupiter.api.Assertions.assertEquals;

import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Nested;
import org.junit.jupiter.api.Tag;
import org.junit.jupiter.api.Test;

import io.sarl.lang.core.SARLVersion;
import io.sarl.lang.core.Event.Bound;
import io.sarl.lang.sarl.SarlPackage;
import io.sarl.lang.tests.api.AbstractSarlTest;

/**
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 * @since 0.14
 */
@DisplayName("Compilation: generic Event")
@Tag("core")
@Tag("sarlValidation")
@SuppressWarnings("all")
public class GenericEventCompilerTest {

	@DisplayName("Direct Event inheritance")
	@Nested
	public class DirectEventInheritance extends AbstractSarlTest {

		@Test
		@DisplayName("Empty event with block")
		public void basicCompile_withBlock() throws Exception {
			String source = "event E1<T1, T2 extends Number, T3 extends Double> { }";
			String expected = multilineString(
					"import io.sarl.lang.core.Address;",
					"import io.sarl.lang.core.Event;",
					"import io.sarl.lang.core.annotation.SarlElementType;",
					"import io.sarl.lang.core.annotation.SarlSpecification;",
					"import io.sarl.lang.core.annotation.SyntheticMember;",
					"import org.eclipse.xtext.xbase.lib.XbaseGenerated;",
					"",
					"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
					"@SarlElementType(" + SarlPackage.SARL_EVENT + ")",
					"@XbaseGenerated",
					"@SuppressWarnings(\"all\")",
					"public class E1<T1 extends Object, T2 extends Number, T3 extends Double> extends Event {",
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
		@DisplayName("Empty event without block")
		public void basicCompile_withoutBlock() throws Exception {
			String source = "event E1<T1, T2 extends Number, T3 extends Double>";
			String expected = multilineString(
					"import io.sarl.lang.core.Address;",
					"import io.sarl.lang.core.Event;",
					"import io.sarl.lang.core.annotation.SarlElementType;",
					"import io.sarl.lang.core.annotation.SarlSpecification;",
					"import io.sarl.lang.core.annotation.SyntheticMember;",
					"import org.eclipse.xtext.xbase.lib.XbaseGenerated;",
					"",
					"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
					"@SarlElementType(" + SarlPackage.SARL_EVENT + ")",
					"@XbaseGenerated",
					"@SuppressWarnings(\"all\")",
					"public class E1<T1 extends Object, T2 extends Number, T3 extends Double> extends Event {",
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
		@DisplayName("With string field")
		public void withVarAttributesCompile() throws Exception {
			String source = multilineString(
					"event E1<T1, T2 extends Number, T3 extends Double> {",
					"  var name : String",
					"}"
					);
			String expected = multilineString(
					"import io.sarl.lang.core.Address;",
					"import io.sarl.lang.core.Event;",
					"import io.sarl.lang.core.annotation.SarlElementType;",
					"import io.sarl.lang.core.annotation.SarlSpecification;",
					"import io.sarl.lang.core.annotation.SyntheticMember;",
					"import java.util.Objects;",
					"import org.eclipse.xtext.xbase.lib.Pure;",
					"import org.eclipse.xtext.xbase.lib.XbaseGenerated;",
					"import org.eclipse.xtext.xbase.lib.util.ToStringBuilder;",
					"",
					"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
					"@SarlElementType(" + SarlPackage.SARL_EVENT + ")",
					"@XbaseGenerated",
					"@SuppressWarnings(\"all\")",
					"public class E1<T1 extends Object, T2 extends Number, T3 extends Double> extends Event {",
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
		@DisplayName("With constructor")
		public void noStaticField() throws Exception {
			String source = multilineString(
					"event E1<T1, T2 extends Number, T3 extends Double> {",
					"	val titi : int = 4",
					"	val toto : int",
					"	new(a : int) {",
					"		this.toto = a",
					"	}",
					"}"
					);
			String expected = multilineString(
					"import io.sarl.lang.core.Event;",
					"import io.sarl.lang.core.annotation.SarlElementType;",
					"import io.sarl.lang.core.annotation.SarlSpecification;",
					"import io.sarl.lang.core.annotation.SyntheticMember;",
					"import org.eclipse.xtext.xbase.lib.Pure;",
					"import org.eclipse.xtext.xbase.lib.XbaseGenerated;",
					"import org.eclipse.xtext.xbase.lib.util.ToStringBuilder;",
					"",
					"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
					"@SarlElementType(" + SarlPackage.SARL_EVENT + ")",
					"@XbaseGenerated",
					"@SuppressWarnings(\"all\")",
					"public class E1<T1 extends Object, T2 extends Number, T3 extends Double> extends Event {",
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
		@DisplayName("Without modifier")
		public void eventmodifier_none() throws Exception {
			getCompileHelper().assertCompilesTo(
				multilineString(
					"event E1<T1, T2 extends Number, T3 extends Double>"
				),
				multilineString(
					"import io.sarl.lang.core.Address;",
					"import io.sarl.lang.core.Event;",
					"import io.sarl.lang.core.annotation.SarlElementType;",
					"import io.sarl.lang.core.annotation.SarlSpecification;",
					"import io.sarl.lang.core.annotation.SyntheticMember;",
					"import org.eclipse.xtext.xbase.lib.XbaseGenerated;",
					"",
					"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
					"@SarlElementType(" + SarlPackage.SARL_EVENT + ")",
					"@XbaseGenerated",
					"@SuppressWarnings(\"all\")",
					"public class E1<T1 extends Object, T2 extends Number, T3 extends Double> extends Event {",
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
		@DisplayName("With public modifier")
		public void eventmodifier_public() throws Exception {
			getCompileHelper().assertCompilesTo(
				multilineString(
					"public event E1<T1, T2 extends Number, T3 extends Double>"
				),
				multilineString(
					"import io.sarl.lang.core.Address;",
					"import io.sarl.lang.core.Event;",
					"import io.sarl.lang.core.annotation.SarlElementType;",
					"import io.sarl.lang.core.annotation.SarlSpecification;",
					"import io.sarl.lang.core.annotation.SyntheticMember;",
					"import org.eclipse.xtext.xbase.lib.XbaseGenerated;",
					"",
					"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
					"@SarlElementType(" + SarlPackage.SARL_EVENT + ")",
					"@XbaseGenerated",
					"@SuppressWarnings(\"all\")",
					"public class E1<T1 extends Object, T2 extends Number, T3 extends Double> extends Event {",
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
		@DisplayName("With abstract modifier")
		public void eventmodifier_abstract() throws Exception {
			getCompileHelper().assertCompilesTo(
				multilineString(
					"abstract event E1<T1, T2 extends Number, T3 extends Double>"
				),
				multilineString(
					"import io.sarl.lang.core.Address;",
					"import io.sarl.lang.core.Event;",
					"import io.sarl.lang.core.annotation.SarlElementType;",
					"import io.sarl.lang.core.annotation.SarlSpecification;",
					"import io.sarl.lang.core.annotation.SyntheticMember;",
					"import org.eclipse.xtext.xbase.lib.XbaseGenerated;",
					"",
					"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
					"@SarlElementType(" + SarlPackage.SARL_EVENT + ")",
					"@XbaseGenerated",
					"@SuppressWarnings(\"all\")",
					"public abstract class E1<T1 extends Object, T2 extends Number, T3 extends Double> extends Event {",
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
		@DisplayName("With package modifier")
		public void eventmodifier_package() throws Exception {
			getCompileHelper().assertCompilesTo(
				multilineString(
					"package event E1<T1, T2 extends Number, T3 extends Double>"
				),
				multilineString(
					"import io.sarl.lang.core.Address;",
					"import io.sarl.lang.core.Event;",
					"import io.sarl.lang.core.annotation.SarlElementType;",
					"import io.sarl.lang.core.annotation.SarlSpecification;",
					"import io.sarl.lang.core.annotation.SyntheticMember;",
					"import org.eclipse.xtext.xbase.lib.XbaseGenerated;",
					"",
					"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
					"@SarlElementType(" + SarlPackage.SARL_EVENT + ")",
					"@XbaseGenerated",
					"@SuppressWarnings(\"all\")",
					"class E1<T1 extends Object, T2 extends Number, T3 extends Double> extends Event {",
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
		@DisplayName("With final modifier")
		public void eventmodifier_final() throws Exception {
			getCompileHelper().assertCompilesTo(
				multilineString(
					"final event E1<T1, T2 extends Number, T3 extends Double>"
				),
				multilineString(
					"import io.sarl.lang.core.Address;",
					"import io.sarl.lang.core.Event;",
					"import io.sarl.lang.core.annotation.SarlElementType;",
					"import io.sarl.lang.core.annotation.SarlSpecification;",
					"import io.sarl.lang.core.annotation.SyntheticMember;",
					"import org.eclipse.xtext.xbase.lib.XbaseGenerated;",
					"",
					"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
					"@SarlElementType(" + SarlPackage.SARL_EVENT + ")",
					"@XbaseGenerated",
					"@SuppressWarnings(\"all\")",
					"public final class E1<T1 extends Object, T2 extends Number, T3 extends Double> extends Event {",
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
		@DisplayName("Without field modifier")
		public void fieldmodifier_none() throws Exception {
			getCompileHelper().assertCompilesTo(
				multilineString(
					"event E1<T1, T2 extends Number, T3 extends Double> {",
					"	var field : int",
					"}"
				),
				multilineString(
					"import io.sarl.lang.core.Address;",
					"import io.sarl.lang.core.Event;",
					"import io.sarl.lang.core.annotation.SarlElementType;",
					"import io.sarl.lang.core.annotation.SarlSpecification;",
					"import io.sarl.lang.core.annotation.SyntheticMember;",
					"import org.eclipse.xtext.xbase.lib.Pure;",
					"import org.eclipse.xtext.xbase.lib.XbaseGenerated;",
					"import org.eclipse.xtext.xbase.lib.util.ToStringBuilder;",
					"",
					"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
					"@SarlElementType(" + SarlPackage.SARL_EVENT + ")",
					"@XbaseGenerated",
					"@SuppressWarnings(\"all\")",
					"public class E1<T1 extends Object, T2 extends Number, T3 extends Double> extends Event {",
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
		@DisplayName("With public field modifier")
		public void fieldmodifier_public() throws Exception {
			getCompileHelper().assertCompilesTo(
				multilineString(
					"event E1<T1, T2 extends Number, T3 extends Double> {",
					"	public var field : int",
					"}"
				),
				multilineString(
					"import io.sarl.lang.core.Address;",
					"import io.sarl.lang.core.Event;",
					"import io.sarl.lang.core.annotation.SarlElementType;",
					"import io.sarl.lang.core.annotation.SarlSpecification;",
					"import io.sarl.lang.core.annotation.SyntheticMember;",
					"import org.eclipse.xtext.xbase.lib.Pure;",
					"import org.eclipse.xtext.xbase.lib.XbaseGenerated;",
					"import org.eclipse.xtext.xbase.lib.util.ToStringBuilder;",
					"",
					"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
					"@SarlElementType(" + SarlPackage.SARL_EVENT + ")",
					"@XbaseGenerated",
					"@SuppressWarnings(\"all\")",
					"public class E1<T1 extends Object, T2 extends Number, T3 extends Double> extends Event {",
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
		@DisplayName("Without constructor modifier")
		public void constructormodifier_none() throws Exception {
			getCompileHelper().assertCompilesTo(
				multilineString(
					"event E1<T1, T2 extends Number, T3 extends Double> {",
					"	new { super(null) }",
					"}"
				),
				multilineString(
					"import io.sarl.lang.core.Event;",
					"import io.sarl.lang.core.annotation.SarlElementType;",
					"import io.sarl.lang.core.annotation.SarlSpecification;",
					"import io.sarl.lang.core.annotation.SyntheticMember;",
					"import org.eclipse.xtext.xbase.lib.XbaseGenerated;",
					"",
					"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
					"@SarlElementType(" + SarlPackage.SARL_EVENT + ")",
					"@XbaseGenerated",
					"@SuppressWarnings(\"all\")",
					"public class E1<T1 extends Object, T2 extends Number, T3 extends Double> extends Event {",
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
		@DisplayName("With public constructor modifier")
		public void constructormodifier_public() throws Exception {
			getCompileHelper().assertCompilesTo(
				multilineString(
					"event E1<T1, T2 extends Number, T3 extends Double> {",
					"	public new { super(null) }",
					"}"
				),
				multilineString(
					"import io.sarl.lang.core.Event;",
					"import io.sarl.lang.core.annotation.SarlElementType;",
					"import io.sarl.lang.core.annotation.SarlSpecification;",
					"import io.sarl.lang.core.annotation.SyntheticMember;",
					"import org.eclipse.xtext.xbase.lib.XbaseGenerated;",
					"",
					"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
					"@SarlElementType(" + SarlPackage.SARL_EVENT + ")",
					"@XbaseGenerated",
					"@SuppressWarnings(\"all\")",
					"public class E1<T1 extends Object, T2 extends Number, T3 extends Double> extends Event {",
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
		@DisplayName("With private constructor modifier")
		public void constructormodifier_private() throws Exception {
			getCompileHelper().assertCompilesTo(
				multilineString(
					"event E1<T1, T2 extends Number, T3 extends Double> {",
					"	private new { super(null) }",
					"}"
				),
				multilineString(
					"import io.sarl.lang.core.Event;",
					"import io.sarl.lang.core.annotation.SarlElementType;",
					"import io.sarl.lang.core.annotation.SarlSpecification;",
					"import io.sarl.lang.core.annotation.SyntheticMember;",
					"import org.eclipse.xtext.xbase.lib.XbaseGenerated;",
					"",
					"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
					"@SarlElementType(" + SarlPackage.SARL_EVENT + ")",
					"@XbaseGenerated",
					"@SuppressWarnings(\"all\")",
					"public class E1<T1 extends Object, T2 extends Number, T3 extends Double> extends Event {",
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
		@DisplayName("With package constructor modifier")
		public void constructormodifier_package() throws Exception {
			getCompileHelper().assertCompilesTo(
				multilineString(
					"event E1<T1, T2 extends Number, T3 extends Double> {",
					"	package new { super(null) }",
					"}"
				),
				multilineString(
					"import io.sarl.lang.core.Event;",
					"import io.sarl.lang.core.annotation.SarlElementType;",
					"import io.sarl.lang.core.annotation.SarlSpecification;",
					"import io.sarl.lang.core.annotation.SyntheticMember;",
					"import org.eclipse.xtext.xbase.lib.XbaseGenerated;",
					"",
					"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
					"@SarlElementType(" + SarlPackage.SARL_EVENT + ")",
					"@XbaseGenerated",
					"@SuppressWarnings(\"all\")",
					"public class E1<T1 extends Object, T2 extends Number, T3 extends Double> extends Event {",
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
		@DisplayName("With protected constructor modifier")
		public void constructormodifier_protected() throws Exception {
			getCompileHelper().assertCompilesTo(
				multilineString(
					"event E1<T1, T2 extends Number, T3 extends Double> {",
					"	protected new { super(null) }",
					"}"
				),
				multilineString(
					"import io.sarl.lang.core.Event;",
					"import io.sarl.lang.core.annotation.SarlElementType;",
					"import io.sarl.lang.core.annotation.SarlSpecification;",
					"import io.sarl.lang.core.annotation.SyntheticMember;",
					"import org.eclipse.xtext.xbase.lib.XbaseGenerated;",
					"",
					"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
					"@SarlElementType(" + SarlPackage.SARL_EVENT + ")",
					"@XbaseGenerated",
					"@SuppressWarnings(\"all\")",
					"public class E1<T1 extends Object, T2 extends Number, T3 extends Double> extends Event {",
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

	@DisplayName("Same super generic prototype")
	@Nested
	public class SameGenericEventInheritance extends AbstractSarlTest {

		@Test
		@DisplayName("Empty event with block")
		public void basicCompile_withBlock() throws Exception {
			String source = multilineString(
					"event E1<T1, T2 extends Number, T3 extends Double> { }",
					"event E2<T1 extends Object, T2 extends Number, T3 extends Double> extends E1<T1, T2, T3> { }");
			String expected = multilineString(
					"import io.sarl.lang.core.Address;",
					"import io.sarl.lang.core.annotation.SarlElementType;",
					"import io.sarl.lang.core.annotation.SarlSpecification;",
					"import io.sarl.lang.core.annotation.SyntheticMember;",
					"import org.eclipse.xtext.xbase.lib.XbaseGenerated;",
					"",
					"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
					"@SarlElementType(" + SarlPackage.SARL_EVENT + ")",
					"@XbaseGenerated",
					"@SuppressWarnings(\"all\")",
					"public class E2<T1 extends Object, T2 extends Number, T3 extends Double> extends E1<T1, T2, T3> {",
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
					"  private static final long serialVersionUID = 500821837L;",
					"}",
					""
					);
			getCompileHelper().assertCompilesTo("E2", source, expected);
		}
	
		@Test
		@DisplayName("Empty event without block")
		public void basicCompile_withoutBlock() throws Exception {
			String source = multilineString(
					"event E1<T1, T2 extends Number, T3 extends Double> { }",
					"event E2<T1 extends Object, T2 extends Number, T3 extends Double> extends E1<T1, T2, T3>");
			String expected = multilineString(
					"import io.sarl.lang.core.Address;",
					"import io.sarl.lang.core.annotation.SarlElementType;",
					"import io.sarl.lang.core.annotation.SarlSpecification;",
					"import io.sarl.lang.core.annotation.SyntheticMember;",
					"import org.eclipse.xtext.xbase.lib.XbaseGenerated;",
					"",
					"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
					"@SarlElementType(" + SarlPackage.SARL_EVENT + ")",
					"@XbaseGenerated",
					"@SuppressWarnings(\"all\")",
					"public class E2<T1 extends Object, T2 extends Number, T3 extends Double> extends E1<T1, T2, T3> {",
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
					"  private static final long serialVersionUID = 500821837L;",
					"}",
					""
					);
			getCompileHelper().assertCompilesTo("E2", source, expected);
		}
	
		@Test
		@DisplayName("With string field")
		public void withVarAttributesCompile() throws Exception {
			String source = multilineString(
					"event E1<T1, T2 extends Number, T3 extends Double> { }",
					"event E2<T1, T2 extends Number, T3 extends Double> extends E1<T1, T2, T3> {",
					"  var name : String",
					"}");
			String expected = multilineString(
					"import io.sarl.lang.core.Address;",
					"import io.sarl.lang.core.annotation.SarlElementType;",
					"import io.sarl.lang.core.annotation.SarlSpecification;",
					"import io.sarl.lang.core.annotation.SyntheticMember;",
					"import java.util.Objects;",
					"import org.eclipse.xtext.xbase.lib.Pure;",
					"import org.eclipse.xtext.xbase.lib.XbaseGenerated;",
					"import org.eclipse.xtext.xbase.lib.util.ToStringBuilder;",
					"",
					"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
					"@SarlElementType(" + SarlPackage.SARL_EVENT + ")",
					"@XbaseGenerated",
					"@SuppressWarnings(\"all\")",
					"public class E2<T1 extends Object, T2 extends Number, T3 extends Double> extends E1<T1, T2, T3> {",
					"  public String name;",
					"  ",
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
					"    E2 other = (E2) obj;",
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
					"   * Returns a String representation of the E2 event's attributes only.",
					"   */",
					"  @SyntheticMember",
					"  @Pure",
					"  protected void toString(final ToStringBuilder builder) {",
					"    super.toString(builder);",
					"    builder.add(\"name\", this.name);",
					"  }",
					"  ",
					"  @SyntheticMember",
					"  private static final long serialVersionUID = 1699455037L;",
					"}",
					""
					);
			getCompileHelper().assertCompilesTo("E2", source, expected);
		}
	
		@Test
		@DisplayName("With constructor")
		public void noStaticField() throws Exception {
			String source = multilineString(
					"event E1<T1, T2 extends Number, T3 extends Double> { }",
					"event E2<T1, T2 extends Number, T3 extends Double> extends E1<T1, T2, T3> {",
					"	val titi : int = 4",
					"	val toto : int",
					"	new(a : int) {",
					"		this.toto = a",
					"	}",
					"}");
			String expected = multilineString(
					"import io.sarl.lang.core.annotation.SarlElementType;",
					"import io.sarl.lang.core.annotation.SarlSpecification;",
					"import io.sarl.lang.core.annotation.SyntheticMember;",
					"import org.eclipse.xtext.xbase.lib.Pure;",
					"import org.eclipse.xtext.xbase.lib.XbaseGenerated;",
					"import org.eclipse.xtext.xbase.lib.util.ToStringBuilder;",
					"",
					"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
					"@SarlElementType(" + SarlPackage.SARL_EVENT + ")",
					"@XbaseGenerated",
					"@SuppressWarnings(\"all\")",
					"public class E2<T1 extends Object, T2 extends Number, T3 extends Double> extends E1<T1, T2, T3> {",
					"  public final int titi = 4;",
					"  ",
					"  public final int toto;",
					"  ",
					"  public E2(final int a) {",
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
					"    E2 other = (E2) obj;",
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
					"   * Returns a String representation of the E2 event's attributes only.",
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
					"  private static final long serialVersionUID = 511397715L;",
					"}",
					""
					);
			getCompileHelper().assertCompilesTo("E2", source, expected);
		}

		@Test
		@DisplayName("Without modifier")
		public void eventmodifier_none() throws Exception {
			getCompileHelper().assertCompilesTo("E2", 
				multilineString(
						"event E1<T1, T2 extends Number, T3 extends Double> { }",
						"event E2<T1, T2 extends Number, T3 extends Double> extends E1<T1, T2, T3>"),
				multilineString(
					"import io.sarl.lang.core.Address;",
					"import io.sarl.lang.core.annotation.SarlElementType;",
					"import io.sarl.lang.core.annotation.SarlSpecification;",
					"import io.sarl.lang.core.annotation.SyntheticMember;",
					"import org.eclipse.xtext.xbase.lib.XbaseGenerated;",
					"",
					"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
					"@SarlElementType(" + SarlPackage.SARL_EVENT + ")",
					"@XbaseGenerated",
					"@SuppressWarnings(\"all\")",
					"public class E2<T1 extends Object, T2 extends Number, T3 extends Double> extends E1<T1, T2, T3> {",
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
					"  private static final long serialVersionUID = 500821837L;",
					"}",
					""));
		}
	
		@Test
		@DisplayName("With public modifier")
		public void eventmodifier_public() throws Exception {
			getCompileHelper().assertCompilesTo("E2", 
				multilineString(
					"event E1<T1, T2 extends Number, T3 extends Double> { }",
					"public event E2<T1, T2 extends Number, T3 extends Double> extends E1<T1, T2, T3>"
				),
				multilineString(
					"import io.sarl.lang.core.Address;",
					"import io.sarl.lang.core.annotation.SarlElementType;",
					"import io.sarl.lang.core.annotation.SarlSpecification;",
					"import io.sarl.lang.core.annotation.SyntheticMember;",
					"import org.eclipse.xtext.xbase.lib.XbaseGenerated;",
					"",
					"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
					"@SarlElementType(" + SarlPackage.SARL_EVENT + ")",
					"@XbaseGenerated",
					"@SuppressWarnings(\"all\")",
					"public class E2<T1 extends Object, T2 extends Number, T3 extends Double> extends E1<T1, T2, T3> {",
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
					"  private static final long serialVersionUID = 500821837L;",
					"}",
					""));
		}

		@Test
		@DisplayName("With abstract modifier")
		public void eventmodifier_abstract() throws Exception {
			getCompileHelper().assertCompilesTo("E2", 
				multilineString(
					"event E1<T1, T2 extends Number, T3 extends Double> { }",
					"abstract event E2<T1, T2 extends Number, T3 extends Double> extends E1<T1, T2, T3>"
				),
				multilineString(
					"import io.sarl.lang.core.Address;",
					"import io.sarl.lang.core.annotation.SarlElementType;",
					"import io.sarl.lang.core.annotation.SarlSpecification;",
					"import io.sarl.lang.core.annotation.SyntheticMember;",
					"import org.eclipse.xtext.xbase.lib.XbaseGenerated;",
					"",
					"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
					"@SarlElementType(" + SarlPackage.SARL_EVENT + ")",
					"@XbaseGenerated",
					"@SuppressWarnings(\"all\")",
					"public abstract class E2<T1 extends Object, T2 extends Number, T3 extends Double> extends E1<T1, T2, T3> {",
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
					"  private static final long serialVersionUID = 500821837L;",
					"}",
					""));
		}
	
		@Test
		@DisplayName("With package modifier")
		public void eventmodifier_package() throws Exception {
			getCompileHelper().assertCompilesTo("E2", 
				multilineString(
					"event E1<T1, T2 extends Number, T3 extends Double> { }",
					"package event E2<T1, T2 extends Number, T3 extends Double> extends E1<T1, T2, T3>"
				),
				multilineString(
					"import io.sarl.lang.core.Address;",
					"import io.sarl.lang.core.annotation.SarlElementType;",
					"import io.sarl.lang.core.annotation.SarlSpecification;",
					"import io.sarl.lang.core.annotation.SyntheticMember;",
					"import org.eclipse.xtext.xbase.lib.XbaseGenerated;",
					"",
					"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
					"@SarlElementType(" + SarlPackage.SARL_EVENT + ")",
					"@XbaseGenerated",
					"@SuppressWarnings(\"all\")",
					"class E2<T1 extends Object, T2 extends Number, T3 extends Double> extends E1<T1, T2, T3> {",
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
					"  private static final long serialVersionUID = 500821837L;",
					"}",
					""));
		}
	
		@Test
		@DisplayName("With final modifier")
		public void eventmodifier_final() throws Exception {
			getCompileHelper().assertCompilesTo("E2", 
				multilineString(
					"event E1<T1, T2 extends Number, T3 extends Double> { }",
					"final event E2<T1, T2 extends Number, T3 extends Double> extends E1<T1, T2, T3>"
				),
				multilineString(
					"import io.sarl.lang.core.Address;",
					"import io.sarl.lang.core.annotation.SarlElementType;",
					"import io.sarl.lang.core.annotation.SarlSpecification;",
					"import io.sarl.lang.core.annotation.SyntheticMember;",
					"import org.eclipse.xtext.xbase.lib.XbaseGenerated;",
					"",
					"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
					"@SarlElementType(" + SarlPackage.SARL_EVENT + ")",
					"@XbaseGenerated",
					"@SuppressWarnings(\"all\")",
					"public final class E2<T1 extends Object, T2 extends Number, T3 extends Double> extends E1<T1, T2, T3> {",
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
					"  private static final long serialVersionUID = 500821837L;",
					"}",
					""));
		}
	
		@Test
		@DisplayName("Without field modifier")
		public void fieldmodifier_none() throws Exception {
			getCompileHelper().assertCompilesTo("E2", 
				multilineString(
					"event E1<T1, T2 extends Number, T3 extends Double> { }",
					"event E2<T1, T2 extends Number, T3 extends Double> extends E1<T1, T2, T3>{",
					"	var field : int",
					"}"
				),
				multilineString(
					"import io.sarl.lang.core.Address;",
					"import io.sarl.lang.core.annotation.SarlElementType;",
					"import io.sarl.lang.core.annotation.SarlSpecification;",
					"import io.sarl.lang.core.annotation.SyntheticMember;",
					"import org.eclipse.xtext.xbase.lib.Pure;",
					"import org.eclipse.xtext.xbase.lib.XbaseGenerated;",
					"import org.eclipse.xtext.xbase.lib.util.ToStringBuilder;",
					"",
					"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
					"@SarlElementType(" + SarlPackage.SARL_EVENT + ")",
					"@XbaseGenerated",
					"@SuppressWarnings(\"all\")",
					"public class E2<T1 extends Object, T2 extends Number, T3 extends Double> extends E1<T1, T2, T3> {",
					"  public int field;",
					"  ",
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
					"    E2 other = (E2) obj;",
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
					"   * Returns a String representation of the E2 event's attributes only.",
					"   */",
					"  @SyntheticMember",
					"  @Pure",
					"  protected void toString(final ToStringBuilder builder) {",
					"    super.toString(builder);",
					"    builder.add(\"field\", this.field);",
					"  }",
					"  ",
					"  @SyntheticMember",
					"  private static final long serialVersionUID = 598353974L;",
					"}",
					""));
		}

		@Test
		@DisplayName("With public field modifier")
		public void fieldmodifier_public() throws Exception {
			getCompileHelper().assertCompilesTo("E2", 
				multilineString(
					"event E1<T1, T2 extends Number, T3 extends Double> { }",
					"event E2<T1, T2 extends Number, T3 extends Double> extends E1<T1, T2, T3>{",
					"	public var field : int",
					"}"
				),
				multilineString(
					"import io.sarl.lang.core.Address;",
					"import io.sarl.lang.core.annotation.SarlElementType;",
					"import io.sarl.lang.core.annotation.SarlSpecification;",
					"import io.sarl.lang.core.annotation.SyntheticMember;",
					"import org.eclipse.xtext.xbase.lib.Pure;",
					"import org.eclipse.xtext.xbase.lib.XbaseGenerated;",
					"import org.eclipse.xtext.xbase.lib.util.ToStringBuilder;",
					"",
					"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
					"@SarlElementType(" + SarlPackage.SARL_EVENT + ")",
					"@XbaseGenerated",
					"@SuppressWarnings(\"all\")",
					"public class E2<T1 extends Object, T2 extends Number, T3 extends Double> extends E1<T1, T2, T3> {",
					"  public int field;",
					"  ",
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
					"    E2 other = (E2) obj;",
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
					"   * Returns a String representation of the E2 event's attributes only.",
					"   */",
					"  @SyntheticMember",
					"  @Pure",
					"  protected void toString(final ToStringBuilder builder) {",
					"    super.toString(builder);",
					"    builder.add(\"field\", this.field);",
					"  }",
					"  ",
					"  @SyntheticMember",
					"  private static final long serialVersionUID = 598353974L;",
					"}",
					""));
		}

		@Test
		@DisplayName("Without constructor modifier")
		public void constructormodifier_none() throws Exception {
			getCompileHelper().assertCompilesTo("E2", 
				multilineString(
					"event E1<T1, T2 extends Number, T3 extends Double> { }",
					"event E2<T1, T2 extends Number, T3 extends Double> extends E1<T1, T2, T3>{",
					"	new { super(null) }",
					"}"
				),
				multilineString(
					"import io.sarl.lang.core.annotation.SarlElementType;",
					"import io.sarl.lang.core.annotation.SarlSpecification;",
					"import io.sarl.lang.core.annotation.SyntheticMember;",
					"import org.eclipse.xtext.xbase.lib.XbaseGenerated;",
					"",
					"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
					"@SarlElementType(" + SarlPackage.SARL_EVENT + ")",
					"@XbaseGenerated",
					"@SuppressWarnings(\"all\")",
					"public class E2<T1 extends Object, T2 extends Number, T3 extends Double> extends E1<T1, T2, T3> {",
					"  public E2() {",
					"    super(null);",
					"  }",
					"  ",
					"  @SyntheticMember",
					"  private static final long serialVersionUID = 500824066L;",
					"}",
					""));
		}
	
		@Test
		@DisplayName("With public constructor modifier")
		public void constructormodifier_public() throws Exception {
			getCompileHelper().assertCompilesTo("E2", 
				multilineString(
					"event E1<T1, T2 extends Number, T3 extends Double> { }",
					"event E2<T1, T2 extends Number, T3 extends Double> extends E1<T1, T2, T3>{",
					"	public new { super(null) }",
					"}"
				),
				multilineString(
					"import io.sarl.lang.core.annotation.SarlElementType;",
					"import io.sarl.lang.core.annotation.SarlSpecification;",
					"import io.sarl.lang.core.annotation.SyntheticMember;",
					"import org.eclipse.xtext.xbase.lib.XbaseGenerated;",
					"",
					"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
					"@SarlElementType(" + SarlPackage.SARL_EVENT + ")",
					"@XbaseGenerated",
					"@SuppressWarnings(\"all\")",
					"public class E2<T1 extends Object, T2 extends Number, T3 extends Double> extends E1<T1, T2, T3> {",
					"  public E2() {",
					"    super(null);",
					"  }",
					"  ",
					"  @SyntheticMember",
					"  private static final long serialVersionUID = 500824066L;",
					"}",
					""));
		}

		@Test
		@DisplayName("With private constructor modifier")
		public void constructormodifier_private() throws Exception {
			getCompileHelper().assertCompilesTo("E2", 
				multilineString(
					"event E1<T1, T2 extends Number, T3 extends Double> { }",
					"event E2<T1, T2 extends Number, T3 extends Double> extends E1<T1, T2, T3>{",
					"	private new { super(null) }",
					"}"
				),
				multilineString(
					"import io.sarl.lang.core.annotation.SarlElementType;",
					"import io.sarl.lang.core.annotation.SarlSpecification;",
					"import io.sarl.lang.core.annotation.SyntheticMember;",
					"import org.eclipse.xtext.xbase.lib.XbaseGenerated;",
					"",
					"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
					"@SarlElementType(" + SarlPackage.SARL_EVENT + ")",
					"@XbaseGenerated",
					"@SuppressWarnings(\"all\")",
					"public class E2<T1 extends Object, T2 extends Number, T3 extends Double> extends E1<T1, T2, T3> {",
					"  private E2() {",
					"    super(null);",
					"  }",
					"  ",
					"  @SyntheticMember",
					"  private static final long serialVersionUID = 500824066L;",
					"}",
					""));
		}
	
		@Test
		@DisplayName("With package constructor modifier")
		public void constructormodifier_package() throws Exception {
			getCompileHelper().assertCompilesTo("E2", 
				multilineString(
					"event E1<T1, T2 extends Number, T3 extends Double> { }",
					"event E2<T1, T2 extends Number, T3 extends Double> extends E1<T1, T2, T3>{",
					"	package new { super(null) }",
					"}"
				),
				multilineString(
					"import io.sarl.lang.core.annotation.SarlElementType;",
					"import io.sarl.lang.core.annotation.SarlSpecification;",
					"import io.sarl.lang.core.annotation.SyntheticMember;",
					"import org.eclipse.xtext.xbase.lib.XbaseGenerated;",
					"",
					"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
					"@SarlElementType(" + SarlPackage.SARL_EVENT + ")",
					"@XbaseGenerated",
					"@SuppressWarnings(\"all\")",
					"public class E2<T1 extends Object, T2 extends Number, T3 extends Double> extends E1<T1, T2, T3> {",
					"  E2() {",
					"    super(null);",
					"  }",
					"  ",
					"  @SyntheticMember",
					"  private static final long serialVersionUID = 500824066L;",
					"}",
					""));
		}

		@Test
		@DisplayName("With protected constructor modifier")
		public void constructormodifier_protected() throws Exception {
			getCompileHelper().assertCompilesTo("E2", 
				multilineString(
					"event E1<T1, T2 extends Number, T3 extends Double> { }",
					"event E2<T1, T2 extends Number, T3 extends Double> extends E1<T1, T2, T3>{",
					"	protected new { super(null) }",
					"}"
				),
				multilineString(
					"import io.sarl.lang.core.annotation.SarlElementType;",
					"import io.sarl.lang.core.annotation.SarlSpecification;",
					"import io.sarl.lang.core.annotation.SyntheticMember;",
					"import org.eclipse.xtext.xbase.lib.XbaseGenerated;",
					"",
					"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
					"@SarlElementType(" + SarlPackage.SARL_EVENT + ")",
					"@XbaseGenerated",
					"@SuppressWarnings(\"all\")",
					"public class E2<T1 extends Object, T2 extends Number, T3 extends Double> extends E1<T1, T2, T3> {",
					"  protected E2() {",
					"    super(null);",
					"  }",
					"  ",
					"  @SyntheticMember",
					"  private static final long serialVersionUID = 500824066L;",
					"}",
					""));
		}

	}

	@DisplayName("Fixed super generic prototype")
	@Nested
	public class FixedGenericEventInheritance extends AbstractSarlTest {

		@Test
		@DisplayName("Empty event with block")
		public void basicCompile_withBlock() throws Exception {
			String source = multilineString(
					"event E1<T1, T2 extends Number, T3 extends Double> { }",
					"event E2 extends E1<String, Float, Double> { }");
			String expected = multilineString(
					"import io.sarl.lang.core.Address;",
					"import io.sarl.lang.core.annotation.SarlElementType;",
					"import io.sarl.lang.core.annotation.SarlSpecification;",
					"import io.sarl.lang.core.annotation.SyntheticMember;",
					"import org.eclipse.xtext.xbase.lib.XbaseGenerated;",
					"",
					"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
					"@SarlElementType(" + SarlPackage.SARL_EVENT + ")",
					"@XbaseGenerated",
					"@SuppressWarnings(\"all\")",
					"public class E2 extends E1<String, Float, Double> {",
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
					"  private static final long serialVersionUID = -2050017593L;",
					"}",
					""
					);
			getCompileHelper().assertCompilesTo("E2", source, expected);
		}
	
		@Test
		@DisplayName("Empty event without block")
		public void basicCompile_withoutBlock() throws Exception {
			String source = multilineString(
					"event E1<T1, T2 extends Number, T3 extends Double> { }",
					"event E2 extends E1<String, Float, Double>");
			String expected = multilineString(
					"import io.sarl.lang.core.Address;",
					"import io.sarl.lang.core.annotation.SarlElementType;",
					"import io.sarl.lang.core.annotation.SarlSpecification;",
					"import io.sarl.lang.core.annotation.SyntheticMember;",
					"import org.eclipse.xtext.xbase.lib.XbaseGenerated;",
					"",
					"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
					"@SarlElementType(" + SarlPackage.SARL_EVENT + ")",
					"@XbaseGenerated",
					"@SuppressWarnings(\"all\")",
					"public class E2 extends E1<String, Float, Double> {",
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
					"  private static final long serialVersionUID = -2050017593L;",
					"}",
					""
					);
			getCompileHelper().assertCompilesTo("E2", source, expected);
		}
	
		@Test
		@DisplayName("With string field")
		public void withVarAttributesCompile() throws Exception {
			String source = multilineString(
					"event E1<T1, T2 extends Number, T3 extends Double> { }",
					"event E2 extends E1<String, Float, Double> {",
					"  var name : String",
					"}");
			String expected = multilineString(
					"import io.sarl.lang.core.Address;",
					"import io.sarl.lang.core.annotation.SarlElementType;",
					"import io.sarl.lang.core.annotation.SarlSpecification;",
					"import io.sarl.lang.core.annotation.SyntheticMember;",
					"import java.util.Objects;",
					"import org.eclipse.xtext.xbase.lib.Pure;",
					"import org.eclipse.xtext.xbase.lib.XbaseGenerated;",
					"import org.eclipse.xtext.xbase.lib.util.ToStringBuilder;",
					"",
					"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
					"@SarlElementType(" + SarlPackage.SARL_EVENT + ")",
					"@XbaseGenerated",
					"@SuppressWarnings(\"all\")",
					"public class E2 extends E1<String, Float, Double> {",
					"  public String name;",
					"  ",
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
					"    E2 other = (E2) obj;",
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
					"   * Returns a String representation of the E2 event's attributes only.",
					"   */",
					"  @SyntheticMember",
					"  @Pure",
					"  protected void toString(final ToStringBuilder builder) {",
					"    super.toString(builder);",
					"    builder.add(\"name\", this.name);",
					"  }",
					"  ",
					"  @SyntheticMember",
					"  private static final long serialVersionUID = -851384393L;",
					"}",
					""
					);
			getCompileHelper().assertCompilesTo("E2", source, expected);
		}
	
		@Test
		@DisplayName("With constructor")
		public void noStaticField() throws Exception {
			String source = multilineString(
					"event E1<T1, T2 extends Number, T3 extends Double> { }",
					"event E2 extends E1<String, Float, Double> {",
					"	val titi : int = 4",
					"	val toto : int",
					"	new(a : int) {",
					"		this.toto = a",
					"	}",
					"}");
			String expected = multilineString(
					"import io.sarl.lang.core.annotation.SarlElementType;",
					"import io.sarl.lang.core.annotation.SarlSpecification;",
					"import io.sarl.lang.core.annotation.SyntheticMember;",
					"import org.eclipse.xtext.xbase.lib.Pure;",
					"import org.eclipse.xtext.xbase.lib.XbaseGenerated;",
					"import org.eclipse.xtext.xbase.lib.util.ToStringBuilder;",
					"",
					"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
					"@SarlElementType(" + SarlPackage.SARL_EVENT + ")",
					"@XbaseGenerated",
					"@SuppressWarnings(\"all\")",
					"public class E2 extends E1<String, Float, Double> {",
					"  public final int titi = 4;",
					"  ",
					"  public final int toto;",
					"  ",
					"  public E2(final int a) {",
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
					"    E2 other = (E2) obj;",
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
					"   * Returns a String representation of the E2 event's attributes only.",
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
					"  private static final long serialVersionUID = -2039441715L;",
					"}",
					""
					);
			getCompileHelper().assertCompilesTo("E2", source, expected);
		}

		@Test
		@DisplayName("Without modifier")
		public void eventmodifier_none() throws Exception {
			getCompileHelper().assertCompilesTo("E2", 
				multilineString(
						"event E1<T1, T2 extends Number, T3 extends Double> { }",
						"event E2 extends E1<String, Float, Double>"),
				multilineString(
					"import io.sarl.lang.core.Address;",
					"import io.sarl.lang.core.annotation.SarlElementType;",
					"import io.sarl.lang.core.annotation.SarlSpecification;",
					"import io.sarl.lang.core.annotation.SyntheticMember;",
					"import org.eclipse.xtext.xbase.lib.XbaseGenerated;",
					"",
					"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
					"@SarlElementType(" + SarlPackage.SARL_EVENT + ")",
					"@XbaseGenerated",
					"@SuppressWarnings(\"all\")",
					"public class E2 extends E1<String, Float, Double> {",
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
					"  private static final long serialVersionUID = -2050017593L;",
					"}",
					""));
		}
	
		@Test
		@DisplayName("With public modifier")
		public void eventmodifier_public() throws Exception {
			getCompileHelper().assertCompilesTo("E2", 
				multilineString(
					"event E1<T1, T2 extends Number, T3 extends Double> { }",
					"public event E2 extends E1<String, Float, Double>"
				),
				multilineString(
					"import io.sarl.lang.core.Address;",
					"import io.sarl.lang.core.annotation.SarlElementType;",
					"import io.sarl.lang.core.annotation.SarlSpecification;",
					"import io.sarl.lang.core.annotation.SyntheticMember;",
					"import org.eclipse.xtext.xbase.lib.XbaseGenerated;",
					"",
					"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
					"@SarlElementType(" + SarlPackage.SARL_EVENT + ")",
					"@XbaseGenerated",
					"@SuppressWarnings(\"all\")",
					"public class E2 extends E1<String, Float, Double> {",
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
					"  private static final long serialVersionUID = -2050017593L;",
					"}",
					""));
		}

		@Test
		@DisplayName("With abstract modifier")
		public void eventmodifier_abstract() throws Exception {
			getCompileHelper().assertCompilesTo("E2", 
				multilineString(
					"event E1<T1, T2 extends Number, T3 extends Double> { }",
					"abstract event E2 extends E1<String, Float, Double>"
				),
				multilineString(
					"import io.sarl.lang.core.Address;",
					"import io.sarl.lang.core.annotation.SarlElementType;",
					"import io.sarl.lang.core.annotation.SarlSpecification;",
					"import io.sarl.lang.core.annotation.SyntheticMember;",
					"import org.eclipse.xtext.xbase.lib.XbaseGenerated;",
					"",
					"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
					"@SarlElementType(" + SarlPackage.SARL_EVENT + ")",
					"@XbaseGenerated",
					"@SuppressWarnings(\"all\")",
					"public abstract class E2 extends E1<String, Float, Double> {",
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
					"  private static final long serialVersionUID = -2050017593L;",
					"}",
					""));
		}
	
		@Test
		@DisplayName("With package modifier")
		public void eventmodifier_package() throws Exception {
			getCompileHelper().assertCompilesTo("E2", 
				multilineString(
					"event E1<T1, T2 extends Number, T3 extends Double> { }",
					"package event E2 extends E1<String, Float, Double>"
				),
				multilineString(
					"import io.sarl.lang.core.Address;",
					"import io.sarl.lang.core.annotation.SarlElementType;",
					"import io.sarl.lang.core.annotation.SarlSpecification;",
					"import io.sarl.lang.core.annotation.SyntheticMember;",
					"import org.eclipse.xtext.xbase.lib.XbaseGenerated;",
					"",
					"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
					"@SarlElementType(" + SarlPackage.SARL_EVENT + ")",
					"@XbaseGenerated",
					"@SuppressWarnings(\"all\")",
					"class E2 extends E1<String, Float, Double> {",
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
					"  private static final long serialVersionUID = -2050017593L;",
					"}",
					""));
		}
	
		@Test
		@DisplayName("With final modifier")
		public void eventmodifier_final() throws Exception {
			getCompileHelper().assertCompilesTo("E2", 
				multilineString(
					"event E1<T1, T2 extends Number, T3 extends Double> { }",
					"final event E2 extends E1<String, Float, Double>"
				),
				multilineString(
					"import io.sarl.lang.core.Address;",
					"import io.sarl.lang.core.annotation.SarlElementType;",
					"import io.sarl.lang.core.annotation.SarlSpecification;",
					"import io.sarl.lang.core.annotation.SyntheticMember;",
					"import org.eclipse.xtext.xbase.lib.XbaseGenerated;",
					"",
					"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
					"@SarlElementType(" + SarlPackage.SARL_EVENT + ")",
					"@XbaseGenerated",
					"@SuppressWarnings(\"all\")",
					"public final class E2 extends E1<String, Float, Double> {",
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
					"  private static final long serialVersionUID = -2050017593L;",
					"}",
					""));
		}
	
		@Test
		@DisplayName("Without field modifier")
		public void fieldmodifier_none() throws Exception {
			getCompileHelper().assertCompilesTo("E2", 
				multilineString(
					"event E1<T1, T2 extends Number, T3 extends Double> { }",
					"event E2 extends E1<String, Float, Double> {",
					"	var field : int",
					"}"
				),
				multilineString(
					"import io.sarl.lang.core.Address;",
					"import io.sarl.lang.core.annotation.SarlElementType;",
					"import io.sarl.lang.core.annotation.SarlSpecification;",
					"import io.sarl.lang.core.annotation.SyntheticMember;",
					"import org.eclipse.xtext.xbase.lib.Pure;",
					"import org.eclipse.xtext.xbase.lib.XbaseGenerated;",
					"import org.eclipse.xtext.xbase.lib.util.ToStringBuilder;",
					"",
					"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
					"@SarlElementType(" + SarlPackage.SARL_EVENT + ")",
					"@XbaseGenerated",
					"@SuppressWarnings(\"all\")",
					"public class E2 extends E1<String, Float, Double> {",
					"  public int field;",
					"  ",
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
					"    E2 other = (E2) obj;",
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
					"   * Returns a String representation of the E2 event's attributes only.",
					"   */",
					"  @SyntheticMember",
					"  @Pure",
					"  protected void toString(final ToStringBuilder builder) {",
					"    super.toString(builder);",
					"    builder.add(\"field\", this.field);",
					"  }",
					"  ",
					"  @SyntheticMember",
					"  private static final long serialVersionUID = -1952485456L;",
					"}",
					""));
		}

		@Test
		@DisplayName("With public field modifier")
		public void fieldmodifier_public() throws Exception {
			getCompileHelper().assertCompilesTo("E2", 
				multilineString(
					"event E1<T1, T2 extends Number, T3 extends Double> { }",
					"event E2 extends E1<String, Float, Double> {",
					"	public var field : int",
					"}"
				),
				multilineString(
					"import io.sarl.lang.core.Address;",
					"import io.sarl.lang.core.annotation.SarlElementType;",
					"import io.sarl.lang.core.annotation.SarlSpecification;",
					"import io.sarl.lang.core.annotation.SyntheticMember;",
					"import org.eclipse.xtext.xbase.lib.Pure;",
					"import org.eclipse.xtext.xbase.lib.XbaseGenerated;",
					"import org.eclipse.xtext.xbase.lib.util.ToStringBuilder;",
					"",
					"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
					"@SarlElementType(" + SarlPackage.SARL_EVENT + ")",
					"@XbaseGenerated",
					"@SuppressWarnings(\"all\")",
					"public class E2 extends E1<String, Float, Double> {",
					"  public int field;",
					"  ",
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
					"    E2 other = (E2) obj;",
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
					"   * Returns a String representation of the E2 event's attributes only.",
					"   */",
					"  @SyntheticMember",
					"  @Pure",
					"  protected void toString(final ToStringBuilder builder) {",
					"    super.toString(builder);",
					"    builder.add(\"field\", this.field);",
					"  }",
					"  ",
					"  @SyntheticMember",
					"  private static final long serialVersionUID = -1952485456L;",
					"}",
					""));
		}

		@Test
		@DisplayName("Without constructor modifier")
		public void constructormodifier_none() throws Exception {
			getCompileHelper().assertCompilesTo("E2", 
				multilineString(
					"event E1<T1, T2 extends Number, T3 extends Double> { }",
					"event E2 extends E1<String, Float, Double> {",
					"	new { super(null) }",
					"}"
				),
				multilineString(
					"import io.sarl.lang.core.annotation.SarlElementType;",
					"import io.sarl.lang.core.annotation.SarlSpecification;",
					"import io.sarl.lang.core.annotation.SyntheticMember;",
					"import org.eclipse.xtext.xbase.lib.XbaseGenerated;",
					"",
					"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
					"@SarlElementType(" + SarlPackage.SARL_EVENT + ")",
					"@XbaseGenerated",
					"@SuppressWarnings(\"all\")",
					"public class E2 extends E1<String, Float, Double> {",
					"  public E2() {",
					"    super(null);",
					"  }",
					"  ",
					"  @SyntheticMember",
					"  private static final long serialVersionUID = -2050015364L;",
					"}",
					""));
		}
	
		@Test
		@DisplayName("With public constructor modifier")
		public void constructormodifier_public() throws Exception {
			getCompileHelper().assertCompilesTo("E2", 
				multilineString(
					"event E1<T1, T2 extends Number, T3 extends Double> { }",
					"event E2 extends E1<String, Float, Double> {",
					"	public new { super(null) }",
					"}"
				),
				multilineString(
					"import io.sarl.lang.core.annotation.SarlElementType;",
					"import io.sarl.lang.core.annotation.SarlSpecification;",
					"import io.sarl.lang.core.annotation.SyntheticMember;",
					"import org.eclipse.xtext.xbase.lib.XbaseGenerated;",
					"",
					"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
					"@SarlElementType(" + SarlPackage.SARL_EVENT + ")",
					"@XbaseGenerated",
					"@SuppressWarnings(\"all\")",
					"public class E2 extends E1<String, Float, Double> {",
					"  public E2() {",
					"    super(null);",
					"  }",
					"  ",
					"  @SyntheticMember",
					"  private static final long serialVersionUID = -2050015364L;",
					"}",
					""));
		}

		@Test
		@DisplayName("With private constructor modifier")
		public void constructormodifier_private() throws Exception {
			getCompileHelper().assertCompilesTo("E2", 
				multilineString(
					"event E1<T1, T2 extends Number, T3 extends Double> { }",
					"event E2 extends E1<String, Float, Double> {",
					"	private new { super(null) }",
					"}"
				),
				multilineString(
					"import io.sarl.lang.core.annotation.SarlElementType;",
					"import io.sarl.lang.core.annotation.SarlSpecification;",
					"import io.sarl.lang.core.annotation.SyntheticMember;",
					"import org.eclipse.xtext.xbase.lib.XbaseGenerated;",
					"",
					"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
					"@SarlElementType(" + SarlPackage.SARL_EVENT + ")",
					"@XbaseGenerated",
					"@SuppressWarnings(\"all\")",
					"public class E2 extends E1<String, Float, Double> {",
					"  private E2() {",
					"    super(null);",
					"  }",
					"  ",
					"  @SyntheticMember",
					"  private static final long serialVersionUID = -2050015364L;",
					"}",
					""));
		}
	
		@Test
		@DisplayName("With package constructor modifier")
		public void constructormodifier_package() throws Exception {
			getCompileHelper().assertCompilesTo("E2", 
				multilineString(
					"event E1<T1, T2 extends Number, T3 extends Double> { }",
					"event E2 extends E1<String, Float, Double> {",
					"	package new { super(null) }",
					"}"
				),
				multilineString(
					"import io.sarl.lang.core.annotation.SarlElementType;",
					"import io.sarl.lang.core.annotation.SarlSpecification;",
					"import io.sarl.lang.core.annotation.SyntheticMember;",
					"import org.eclipse.xtext.xbase.lib.XbaseGenerated;",
					"",
					"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
					"@SarlElementType(" + SarlPackage.SARL_EVENT + ")",
					"@XbaseGenerated",
					"@SuppressWarnings(\"all\")",
					"public class E2 extends E1<String, Float, Double> {",
					"  E2() {",
					"    super(null);",
					"  }",
					"  ",
					"  @SyntheticMember",
					"  private static final long serialVersionUID = -2050015364L;",
					"}",
					""));
		}

		@Test
		@DisplayName("With protected constructor modifier")
		public void constructormodifier_protected() throws Exception {
			getCompileHelper().assertCompilesTo("E2", 
				multilineString(
					"event E1<T1, T2 extends Number, T3 extends Double> { }",
					"event E2 extends E1<String, Float, Double> {",
					"	protected new { super(null) }",
					"}"
				),
				multilineString(
					"import io.sarl.lang.core.annotation.SarlElementType;",
					"import io.sarl.lang.core.annotation.SarlSpecification;",
					"import io.sarl.lang.core.annotation.SyntheticMember;",
					"import org.eclipse.xtext.xbase.lib.XbaseGenerated;",
					"",
					"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
					"@SarlElementType(" + SarlPackage.SARL_EVENT + ")",
					"@XbaseGenerated",
					"@SuppressWarnings(\"all\")",
					"public class E2 extends E1<String, Float, Double> {",
					"  protected E2() {",
					"    super(null);",
					"  }",
					"  ",
					"  @SyntheticMember",
					"  private static final long serialVersionUID = -2050015364L;",
					"}",
					""));
		}

	}

	@DisplayName("Limited super generic prototype")
	@Nested
	public class LimitedGenericEventInheritance extends AbstractSarlTest {

		@Test
		@DisplayName("Empty event with block")
		public void basicCompile_withBlock() throws Exception {
			String source = multilineString(
					"event E1<T1, T2 extends Number, T3 extends Double> { }",
					"event E2<T2 extends Number> extends E1<String, T2, Double> { }");
			String expected = multilineString(
					"import io.sarl.lang.core.Address;",
					"import io.sarl.lang.core.annotation.SarlElementType;",
					"import io.sarl.lang.core.annotation.SarlSpecification;",
					"import io.sarl.lang.core.annotation.SyntheticMember;",
					"import org.eclipse.xtext.xbase.lib.XbaseGenerated;",
					"",
					"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
					"@SarlElementType(" + SarlPackage.SARL_EVENT + ")",
					"@XbaseGenerated",
					"@SuppressWarnings(\"all\")",
					"public class E2<T2 extends Number> extends E1<String, T2, Double> {",
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
					"  private static final long serialVersionUID = -487619457L;",
					"}",
					""
					);
			getCompileHelper().assertCompilesTo("E2", source, expected);
		}
	
		@Test
		@DisplayName("Empty event without block")
		public void basicCompile_withoutBlock() throws Exception {
			String source = multilineString(
					"event E1<T1, T2 extends Number, T3 extends Double> { }",
					"event E2<T2 extends Number> extends E1<String, T2, Double>");
			String expected = multilineString(
					"import io.sarl.lang.core.Address;",
					"import io.sarl.lang.core.annotation.SarlElementType;",
					"import io.sarl.lang.core.annotation.SarlSpecification;",
					"import io.sarl.lang.core.annotation.SyntheticMember;",
					"import org.eclipse.xtext.xbase.lib.XbaseGenerated;",
					"",
					"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
					"@SarlElementType(" + SarlPackage.SARL_EVENT + ")",
					"@XbaseGenerated",
					"@SuppressWarnings(\"all\")",
					"public class E2<T2 extends Number> extends E1<String, T2, Double> {",
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
					"  private static final long serialVersionUID = -487619457L;",
					"}",
					""
					);
			getCompileHelper().assertCompilesTo("E2", source, expected);
		}
	
		@Test
		@DisplayName("With string field")
		public void withVarAttributesCompile() throws Exception {
			String source = multilineString(
					"event E1<T1, T2 extends Number, T3 extends Double> { }",
					"event E2<T2 extends Number> extends E1<String, T2, Double> {",
					"  var name : String",
					"}");
			String expected = multilineString(
					"import io.sarl.lang.core.Address;",
					"import io.sarl.lang.core.annotation.SarlElementType;",
					"import io.sarl.lang.core.annotation.SarlSpecification;",
					"import io.sarl.lang.core.annotation.SyntheticMember;",
					"import java.util.Objects;",
					"import org.eclipse.xtext.xbase.lib.Pure;",
					"import org.eclipse.xtext.xbase.lib.XbaseGenerated;",
					"import org.eclipse.xtext.xbase.lib.util.ToStringBuilder;",
					"",
					"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
					"@SarlElementType(" + SarlPackage.SARL_EVENT + ")",
					"@XbaseGenerated",
					"@SuppressWarnings(\"all\")",
					"public class E2<T2 extends Number> extends E1<String, T2, Double> {",
					"  public String name;",
					"  ",
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
					"    E2 other = (E2) obj;",
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
					"   * Returns a String representation of the E2 event's attributes only.",
					"   */",
					"  @SyntheticMember",
					"  @Pure",
					"  protected void toString(final ToStringBuilder builder) {",
					"    super.toString(builder);",
					"    builder.add(\"name\", this.name);",
					"  }",
					"  ",
					"  @SyntheticMember",
					"  private static final long serialVersionUID = 711013743L;",
					"}",
					""
					);
			getCompileHelper().assertCompilesTo("E2", source, expected);
		}
	
		@Test
		@DisplayName("With constructor")
		public void noStaticField() throws Exception {
			String source = multilineString(
					"event E1<T1, T2 extends Number, T3 extends Double> { }",
					"event E2<T2 extends Number> extends E1<String, T2, Double> {",
					"	val titi : int = 4",
					"	val toto : int",
					"	new(a : int) {",
					"		this.toto = a",
					"	}",
					"}");
			String expected = multilineString(
					"import io.sarl.lang.core.annotation.SarlElementType;",
					"import io.sarl.lang.core.annotation.SarlSpecification;",
					"import io.sarl.lang.core.annotation.SyntheticMember;",
					"import org.eclipse.xtext.xbase.lib.Pure;",
					"import org.eclipse.xtext.xbase.lib.XbaseGenerated;",
					"import org.eclipse.xtext.xbase.lib.util.ToStringBuilder;",
					"",
					"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
					"@SarlElementType(" + SarlPackage.SARL_EVENT + ")",
					"@XbaseGenerated",
					"@SuppressWarnings(\"all\")",
					"public class E2<T2 extends Number> extends E1<String, T2, Double> {",
					"  public final int titi = 4;",
					"  ",
					"  public final int toto;",
					"  ",
					"  public E2(final int a) {",
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
					"    E2 other = (E2) obj;",
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
					"   * Returns a String representation of the E2 event's attributes only.",
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
					"  private static final long serialVersionUID = -477043579L;",
					"}",
					""
					);
			getCompileHelper().assertCompilesTo("E2", source, expected);
		}

		@Test
		@DisplayName("Without modifier")
		public void eventmodifier_none() throws Exception {
			getCompileHelper().assertCompilesTo("E2", 
				multilineString(
						"event E1<T1, T2 extends Number, T3 extends Double> { }",
						"event E2<T2 extends Number> extends E1<String, T2, Double>"),
				multilineString(
					"import io.sarl.lang.core.Address;",
					"import io.sarl.lang.core.annotation.SarlElementType;",
					"import io.sarl.lang.core.annotation.SarlSpecification;",
					"import io.sarl.lang.core.annotation.SyntheticMember;",
					"import org.eclipse.xtext.xbase.lib.XbaseGenerated;",
					"",
					"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
					"@SarlElementType(" + SarlPackage.SARL_EVENT + ")",
					"@XbaseGenerated",
					"@SuppressWarnings(\"all\")",
					"public class E2<T2 extends Number> extends E1<String, T2, Double> {",
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
					"  private static final long serialVersionUID = -487619457L;",
					"}",
					""));
		}
	
		@Test
		@DisplayName("With public modifier")
		public void eventmodifier_public() throws Exception {
			getCompileHelper().assertCompilesTo("E2", 
				multilineString(
					"event E1<T1, T2 extends Number, T3 extends Double> { }",
					"public event E2<T2 extends Number> extends E1<String, T2, Double>"
				),
				multilineString(
					"import io.sarl.lang.core.Address;",
					"import io.sarl.lang.core.annotation.SarlElementType;",
					"import io.sarl.lang.core.annotation.SarlSpecification;",
					"import io.sarl.lang.core.annotation.SyntheticMember;",
					"import org.eclipse.xtext.xbase.lib.XbaseGenerated;",
					"",
					"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
					"@SarlElementType(" + SarlPackage.SARL_EVENT + ")",
					"@XbaseGenerated",
					"@SuppressWarnings(\"all\")",
					"public class E2<T2 extends Number> extends E1<String, T2, Double> {",
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
					"  private static final long serialVersionUID = -487619457L;",
					"}",
					""));
		}

		@Test
		@DisplayName("With abstract modifier")
		public void eventmodifier_abstract() throws Exception {
			getCompileHelper().assertCompilesTo("E2", 
				multilineString(
					"event E1<T1, T2 extends Number, T3 extends Double> { }",
					"abstract event E2<T2 extends Number> extends E1<String, T2, Double>"
				),
				multilineString(
					"import io.sarl.lang.core.Address;",
					"import io.sarl.lang.core.annotation.SarlElementType;",
					"import io.sarl.lang.core.annotation.SarlSpecification;",
					"import io.sarl.lang.core.annotation.SyntheticMember;",
					"import org.eclipse.xtext.xbase.lib.XbaseGenerated;",
					"",
					"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
					"@SarlElementType(" + SarlPackage.SARL_EVENT + ")",
					"@XbaseGenerated",
					"@SuppressWarnings(\"all\")",
					"public abstract class E2<T2 extends Number> extends E1<String, T2, Double> {",
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
					"  private static final long serialVersionUID = -487619457L;",
					"}",
					""));
		}
	
		@Test
		@DisplayName("With package modifier")
		public void eventmodifier_package() throws Exception {
			getCompileHelper().assertCompilesTo("E2", 
				multilineString(
					"event E1<T1, T2 extends Number, T3 extends Double> { }",
					"package event E2<T2 extends Number> extends E1<String, T2, Double>"
				),
				multilineString(
					"import io.sarl.lang.core.Address;",
					"import io.sarl.lang.core.annotation.SarlElementType;",
					"import io.sarl.lang.core.annotation.SarlSpecification;",
					"import io.sarl.lang.core.annotation.SyntheticMember;",
					"import org.eclipse.xtext.xbase.lib.XbaseGenerated;",
					"",
					"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
					"@SarlElementType(" + SarlPackage.SARL_EVENT + ")",
					"@XbaseGenerated",
					"@SuppressWarnings(\"all\")",
					"class E2<T2 extends Number> extends E1<String, T2, Double> {",
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
					"  private static final long serialVersionUID = -487619457L;",
					"}",
					""));
		}
	
		@Test
		@DisplayName("With final modifier")
		public void eventmodifier_final() throws Exception {
			getCompileHelper().assertCompilesTo("E2", 
				multilineString(
					"event E1<T1, T2 extends Number, T3 extends Double> { }",
					"final event E2<T2 extends Number> extends E1<String, T2, Double>"
				),
				multilineString(
					"import io.sarl.lang.core.Address;",
					"import io.sarl.lang.core.annotation.SarlElementType;",
					"import io.sarl.lang.core.annotation.SarlSpecification;",
					"import io.sarl.lang.core.annotation.SyntheticMember;",
					"import org.eclipse.xtext.xbase.lib.XbaseGenerated;",
					"",
					"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
					"@SarlElementType(" + SarlPackage.SARL_EVENT + ")",
					"@XbaseGenerated",
					"@SuppressWarnings(\"all\")",
					"public final class E2<T2 extends Number> extends E1<String, T2, Double> {",
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
					"  private static final long serialVersionUID = -487619457L;",
					"}",
					""));
		}
	
		@Test
		@DisplayName("Without field modifier")
		public void fieldmodifier_none() throws Exception {
			getCompileHelper().assertCompilesTo("E2", 
				multilineString(
					"event E1<T1, T2 extends Number, T3 extends Double> { }",
					"event E2<T2 extends Number> extends E1<String, T2, Double> {",
					"	var field : int",
					"}"
				),
				multilineString(
					"import io.sarl.lang.core.Address;",
					"import io.sarl.lang.core.annotation.SarlElementType;",
					"import io.sarl.lang.core.annotation.SarlSpecification;",
					"import io.sarl.lang.core.annotation.SyntheticMember;",
					"import org.eclipse.xtext.xbase.lib.Pure;",
					"import org.eclipse.xtext.xbase.lib.XbaseGenerated;",
					"import org.eclipse.xtext.xbase.lib.util.ToStringBuilder;",
					"",
					"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
					"@SarlElementType(" + SarlPackage.SARL_EVENT + ")",
					"@XbaseGenerated",
					"@SuppressWarnings(\"all\")",
					"public class E2<T2 extends Number> extends E1<String, T2, Double> {",
					"  public int field;",
					"  ",
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
					"    E2 other = (E2) obj;",
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
					"   * Returns a String representation of the E2 event's attributes only.",
					"   */",
					"  @SyntheticMember",
					"  @Pure",
					"  protected void toString(final ToStringBuilder builder) {",
					"    super.toString(builder);",
					"    builder.add(\"field\", this.field);",
					"  }",
					"  ",
					"  @SyntheticMember",
					"  private static final long serialVersionUID = -390087320L;",
					"}",
					""));
		}

		@Test
		@DisplayName("With public field modifier")
		public void fieldmodifier_public() throws Exception {
			getCompileHelper().assertCompilesTo("E2", 
				multilineString(
					"event E1<T1, T2 extends Number, T3 extends Double> { }",
					"event E2<T2 extends Number> extends E1<String, T2, Double> {",
					"	public var field : int",
					"}"
				),
				multilineString(
					"import io.sarl.lang.core.Address;",
					"import io.sarl.lang.core.annotation.SarlElementType;",
					"import io.sarl.lang.core.annotation.SarlSpecification;",
					"import io.sarl.lang.core.annotation.SyntheticMember;",
					"import org.eclipse.xtext.xbase.lib.Pure;",
					"import org.eclipse.xtext.xbase.lib.XbaseGenerated;",
					"import org.eclipse.xtext.xbase.lib.util.ToStringBuilder;",
					"",
					"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
					"@SarlElementType(" + SarlPackage.SARL_EVENT + ")",
					"@XbaseGenerated",
					"@SuppressWarnings(\"all\")",
					"public class E2<T2 extends Number> extends E1<String, T2, Double> {",
					"  public int field;",
					"  ",
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
					"    E2 other = (E2) obj;",
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
					"   * Returns a String representation of the E2 event's attributes only.",
					"   */",
					"  @SyntheticMember",
					"  @Pure",
					"  protected void toString(final ToStringBuilder builder) {",
					"    super.toString(builder);",
					"    builder.add(\"field\", this.field);",
					"  }",
					"  ",
					"  @SyntheticMember",
					"  private static final long serialVersionUID = -390087320L;",
					"}",
					""));
		}

		@Test
		@DisplayName("Without constructor modifier")
		public void constructormodifier_none() throws Exception {
			getCompileHelper().assertCompilesTo("E2", 
				multilineString(
					"event E1<T1, T2 extends Number, T3 extends Double> { }",
					"event E2<T2 extends Number> extends E1<String, T2, Double> {",
					"	new { super(null) }",
					"}"
				),
				multilineString(
					"import io.sarl.lang.core.annotation.SarlElementType;",
					"import io.sarl.lang.core.annotation.SarlSpecification;",
					"import io.sarl.lang.core.annotation.SyntheticMember;",
					"import org.eclipse.xtext.xbase.lib.XbaseGenerated;",
					"",
					"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
					"@SarlElementType(" + SarlPackage.SARL_EVENT + ")",
					"@XbaseGenerated",
					"@SuppressWarnings(\"all\")",
					"public class E2<T2 extends Number> extends E1<String, T2, Double> {",
					"  public E2() {",
					"    super(null);",
					"  }",
					"  ",
					"  @SyntheticMember",
					"  private static final long serialVersionUID = -487617228L;",
					"}",
					""));
		}
	
		@Test
		@DisplayName("With public constructor modifier")
		public void constructormodifier_public() throws Exception {
			getCompileHelper().assertCompilesTo("E2", 
				multilineString(
					"event E1<T1, T2 extends Number, T3 extends Double> { }",
					"event E2<T2 extends Number> extends E1<String, T2, Double> {",
					"	public new { super(null) }",
					"}"
				),
				multilineString(
					"import io.sarl.lang.core.annotation.SarlElementType;",
					"import io.sarl.lang.core.annotation.SarlSpecification;",
					"import io.sarl.lang.core.annotation.SyntheticMember;",
					"import org.eclipse.xtext.xbase.lib.XbaseGenerated;",
					"",
					"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
					"@SarlElementType(" + SarlPackage.SARL_EVENT + ")",
					"@XbaseGenerated",
					"@SuppressWarnings(\"all\")",
					"public class E2<T2 extends Number> extends E1<String, T2, Double> {",
					"  public E2() {",
					"    super(null);",
					"  }",
					"  ",
					"  @SyntheticMember",
					"  private static final long serialVersionUID = -487617228L;",
					"}",
					""));
		}

		@Test
		@DisplayName("With private constructor modifier")
		public void constructormodifier_private() throws Exception {
			getCompileHelper().assertCompilesTo("E2", 
				multilineString(
					"event E1<T1, T2 extends Number, T3 extends Double> { }",
					"event E2<T2 extends Number> extends E1<String, T2, Double> {",
					"	private new { super(null) }",
					"}"
				),
				multilineString(
					"import io.sarl.lang.core.annotation.SarlElementType;",
					"import io.sarl.lang.core.annotation.SarlSpecification;",
					"import io.sarl.lang.core.annotation.SyntheticMember;",
					"import org.eclipse.xtext.xbase.lib.XbaseGenerated;",
					"",
					"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
					"@SarlElementType(" + SarlPackage.SARL_EVENT + ")",
					"@XbaseGenerated",
					"@SuppressWarnings(\"all\")",
					"public class E2<T2 extends Number> extends E1<String, T2, Double> {",
					"  private E2() {",
					"    super(null);",
					"  }",
					"  ",
					"  @SyntheticMember",
					"  private static final long serialVersionUID = -487617228L;",
					"}",
					""));
		}
	
		@Test
		@DisplayName("With package constructor modifier")
		public void constructormodifier_package() throws Exception {
			getCompileHelper().assertCompilesTo("E2", 
				multilineString(
					"event E1<T1, T2 extends Number, T3 extends Double> { }",
					"event E2<T2 extends Number> extends E1<String, T2, Double> {",
					"	package new { super(null) }",
					"}"
				),
				multilineString(
					"import io.sarl.lang.core.annotation.SarlElementType;",
					"import io.sarl.lang.core.annotation.SarlSpecification;",
					"import io.sarl.lang.core.annotation.SyntheticMember;",
					"import org.eclipse.xtext.xbase.lib.XbaseGenerated;",
					"",
					"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
					"@SarlElementType(" + SarlPackage.SARL_EVENT + ")",
					"@XbaseGenerated",
					"@SuppressWarnings(\"all\")",
					"public class E2<T2 extends Number> extends E1<String, T2, Double> {",
					"  E2() {",
					"    super(null);",
					"  }",
					"  ",
					"  @SyntheticMember",
					"  private static final long serialVersionUID = -487617228L;",
					"}",
					""));
		}

		@Test
		@DisplayName("With protected constructor modifier")
		public void constructormodifier_protected() throws Exception {
			getCompileHelper().assertCompilesTo("E2", 
				multilineString(
					"event E1<T1, T2 extends Number, T3 extends Double> { }",
					"event E2<T2 extends Number> extends E1<String, T2, Double> {",
					"	protected new { super(null) }",
					"}"
				),
				multilineString(
					"import io.sarl.lang.core.annotation.SarlElementType;",
					"import io.sarl.lang.core.annotation.SarlSpecification;",
					"import io.sarl.lang.core.annotation.SyntheticMember;",
					"import org.eclipse.xtext.xbase.lib.XbaseGenerated;",
					"",
					"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
					"@SarlElementType(" + SarlPackage.SARL_EVENT + ")",
					"@XbaseGenerated",
					"@SuppressWarnings(\"all\")",
					"public class E2<T2 extends Number> extends E1<String, T2, Double> {",
					"  protected E2() {",
					"    super(null);",
					"  }",
					"  ",
					"  @SyntheticMember",
					"  private static final long serialVersionUID = -487617228L;",
					"}",
					""));
		}

	}

}
