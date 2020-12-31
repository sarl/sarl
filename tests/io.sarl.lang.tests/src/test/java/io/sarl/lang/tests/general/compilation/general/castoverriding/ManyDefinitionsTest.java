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
package io.sarl.lang.tests.general.compilation.general.castoverriding;

import static io.sarl.tests.api.tools.TestUtils.multilineString;

import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Tag;

import io.sarl.lang.SARLVersion;
import io.sarl.lang.sarl.SarlPackage;
import io.sarl.tests.api.AbstractSarlTest;
import io.sarl.tests.api.globalcompilation.GlobalCompilationSuite;
import io.sarl.tests.api.globalcompilation.GlobalCompilationTestContribution;
import io.sarl.tests.api.globalcompilation.ResourceSetGlobalCompilationContext;


/**
 * @author $Author: sgalland$
 * @version $Name$ $Revision$ $Date$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 * @since 0.9
 */
@SuppressWarnings("all")
@DisplayName("Compilation: multiple cast functions")
@Tag("core")
@Tag("compileToJava")
public class ManyDefinitionsTest {

	/** This class enables to test the linking decision to multiple getter functions.
	 *
	 * @author $Author: sgalland$
	 * @version $FullVersion$
	 * @mavengroupid $GroupId$
	 * @mavenartifactid $ArtifactId$
	 * @since 0.9
	 */
	@GlobalCompilationSuite
	@DisplayName("Multiple functions")
	public static class StandardFunctionCalls extends AbstractSarlTest {

		private final String STATIC_EXTFIELD_EXTPARAM_INSTANCE_LOCALFUNCTION_A_SARL = multilineString(
				"package io.sarl.lang.core.tests.compile00",
				"import java.util.UUID",
				"class A0 {",
				"  def getValue : UUID { null }",
				"}",
				"class A1 {",
				"  def getValue(v : A0) : UUID { null }",
				"}",
				"class StaticA1 {",
				"  static def getValue(v : A0) : UUID { null }",
				"}"
				);

		private final String STATIC_EXTFIELD_EXTPARAM_INSTANCE_LOCALFUNCTION_B_SARL = multilineString(
				"package io.sarl.lang.core.tests.compile00",
				"import static extension o.sarl.lang.core.tests.compile00.StaticA1.*",
				"import java.util.UUID",
				"class A2 {",
				"  extension var foo : A1",
				"  def fct(x : A0, extension y : A1) : UUID {",
				"    x.value",
				"  }",
				"  def getValue(x : A0) : UUID { null }",
				"}"
				);

		private final String STATIC_EXTFIELD_EXTPARAM_INSTANCE_LOCALFUNCTION_JAVA = multilineString(
				"package io.sarl.lang.core.tests.compile00;",
				"",
				"import io.sarl.lang.annotation.SarlElementType;",
				"import io.sarl.lang.annotation.SarlSpecification;",
				"import io.sarl.lang.annotation.SyntheticMember;",
				"import io.sarl.lang.core.tests.compile00.A0;",
				"import io.sarl.lang.core.tests.compile00.A1;",
				"import java.util.UUID;",
				"import org.eclipse.xtext.xbase.lib.Extension;",
				"import org.eclipse.xtext.xbase.lib.Pure;",
				"",
				"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
				"@SarlElementType(" + SarlPackage.SARL_CLASS + ")",
				"@SuppressWarnings(\"all\")",
				"public class A2 {",
				"  @Extension",
				"  private A1 foo;",
				"  ",
				"  @Pure",
				"  public UUID fct(final A0 x, @Extension final A1 y) {",
				"    return x.getValue();",
				"  }",
				"  ",
				"  @Pure",
				"  public UUID getValue(final A0 x) {",
				"    return null;",
				"  }",
				"  ",
				"  @Override",
				"  @Pure",
				"  @SyntheticMember",
				"  public boolean equals(final Object obj) {",
				"    return super.equals(obj);",
				"  }",
				"  ",
				"  @Override",
				"  @Pure",
				"  @SyntheticMember",
				"  public int hashCode() {",
				"    int result = super.hashCode();",
				"    return result;",
				"  }",
				"  ",
				"  @SyntheticMember",
				"  public A2() {",
				"    super();",
				"  }",
				"}",
				"");

		@GlobalCompilationTestContribution
		public void compile00(ResourceSetGlobalCompilationContext ctx) throws Exception {
			ctx.compileTo(
					new String[] { STATIC_EXTFIELD_EXTPARAM_INSTANCE_LOCALFUNCTION_A_SARL, STATIC_EXTFIELD_EXTPARAM_INSTANCE_LOCALFUNCTION_B_SARL },
					"io.sarl.lang.core.tests.compile00.A2",
					STATIC_EXTFIELD_EXTPARAM_INSTANCE_LOCALFUNCTION_JAVA);
		}

		private final String STATIC_EXTFIELD_EXTPARAM_LOCALFUNCTION_A_SARL = multilineString(
				"package io.sarl.lang.core.tests.compile01",
				"import java.util.UUID",
				"class A0 {",
				"}",
				"class A1 {",
				"  def getValue(v : A0) : UUID { null }",
				"}",
				"class StaticA1 {",
				"  static def getValue(v : A0) : UUID { null }",
				"}"
				);

		private final String STATIC_EXTFIELD_EXTPARAM_LOCALFUNCTION_B_SARL = multilineString(
				"package io.sarl.lang.core.tests.compile01",
				"import static extension o.sarl.lang.core.tests.compile01.StaticA1.*",
				"import java.util.UUID",
				"class A2 {",
				"  extension var foo : A1",
				"  def fct(x : A0, extension y : A1) : UUID {",
				"    x.value",
				"  }",
				"  def getValue(x : A0) : UUID { null }",
				"}"
				);

		private final String STATIC_EXTFIELD_EXTPARAM_LOCALFUNCTION_JAVA = multilineString(
				"package io.sarl.lang.core.tests.compile01;",
				"",
				"import io.sarl.lang.annotation.SarlElementType;",
				"import io.sarl.lang.annotation.SarlSpecification;",
				"import io.sarl.lang.annotation.SyntheticMember;",
				"import io.sarl.lang.core.tests.compile01.A0;",
				"import io.sarl.lang.core.tests.compile01.A1;",
				"import java.util.UUID;",
				"import org.eclipse.xtext.xbase.lib.Extension;",
				"import org.eclipse.xtext.xbase.lib.Pure;",
				"",
				"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
				"@SarlElementType(" + SarlPackage.SARL_CLASS + ")",
				"@SuppressWarnings(\"all\")",
				"public class A2 {",
				"  @Extension",
				"  private A1 foo;",
				"  ",
				"  @Pure",
				"  public UUID fct(final A0 x, @Extension final A1 y) {",
				"    return y.getValue(x);",
				"  }",
				"  ",
				"  @Pure",
				"  public UUID getValue(final A0 x) {",
				"    return null;",
				"  }",
				"  ",
				"  @Override",
				"  @Pure",
				"  @SyntheticMember",
				"  public boolean equals(final Object obj) {",
				"    return super.equals(obj);",
				"  }",
				"  ",
				"  @Override",
				"  @Pure",
				"  @SyntheticMember",
				"  public int hashCode() {",
				"    int result = super.hashCode();",
				"    return result;",
				"  }",
				"  ",
				"  @SyntheticMember",
				"  public A2() {",
				"    super();",
				"  }",
				"}",
				"");

		@GlobalCompilationTestContribution
		public void compile01(ResourceSetGlobalCompilationContext ctx) throws Exception {
			ctx.compileTo(
					new String[] { STATIC_EXTFIELD_EXTPARAM_LOCALFUNCTION_A_SARL, STATIC_EXTFIELD_EXTPARAM_LOCALFUNCTION_B_SARL },
					"io.sarl.lang.core.tests.compile01.A2",
					STATIC_EXTFIELD_EXTPARAM_LOCALFUNCTION_JAVA);
		}

		private final String STATIC_EXTFIELD_LOCALFUNCTION_A_SARL = multilineString(
				"package io.sarl.lang.core.tests.compile02",
				"import java.util.UUID",
				"class A0 {",
				"}",
				"class A1 {",
				"  def getValue(v : A0) : UUID { null }",
				"}",
				"class StaticA1 {",
				"  static def getValue(v : A0) : UUID { null }",
				"}"
				);

		private final String STATIC_EXTFIELD_LOCALFUNCTION_B_SARL = multilineString(
				"package io.sarl.lang.core.tests.compile02",
				"import static extension o.sarl.lang.core.tests.compile02.StaticA1.*",
				"import java.util.UUID",
				"class A2 {",
				"  extension var foo : A1",
				"  def fct(x : A0) : UUID {",
				"    x.value",
				"  }",
				"  def getValue(x : A0) : UUID { null }",
				"}"
				);

		private final String STATIC_EXTFIELD_LOCALFUNCTION_JAVA = multilineString(
				"package io.sarl.lang.core.tests.compile02;",
				"",
				"import io.sarl.lang.annotation.SarlElementType;",
				"import io.sarl.lang.annotation.SarlSpecification;",
				"import io.sarl.lang.annotation.SyntheticMember;",
				"import io.sarl.lang.core.tests.compile02.A0;",
				"import io.sarl.lang.core.tests.compile02.A1;",
				"import java.util.UUID;",
				"import org.eclipse.xtext.xbase.lib.Extension;",
				"import org.eclipse.xtext.xbase.lib.Pure;",
				"",
				"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
				"@SarlElementType(" + SarlPackage.SARL_CLASS + ")",
				"@SuppressWarnings(\"all\")",
				"public class A2 {",
				"  @Extension",
				"  private A1 foo;",
				"  ",
				"  @Pure",
				"  public UUID fct(final A0 x) {",
				"    return this.getValue(x);",
				"  }",
				"  ",
				"  @Pure",
				"  public UUID getValue(final A0 x) {",
				"    return null;",
				"  }",
				"  ",
				"  @Override",
				"  @Pure",
				"  @SyntheticMember",
				"  public boolean equals(final Object obj) {",
				"    return super.equals(obj);",
				"  }",
				"  ",
				"  @Override",
				"  @Pure",
				"  @SyntheticMember",
				"  public int hashCode() {",
				"    int result = super.hashCode();",
				"    return result;",
				"  }",
				"  ",
				"  @SyntheticMember",
				"  public A2() {",
				"    super();",
				"  }",
				"}",
				"");

		@GlobalCompilationTestContribution
		public void compile02(ResourceSetGlobalCompilationContext ctx) throws Exception {
			ctx.compileTo(
					new String[] { STATIC_EXTFIELD_LOCALFUNCTION_A_SARL, STATIC_EXTFIELD_LOCALFUNCTION_B_SARL },
					"io.sarl.lang.core.tests.compile02.A2",
					STATIC_EXTFIELD_LOCALFUNCTION_JAVA);
		}

		private final String STATIC_EXTFIELD_A_SARL = multilineString(
				"package io.sarl.lang.core.tests.compile03",
				"import java.util.UUID",
				"class A0 {",
				"}",
				"class A1 {",
				"  def getValue(v : A0) : UUID { null }",
				"}",
				"class StaticA1 {",
				"  static def getValue(v : A0) : UUID { null }",
				"}"
				);

		private final String STATIC_EXTFIELD_B_SARL = multilineString(
				"package io.sarl.lang.core.tests.compile03",
				"import static extension o.sarl.lang.core.tests.compile03.StaticA1.*",
				"import java.util.UUID",
				"class A2 {",
				"  extension var foo : A1",
				"  def fct(x : A0) : UUID {",
				"    x.value",
				"  }",
				"}"
				);

		private final String STATIC_EXTFIELD_JAVA = multilineString(
				"package io.sarl.lang.core.tests.compile03;",
				"",
				"import io.sarl.lang.annotation.SarlElementType;",
				"import io.sarl.lang.annotation.SarlSpecification;",
				"import io.sarl.lang.annotation.SyntheticMember;",
				"import io.sarl.lang.core.tests.compile03.A0;",
				"import io.sarl.lang.core.tests.compile03.A1;",
				"import java.util.UUID;",
				"import org.eclipse.xtext.xbase.lib.Extension;",
				"import org.eclipse.xtext.xbase.lib.Pure;",
				"",
				"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
				"@SarlElementType(" + SarlPackage.SARL_CLASS + ")",
				"@SuppressWarnings(\"all\")",
				"public class A2 {",
				"  @Extension",
				"  private A1 foo;",
				"  ",
				"  @Pure",
				"  public UUID fct(final A0 x) {",
				"    return this.foo.getValue(x);",
				"  }",
				"  ",
				"  @Override",
				"  @Pure",
				"  @SyntheticMember",
				"  public boolean equals(final Object obj) {",
				"    return super.equals(obj);",
				"  }",
				"  ",
				"  @Override",
				"  @Pure",
				"  @SyntheticMember",
				"  public int hashCode() {",
				"    int result = super.hashCode();",
				"    return result;",
				"  }",
				"  ",
				"  @SyntheticMember",
				"  public A2() {",
				"    super();",
				"  }",
				"}",
				"");

		@GlobalCompilationTestContribution
		public void compile03(ResourceSetGlobalCompilationContext ctx) throws Exception {
			ctx.compileTo(
					new String[] { STATIC_EXTFIELD_A_SARL, STATIC_EXTFIELD_B_SARL },
					"io.sarl.lang.core.tests.compile03.A2",
					STATIC_EXTFIELD_JAVA);
		}

	}

	/** This class enables to test the linking decision to multiple getter functions.
	 *
	 * <p>The cases are the same as inside {@link StandardFunctionCalls} but applied to cast operators.
	 *
	 * @author $Author: sgalland$
	 * @version $FullVersion$
	 * @mavengroupid $GroupId$
	 * @mavenartifactid $ArtifactId$
	 * @since 0.9
	 */
	@GlobalCompilationSuite
	@DisplayName("Compilation: Casting functions")
	public static class CastFunctionCalls extends AbstractSarlTest {

		private final String STATIC_EXTFIELD_EXTPARAM_INSTANCE_LOCALFUNCTION_A_SARL = multilineString(
				"package io.sarl.lang.core.tests.compile00",
				"import java.util.UUID",
				"class A0 {",
				"  def toUUID : UUID { null }",
				"}",
				"class A1 {",
				"  def toUUID(v : A0) : UUID { null }",
				"}",
				"class StaticA1 {",
				"  static def toUUID(v : A0) : UUID { null }",
				"}"
				);

		private final String STATIC_EXTFIELD_EXTPARAM_INSTANCE_LOCALFUNCTION_B_SARL = multilineString(
				"package io.sarl.lang.core.tests.compile00",
				"import static extension o.sarl.lang.core.tests.compile00.StaticA1.*",
				"import java.util.UUID",
				"class A2 {",
				"  extension var foo : A1",
				"  def fct(x : A0, extension y : A1) : UUID {",
				"    x as UUID",
				"  }",
				"  def toUUID(x : A0) : UUID { null }",
				"}"
				);

		private final String STATIC_EXTFIELD_EXTPARAM_INSTANCE_LOCALFUNCTION_JAVA = multilineString(
				"package io.sarl.lang.core.tests.compile00;",
				"",
				"import io.sarl.lang.annotation.SarlElementType;",
				"import io.sarl.lang.annotation.SarlSpecification;",
				"import io.sarl.lang.annotation.SyntheticMember;",
				"import io.sarl.lang.core.tests.compile00.A0;",
				"import io.sarl.lang.core.tests.compile00.A1;",
				"import java.util.UUID;",
				"import org.eclipse.xtext.xbase.lib.Extension;",
				"import org.eclipse.xtext.xbase.lib.Pure;",
				"",
				"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
				"@SarlElementType(" + SarlPackage.SARL_CLASS + ")",
				"@SuppressWarnings(\"all\")",
				"public class A2 {",
				"  @Extension",
				"  private A1 foo;",
				"  ",
				"  @Pure",
				"  public UUID fct(final A0 x, @Extension final A1 y) {",
				"    return (x == null ? null : x.toUUID());",
				"  }",
				"  ",
				"  @Pure",
				"  public UUID toUUID(final A0 x) {",
				"    return null;",
				"  }",
				"  ",
				"  @Override",
				"  @Pure",
				"  @SyntheticMember",
				"  public boolean equals(final Object obj) {",
				"    return super.equals(obj);",
				"  }",
				"  ",
				"  @Override",
				"  @Pure",
				"  @SyntheticMember",
				"  public int hashCode() {",
				"    int result = super.hashCode();",
				"    return result;",
				"  }",
				"  ",
				"  @SyntheticMember",
				"  public A2() {",
				"    super();",
				"  }",
				"}",
				"");

		@GlobalCompilationTestContribution
		public void compile00(ResourceSetGlobalCompilationContext ctx) throws Exception {
			ctx.compileTo(
					new String[] { STATIC_EXTFIELD_EXTPARAM_INSTANCE_LOCALFUNCTION_A_SARL, STATIC_EXTFIELD_EXTPARAM_INSTANCE_LOCALFUNCTION_B_SARL },
					"io.sarl.lang.core.tests.compile00.A2",
					STATIC_EXTFIELD_EXTPARAM_INSTANCE_LOCALFUNCTION_JAVA);
		}

		private final String STATIC_EXTFIELD_EXTPARAM_LOCALFUNCTION_A_SARL = multilineString(
				"package io.sarl.lang.core.tests.compile01",
				"import java.util.UUID",
				"class A0 {",
				"}",
				"class A1 {",
				"  def toUUID(v : A0) : UUID { null }",
				"}",
				"class StaticA1 {",
				"  static def toUUID(v : A0) : UUID { null }",
				"}"
				);

		private final String STATIC_EXTFIELD_EXTPARAM_LOCALFUNCTION_B_SARL = multilineString(
				"package io.sarl.lang.core.tests.compile01",
				"import static extension o.sarl.lang.core.tests.compile01.StaticA1.*",
				"import java.util.UUID",
				"class A2 {",
				"  extension var foo : A1",
				"  def fct(x : A0, extension y : A1) : UUID {",
				"    x as UUID",
				"  }",
				"  def toUUID(x : A0) : UUID { null }",
				"}"
				);

		private final String STATIC_EXTFIELD_EXTPARAM_LOCALFUNCTION_JAVA = multilineString(
				"package io.sarl.lang.core.tests.compile01;",
				"",
				"import io.sarl.lang.annotation.SarlElementType;",
				"import io.sarl.lang.annotation.SarlSpecification;",
				"import io.sarl.lang.annotation.SyntheticMember;",
				"import io.sarl.lang.core.tests.compile01.A0;",
				"import io.sarl.lang.core.tests.compile01.A1;",
				"import java.util.UUID;",
				"import org.eclipse.xtext.xbase.lib.Extension;",
				"import org.eclipse.xtext.xbase.lib.Pure;",
				"",
				"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
				"@SarlElementType(" + SarlPackage.SARL_CLASS + ")",
				"@SuppressWarnings(\"all\")",
				"public class A2 {",
				"  @Extension",
				"  private A1 foo;",
				"  ",
				"  @Pure",
				"  public UUID fct(final A0 x, @Extension final A1 y) {",
				"    return (x == null ? null : y.toUUID(x));",
				"  }",
				"  ",
				"  @Pure",
				"  public UUID toUUID(final A0 x) {",
				"    return null;",
				"  }",
				"  ",
				"  @Override",
				"  @Pure",
				"  @SyntheticMember",
				"  public boolean equals(final Object obj) {",
				"    return super.equals(obj);",
				"  }",
				"  ",
				"  @Override",
				"  @Pure",
				"  @SyntheticMember",
				"  public int hashCode() {",
				"    int result = super.hashCode();",
				"    return result;",
				"  }",
				"  ",
				"  @SyntheticMember",
				"  public A2() {",
				"    super();",
				"  }",
				"}",
				"");

		@GlobalCompilationTestContribution
		public void compile01(ResourceSetGlobalCompilationContext ctx) throws Exception {
			ctx.compileTo(
					new String[] { STATIC_EXTFIELD_EXTPARAM_LOCALFUNCTION_A_SARL, STATIC_EXTFIELD_EXTPARAM_LOCALFUNCTION_B_SARL },
					"io.sarl.lang.core.tests.compile01.A2",
					STATIC_EXTFIELD_EXTPARAM_LOCALFUNCTION_JAVA);
		}

		private final String STATIC_EXTFIELD_LOCALFUNCTION_A_SARL = multilineString(
				"package io.sarl.lang.core.tests.compile02",
				"import java.util.UUID",
				"class A0 {",
				"}",
				"class A1 {",
				"  def toUUID(v : A0) : UUID { null }",
				"}",
				"class StaticA1 {",
				"  static def toUUID(v : A0) : UUID { null }",
				"}"
				);

		private final String STATIC_EXTFIELD_LOCALFUNCTION_B_SARL = multilineString(
				"package io.sarl.lang.core.tests.compile02",
				"import static extension o.sarl.lang.core.tests.compile02.StaticA1.*",
				"import java.util.UUID",
				"class A2 {",
				"  extension var foo : A1",
				"  def fct(x : A0) : UUID {",
				"    x as UUID",
				"  }",
				"  def toUUID(x : A0) : UUID { null }",
				"}"
				);

		private final String STATIC_EXTFIELD_LOCALFUNCTION_JAVA = multilineString(
				"package io.sarl.lang.core.tests.compile02;",
				"",
				"import io.sarl.lang.annotation.SarlElementType;",
				"import io.sarl.lang.annotation.SarlSpecification;",
				"import io.sarl.lang.annotation.SyntheticMember;",
				"import io.sarl.lang.core.tests.compile02.A0;",
				"import io.sarl.lang.core.tests.compile02.A1;",
				"import java.util.UUID;",
				"import org.eclipse.xtext.xbase.lib.Extension;",
				"import org.eclipse.xtext.xbase.lib.Pure;",
				"",
				"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
				"@SarlElementType(" + SarlPackage.SARL_CLASS + ")",
				"@SuppressWarnings(\"all\")",
				"public class A2 {",
				"  @Extension",
				"  private A1 foo;",
				"  ",
				"  @Pure",
				"  public UUID fct(final A0 x) {",
				"    return (x == null ? null : this.toUUID(x));",
				"  }",
				"  ",
				"  @Pure",
				"  public UUID toUUID(final A0 x) {",
				"    return null;",
				"  }",
				"  ",
				"  @Override",
				"  @Pure",
				"  @SyntheticMember",
				"  public boolean equals(final Object obj) {",
				"    return super.equals(obj);",
				"  }",
				"  ",
				"  @Override",
				"  @Pure",
				"  @SyntheticMember",
				"  public int hashCode() {",
				"    int result = super.hashCode();",
				"    return result;",
				"  }",
				"  ",
				"  @SyntheticMember",
				"  public A2() {",
				"    super();",
				"  }",
				"}",
				"");

		@GlobalCompilationTestContribution
		public void compile02(ResourceSetGlobalCompilationContext ctx) throws Exception {
			ctx.compileTo(
					new String[] { STATIC_EXTFIELD_LOCALFUNCTION_A_SARL, STATIC_EXTFIELD_LOCALFUNCTION_B_SARL },
					"io.sarl.lang.core.tests.compile02.A2",
					STATIC_EXTFIELD_LOCALFUNCTION_JAVA);
		}

		private final String STATIC_EXTFIELD_A_SARL = multilineString(
				"package io.sarl.lang.core.tests.compile03",
				"import java.util.UUID",
				"class A0 {",
				"}",
				"class A1 {",
				"  def toUUID(v : A0) : UUID { null }",
				"}",
				"class StaticA1 {",
				"  static def toUUID(v : A0) : UUID { null }",
				"}"
				);

		private final String STATIC_EXTFIELD_B_SARL = multilineString(
				"package io.sarl.lang.core.tests.compile03",
				"import static extension o.sarl.lang.core.tests.compile03.StaticA1.*",
				"import java.util.UUID",
				"class A2 {",
				"  extension var foo : A1",
				"  def fct(x : A0) : UUID {",
				"    x as UUID",
				"  }",
				"}"
				);

		private final String STATIC_EXTFIELD_JAVA = multilineString(
				"package io.sarl.lang.core.tests.compile03;",
				"",
				"import io.sarl.lang.annotation.SarlElementType;",
				"import io.sarl.lang.annotation.SarlSpecification;",
				"import io.sarl.lang.annotation.SyntheticMember;",
				"import io.sarl.lang.core.tests.compile03.A0;",
				"import io.sarl.lang.core.tests.compile03.A1;",
				"import java.util.UUID;",
				"import org.eclipse.xtext.xbase.lib.Extension;",
				"import org.eclipse.xtext.xbase.lib.Pure;",
				"",
				"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
				"@SarlElementType(" + SarlPackage.SARL_CLASS + ")",
				"@SuppressWarnings(\"all\")",
				"public class A2 {",
				"  @Extension",
				"  private A1 foo;",
				"  ",
				"  @Pure",
				"  public UUID fct(final A0 x) {",
				"    return (x == null ? null : this.foo.toUUID(x));",
				"  }",
				"  ",
				"  @Override",
				"  @Pure",
				"  @SyntheticMember",
				"  public boolean equals(final Object obj) {",
				"    return super.equals(obj);",
				"  }",
				"  ",
				"  @Override",
				"  @Pure",
				"  @SyntheticMember",
				"  public int hashCode() {",
				"    int result = super.hashCode();",
				"    return result;",
				"  }",
				"  ",
				"  @SyntheticMember",
				"  public A2() {",
				"    super();",
				"  }",
				"}",
				"");

		@GlobalCompilationTestContribution
		public void compile03(ResourceSetGlobalCompilationContext ctx) throws Exception {
			ctx.compileTo(
					new String[] { STATIC_EXTFIELD_A_SARL, STATIC_EXTFIELD_B_SARL },
					"io.sarl.lang.core.tests.compile03.A2",
					STATIC_EXTFIELD_JAVA);
		}

	}

}
