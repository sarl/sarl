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

import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Nested;
import org.junit.jupiter.api.Tag;
import org.junit.jupiter.api.Test;

import io.sarl.lang.SARLVersion;
import io.sarl.lang.sarl.SarlPackage;
import io.sarl.lang.sarl.SarlScript;
import io.sarl.tests.api.AbstractSarlTest;
import io.sarl.tests.api.tools.TestValidator.Validator;

/** Testing class for issue: Invalid enclosing type from anonymous class.
 *
 * <p>https://github.com/sarl/sarl/issues/600
 *
 * @author $Author: sgalland$
 * @version $Name$ $Revision$ $Date$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 */
@DisplayName("Bug #600")
@SuppressWarnings("all")
@Tag("core")
public class Bug600Test {

	@Nested
	public class WithinConstructor extends AbstractSarlTest {

		private final String SNIPSET1 = multilineString(
				"package io.sarl.lang.tests.bug600",
				"import java.awt.^event.MouseMotionListener",
				"import java.awt.^event.MouseEvent",
				"class XXX {",
				"  new {",
				"    new MouseMotionListener {",
				"      override mouseDragged(e : MouseEvent) {",
				"      }",
				"      override mouseMoved(e : MouseEvent) {",
				"       XXX.this.setMouseTargetOnScreen(new Object)",
				"      }",
				"    }",
				"  }",
				"  protected def setMouseTargetOnScreen(obj : Object) : void {",
				"  }",
				"}");

		private final String SNIPSET2 = multilineString(
				"package io.sarl.lang.tests.bug600",
				"import java.awt.^event.MouseMotionListener",
				"import java.awt.^event.MouseEvent",
				"class XXX {",
				"  new {",
				"    new MouseMotionListener {",
				"      override mouseDragged(e : MouseEvent) {",
				"      }",
				"      override mouseMoved(e : MouseEvent) {",
				"       setMouseTargetOnScreen(new Object)",
				"      }",
				"    }",
				"  }",
				"  protected def setMouseTargetOnScreen(obj : Object) : void {",
				"  }",
				"}");

		private final String SNIPSET3 = multilineString(
				"package io.sarl.lang.tests.bug600",
				"import java.awt.^event.MouseMotionListener",
				"import java.awt.^event.MouseEvent",
				"class XXX {",
				"  new {",
				"    new MouseMotionListener {",
				"      override mouseDragged(e : MouseEvent) {",
				"      }",
				"      override mouseMoved(e : MouseEvent) {",
				"       XXX.this.mouseTargetOnScreen = new Object",
				"      }",
				"    }",
				"  }",
				"  protected def setMouseTargetOnScreen(obj : Object) : void {",
				"  }",
				"}");

		private final String SNIPSET4 = multilineString(
				"package io.sarl.lang.tests.bug600",
				"import java.awt.^event.MouseMotionListener",
				"import java.awt.^event.MouseEvent",
				"class XXX {",
				"  new {",
				"    new MouseMotionListener {",
				"      override mouseDragged(e : MouseEvent) {",
				"      }",
				"      override mouseMoved(e : MouseEvent) {",
				"       mouseTargetOnScreen = new Object",
				"      }",
				"    }",
				"  }",
				"  protected def setMouseTargetOnScreen(obj : Object) : void {",
				"  }",
				"}");

		private final String SNIPSET5 = multilineString(
				"package io.sarl.lang.tests.bug600",
				"import java.awt.^event.MouseMotionListener",
				"import java.awt.^event.MouseEvent",
				"class XXX {",
				"  protected def setMouseTargetOnScreen(obj : Object) : void {",
				"  }",
				"  new {",
				"    new MouseMotionListener {",
				"      override mouseDragged(e : MouseEvent) {",
				"      }",
				"      override mouseMoved(e : MouseEvent) {",
				"       XXX.this.setMouseTargetOnScreen(new Object)",
				"      }",
				"    }",
				"  }",
				"}");

		private final String SNIPSET6 = multilineString(
				"package io.sarl.lang.tests.bug600",
				"import java.awt.^event.MouseMotionListener",
				"import java.awt.^event.MouseEvent",
				"class XXX {",
				"  protected def setMouseTargetOnScreen(obj : Object) : void {",
				"  }",
				"  new {",
				"    new MouseMotionListener {",
				"      override mouseDragged(e : MouseEvent) {",
				"      }",
				"      override mouseMoved(e : MouseEvent) {",
				"       setMouseTargetOnScreen(new Object)",
				"      }",
				"    }",
				"  }",
				"}");

		private final String SNIPSET7 = multilineString(
				"package io.sarl.lang.tests.bug600",
				"import java.awt.^event.MouseMotionListener",
				"import java.awt.^event.MouseEvent",
				"class XXX {",
				"  protected def setMouseTargetOnScreen(obj : Object) : void {",
				"  }",
				"  new {",
				"    new MouseMotionListener {",
				"      override mouseDragged(e : MouseEvent) {",
				"      }",
				"      override mouseMoved(e : MouseEvent) {",
				"       XXX.this.mouseTargetOnScreen = new Object",
				"      }",
				"    }",
				"  }",
				"}");

		private final String SNIPSET8 = multilineString(
				"package io.sarl.lang.tests.bug600",
				"import java.awt.^event.MouseMotionListener",
				"import java.awt.^event.MouseEvent",
				"class XXX {",
				"  protected def setMouseTargetOnScreen(obj : Object) : void {",
				"  }",
				"  new {",
				"    new MouseMotionListener {",
				"      override mouseDragged(e : MouseEvent) {",
				"      }",
				"      override mouseMoved(e : MouseEvent) {",
				"       mouseTargetOnScreen = new Object",
				"      }",
				"    }",
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
					"package io.sarl.lang.tests.bug600;",
					"",
					"import io.sarl.lang.annotation.SarlElementType;",
					"import io.sarl.lang.annotation.SarlSpecification;",
					"import java.awt.event.MouseEvent;",
					"import java.awt.event.MouseMotionListener;",
					"",
					"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
					"@SarlElementType(" + SarlPackage.SARL_CLASS + ")",
					"@SuppressWarnings(\"all\")",
					"public class XXX {",
					"  public XXX() {",
					"    new MouseMotionListener() {",
					"      @Override",
					"      public void mouseDragged(final MouseEvent e) {",
					"      }",
					"      ",
					"      @Override",
					"      public void mouseMoved(final MouseEvent e) {",
					"        Object _object = new Object();",
					"        XXX.this.setMouseTargetOnScreen(_object);",
					"      }",
					"    };",
					"  }",
					"  ",
					"  protected void setMouseTargetOnScreen(final Object obj) {",
					"  }",
					"}",
					""));
		}

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
					"package io.sarl.lang.tests.bug600;",
					"",
					"import io.sarl.lang.annotation.SarlElementType;",
					"import io.sarl.lang.annotation.SarlSpecification;",
					"import java.awt.event.MouseEvent;",
					"import java.awt.event.MouseMotionListener;",
					"",
					"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
					"@SarlElementType(" + SarlPackage.SARL_CLASS + ")",
					"@SuppressWarnings(\"all\")",
					"public class XXX {",
					"  public XXX() {",
					"    new MouseMotionListener() {",
					"      @Override",
					"      public void mouseDragged(final MouseEvent e) {",
					"      }",
					"      ",
					"      @Override",
					"      public void mouseMoved(final MouseEvent e) {",
					"        Object _object = new Object();",
					"        XXX.this.setMouseTargetOnScreen(_object);",
					"      }",
					"    };",
					"  }",
					"  ",
					"  protected void setMouseTargetOnScreen(final Object obj) {",
					"  }",
					"}",
					""));
		}

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
					"package io.sarl.lang.tests.bug600;",
					"",
					"import io.sarl.lang.annotation.SarlElementType;",
					"import io.sarl.lang.annotation.SarlSpecification;",
					"import java.awt.event.MouseEvent;",
					"import java.awt.event.MouseMotionListener;",
					"",
					"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
					"@SarlElementType(" + SarlPackage.SARL_CLASS + ")",
					"@SuppressWarnings(\"all\")",
					"public class XXX {",
					"  public XXX() {",
					"    new MouseMotionListener() {",
					"      @Override",
					"      public void mouseDragged(final MouseEvent e) {",
					"      }",
					"      ",
					"      @Override",
					"      public void mouseMoved(final MouseEvent e) {",
					"        Object _object = new Object();",
					"        XXX.this.setMouseTargetOnScreen(_object);",
					"      }",
					"    };",
					"  }",
					"  ",
					"  protected void setMouseTargetOnScreen(final Object obj) {",
					"  }",
					"}",
					""));
		}

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
					"package io.sarl.lang.tests.bug600;",
					"",
					"import io.sarl.lang.annotation.SarlElementType;",
					"import io.sarl.lang.annotation.SarlSpecification;",
					"import java.awt.event.MouseEvent;",
					"import java.awt.event.MouseMotionListener;",
					"",
					"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
					"@SarlElementType(" + SarlPackage.SARL_CLASS + ")",
					"@SuppressWarnings(\"all\")",
					"public class XXX {",
					"  public XXX() {",
					"    new MouseMotionListener() {",
					"      @Override",
					"      public void mouseDragged(final MouseEvent e) {",
					"      }",
					"      ",
					"      @Override",
					"      public void mouseMoved(final MouseEvent e) {",
					"        Object _object = new Object();",
					"        XXX.this.setMouseTargetOnScreen(_object);",
					"      }",
					"    };",
					"  }",
					"  ",
					"  protected void setMouseTargetOnScreen(final Object obj) {",
					"  }",
					"}",
					""));
		}

		@Test
		@Tag("sarlValidation")
		public void parsing_05() throws Exception {
			SarlScript mas = file(getParseHelper(), SNIPSET5);
			final Validator validator = validate(getValidationHelper(), getInjector(), mas);
			validator.assertNoErrors();
		}

		@Test
		@Tag("compileToJava")
		public void compiling_05() throws Exception {
			getCompileHelper().assertCompilesTo(SNIPSET5, multilineString(
					"package io.sarl.lang.tests.bug600;",
					"",
					"import io.sarl.lang.annotation.SarlElementType;",
					"import io.sarl.lang.annotation.SarlSpecification;",
					"import java.awt.event.MouseEvent;",
					"import java.awt.event.MouseMotionListener;",
					"",
					"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
					"@SarlElementType(" + SarlPackage.SARL_CLASS + ")",
					"@SuppressWarnings(\"all\")",
					"public class XXX {",
					"  protected void setMouseTargetOnScreen(final Object obj) {",
					"  }",
					"  ",
					"  public XXX() {",
					"    new MouseMotionListener() {",
					"      @Override",
					"      public void mouseDragged(final MouseEvent e) {",
					"      }",
					"      ",
					"      @Override",
					"      public void mouseMoved(final MouseEvent e) {",
					"        Object _object = new Object();",
					"        XXX.this.setMouseTargetOnScreen(_object);",
					"      }",
					"    };",
					"  }",
					"}",
					""));
		}

		@Test
		@Tag("sarlValidation")
		public void parsing_06() throws Exception {
			SarlScript mas = file(getParseHelper(), SNIPSET6);
			final Validator validator = validate(getValidationHelper(), getInjector(), mas);
			validator.assertNoErrors();
		}

		@Test
		@Tag("compileToJava")
		public void compiling_06() throws Exception {
			getCompileHelper().assertCompilesTo(SNIPSET6, multilineString(
					"package io.sarl.lang.tests.bug600;",
					"",
					"import io.sarl.lang.annotation.SarlElementType;",
					"import io.sarl.lang.annotation.SarlSpecification;",
					"import java.awt.event.MouseEvent;",
					"import java.awt.event.MouseMotionListener;",
					"",
					"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
					"@SarlElementType(" + SarlPackage.SARL_CLASS + ")",
					"@SuppressWarnings(\"all\")",
					"public class XXX {",
					"  protected void setMouseTargetOnScreen(final Object obj) {",
					"  }",
					"  ",
					"  public XXX() {",
					"    new MouseMotionListener() {",
					"      @Override",
					"      public void mouseDragged(final MouseEvent e) {",
					"      }",
					"      ",
					"      @Override",
					"      public void mouseMoved(final MouseEvent e) {",
					"        Object _object = new Object();",
					"        XXX.this.setMouseTargetOnScreen(_object);",
					"      }",
					"    };",
					"  }",
					"}",
					""));
		}

		@Test
		@Tag("sarlValidation")
		public void parsing_07() throws Exception {
			SarlScript mas = file(getParseHelper(), SNIPSET7);
			final Validator validator = validate(getValidationHelper(), getInjector(), mas);
			validator.assertNoErrors();
		}

		@Test
		@Tag("compileToJava")
		public void compiling_07() throws Exception {
			getCompileHelper().assertCompilesTo(SNIPSET7, multilineString(
					"package io.sarl.lang.tests.bug600;",
					"",
					"import io.sarl.lang.annotation.SarlElementType;",
					"import io.sarl.lang.annotation.SarlSpecification;",
					"import java.awt.event.MouseEvent;",
					"import java.awt.event.MouseMotionListener;",
					"",
					"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
					"@SarlElementType(" + SarlPackage.SARL_CLASS + ")",
					"@SuppressWarnings(\"all\")",
					"public class XXX {",
					"  protected void setMouseTargetOnScreen(final Object obj) {",
					"  }",
					"  ",
					"  public XXX() {",
					"    new MouseMotionListener() {",
					"      @Override",
					"      public void mouseDragged(final MouseEvent e) {",
					"      }",
					"      ",
					"      @Override",
					"      public void mouseMoved(final MouseEvent e) {",
					"        Object _object = new Object();",
					"        XXX.this.setMouseTargetOnScreen(_object);",
					"      }",
					"    };",
					"  }",
					"}",
					""));
		}

		@Test
		@Tag("sarlValidation")
		public void parsing_08() throws Exception {
			SarlScript mas = file(getParseHelper(), SNIPSET8);
			final Validator validator = validate(getValidationHelper(), getInjector(), mas);
			validator.assertNoErrors();
		}

		@Test
		@Tag("compileToJava")
		public void compiling_08() throws Exception {
			getCompileHelper().assertCompilesTo(SNIPSET8, multilineString(
					"package io.sarl.lang.tests.bug600;",
					"",
					"import io.sarl.lang.annotation.SarlElementType;",
					"import io.sarl.lang.annotation.SarlSpecification;",
					"import java.awt.event.MouseEvent;",
					"import java.awt.event.MouseMotionListener;",
					"",
					"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
					"@SarlElementType(" + SarlPackage.SARL_CLASS + ")",
					"@SuppressWarnings(\"all\")",
					"public class XXX {",
					"  protected void setMouseTargetOnScreen(final Object obj) {",
					"  }",
					"  ",
					"  public XXX() {",
					"    new MouseMotionListener() {",
					"      @Override",
					"      public void mouseDragged(final MouseEvent e) {",
					"      }",
					"      ",
					"      @Override",
					"      public void mouseMoved(final MouseEvent e) {",
					"        Object _object = new Object();",
					"        XXX.this.setMouseTargetOnScreen(_object);",
					"      }",
					"    };",
					"  }",
					"}",
					""));
		}

	}

	@Nested
	public class WithinAction extends AbstractSarlTest {

		private final String SNIPSET1 = multilineString(
				"package io.sarl.lang.tests.bug600",
				"import java.awt.^event.MouseMotionListener",
				"import java.awt.^event.MouseEvent",
				"class XXX {",
				"  def originalFunction : Object {",
				"    new MouseMotionListener {",
				"      override mouseDragged(e : MouseEvent) {",
				"      }",
				"      override mouseMoved(e : MouseEvent) {",
				"       XXX.this.setMouseTargetOnScreen(new Object)",
				"      }",
				"    }",
				"  }",
				"  protected def setMouseTargetOnScreen(obj : Object) : void {",
				"  }",
				"}");

		private final String SNIPSET2 = multilineString(
				"package io.sarl.lang.tests.bug600",
				"import java.awt.^event.MouseMotionListener",
				"import java.awt.^event.MouseEvent",
				"class XXX {",
				"  def originalFunction : Object {",
				"    new MouseMotionListener {",
				"      override mouseDragged(e : MouseEvent) {",
				"      }",
				"      override mouseMoved(e : MouseEvent) {",
				"       setMouseTargetOnScreen(new Object)",
				"      }",
				"    }",
				"  }",
				"  protected def setMouseTargetOnScreen(obj : Object) : void {",
				"  }",
				"}");

		private final String SNIPSET3 = multilineString(
				"package io.sarl.lang.tests.bug600",
				"import java.awt.^event.MouseMotionListener",
				"import java.awt.^event.MouseEvent",
				"class XXX {",
				"  def originalFunction : Object {",
				"    new MouseMotionListener {",
				"      override mouseDragged(e : MouseEvent) {",
				"      }",
				"      override mouseMoved(e : MouseEvent) {",
				"       XXX.this.mouseTargetOnScreen = new Object",
				"      }",
				"    }",
				"  }",
				"  protected def setMouseTargetOnScreen(obj : Object) : void {",
				"  }",
				"}");

		private final String SNIPSET4 = multilineString(
				"package io.sarl.lang.tests.bug600",
				"import java.awt.^event.MouseMotionListener",
				"import java.awt.^event.MouseEvent",
				"class XXX {",
				"  def originalFunction : Object {",
				"    new MouseMotionListener {",
				"      override mouseDragged(e : MouseEvent) {",
				"      }",
				"      override mouseMoved(e : MouseEvent) {",
				"       mouseTargetOnScreen = new Object",
				"      }",
				"    }",
				"  }",
				"  protected def setMouseTargetOnScreen(obj : Object) : void {",
				"  }",
				"}");

		private final String SNIPSET5 = multilineString(
				"package io.sarl.lang.tests.bug600",
				"import java.awt.^event.MouseMotionListener",
				"import java.awt.^event.MouseEvent",
				"class XXX {",
				"  protected def setMouseTargetOnScreen(obj : Object) : void {",
				"  }",
				"  def originalFunction : Object {",
				"    new MouseMotionListener {",
				"      override mouseDragged(e : MouseEvent) {",
				"      }",
				"      override mouseMoved(e : MouseEvent) {",
				"       XXX.this.setMouseTargetOnScreen(new Object)",
				"      }",
				"    }",
				"  }",
				"}");

		private final String SNIPSET6 = multilineString(
				"package io.sarl.lang.tests.bug600",
				"import java.awt.^event.MouseMotionListener",
				"import java.awt.^event.MouseEvent",
				"class XXX {",
				"  protected def setMouseTargetOnScreen(obj : Object) : void {",
				"  }",
				"  def originalFunction : Object {",
				"    new MouseMotionListener {",
				"      override mouseDragged(e : MouseEvent) {",
				"      }",
				"      override mouseMoved(e : MouseEvent) {",
				"       setMouseTargetOnScreen(new Object)",
				"      }",
				"    }",
				"  }",
				"}");

		private final String SNIPSET7 = multilineString(
				"package io.sarl.lang.tests.bug600",
				"import java.awt.^event.MouseMotionListener",
				"import java.awt.^event.MouseEvent",
				"class XXX {",
				"  protected def setMouseTargetOnScreen(obj : Object) : void {",
				"  }",
				"  def originalFunction : Object {",
				"    new MouseMotionListener {",
				"      override mouseDragged(e : MouseEvent) {",
				"      }",
				"      override mouseMoved(e : MouseEvent) {",
				"       XXX.this.mouseTargetOnScreen = new Object",
				"      }",
				"    }",
				"  }",
				"}");

		private final String SNIPSET8 = multilineString(
				"package io.sarl.lang.tests.bug600",
				"import java.awt.^event.MouseMotionListener",
				"import java.awt.^event.MouseEvent",
				"class XXX {",
				"  protected def setMouseTargetOnScreen(obj : Object) : void {",
				"  }",
				"  def originalFunction : Object {",
				"    new MouseMotionListener {",
				"      override mouseDragged(e : MouseEvent) {",
				"      }",
				"      override mouseMoved(e : MouseEvent) {",
				"       mouseTargetOnScreen = new Object",
				"      }",
				"    }",
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
					"package io.sarl.lang.tests.bug600;",
					"",
					"import io.sarl.lang.annotation.SarlElementType;",
					"import io.sarl.lang.annotation.SarlSpecification;",
					"import io.sarl.lang.annotation.SyntheticMember;",
					"import java.awt.event.MouseEvent;",
					"import java.awt.event.MouseMotionListener;",
					"import org.eclipse.xtext.xbase.lib.Pure;",
					"",
					"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
					"@SarlElementType(" + SarlPackage.SARL_CLASS + ")",
					"@SuppressWarnings(\"all\")",
					"public class XXX {",
					"  @Pure",
					"  public Object originalFunction() {",
					"    return new MouseMotionListener() {",
					"      @Override",
					"      public void mouseDragged(final MouseEvent e) {",
					"      }",
					"      ",
					"      @Override",
					"      public void mouseMoved(final MouseEvent e) {",
					"        Object _object = new Object();",
					"        XXX.this.setMouseTargetOnScreen(_object);",
					"      }",
					"    };",
					"  }",
					"  ",
					"  protected void setMouseTargetOnScreen(final Object obj) {",
					"  }",
					"  ",
					"  @SyntheticMember",
					"  public XXX() {",
					"    super();",
					"  }",
					"}",
					""));
		}

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
					"package io.sarl.lang.tests.bug600;",
					"",
					"import io.sarl.lang.annotation.SarlElementType;",
					"import io.sarl.lang.annotation.SarlSpecification;",
					"import io.sarl.lang.annotation.SyntheticMember;",
					"import java.awt.event.MouseEvent;",
					"import java.awt.event.MouseMotionListener;",
					"import org.eclipse.xtext.xbase.lib.Pure;",
					"",
					"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
					"@SarlElementType(" + SarlPackage.SARL_CLASS + ")",
					"@SuppressWarnings(\"all\")",
					"public class XXX {",
					"  @Pure",
					"  public Object originalFunction() {",
					"    return new MouseMotionListener() {",
					"      @Override",
					"      public void mouseDragged(final MouseEvent e) {",
					"      }",
					"      ",
					"      @Override",
					"      public void mouseMoved(final MouseEvent e) {",
					"        Object _object = new Object();",
					"        XXX.this.setMouseTargetOnScreen(_object);",
					"      }",
					"    };",
					"  }",
					"  ",
					"  protected void setMouseTargetOnScreen(final Object obj) {",
					"  }",
					"  ",
					"  @SyntheticMember",
					"  public XXX() {",
					"    super();",
					"  }",
					"}",
					""));
		}

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
					"package io.sarl.lang.tests.bug600;",
					"",
					"import io.sarl.lang.annotation.SarlElementType;",
					"import io.sarl.lang.annotation.SarlSpecification;",
					"import io.sarl.lang.annotation.SyntheticMember;",
					"import java.awt.event.MouseEvent;",
					"import java.awt.event.MouseMotionListener;",
					"import org.eclipse.xtext.xbase.lib.Pure;",
					"",
					"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
					"@SarlElementType(" + SarlPackage.SARL_CLASS + ")",
					"@SuppressWarnings(\"all\")",
					"public class XXX {",
					"  @Pure",
					"  public Object originalFunction() {",
					"    return new MouseMotionListener() {",
					"      @Override",
					"      public void mouseDragged(final MouseEvent e) {",
					"      }",
					"      ",
					"      @Override",
					"      public void mouseMoved(final MouseEvent e) {",
					"        Object _object = new Object();",
					"        XXX.this.setMouseTargetOnScreen(_object);",
					"      }",
					"    };",
					"  }",
					"  ",
					"  protected void setMouseTargetOnScreen(final Object obj) {",
					"  }",
					"  ",
					"  @SyntheticMember",
					"  public XXX() {",
					"    super();",
					"  }",
					"}",
					""));
		}

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
					"package io.sarl.lang.tests.bug600;",
					"",
					"import io.sarl.lang.annotation.SarlElementType;",
					"import io.sarl.lang.annotation.SarlSpecification;",
					"import io.sarl.lang.annotation.SyntheticMember;",
					"import java.awt.event.MouseEvent;",
					"import java.awt.event.MouseMotionListener;",
					"import org.eclipse.xtext.xbase.lib.Pure;",
					"",
					"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
					"@SarlElementType(" + SarlPackage.SARL_CLASS + ")",
					"@SuppressWarnings(\"all\")",
					"public class XXX {",
					"  @Pure",
					"  public Object originalFunction() {",
					"    return new MouseMotionListener() {",
					"      @Override",
					"      public void mouseDragged(final MouseEvent e) {",
					"      }",
					"      ",
					"      @Override",
					"      public void mouseMoved(final MouseEvent e) {",
					"        Object _object = new Object();",
					"        XXX.this.setMouseTargetOnScreen(_object);",
					"      }",
					"    };",
					"  }",
					"  ",
					"  protected void setMouseTargetOnScreen(final Object obj) {",
					"  }",
					"  ",
					"  @SyntheticMember",
					"  public XXX() {",
					"    super();",
					"  }",
					"}",
					""));
		}

		@Test
		@Tag("sarlValidation")
		public void parsing_05() throws Exception {
			SarlScript mas = file(getParseHelper(), SNIPSET5);
			final Validator validator = validate(getValidationHelper(), getInjector(), mas);
			validator.assertNoErrors();
		}

		@Test
		@Tag("compileToJava")
		public void compiling_05() throws Exception {
			getCompileHelper().assertCompilesTo(SNIPSET5, multilineString(
					"package io.sarl.lang.tests.bug600;",
					"",
					"import io.sarl.lang.annotation.SarlElementType;",
					"import io.sarl.lang.annotation.SarlSpecification;",
					"import io.sarl.lang.annotation.SyntheticMember;",
					"import java.awt.event.MouseEvent;",
					"import java.awt.event.MouseMotionListener;",
					"import org.eclipse.xtext.xbase.lib.Pure;",
					"",
					"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
					"@SarlElementType(" + SarlPackage.SARL_CLASS + ")",
					"@SuppressWarnings(\"all\")",
					"public class XXX {",
					"  protected void setMouseTargetOnScreen(final Object obj) {",
					"  }",
					"  ",
					"  @Pure",
					"  public Object originalFunction() {",
					"    return new MouseMotionListener() {",
					"      @Override",
					"      public void mouseDragged(final MouseEvent e) {",
					"      }",
					"      ",
					"      @Override",
					"      public void mouseMoved(final MouseEvent e) {",
					"        Object _object = new Object();",
					"        XXX.this.setMouseTargetOnScreen(_object);",
					"      }",
					"    };",
					"  }",
					"  ",
					"  @SyntheticMember",
					"  public XXX() {",
					"    super();",
					"  }",
					"}",
					""));
		}

		@Test
		@Tag("sarlValidation")
		public void parsing_06() throws Exception {
			SarlScript mas = file(getParseHelper(), SNIPSET6);
			final Validator validator = validate(getValidationHelper(), getInjector(), mas);
			validator.assertNoErrors();
		}

		@Test
		@Tag("compileToJava")
		public void compiling_06() throws Exception {
			getCompileHelper().assertCompilesTo(SNIPSET6, multilineString(
					"package io.sarl.lang.tests.bug600;",
					"",
					"import io.sarl.lang.annotation.SarlElementType;",
					"import io.sarl.lang.annotation.SarlSpecification;",
					"import io.sarl.lang.annotation.SyntheticMember;",
					"import java.awt.event.MouseEvent;",
					"import java.awt.event.MouseMotionListener;",
					"import org.eclipse.xtext.xbase.lib.Pure;",
					"",
					"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
					"@SarlElementType(" + SarlPackage.SARL_CLASS + ")",
					"@SuppressWarnings(\"all\")",
					"public class XXX {",
					"  protected void setMouseTargetOnScreen(final Object obj) {",
					"  }",
					"  ",
					"  @Pure",
					"  public Object originalFunction() {",
					"    return new MouseMotionListener() {",
					"      @Override",
					"      public void mouseDragged(final MouseEvent e) {",
					"      }",
					"      ",
					"      @Override",
					"      public void mouseMoved(final MouseEvent e) {",
					"        Object _object = new Object();",
					"        XXX.this.setMouseTargetOnScreen(_object);",
					"      }",
					"    };",
					"  }",
					"  ",
					"  @SyntheticMember",
					"  public XXX() {",
					"    super();",
					"  }",
					"}",
					""));
		}

		@Test
		@Tag("sarlValidation")
		public void parsing_07() throws Exception {
			SarlScript mas = file(getParseHelper(), SNIPSET7);
			final Validator validator = validate(getValidationHelper(), getInjector(), mas);
			validator.assertNoErrors();
		}

		@Test
		@Tag("compileToJava")
		public void compiling_07() throws Exception {
			getCompileHelper().assertCompilesTo(SNIPSET7, multilineString(
					"package io.sarl.lang.tests.bug600;",
					"",
					"import io.sarl.lang.annotation.SarlElementType;",
					"import io.sarl.lang.annotation.SarlSpecification;",
					"import io.sarl.lang.annotation.SyntheticMember;",
					"import java.awt.event.MouseEvent;",
					"import java.awt.event.MouseMotionListener;",
					"import org.eclipse.xtext.xbase.lib.Pure;",
					"",
					"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
					"@SarlElementType(" + SarlPackage.SARL_CLASS + ")",
					"@SuppressWarnings(\"all\")",
					"public class XXX {",
					"  protected void setMouseTargetOnScreen(final Object obj) {",
					"  }",
					"  ",
					"  @Pure",
					"  public Object originalFunction() {",
					"    return new MouseMotionListener() {",
					"      @Override",
					"      public void mouseDragged(final MouseEvent e) {",
					"      }",
					"      ",
					"      @Override",
					"      public void mouseMoved(final MouseEvent e) {",
					"        Object _object = new Object();",
					"        XXX.this.setMouseTargetOnScreen(_object);",
					"      }",
					"    };",
					"  }",
					"  ",
					"  @SyntheticMember",
					"  public XXX() {",
					"    super();",
					"  }",
					"}",
					""));
		}

		@Test
		@Tag("sarlValidation")
		public void parsing_08() throws Exception {
			SarlScript mas = file(getParseHelper(), SNIPSET8);
			final Validator validator = validate(getValidationHelper(), getInjector(), mas);
			validator.assertNoErrors();
		}

		@Test
		@Tag("compileToJava")
		public void compiling_08() throws Exception {
			getCompileHelper().assertCompilesTo(SNIPSET8, multilineString(
					"package io.sarl.lang.tests.bug600;",
					"",
					"import io.sarl.lang.annotation.SarlElementType;",
					"import io.sarl.lang.annotation.SarlSpecification;",
					"import io.sarl.lang.annotation.SyntheticMember;",
					"import java.awt.event.MouseEvent;",
					"import java.awt.event.MouseMotionListener;",
					"import org.eclipse.xtext.xbase.lib.Pure;",
					"",
					"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
					"@SarlElementType(" + SarlPackage.SARL_CLASS + ")",
					"@SuppressWarnings(\"all\")",
					"public class XXX {",
					"  protected void setMouseTargetOnScreen(final Object obj) {",
					"  }",
					"  ",
					"  @Pure",
					"  public Object originalFunction() {",
					"    return new MouseMotionListener() {",
					"      @Override",
					"      public void mouseDragged(final MouseEvent e) {",
					"      }",
					"      ",
					"      @Override",
					"      public void mouseMoved(final MouseEvent e) {",
					"        Object _object = new Object();",
					"        XXX.this.setMouseTargetOnScreen(_object);",
					"      }",
					"    };",
					"  }",
					"  ",
					"  @SyntheticMember",
					"  public XXX() {",
					"    super();",
					"  }",
					"}",
					""));
		}

	}

}
