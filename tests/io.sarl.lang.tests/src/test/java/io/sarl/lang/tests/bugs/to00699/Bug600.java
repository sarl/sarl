/*
 * Copyright (C) 2014-2018 the original authors or authors.
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

import java.awt.event.WindowEvent;

import com.google.inject.Inject;
import org.eclipse.xtend.core.validation.IssueCodes;
import org.eclipse.xtext.xbase.XbasePackage;
import org.eclipse.xtext.xbase.testing.CompilationTestHelper;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.junit.runners.Suite;
import org.junit.runners.Suite.SuiteClasses;

import io.sarl.lang.SARLVersion;
import io.sarl.lang.sarl.SarlPackage;
import io.sarl.lang.sarl.SarlScript;
import io.sarl.tests.api.AbstractSarlTest;

/** Testing class for issue: Invalid enclosing type from anonymous class.
 *
 * <p>https://github.com/sarl/sarl/issues/600
 *
 * @author $Author: sgalland$
 * @version $Name$ $Revision$ $Date$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 */
@RunWith(Suite.class)
@SuiteClasses({
	Bug600.WithinConstructor.class,
	Bug600.WithinAction.class,
})
@SuppressWarnings("all")
public class Bug600 {

	public static class WithinConstructor extends AbstractSarlTest {

		private static final String SNIPSET1 = multilineString(
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

		private static final String SNIPSET2 = multilineString(
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

		private static final String SNIPSET3 = multilineString(
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

		private static final String SNIPSET4 = multilineString(
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

		private static final String SNIPSET5 = multilineString(
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

		private static final String SNIPSET6 = multilineString(
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

		private static final String SNIPSET7 = multilineString(
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

		private static final String SNIPSET8 = multilineString(
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
		public void parsing_01() throws Exception {
			SarlScript mas = file(SNIPSET1);
			final Validator validator = validate(mas);
			validator.assertNoErrors();
		}

		@Test
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
		public void parsing_02() throws Exception {
			SarlScript mas = file(SNIPSET2);
			final Validator validator = validate(mas);
			validator.assertNoErrors();
		}

		@Test
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
		public void parsing_03() throws Exception {
			SarlScript mas = file(SNIPSET3);
			final Validator validator = validate(mas);
			validator.assertNoErrors();
		}

		@Test
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
		public void parsing_04() throws Exception {
			SarlScript mas = file(SNIPSET4);
			final Validator validator = validate(mas);
			validator.assertNoErrors();
		}

		@Test
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
		public void parsing_05() throws Exception {
			SarlScript mas = file(SNIPSET5);
			final Validator validator = validate(mas);
			validator.assertNoErrors();
		}

		@Test
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
		public void parsing_06() throws Exception {
			SarlScript mas = file(SNIPSET6);
			final Validator validator = validate(mas);
			validator.assertNoErrors();
		}

		@Test
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
		public void parsing_07() throws Exception {
			SarlScript mas = file(SNIPSET7);
			final Validator validator = validate(mas);
			validator.assertNoErrors();
		}

		@Test
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
		public void parsing_08() throws Exception {
			SarlScript mas = file(SNIPSET8);
			final Validator validator = validate(mas);
			validator.assertNoErrors();
		}

		@Test
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

	public static class WithinAction extends AbstractSarlTest {

		private static final String SNIPSET1 = multilineString(
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

		private static final String SNIPSET2 = multilineString(
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

		private static final String SNIPSET3 = multilineString(
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

		private static final String SNIPSET4 = multilineString(
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

		private static final String SNIPSET5 = multilineString(
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

		private static final String SNIPSET6 = multilineString(
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

		private static final String SNIPSET7 = multilineString(
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

		private static final String SNIPSET8 = multilineString(
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
		public void parsing_01() throws Exception {
			SarlScript mas = file(SNIPSET1);
			final Validator validator = validate(mas);
			validator.assertNoErrors();
		}

		@Test
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
		public void parsing_02() throws Exception {
			SarlScript mas = file(SNIPSET2);
			final Validator validator = validate(mas);
			validator.assertNoErrors();
		}

		@Test
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
		public void parsing_03() throws Exception {
			SarlScript mas = file(SNIPSET3);
			final Validator validator = validate(mas);
			validator.assertNoErrors();
		}

		@Test
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
		public void parsing_04() throws Exception {
			SarlScript mas = file(SNIPSET4);
			final Validator validator = validate(mas);
			validator.assertNoErrors();
		}

		@Test
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
		public void parsing_05() throws Exception {
			SarlScript mas = file(SNIPSET5);
			final Validator validator = validate(mas);
			validator.assertNoErrors();
		}

		@Test
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
		public void parsing_06() throws Exception {
			SarlScript mas = file(SNIPSET6);
			final Validator validator = validate(mas);
			validator.assertNoErrors();
		}

		@Test
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
		public void parsing_07() throws Exception {
			SarlScript mas = file(SNIPSET7);
			final Validator validator = validate(mas);
			validator.assertNoErrors();
		}

		@Test
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
		public void parsing_08() throws Exception {
			SarlScript mas = file(SNIPSET8);
			final Validator validator = validate(mas);
			validator.assertNoErrors();
		}

		@Test
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
