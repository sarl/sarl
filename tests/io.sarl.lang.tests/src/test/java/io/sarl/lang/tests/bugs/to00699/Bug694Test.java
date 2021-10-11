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

/** Testing class for issue: Pure function not detected.
 *
 * <p>https://github.com/sarl/sarl/issues/694
 *
 * @author $Author: sgalland$
 * @version $Name$ $Revision$ $Date$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 */
@DisplayName("Bug #694")
@SuppressWarnings("all")
@Tag("core")
public class Bug694Test extends AbstractSarlTest {

	private static final String SNIPSET1 = multilineString(
			"package io.sarl.lang.tests.bug694",
			"import java.util.List",
			"import java.util.ArrayList",
			"class X {",
			"   var f : int",
			"	def myfunction00{",
			"	}",
			"	def myfunction01(p : float) : float{",
			"		p",
			"	}",
			"	def myfunction02 : float{",
			"		6",
			"	}",
			"	def myfunction03{",
			"      myfunction04",
			"	}",
			"	def myfunction04 : float{",
			"		f + 5",
			"	}",
			"	def myfunction05(p : float){",
			"		f = p as int",
			"	}",
			"	def myfunction06 {",
			"		myfunction03",
			"	}",
			"	def myfunction07(p: float){",
			"		myfunction05(p)",
			"	}",
			"	def myfunction08(p: float){",
			"		myfunction01(p)",
			"	}",
			"	def myfunction09(p : List<Object>){",
			"		p.add(new Object)",
			"	}",
			"	def myfunction10(p : List<Object>){",
			"       p.empty",
			"	}",
			"	def myfunction11(p : List<Object>){",
			"       var x = p",
			"		x.add(new Object)",
			"	}",
			"	def myfunction12(p : List<Object>){",
			"       var x = p",
			"       if (p.empty) {",
			"         x = new ArrayList",
			"       }",
			"		x.add(new Object)",
			"	}",
			"	def myfunction13(p : List<Object>){",
			"       var x = p",
			"       if (p.empty) {",
			"         x = null",
			"       } else {",
			"         x = new ArrayList",
			"       }",
			"		x.add(new Object)",
			"	}",
			"}");

	private static final String SNIPSET2 = multilineString(
			"package io.sarl.lang.tests.bug694",
			"import java.util.List",
			"class X {",
			"   var f : int",
			"	def myfunction00 : float{",
			"		6",
			"	}",
			"	def myfunction01 : void{",
			"      f = 1",
			"	}",
			"}",
			"class Y extends X {",
			"	def myfunction00 : float{",
			"		7",
			"	}",
			"	def myfunction01 : void{",
			"      ",
			"	}",
			"}");

	private static final String SNIPSET3 = multilineString(
			"package io.sarl.lang.tests.bug694",
			"import java.util.ArrayList",
			"import java.util.List",
			"class X {",
			"	def f01(a : int, b : List<Object>) : void{",
			"		var y = new ArrayList",
			"		for (x : b) {",
			"			if (x == a) {",
			"				y.add(x)",
			"			}",
			"		}",
			"		if (b.empty) {",
			"			b.clear",
			"		}",
			"	}",
			"	def f02(a : int, b : List<Object>) : void{",
			"		var z = b",
			"		switch (a){",
			"		case 1: z = new ArrayList",
			"		case 2: z = null",
			"		}",
			"		z.add(new Object)",
			"	}",
			"	def f03(a : int, b : List<Object>) : void{",
			"		var z = b",
			"		if (true){",
			"			if (true){",
			"				z = null",
			"			}",
			"		}else{",
			"			if (true){",
			"				z = new ArrayList",
			"			}else{",
			"				z = null",
			"			}",
			"		}",
			"		z.clear",
			"	}",
			"	def f04(a : int, b : List<Object>) : void{",
			"		var w = a",
			"		w = 4",
			"	}",
			"}");

	private final String EXPECTED1 = multilineString(
			"package io.sarl.lang.tests.bug694;",
			"",
			"import io.sarl.lang.annotation.SarlElementType;", 
			"import io.sarl.lang.annotation.SarlSpecification;",
			"import io.sarl.lang.annotation.SyntheticMember;",
			"import java.util.ArrayList;",
			"import java.util.List;",
			"import org.eclipse.xtext.xbase.lib.Pure;",
			"",
			"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
			"@SarlElementType(" + SarlPackage.SARL_CLASS + ")",
			"@SuppressWarnings(\"all\")",
			"public class X {",
			"  private int f;",
			"  ",
			"  public void myfunction00() {",
			"  }",
			"  ",
			"  @Pure",
			"  public float myfunction01(final float p) {",
			"    return p;",
			"  }",
			"  ",
			"  @Pure",
			"  public float myfunction02() {",
			"    return 6;",
			"  }",
			"  ",
			"  @Pure",
			"  public float myfunction03() {",
			"    return this.myfunction04();",
			"  }",
			"  ",
			"  @Pure",
			"  public float myfunction04() {",
			"    return (this.f + 5);",
			"  }",
			"  ",
			"  public void myfunction05(final float p) {",
			"    this.f = ((int) p);",
			"  }",
			"  ",
			"  @Pure",
			"  public float myfunction06() {",
			"    return this.myfunction03();",
			"  }",
			"  ",
			"  public void myfunction07(final float p) {",
			"    this.myfunction05(p);",
			"  }",
			"  ",
			"  @Pure",
			"  public float myfunction08(final float p) {",
			"    return this.myfunction01(p);",
			"  }",
			"  ",
			"  public boolean myfunction09(final List<Object> p) {",
			"    Object _object = new Object();", 
			"    return p.add(_object);",
			"  }",
			"  ",
			"  @Pure",
			"  public boolean myfunction10(final List<Object> p) {",
			"    return p.isEmpty();",
			"  }",
			"  ",
			"  public boolean myfunction11(final List<Object> p) {",
			"    boolean _xblockexpression = false;",
			"    {",
			"      List<Object> x = p;", 
			"      Object _object = new Object();",
			"      _xblockexpression = x.add(_object);", 
			"    }",
			"    return _xblockexpression;",
			"  }",
			"  ",
			"  public boolean myfunction12(final List<Object> p) {",
			"    boolean _xblockexpression = false;",
			"    {",
			"      List<Object> x = p;",
			"      boolean _isEmpty = p.isEmpty();",
			"      if (_isEmpty) {",
			"        ArrayList<Object> _arrayList = new ArrayList<Object>();",
			"        x = _arrayList;",
			"      }",
			"      Object _object = new Object();",
			"      _xblockexpression = x.add(_object);",
			"    }",
			"    return _xblockexpression;",
			"  }",
			"  ",
			"  public boolean myfunction13(final List<Object> p) {",
			"    boolean _xblockexpression = false;",
			"    {",
			"      List<Object> x = p;",
			"      boolean _isEmpty = p.isEmpty();",
			"      if (_isEmpty) {",
			"        x = null;",
			"      } else {",
			"        ArrayList<Object> _arrayList = new ArrayList<Object>();",
			"        x = _arrayList;",
			"      }",
			"      Object _object = new Object();",
			"      _xblockexpression = x.add(_object);",
			"    }",
			"    return _xblockexpression;",
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
			"    X other = (X) obj;",
			"    if (other.f != this.f)",
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
			"    result = prime * result + Integer.hashCode(this.f);",
			"    return result;",
			"  }",
			"  ",
			"  @SyntheticMember", 
			"  public X() {",
			"    super();", 
			"  }",
			"}",
			"");

	private final String EXPECTED2_1 = multilineString(
			"package io.sarl.lang.tests.bug694;",
			"",
			"import io.sarl.lang.annotation.SarlElementType;", 
			"import io.sarl.lang.annotation.SarlSpecification;",
			"import io.sarl.lang.annotation.SyntheticMember;",
			"import org.eclipse.xtext.xbase.lib.Pure;",
			"",
			"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
			"@SarlElementType(" + SarlPackage.SARL_CLASS + ")",
			"@SuppressWarnings(\"all\")",
			"public class X {",
			"  private int f;",
			"  ",
			"  @Pure",
			"  public float myfunction00() {",
			"    return 6;",
			"  }",
			"  ",
			"  public void myfunction01() {",
			"    this.f = 1;",
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
			"    X other = (X) obj;",
			"    if (other.f != this.f)",
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
			"    result = prime * result + Integer.hashCode(this.f);",
			"    return result;",
			"  }",
			"  ",
			"  @SyntheticMember", 
			"  public X() {",
			"    super();", 
			"  }",
			"}",
			"");

	private final String EXPECTED2_2 = multilineString(
			"package io.sarl.lang.tests.bug694;",
			"",
			"import io.sarl.lang.annotation.SarlElementType;", 
			"import io.sarl.lang.annotation.SarlSpecification;",
			"import io.sarl.lang.annotation.SyntheticMember;",
			"import org.eclipse.xtext.xbase.lib.Pure;",
			"",
			"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
			"@SarlElementType(" + SarlPackage.SARL_CLASS + ")",
			"@SuppressWarnings(\"all\")",
			"public class Y extends X {",
			"  @Pure",
			"  public float myfunction00() {",
			"    return 7;",
			"  }",
			"  ",
			"  public void myfunction01() {",
			"  }",
			"  ",
			"  @SyntheticMember", 
			"  public Y() {",
			"    super();", 
			"  }",
			"}",
			"");

	private final String EXPECTED3 = multilineString(
			"package io.sarl.lang.tests.bug694;",
			"",
			"import com.google.common.base.Objects;",
			"import io.sarl.lang.annotation.SarlElementType;", 
			"import io.sarl.lang.annotation.SarlSpecification;",
			"import io.sarl.lang.annotation.SyntheticMember;",
			"import java.util.ArrayList;",
			"import java.util.List;",
			"",
			"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
			"@SarlElementType(" + SarlPackage.SARL_CLASS + ")",
			"@SuppressWarnings(\"all\")",
			"public class X {",
			"  public void f01(final int a, final List<Object> b) {", 
			"    ArrayList<Object> y = new ArrayList<Object>();", 
			"    for (final Object x : b) {", 
			"      boolean _equals = Objects.equal(x, Integer.valueOf(a));", 
			"      if (_equals) {", 
			"        y.add(x);", 
			"      }", 
			"    }", 
			"    boolean _isEmpty = b.isEmpty();", 
			"    if (_isEmpty) {", 
			"      b.clear();", 
			"    }", 
			"  }", 
			"  ", 
			"  public void f02(final int a, final List<Object> b) {", 
			"    List<Object> z = b;", 
			"    switch (a) {", 
			"      case 1:", 
			"        ArrayList<Object> _arrayList = new ArrayList<Object>();", 
			"        z = _arrayList;", 
			"        break;", 
			"      case 2:", 
			"        z = null;", 
			"        break;", 
			"    }", 
			"    Object _object = new Object();", 
			"    z.add(_object);", 
			"  }", 
			"  ", 
			"  public void f03(final int a, final List<Object> b) {", 
			"    List<Object> z = b;", 
			"    if (true) {", 
			"      if (true) {", 
			"        z = null;", 
			"      }", 
			"    } else {", 
			"      if (true) {", 
			"        ArrayList<Object> _arrayList = new ArrayList<Object>();", 
			"        z = _arrayList;", 
			"      } else {", 
			"        z = null;", 
			"      }", 
			"    }", 
			"    z.clear();", 
			"  }", 
			"  ", 
			"  public void f04(final int a, final List<Object> b) {", 
			"    int w = a;", 
			"    w = 4;", 
			"  }",
			"  ",
			"  @SyntheticMember", 
			"  public X() {",
			"    super();", 
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
			final String actual = it.getGeneratedCode("io.sarl.lang.tests.bug694.X");
			assertEquals(EXPECTED1, actual);
		});
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
		getCompileHelper().compile(SNIPSET2, (it) -> {
			assertEquals(EXPECTED2_1, it.getGeneratedCode("io.sarl.lang.tests.bug694.X"));
			assertEquals(EXPECTED2_2, it.getGeneratedCode("io.sarl.lang.tests.bug694.Y"));
		});
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
		getCompileHelper().compile(SNIPSET3, (it) -> {
			assertEquals(EXPECTED3, it.getGeneratedCode("io.sarl.lang.tests.bug694.X"));
		});
	}

	private static final String SNIPSET4 = multilineString(
			"package io.sarl.lang.tests.bug694",
			"import java.util.List",
			"class XXX {",
			"  def fct(a : int) : int {",
			"    fct(a + 1) + 1",
			"  }",
			"  def fct2(a : List<Object>) : void {",
			"    fct2(a)",
			"    a.add(new Object)",
			"  }",
			"}");

	private final String EXPECTED4 = multilineString(
			"package io.sarl.lang.tests.bug694;",
			"",
			"import io.sarl.lang.annotation.SarlElementType;",
			"import io.sarl.lang.annotation.SarlSpecification;",
			"import io.sarl.lang.annotation.SyntheticMember;",
			"import java.util.List;",
			"import org.eclipse.xtext.xbase.lib.Pure;",
			"",
			"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
			"@SarlElementType(" + SarlPackage.SARL_CLASS + ")",
			"@SuppressWarnings(\"all\")",
			"public class XXX {",
			"  @Pure",
			"  public int fct(final int a) {",
			"    int _fct = this.fct((a + 1));",
			"    return (_fct + 1);",
			"  }",
			"  ",
			"  public void fct2(final List<Object> a) {",
			"    this.fct2(a);",
			"    Object _object = new Object();",
			"    a.add(_object);",
			"  }",
			"  ",
			"  @SyntheticMember",
			"  public XXX() {",
			"    super();",
			"  }",
			"}",
			"");

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
		getCompileHelper().compile(SNIPSET4, (it) -> {
			assertEquals(EXPECTED4, it.getGeneratedCode("io.sarl.lang.tests.bug694.XXX"));
		});
	}

	private static final String SNIPSET5 = multilineString(
			"package io.sarl.lang.tests.bug694",
			"import java.util.List",
			"class XXX {",
			"  def fct01(a : int) : int {",
			"    fct02(a + 1) + 1",
			"  }",
			"  def fct02(a : int) : int {",
			"    fct03(a + 1) + 1",
			"  }",
			"  def fct03(a : int) : int {",
			"    fct01(a + 1) + 1",
			"  }",
			"  def fct11(a : List<Object>) : void {",
			"    fct12(a)",
			"  }",
			"  def fct12(a : List<Object>) : void {",
			"    fct13(a)",
			"  }",
			"  def fct13(a : List<Object>) : void {",
			"    fct11(a)",
			"    a.add(new Object)",
			"  }",
			"}");

	private final String EXPECTED5 = multilineString(
			"package io.sarl.lang.tests.bug694;",
			"",
			"import io.sarl.lang.annotation.SarlElementType;",
			"import io.sarl.lang.annotation.SarlSpecification;",
			"import io.sarl.lang.annotation.SyntheticMember;",
			"import java.util.List;",
			"import org.eclipse.xtext.xbase.lib.Pure;",
			"",
			"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
			"@SarlElementType(" + SarlPackage.SARL_CLASS + ")",
			"@SuppressWarnings(\"all\")",
			"public class XXX {",
			"  @Pure",
			"  public int fct01(final int a) {",
			"    int _fct02 = this.fct02((a + 1));",
			"    return (_fct02 + 1);",
			"  }",
			"  ",
			"  @Pure",
			"  public int fct02(final int a) {",
			"    int _fct03 = this.fct03((a + 1));",
			"    return (_fct03 + 1);",
			"  }",
			"  ",
			"  @Pure",
			"  public int fct03(final int a) {",
			"    int _fct01 = this.fct01((a + 1));",
			"    return (_fct01 + 1);",
			"  }",
			"  ",
			"  public void fct11(final List<Object> a) {",
			"    this.fct12(a);",
			"  }",
			"  ",
			"  public void fct12(final List<Object> a) {",
			"    this.fct13(a);",
			"  }",
			"  ",
			"  public void fct13(final List<Object> a) {",
			"    this.fct11(a);",
			"    Object _object = new Object();",
			"    a.add(_object);",
			"  }",
			"  ",
			"  @SyntheticMember",
			"  public XXX() {",
			"    super();",
			"  }",
			"}",
			"");

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
		getCompileHelper().compile(SNIPSET5, (it) -> {
			assertEquals(EXPECTED5, it.getGeneratedCode("io.sarl.lang.tests.bug694.XXX"));
		});
	}

}
