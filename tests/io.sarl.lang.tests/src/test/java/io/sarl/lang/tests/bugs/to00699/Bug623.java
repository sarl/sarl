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

package io.sarl.lang.tests.bugs.to00699;

import com.google.inject.Inject;
import org.eclipse.xtext.xbase.lib.Functions.Function1;
import org.eclipse.xtext.xbase.testing.CompilationTestHelper;
import org.junit.Test;

import io.sarl.lang.SARLVersion;
import io.sarl.lang.sarl.SarlPackage;
import io.sarl.lang.sarl.SarlScript;
import io.sarl.tests.api.AbstractSarlTest;

/** Testing class for issue: Generate real lambda.
 *
 * <p>https://github.com/sarl/sarl/issues/623
 *
 * @author $Author: sgalland$
 * @version $Name$ $Revision$ $Date$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 */
@SuppressWarnings("all")
public class Bug623 extends AbstractSarlTest {

	private static final String SNIPSET1 = multilineString(
			"package io.sarl.lang.tests.bug623",
			"class XXX {",
			"  def fct {",
			"    fct2 [ it + 4 ]",
			"  }",
			"  def fct2(a : (int) => int) : void {",
			"  }",
			"}");

	private static final String SNIPSET2 = multilineString(
			"package io.sarl.lang.tests.bug623",
			"class XXX {",
			"  def fct {",
			"    val vv = 4",
			"    fct2 [ it + vv ]",
			"  }",
			"  def fct2(a : (int) => int) : void {",
			"  }",
			"}");

	private static final String SNIPSET3 = multilineString(
			"package io.sarl.lang.tests.bug623",
			"class XXX {",
			"  def fct : int {",
			"    var vv = 4",
			"    vv += 1",
			"    fct2 [ it + vv ]",
			"    vv += 1",
			"    vv",
			"  }",
			"  def fct2(a : (int) => int) : void {",
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
				"package io.sarl.lang.tests.bug623;",
				"",
				"import io.sarl.lang.annotation.SarlElementType;",
				"import io.sarl.lang.annotation.SarlSpecification;",
				"import io.sarl.lang.annotation.SyntheticMember;",
				"import org.eclipse.xtext.xbase.lib.Functions.Function1;",
				"import org.eclipse.xtext.xbase.lib.Pure;",
				"",
				"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
				"@SarlElementType(" + SarlPackage.SARL_CLASS + ")",
				"@SuppressWarnings(\"all\")",
				"public class XXX {",
				"  @Pure",
				"  public void fct() {",
				"    final Function1<Integer, Integer> _function = (Integer it) -> {",
				"      return Integer.valueOf((((it) == null ? 0 : (it).intValue()) + 4));",
				"    };",
				"    this.fct2(_function);",
				"  }",
				"  ",
				"  @Pure",
				"  public void fct2(final Function1<? super Integer, ? extends Integer> a) {",
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
				"package io.sarl.lang.tests.bug623;",
				"",
				"import io.sarl.lang.annotation.SarlElementType;",
				"import io.sarl.lang.annotation.SarlSpecification;",
				"import io.sarl.lang.annotation.SyntheticMember;",
				"import org.eclipse.xtext.xbase.lib.Functions.Function1;",
				"import org.eclipse.xtext.xbase.lib.Pure;",
				"",
				"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
				"@SarlElementType(" + SarlPackage.SARL_CLASS + ")",
				"@SuppressWarnings(\"all\")",
				"public class XXX {",
				"  @Pure",
				"  public void fct() {",
				"    final int vv = 4;",
				"    final Function1<Integer, Integer> _function = (Integer it) -> {",
				"      return Integer.valueOf((((it) == null ? 0 : (it).intValue()) + vv));",
				"    };",
				"    this.fct2(_function);",
				"  }",
				"  ",
				"  @Pure",
				"  public void fct2(final Function1<? super Integer, ? extends Integer> a) {",
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
				"package io.sarl.lang.tests.bug623;",
				"",
				"import io.sarl.lang.annotation.SarlElementType;",
				"import io.sarl.lang.annotation.SarlSpecification;",
				"import io.sarl.lang.annotation.SyntheticMember;",
				"import org.eclipse.xtext.xbase.lib.Functions.Function1;",
				"import org.eclipse.xtext.xbase.lib.Pure;",
				"",
				"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
				"@SarlElementType(" + SarlPackage.SARL_CLASS + ")",
				"@SuppressWarnings(\"all\")",
				"public class XXX {",
				"  @Pure",
				"  public int fct() {",
				"    int _xblockexpression = (int) 0;",
				"    {",
				"      int vv = 4;",
				"      vv = (vv + 1);",
				"      final Function1<Integer, Integer> _function = (Integer it) -> {",
				"        return Integer.valueOf((((it) == null ? 0 : (it).intValue()) + vv));",
				"      };",
				"      this.fct2(_function);",
				"      vv = (vv + 1);",
				"      _xblockexpression = vv;",
				"    }",
				"    return _xblockexpression;",
				"  }",
				"  ",
				"  @Pure",
				"  public void fct2(final Function1<? super Integer, ? extends Integer> a) {",
				"  }",
				"  ",
				"  @SyntheticMember",
				"  public XXX() {",
				"    super();",
				"  }",
				"}",
				""));
	}

	// Only for checking if Java compiler support variables in lambdas.
	public void fct() {
		int vv = 4;
		final Function1<Integer, Integer> _function = (Integer it) -> {
			return Integer.valueOf(((it).intValue() + vv));
		};
		this.fct2(_function);
	}

	// Only for checking if Java compiler support variables in lambdas.
	public void fct2(final Function1<? super Integer, ? extends Integer> a) {
	}

}
