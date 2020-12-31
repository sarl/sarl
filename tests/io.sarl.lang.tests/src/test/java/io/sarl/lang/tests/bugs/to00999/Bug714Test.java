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

package io.sarl.lang.tests.bugs.to00999;

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

/** Testing class for issue: Generates try-with-resources.
 *
 * <p>https://github.com/sarl/sarl/issues/714
 *
 * @author $Author: sgalland$
 * @version $Name$ $Revision$ $Date$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 */
@DisplayName("Bug #714")
@SuppressWarnings("all")
@Tag("core")
public class Bug714Test extends AbstractSarlTest {

	private static final String SARL_CODE_01 = multilineString(
			"package io.sarl.lang.tests.bug714",
			"import java.io.FileReader",
			"import java.nio.CharBuffer",
			"class X {",
			"  def test {",
			"    try (val text = new FileReader('file.txt')) {",
			"      val buf = CharBuffer::allocate(1024)",
			"      text.read(buf)",
			"      buf.rewind",
			"      buf.toString",
			"    }",
			"  }",
			"}");

	private final String JAVA_CODE_01 = multilineString(
			"package io.sarl.lang.tests.bug714;",
			"",
			"import io.sarl.lang.annotation.SarlElementType;",
			"import io.sarl.lang.annotation.SarlSpecification;",
			"import io.sarl.lang.annotation.SyntheticMember;",
			"import java.io.FileReader;",
			"import java.nio.CharBuffer;",
			"import org.eclipse.xtext.xbase.lib.Exceptions;",
			"import org.eclipse.xtext.xbase.lib.Functions.Function0;",
			"",
			"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
			"@SarlElementType(" + SarlPackage.SARL_CLASS + ")",
			"@SuppressWarnings(\"all\")",
			"public class X {",
			"  public String test() {",
			"    try {",
			"      String _xtrycatchfinallyexpression = null;",
			"      try (final FileReader text = new Function0<FileReader>() {",
			"        @Override",
			"        public FileReader apply() {",
			"          try {",
			"            return new FileReader(\"file.txt\");",
			"          } catch (Throwable _e) {",
			"            throw Exceptions.sneakyThrow(_e);",
			"          }",
			"        }",
			"      }.apply()) {",
			"        String _xblockexpression = null;",
			"        {",
			"          final CharBuffer buf = CharBuffer.allocate(1024);",
			"          text.read(buf);",
			"          buf.rewind();",
			"          _xblockexpression = buf.toString();",
			"        }",
			"        _xtrycatchfinallyexpression = _xblockexpression;",
			"      }",
			"      return _xtrycatchfinallyexpression;",
			"    } catch (Throwable _e) {",
			"      throw Exceptions.sneakyThrow(_e);",
			"    }",
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
		SarlScript mas = file(getParseHelper(), SARL_CODE_01);
		final Validator validator = validate(getValidationHelper(), getInjector(), mas);
		validator.assertNoErrors();
	}

	@Test
	@Tag("compileToJava")
	public void compiling_01() throws Exception {
		getCompileHelper().compile(SARL_CODE_01, (it) -> {
			final String actual = it.getGeneratedCode("io.sarl.lang.tests.bug714.X");
			assertEquals(JAVA_CODE_01, actual);
		});
	}

	private static final String SARL_CODE_02 = multilineString(
			"package io.sarl.lang.tests.bug714",
			"import java.io.FileReader",
			"import java.nio.CharBuffer",
			"class X {",
			"  def test {",
			"    try (var text = new FileReader('file.txt')) {",
			"      val buf = CharBuffer::allocate(1024)",
			"      text.read(buf)",
			"      buf.rewind",
			"      buf.toString",
			"    }",
			"  }",
			"}");

	private final String JAVA_CODE_02 = multilineString(
			"package io.sarl.lang.tests.bug714;",
			"",
			"import io.sarl.lang.annotation.SarlElementType;",
			"import io.sarl.lang.annotation.SarlSpecification;",
			"import io.sarl.lang.annotation.SyntheticMember;",
			"import java.io.FileReader;",
			"import java.nio.CharBuffer;",
			"import org.eclipse.xtext.xbase.lib.Exceptions;",
			"import org.eclipse.xtext.xbase.lib.Functions.Function0;",
			"",
			"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
			"@SarlElementType(" + SarlPackage.SARL_CLASS + ")",
			"@SuppressWarnings(\"all\")",
			"public class X {",
			"  public String test() {",
			"    try {",
			"      String _xtrycatchfinallyexpression = null;",
			"      try (FileReader text = new Function0<FileReader>() {",
			"        @Override",
			"        public FileReader apply() {",
			"          try {",
			"            return new FileReader(\"file.txt\");",
			"          } catch (Throwable _e) {",
			"            throw Exceptions.sneakyThrow(_e);",
			"          }",
			"        }",
			"      }.apply()) {",
			"        String _xblockexpression = null;",
			"        {",
			"          final CharBuffer buf = CharBuffer.allocate(1024);",
			"          text.read(buf);",
			"          buf.rewind();",
			"          _xblockexpression = buf.toString();",
			"        }",
			"        _xtrycatchfinallyexpression = _xblockexpression;",
			"      }",
			"      return _xtrycatchfinallyexpression;",
			"    } catch (Throwable _e) {",
			"      throw Exceptions.sneakyThrow(_e);",
			"    }",
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
	public void parsing_02() throws Exception {
		SarlScript mas = file(getParseHelper(), SARL_CODE_02);
		final Validator validator = validate(getValidationHelper(), getInjector(), mas);
		validator.assertNoErrors();
	}

	@Test
	@Tag("compileToJava")
	public void compiling_02() throws Exception {
		getCompileHelper().compile(SARL_CODE_02, (it) -> {
			final String actual = it.getGeneratedCode("io.sarl.lang.tests.bug714.X");
			assertEquals(JAVA_CODE_02, actual);
		});
	}

}
