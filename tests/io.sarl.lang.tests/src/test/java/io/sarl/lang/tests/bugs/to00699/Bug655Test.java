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

/** Testing class for issue: Anonymous function not working.
 *
 * <p>https://github.com/sarl/sarl/issues/655
 *
 * @author $Author: sgalland$
 * @version $Name$ $Revision$ $Date$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 */
@DisplayName("Bug #655")
@SuppressWarnings("all")
@Tag("core")
public class Bug655Test extends AbstractSarlTest {

	private static final String SNIPSET1 = multilineString(
			"package io.sarl.lang.tests.bug655",
			"import io.sarl.lang.core.Event",
			"import java.util.Timer",
			"import java.util.TimerTask",
			"event InitAgents",
			"capacity MyCapacity {",
			"	def emit(e : Event)",
			"}",
			"agent X {",
			"	uses MyCapacity",
			"	val timer = new Timer",
			"	def initialize {",
			"		timer.schedule(new TimerTask() {",
			"			override run {",
			"				emit(new InitAgents)",
			"			}",
			"		}, 0, 500)",
			"	}",
			"}");

	private final String EXPECTED = multilineString(
			"package io.sarl.lang.tests.bug655;",
			"",
			"import io.sarl.lang.annotation.ImportedCapacityFeature;",
			"import io.sarl.lang.annotation.SarlElementType;",
			"import io.sarl.lang.annotation.SarlSpecification;",
			"import io.sarl.lang.annotation.SyntheticMember;",
			"import io.sarl.lang.core.Agent;",
			"import io.sarl.lang.core.AtomicSkillReference;",
			"import io.sarl.lang.core.DynamicSkillProvider;",
			"import java.util.Timer;",
			"import java.util.TimerTask;",
			"import java.util.UUID;",
			"import javax.inject.Inject;",
			"import org.eclipse.xtext.xbase.lib.Extension;",
			"import org.eclipse.xtext.xbase.lib.Pure;",
			"",
			"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
			"@SarlElementType(" + SarlPackage.SARL_AGENT + ")",
			"@SuppressWarnings(\"all\")",
			"public class X extends Agent {",
			"  private final Timer timer = new Timer();",
			"  ",
			"  protected void initialize() {",
			"    this.timer.schedule(new TimerTask() {",
			"      @Override",
			"      public void run() {",
			"        MyCapacity _$CAPACITY_USE$IO_SARL_LANG_TESTS_BUG655_MYCAPACITY$CALLER = X.this.$CAPACITY_USE$IO_SARL_LANG_TESTS_BUG655_MYCAPACITY$CALLER();",
			"        InitAgents _initAgents = new InitAgents();",
			"        _$CAPACITY_USE$IO_SARL_LANG_TESTS_BUG655_MYCAPACITY$CALLER.emit(_initAgents);",
			"      }",
			"    }, 0, 500);",
			"  }",
			"  ",
			"  @Extension",
			"  @ImportedCapacityFeature(MyCapacity.class)",
			"  @SyntheticMember",
			"  private transient AtomicSkillReference $CAPACITY_USE$IO_SARL_LANG_TESTS_BUG655_MYCAPACITY;",
			"  ",
			"  @SyntheticMember",
			"  @Pure",
			"  private MyCapacity $CAPACITY_USE$IO_SARL_LANG_TESTS_BUG655_MYCAPACITY$CALLER() {",
			"    if (this.$CAPACITY_USE$IO_SARL_LANG_TESTS_BUG655_MYCAPACITY == null || this.$CAPACITY_USE$IO_SARL_LANG_TESTS_BUG655_MYCAPACITY.get() == null) {",
			"      this.$CAPACITY_USE$IO_SARL_LANG_TESTS_BUG655_MYCAPACITY = $getSkill(MyCapacity.class);",
			"    }",
			"    return $castSkill(MyCapacity.class, this.$CAPACITY_USE$IO_SARL_LANG_TESTS_BUG655_MYCAPACITY);",
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
			"  public X(final UUID arg0, final UUID arg1) {",
			"    super(arg0, arg1);",
			"  }",
			"  ", 
			"  @SyntheticMember", 
			"  @Inject", 
			"  public X(final UUID arg0, final UUID arg1, final DynamicSkillProvider arg2) {", 
			"    super(arg0, arg1, arg2);", 
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
			final String actual = it.getGeneratedCode("io.sarl.lang.tests.bug655.X");
			assertEquals(EXPECTED, actual);
		});
	}

}
