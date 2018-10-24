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

import com.google.inject.Inject;
import org.eclipse.xtext.xbase.lib.Functions.Function1;
import org.eclipse.xtext.xbase.testing.CompilationTestHelper;
import org.junit.Test;

import io.sarl.lang.SARLVersion;
import io.sarl.lang.sarl.SarlPackage;
import io.sarl.lang.sarl.SarlScript;
import io.sarl.tests.api.AbstractSarlTest;

/** Testing class for issue: Anonymous function not working.
 *
 * <p>https://github.com/sarl/sarl/issues/655
 *
 * @author $Author: sgalland$
 * @version $Name$ $Revision$ $Date$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 */
@SuppressWarnings("all")
public class Bug655 extends AbstractSarlTest {

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
			"import io.sarl.lang.core.BuiltinCapacitiesProvider;",
			"import io.sarl.lang.core.DynamicSkillProvider;",
			"import io.sarl.lang.core.Skill;",
			"import io.sarl.lang.tests.bug655.InitAgents;",
			"import io.sarl.lang.tests.bug655.MyCapacity;",
			"import io.sarl.lang.util.ClearableReference;",
			"import java.util.Timer;",
			"import java.util.TimerTask;",
			"import java.util.UUID;",
			"import javax.inject.Inject;",
			"import org.eclipse.xtext.xbase.lib.Extension;",
			"import org.eclipse.xtext.xbase.lib.Inline;",
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
			"        MyCapacity _$CAPACITY_USE$IO_SARL_LANG_TESTS_BUG655_MYCAPACITY$CALLER = X.this.$castSkill(MyCapacity.class, (X.this.$CAPACITY_USE$IO_SARL_LANG_TESTS_BUG655_MYCAPACITY == null || X.this.$CAPACITY_USE$IO_SARL_LANG_TESTS_BUG655_MYCAPACITY.get() == null) ? (X.this.$CAPACITY_USE$IO_SARL_LANG_TESTS_BUG655_MYCAPACITY = X.this.$getSkill(MyCapacity.class)) : X.this.$CAPACITY_USE$IO_SARL_LANG_TESTS_BUG655_MYCAPACITY);",
			"        InitAgents _initAgents = new InitAgents();",
			"        _$CAPACITY_USE$IO_SARL_LANG_TESTS_BUG655_MYCAPACITY$CALLER.emit(_initAgents);",
			"      }",
			"    }, 0, 500);",
			"  }",
			"  ",
			"  @Extension",
			"  @ImportedCapacityFeature(MyCapacity.class)",
			"  @SyntheticMember",
			"  private transient ClearableReference<Skill> $CAPACITY_USE$IO_SARL_LANG_TESTS_BUG655_MYCAPACITY;",
			"  ",
			"  @SyntheticMember",
			"  @Pure",
			"  @Inline(value = \"$castSkill(MyCapacity.class, ($0$CAPACITY_USE$IO_SARL_LANG_TESTS_BUG655_MYCAPACITY == null || $0$CAPACITY_USE$IO_SARL_LANG_TESTS_BUG655_MYCAPACITY.get() == null) ? ($0$CAPACITY_USE$IO_SARL_LANG_TESTS_BUG655_MYCAPACITY = $0$getSkill(MyCapacity.class)) : $0$CAPACITY_USE$IO_SARL_LANG_TESTS_BUG655_MYCAPACITY)\", imported = MyCapacity.class)",
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
			"  @Deprecated",
			"  @Inject",
			"  public X(final BuiltinCapacitiesProvider arg0, final UUID arg1, final UUID arg2) {",
			"    super(arg0, arg1, arg2);",
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
	public void parsing_01() throws Exception {
		SarlScript mas = file(SNIPSET1);
		final Validator validator = validate(mas);
		validator.assertNoErrors();
	}

	@Test
	public void compiling_01() throws Exception {
		getCompileHelper().compile(SNIPSET1, (it) -> {
			final String actual = it.getGeneratedCode("io.sarl.lang.tests.bug655.X");
			assertEquals(EXPECTED, actual);
		});
	}

}
