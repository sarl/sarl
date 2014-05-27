/**
 * Copyright 2014 Sebastian RODRIGUEZ, Nicolas GAUD, Stéphane GALLAND.
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
package io.sarl.lang.tests.compilation;

import com.google.inject.Inject;
import io.sarl.lang.SARLInjectorProvider;
import org.eclipse.xtend2.lib.StringConcatenation;
import org.eclipse.xtext.junit4.InjectWith;
import org.eclipse.xtext.junit4.XtextRunner;
import org.eclipse.xtext.xbase.compiler.CompilationTestHelper;
import org.eclipse.xtext.xbase.lib.Exceptions;
import org.eclipse.xtext.xbase.lib.Extension;
import org.junit.Test;
import org.junit.runner.RunWith;

/**
 * @author $Author: sgalland$
 * @version $Name$ $Revision$ $Date$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 */
@RunWith(XtextRunner.class)
@InjectWith(SARLInjectorProvider.class)
@SuppressWarnings("all")
public class VarArgsCompilerTest {
  @Inject
  @Extension
  private CompilationTestHelper _compilationTestHelper;
  
  @Test
  public void inAgentAction_singleParam() {
    try {
      StringConcatenation _builder = new StringConcatenation();
      _builder.append("agent A1 {");
      _builder.newLine();
      _builder.append("\t");
      _builder.append("def myaction(arg : int*) {");
      _builder.newLine();
      _builder.append("\t\t");
      _builder.append("System.out.println(arg)");
      _builder.newLine();
      _builder.append("\t");
      _builder.append("}");
      _builder.newLine();
      _builder.append("}");
      _builder.newLine();
      StringConcatenation _builder_1 = new StringConcatenation();
      _builder_1.append("import io.sarl.lang.annotation.Generated;");
      _builder_1.newLine();
      _builder_1.append("import io.sarl.lang.core.Agent;");
      _builder_1.newLine();
      _builder_1.newLine();
      _builder_1.append("@SuppressWarnings(\"all\")");
      _builder_1.newLine();
      _builder_1.append("public class A1 extends Agent {");
      _builder_1.newLine();
      _builder_1.append("  ");
      _builder_1.append("/**");
      _builder_1.newLine();
      _builder_1.append("   ");
      _builder_1.append("* Construct an agent.");
      _builder_1.newLine();
      _builder_1.append("   ");
      _builder_1.append("* @param parentID - identifier of the parent. It is the identifer");
      _builder_1.newLine();
      _builder_1.append("   ");
      _builder_1.append("* of the parent agent and the enclosing contect, at the same time.");
      _builder_1.newLine();
      _builder_1.append("   ");
      _builder_1.append("* ");
      _builder_1.newLine();
      _builder_1.append("   ");
      _builder_1.append("*/");
      _builder_1.newLine();
      _builder_1.append("  ");
      _builder_1.append("@Generated");
      _builder_1.newLine();
      _builder_1.append("  ");
      _builder_1.append("public A1(final java.util.UUID parentID) {");
      _builder_1.newLine();
      _builder_1.append("    ");
      _builder_1.append("super(parentID);");
      _builder_1.newLine();
      _builder_1.append("  ");
      _builder_1.append("}");
      _builder_1.newLine();
      _builder_1.append("  ");
      _builder_1.newLine();
      _builder_1.append("  ");
      _builder_1.append("public void myaction(final int... arg) {");
      _builder_1.newLine();
      _builder_1.append("    ");
      _builder_1.append("System.out.println(arg);");
      _builder_1.newLine();
      _builder_1.append("  ");
      _builder_1.append("}");
      _builder_1.newLine();
      _builder_1.append("}");
      _builder_1.newLine();
      this._compilationTestHelper.assertCompilesTo(_builder, _builder_1);
    } catch (Throwable _e) {
      throw Exceptions.sneakyThrow(_e);
    }
  }
  
  @Test
  public void inAgentAction() {
    try {
      StringConcatenation _builder = new StringConcatenation();
      _builder.append("agent A1 {");
      _builder.newLine();
      _builder.append("\t");
      _builder.append("def myaction(arg1 : char, arg2 : boolean, arg3 : int*) {");
      _builder.newLine();
      _builder.append("\t\t");
      _builder.append("System.out.println(arg3)");
      _builder.newLine();
      _builder.append("\t");
      _builder.append("}");
      _builder.newLine();
      _builder.append("}");
      _builder.newLine();
      StringConcatenation _builder_1 = new StringConcatenation();
      _builder_1.append("import io.sarl.lang.annotation.Generated;");
      _builder_1.newLine();
      _builder_1.append("import io.sarl.lang.core.Agent;");
      _builder_1.newLine();
      _builder_1.newLine();
      _builder_1.append("@SuppressWarnings(\"all\")");
      _builder_1.newLine();
      _builder_1.append("public class A1 extends Agent {");
      _builder_1.newLine();
      _builder_1.append("  ");
      _builder_1.append("/**");
      _builder_1.newLine();
      _builder_1.append("   ");
      _builder_1.append("* Construct an agent.");
      _builder_1.newLine();
      _builder_1.append("   ");
      _builder_1.append("* @param parentID - identifier of the parent. It is the identifer");
      _builder_1.newLine();
      _builder_1.append("   ");
      _builder_1.append("* of the parent agent and the enclosing contect, at the same time.");
      _builder_1.newLine();
      _builder_1.append("   ");
      _builder_1.append("* ");
      _builder_1.newLine();
      _builder_1.append("   ");
      _builder_1.append("*/");
      _builder_1.newLine();
      _builder_1.append("  ");
      _builder_1.append("@Generated");
      _builder_1.newLine();
      _builder_1.append("  ");
      _builder_1.append("public A1(final java.util.UUID parentID) {");
      _builder_1.newLine();
      _builder_1.append("    ");
      _builder_1.append("super(parentID);");
      _builder_1.newLine();
      _builder_1.append("  ");
      _builder_1.append("}");
      _builder_1.newLine();
      _builder_1.append("  ");
      _builder_1.newLine();
      _builder_1.append("  ");
      _builder_1.append("public void myaction(final char arg1, final boolean arg2, final int... arg3) {");
      _builder_1.newLine();
      _builder_1.append("    ");
      _builder_1.append("System.out.println(arg3);");
      _builder_1.newLine();
      _builder_1.append("  ");
      _builder_1.append("}");
      _builder_1.newLine();
      _builder_1.append("}");
      _builder_1.newLine();
      this._compilationTestHelper.assertCompilesTo(_builder, _builder_1);
    } catch (Throwable _e) {
      throw Exceptions.sneakyThrow(_e);
    }
  }
  
  @Test
  public void inBehaviorAction_singleParam() {
    try {
      StringConcatenation _builder = new StringConcatenation();
      _builder.append("behavior B1 {");
      _builder.newLine();
      _builder.append("\t");
      _builder.append("def myaction(arg : int*) {");
      _builder.newLine();
      _builder.append("\t\t");
      _builder.append("System.out.println(arg)");
      _builder.newLine();
      _builder.append("\t");
      _builder.append("}");
      _builder.newLine();
      _builder.append("}");
      _builder.newLine();
      StringConcatenation _builder_1 = new StringConcatenation();
      _builder_1.append("import io.sarl.lang.core.Behavior;");
      _builder_1.newLine();
      _builder_1.newLine();
      _builder_1.append("@SuppressWarnings(\"all\")");
      _builder_1.newLine();
      _builder_1.append("public class B1 extends Behavior {");
      _builder_1.newLine();
      _builder_1.append("  ");
      _builder_1.append("public void myaction(final int... arg) {");
      _builder_1.newLine();
      _builder_1.append("    ");
      _builder_1.append("System.out.println(arg);");
      _builder_1.newLine();
      _builder_1.append("  ");
      _builder_1.append("}");
      _builder_1.newLine();
      _builder_1.append("  ");
      _builder_1.newLine();
      _builder_1.append("  ");
      _builder_1.append("/**");
      _builder_1.newLine();
      _builder_1.append("   ");
      _builder_1.append("* Construct a behavior.");
      _builder_1.newLine();
      _builder_1.append("   ");
      _builder_1.append("* @param owner - reference to the agent that is owning this behavior.");
      _builder_1.newLine();
      _builder_1.append("   ");
      _builder_1.append("* ");
      _builder_1.newLine();
      _builder_1.append("   ");
      _builder_1.append("*/");
      _builder_1.newLine();
      _builder_1.append("  ");
      _builder_1.append("public B1(final io.sarl.lang.core.Agent owner) {");
      _builder_1.newLine();
      _builder_1.append("    ");
      _builder_1.append("super(owner);");
      _builder_1.newLine();
      _builder_1.append("  ");
      _builder_1.append("}");
      _builder_1.newLine();
      _builder_1.append("}");
      _builder_1.newLine();
      this._compilationTestHelper.assertCompilesTo(_builder, _builder_1);
    } catch (Throwable _e) {
      throw Exceptions.sneakyThrow(_e);
    }
  }
  
  @Test
  public void inBehaviorAction() {
    try {
      StringConcatenation _builder = new StringConcatenation();
      _builder.append("behavior B1 {");
      _builder.newLine();
      _builder.append("\t");
      _builder.append("def myaction(arg1 : char, arg2 : boolean, arg3 : int*) {");
      _builder.newLine();
      _builder.append("\t\t");
      _builder.append("System.out.println(arg3)");
      _builder.newLine();
      _builder.append("\t");
      _builder.append("}");
      _builder.newLine();
      _builder.append("}");
      _builder.newLine();
      StringConcatenation _builder_1 = new StringConcatenation();
      _builder_1.append("import io.sarl.lang.core.Behavior;");
      _builder_1.newLine();
      _builder_1.newLine();
      _builder_1.append("@SuppressWarnings(\"all\")");
      _builder_1.newLine();
      _builder_1.append("public class B1 extends Behavior {");
      _builder_1.newLine();
      _builder_1.append("  ");
      _builder_1.append("public void myaction(final char arg1, final boolean arg2, final int... arg3) {");
      _builder_1.newLine();
      _builder_1.append("    ");
      _builder_1.append("System.out.println(arg3);");
      _builder_1.newLine();
      _builder_1.append("  ");
      _builder_1.append("}");
      _builder_1.newLine();
      _builder_1.append("  ");
      _builder_1.newLine();
      _builder_1.append("  ");
      _builder_1.append("/**");
      _builder_1.newLine();
      _builder_1.append("   ");
      _builder_1.append("* Construct a behavior.");
      _builder_1.newLine();
      _builder_1.append("   ");
      _builder_1.append("* @param owner - reference to the agent that is owning this behavior.");
      _builder_1.newLine();
      _builder_1.append("   ");
      _builder_1.append("* ");
      _builder_1.newLine();
      _builder_1.append("   ");
      _builder_1.append("*/");
      _builder_1.newLine();
      _builder_1.append("  ");
      _builder_1.append("public B1(final io.sarl.lang.core.Agent owner) {");
      _builder_1.newLine();
      _builder_1.append("    ");
      _builder_1.append("super(owner);");
      _builder_1.newLine();
      _builder_1.append("  ");
      _builder_1.append("}");
      _builder_1.newLine();
      _builder_1.append("}");
      _builder_1.newLine();
      this._compilationTestHelper.assertCompilesTo(_builder, _builder_1);
    } catch (Throwable _e) {
      throw Exceptions.sneakyThrow(_e);
    }
  }
  
  @Test
  public void inBehaviorConstructor_singleParam() {
    try {
      StringConcatenation _builder = new StringConcatenation();
      _builder.append("behavior B1 {");
      _builder.newLine();
      _builder.append("\t");
      _builder.append("new(arg : int*) {");
      _builder.newLine();
      _builder.append("\t\t");
      _builder.append("System.out.println(arg)");
      _builder.newLine();
      _builder.append("\t");
      _builder.append("}");
      _builder.newLine();
      _builder.append("}");
      _builder.newLine();
      StringConcatenation _builder_1 = new StringConcatenation();
      _builder_1.append("import io.sarl.lang.core.Behavior;");
      _builder_1.newLine();
      _builder_1.newLine();
      _builder_1.append("@SuppressWarnings(\"all\")");
      _builder_1.newLine();
      _builder_1.append("public class B1 extends Behavior {");
      _builder_1.newLine();
      _builder_1.append("  ");
      _builder_1.append("public B1(final int... arg) {");
      _builder_1.newLine();
      _builder_1.append("    ");
      _builder_1.append("System.out.println(arg);");
      _builder_1.newLine();
      _builder_1.append("  ");
      _builder_1.append("}");
      _builder_1.newLine();
      _builder_1.append("}");
      _builder_1.newLine();
      this._compilationTestHelper.assertCompilesTo(_builder, _builder_1);
    } catch (Throwable _e) {
      throw Exceptions.sneakyThrow(_e);
    }
  }
  
  @Test
  public void inBehaviorConstructor() {
    try {
      StringConcatenation _builder = new StringConcatenation();
      _builder.append("behavior B1 {");
      _builder.newLine();
      _builder.append("\t");
      _builder.append("new(arg1 : char, arg2 : boolean, arg3 : int*) {");
      _builder.newLine();
      _builder.append("\t\t");
      _builder.append("System.out.println(arg3)");
      _builder.newLine();
      _builder.append("\t");
      _builder.append("}");
      _builder.newLine();
      _builder.append("}");
      _builder.newLine();
      StringConcatenation _builder_1 = new StringConcatenation();
      _builder_1.append("import io.sarl.lang.core.Behavior;");
      _builder_1.newLine();
      _builder_1.newLine();
      _builder_1.append("@SuppressWarnings(\"all\")");
      _builder_1.newLine();
      _builder_1.append("public class B1 extends Behavior {");
      _builder_1.newLine();
      _builder_1.append("  ");
      _builder_1.append("public B1(final char arg1, final boolean arg2, final int... arg3) {");
      _builder_1.newLine();
      _builder_1.append("    ");
      _builder_1.append("System.out.println(arg3);");
      _builder_1.newLine();
      _builder_1.append("  ");
      _builder_1.append("}");
      _builder_1.newLine();
      _builder_1.append("}");
      _builder_1.newLine();
      this._compilationTestHelper.assertCompilesTo(_builder, _builder_1);
    } catch (Throwable _e) {
      throw Exceptions.sneakyThrow(_e);
    }
  }
  
  @Test
  public void inCapacityAction_singleParam() {
    try {
      StringConcatenation _builder = new StringConcatenation();
      _builder.append("capacity C1 {");
      _builder.newLine();
      _builder.append("\t");
      _builder.append("def myaction(arg : int*)");
      _builder.newLine();
      _builder.append("}");
      _builder.newLine();
      StringConcatenation _builder_1 = new StringConcatenation();
      _builder_1.append("import io.sarl.lang.core.Capacity;");
      _builder_1.newLine();
      _builder_1.newLine();
      _builder_1.append("@SuppressWarnings(\"all\")");
      _builder_1.newLine();
      _builder_1.append("public interface C1 extends Capacity {");
      _builder_1.newLine();
      _builder_1.append("  ");
      _builder_1.append("public abstract void myaction(final int... arg);");
      _builder_1.newLine();
      _builder_1.append("}");
      _builder_1.newLine();
      this._compilationTestHelper.assertCompilesTo(_builder, _builder_1);
    } catch (Throwable _e) {
      throw Exceptions.sneakyThrow(_e);
    }
  }
  
  @Test
  public void inCapacityAction() {
    try {
      StringConcatenation _builder = new StringConcatenation();
      _builder.append("capacity C1 {");
      _builder.newLine();
      _builder.append("\t");
      _builder.append("def myaction(arg1 : char, arg2 : boolean, arg3 : int*)");
      _builder.newLine();
      _builder.append("}");
      _builder.newLine();
      StringConcatenation _builder_1 = new StringConcatenation();
      _builder_1.append("import io.sarl.lang.core.Capacity;");
      _builder_1.newLine();
      _builder_1.newLine();
      _builder_1.append("@SuppressWarnings(\"all\")");
      _builder_1.newLine();
      _builder_1.append("public interface C1 extends Capacity {");
      _builder_1.newLine();
      _builder_1.append("  ");
      _builder_1.append("public abstract void myaction(final char arg1, final boolean arg2, final int... arg3);");
      _builder_1.newLine();
      _builder_1.append("}");
      _builder_1.newLine();
      this._compilationTestHelper.assertCompilesTo(_builder, _builder_1);
    } catch (Throwable _e) {
      throw Exceptions.sneakyThrow(_e);
    }
  }
  
  @Test
  public void inEventAction_singleParam() {
    try {
      StringConcatenation _builder = new StringConcatenation();
      _builder.append("event E1 {");
      _builder.newLine();
      _builder.append("\t");
      _builder.append("new(arg : int*) {");
      _builder.newLine();
      _builder.append("\t\t");
      _builder.append("System.out.println(arg)");
      _builder.newLine();
      _builder.append("\t");
      _builder.append("}");
      _builder.newLine();
      _builder.append("}");
      _builder.newLine();
      StringConcatenation _builder_1 = new StringConcatenation();
      _builder_1.append("import io.sarl.lang.annotation.Generated;");
      _builder_1.newLine();
      _builder_1.append("import io.sarl.lang.core.Event;");
      _builder_1.newLine();
      _builder_1.newLine();
      _builder_1.append("@SuppressWarnings(\"all\")");
      _builder_1.newLine();
      _builder_1.append("public class E1 extends Event {");
      _builder_1.newLine();
      _builder_1.append("  ");
      _builder_1.append("public E1(final int... arg) {");
      _builder_1.newLine();
      _builder_1.append("    ");
      _builder_1.append("System.out.println(arg);");
      _builder_1.newLine();
      _builder_1.append("  ");
      _builder_1.append("}");
      _builder_1.newLine();
      _builder_1.append("  ");
      _builder_1.newLine();
      _builder_1.append("  ");
      _builder_1.append("@Generated");
      _builder_1.newLine();
      _builder_1.append("  ");
      _builder_1.append("private final static long serialVersionUID = 588370681L;");
      _builder_1.newLine();
      _builder_1.append("}");
      _builder_1.newLine();
      this._compilationTestHelper.assertCompilesTo(_builder, _builder_1);
    } catch (Throwable _e) {
      throw Exceptions.sneakyThrow(_e);
    }
  }
  
  @Test
  public void inEventConstructor() {
    try {
      StringConcatenation _builder = new StringConcatenation();
      _builder.append("event E1 {");
      _builder.newLine();
      _builder.append("\t");
      _builder.append("new(arg1 : char, arg2 : boolean, arg3 : int*) {");
      _builder.newLine();
      _builder.append("\t\t");
      _builder.append("System.out.println(arg3)");
      _builder.newLine();
      _builder.append("\t");
      _builder.append("}");
      _builder.newLine();
      _builder.append("}");
      _builder.newLine();
      StringConcatenation _builder_1 = new StringConcatenation();
      _builder_1.append("import io.sarl.lang.annotation.Generated;");
      _builder_1.newLine();
      _builder_1.append("import io.sarl.lang.core.Event;");
      _builder_1.newLine();
      _builder_1.newLine();
      _builder_1.append("@SuppressWarnings(\"all\")");
      _builder_1.newLine();
      _builder_1.append("public class E1 extends Event {");
      _builder_1.newLine();
      _builder_1.append("  ");
      _builder_1.append("public E1(final char arg1, final boolean arg2, final int... arg3) {");
      _builder_1.newLine();
      _builder_1.append("    ");
      _builder_1.append("System.out.println(arg3);");
      _builder_1.newLine();
      _builder_1.append("  ");
      _builder_1.append("}");
      _builder_1.newLine();
      _builder_1.append("  ");
      _builder_1.newLine();
      _builder_1.append("  ");
      _builder_1.append("@Generated");
      _builder_1.newLine();
      _builder_1.append("  ");
      _builder_1.append("private final static long serialVersionUID = 588370681L;");
      _builder_1.newLine();
      _builder_1.append("}");
      _builder_1.newLine();
      this._compilationTestHelper.assertCompilesTo(_builder, _builder_1);
    } catch (Throwable _e) {
      throw Exceptions.sneakyThrow(_e);
    }
  }
  
  @Test
  public void inSkillAction_singleParam() {
    try {
      StringConcatenation _builder = new StringConcatenation();
      _builder.append("capacity C1 {}");
      _builder.newLine();
      _builder.append("skill S1 implements C1 {");
      _builder.newLine();
      _builder.append("\t");
      _builder.append("def myaction(arg : int*) {");
      _builder.newLine();
      _builder.append("\t\t");
      _builder.append("System.out.println(arg)");
      _builder.newLine();
      _builder.append("\t");
      _builder.append("}");
      _builder.newLine();
      _builder.append("}");
      _builder.newLine();
      StringConcatenation _builder_1 = new StringConcatenation();
      _builder_1.append("MULTIPLE FILES WERE GENERATED");
      _builder_1.newLine();
      _builder_1.newLine();
      _builder_1.append("File 1 : C1.java");
      _builder_1.newLine();
      _builder_1.newLine();
      _builder_1.append("import io.sarl.lang.core.Capacity;");
      _builder_1.newLine();
      _builder_1.newLine();
      _builder_1.append("@SuppressWarnings(\"all\")");
      _builder_1.newLine();
      _builder_1.append("public interface C1 extends Capacity {");
      _builder_1.newLine();
      _builder_1.append("}");
      _builder_1.newLine();
      _builder_1.newLine();
      _builder_1.append("File 2 : S1.java");
      _builder_1.newLine();
      _builder_1.newLine();
      _builder_1.append("import io.sarl.lang.core.Skill;");
      _builder_1.newLine();
      _builder_1.newLine();
      _builder_1.append("@SuppressWarnings(\"all\")");
      _builder_1.newLine();
      _builder_1.append("public class S1 extends Skill implements C1 {");
      _builder_1.newLine();
      _builder_1.append("  ");
      _builder_1.append("public void myaction(final int... arg) {");
      _builder_1.newLine();
      _builder_1.append("    ");
      _builder_1.append("System.out.println(arg);");
      _builder_1.newLine();
      _builder_1.append("  ");
      _builder_1.append("}");
      _builder_1.newLine();
      _builder_1.append("  ");
      _builder_1.newLine();
      _builder_1.append("  ");
      _builder_1.append("/**");
      _builder_1.newLine();
      _builder_1.append("   ");
      _builder_1.append("* Construct a skill.");
      _builder_1.newLine();
      _builder_1.append("   ");
      _builder_1.append("* @param owner - agent that is owning this skill. ");
      _builder_1.newLine();
      _builder_1.append("   ");
      _builder_1.append("* ");
      _builder_1.newLine();
      _builder_1.append("   ");
      _builder_1.append("*/");
      _builder_1.newLine();
      _builder_1.append("  ");
      _builder_1.append("public S1(final io.sarl.lang.core.Agent owner) {");
      _builder_1.newLine();
      _builder_1.append("    ");
      _builder_1.append("super(owner);");
      _builder_1.newLine();
      _builder_1.append("  ");
      _builder_1.append("}");
      _builder_1.newLine();
      _builder_1.append("  ");
      _builder_1.newLine();
      _builder_1.append("  ");
      _builder_1.append("/**");
      _builder_1.newLine();
      _builder_1.append("   ");
      _builder_1.append("* Construct a skill. The owning agent is unknown. ");
      _builder_1.newLine();
      _builder_1.append("   ");
      _builder_1.append("* ");
      _builder_1.newLine();
      _builder_1.append("   ");
      _builder_1.append("*/");
      _builder_1.newLine();
      _builder_1.append("  ");
      _builder_1.append("public S1() {");
      _builder_1.newLine();
      _builder_1.append("    ");
      _builder_1.append("super();");
      _builder_1.newLine();
      _builder_1.append("  ");
      _builder_1.append("}");
      _builder_1.newLine();
      _builder_1.append("}");
      _builder_1.newLine();
      _builder_1.newLine();
      this._compilationTestHelper.assertCompilesTo(_builder, _builder_1);
    } catch (Throwable _e) {
      throw Exceptions.sneakyThrow(_e);
    }
  }
  
  @Test
  public void inSkillAction() {
    try {
      StringConcatenation _builder = new StringConcatenation();
      _builder.append("capacity C1 {}");
      _builder.newLine();
      _builder.append("skill S1 implements C1 {");
      _builder.newLine();
      _builder.append("\t");
      _builder.append("def myaction(arg1 : char, arg2 : boolean, arg3 : int*) {");
      _builder.newLine();
      _builder.append("\t\t");
      _builder.append("System.out.println(arg3)");
      _builder.newLine();
      _builder.append("\t");
      _builder.append("}");
      _builder.newLine();
      _builder.append("}");
      _builder.newLine();
      StringConcatenation _builder_1 = new StringConcatenation();
      _builder_1.append("MULTIPLE FILES WERE GENERATED");
      _builder_1.newLine();
      _builder_1.newLine();
      _builder_1.append("File 1 : C1.java");
      _builder_1.newLine();
      _builder_1.newLine();
      _builder_1.append("import io.sarl.lang.core.Capacity;");
      _builder_1.newLine();
      _builder_1.newLine();
      _builder_1.append("@SuppressWarnings(\"all\")");
      _builder_1.newLine();
      _builder_1.append("public interface C1 extends Capacity {");
      _builder_1.newLine();
      _builder_1.append("}");
      _builder_1.newLine();
      _builder_1.newLine();
      _builder_1.append("File 2 : S1.java");
      _builder_1.newLine();
      _builder_1.newLine();
      _builder_1.append("import io.sarl.lang.core.Skill;");
      _builder_1.newLine();
      _builder_1.newLine();
      _builder_1.append("@SuppressWarnings(\"all\")");
      _builder_1.newLine();
      _builder_1.append("public class S1 extends Skill implements C1 {");
      _builder_1.newLine();
      _builder_1.append("  ");
      _builder_1.append("public void myaction(final char arg1, final boolean arg2, final int... arg3) {");
      _builder_1.newLine();
      _builder_1.append("    ");
      _builder_1.append("System.out.println(arg3);");
      _builder_1.newLine();
      _builder_1.append("  ");
      _builder_1.append("}");
      _builder_1.newLine();
      _builder_1.append("  ");
      _builder_1.newLine();
      _builder_1.append("  ");
      _builder_1.append("/**");
      _builder_1.newLine();
      _builder_1.append("   ");
      _builder_1.append("* Construct a skill.");
      _builder_1.newLine();
      _builder_1.append("   ");
      _builder_1.append("* @param owner - agent that is owning this skill. ");
      _builder_1.newLine();
      _builder_1.append("   ");
      _builder_1.append("* ");
      _builder_1.newLine();
      _builder_1.append("   ");
      _builder_1.append("*/");
      _builder_1.newLine();
      _builder_1.append("  ");
      _builder_1.append("public S1(final io.sarl.lang.core.Agent owner) {");
      _builder_1.newLine();
      _builder_1.append("    ");
      _builder_1.append("super(owner);");
      _builder_1.newLine();
      _builder_1.append("  ");
      _builder_1.append("}");
      _builder_1.newLine();
      _builder_1.append("  ");
      _builder_1.newLine();
      _builder_1.append("  ");
      _builder_1.append("/**");
      _builder_1.newLine();
      _builder_1.append("   ");
      _builder_1.append("* Construct a skill. The owning agent is unknown. ");
      _builder_1.newLine();
      _builder_1.append("   ");
      _builder_1.append("* ");
      _builder_1.newLine();
      _builder_1.append("   ");
      _builder_1.append("*/");
      _builder_1.newLine();
      _builder_1.append("  ");
      _builder_1.append("public S1() {");
      _builder_1.newLine();
      _builder_1.append("    ");
      _builder_1.append("super();");
      _builder_1.newLine();
      _builder_1.append("  ");
      _builder_1.append("}");
      _builder_1.newLine();
      _builder_1.append("}");
      _builder_1.newLine();
      _builder_1.newLine();
      this._compilationTestHelper.assertCompilesTo(_builder, _builder_1);
    } catch (Throwable _e) {
      throw Exceptions.sneakyThrow(_e);
    }
  }
  
  @Test
  public void inSkillConstructor_singleParam() {
    try {
      StringConcatenation _builder = new StringConcatenation();
      _builder.append("capacity C1 {}");
      _builder.newLine();
      _builder.append("skill S1 implements C1 {");
      _builder.newLine();
      _builder.append("\t");
      _builder.append("new(arg : int*) {");
      _builder.newLine();
      _builder.append("\t\t");
      _builder.append("System.out.println(arg)");
      _builder.newLine();
      _builder.append("\t");
      _builder.append("}");
      _builder.newLine();
      _builder.append("}");
      _builder.newLine();
      StringConcatenation _builder_1 = new StringConcatenation();
      _builder_1.append("MULTIPLE FILES WERE GENERATED");
      _builder_1.newLine();
      _builder_1.newLine();
      _builder_1.append("File 1 : C1.java");
      _builder_1.newLine();
      _builder_1.newLine();
      _builder_1.append("import io.sarl.lang.core.Capacity;");
      _builder_1.newLine();
      _builder_1.newLine();
      _builder_1.append("@SuppressWarnings(\"all\")");
      _builder_1.newLine();
      _builder_1.append("public interface C1 extends Capacity {");
      _builder_1.newLine();
      _builder_1.append("}");
      _builder_1.newLine();
      _builder_1.newLine();
      _builder_1.append("File 2 : S1.java");
      _builder_1.newLine();
      _builder_1.newLine();
      _builder_1.append("import io.sarl.lang.core.Skill;");
      _builder_1.newLine();
      _builder_1.newLine();
      _builder_1.append("@SuppressWarnings(\"all\")");
      _builder_1.newLine();
      _builder_1.append("public class S1 extends Skill implements C1 {");
      _builder_1.newLine();
      _builder_1.append("  ");
      _builder_1.append("public S1(final int... arg) {");
      _builder_1.newLine();
      _builder_1.append("    ");
      _builder_1.append("System.out.println(arg);");
      _builder_1.newLine();
      _builder_1.append("  ");
      _builder_1.append("}");
      _builder_1.newLine();
      _builder_1.append("}");
      _builder_1.newLine();
      _builder_1.newLine();
      this._compilationTestHelper.assertCompilesTo(_builder, _builder_1);
    } catch (Throwable _e) {
      throw Exceptions.sneakyThrow(_e);
    }
  }
  
  @Test
  public void inSkillConstructor() {
    try {
      StringConcatenation _builder = new StringConcatenation();
      _builder.append("capacity C1 {}");
      _builder.newLine();
      _builder.append("skill S1 implements C1 {");
      _builder.newLine();
      _builder.append("\t");
      _builder.append("new(arg1 : char, arg2 : boolean, arg3 : int*) {");
      _builder.newLine();
      _builder.append("\t\t");
      _builder.append("System.out.println(arg3)");
      _builder.newLine();
      _builder.append("\t");
      _builder.append("}");
      _builder.newLine();
      _builder.append("}");
      _builder.newLine();
      StringConcatenation _builder_1 = new StringConcatenation();
      _builder_1.append("MULTIPLE FILES WERE GENERATED");
      _builder_1.newLine();
      _builder_1.newLine();
      _builder_1.append("File 1 : C1.java");
      _builder_1.newLine();
      _builder_1.newLine();
      _builder_1.append("import io.sarl.lang.core.Capacity;");
      _builder_1.newLine();
      _builder_1.newLine();
      _builder_1.append("@SuppressWarnings(\"all\")");
      _builder_1.newLine();
      _builder_1.append("public interface C1 extends Capacity {");
      _builder_1.newLine();
      _builder_1.append("}");
      _builder_1.newLine();
      _builder_1.newLine();
      _builder_1.append("File 2 : S1.java");
      _builder_1.newLine();
      _builder_1.newLine();
      _builder_1.append("import io.sarl.lang.core.Skill;");
      _builder_1.newLine();
      _builder_1.newLine();
      _builder_1.append("@SuppressWarnings(\"all\")");
      _builder_1.newLine();
      _builder_1.append("public class S1 extends Skill implements C1 {");
      _builder_1.newLine();
      _builder_1.append("  ");
      _builder_1.append("public S1(final char arg1, final boolean arg2, final int... arg3) {");
      _builder_1.newLine();
      _builder_1.append("    ");
      _builder_1.append("System.out.println(arg3);");
      _builder_1.newLine();
      _builder_1.append("  ");
      _builder_1.append("}");
      _builder_1.newLine();
      _builder_1.append("}");
      _builder_1.newLine();
      _builder_1.newLine();
      this._compilationTestHelper.assertCompilesTo(_builder, _builder_1);
    } catch (Throwable _e) {
      throw Exceptions.sneakyThrow(_e);
    }
  }
}
