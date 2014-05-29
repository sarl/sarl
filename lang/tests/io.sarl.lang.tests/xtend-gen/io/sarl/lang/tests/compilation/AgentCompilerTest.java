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
import org.eclipse.xtext.util.IAcceptor;
import org.eclipse.xtext.xbase.compiler.CompilationTestHelper;
import org.eclipse.xtext.xbase.lib.Exceptions;
import org.eclipse.xtext.xbase.lib.Extension;
import org.junit.Assert;
import org.junit.Test;
import org.junit.runner.RunWith;

/**
 * @author $Author: srodriguez$
 * @version $Name$ $Revision$ $Date$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 */
@RunWith(XtextRunner.class)
@InjectWith(SARLInjectorProvider.class)
@SuppressWarnings("all")
public class AgentCompilerTest {
  @Inject
  @Extension
  private CompilationTestHelper _compilationTestHelper;
  
  @Test
  public void basicAgentCompile() {
    try {
      StringConcatenation _builder = new StringConcatenation();
      _builder.append("agent A1 {");
      _builder.newLine();
      _builder.append("\t");
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
      _builder_1.append("}");
      _builder_1.newLine();
      this._compilationTestHelper.assertCompilesTo(_builder, _builder_1);
    } catch (Throwable _e) {
      throw Exceptions.sneakyThrow(_e);
    }
  }
  
  @Test
  public void trueGuardBehaviorUnit() {
    try {
      StringConcatenation _builder = new StringConcatenation();
      _builder.append("import io.sarl.lang.annotation.Generated;");
      _builder.newLine();
      _builder.append("import io.sarl.lang.core.Event;");
      _builder.newLine();
      _builder.newLine();
      _builder.append("@SuppressWarnings(\"all\")");
      _builder.newLine();
      _builder.append("public class E1 extends Event {");
      _builder.newLine();
      _builder.append("  ");
      _builder.append("/**");
      _builder.newLine();
      _builder.append("   ");
      _builder.append("* Construct an event. The source of the event is unknown.");
      _builder.newLine();
      _builder.append("   ");
      _builder.append("* ");
      _builder.newLine();
      _builder.append("   ");
      _builder.append("*/");
      _builder.newLine();
      _builder.append("  ");
      _builder.append("@Generated");
      _builder.newLine();
      _builder.append("  ");
      _builder.append("public E1() {");
      _builder.newLine();
      _builder.append("    ");
      _builder.append("super();");
      _builder.newLine();
      _builder.append("  ");
      _builder.append("}");
      _builder.newLine();
      _builder.append("  ");
      _builder.newLine();
      _builder.append("  ");
      _builder.append("/**");
      _builder.newLine();
      _builder.append("   ");
      _builder.append("* Construct an event.");
      _builder.newLine();
      _builder.append("   ");
      _builder.append("* @param source - address of the agent that is emitting this event.");
      _builder.newLine();
      _builder.append("   ");
      _builder.append("* ");
      _builder.newLine();
      _builder.append("   ");
      _builder.append("*/");
      _builder.newLine();
      _builder.append("  ");
      _builder.append("@Generated");
      _builder.newLine();
      _builder.append("  ");
      _builder.append("public E1(final io.sarl.lang.core.Address source) {");
      _builder.newLine();
      _builder.append("    ");
      _builder.append("super(source);");
      _builder.newLine();
      _builder.append("  ");
      _builder.append("}");
      _builder.newLine();
      _builder.append("  ");
      _builder.newLine();
      _builder.append("  ");
      _builder.append("@Generated");
      _builder.newLine();
      _builder.append("  ");
      _builder.append("private final static long serialVersionUID = 588368462L;");
      _builder.newLine();
      _builder.append("}");
      _builder.newLine();
      final String expectedE1 = _builder.toString();
      StringConcatenation _builder_1 = new StringConcatenation();
      _builder_1.append("import io.sarl.lang.annotation.Generated;");
      _builder_1.newLine();
      _builder_1.append("import io.sarl.lang.core.Agent;");
      _builder_1.newLine();
      _builder_1.append("import io.sarl.lang.core.Percept;");
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
      _builder_1.append("@Percept");
      _builder_1.newLine();
      _builder_1.append("  ");
      _builder_1.append("public void _handle_E1_1(final E1 occurrence) {");
      _builder_1.newLine();
      _builder_1.append("    ");
      _builder_1.append("System.out.println(occurrence);");
      _builder_1.newLine();
      _builder_1.append("  ");
      _builder_1.append("}");
      _builder_1.newLine();
      _builder_1.append("}");
      _builder_1.newLine();
      final String expectedA1 = _builder_1.toString();
      StringConcatenation _builder_2 = new StringConcatenation();
      _builder_2.append("event E1");
      _builder_2.newLine();
      _builder_2.append("agent A1 {");
      _builder_2.newLine();
      _builder_2.append("\t");
      _builder_2.append("on E1 [ true ] {");
      _builder_2.newLine();
      _builder_2.append("\t\t");
      _builder_2.append("System.out.println(occurrence)");
      _builder_2.newLine();
      _builder_2.append("\t");
      _builder_2.append("}");
      _builder_2.newLine();
      _builder_2.append("}");
      _builder_2.newLine();
      final IAcceptor<CompilationTestHelper.Result> _function = new IAcceptor<CompilationTestHelper.Result>() {
        public void accept(final CompilationTestHelper.Result r) {
          String _generatedCode = r.getGeneratedCode("E1");
          Assert.assertEquals(expectedE1, _generatedCode);
          String _generatedCode_1 = r.getGeneratedCode("A1");
          Assert.assertEquals(expectedA1, _generatedCode_1);
        }
      };
      this._compilationTestHelper.compile(_builder_2, _function);
    } catch (Throwable _e) {
      throw Exceptions.sneakyThrow(_e);
    }
  }
  
  @Test
  public void falseGuardBehaviorUnit() {
    try {
      StringConcatenation _builder = new StringConcatenation();
      _builder.append("import io.sarl.lang.annotation.Generated;");
      _builder.newLine();
      _builder.append("import io.sarl.lang.core.Event;");
      _builder.newLine();
      _builder.newLine();
      _builder.append("@SuppressWarnings(\"all\")");
      _builder.newLine();
      _builder.append("public class E1 extends Event {");
      _builder.newLine();
      _builder.append("  ");
      _builder.append("/**");
      _builder.newLine();
      _builder.append("   ");
      _builder.append("* Construct an event. The source of the event is unknown.");
      _builder.newLine();
      _builder.append("   ");
      _builder.append("* ");
      _builder.newLine();
      _builder.append("   ");
      _builder.append("*/");
      _builder.newLine();
      _builder.append("  ");
      _builder.append("@Generated");
      _builder.newLine();
      _builder.append("  ");
      _builder.append("public E1() {");
      _builder.newLine();
      _builder.append("    ");
      _builder.append("super();");
      _builder.newLine();
      _builder.append("  ");
      _builder.append("}");
      _builder.newLine();
      _builder.append("  ");
      _builder.newLine();
      _builder.append("  ");
      _builder.append("/**");
      _builder.newLine();
      _builder.append("   ");
      _builder.append("* Construct an event.");
      _builder.newLine();
      _builder.append("   ");
      _builder.append("* @param source - address of the agent that is emitting this event.");
      _builder.newLine();
      _builder.append("   ");
      _builder.append("* ");
      _builder.newLine();
      _builder.append("   ");
      _builder.append("*/");
      _builder.newLine();
      _builder.append("  ");
      _builder.append("@Generated");
      _builder.newLine();
      _builder.append("  ");
      _builder.append("public E1(final io.sarl.lang.core.Address source) {");
      _builder.newLine();
      _builder.append("    ");
      _builder.append("super(source);");
      _builder.newLine();
      _builder.append("  ");
      _builder.append("}");
      _builder.newLine();
      _builder.append("  ");
      _builder.newLine();
      _builder.append("  ");
      _builder.append("@Generated");
      _builder.newLine();
      _builder.append("  ");
      _builder.append("private final static long serialVersionUID = 588368462L;");
      _builder.newLine();
      _builder.append("}");
      _builder.newLine();
      final String expectedE1 = _builder.toString();
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
      _builder_1.append("}");
      _builder_1.newLine();
      final String expectedA1 = _builder_1.toString();
      StringConcatenation _builder_2 = new StringConcatenation();
      _builder_2.append("event E1");
      _builder_2.newLine();
      _builder_2.append("agent A1 {");
      _builder_2.newLine();
      _builder_2.append("\t");
      _builder_2.append("on E1 [ false ] {");
      _builder_2.newLine();
      _builder_2.append("\t\t");
      _builder_2.append("System.out.println(occurrence)");
      _builder_2.newLine();
      _builder_2.append("\t");
      _builder_2.append("}");
      _builder_2.newLine();
      _builder_2.append("}");
      _builder_2.newLine();
      final IAcceptor<CompilationTestHelper.Result> _function = new IAcceptor<CompilationTestHelper.Result>() {
        public void accept(final CompilationTestHelper.Result r) {
          String _generatedCode = r.getGeneratedCode("E1");
          Assert.assertEquals(expectedE1, _generatedCode);
          String _generatedCode_1 = r.getGeneratedCode("A1");
          Assert.assertEquals(expectedA1, _generatedCode_1);
        }
      };
      this._compilationTestHelper.compile(_builder_2, _function);
    } catch (Throwable _e) {
      throw Exceptions.sneakyThrow(_e);
    }
  }
}
