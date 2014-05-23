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
public class ArgDefaultValueCompilerTest {
  @Inject
  @Extension
  private CompilationTestHelper _compilationTestHelper;
  
  @Test
  public void inAgentAction_1p_int() {
    try {
      StringConcatenation _builder = new StringConcatenation();
      _builder.append("agent A1 {");
      _builder.newLine();
      _builder.append("\t");
      _builder.append("def myaction(arg : int=4) {");
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
      _builder_1.append("* Creates a new Agent of type A1");
      _builder_1.newLine();
      _builder_1.append("   ");
      _builder_1.append("*/");
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
      _builder_1.append("public void myaction(final int arg) {");
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
      _builder_1.append("public final void myaction() {");
      _builder_1.newLine();
      _builder_1.append("    ");
      _builder_1.append("myaction(4);");
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
  public void inAgentAction_1p_float() {
    try {
      StringConcatenation _builder = new StringConcatenation();
      _builder.append("agent A1 {");
      _builder.newLine();
      _builder.append("\t");
      _builder.append("def myaction(arg : float=4.5) {");
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
      _builder_1.append("* Creates a new Agent of type A1");
      _builder_1.newLine();
      _builder_1.append("   ");
      _builder_1.append("*/");
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
      _builder_1.append("public void myaction(final float arg) {");
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
      _builder_1.append("public final void myaction() {");
      _builder_1.newLine();
      _builder_1.append("    ");
      _builder_1.append("myaction(4.5);");
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
  public void inAgentAction_1p_boolean() {
    try {
      StringConcatenation _builder = new StringConcatenation();
      _builder.append("agent A1 {");
      _builder.newLine();
      _builder.append("\t");
      _builder.append("def myaction(arg : boolean=true) {");
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
      _builder_1.append("* Creates a new Agent of type A1");
      _builder_1.newLine();
      _builder_1.append("   ");
      _builder_1.append("*/");
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
      _builder_1.append("public void myaction(final boolean arg) {");
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
      _builder_1.append("public final void myaction() {");
      _builder_1.newLine();
      _builder_1.append("    ");
      _builder_1.append("myaction(true);");
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
  public void inAgentAction_1p_byte() {
    try {
      StringConcatenation _builder = new StringConcatenation();
      _builder.append("agent A1 {");
      _builder.newLine();
      _builder.append("\t");
      _builder.append("def myaction(arg : byte=54) {");
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
      _builder_1.append("* Creates a new Agent of type A1");
      _builder_1.newLine();
      _builder_1.append("   ");
      _builder_1.append("*/");
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
      _builder_1.append("public void myaction(final byte arg) {");
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
      _builder_1.append("public final void myaction() {");
      _builder_1.newLine();
      _builder_1.append("    ");
      _builder_1.append("myaction(54);");
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
  public void inAgentAction_1p_double() {
    try {
      StringConcatenation _builder = new StringConcatenation();
      _builder.append("agent A1 {");
      _builder.newLine();
      _builder.append("\t");
      _builder.append("def myaction(arg : double=4.5) {");
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
      _builder_1.append("* Creates a new Agent of type A1");
      _builder_1.newLine();
      _builder_1.append("   ");
      _builder_1.append("*/");
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
      _builder_1.append("public void myaction(final double arg) {");
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
      _builder_1.append("public final void myaction() {");
      _builder_1.newLine();
      _builder_1.append("    ");
      _builder_1.append("myaction(4.5);");
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
  public void inAgentAction_1p_long() {
    try {
      StringConcatenation _builder = new StringConcatenation();
      _builder.append("agent A1 {");
      _builder.newLine();
      _builder.append("\t");
      _builder.append("def myaction(arg : long=450) {");
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
      _builder_1.append("* Creates a new Agent of type A1");
      _builder_1.newLine();
      _builder_1.append("   ");
      _builder_1.append("*/");
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
      _builder_1.append("public void myaction(final long arg) {");
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
      _builder_1.append("public final void myaction() {");
      _builder_1.newLine();
      _builder_1.append("    ");
      _builder_1.append("myaction(450);");
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
  public void inAgentAction_1p_short() {
    try {
      StringConcatenation _builder = new StringConcatenation();
      _builder.append("agent A1 {");
      _builder.newLine();
      _builder.append("\t");
      _builder.append("def myaction(arg : short=450) {");
      _builder.newLine();
      _builder.append("\t\t");
      _builder.append("System.out.println(arg as int)");
      _builder.newLine();
      _builder.append("\t");
      _builder.append("}");
      _builder.newLine();
      _builder.append("}");
      _builder.newLine();
      StringConcatenation _builder_1 = new StringConcatenation();
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
      _builder_1.append("* Creates a new Agent of type A1");
      _builder_1.newLine();
      _builder_1.append("   ");
      _builder_1.append("*/");
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
      _builder_1.append("public void myaction(final short arg) {");
      _builder_1.newLine();
      _builder_1.append("    ");
      _builder_1.append("System.out.println(((int) arg));");
      _builder_1.newLine();
      _builder_1.append("  ");
      _builder_1.append("}");
      _builder_1.newLine();
      _builder_1.append("  ");
      _builder_1.newLine();
      _builder_1.append("  ");
      _builder_1.append("public final void myaction() {");
      _builder_1.newLine();
      _builder_1.append("    ");
      _builder_1.append("myaction(450);");
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
  public void inAgentAction_1p_String() {
    try {
      StringConcatenation _builder = new StringConcatenation();
      _builder.append("agent A1 {");
      _builder.newLine();
      _builder.append("\t");
      _builder.append("def myaction(arg : String=\"abcd\") {");
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
      _builder_1.append("* Creates a new Agent of type A1");
      _builder_1.newLine();
      _builder_1.append("   ");
      _builder_1.append("*/");
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
      _builder_1.append("public void myaction(final String arg) {");
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
      _builder_1.append("public final void myaction() {");
      _builder_1.newLine();
      _builder_1.append("    ");
      _builder_1.append("myaction(\"abcd\");");
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
  public void inAgentAction_1p_char() {
    try {
      StringConcatenation _builder = new StringConcatenation();
      _builder.append("agent A1 {");
      _builder.newLine();
      _builder.append("\t");
      _builder.append("def myaction(arg : char=\'d\') {");
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
      _builder_1.append("* Creates a new Agent of type A1");
      _builder_1.newLine();
      _builder_1.append("   ");
      _builder_1.append("*/");
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
      _builder_1.append("public void myaction(final char arg) {");
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
      _builder_1.append("public final void myaction() {");
      _builder_1.newLine();
      _builder_1.append("    ");
      _builder_1.append("myaction(\'d\');");
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
  public void inAgentAction_5p_0() {
    try {
      StringConcatenation _builder = new StringConcatenation();
      _builder.append("agent A1 {");
      _builder.newLine();
      _builder.append("\t");
      _builder.append("def myaction(arg0 : int=4, arg1 : String, arg2 : int, arg3 : int, arg4 : String) {");
      _builder.newLine();
      _builder.append("\t\t");
      _builder.append("System.out.println(arg0)");
      _builder.newLine();
      _builder.append("\t");
      _builder.append("}");
      _builder.newLine();
      _builder.append("}");
      _builder.newLine();
      StringConcatenation _builder_1 = new StringConcatenation();
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
      _builder_1.append("* Creates a new Agent of type A1");
      _builder_1.newLine();
      _builder_1.append("   ");
      _builder_1.append("*/");
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
      _builder_1.append("public void myaction(final int arg0, final String arg1, final int arg2, final int arg3, final String arg4) {");
      _builder_1.newLine();
      _builder_1.append("    ");
      _builder_1.append("System.out.println(arg0);");
      _builder_1.newLine();
      _builder_1.append("  ");
      _builder_1.append("}");
      _builder_1.newLine();
      _builder_1.append("  ");
      _builder_1.newLine();
      _builder_1.append("  ");
      _builder_1.append("public final void myaction(final String arg1, final int arg2, final int arg3, final String arg4) {");
      _builder_1.newLine();
      _builder_1.append("    ");
      _builder_1.append("myaction(4, arg1, arg2, arg3, arg4);");
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
  public void inAgentAction_5p_1() {
    try {
      StringConcatenation _builder = new StringConcatenation();
      _builder.append("agent A1 {");
      _builder.newLine();
      _builder.append("\t");
      _builder.append("def myaction(arg0 : int, arg1 : String=\"abc\", arg2 : int, arg3 : int, arg4 : String) {");
      _builder.newLine();
      _builder.append("\t\t");
      _builder.append("System.out.println(arg0)");
      _builder.newLine();
      _builder.append("\t");
      _builder.append("}");
      _builder.newLine();
      _builder.append("}");
      _builder.newLine();
      StringConcatenation _builder_1 = new StringConcatenation();
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
      _builder_1.append("* Creates a new Agent of type A1");
      _builder_1.newLine();
      _builder_1.append("   ");
      _builder_1.append("*/");
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
      _builder_1.append("public void myaction(final int arg0, final String arg1, final int arg2, final int arg3, final String arg4) {");
      _builder_1.newLine();
      _builder_1.append("    ");
      _builder_1.append("System.out.println(arg0);");
      _builder_1.newLine();
      _builder_1.append("  ");
      _builder_1.append("}");
      _builder_1.newLine();
      _builder_1.append("  ");
      _builder_1.newLine();
      _builder_1.append("  ");
      _builder_1.append("public final void myaction(final int arg0, final int arg2, final int arg3, final String arg4) {");
      _builder_1.newLine();
      _builder_1.append("    ");
      _builder_1.append("myaction(arg0, \"abc\", arg2, arg3, arg4);");
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
  public void inAgentAction_5p_2() {
    try {
      StringConcatenation _builder = new StringConcatenation();
      _builder.append("agent A1 {");
      _builder.newLine();
      _builder.append("\t");
      _builder.append("def myaction(arg0 : int, arg1 : String, arg2 : int=18, arg3 : int, arg4 : String) {");
      _builder.newLine();
      _builder.append("\t\t");
      _builder.append("System.out.println(arg0)");
      _builder.newLine();
      _builder.append("\t");
      _builder.append("}");
      _builder.newLine();
      _builder.append("}");
      _builder.newLine();
      StringConcatenation _builder_1 = new StringConcatenation();
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
      _builder_1.append("* Creates a new Agent of type A1");
      _builder_1.newLine();
      _builder_1.append("   ");
      _builder_1.append("*/");
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
      _builder_1.append("public void myaction(final int arg0, final String arg1, final int arg2, final int arg3, final String arg4) {");
      _builder_1.newLine();
      _builder_1.append("    ");
      _builder_1.append("System.out.println(arg0);");
      _builder_1.newLine();
      _builder_1.append("  ");
      _builder_1.append("}");
      _builder_1.newLine();
      _builder_1.append("  ");
      _builder_1.newLine();
      _builder_1.append("  ");
      _builder_1.append("public final void myaction(final int arg0, final String arg1, final int arg3, final String arg4) {");
      _builder_1.newLine();
      _builder_1.append("    ");
      _builder_1.append("myaction(arg0, arg1, 18, arg3, arg4);");
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
  public void inAgentAction_5p_3() {
    try {
      StringConcatenation _builder = new StringConcatenation();
      _builder.append("agent A1 {");
      _builder.newLine();
      _builder.append("\t");
      _builder.append("def myaction(arg0 : int, arg1 : String, arg2 : int, arg3 : int = 34, arg4 : String) {");
      _builder.newLine();
      _builder.append("\t\t");
      _builder.append("System.out.println(arg0)");
      _builder.newLine();
      _builder.append("\t");
      _builder.append("}");
      _builder.newLine();
      _builder.append("}");
      _builder.newLine();
      StringConcatenation _builder_1 = new StringConcatenation();
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
      _builder_1.append("* Creates a new Agent of type A1");
      _builder_1.newLine();
      _builder_1.append("   ");
      _builder_1.append("*/");
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
      _builder_1.append("public void myaction(final int arg0, final String arg1, final int arg2, final int arg3, final String arg4) {");
      _builder_1.newLine();
      _builder_1.append("    ");
      _builder_1.append("System.out.println(arg0);");
      _builder_1.newLine();
      _builder_1.append("  ");
      _builder_1.append("}");
      _builder_1.newLine();
      _builder_1.append("  ");
      _builder_1.newLine();
      _builder_1.append("  ");
      _builder_1.append("public final void myaction(final int arg0, final String arg1, final int arg2, final String arg4) {");
      _builder_1.newLine();
      _builder_1.append("    ");
      _builder_1.append("myaction(arg0, arg1, arg2, 34, arg4);");
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
  public void inAgentAction_5p_4() {
    try {
      StringConcatenation _builder = new StringConcatenation();
      _builder.append("agent A1 {");
      _builder.newLine();
      _builder.append("\t");
      _builder.append("def myaction(arg0 : int, arg1 : String, arg2 : int, arg3 : int, arg4 : String=\"xyz\") {");
      _builder.newLine();
      _builder.append("\t\t");
      _builder.append("System.out.println(arg0)");
      _builder.newLine();
      _builder.append("\t");
      _builder.append("}");
      _builder.newLine();
      _builder.append("}");
      _builder.newLine();
      StringConcatenation _builder_1 = new StringConcatenation();
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
      _builder_1.append("* Creates a new Agent of type A1");
      _builder_1.newLine();
      _builder_1.append("   ");
      _builder_1.append("*/");
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
      _builder_1.append("public void myaction(final int arg0, final String arg1, final int arg2, final int arg3, final String arg4) {");
      _builder_1.newLine();
      _builder_1.append("    ");
      _builder_1.append("System.out.println(arg0);");
      _builder_1.newLine();
      _builder_1.append("  ");
      _builder_1.append("}");
      _builder_1.newLine();
      _builder_1.append("  ");
      _builder_1.newLine();
      _builder_1.append("  ");
      _builder_1.append("public final void myaction(final int arg0, final String arg1, final int arg2, final int arg3) {");
      _builder_1.newLine();
      _builder_1.append("    ");
      _builder_1.append("myaction(arg0, arg1, arg2, arg3, \"xyz\");");
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
  public void inAgentAction_5p_0_3() {
    try {
      StringConcatenation _builder = new StringConcatenation();
      _builder.append("agent A1 {");
      _builder.newLine();
      _builder.append("\t");
      _builder.append("def myaction(arg0 : int=4, arg1 : String, arg2 : int, arg3 : int=56, arg4 : String) {");
      _builder.newLine();
      _builder.append("\t\t");
      _builder.append("System.out.println(arg0)");
      _builder.newLine();
      _builder.append("\t");
      _builder.append("}");
      _builder.newLine();
      _builder.append("}");
      _builder.newLine();
      StringConcatenation _builder_1 = new StringConcatenation();
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
      _builder_1.append("* Creates a new Agent of type A1");
      _builder_1.newLine();
      _builder_1.append("   ");
      _builder_1.append("*/");
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
      _builder_1.append("public void myaction(final int arg0, final String arg1, final int arg2, final int arg3, final String arg4) {");
      _builder_1.newLine();
      _builder_1.append("    ");
      _builder_1.append("System.out.println(arg0);");
      _builder_1.newLine();
      _builder_1.append("  ");
      _builder_1.append("}");
      _builder_1.newLine();
      _builder_1.append("  ");
      _builder_1.newLine();
      _builder_1.append("  ");
      _builder_1.append("public final void myaction(final String arg1, final int arg2, final String arg4) {");
      _builder_1.newLine();
      _builder_1.append("    ");
      _builder_1.append("myaction(4, arg1, arg2, 56, arg4);");
      _builder_1.newLine();
      _builder_1.append("  ");
      _builder_1.append("}");
      _builder_1.newLine();
      _builder_1.append("  ");
      _builder_1.newLine();
      _builder_1.append("  ");
      _builder_1.append("public final void myaction(final int arg0, final String arg1, final int arg2, final String arg4) {");
      _builder_1.newLine();
      _builder_1.append("    ");
      _builder_1.append("myaction(arg0, arg1, arg2, 56, arg4);");
      _builder_1.newLine();
      _builder_1.append("  ");
      _builder_1.append("}");
      _builder_1.newLine();
      _builder_1.append("  ");
      _builder_1.newLine();
      _builder_1.append("  ");
      _builder_1.append("public final void myaction(final String arg1, final int arg2, final int arg3, final String arg4) {");
      _builder_1.newLine();
      _builder_1.append("    ");
      _builder_1.append("myaction(4, arg1, arg2, arg3, arg4);");
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
  public void inAgentAction_3p_vararg_1() {
    try {
      StringConcatenation _builder = new StringConcatenation();
      _builder.append("agent A1 {");
      _builder.newLine();
      _builder.append("\t");
      _builder.append("def myaction(arg0 : int, arg1 : int=45, arg2 : int*) {");
      _builder.newLine();
      _builder.append("\t\t");
      _builder.append("System.out.println(arg0)");
      _builder.newLine();
      _builder.append("\t");
      _builder.append("}");
      _builder.newLine();
      _builder.append("}");
      _builder.newLine();
      StringConcatenation _builder_1 = new StringConcatenation();
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
      _builder_1.append("* Creates a new Agent of type A1");
      _builder_1.newLine();
      _builder_1.append("   ");
      _builder_1.append("*/");
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
      _builder_1.append("public void myaction(final int arg0, final int arg1, final int... arg2) {");
      _builder_1.newLine();
      _builder_1.append("    ");
      _builder_1.append("System.out.println(arg0);");
      _builder_1.newLine();
      _builder_1.append("  ");
      _builder_1.append("}");
      _builder_1.newLine();
      _builder_1.append("  ");
      _builder_1.newLine();
      _builder_1.append("  ");
      _builder_1.append("public final void myaction(final int arg0, final int... arg2) {");
      _builder_1.newLine();
      _builder_1.append("    ");
      _builder_1.append("myaction(arg0, 45, arg2);");
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
  public void inAgentAction_3p_vararg_0() {
    try {
      StringConcatenation _builder = new StringConcatenation();
      _builder.append("agent A1 {");
      _builder.newLine();
      _builder.append("\t");
      _builder.append("def myaction(arg0 : int=45, arg1 : int, arg2 : int*) {");
      _builder.newLine();
      _builder.append("\t\t");
      _builder.append("System.out.println(arg0)");
      _builder.newLine();
      _builder.append("\t");
      _builder.append("}");
      _builder.newLine();
      _builder.append("}");
      _builder.newLine();
      StringConcatenation _builder_1 = new StringConcatenation();
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
      _builder_1.append("* Creates a new Agent of type A1");
      _builder_1.newLine();
      _builder_1.append("   ");
      _builder_1.append("*/");
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
      _builder_1.append("public void myaction(final int arg0, final int arg1, final int... arg2) {");
      _builder_1.newLine();
      _builder_1.append("    ");
      _builder_1.append("System.out.println(arg0);");
      _builder_1.newLine();
      _builder_1.append("  ");
      _builder_1.append("}");
      _builder_1.newLine();
      _builder_1.append("  ");
      _builder_1.newLine();
      _builder_1.append("  ");
      _builder_1.append("public final void myaction(final int arg1, final int... arg2) {");
      _builder_1.newLine();
      _builder_1.append("    ");
      _builder_1.append("myaction(45, arg1, arg2);");
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
  public void inAgentAction_3p_vararg_0_1() {
    try {
      StringConcatenation _builder = new StringConcatenation();
      _builder.append("agent A1 {");
      _builder.newLine();
      _builder.append("\t");
      _builder.append("def myaction(arg0 : int=45, arg1 : int=56, arg2 : int*) {");
      _builder.newLine();
      _builder.append("\t\t");
      _builder.append("System.out.println(arg0)");
      _builder.newLine();
      _builder.append("\t");
      _builder.append("}");
      _builder.newLine();
      _builder.append("}");
      _builder.newLine();
      StringConcatenation _builder_1 = new StringConcatenation();
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
      _builder_1.append("* Creates a new Agent of type A1");
      _builder_1.newLine();
      _builder_1.append("   ");
      _builder_1.append("*/");
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
      _builder_1.append("public void myaction(final int arg0, final int arg1, final int... arg2) {");
      _builder_1.newLine();
      _builder_1.append("    ");
      _builder_1.append("System.out.println(arg0);");
      _builder_1.newLine();
      _builder_1.append("  ");
      _builder_1.append("}");
      _builder_1.newLine();
      _builder_1.append("  ");
      _builder_1.newLine();
      _builder_1.append("  ");
      _builder_1.append("public final void myaction(final int... arg2) {");
      _builder_1.newLine();
      _builder_1.append("    ");
      _builder_1.append("myaction(45, 56, arg2);");
      _builder_1.newLine();
      _builder_1.append("  ");
      _builder_1.append("}");
      _builder_1.newLine();
      _builder_1.append("  ");
      _builder_1.newLine();
      _builder_1.append("  ");
      _builder_1.append("public final void myaction(final int arg0, final int... arg2) {");
      _builder_1.newLine();
      _builder_1.append("    ");
      _builder_1.append("myaction(arg0, 56, arg2);");
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
  public void inAgentAction_4p_0_1_2_3() {
    try {
      StringConcatenation _builder = new StringConcatenation();
      _builder.append("agent A1 {");
      _builder.newLine();
      _builder.append("\t");
      _builder.append("def myaction(arg0 : int=45, arg1 : int=56, arg2 : int=78, arg3 : int=14) {");
      _builder.newLine();
      _builder.append("\t\t");
      _builder.append("System.out.println(arg0)");
      _builder.newLine();
      _builder.append("\t");
      _builder.append("}");
      _builder.newLine();
      _builder.append("}");
      _builder.newLine();
      StringConcatenation _builder_1 = new StringConcatenation();
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
      _builder_1.append("* Creates a new Agent of type A1");
      _builder_1.newLine();
      _builder_1.append("   ");
      _builder_1.append("*/");
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
      _builder_1.append("public void myaction(final int arg0, final int arg1, final int arg2, final int arg3) {");
      _builder_1.newLine();
      _builder_1.append("    ");
      _builder_1.append("System.out.println(arg0);");
      _builder_1.newLine();
      _builder_1.append("  ");
      _builder_1.append("}");
      _builder_1.newLine();
      _builder_1.append("  ");
      _builder_1.newLine();
      _builder_1.append("  ");
      _builder_1.append("public final void myaction() {");
      _builder_1.newLine();
      _builder_1.append("    ");
      _builder_1.append("myaction(45, 56, 78, 14);");
      _builder_1.newLine();
      _builder_1.append("  ");
      _builder_1.append("}");
      _builder_1.newLine();
      _builder_1.append("  ");
      _builder_1.newLine();
      _builder_1.append("  ");
      _builder_1.append("public final void myaction(final int arg0) {");
      _builder_1.newLine();
      _builder_1.append("    ");
      _builder_1.append("myaction(arg0, 56, 78, 14);");
      _builder_1.newLine();
      _builder_1.append("  ");
      _builder_1.append("}");
      _builder_1.newLine();
      _builder_1.append("  ");
      _builder_1.newLine();
      _builder_1.append("  ");
      _builder_1.append("public final void myaction(final int arg0, final int arg1) {");
      _builder_1.newLine();
      _builder_1.append("    ");
      _builder_1.append("myaction(arg0, arg1, 78, 14);");
      _builder_1.newLine();
      _builder_1.append("  ");
      _builder_1.append("}");
      _builder_1.newLine();
      _builder_1.append("  ");
      _builder_1.newLine();
      _builder_1.append("  ");
      _builder_1.append("public final void myaction(final int arg0, final int arg1, final int arg2) {");
      _builder_1.newLine();
      _builder_1.append("    ");
      _builder_1.append("myaction(arg0, arg1, arg2, 14);");
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
  public void inBehaviorConstructor_1p_int() {
    try {
      StringConcatenation _builder = new StringConcatenation();
      _builder.append("behavior B1 {");
      _builder.newLine();
      _builder.append("\t");
      _builder.append("new(arg : int=4) {");
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
      _builder_1.append("public B1(final int arg) {");
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
      _builder_1.append("public B1() {");
      _builder_1.newLine();
      _builder_1.append("    ");
      _builder_1.append("this(4);");
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
  public void inBehaviorConstructor_1p_float() {
    try {
      StringConcatenation _builder = new StringConcatenation();
      _builder.append("behavior B1 {");
      _builder.newLine();
      _builder.append("\t");
      _builder.append("new(arg : float=4.5) {");
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
      _builder_1.append("public B1(final float arg) {");
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
      _builder_1.append("public B1() {");
      _builder_1.newLine();
      _builder_1.append("    ");
      _builder_1.append("this(4.5);");
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
  public void inBehaviorConstructor_1p_boolean() {
    try {
      StringConcatenation _builder = new StringConcatenation();
      _builder.append("behavior B1 {");
      _builder.newLine();
      _builder.append("\t");
      _builder.append("new(arg : boolean=true) {");
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
      _builder_1.append("public B1(final boolean arg) {");
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
      _builder_1.append("public B1() {");
      _builder_1.newLine();
      _builder_1.append("    ");
      _builder_1.append("this(true);");
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
  public void inBehaviorConstructor_1p_byte() {
    try {
      StringConcatenation _builder = new StringConcatenation();
      _builder.append("behavior B1 {");
      _builder.newLine();
      _builder.append("\t");
      _builder.append("new(arg : byte=54) {");
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
      _builder_1.append("public B1(final byte arg) {");
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
      _builder_1.append("public B1() {");
      _builder_1.newLine();
      _builder_1.append("    ");
      _builder_1.append("this(54);");
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
  public void inBehaviorConstructor_1p_double() {
    try {
      StringConcatenation _builder = new StringConcatenation();
      _builder.append("behavior B1 {");
      _builder.newLine();
      _builder.append("\t");
      _builder.append("new(arg : double=4.5) {");
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
      _builder_1.append("public B1(final double arg) {");
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
      _builder_1.append("public B1() {");
      _builder_1.newLine();
      _builder_1.append("    ");
      _builder_1.append("this(4.5);");
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
  public void inBehaviorConstructor_1p_long() {
    try {
      StringConcatenation _builder = new StringConcatenation();
      _builder.append("behavior B1 {");
      _builder.newLine();
      _builder.append("\t");
      _builder.append("new(arg : long=450) {");
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
      _builder_1.append("public B1(final long arg) {");
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
      _builder_1.append("public B1() {");
      _builder_1.newLine();
      _builder_1.append("    ");
      _builder_1.append("this(450);");
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
  public void inBehaviorConstructor_1p_short() {
    try {
      StringConcatenation _builder = new StringConcatenation();
      _builder.append("behavior B1 {");
      _builder.newLine();
      _builder.append("\t");
      _builder.append("new(arg : short=450) {");
      _builder.newLine();
      _builder.append("\t\t");
      _builder.append("System.out.println(arg as int)");
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
      _builder_1.append("public B1(final short arg) {");
      _builder_1.newLine();
      _builder_1.append("    ");
      _builder_1.append("System.out.println(((int) arg));");
      _builder_1.newLine();
      _builder_1.append("  ");
      _builder_1.append("}");
      _builder_1.newLine();
      _builder_1.append("  ");
      _builder_1.newLine();
      _builder_1.append("  ");
      _builder_1.append("public B1() {");
      _builder_1.newLine();
      _builder_1.append("    ");
      _builder_1.append("this(450);");
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
  public void inBehaviorConstructor_1p_String() {
    try {
      StringConcatenation _builder = new StringConcatenation();
      _builder.append("behavior B1 {");
      _builder.newLine();
      _builder.append("\t");
      _builder.append("new(arg : String=\"abcd\") {");
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
      _builder_1.append("public B1(final String arg) {");
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
      _builder_1.append("public B1() {");
      _builder_1.newLine();
      _builder_1.append("    ");
      _builder_1.append("this(\"abcd\");");
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
  public void inBehaviorConstructor_1p_char() {
    try {
      StringConcatenation _builder = new StringConcatenation();
      _builder.append("behavior B1 {");
      _builder.newLine();
      _builder.append("\t");
      _builder.append("new(arg : char=\'d\') {");
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
      _builder_1.append("public B1(final char arg) {");
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
      _builder_1.append("public B1() {");
      _builder_1.newLine();
      _builder_1.append("    ");
      _builder_1.append("this(\'d\');");
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
  public void inBehaviorConstructor_5p_0() {
    try {
      StringConcatenation _builder = new StringConcatenation();
      _builder.append("behavior B1 {");
      _builder.newLine();
      _builder.append("\t");
      _builder.append("new(arg0 : int=4, arg1 : String, arg2 : int, arg3 : int, arg4 : String) {");
      _builder.newLine();
      _builder.append("\t\t");
      _builder.append("System.out.println(arg0)");
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
      _builder_1.append("public B1(final int arg0, final String arg1, final int arg2, final int arg3, final String arg4) {");
      _builder_1.newLine();
      _builder_1.append("    ");
      _builder_1.append("System.out.println(arg0);");
      _builder_1.newLine();
      _builder_1.append("  ");
      _builder_1.append("}");
      _builder_1.newLine();
      _builder_1.append("  ");
      _builder_1.newLine();
      _builder_1.append("  ");
      _builder_1.append("public B1(final String arg1, final int arg2, final int arg3, final String arg4) {");
      _builder_1.newLine();
      _builder_1.append("    ");
      _builder_1.append("this(4, arg1, arg2, arg3, arg4);");
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
  public void inBehaviorConstructor_5p_1() {
    try {
      StringConcatenation _builder = new StringConcatenation();
      _builder.append("behavior B1 {");
      _builder.newLine();
      _builder.append("\t");
      _builder.append("new(arg0 : int, arg1 : String=\"abc\", arg2 : int, arg3 : int, arg4 : String) {");
      _builder.newLine();
      _builder.append("\t\t");
      _builder.append("System.out.println(arg0)");
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
      _builder_1.append("public B1(final int arg0, final String arg1, final int arg2, final int arg3, final String arg4) {");
      _builder_1.newLine();
      _builder_1.append("    ");
      _builder_1.append("System.out.println(arg0);");
      _builder_1.newLine();
      _builder_1.append("  ");
      _builder_1.append("}");
      _builder_1.newLine();
      _builder_1.append("  ");
      _builder_1.newLine();
      _builder_1.append("  ");
      _builder_1.append("public B1(final int arg0, final int arg2, final int arg3, final String arg4) {");
      _builder_1.newLine();
      _builder_1.append("    ");
      _builder_1.append("this(arg0, \"abc\", arg2, arg3, arg4);");
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
  public void inBehaviorConstructor_5p_2() {
    try {
      StringConcatenation _builder = new StringConcatenation();
      _builder.append("behavior B1 {");
      _builder.newLine();
      _builder.append("\t");
      _builder.append("new(arg0 : int, arg1 : String, arg2 : int=18, arg3 : int, arg4 : String) {");
      _builder.newLine();
      _builder.append("\t\t");
      _builder.append("System.out.println(arg0)");
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
      _builder_1.append("public B1(final int arg0, final String arg1, final int arg2, final int arg3, final String arg4) {");
      _builder_1.newLine();
      _builder_1.append("    ");
      _builder_1.append("System.out.println(arg0);");
      _builder_1.newLine();
      _builder_1.append("  ");
      _builder_1.append("}");
      _builder_1.newLine();
      _builder_1.append("  ");
      _builder_1.newLine();
      _builder_1.append("  ");
      _builder_1.append("public B1(final int arg0, final String arg1, final int arg3, final String arg4) {");
      _builder_1.newLine();
      _builder_1.append("    ");
      _builder_1.append("this(arg0, arg1, 18, arg3, arg4);");
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
  public void inBehaviorConstructor_5p_3() {
    try {
      StringConcatenation _builder = new StringConcatenation();
      _builder.append("behavior B1 {");
      _builder.newLine();
      _builder.append("\t");
      _builder.append("new(arg0 : int, arg1 : String, arg2 : int, arg3 : int = 34, arg4 : String) {");
      _builder.newLine();
      _builder.append("\t\t");
      _builder.append("System.out.println(arg0)");
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
      _builder_1.append("public B1(final int arg0, final String arg1, final int arg2, final int arg3, final String arg4) {");
      _builder_1.newLine();
      _builder_1.append("    ");
      _builder_1.append("System.out.println(arg0);");
      _builder_1.newLine();
      _builder_1.append("  ");
      _builder_1.append("}");
      _builder_1.newLine();
      _builder_1.append("  ");
      _builder_1.newLine();
      _builder_1.append("  ");
      _builder_1.append("public B1(final int arg0, final String arg1, final int arg2, final String arg4) {");
      _builder_1.newLine();
      _builder_1.append("    ");
      _builder_1.append("this(arg0, arg1, arg2, 34, arg4);");
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
  public void inBehaviorConstructor_5p_4() {
    try {
      StringConcatenation _builder = new StringConcatenation();
      _builder.append("behavior B1 {");
      _builder.newLine();
      _builder.append("\t");
      _builder.append("new(arg0 : int, arg1 : String, arg2 : int, arg3 : int, arg4 : String=\"xyz\") {");
      _builder.newLine();
      _builder.append("\t\t");
      _builder.append("System.out.println(arg0)");
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
      _builder_1.append("public B1(final int arg0, final String arg1, final int arg2, final int arg3, final String arg4) {");
      _builder_1.newLine();
      _builder_1.append("    ");
      _builder_1.append("System.out.println(arg0);");
      _builder_1.newLine();
      _builder_1.append("  ");
      _builder_1.append("}");
      _builder_1.newLine();
      _builder_1.append("  ");
      _builder_1.newLine();
      _builder_1.append("  ");
      _builder_1.append("public B1(final int arg0, final String arg1, final int arg2, final int arg3) {");
      _builder_1.newLine();
      _builder_1.append("    ");
      _builder_1.append("this(arg0, arg1, arg2, arg3, \"xyz\");");
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
  public void inBehaviorConstructor_5p_0_3() {
    try {
      StringConcatenation _builder = new StringConcatenation();
      _builder.append("behavior B1 {");
      _builder.newLine();
      _builder.append("\t");
      _builder.append("new(arg0 : int=4, arg1 : String, arg2 : int, arg3 : int=56, arg4 : String) {");
      _builder.newLine();
      _builder.append("\t\t");
      _builder.append("System.out.println(arg0)");
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
      _builder_1.append("public B1(final int arg0, final String arg1, final int arg2, final int arg3, final String arg4) {");
      _builder_1.newLine();
      _builder_1.append("    ");
      _builder_1.append("System.out.println(arg0);");
      _builder_1.newLine();
      _builder_1.append("  ");
      _builder_1.append("}");
      _builder_1.newLine();
      _builder_1.append("  ");
      _builder_1.newLine();
      _builder_1.append("  ");
      _builder_1.append("public B1(final String arg1, final int arg2, final String arg4) {");
      _builder_1.newLine();
      _builder_1.append("    ");
      _builder_1.append("this(4, arg1, arg2, 56, arg4);");
      _builder_1.newLine();
      _builder_1.append("  ");
      _builder_1.append("}");
      _builder_1.newLine();
      _builder_1.append("  ");
      _builder_1.newLine();
      _builder_1.append("  ");
      _builder_1.append("public B1(final int arg0, final String arg1, final int arg2, final String arg4) {");
      _builder_1.newLine();
      _builder_1.append("    ");
      _builder_1.append("this(arg0, arg1, arg2, 56, arg4);");
      _builder_1.newLine();
      _builder_1.append("  ");
      _builder_1.append("}");
      _builder_1.newLine();
      _builder_1.append("  ");
      _builder_1.newLine();
      _builder_1.append("  ");
      _builder_1.append("public B1(final String arg1, final int arg2, final int arg3, final String arg4) {");
      _builder_1.newLine();
      _builder_1.append("    ");
      _builder_1.append("this(4, arg1, arg2, arg3, arg4);");
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
  public void inBehaviorConstructor_3p_vararg_1() {
    try {
      StringConcatenation _builder = new StringConcatenation();
      _builder.append("behavior B1 {");
      _builder.newLine();
      _builder.append("\t");
      _builder.append("new(arg0 : int, arg1 : int=45, arg2 : int*) {");
      _builder.newLine();
      _builder.append("\t\t");
      _builder.append("System.out.println(arg0)");
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
      _builder_1.append("public B1(final int arg0, final int arg1, final int... arg2) {");
      _builder_1.newLine();
      _builder_1.append("    ");
      _builder_1.append("System.out.println(arg0);");
      _builder_1.newLine();
      _builder_1.append("  ");
      _builder_1.append("}");
      _builder_1.newLine();
      _builder_1.append("  ");
      _builder_1.newLine();
      _builder_1.append("  ");
      _builder_1.append("public B1(final int arg0, final int... arg2) {");
      _builder_1.newLine();
      _builder_1.append("    ");
      _builder_1.append("this(arg0, 45, arg2);");
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
  public void inBehaviorConstructor_3p_vararg_0() {
    try {
      StringConcatenation _builder = new StringConcatenation();
      _builder.append("behavior B1 {");
      _builder.newLine();
      _builder.append("\t");
      _builder.append("new(arg0 : int=45, arg1 : int, arg2 : int*) {");
      _builder.newLine();
      _builder.append("\t\t");
      _builder.append("System.out.println(arg0)");
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
      _builder_1.append("public B1(final int arg0, final int arg1, final int... arg2) {");
      _builder_1.newLine();
      _builder_1.append("    ");
      _builder_1.append("System.out.println(arg0);");
      _builder_1.newLine();
      _builder_1.append("  ");
      _builder_1.append("}");
      _builder_1.newLine();
      _builder_1.append("  ");
      _builder_1.newLine();
      _builder_1.append("  ");
      _builder_1.append("public B1(final int arg1, final int... arg2) {");
      _builder_1.newLine();
      _builder_1.append("    ");
      _builder_1.append("this(45, arg1, arg2);");
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
  public void inBehaviorConstructor_3p_vararg_0_1() {
    try {
      StringConcatenation _builder = new StringConcatenation();
      _builder.append("behavior B1 {");
      _builder.newLine();
      _builder.append("\t");
      _builder.append("new(arg0 : int=45, arg1 : int=56, arg2 : int*) {");
      _builder.newLine();
      _builder.append("\t\t");
      _builder.append("System.out.println(arg0)");
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
      _builder_1.append("public B1(final int arg0, final int arg1, final int... arg2) {");
      _builder_1.newLine();
      _builder_1.append("    ");
      _builder_1.append("System.out.println(arg0);");
      _builder_1.newLine();
      _builder_1.append("  ");
      _builder_1.append("}");
      _builder_1.newLine();
      _builder_1.append("  ");
      _builder_1.newLine();
      _builder_1.append("  ");
      _builder_1.append("public B1(final int... arg2) {");
      _builder_1.newLine();
      _builder_1.append("    ");
      _builder_1.append("this(45, 56, arg2);");
      _builder_1.newLine();
      _builder_1.append("  ");
      _builder_1.append("}");
      _builder_1.newLine();
      _builder_1.append("  ");
      _builder_1.newLine();
      _builder_1.append("  ");
      _builder_1.append("public B1(final int arg0, final int... arg2) {");
      _builder_1.newLine();
      _builder_1.append("    ");
      _builder_1.append("this(arg0, 56, arg2);");
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
  public void inBehaviorConstructor_4p_0_1_2_3() {
    try {
      StringConcatenation _builder = new StringConcatenation();
      _builder.append("behavior B1 {");
      _builder.newLine();
      _builder.append("\t");
      _builder.append("new(arg0 : int=45, arg1 : int=56, arg2 : int=78, arg3 : int=14) {");
      _builder.newLine();
      _builder.append("\t\t");
      _builder.append("System.out.println(arg0)");
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
      _builder_1.append("public B1(final int arg0, final int arg1, final int arg2, final int arg3) {");
      _builder_1.newLine();
      _builder_1.append("    ");
      _builder_1.append("System.out.println(arg0);");
      _builder_1.newLine();
      _builder_1.append("  ");
      _builder_1.append("}");
      _builder_1.newLine();
      _builder_1.append("  ");
      _builder_1.newLine();
      _builder_1.append("  ");
      _builder_1.append("public B1() {");
      _builder_1.newLine();
      _builder_1.append("    ");
      _builder_1.append("this(45, 56, 78, 14);");
      _builder_1.newLine();
      _builder_1.append("  ");
      _builder_1.append("}");
      _builder_1.newLine();
      _builder_1.append("  ");
      _builder_1.newLine();
      _builder_1.append("  ");
      _builder_1.append("public B1(final int arg0) {");
      _builder_1.newLine();
      _builder_1.append("    ");
      _builder_1.append("this(arg0, 56, 78, 14);");
      _builder_1.newLine();
      _builder_1.append("  ");
      _builder_1.append("}");
      _builder_1.newLine();
      _builder_1.append("  ");
      _builder_1.newLine();
      _builder_1.append("  ");
      _builder_1.append("public B1(final int arg0, final int arg1) {");
      _builder_1.newLine();
      _builder_1.append("    ");
      _builder_1.append("this(arg0, arg1, 78, 14);");
      _builder_1.newLine();
      _builder_1.append("  ");
      _builder_1.append("}");
      _builder_1.newLine();
      _builder_1.append("  ");
      _builder_1.newLine();
      _builder_1.append("  ");
      _builder_1.append("public B1(final int arg0, final int arg1, final int arg2) {");
      _builder_1.newLine();
      _builder_1.append("    ");
      _builder_1.append("this(arg0, arg1, arg2, 14);");
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
}
