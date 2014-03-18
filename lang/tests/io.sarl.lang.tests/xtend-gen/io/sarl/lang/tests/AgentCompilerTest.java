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
package io.sarl.lang.tests;

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
 * @author $Author: Sebastian Rodriguez$
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
      _builder_1.append("}");
      _builder_1.newLine();
      this._compilationTestHelper.assertCompilesTo(_builder, _builder_1);
    } catch (Throwable _e) {
      throw Exceptions.sneakyThrow(_e);
    }
  }
}
