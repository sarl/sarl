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
package io.sarl.lang.tests.bugs;

import com.google.inject.Inject;
import io.sarl.lang.SARLInjectorProvider;
import io.sarl.lang.sarl.SarlScript;
import org.eclipse.xtend2.lib.StringConcatenation;
import org.eclipse.xtext.junit4.InjectWith;
import org.eclipse.xtext.junit4.XtextRunner;
import org.eclipse.xtext.junit4.util.ParseHelper;
import org.eclipse.xtext.junit4.validation.ValidationTestHelper;
import org.eclipse.xtext.util.IAcceptor;
import org.eclipse.xtext.xbase.compiler.CompilationTestHelper;
import org.eclipse.xtext.xbase.lib.Exceptions;
import org.eclipse.xtext.xbase.lib.Extension;
import org.eclipse.xtext.xbase.lib.Functions.Function0;
import org.junit.Assert;
import org.junit.Test;
import org.junit.runner.RunWith;

/**
 * @author $Author: srodriguez$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 */
@RunWith(XtextRunner.class)
@InjectWith(SARLInjectorProvider.class)
@SuppressWarnings("all")
public class Bug23 {
  @Inject
  @Extension
  private ParseHelper<SarlScript> _parseHelper;
  
  @Inject
  @Extension
  private ValidationTestHelper _validationTestHelper;
  
  @Inject
  @Extension
  private CompilationTestHelper _compilationTestHelper;
  
  private final CharSequence snippet = new Function0<CharSequence>() {
    public CharSequence apply() {
      StringConcatenation _builder = new StringConcatenation();
      _builder.append("import java.util.UUID");
      _builder.newLine();
      _builder.append("event AgentSpawned {");
      _builder.newLine();
      _builder.append("\t");
      _builder.append("var agentID : UUID");
      _builder.newLine();
      _builder.append("}");
      _builder.newLine();
      _builder.newLine();
      _builder.append("event MyAgentSpawned extends AgentSpawned{");
      _builder.newLine();
      _builder.append("\t");
      _builder.append("var titi : UUID");
      _builder.newLine();
      _builder.append("}");
      _builder.newLine();
      _builder.newLine();
      _builder.append("agent MyAgent {");
      _builder.newLine();
      _builder.append("\t");
      _builder.append("on MyAgentSpawned {");
      _builder.newLine();
      _builder.append("\t\t");
      _builder.append("System.out.println(occurrence.titi)");
      _builder.newLine();
      _builder.append("\t\t");
      _builder.append("System.out.println(occurrence.agentID)");
      _builder.newLine();
      _builder.append("\t");
      _builder.append("}");
      _builder.newLine();
      _builder.append("}");
      _builder.newLine();
      return _builder;
    }
  }.apply();
  
  @Test
  public void bug23() {
    try {
      SarlScript mas = this._parseHelper.parse(this.snippet);
      this._validationTestHelper.assertNoErrors(mas);
    } catch (Throwable _e) {
      throw Exceptions.sneakyThrow(_e);
    }
  }
  
  @Test
  public void myAgentSpawnedCompile() {
    try {
      StringConcatenation _builder = new StringConcatenation();
      _builder.append("import java.util.UUID;");
      _builder.newLine();
      _builder.newLine();
      _builder.append("@SuppressWarnings(\"all\")");
      _builder.newLine();
      _builder.append("public class MyAgentSpawned extends AgentSpawned {");
      _builder.newLine();
      _builder.append("  ");
      _builder.append("public UUID titi;");
      _builder.newLine();
      _builder.append("  ");
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
      _builder.append("public MyAgentSpawned() {");
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
      _builder.append("public MyAgentSpawned(final io.sarl.lang.core.Address source) {");
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
      _builder.append("@Override");
      _builder.newLine();
      _builder.append("  ");
      _builder.append("public boolean equals(final Object obj) {");
      _builder.newLine();
      _builder.append("    ");
      _builder.append("if (this == obj)");
      _builder.newLine();
      _builder.append("      ");
      _builder.append("return true;");
      _builder.newLine();
      _builder.append("    ");
      _builder.append("if (obj == null)");
      _builder.newLine();
      _builder.append("      ");
      _builder.append("return false;");
      _builder.newLine();
      _builder.append("    ");
      _builder.append("if (getClass() != obj.getClass())");
      _builder.newLine();
      _builder.append("      ");
      _builder.append("return false;");
      _builder.newLine();
      _builder.append("    ");
      _builder.append("if (!super.equals(obj))");
      _builder.newLine();
      _builder.append("      ");
      _builder.append("return false;");
      _builder.newLine();
      _builder.append("    ");
      _builder.append("MyAgentSpawned other = (MyAgentSpawned) obj;");
      _builder.newLine();
      _builder.append("    ");
      _builder.append("if (this.titi == null) {");
      _builder.newLine();
      _builder.append("      ");
      _builder.append("if (other.titi != null)");
      _builder.newLine();
      _builder.append("        ");
      _builder.append("return false;");
      _builder.newLine();
      _builder.append("    ");
      _builder.append("} else if (!this.titi.equals(other.titi))");
      _builder.newLine();
      _builder.append("      ");
      _builder.append("return false;");
      _builder.newLine();
      _builder.append("    ");
      _builder.append("return true;");
      _builder.newLine();
      _builder.append("  ");
      _builder.append("}");
      _builder.newLine();
      _builder.append("  ");
      _builder.newLine();
      _builder.append("  ");
      _builder.append("@Override");
      _builder.newLine();
      _builder.append("  ");
      _builder.append("public int hashCode() {");
      _builder.newLine();
      _builder.append("    ");
      _builder.append("final int prime = 31;");
      _builder.newLine();
      _builder.append("    ");
      _builder.append("int result = super.hashCode();");
      _builder.newLine();
      _builder.append("    ");
      _builder.append("result = prime * result + ((this.titi== null) ? 0 : this.titi.hashCode());");
      _builder.newLine();
      _builder.append("    ");
      _builder.append("return result;");
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
      _builder.append("* Returns a String representation of the Event MyAgentSpawned attributes only.");
      _builder.newLine();
      _builder.append("   ");
      _builder.append("*/");
      _builder.newLine();
      _builder.append("  ");
      _builder.append("protected String attributesToString() {");
      _builder.newLine();
      _builder.append("    ");
      _builder.append("StringBuilder result = new StringBuilder(super.attributesToString());");
      _builder.newLine();
      _builder.append("    ");
      _builder.append("result.append(\"titi  = \").append(this.titi);");
      _builder.newLine();
      _builder.append("    ");
      _builder.append("return result.toString();");
      _builder.newLine();
      _builder.append("  ");
      _builder.append("}");
      _builder.newLine();
      _builder.append("  ");
      _builder.newLine();
      _builder.append("  ");
      _builder.append("private final static long serialVersionUID = -267285920L;");
      _builder.newLine();
      _builder.append("}");
      _builder.newLine();
      final String expectedMyAgentSpawned = _builder.toString();
      final IAcceptor<CompilationTestHelper.Result> _function = new IAcceptor<CompilationTestHelper.Result>() {
        public void accept(final CompilationTestHelper.Result r) {
          String _generatedCode = r.getGeneratedCode("MyAgentSpawned");
          Assert.assertEquals(expectedMyAgentSpawned, _generatedCode);
        }
      };
      this._compilationTestHelper.compile(this.snippet, _function);
    } catch (Throwable _e) {
      throw Exceptions.sneakyThrow(_e);
    }
  }
}
