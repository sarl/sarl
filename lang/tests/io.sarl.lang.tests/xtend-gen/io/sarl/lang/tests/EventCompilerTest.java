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
import org.eclipse.xtext.util.IAcceptor;
import org.eclipse.xtext.xbase.compiler.CompilationTestHelper;
import org.eclipse.xtext.xbase.lib.Exceptions;
import org.eclipse.xtext.xbase.lib.Extension;
import org.junit.Assert;
import org.junit.Test;
import org.junit.runner.RunWith;

/**
 * @author $Author: srodriguez$
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 */
@RunWith(XtextRunner.class)
@InjectWith(SARLInjectorProvider.class)
@SuppressWarnings("all")
public class EventCompilerTest {
  @Inject
  @Extension
  private CompilationTestHelper _compilationTestHelper;
  
  @Test
  public void basicCompile() {
    try {
      StringConcatenation _builder = new StringConcatenation();
      _builder.append("event E1 {");
      _builder.newLine();
      _builder.append("\t");
      _builder.newLine();
      _builder.append("}");
      _builder.newLine();
      StringConcatenation _builder_1 = new StringConcatenation();
      _builder_1.append("import io.sarl.lang.core.Event;");
      _builder_1.newLine();
      _builder_1.newLine();
      _builder_1.append("@SuppressWarnings(\"all\")");
      _builder_1.newLine();
      _builder_1.append("public class E1 extends Event {");
      _builder_1.newLine();
      _builder_1.append("  ");
      _builder_1.append("/**");
      _builder_1.newLine();
      _builder_1.append("   ");
      _builder_1.append("* Returns a String representation of the Event E1.");
      _builder_1.newLine();
      _builder_1.append("   ");
      _builder_1.append("*/");
      _builder_1.newLine();
      _builder_1.append("  ");
      _builder_1.append("public String toString() {");
      _builder_1.newLine();
      _builder_1.append("    ");
      _builder_1.append("StringBuilder result = new StringBuilder();");
      _builder_1.newLine();
      _builder_1.append("    ");
      _builder_1.append("result.append(\"E1[\");");
      _builder_1.newLine();
      _builder_1.append("    ");
      _builder_1.append("result.append(attributesToString());");
      _builder_1.newLine();
      _builder_1.append("    ");
      _builder_1.append("result.append(\"]\");");
      _builder_1.newLine();
      _builder_1.append("    ");
      _builder_1.append("return result.toString();");
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
  public void withVarAttributesCompile() {
    try {
      StringConcatenation _builder = new StringConcatenation();
      _builder.append("event E1 {");
      _builder.newLine();
      _builder.append("\t");
      _builder.append("var name : String");
      _builder.newLine();
      _builder.append("}");
      _builder.newLine();
      StringConcatenation _builder_1 = new StringConcatenation();
      _builder_1.append("import io.sarl.lang.core.Event;");
      _builder_1.newLine();
      _builder_1.newLine();
      _builder_1.append("@SuppressWarnings(\"all\")");
      _builder_1.newLine();
      _builder_1.append("public class E1 extends Event {");
      _builder_1.newLine();
      _builder_1.append("  ");
      _builder_1.append("private String name;");
      _builder_1.newLine();
      _builder_1.append("  ");
      _builder_1.newLine();
      _builder_1.append("  ");
      _builder_1.append("public String getName() {");
      _builder_1.newLine();
      _builder_1.append("    ");
      _builder_1.append("return this.name;");
      _builder_1.newLine();
      _builder_1.append("  ");
      _builder_1.append("}");
      _builder_1.newLine();
      _builder_1.append("  ");
      _builder_1.newLine();
      _builder_1.append("  ");
      _builder_1.append("public void setName(final String name) {");
      _builder_1.newLine();
      _builder_1.append("    ");
      _builder_1.append("this.name = name;");
      _builder_1.newLine();
      _builder_1.append("  ");
      _builder_1.append("}");
      _builder_1.newLine();
      _builder_1.append("  ");
      _builder_1.newLine();
      _builder_1.append("  ");
      _builder_1.append("@Override");
      _builder_1.newLine();
      _builder_1.append("  ");
      _builder_1.append("public boolean equals(final Object obj) {");
      _builder_1.newLine();
      _builder_1.append("    ");
      _builder_1.append("if (this == obj)");
      _builder_1.newLine();
      _builder_1.append("      ");
      _builder_1.append("return true;");
      _builder_1.newLine();
      _builder_1.append("    ");
      _builder_1.append("if (obj == null)");
      _builder_1.newLine();
      _builder_1.append("      ");
      _builder_1.append("return false;");
      _builder_1.newLine();
      _builder_1.append("    ");
      _builder_1.append("if (getClass() != obj.getClass())");
      _builder_1.newLine();
      _builder_1.append("      ");
      _builder_1.append("return false;");
      _builder_1.newLine();
      _builder_1.append("    ");
      _builder_1.append("if (!super.equals(obj))");
      _builder_1.newLine();
      _builder_1.append("      ");
      _builder_1.append("return false;");
      _builder_1.newLine();
      _builder_1.append("    ");
      _builder_1.append("E1 other = (E1) obj;");
      _builder_1.newLine();
      _builder_1.append("    ");
      _builder_1.append("if (this.name == null) {");
      _builder_1.newLine();
      _builder_1.append("      ");
      _builder_1.append("if (other.name != null)");
      _builder_1.newLine();
      _builder_1.append("        ");
      _builder_1.append("return false;");
      _builder_1.newLine();
      _builder_1.append("    ");
      _builder_1.append("} else if (!this.name.equals(other.name))");
      _builder_1.newLine();
      _builder_1.append("      ");
      _builder_1.append("return false;");
      _builder_1.newLine();
      _builder_1.append("    ");
      _builder_1.append("return true;");
      _builder_1.newLine();
      _builder_1.append("  ");
      _builder_1.append("}");
      _builder_1.newLine();
      _builder_1.append("  ");
      _builder_1.newLine();
      _builder_1.append("  ");
      _builder_1.append("@Override");
      _builder_1.newLine();
      _builder_1.append("  ");
      _builder_1.append("public int hashCode() {");
      _builder_1.newLine();
      _builder_1.append("    ");
      _builder_1.append("final int prime = 31;");
      _builder_1.newLine();
      _builder_1.append("    ");
      _builder_1.append("int result = super.hashCode();");
      _builder_1.newLine();
      _builder_1.append("    ");
      _builder_1.append("result = prime * result + ((this.name== null) ? 0 : this.name.hashCode());");
      _builder_1.newLine();
      _builder_1.append("    ");
      _builder_1.append("return result;");
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
      _builder_1.append("* Returns a String representation of the Event E1 attributes only.");
      _builder_1.newLine();
      _builder_1.append("   ");
      _builder_1.append("*/");
      _builder_1.newLine();
      _builder_1.append("  ");
      _builder_1.append("protected String attributesToString() {");
      _builder_1.newLine();
      _builder_1.append("    ");
      _builder_1.append("StringBuilder result = new StringBuilder(super.attributesToString());");
      _builder_1.newLine();
      _builder_1.append("    ");
      _builder_1.append("result.append(\"name  = \").append(this.name);");
      _builder_1.newLine();
      _builder_1.append("    ");
      _builder_1.append("return result.toString();");
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
      _builder_1.append("* Returns a String representation of the Event E1.");
      _builder_1.newLine();
      _builder_1.append("   ");
      _builder_1.append("*/");
      _builder_1.newLine();
      _builder_1.append("  ");
      _builder_1.append("public String toString() {");
      _builder_1.newLine();
      _builder_1.append("    ");
      _builder_1.append("StringBuilder result = new StringBuilder();");
      _builder_1.newLine();
      _builder_1.append("    ");
      _builder_1.append("result.append(\"E1[\");");
      _builder_1.newLine();
      _builder_1.append("    ");
      _builder_1.append("result.append(attributesToString());");
      _builder_1.newLine();
      _builder_1.append("    ");
      _builder_1.append("result.append(\"]\");");
      _builder_1.newLine();
      _builder_1.append("    ");
      _builder_1.append("return result.toString();");
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
  public void inheritanceCompile() {
    try {
      StringConcatenation _builder = new StringConcatenation();
      _builder.append("@SuppressWarnings(\"all\")");
      _builder.newLine();
      _builder.append("public class E2 extends E1 {");
      _builder.newLine();
      _builder.append("  ");
      _builder.append("/**");
      _builder.newLine();
      _builder.append("   ");
      _builder.append("* Returns a String representation of the Event E2.");
      _builder.newLine();
      _builder.append("   ");
      _builder.append("*/");
      _builder.newLine();
      _builder.append("  ");
      _builder.append("public String toString() {");
      _builder.newLine();
      _builder.append("    ");
      _builder.append("StringBuilder result = new StringBuilder();");
      _builder.newLine();
      _builder.append("    ");
      _builder.append("result.append(\"E2[\");");
      _builder.newLine();
      _builder.append("    ");
      _builder.append("result.append(attributesToString());");
      _builder.newLine();
      _builder.append("    ");
      _builder.append("result.append(\"]\");");
      _builder.newLine();
      _builder.append("    ");
      _builder.append("return result.toString();");
      _builder.newLine();
      _builder.append("  ");
      _builder.append("}");
      _builder.newLine();
      _builder.append("}");
      _builder.newLine();
      final String expectedE2 = _builder.toString();
      StringConcatenation _builder_1 = new StringConcatenation();
      _builder_1.append("event E1 {");
      _builder_1.newLine();
      _builder_1.append("\t");
      _builder_1.append("var name : String");
      _builder_1.newLine();
      _builder_1.append("}");
      _builder_1.newLine();
      _builder_1.newLine();
      _builder_1.append("event E2 extends E1{");
      _builder_1.newLine();
      _builder_1.append("\t");
      _builder_1.newLine();
      _builder_1.append("}");
      _builder_1.newLine();
      final IAcceptor<CompilationTestHelper.Result> _function = new IAcceptor<CompilationTestHelper.Result>() {
        public void accept(final CompilationTestHelper.Result r) {
          String _generatedCode = r.getGeneratedCode("E2");
          Assert.assertEquals(expectedE2, _generatedCode);
        }
      };
      this._compilationTestHelper.compile(_builder_1, _function);
    } catch (Throwable _e) {
      throw Exceptions.sneakyThrow(_e);
    }
  }
}
