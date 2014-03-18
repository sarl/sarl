package io.sarl.lang.tests.bugs;

import com.google.inject.Inject;
import io.sarl.lang.SARLInjectorProvider;
import io.sarl.lang.sarl.Model;
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
  private ParseHelper<Model> _parseHelper;
  
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
      _builder.append("occurrence.titi");
      _builder.newLine();
      _builder.append("\t\t");
      _builder.append("occurrence.agentID");
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
      Model mas = this._parseHelper.parse(this.snippet);
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
      _builder.append("private UUID titi;");
      _builder.newLine();
      _builder.append("  ");
      _builder.newLine();
      _builder.append("  ");
      _builder.append("public UUID getTiti() {");
      _builder.newLine();
      _builder.append("    ");
      _builder.append("return this.titi;");
      _builder.newLine();
      _builder.append("  ");
      _builder.append("}");
      _builder.newLine();
      _builder.append("  ");
      _builder.newLine();
      _builder.append("  ");
      _builder.append("public void setTiti(final UUID titi) {");
      _builder.newLine();
      _builder.append("    ");
      _builder.append("this.titi = titi;");
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
      _builder.append("StringBuilder result = new StringBuilder();");
      _builder.newLine();
      _builder.append("    ");
      _builder.append("result.append(super.attributesToString());");
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
      _builder.append("/**");
      _builder.newLine();
      _builder.append("   ");
      _builder.append("* Returns a String representation of the Event MyAgentSpawned.");
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
      _builder.append("result.append(\"MyAgentSpawned[\");");
      _builder.newLine();
      _builder.append("    ");
      _builder.append("result.append(this.attributesToString());");
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
