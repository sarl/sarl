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
package io.sarl.docs.gettingstarted;

import com.google.common.collect.Iterables;
import com.google.inject.Inject;
import io.sarl.docs.gettingstarted.AgentDefinitionIntroductionAgentPerceptionsSpec;
import io.sarl.docs.utils.SARLParser;
import io.sarl.docs.utils.SARLSpecCreator;
import io.sarl.lang.sarl.Agent;
import io.sarl.lang.sarl.SarlScript;
import io.sarl.lang.sarl.TopElement;
import org.eclipse.emf.common.util.EList;
import org.eclipse.xtend2.lib.StringConcatenation;
import org.eclipse.xtext.naming.IQualifiedNameProvider;
import org.eclipse.xtext.naming.QualifiedName;
import org.eclipse.xtext.xbase.lib.Extension;
import org.eclipse.xtext.xbase.lib.IterableExtensions;
import org.jnario.runner.Contains;
import org.jnario.runner.CreateWith;
import org.jnario.runner.ExampleGroupRunner;
import org.jnario.runner.Named;
import org.jnario.runner.Order;
import org.junit.Assert;
import org.junit.Test;
import org.junit.runner.RunWith;

/**
 * Once you have installed SARL SDK in eclipse you can create a new Maven project and
 * reuse the pom found on the demos.
 * 
 * To create our first agent, right click on the project and follow **New** > **File**.
 * Name the file **demosarl.sarl**.
 * 
 * The SARL default editor will open.
 */
@Contains(AgentDefinitionIntroductionAgentPerceptionsSpec.class)
@Named("Agent Definition Introduction")
@RunWith(ExampleGroupRunner.class)
@CreateWith(SARLSpecCreator.class)
@SuppressWarnings("all")
public class AgentDefinitionIntroductionSpec {
  @Inject
  @Extension
  @org.jnario.runner.Extension
  public SARLParser _sARLParser;
  
  @Inject
  @Extension
  @org.jnario.runner.Extension
  public IQualifiedNameProvider _iQualifiedNameProvider;
  
  /**
   * Agents are defined using the `agent` keyword.
   * 
   * SARL elements are organized in packages.
   * You can define the package using the <code>package</code> keyword.
   * 
   * The following code will define an agent with a fully qualified name of `myapp.demo.MyAgent`.
   * 
   * 
   * 
   * @filter(.* = '''|'''|.parsesSuccessfully.*)
   */
  @Test
  @Named("Basic agent definition")
  @Order(1)
  public void _basicAgentDefinition() throws Exception {
    StringConcatenation _builder = new StringConcatenation();
    _builder.append("package myapp.demo");
    _builder.newLine();
    _builder.newLine();
    _builder.append("agent MyAgent {}");
    _builder.newLine();
    final SarlScript model = this._sARLParser.parsesSuccessfully(_builder);
    EList<TopElement> _elements = model.getElements();
    Iterable<Agent> _filter = Iterables.<Agent>filter(_elements, Agent.class);
    Agent _head = IterableExtensions.<Agent>head(_filter);
    QualifiedName _fullyQualifiedName = this._iQualifiedNameProvider.getFullyQualifiedName(_head);
    String _string = _fullyQualifiedName.toString();
    Assert.assertEquals("myapp.demo.MyAgent", _string);
  }
  
  /**
   * <span class="label label-info">Important</span> The package keyword defines the package
   * for all elements in the same SARL file.
   * 
   * Therefore FirstAgent and SecondAgent belong to the same package (i.e. `myapp.demo`).
   * 
   * @filter(.* = '''|'''|.parsesSuccessfully.*)
   */
  @Test
  @Named("Package definition")
  @Order(2)
  public void _packageDefinition() throws Exception {
    StringConcatenation _builder = new StringConcatenation();
    _builder.append("package myapp.demo");
    _builder.newLine();
    _builder.append("agent MyAgent {}");
    _builder.newLine();
    _builder.append("agent SecondAgent {}");
    _builder.newLine();
    final SarlScript model = this._sARLParser.parsesSuccessfully(_builder);
    EList<TopElement> _elements = model.getElements();
    Iterable<Agent> _filter = Iterables.<Agent>filter(_elements, Agent.class);
    Agent _head = IterableExtensions.<Agent>head(_filter);
    QualifiedName _fullyQualifiedName = this._iQualifiedNameProvider.getFullyQualifiedName(_head);
    String _string = _fullyQualifiedName.toString();
    Assert.assertEquals("myapp.demo.MyAgent", _string);
    EList<TopElement> _elements_1 = model.getElements();
    Iterable<Agent> _filter_1 = Iterables.<Agent>filter(_elements_1, Agent.class);
    Agent _last = IterableExtensions.<Agent>last(_filter_1);
    QualifiedName _fullyQualifiedName_1 = this._iQualifiedNameProvider.getFullyQualifiedName(_last);
    String _string_1 = _fullyQualifiedName_1.toString();
    Assert.assertEquals("myapp.demo.SecondAgent", _string_1);
  }
  
  /**
   * In the next section we will learn how to start a SARL agent.
   * 
   * 
   * **[Next](RunningSARLSpec.html)**.
   * @filter(.*)
   */
  @Test
  @Named("What\\\'s next?")
  @Order(3)
  public void _whatSNext() throws Exception {
    Assert.assertTrue(true);
  }
}
