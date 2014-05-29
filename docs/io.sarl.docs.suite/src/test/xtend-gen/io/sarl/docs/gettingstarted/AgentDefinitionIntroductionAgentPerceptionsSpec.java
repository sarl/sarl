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

import io.sarl.docs.gettingstarted.AgentDefinitionIntroductionSpec;
import io.sarl.lang.sarl.SarlScript;
import org.eclipse.xtend2.lib.StringConcatenation;
import org.jnario.runner.ExampleGroupRunner;
import org.jnario.runner.Named;
import org.jnario.runner.Order;
import org.junit.Test;
import org.junit.runner.RunWith;

/**
 * Agents need to perceive their environment in order to react to external stimuli.
 * Perceptions take the form of events.
 */
@Named("Agent Perceptions")
@RunWith(ExampleGroupRunner.class)
@SuppressWarnings("all")
public class AgentDefinitionIntroductionAgentPerceptionsSpec extends AgentDefinitionIntroductionSpec {
  /**
   * To declare a new event use the `event` keyword.
   * The following code defines a new event `MyEvent`
   * @filter('''|.parsesSuccessfully)
   */
  @Test
  @Named("Declare an Event")
  @Order(1)
  public void _declareAnEvent() throws Exception {
    StringConcatenation _builder = new StringConcatenation();
    _builder.append("event MyEvent {}");
    _builder.newLine();
    this._sARLParser.parsesSuccessfully(_builder);
  }
  
  /**
   * Now we will want our agent to react to `MyEvent` and print a message on the console
   * 
   * @filter(.* = '''|'''|.parsesSuccessfully.*)
   */
  @Test
  @Named("Define an agent Perceptions")
  @Order(2)
  public void _defineAnAgentPerceptions() throws Exception {
    StringConcatenation _builder = new StringConcatenation();
    _builder.append("package myapp.demo");
    _builder.newLine();
    _builder.newLine();
    _builder.append("event MyEvent");
    _builder.newLine();
    _builder.newLine();
    _builder.append("agent MyAgent {");
    _builder.newLine();
    _builder.append("\t");
    _builder.append("on MyEvent {");
    _builder.newLine();
    _builder.append("\t\t");
    _builder.append("System.out.println(\"Received MyEvent\")");
    _builder.newLine();
    _builder.append("\t");
    _builder.append("}");
    _builder.newLine();
    _builder.append("} ");
    _builder.newLine();
    final SarlScript model = this._sARLParser.parsesSuccessfully(_builder);
  }
  
  /**
   * SARL defines two **lifecycle** events :
   * 
   *  * `Initialize`:  Notifies the creation of the agent and passes it initialization parameters.
   *  * `Destroy`: Notifies the Destruction of the agent.
   * 
   * This means that when agent has been spawned and its ready to begin its execution, it will receive an `Initialize` event.
   * You can react to this event just like with any other event defined in SARL.
   * 
   * Likewise, when the agent is going to stop its execution (we will see how to stop an agent later on), it will
   * receive a `Destroy` Event. The purpose of this event is to release any System resources properly.
   * 
   * 
   * @filter(.* = '''|'''|.parsesSuccessfully.*)
   */
  @Test
  @Named("Lifecycle events")
  @Order(3)
  public void _lifecycleEvents() throws Exception {
    StringConcatenation _builder = new StringConcatenation();
    _builder.append("package myapp.demo");
    _builder.newLine();
    _builder.newLine();
    _builder.append("import io.sarl.core.Initialize");
    _builder.newLine();
    _builder.append("import io.sarl.core.Destroy");
    _builder.newLine();
    _builder.newLine();
    _builder.append("agent MyAgent {");
    _builder.newLine();
    _builder.append("\t");
    _builder.newLine();
    _builder.append("\t");
    _builder.append("on Initialize {");
    _builder.newLine();
    _builder.append("\t\t");
    _builder.append("System.out.println(\"MyAgent spawned\")");
    _builder.newLine();
    _builder.append("\t");
    _builder.append("}");
    _builder.newLine();
    _builder.append("\t");
    _builder.newLine();
    _builder.append("\t");
    _builder.append("on Destroy {");
    _builder.newLine();
    _builder.append("\t\t");
    _builder.append("System.out.println(\"MyAgent destroyed\")");
    _builder.newLine();
    _builder.append("\t");
    _builder.append("}");
    _builder.newLine();
    _builder.append("}");
    _builder.newLine();
    final SarlScript model = this._sARLParser.parsesSuccessfully(_builder);
  }
  
  /**
   * Inside a behavior declaration you may need to access the event
   * instance the agent is reacting to.
   * 
   * This instance is called an `occurrence`.
   * 
   * In the case of an Initialize events you can access the parameters
   * for the agent spawn using `occurrence.parameters`
   * 
   * @filter(.* = '''|'''|.parsesSuccessfully.*)
   */
  @Test
  @Named("Accessing the event\\\'s occurrence")
  @Order(4)
  public void _accessingTheEventSOccurrence() throws Exception {
    StringConcatenation _builder = new StringConcatenation();
    _builder.append("package myapp.demo");
    _builder.newLine();
    _builder.newLine();
    _builder.append("import io.sarl.core.Initialize");
    _builder.newLine();
    _builder.append("import io.sarl.core.Destroy");
    _builder.newLine();
    _builder.newLine();
    _builder.append("agent MyAgent {");
    _builder.newLine();
    _builder.append("\t");
    _builder.newLine();
    _builder.append("\t");
    _builder.append("on Initialize {");
    _builder.newLine();
    _builder.append("\t\t");
    _builder.append("System.out.println(\"MyAgent spawned\")");
    _builder.newLine();
    _builder.append("\t\t");
    _builder.append("System.out.println(\"My Parameters are :\" + occurrence.parameters.toString)");
    _builder.newLine();
    _builder.append("\t");
    _builder.append("}");
    _builder.newLine();
    _builder.append("\t");
    _builder.newLine();
    _builder.append("\t");
    _builder.append("on Destroy {");
    _builder.newLine();
    _builder.append("\t\t");
    _builder.append("System.out.println(\"MyAgent destroyed\")");
    _builder.newLine();
    _builder.append("\t");
    _builder.append("}");
    _builder.newLine();
    _builder.append("}");
    _builder.newLine();
    final SarlScript model = this._sARLParser.parsesSuccessfully(_builder);
  }
}
