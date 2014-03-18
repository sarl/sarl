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

import com.google.common.collect.Iterables;
import com.google.inject.Inject;
import io.sarl.lang.SARLInjectorProvider;
import io.sarl.lang.sarl.AbstractElement;
import io.sarl.lang.sarl.Agent;
import io.sarl.lang.sarl.Capacity;
import io.sarl.lang.sarl.Model;
import org.eclipse.emf.common.util.EList;
import org.eclipse.xtend2.lib.StringConcatenation;
import org.eclipse.xtext.junit4.InjectWith;
import org.eclipse.xtext.junit4.XtextRunner;
import org.eclipse.xtext.junit4.util.ParseHelper;
import org.eclipse.xtext.junit4.validation.ValidationTestHelper;
import org.eclipse.xtext.naming.IQualifiedNameProvider;
import org.eclipse.xtext.xbase.lib.Exceptions;
import org.eclipse.xtext.xbase.lib.Extension;
import org.eclipse.xtext.xbase.lib.IterableExtensions;
import org.junit.Assert;
import org.junit.Before;
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
public class CapacityUtilsTest {
  @Inject
  @Extension
  private ParseHelper<Model> _parseHelper;
  
  @Inject
  @Extension
  private ValidationTestHelper _validationTestHelper;
  
  private Model mas;
  
  private Iterable<Capacity> knownCapacities;
  
  @Inject
  @Extension
  private IQualifiedNameProvider _iQualifiedNameProvider;
  
  @Before
  public void setUp() {
    try {
      StringConcatenation _builder = new StringConcatenation();
      _builder.append("package test");
      _builder.newLine();
      _builder.append("capacity C {");
      _builder.newLine();
      _builder.append("\t");
      _builder.append("def op1");
      _builder.newLine();
      _builder.append("}");
      _builder.newLine();
      _builder.append("capacity C2 {");
      _builder.newLine();
      _builder.append("\t");
      _builder.append("def op2");
      _builder.newLine();
      _builder.append("}");
      _builder.newLine();
      _builder.append("capacity C3 {");
      _builder.newLine();
      _builder.append("\t");
      _builder.append("def op3");
      _builder.newLine();
      _builder.append("}");
      _builder.newLine();
      _builder.append("capacity C4 {");
      _builder.newLine();
      _builder.append("\t");
      _builder.append("def op4");
      _builder.newLine();
      _builder.append("}");
      _builder.newLine();
      _builder.newLine();
      _builder.append("skill S implements C {");
      _builder.newLine();
      _builder.append("\t");
      _builder.append("def op1 {");
      _builder.newLine();
      _builder.append("\t\t");
      _builder.append("//do Something");
      _builder.newLine();
      _builder.append("\t");
      _builder.append("}");
      _builder.newLine();
      _builder.append("}");
      _builder.newLine();
      _builder.append("skill S2 implements C2 {");
      _builder.newLine();
      _builder.append("\t");
      _builder.append("def op1 {");
      _builder.newLine();
      _builder.append("\t\t");
      _builder.append("//do Something");
      _builder.newLine();
      _builder.append("\t");
      _builder.append("}");
      _builder.newLine();
      _builder.append("}");
      _builder.newLine();
      _builder.append("event E {}");
      _builder.newLine();
      _builder.append("agent A {");
      _builder.newLine();
      _builder.append("\t");
      _builder.append("on E {");
      _builder.newLine();
      _builder.append("\t\t");
      _builder.append("val shouldChange = true");
      _builder.newLine();
      _builder.append("\t\t");
      _builder.append("if (shouldChange) {");
      _builder.newLine();
      _builder.append("\t\t");
      _builder.append("C.setSkill(S)\t");
      _builder.newLine();
      _builder.append("\t\t");
      _builder.append("}else {");
      _builder.newLine();
      _builder.append("\t\t");
      _builder.newLine();
      _builder.append("\t\t ");
      _builder.append("while(true) {");
      _builder.newLine();
      _builder.append("\t\t ");
      _builder.append("C3.op3 \t");
      _builder.newLine();
      _builder.append("\t\t ");
      _builder.append("}");
      _builder.newLine();
      _builder.append("\t\t ");
      _builder.append("C3.read ");
      _builder.newLine();
      _builder.append("\t\t");
      _builder.newLine();
      _builder.append("\t\t");
      _builder.append("}");
      _builder.newLine();
      _builder.append("\t\t");
      _builder.newLine();
      _builder.append("\t");
      _builder.append("}");
      _builder.newLine();
      _builder.append("\t");
      _builder.newLine();
      _builder.append("\t");
      _builder.append("on E {");
      _builder.newLine();
      _builder.append("\t\t");
      _builder.append("C2.op1");
      _builder.newLine();
      _builder.append("\t\t");
      _builder.newLine();
      _builder.append("\t");
      _builder.append("}");
      _builder.newLine();
      _builder.append("}");
      _builder.newLine();
      _builder.append("agent B {");
      _builder.newLine();
      _builder.append("\t");
      _builder.append("on E {");
      _builder.newLine();
      _builder.append("\t\t");
      _builder.append("doSomething(C3)");
      _builder.newLine();
      _builder.append("\t\t");
      _builder.append("C4.op4");
      _builder.newLine();
      _builder.append("\t");
      _builder.append("}");
      _builder.newLine();
      _builder.append("}");
      _builder.newLine();
      Model _parse = this._parseHelper.parse(_builder);
      this.mas = _parse;
      EList<AbstractElement> _elements = this.mas.getElements();
      Iterable<Capacity> _filter = Iterables.<Capacity>filter(_elements, Capacity.class);
      this.knownCapacities = _filter;
    } catch (Throwable _e) {
      throw Exceptions.sneakyThrow(_e);
    }
  }
  
  @Test
  public void testParsedElements() {
    EList<AbstractElement> _elements = this.mas.getElements();
    int _size = _elements.size();
    Assert.assertEquals(9, _size);
  }
  
  public void testErrorFreeExampleCode() {
    this._validationTestHelper.assertNoErrors(this.mas);
  }
  
  @Test
  public void testAgentFind() {
    EList<AbstractElement> _elements = this.mas.getElements();
    final Iterable<Agent> agents = Iterables.<Agent>filter(_elements, Agent.class);
    int _size = IterableExtensions.size(agents);
    Assert.assertEquals(2, _size);
    Agent _head = IterableExtensions.<Agent>head(agents);
    String _name = _head.getName();
    Assert.assertEquals("A", _name);
  }
  
  @Test
  public void testFindCapacityReferences() {
    EList<AbstractElement> _elements = this.mas.getElements();
    final Iterable<Agent> agents = Iterables.<Agent>filter(_elements, Agent.class);
    int _size = IterableExtensions.size(agents);
    Assert.assertEquals(2, _size);
  }
}
