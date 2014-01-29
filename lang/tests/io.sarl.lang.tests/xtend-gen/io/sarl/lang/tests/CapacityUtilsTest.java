/**
 * $Id$
 * 
 * SARL is an open-source multiagent language.
 * More details on &lt;http://www.sarl.io&gt;
 * Copyright (C) 2014 SARL Core Developers
 * 
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 * 
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 * 
 * You should have received a copy of the GNU General Public License
 * along with this program.  If not, see &lt;http://www.gnu.org/licenses/&gt;.
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
 * @author $Author: Sebastian Rodriguez$
 * @version $Name$ $Revision$ $Date$
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
