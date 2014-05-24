/**
 * Copyright 2014 Sebastian RODRIGUEZ, Nicolas GAUD, Stéphane GALLAND
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
package io.sarl.docs.reference;

import com.google.common.collect.Iterables;
import com.google.inject.Inject;
import io.sarl.docs.utils.SARLParser;
import io.sarl.docs.utils.SARLSpecCreator;
import io.sarl.lang.sarl.Attribute;
import io.sarl.lang.sarl.Event;
import io.sarl.lang.sarl.SarlScript;
import io.sarl.lang.sarl.TopElement;
import org.eclipse.emf.common.util.EList;
import org.eclipse.emf.ecore.EObject;
import org.eclipse.xtend2.lib.StringConcatenation;
import org.eclipse.xtext.common.types.JvmTypeReference;
import org.eclipse.xtext.naming.IQualifiedNameProvider;
import org.eclipse.xtext.naming.QualifiedName;
import org.eclipse.xtext.xbase.lib.Extension;
import org.eclipse.xtext.xbase.lib.IterableExtensions;
import org.jnario.runner.CreateWith;
import org.jnario.runner.ExampleGroupRunner;
import org.jnario.runner.Named;
import org.jnario.runner.Order;
import org.junit.Assert;
import org.junit.Test;
import org.junit.runner.RunWith;

/**
 * This document defines how to declare events in SARL.
 */
@Named("Event Reference")
@RunWith(ExampleGroupRunner.class)
@CreateWith(SARLSpecCreator.class)
@SuppressWarnings("all")
public class EventReferenceSpec {
  @Inject
  @Extension
  @org.jnario.runner.Extension
  public SARLParser _sARLParser;
  
  @Inject
  @Extension
  @org.jnario.runner.Extension
  public IQualifiedNameProvider _iQualifiedNameProvider;
  
  /**
   * @filter(.* = '''|'''|.parsesSuccessfully.*)
   */
  @Test
  @Named("Define an event")
  @Order(1)
  public void _defineAnEvent() throws Exception {
    StringConcatenation _builder = new StringConcatenation();
    _builder.append("package events");
    _builder.newLine();
    _builder.append("event MyEvent {");
    _builder.newLine();
    _builder.append("}");
    _builder.newLine();
    final SarlScript model = this._sARLParser.parsesSuccessfully(_builder);
    EList<TopElement> _elements = model.getElements();
    Iterable<Event> _filter = Iterables.<Event>filter(_elements, Event.class);
    final Event evt = IterableExtensions.<Event>head(_filter);
    QualifiedName _fullyQualifiedName = this._iQualifiedNameProvider.getFullyQualifiedName(evt);
    String _string = _fullyQualifiedName.toString();
    Assert.assertEquals("events.MyEvent", _string);
  }
  
  /**
   * Events in SARL can carry information
   * 
   * @filter(.* = '''|'''|.parsesSuccessfully.*)
   */
  @Test
  @Named("Declare event with attributes")
  @Order(2)
  public void _declareEventWithAttributes() throws Exception {
    StringConcatenation _builder = new StringConcatenation();
    _builder.append("package myapp.demo");
    _builder.newLine();
    _builder.append("event MyEvent {");
    _builder.newLine();
    _builder.append("\t");
    _builder.append("var number : Integer");
    _builder.newLine();
    _builder.append("}");
    _builder.newLine();
    final SarlScript model = this._sARLParser.parsesSuccessfully(_builder);
    EList<TopElement> _elements = model.getElements();
    Iterable<Event> _filter = Iterables.<Event>filter(_elements, Event.class);
    final Event evt = IterableExtensions.<Event>head(_filter);
    EList<EObject> _features = evt.getFeatures();
    int _size = _features.size();
    Assert.assertEquals(1, _size);
    EList<EObject> _features_1 = evt.getFeatures();
    Iterable<Attribute> _filter_1 = Iterables.<Attribute>filter(_features_1, Attribute.class);
    final Attribute att = IterableExtensions.<Attribute>head(_filter_1);
    String _name = att.getName();
    Assert.assertEquals("number", _name);
    JvmTypeReference _type = att.getType();
    String _identifier = _type.getIdentifier();
    Assert.assertEquals("java.lang.Integer", _identifier);
  }
  
  /**
   * Events in SARL can carry information that is unmodifiable using `val` d
   * 
   * @filter(.* = '''|'''|.parsesSuccessfully.*)
   */
  @Test
  @Named("Declare event with value attributes")
  @Order(3)
  public void _declareEventWithValueAttributes() throws Exception {
    StringConcatenation _builder = new StringConcatenation();
    _builder.append("package myapp.demo");
    _builder.newLine();
    _builder.append("import io.sarl.lang.core.Agent");
    _builder.newLine();
    _builder.append("event MyEvent {");
    _builder.newLine();
    _builder.append("\t");
    _builder.append("val number : Integer");
    _builder.newLine();
    _builder.append("\t");
    _builder.newLine();
    _builder.append("\t");
    _builder.append("new(nb:Integer){");
    _builder.newLine();
    _builder.append("\t\t");
    _builder.append("number = nb");
    _builder.newLine();
    _builder.append("\t");
    _builder.append("}");
    _builder.newLine();
    _builder.append("}");
    _builder.newLine();
    final SarlScript model = this._sARLParser.parsesSuccessfully(_builder);
    EList<TopElement> _elements = model.getElements();
    Iterable<Event> _filter = Iterables.<Event>filter(_elements, Event.class);
    final Event evt = IterableExtensions.<Event>head(_filter);
    EList<EObject> _features = evt.getFeatures();
    int _size = _features.size();
    Assert.assertEquals(2, _size);
    EList<EObject> _features_1 = evt.getFeatures();
    Iterable<Attribute> _filter_1 = Iterables.<Attribute>filter(_features_1, Attribute.class);
    final Attribute att = IterableExtensions.<Attribute>head(_filter_1);
    String _name = att.getName();
    Assert.assertEquals("number", _name);
    JvmTypeReference _type = att.getType();
    String _identifier = _type.getIdentifier();
    Assert.assertEquals("java.lang.Integer", _identifier);
  }
}
