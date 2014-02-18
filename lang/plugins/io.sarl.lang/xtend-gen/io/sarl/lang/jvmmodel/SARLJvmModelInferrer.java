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
package io.sarl.lang.jvmmodel;

import com.google.common.base.Objects;
import com.google.common.collect.Iterables;
import com.google.inject.Inject;
import io.sarl.lang.core.Percept;
import io.sarl.lang.sarl.AbstractElement;
import io.sarl.lang.sarl.Action;
import io.sarl.lang.sarl.ActionSignature;
import io.sarl.lang.sarl.Agent;
import io.sarl.lang.sarl.AgentFeature;
import io.sarl.lang.sarl.Attribute;
import io.sarl.lang.sarl.Behavior;
import io.sarl.lang.sarl.BehaviorFeature;
import io.sarl.lang.sarl.BehaviorUnit;
import io.sarl.lang.sarl.Capacity;
import io.sarl.lang.sarl.CapacityUses;
import io.sarl.lang.sarl.Constructor;
import io.sarl.lang.sarl.Event;
import io.sarl.lang.sarl.EventFeature;
import io.sarl.lang.sarl.Parameter;
import io.sarl.lang.sarl.RequiredCapacity;
import io.sarl.lang.sarl.Skill;
import io.sarl.lang.sarl.SkillFeature;
import java.util.Arrays;
import java.util.UUID;
import java.util.logging.Logger;
import org.eclipse.emf.common.util.EList;
import org.eclipse.emf.ecore.EObject;
import org.eclipse.xtend2.lib.StringConcatenation;
import org.eclipse.xtend2.lib.StringConcatenationClient;
import org.eclipse.xtext.common.types.JvmAnnotationReference;
import org.eclipse.xtext.common.types.JvmConstructor;
import org.eclipse.xtext.common.types.JvmField;
import org.eclipse.xtext.common.types.JvmFormalParameter;
import org.eclipse.xtext.common.types.JvmGenericType;
import org.eclipse.xtext.common.types.JvmMember;
import org.eclipse.xtext.common.types.JvmOperation;
import org.eclipse.xtext.common.types.JvmType;
import org.eclipse.xtext.common.types.JvmTypeReference;
import org.eclipse.xtext.common.types.TypesFactory;
import org.eclipse.xtext.naming.IQualifiedNameProvider;
import org.eclipse.xtext.naming.QualifiedName;
import org.eclipse.xtext.xbase.XExpression;
import org.eclipse.xtext.xbase.compiler.XbaseCompiler;
import org.eclipse.xtext.xbase.compiler.output.ITreeAppendable;
import org.eclipse.xtext.xbase.jvmmodel.AbstractModelInferrer;
import org.eclipse.xtext.xbase.jvmmodel.IJvmDeclaredTypeAcceptor;
import org.eclipse.xtext.xbase.jvmmodel.JvmModelAssociator;
import org.eclipse.xtext.xbase.jvmmodel.JvmTypesBuilder;
import org.eclipse.xtext.xbase.lib.Extension;
import org.eclipse.xtext.xbase.lib.Functions.Function1;
import org.eclipse.xtext.xbase.lib.InputOutput;
import org.eclipse.xtext.xbase.lib.IterableExtensions;
import org.eclipse.xtext.xbase.lib.Procedures.Procedure1;

/**
 * <p>Infers a JVM model from the source model.</p>
 * 
 * <p>The JVM model should contain all elements that would appear in the Java code
 * which is generated from the source model. Other models link against the JVM model rather than the source model.</p>
 */
@SuppressWarnings("all")
public class SARLJvmModelInferrer extends AbstractModelInferrer {
  public final static String KEYWORD_OCCURRENCE = "occurrence";
  
  /**
   * convenience API to build and initialize JVM types and their members.
   */
  @Inject
  @Extension
  private JvmTypesBuilder _jvmTypesBuilder;
  
  @Inject
  @Extension
  private IQualifiedNameProvider _iQualifiedNameProvider;
  
  @Inject
  protected XbaseCompiler xbaseCompiler;
  
  @Inject
  protected JvmModelAssociator jvmModelAssociator;
  
  @Inject
  private TypesFactory typesFactory;
  
  @Inject
  private Logger log;
  
  private Iterable<Capacity> kCapacities = null;
  
  /**
   * The dispatch method {@code infer} is called for each instance of the
   * given element's type that is contained in a resource.
   * 
   * @param element
   *            the model to create one or more
   *            {@link org.eclipse.xtext.common.types.JvmDeclaredType declared
   *            types} from.
   * @param acceptor
   *            each created
   *            {@link org.eclipse.xtext.common.types.JvmDeclaredType type}
   *            without a container should be passed to the acceptor in order
   *            get attached to the current resource. The acceptor's
   *            {@link IJvmDeclaredTypeAcceptor#accept(org.eclipse.xtext.common.types.JvmDeclaredType)
   *            accept(..)} method takes the constructed empty type for the
   *            pre-indexing phase. This one is further initialized in the
   *            indexing phase using the closure you pass to the returned
   *            {@link org.eclipse.xtext.xbase.jvmmodel.IJvmDeclaredTypeAcceptor.IPostIndexingInitializing#initializeLater(org.eclipse.xtext.xbase.lib.Procedures.Procedure1)
   *            initializeLater(..)}.
   * @param isPreIndexingPhase
   *            whether the method is called in a pre-indexing phase, i.e.
   *            when the global index is not yet fully updated. You must not
   *            rely on linking using the index if isPreIndexingPhase is
   *            <code>true</code>.
   */
  protected void _infer(final Event element, final IJvmDeclaredTypeAcceptor acceptor, final boolean isPreIndexingPhase) {
    QualifiedName _fullyQualifiedName = this._iQualifiedNameProvider.getFullyQualifiedName(element);
    JvmGenericType _class = this._jvmTypesBuilder.toClass(element, _fullyQualifiedName);
    IJvmDeclaredTypeAcceptor.IPostIndexingInitializing<JvmGenericType> _accept = acceptor.<JvmGenericType>accept(_class);
    final Procedure1<JvmGenericType> _function = new Procedure1<JvmGenericType>() {
      public void apply(final JvmGenericType it) {
        String _documentation = SARLJvmModelInferrer.this._jvmTypesBuilder.getDocumentation(element);
        SARLJvmModelInferrer.this._jvmTypesBuilder.setDocumentation(it, _documentation);
        EList<JvmTypeReference> _superTypes = it.getSuperTypes();
        JvmTypeReference _newTypeRef = SARLJvmModelInferrer.this._jvmTypesBuilder.newTypeRef(element, io.sarl.lang.core.Event.class);
        SARLJvmModelInferrer.this._jvmTypesBuilder.<JvmTypeReference>operator_add(_superTypes, _newTypeRef);
        EList<EventFeature> _features = element.getFeatures();
        for (final EventFeature feature : _features) {
          boolean _matched = false;
          if (!_matched) {
            if (feature instanceof Action) {
              _matched=true;
              ActionSignature _signature = ((Action)feature).getSignature();
              XExpression _body = ((Action)feature).getBody();
              SARLJvmModelInferrer.this.generateAction(it, _signature, _body);
            }
          }
          if (!_matched) {
            if (feature instanceof Attribute) {
              _matched=true;
              EList<JvmMember> _members = it.getMembers();
              String _name = ((Attribute)feature).getName();
              JvmTypeReference _type = ((Attribute)feature).getType();
              final Procedure1<JvmField> _function = new Procedure1<JvmField>() {
                public void apply(final JvmField it) {
                  boolean _isWriteable = ((Attribute)feature).isWriteable();
                  boolean _not = (!_isWriteable);
                  it.setFinal(_not);
                  XExpression _initialValue = ((Attribute)feature).getInitialValue();
                  SARLJvmModelInferrer.this._jvmTypesBuilder.setInitializer(it, _initialValue);
                }
              };
              JvmField _field = SARLJvmModelInferrer.this._jvmTypesBuilder.toField(feature, _name, _type, _function);
              SARLJvmModelInferrer.this._jvmTypesBuilder.<JvmField>operator_add(_members, _field);
              EList<JvmMember> _members_1 = it.getMembers();
              String _name_1 = ((Attribute)feature).getName();
              JvmTypeReference _type_1 = ((Attribute)feature).getType();
              JvmOperation _getter = SARLJvmModelInferrer.this._jvmTypesBuilder.toGetter(feature, _name_1, _type_1);
              SARLJvmModelInferrer.this._jvmTypesBuilder.<JvmOperation>operator_add(_members_1, _getter);
              boolean _isWriteable = ((Attribute)feature).isWriteable();
              if (_isWriteable) {
                EList<JvmMember> _members_2 = it.getMembers();
                String _name_2 = ((Attribute)feature).getName();
                JvmTypeReference _type_2 = ((Attribute)feature).getType();
                JvmOperation _setter = SARLJvmModelInferrer.this._jvmTypesBuilder.toSetter(feature, _name_2, _type_2);
                SARLJvmModelInferrer.this._jvmTypesBuilder.<JvmOperation>operator_add(_members_2, _setter);
              }
            }
          }
          if (!_matched) {
            if (feature instanceof CapacityUses) {
              _matched=true;
              EList<Capacity> _capacitiesUsed = ((CapacityUses)feature).getCapacitiesUsed();
              for (final Capacity used : _capacitiesUsed) {
                SARLJvmModelInferrer.this.generateCapactyDelegatorMethods(it, element, used);
              }
            }
          }
          if (!_matched) {
            if (feature instanceof Constructor) {
              _matched=true;
              SARLJvmModelInferrer.this.generateContructor(it, element, ((Constructor)feature));
            }
          }
        }
        EList<JvmMember> _members = it.getMembers();
        JvmTypeReference _newTypeRef_1 = SARLJvmModelInferrer.this._jvmTypesBuilder.newTypeRef(it, String.class);
        final Procedure1<JvmOperation> _function = new Procedure1<JvmOperation>() {
          public void apply(final JvmOperation it) {
            StringConcatenation _builder = new StringConcatenation();
            _builder.append("Returns a String representation of the Event ");
            String _name = element.getName();
            _builder.append(_name, "");
            SARLJvmModelInferrer.this._jvmTypesBuilder.setDocumentation(it, _builder.toString());
            final Procedure1<ITreeAppendable> _function = new Procedure1<ITreeAppendable>() {
              public void apply(final ITreeAppendable it) {
                StringConcatenation _builder = new StringConcatenation();
                _builder.append("StringBuilder result = new StringBuilder();");
                _builder.newLine();
                _builder.append("result.append(\"");
                String _name = element.getName();
                _builder.append(_name, "");
                _builder.append("[\");");
                _builder.newLineIfNotEmpty();
                {
                  EList<EventFeature> _features = element.getFeatures();
                  Iterable<Attribute> _filter = Iterables.<Attribute>filter(_features, Attribute.class);
                  for(final Attribute attr : _filter) {
                    _builder.append("result.append(\"");
                    String _name_1 = attr.getName();
                    _builder.append(_name_1, "");
                    _builder.append("  = \").append(this.");
                    String _name_2 = attr.getName();
                    _builder.append(_name_2, "");
                    _builder.append(");");
                    _builder.newLineIfNotEmpty();
                  }
                }
                _builder.append("result.append(\"]\");");
                _builder.newLine();
                _builder.append("return result.toString();");
                it.append(_builder);
              }
            };
            SARLJvmModelInferrer.this._jvmTypesBuilder.setBody(it, _function);
          }
        };
        JvmOperation _method = SARLJvmModelInferrer.this._jvmTypesBuilder.toMethod(element, "toString", _newTypeRef_1, _function);
        SARLJvmModelInferrer.this._jvmTypesBuilder.<JvmOperation>operator_add(_members, _method);
      }
    };
    _accept.initializeLater(_function);
  }
  
  protected void _infer(final Capacity capacity, final IJvmDeclaredTypeAcceptor acceptor, final boolean isPreIndexingPhase) {
    QualifiedName _fullyQualifiedName = this._iQualifiedNameProvider.getFullyQualifiedName(capacity);
    String _string = _fullyQualifiedName.toString();
    JvmGenericType _interface = this._jvmTypesBuilder.toInterface(capacity, _string, null);
    IJvmDeclaredTypeAcceptor.IPostIndexingInitializing<JvmGenericType> _accept = acceptor.<JvmGenericType>accept(_interface);
    final Procedure1<JvmGenericType> _function = new Procedure1<JvmGenericType>() {
      public void apply(final JvmGenericType it) {
        String _documentation = SARLJvmModelInferrer.this._jvmTypesBuilder.getDocumentation(capacity);
        SARLJvmModelInferrer.this._jvmTypesBuilder.setDocumentation(it, _documentation);
        boolean _and = false;
        Capacity _superType = capacity.getSuperType();
        boolean _notEquals = (!Objects.equal(_superType, null));
        if (!_notEquals) {
          _and = false;
        } else {
          Capacity _superType_1 = capacity.getSuperType();
          QualifiedName _fullyQualifiedName = SARLJvmModelInferrer.this._iQualifiedNameProvider.getFullyQualifiedName(_superType_1);
          boolean _notEquals_1 = (!Objects.equal(_fullyQualifiedName, null));
          _and = (_notEquals && _notEquals_1);
        }
        if (_and) {
          EList<JvmTypeReference> _superTypes = it.getSuperTypes();
          Capacity _superType_2 = capacity.getSuperType();
          QualifiedName _fullyQualifiedName_1 = SARLJvmModelInferrer.this._iQualifiedNameProvider.getFullyQualifiedName(_superType_2);
          String _string = _fullyQualifiedName_1.toString();
          JvmTypeReference _newTypeRef = SARLJvmModelInferrer.this._jvmTypesBuilder.newTypeRef(it, _string);
          SARLJvmModelInferrer.this._jvmTypesBuilder.<JvmTypeReference>operator_add(_superTypes, _newTypeRef);
        } else {
          EList<JvmTypeReference> _superTypes_1 = it.getSuperTypes();
          JvmTypeReference _newTypeRef_1 = SARLJvmModelInferrer.this._jvmTypesBuilder.newTypeRef(capacity, io.sarl.lang.core.Capacity.class);
          SARLJvmModelInferrer.this._jvmTypesBuilder.<JvmTypeReference>operator_add(_superTypes_1, _newTypeRef_1);
        }
        EList<ActionSignature> _actions = capacity.getActions();
        for (final ActionSignature feature : _actions) {
          SARLJvmModelInferrer.this.generateAction(it, feature, null);
        }
      }
    };
    _accept.initializeLater(_function);
  }
  
  protected void _infer(final Skill element, final IJvmDeclaredTypeAcceptor acceptor, final boolean isPreIndexingPhase) {
    QualifiedName _fullyQualifiedName = this._iQualifiedNameProvider.getFullyQualifiedName(element);
    JvmGenericType _class = this._jvmTypesBuilder.toClass(element, _fullyQualifiedName);
    IJvmDeclaredTypeAcceptor.IPostIndexingInitializing<JvmGenericType> _accept = acceptor.<JvmGenericType>accept(_class);
    final Procedure1<JvmGenericType> _function = new Procedure1<JvmGenericType>() {
      public void apply(final JvmGenericType it) {
        String _documentation = SARLJvmModelInferrer.this._jvmTypesBuilder.getDocumentation(element);
        SARLJvmModelInferrer.this._jvmTypesBuilder.setDocumentation(it, _documentation);
        EList<JvmTypeReference> _superTypes = it.getSuperTypes();
        JvmTypeReference _newTypeRef = SARLJvmModelInferrer.this._jvmTypesBuilder.newTypeRef(element, io.sarl.lang.core.Skill.class);
        SARLJvmModelInferrer.this._jvmTypesBuilder.<JvmTypeReference>operator_add(_superTypes, _newTypeRef);
        EList<Capacity> _implementedCapacities = element.getImplementedCapacities();
        for (final Capacity cap : _implementedCapacities) {
          QualifiedName _fullyQualifiedName = SARLJvmModelInferrer.this._iQualifiedNameProvider.getFullyQualifiedName(cap);
          boolean _notEquals = (!Objects.equal(_fullyQualifiedName, null));
          if (_notEquals) {
            EList<JvmTypeReference> _superTypes_1 = it.getSuperTypes();
            QualifiedName _fullyQualifiedName_1 = SARLJvmModelInferrer.this._iQualifiedNameProvider.getFullyQualifiedName(cap);
            String _string = _fullyQualifiedName_1.toString();
            JvmTypeReference _newTypeRef_1 = SARLJvmModelInferrer.this._jvmTypesBuilder.newTypeRef(element, _string);
            SARLJvmModelInferrer.this._jvmTypesBuilder.<JvmTypeReference>operator_add(_superTypes_1, _newTypeRef_1);
          } else {
            String _name = cap.getName();
            String _plus = ("Null FQN for Capacity:" + _name);
            InputOutput.<String>println(_plus);
          }
        }
        EList<SkillFeature> _features = element.getFeatures();
        for (final SkillFeature feature : _features) {
          boolean _matched = false;
          if (!_matched) {
            if (feature instanceof Action) {
              _matched=true;
              ActionSignature _signature = ((Action)feature).getSignature();
              XExpression _body = ((Action)feature).getBody();
              SARLJvmModelInferrer.this.generateAction(it, _signature, _body);
            }
          }
          if (!_matched) {
            if (feature instanceof Attribute) {
              _matched=true;
              EList<JvmMember> _members = it.getMembers();
              String _name_1 = ((Attribute)feature).getName();
              JvmTypeReference _type = ((Attribute)feature).getType();
              final Procedure1<JvmField> _function = new Procedure1<JvmField>() {
                public void apply(final JvmField it) {
                  String _documentation = SARLJvmModelInferrer.this._jvmTypesBuilder.getDocumentation(feature);
                  SARLJvmModelInferrer.this._jvmTypesBuilder.setDocumentation(it, _documentation);
                  boolean _isWriteable = ((Attribute)feature).isWriteable();
                  boolean _not = (!_isWriteable);
                  it.setFinal(_not);
                  XExpression _initialValue = ((Attribute)feature).getInitialValue();
                  SARLJvmModelInferrer.this._jvmTypesBuilder.setInitializer(it, _initialValue);
                }
              };
              JvmField _field = SARLJvmModelInferrer.this._jvmTypesBuilder.toField(feature, _name_1, _type, _function);
              SARLJvmModelInferrer.this._jvmTypesBuilder.<JvmField>operator_add(_members, _field);
            }
          }
          if (!_matched) {
            if (feature instanceof CapacityUses) {
              _matched=true;
              EList<Capacity> _capacitiesUsed = ((CapacityUses)feature).getCapacitiesUsed();
              for (final Capacity used : _capacitiesUsed) {
                SARLJvmModelInferrer.this.generateCapactyDelegatorMethods(it, element, used);
              }
            }
          }
          if (!_matched) {
            if (feature instanceof Constructor) {
              _matched=true;
              SARLJvmModelInferrer.this.generateContructor(it, element, ((Constructor)feature));
            }
          }
        }
      }
    };
    _accept.initializeLater(_function);
  }
  
  protected void _infer(final Behavior element, final IJvmDeclaredTypeAcceptor acceptor, final boolean isPreIndexingPhase) {
    QualifiedName _fullyQualifiedName = this._iQualifiedNameProvider.getFullyQualifiedName(element);
    JvmGenericType _class = this._jvmTypesBuilder.toClass(element, _fullyQualifiedName);
    IJvmDeclaredTypeAcceptor.IPostIndexingInitializing<JvmGenericType> _accept = acceptor.<JvmGenericType>accept(_class);
    final Procedure1<JvmGenericType> _function = new Procedure1<JvmGenericType>() {
      public void apply(final JvmGenericType it) {
        String _documentation = SARLJvmModelInferrer.this._jvmTypesBuilder.getDocumentation(element);
        SARLJvmModelInferrer.this._jvmTypesBuilder.setDocumentation(it, _documentation);
        int counter = 0;
        EList<JvmTypeReference> _superTypes = it.getSuperTypes();
        JvmTypeReference _newTypeRef = SARLJvmModelInferrer.this._jvmTypesBuilder.newTypeRef(element, io.sarl.lang.core.Behavior.class);
        SARLJvmModelInferrer.this._jvmTypesBuilder.<JvmTypeReference>operator_add(_superTypes, _newTypeRef);
        EList<BehaviorFeature> _features = element.getFeatures();
        for (final BehaviorFeature feature : _features) {
          boolean _matched = false;
          if (!_matched) {
            if (feature instanceof RequiredCapacity) {
              _matched=true;
            }
          }
          if (!_matched) {
            if (feature instanceof BehaviorUnit) {
              _matched=true;
              EList<JvmMember> _members = it.getMembers();
              JvmOperation _generateBehaviorUnit = SARLJvmModelInferrer.this.generateBehaviorUnit(it, ((BehaviorUnit) feature), counter);
              SARLJvmModelInferrer.this._jvmTypesBuilder.<JvmOperation>operator_add(_members, _generateBehaviorUnit);
            }
          }
          if (!_matched) {
            if (feature instanceof CapacityUses) {
              _matched=true;
              EList<Capacity> _capacitiesUsed = ((CapacityUses)feature).getCapacitiesUsed();
              for (final Capacity used : _capacitiesUsed) {
                SARLJvmModelInferrer.this.generateCapactyDelegatorMethods(it, element, used);
              }
            }
          }
          if (!_matched) {
            if (feature instanceof Constructor) {
              _matched=true;
              SARLJvmModelInferrer.this.generateContructor(it, element, ((Constructor)feature));
            }
          }
          if (!_matched) {
            if (feature instanceof Attribute) {
              _matched=true;
              EList<JvmMember> _members = it.getMembers();
              String _name = ((Attribute)feature).getName();
              JvmTypeReference _type = ((Attribute)feature).getType();
              final Procedure1<JvmField> _function = new Procedure1<JvmField>() {
                public void apply(final JvmField it) {
                  String _documentation = SARLJvmModelInferrer.this._jvmTypesBuilder.getDocumentation(feature);
                  SARLJvmModelInferrer.this._jvmTypesBuilder.setDocumentation(it, _documentation);
                  boolean _isWriteable = ((Attribute)feature).isWriteable();
                  boolean _not = (!_isWriteable);
                  it.setFinal(_not);
                  XExpression _initialValue = ((Attribute)feature).getInitialValue();
                  SARLJvmModelInferrer.this._jvmTypesBuilder.setInitializer(it, _initialValue);
                }
              };
              JvmField _field = SARLJvmModelInferrer.this._jvmTypesBuilder.toField(feature, _name, _type, _function);
              SARLJvmModelInferrer.this._jvmTypesBuilder.<JvmField>operator_add(_members, _field);
            }
          }
        }
      }
    };
    _accept.initializeLater(_function);
  }
  
  protected void _infer(final Agent agent, final IJvmDeclaredTypeAcceptor acceptor, final boolean isPreIndexingPhase) {
    QualifiedName _fullyQualifiedName = this._iQualifiedNameProvider.getFullyQualifiedName(agent);
    JvmGenericType _class = this._jvmTypesBuilder.toClass(agent, _fullyQualifiedName);
    IJvmDeclaredTypeAcceptor.IPostIndexingInitializing<JvmGenericType> _accept = acceptor.<JvmGenericType>accept(_class);
    final Procedure1<JvmGenericType> _function = new Procedure1<JvmGenericType>() {
      public void apply(final JvmGenericType it) {
        String _documentation = SARLJvmModelInferrer.this._jvmTypesBuilder.getDocumentation(agent);
        SARLJvmModelInferrer.this._jvmTypesBuilder.setDocumentation(it, _documentation);
        EList<JvmTypeReference> _superTypes = it.getSuperTypes();
        JvmTypeReference _newTypeRef = SARLJvmModelInferrer.this._jvmTypesBuilder.newTypeRef(agent, io.sarl.lang.core.Agent.class);
        SARLJvmModelInferrer.this._jvmTypesBuilder.<JvmTypeReference>operator_add(_superTypes, _newTypeRef);
        EList<JvmMember> _members = it.getMembers();
        final Procedure1<JvmConstructor> _function = new Procedure1<JvmConstructor>() {
          public void apply(final JvmConstructor it) {
            StringConcatenation _builder = new StringConcatenation();
            _builder.append("Creates a new Agent of type ");
            String _name = agent.getName();
            _builder.append(_name, "");
            SARLJvmModelInferrer.this._jvmTypesBuilder.setDocumentation(it, _builder.toString());
            EList<JvmFormalParameter> _parameters = it.getParameters();
            JvmTypeReference _newTypeRef = SARLJvmModelInferrer.this._jvmTypesBuilder.newTypeRef(it, UUID.class);
            JvmFormalParameter _parameter = SARLJvmModelInferrer.this._jvmTypesBuilder.toParameter(agent, "parentID", _newTypeRef);
            SARLJvmModelInferrer.this._jvmTypesBuilder.<JvmFormalParameter>operator_add(_parameters, _parameter);
            StringConcatenationClient _client = new StringConcatenationClient() {
              @Override
              protected void appendTo(StringConcatenationClient.TargetStringConcatenation _builder) {
                _builder.append("super(parentID);");
                _builder.newLine();
              }
            };
            SARLJvmModelInferrer.this._jvmTypesBuilder.setBody(it, _client);
          }
        };
        JvmConstructor _constructor = SARLJvmModelInferrer.this._jvmTypesBuilder.toConstructor(agent, _function);
        SARLJvmModelInferrer.this._jvmTypesBuilder.<JvmConstructor>operator_add(_members, _constructor);
        int counter = 0;
        EList<AgentFeature> _features = agent.getFeatures();
        for (final AgentFeature feature : _features) {
          boolean _matched = false;
          if (!_matched) {
            if (feature instanceof BehaviorUnit) {
              _matched=true;
              counter = (counter + 1);
              final JvmOperation bMethod = SARLJvmModelInferrer.this.generateBehaviorUnit(it, ((BehaviorUnit) feature), counter);
              EList<JvmMember> _members_1 = it.getMembers();
              SARLJvmModelInferrer.this._jvmTypesBuilder.<JvmOperation>operator_add(_members_1, bMethod);
            }
          }
          if (!_matched) {
            if (feature instanceof Action) {
              _matched=true;
              ActionSignature _signature = ((Action)feature).getSignature();
              XExpression _body = ((Action)feature).getBody();
              SARLJvmModelInferrer.this.generateAction(it, _signature, _body);
            }
          }
          if (!_matched) {
            if (feature instanceof Attribute) {
              _matched=true;
              EList<JvmMember> _members_1 = it.getMembers();
              String _name = ((Attribute)feature).getName();
              JvmTypeReference _type = ((Attribute)feature).getType();
              final Procedure1<JvmField> _function_1 = new Procedure1<JvmField>() {
                public void apply(final JvmField it) {
                  String _documentation = SARLJvmModelInferrer.this._jvmTypesBuilder.getDocumentation(feature);
                  SARLJvmModelInferrer.this._jvmTypesBuilder.setDocumentation(it, _documentation);
                  XExpression _initialValue = ((Attribute)feature).getInitialValue();
                  SARLJvmModelInferrer.this._jvmTypesBuilder.setInitializer(it, _initialValue);
                  boolean _isWriteable = ((Attribute)feature).isWriteable();
                  boolean _not = (!_isWriteable);
                  it.setFinal(_not);
                }
              };
              JvmField _field = SARLJvmModelInferrer.this._jvmTypesBuilder.toField(feature, _name, _type, _function_1);
              SARLJvmModelInferrer.this._jvmTypesBuilder.<JvmField>operator_add(_members_1, _field);
            }
          }
          if (!_matched) {
            if (feature instanceof CapacityUses) {
              _matched=true;
              EList<Capacity> _capacitiesUsed = ((CapacityUses)feature).getCapacitiesUsed();
              for (final Capacity used : _capacitiesUsed) {
                SARLJvmModelInferrer.this.generateCapactyDelegatorMethods(it, agent, used);
              }
            }
          }
        }
      }
    };
    _accept.initializeLater(_function);
  }
  
  public void generateCapactyDelegatorMethods(final JvmGenericType owner, final AbstractElement context, final Capacity capacity) {
    EList<ActionSignature> _actions = capacity.getActions();
    for (final ActionSignature signature : _actions) {
      JvmOperation _generateAction = this.generateAction(owner, signature, null);
      final Procedure1<ITreeAppendable> _function = new Procedure1<ITreeAppendable>() {
        public void apply(final ITreeAppendable it) {
          JvmTypeReference _type = signature.getType();
          boolean _notEquals = (!Objects.equal(_type, null));
          if (_notEquals) {
            StringConcatenation _builder = new StringConcatenation();
            _builder.append("return ");
            it.append(_builder);
          }
          StringConcatenation _builder_1 = new StringConcatenation();
          _builder_1.append("this.getSkill(");
          it.append(_builder_1);
          QualifiedName _fullyQualifiedName = SARLJvmModelInferrer.this._iQualifiedNameProvider.getFullyQualifiedName(capacity);
          String _string = _fullyQualifiedName.toString();
          JvmTypeReference _newTypeRef = SARLJvmModelInferrer.this._jvmTypesBuilder.newTypeRef(context, _string);
          JvmType _type_1 = _newTypeRef.getType();
          it.append(_type_1);
          StringConcatenation _builder_2 = new StringConcatenation();
          _builder_2.append(".class).");
          String _name = signature.getName();
          _builder_2.append(_name, "");
          _builder_2.append("(");
          it.append(_builder_2);
          EList<Parameter> _params = signature.getParams();
          final Function1<Parameter,String> _function = new Function1<Parameter,String>() {
            public String apply(final Parameter it) {
              String _name = it.getName();
              return _name;
            }
          };
          String _join = IterableExtensions.<Parameter>join(_params, ", ", _function);
          it.append(_join);
          it.append(");");
        }
      };
      this._jvmTypesBuilder.setBody(_generateAction, _function);
    }
  }
  
  public JvmOperation generateBehaviorUnit(final JvmGenericType owner, final BehaviorUnit unit, final int index) {
    JvmOperation _xblockexpression = null;
    {
      Event _event = unit.getEvent();
      String _name = _event.getName();
      String _plus = ("_handle_" + _name);
      String _plus_1 = (_plus + "_");
      final String behName = (_plus_1 + Integer.valueOf(index));
      JvmTypeReference _newTypeRef = this._jvmTypesBuilder.newTypeRef(unit, Void.TYPE);
      final Procedure1<JvmOperation> _function = new Procedure1<JvmOperation>() {
        public void apply(final JvmOperation it) {
          String _documentation = SARLJvmModelInferrer.this._jvmTypesBuilder.getDocumentation(unit);
          SARLJvmModelInferrer.this._jvmTypesBuilder.setDocumentation(it, _documentation);
          EList<JvmAnnotationReference> _annotations = it.getAnnotations();
          JvmAnnotationReference _annotation = SARLJvmModelInferrer.this._jvmTypesBuilder.toAnnotation(unit, Percept.class);
          SARLJvmModelInferrer.this._jvmTypesBuilder.<JvmAnnotationReference>operator_add(_annotations, _annotation);
          EList<JvmFormalParameter> _parameters = it.getParameters();
          Event _event = unit.getEvent();
          Event _event_1 = unit.getEvent();
          Event _event_2 = unit.getEvent();
          QualifiedName _fullyQualifiedName = SARLJvmModelInferrer.this._iQualifiedNameProvider.getFullyQualifiedName(_event_2);
          String _string = _fullyQualifiedName.toString();
          JvmTypeReference _newTypeRef = SARLJvmModelInferrer.this._jvmTypesBuilder.newTypeRef(_event_1, _string);
          JvmFormalParameter _parameter = SARLJvmModelInferrer.this._jvmTypesBuilder.toParameter(_event, SARLJvmModelInferrer.KEYWORD_OCCURRENCE, _newTypeRef);
          SARLJvmModelInferrer.this._jvmTypesBuilder.<JvmFormalParameter>operator_add(_parameters, _parameter);
        }
      };
      final JvmOperation behaviorMethod = this._jvmTypesBuilder.toMethod(unit, behName, _newTypeRef, _function);
      XExpression _guard = unit.getGuard();
      boolean _equals = Objects.equal(_guard, null);
      if (_equals) {
        XExpression _body = unit.getBody();
        this._jvmTypesBuilder.setBody(behaviorMethod, _body);
      } else {
        final XExpression guard = unit.getGuard();
        final String guardMethodName = (behName + "_Guard");
        JvmTypeReference _newTypeRef_1 = this._jvmTypesBuilder.newTypeRef(guard, Boolean.TYPE);
        final Procedure1<JvmOperation> _function_1 = new Procedure1<JvmOperation>() {
          public void apply(final JvmOperation it) {
            String _string = guard.toString();
            String _plus = ((("Ensures that the behavior " + behName) + " is called only when the guard ") + _string);
            String _plus_1 = (_plus + " is valid");
            SARLJvmModelInferrer.this._jvmTypesBuilder.setDocumentation(it, _plus_1);
            EList<JvmFormalParameter> _parameters = it.getParameters();
            Event _event = unit.getEvent();
            Event _event_1 = unit.getEvent();
            Event _event_2 = unit.getEvent();
            QualifiedName _fullyQualifiedName = SARLJvmModelInferrer.this._iQualifiedNameProvider.getFullyQualifiedName(_event_2);
            String _string_1 = _fullyQualifiedName.toString();
            JvmTypeReference _newTypeRef = SARLJvmModelInferrer.this._jvmTypesBuilder.newTypeRef(_event_1, _string_1);
            JvmFormalParameter _parameter = SARLJvmModelInferrer.this._jvmTypesBuilder.toParameter(_event, SARLJvmModelInferrer.KEYWORD_OCCURRENCE, _newTypeRef);
            SARLJvmModelInferrer.this._jvmTypesBuilder.<JvmFormalParameter>operator_add(_parameters, _parameter);
          }
        };
        final JvmOperation guardMethod = this._jvmTypesBuilder.toMethod(guard, guardMethodName, _newTypeRef_1, _function_1);
        this._jvmTypesBuilder.setBody(guardMethod, guard);
        XExpression _body_1 = unit.getBody();
        this.jvmModelAssociator.associateLogicalContainer(_body_1, behaviorMethod);
        final Procedure1<ITreeAppendable> _function_2 = new Procedure1<ITreeAppendable>() {
          public void apply(final ITreeAppendable it) {
            StringConcatenation _builder = new StringConcatenation();
            _builder.append("if ( ");
            _builder.append(guardMethodName, "");
            _builder.append("(");
            _builder.append(SARLJvmModelInferrer.KEYWORD_OCCURRENCE, "");
            _builder.append(")) { ");
            it.append(_builder);
            XExpression _body = unit.getBody();
            JvmTypeReference _newTypeRef = SARLJvmModelInferrer.this._jvmTypesBuilder.newTypeRef(behaviorMethod, Void.TYPE);
            SARLJvmModelInferrer.this.xbaseCompiler.compile(_body, it, _newTypeRef);
            it.append("}");
          }
        };
        this._jvmTypesBuilder.setBody(behaviorMethod, _function_2);
        EList<JvmMember> _members = owner.getMembers();
        this._jvmTypesBuilder.<JvmOperation>operator_add(_members, guardMethod);
      }
      _xblockexpression = (behaviorMethod);
    }
    return _xblockexpression;
  }
  
  public JvmOperation generateAction(final JvmGenericType owner, final ActionSignature signature, final XExpression operationBody) {
    JvmTypeReference returnType = signature.getType();
    boolean _equals = Objects.equal(returnType, null);
    if (_equals) {
      JvmTypeReference _newTypeRef = this._jvmTypesBuilder.newTypeRef(signature, Void.TYPE);
      returnType = _newTypeRef;
    }
    String _name = signature.getName();
    final Procedure1<JvmOperation> _function = new Procedure1<JvmOperation>() {
      public void apply(final JvmOperation it) {
        String _documentation = SARLJvmModelInferrer.this._jvmTypesBuilder.getDocumentation(signature);
        SARLJvmModelInferrer.this._jvmTypesBuilder.setDocumentation(it, _documentation);
        EList<Parameter> _params = signature.getParams();
        for (final Parameter p : _params) {
          EList<JvmFormalParameter> _parameters = it.getParameters();
          String _name = p.getName();
          JvmTypeReference _parameterType = p.getParameterType();
          JvmFormalParameter _parameter = SARLJvmModelInferrer.this._jvmTypesBuilder.toParameter(p, _name, _parameterType);
          SARLJvmModelInferrer.this._jvmTypesBuilder.<JvmFormalParameter>operator_add(_parameters, _parameter);
        }
        SARLJvmModelInferrer.this._jvmTypesBuilder.setBody(it, operationBody);
      }
    };
    final JvmOperation op = this._jvmTypesBuilder.toMethod(owner, _name, returnType, _function);
    EList<JvmMember> _members = owner.getMembers();
    this._jvmTypesBuilder.<JvmOperation>operator_add(_members, op);
    return op;
  }
  
  public void generateContructor(final JvmGenericType owner, final AbstractElement context, final Constructor constructor) {
    EList<JvmMember> _members = owner.getMembers();
    final Procedure1<JvmConstructor> _function = new Procedure1<JvmConstructor>() {
      public void apply(final JvmConstructor it) {
        String _documentation = SARLJvmModelInferrer.this._jvmTypesBuilder.getDocumentation(constructor);
        SARLJvmModelInferrer.this._jvmTypesBuilder.setDocumentation(it, _documentation);
        EList<Parameter> _params = constructor.getParams();
        for (final Parameter p : _params) {
          EList<JvmFormalParameter> _parameters = it.getParameters();
          String _name = p.getName();
          JvmTypeReference _parameterType = p.getParameterType();
          JvmFormalParameter _parameter = SARLJvmModelInferrer.this._jvmTypesBuilder.toParameter(p, _name, _parameterType);
          SARLJvmModelInferrer.this._jvmTypesBuilder.<JvmFormalParameter>operator_add(_parameters, _parameter);
        }
        XExpression _body = constructor.getBody();
        SARLJvmModelInferrer.this._jvmTypesBuilder.setBody(it, _body);
      }
    };
    JvmConstructor _constructor = this._jvmTypesBuilder.toConstructor(context, _function);
    this._jvmTypesBuilder.<JvmConstructor>operator_add(_members, _constructor);
  }
  
  public void infer(final EObject agent, final IJvmDeclaredTypeAcceptor acceptor, final boolean isPreIndexingPhase) {
    if (agent instanceof Agent) {
      _infer((Agent)agent, acceptor, isPreIndexingPhase);
      return;
    } else if (agent instanceof Behavior) {
      _infer((Behavior)agent, acceptor, isPreIndexingPhase);
      return;
    } else if (agent instanceof Capacity) {
      _infer((Capacity)agent, acceptor, isPreIndexingPhase);
      return;
    } else if (agent instanceof Event) {
      _infer((Event)agent, acceptor, isPreIndexingPhase);
      return;
    } else if (agent instanceof Skill) {
      _infer((Skill)agent, acceptor, isPreIndexingPhase);
      return;
    } else if (agent != null) {
      _infer(agent, acceptor, isPreIndexingPhase);
      return;
    } else {
      throw new IllegalArgumentException("Unhandled parameter types: " +
        Arrays.<Object>asList(agent, acceptor, isPreIndexingPhase).toString());
    }
  }
}
