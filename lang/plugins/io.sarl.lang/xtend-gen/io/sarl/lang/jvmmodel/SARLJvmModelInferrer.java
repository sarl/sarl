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
import io.sarl.lang.SARLKeywords;
import io.sarl.lang.core.Percept;
import io.sarl.lang.sarl.Action;
import io.sarl.lang.sarl.ActionSignature;
import io.sarl.lang.sarl.Agent;
import io.sarl.lang.sarl.Attribute;
import io.sarl.lang.sarl.Behavior;
import io.sarl.lang.sarl.BehaviorUnit;
import io.sarl.lang.sarl.Capacity;
import io.sarl.lang.sarl.CapacityUses;
import io.sarl.lang.sarl.Constructor;
import io.sarl.lang.sarl.Event;
import io.sarl.lang.sarl.FormalParameter;
import io.sarl.lang.sarl.InheritingElement;
import io.sarl.lang.sarl.NamedElement;
import io.sarl.lang.sarl.ParameterizedFeature;
import io.sarl.lang.sarl.RequiredCapacity;
import io.sarl.lang.sarl.Skill;
import io.sarl.lang.sarl.TopElement;
import io.sarl.lang.signature.ActionNameKey;
import io.sarl.lang.signature.ActionSignatureComparator;
import io.sarl.lang.signature.ActionSignatureProvider;
import io.sarl.lang.signature.InferredActionSignature;
import io.sarl.lang.signature.InferredStandardParameter;
import io.sarl.lang.signature.InferredValuedParameter;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.TreeMap;
import java.util.TreeSet;
import java.util.UUID;
import java.util.logging.Logger;
import org.eclipse.emf.common.util.EList;
import org.eclipse.emf.ecore.EObject;
import org.eclipse.xtend2.lib.StringConcatenation;
import org.eclipse.xtend2.lib.StringConcatenationClient;
import org.eclipse.xtext.common.types.JvmAnnotationReference;
import org.eclipse.xtext.common.types.JvmConstructor;
import org.eclipse.xtext.common.types.JvmDeclaredType;
import org.eclipse.xtext.common.types.JvmExecutable;
import org.eclipse.xtext.common.types.JvmField;
import org.eclipse.xtext.common.types.JvmFormalParameter;
import org.eclipse.xtext.common.types.JvmGenericType;
import org.eclipse.xtext.common.types.JvmMember;
import org.eclipse.xtext.common.types.JvmOperation;
import org.eclipse.xtext.common.types.JvmType;
import org.eclipse.xtext.common.types.JvmTypeReference;
import org.eclipse.xtext.common.types.JvmVisibility;
import org.eclipse.xtext.naming.IQualifiedNameProvider;
import org.eclipse.xtext.naming.QualifiedName;
import org.eclipse.xtext.xbase.XExpression;
import org.eclipse.xtext.xbase.XStringLiteral;
import org.eclipse.xtext.xbase.compiler.XbaseCompiler;
import org.eclipse.xtext.xbase.compiler.output.FakeTreeAppendable;
import org.eclipse.xtext.xbase.compiler.output.ITreeAppendable;
import org.eclipse.xtext.xbase.jvmmodel.AbstractModelInferrer;
import org.eclipse.xtext.xbase.jvmmodel.IJvmDeclaredTypeAcceptor;
import org.eclipse.xtext.xbase.jvmmodel.IJvmDeclaredTypeAcceptor.IPostIndexingInitializing;
import org.eclipse.xtext.xbase.jvmmodel.JvmModelAssociator;
import org.eclipse.xtext.xbase.jvmmodel.JvmTypesBuilder;
import org.eclipse.xtext.xbase.lib.Conversions;
import org.eclipse.xtext.xbase.lib.Extension;
import org.eclipse.xtext.xbase.lib.Functions.Function1;
import org.eclipse.xtext.xbase.lib.IterableExtensions;
import org.eclipse.xtext.xbase.lib.Procedures.Procedure1;
import org.eclipse.xtext.xbase.lib.Procedures.Procedure2;
import org.eclipse.xtext.xbase.typesystem.legacy.StandardTypeReferenceOwner;
import org.eclipse.xtext.xbase.typesystem.references.LightweightTypeReference;
import org.eclipse.xtext.xbase.typesystem.references.OwnedConverter;
import org.eclipse.xtext.xbase.typesystem.util.CommonTypeComputationServices;

/**
 * <p>Infers a JVM model from the source model.</p>
 * 
 * <p>The JVM model should contain all elements that would appear in the Java code
 * which is generated from the source model. Other models link against
 * the JVM model rather than the source model.</p>
 * 
 * @author $Author: srodriguez$
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 */
@SuppressWarnings("all")
public class SARLJvmModelInferrer extends AbstractModelInferrer {
  @Inject
  @Extension
  private JvmTypesBuilder _jvmTypesBuilder;
  
  @Inject
  @Extension
  private IQualifiedNameProvider _iQualifiedNameProvider;
  
  @Inject
  private XbaseCompiler xbaseCompiler;
  
  @Inject
  private JvmModelAssociator jvmModelAssociator;
  
  @Inject
  private Logger log;
  
  @Inject
  private ActionSignatureProvider sarlSignatureProvider;
  
  @Inject
  private CommonTypeComputationServices services;
  
  /**
   * The dispatch method {@code infer} is called for each instance of the
   * given element's type that is contained in a resource.
   * 
   * @param element
   *            the model to create one or more
   *            {@link JvmDeclaredType declared
   *            types} from.
   * @param acceptor
   *            each created
   *            {@code type}
   *            without a container should be passed to the acceptor in order
   *            get attached to the current resource. The acceptor's
   *            {@link IJvmDeclaredTypeAcceptor#accept(org.eclipse.xtext.common.types.JvmDeclaredType)
   *            accept(..)} method takes the constructed empty type for the
   *            pre-indexing phase. This one is further initialized in the
   *            indexing phase using the closure you pass to the returned
   *            {@link IPostIndexingInitializing#initializeLater(org.eclipse.xtext.xbase.lib.Procedures.Procedure1)
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
        SARLJvmModelInferrer.this.sarlSignatureProvider.resetSignatures(it);
        String _documentation = SARLJvmModelInferrer.this._jvmTypesBuilder.getDocumentation(element);
        SARLJvmModelInferrer.this._jvmTypesBuilder.setDocumentation(it, _documentation);
        long serial = 1L;
        long _generateSuperTypes = SARLJvmModelInferrer.this.generateSuperTypes(it, element, io.sarl.lang.core.Event.class);
        long _plus = (serial + _generateSuperTypes);
        serial = _plus;
        JvmField jvmField = null;
        List<JvmField> jvmFields = new ArrayList<JvmField>();
        EList<EObject> _features = element.getFeatures();
        for (final EObject feature : _features) {
          boolean _matched = false;
          if (!_matched) {
            if (feature instanceof Attribute) {
              _matched=true;
              JvmField _generateAttribute = SARLJvmModelInferrer.this.generateAttribute(it, ((Attribute)feature), JvmVisibility.PUBLIC);
              jvmField = _generateAttribute;
              jvmFields.add(jvmField);
              EList<JvmMember> _members = it.getMembers();
              SARLJvmModelInferrer.this._jvmTypesBuilder.<JvmField>operator_add(_members, jvmField);
              String _name = ((Attribute)feature).getName();
              int _hashCode = _name.hashCode();
              long _plus_1 = (serial + _hashCode);
              serial = _plus_1;
            }
          }
          if (!_matched) {
            if (feature instanceof Constructor) {
              _matched=true;
              SARLJvmModelInferrer.this.generateConstructor(it, element, ((Constructor)feature));
              QualifiedName _fullyQualifiedName = SARLJvmModelInferrer.this._iQualifiedNameProvider.getFullyQualifiedName(element);
              int _hashCode = _fullyQualifiedName.hashCode();
              long _plus_1 = (serial + _hashCode);
              serial = _plus_1;
            }
          }
        }
        boolean _isEmpty = jvmFields.isEmpty();
        boolean _not = (!_isEmpty);
        if (_not) {
          final JvmField[] tab = ((JvmField[])Conversions.unwrapArray(jvmFields, JvmField.class));
          QualifiedName _fullyQualifiedName = SARLJvmModelInferrer.this._iQualifiedNameProvider.getFullyQualifiedName(element);
          JvmGenericType elementType = SARLJvmModelInferrer.this._jvmTypesBuilder.toClass(element, _fullyQualifiedName);
          EList<JvmMember> _members = it.getMembers();
          JvmOperation _equalsMethod_Bug434912 = SARLJvmModelInferrer.this.toEqualsMethod_Bug434912(it, element, elementType, true, tab);
          SARLJvmModelInferrer.this._jvmTypesBuilder.<JvmOperation>operator_add(_members, _equalsMethod_Bug434912);
          EList<JvmMember> _members_1 = it.getMembers();
          JvmOperation _hashCodeMethod_Bug392440 = SARLJvmModelInferrer.this.toHashCodeMethod_Bug392440(it, element, true, tab);
          SARLJvmModelInferrer.this._jvmTypesBuilder.<JvmOperation>operator_add(_members_1, _hashCodeMethod_Bug392440);
          EList<JvmMember> _members_2 = it.getMembers();
          JvmTypeReference _newTypeRef = SARLJvmModelInferrer.this._jvmTypesBuilder.newTypeRef(it, String.class);
          final Procedure1<JvmOperation> _function = new Procedure1<JvmOperation>() {
            public void apply(final JvmOperation it) {
              it.setVisibility(JvmVisibility.PROTECTED);
              StringConcatenation _builder = new StringConcatenation();
              _builder.append("Returns a String representation of the Event ");
              String _name = element.getName();
              _builder.append(_name, "");
              _builder.append(" attributes only.");
              SARLJvmModelInferrer.this._jvmTypesBuilder.setDocumentation(it, _builder.toString());
              final Procedure1<ITreeAppendable> _function = new Procedure1<ITreeAppendable>() {
                public void apply(final ITreeAppendable it) {
                  StringConcatenation _builder = new StringConcatenation();
                  _builder.append("StringBuilder result = new StringBuilder(super.attributesToString());");
                  _builder.newLine();
                  {
                    EList<EObject> _features = element.getFeatures();
                    Iterable<Attribute> _filter = Iterables.<Attribute>filter(_features, Attribute.class);
                    for(final Attribute attr : _filter) {
                      _builder.append("result.append(\"");
                      String _name = attr.getName();
                      _builder.append(_name, "");
                      _builder.append("  = \").append(this.");
                      String _name_1 = attr.getName();
                      _builder.append(_name_1, "");
                      _builder.append(");");
                      _builder.newLineIfNotEmpty();
                    }
                  }
                  _builder.append("return result.toString();");
                  it.append(_builder);
                }
              };
              SARLJvmModelInferrer.this._jvmTypesBuilder.setBody(it, _function);
            }
          };
          JvmOperation _method = SARLJvmModelInferrer.this._jvmTypesBuilder.toMethod(element, "attributesToString", _newTypeRef, _function);
          SARLJvmModelInferrer.this._jvmTypesBuilder.<JvmOperation>operator_add(_members_2, _method);
        }
        final long serialValue = serial;
        EList<JvmMember> _members_3 = it.getMembers();
        JvmTypeReference _newTypeRef_1 = SARLJvmModelInferrer.this._jvmTypesBuilder.newTypeRef(it, long.class);
        final Procedure1<JvmField> _function_1 = new Procedure1<JvmField>() {
          public void apply(final JvmField it) {
            it.setVisibility(JvmVisibility.PRIVATE);
            it.setFinal(true);
            it.setStatic(true);
            final Procedure1<ITreeAppendable> _function = new Procedure1<ITreeAppendable>() {
              public void apply(final ITreeAppendable it) {
                String _plus = (Long.valueOf(serialValue) + "L");
                it.append(_plus);
              }
            };
            SARLJvmModelInferrer.this._jvmTypesBuilder.setInitializer(it, _function);
          }
        };
        JvmField _field = SARLJvmModelInferrer.this._jvmTypesBuilder.toField(element, "serialVersionUID", _newTypeRef_1, _function_1);
        SARLJvmModelInferrer.this._jvmTypesBuilder.<JvmField>operator_add(_members_3, _field);
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
        SARLJvmModelInferrer.this.sarlSignatureProvider.resetSignatures(it);
        String _documentation = SARLJvmModelInferrer.this._jvmTypesBuilder.getDocumentation(capacity);
        SARLJvmModelInferrer.this._jvmTypesBuilder.setDocumentation(it, _documentation);
        SARLJvmModelInferrer.this.generateSuperTypes(it, capacity, io.sarl.lang.core.Capacity.class);
        EList<EObject> _features = capacity.getFeatures();
        for (final EObject feature : _features) {
          SARLJvmModelInferrer.this.generateAction(it, ((ActionSignature) feature), null);
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
        SARLJvmModelInferrer.this.sarlSignatureProvider.resetSignatures(it);
        String _documentation = SARLJvmModelInferrer.this._jvmTypesBuilder.getDocumentation(element);
        SARLJvmModelInferrer.this._jvmTypesBuilder.setDocumentation(it, _documentation);
        EList<JvmTypeReference> _superTypes = it.getSuperTypes();
        JvmTypeReference _newTypeRef = SARLJvmModelInferrer.this._jvmTypesBuilder.newTypeRef(element, io.sarl.lang.core.Skill.class);
        SARLJvmModelInferrer.this._jvmTypesBuilder.<JvmTypeReference>operator_add(_superTypes, _newTypeRef);
        EList<InheritingElement> _implementedTypes = element.getImplementedTypes();
        for (final InheritingElement cap : _implementedTypes) {
          String _name = cap.getName();
          boolean _notEquals = (!Objects.equal(_name, null));
          if (_notEquals) {
            QualifiedName _fullyQualifiedName = SARLJvmModelInferrer.this._iQualifiedNameProvider.getFullyQualifiedName(cap);
            boolean _notEquals_1 = (!Objects.equal(_fullyQualifiedName, null));
            if (_notEquals_1) {
              EList<JvmTypeReference> _superTypes_1 = it.getSuperTypes();
              QualifiedName _fullyQualifiedName_1 = SARLJvmModelInferrer.this._iQualifiedNameProvider.getFullyQualifiedName(cap);
              String _string = _fullyQualifiedName_1.toString();
              JvmTypeReference _newTypeRef_1 = SARLJvmModelInferrer.this._jvmTypesBuilder.newTypeRef(element, _string);
              SARLJvmModelInferrer.this._jvmTypesBuilder.<JvmTypeReference>operator_add(_superTypes_1, _newTypeRef_1);
            } else {
              String _name_1 = cap.getName();
              String _plus = ("Unable to resolve the fully qualified name of the implemented capacity \'" + _name_1);
              String _plus_1 = (_plus + "\' for the skill:");
              String _name_2 = element.getName();
              String _plus_2 = (_plus_1 + _name_2);
              SARLJvmModelInferrer.this.log.fine(_plus_2);
            }
          } else {
            String _name_3 = element.getName();
            String _plus_3 = ("Unable to resolve an implemented capacity name for the skill:" + _name_3);
            SARLJvmModelInferrer.this.log.fine(_plus_3);
          }
        }
        EList<EObject> _features = element.getFeatures();
        for (final EObject feature : _features) {
          boolean _matched = false;
          if (!_matched) {
            if (feature instanceof Action) {
              _matched=true;
              ParameterizedFeature _signature = ((Action)feature).getSignature();
              XExpression _body = ((Action)feature).getBody();
              SARLJvmModelInferrer.this.generateAction(it, ((ActionSignature) _signature), _body);
            }
          }
          if (!_matched) {
            if (feature instanceof Attribute) {
              _matched=true;
              SARLJvmModelInferrer.this.generateAttribute(it, ((Attribute)feature), JvmVisibility.PROTECTED);
            }
          }
          if (!_matched) {
            if (feature instanceof CapacityUses) {
              _matched=true;
              EList<Capacity> _capacitiesUsed = ((CapacityUses)feature).getCapacitiesUsed();
              for (final Capacity used : _capacitiesUsed) {
                SARLJvmModelInferrer.this.generateCapacityDelegatorMethods(it, element, used);
              }
            }
          }
          if (!_matched) {
            if (feature instanceof Constructor) {
              _matched=true;
              SARLJvmModelInferrer.this.generateConstructor(it, element, ((Constructor)feature));
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
        SARLJvmModelInferrer.this.sarlSignatureProvider.resetSignatures(it);
        String _documentation = SARLJvmModelInferrer.this._jvmTypesBuilder.getDocumentation(element);
        SARLJvmModelInferrer.this._jvmTypesBuilder.setDocumentation(it, _documentation);
        SARLJvmModelInferrer.this.generateSuperTypes(it, element, io.sarl.lang.core.Behavior.class);
        int counter = 1;
        EList<EObject> _features = element.getFeatures();
        for (final EObject feature : _features) {
          boolean _matched = false;
          if (!_matched) {
            if (feature instanceof RequiredCapacity) {
              _matched=true;
            }
          }
          if (!_matched) {
            if (feature instanceof BehaviorUnit) {
              _matched=true;
              final JvmOperation bMethod = SARLJvmModelInferrer.this.generateBehaviorUnit(it, ((BehaviorUnit)feature), counter);
              boolean _tripleNotEquals = (bMethod != null);
              if (_tripleNotEquals) {
                counter = (counter + 1);
                EList<JvmMember> _members = it.getMembers();
                SARLJvmModelInferrer.this._jvmTypesBuilder.<JvmOperation>operator_add(_members, bMethod);
              }
            }
          }
          if (!_matched) {
            if (feature instanceof Action) {
              _matched=true;
              ParameterizedFeature _signature = ((Action)feature).getSignature();
              XExpression _body = ((Action)feature).getBody();
              SARLJvmModelInferrer.this.generateAction(it, ((ActionSignature) _signature), _body);
            }
          }
          if (!_matched) {
            if (feature instanceof CapacityUses) {
              _matched=true;
              EList<Capacity> _capacitiesUsed = ((CapacityUses)feature).getCapacitiesUsed();
              for (final Capacity used : _capacitiesUsed) {
                SARLJvmModelInferrer.this.generateCapacityDelegatorMethods(it, element, used);
              }
            }
          }
          if (!_matched) {
            if (feature instanceof Constructor) {
              _matched=true;
              SARLJvmModelInferrer.this.generateConstructor(it, element, ((Constructor)feature));
            }
          }
          if (!_matched) {
            if (feature instanceof Attribute) {
              _matched=true;
              SARLJvmModelInferrer.this.generateAttribute(it, ((Attribute)feature), JvmVisibility.PROTECTED);
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
        SARLJvmModelInferrer.this.sarlSignatureProvider.resetSignatures(it);
        String _documentation = SARLJvmModelInferrer.this._jvmTypesBuilder.getDocumentation(agent);
        SARLJvmModelInferrer.this._jvmTypesBuilder.setDocumentation(it, _documentation);
        SARLJvmModelInferrer.this.generateSuperTypes(it, agent, io.sarl.lang.core.Agent.class);
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
        int counter = 1;
        EList<EObject> _features = agent.getFeatures();
        for (final EObject feature : _features) {
          boolean _matched = false;
          if (!_matched) {
            if (feature instanceof BehaviorUnit) {
              _matched=true;
              final JvmOperation bMethod = SARLJvmModelInferrer.this.generateBehaviorUnit(it, ((BehaviorUnit)feature), counter);
              boolean _tripleNotEquals = (bMethod != null);
              if (_tripleNotEquals) {
                counter = (counter + 1);
                EList<JvmMember> _members_1 = it.getMembers();
                SARLJvmModelInferrer.this._jvmTypesBuilder.<JvmOperation>operator_add(_members_1, bMethod);
              }
            }
          }
          if (!_matched) {
            if (feature instanceof Action) {
              _matched=true;
              ParameterizedFeature _signature = ((Action)feature).getSignature();
              XExpression _body = ((Action)feature).getBody();
              SARLJvmModelInferrer.this.generateAction(it, ((ActionSignature) _signature), _body);
            }
          }
          if (!_matched) {
            if (feature instanceof Attribute) {
              _matched=true;
              SARLJvmModelInferrer.this.generateAttribute(it, ((Attribute)feature), JvmVisibility.PROTECTED);
            }
          }
          if (!_matched) {
            if (feature instanceof CapacityUses) {
              _matched=true;
              EList<Capacity> _capacitiesUsed = ((CapacityUses)feature).getCapacitiesUsed();
              for (final Capacity used : _capacitiesUsed) {
                SARLJvmModelInferrer.this.generateCapacityDelegatorMethods(it, agent, used);
              }
            }
          }
        }
      }
    };
    _accept.initializeLater(_function);
  }
  
  protected long generateSuperTypes(final JvmGenericType owner, final InheritingElement element, final Class<?> defaultType) {
    long serial = 0L;
    EList<InheritingElement> _superTypes = element.getSuperTypes();
    boolean _isEmpty = _superTypes.isEmpty();
    boolean _not = (!_isEmpty);
    if (_not) {
      EList<InheritingElement> _superTypes_1 = element.getSuperTypes();
      for (final InheritingElement superType : _superTypes_1) {
        boolean _and = false;
        boolean _tripleNotEquals = (superType != null);
        if (!_tripleNotEquals) {
          _and = false;
        } else {
          QualifiedName _fullyQualifiedName = this._iQualifiedNameProvider.getFullyQualifiedName(superType);
          boolean _notEquals = (!Objects.equal(_fullyQualifiedName, null));
          _and = _notEquals;
        }
        if (_and) {
          QualifiedName _fullyQualifiedName_1 = this._iQualifiedNameProvider.getFullyQualifiedName(superType);
          String _string = _fullyQualifiedName_1.toString();
          JvmTypeReference type = this._jvmTypesBuilder.newTypeRef(element, _string);
          EList<JvmTypeReference> _superTypes_2 = owner.getSuperTypes();
          this._jvmTypesBuilder.<JvmTypeReference>operator_add(_superTypes_2, type);
          String _identifier = type.getIdentifier();
          int _hashCode = _identifier.hashCode();
          long _plus = (serial + _hashCode);
          serial = _plus;
        }
      }
    } else {
      JvmTypeReference type_1 = this._jvmTypesBuilder.newTypeRef(element, defaultType);
      EList<JvmTypeReference> _superTypes_3 = owner.getSuperTypes();
      this._jvmTypesBuilder.<JvmTypeReference>operator_add(_superTypes_3, type_1);
      String _identifier_1 = type_1.getIdentifier();
      int _hashCode_1 = _identifier_1.hashCode();
      long _plus_1 = (serial + _hashCode_1);
      serial = _plus_1;
    }
    return serial;
  }
  
  protected JvmField generateAttribute(final JvmGenericType owner, final Attribute attr, final JvmVisibility attrVisibility) {
    String _name = attr.getName();
    JvmTypeReference _type = attr.getType();
    final Procedure1<JvmField> _function = new Procedure1<JvmField>() {
      public void apply(final JvmField it) {
        it.setVisibility(attrVisibility);
        String _documentation = SARLJvmModelInferrer.this._jvmTypesBuilder.getDocumentation(attr);
        SARLJvmModelInferrer.this._jvmTypesBuilder.setDocumentation(it, _documentation);
        boolean _isWriteable = attr.isWriteable();
        boolean _not = (!_isWriteable);
        it.setFinal(_not);
        boolean _and = false;
        boolean _isWriteable_1 = attr.isWriteable();
        boolean _not_1 = (!_isWriteable_1);
        if (!_not_1) {
          _and = false;
        } else {
          XExpression _initialValue = attr.getInitialValue();
          boolean _tripleNotEquals = (_initialValue != null);
          _and = _tripleNotEquals;
        }
        it.setStatic(_and);
        XExpression _initialValue_1 = attr.getInitialValue();
        SARLJvmModelInferrer.this._jvmTypesBuilder.setInitializer(it, _initialValue_1);
      }
    };
    JvmField field = this._jvmTypesBuilder.toField(attr, _name, _type, _function);
    EList<JvmMember> _members = owner.getMembers();
    this._jvmTypesBuilder.<JvmField>operator_add(_members, field);
    return field;
  }
  
  protected void iterateOnActions(final Capacity capacity, final Procedure2<? super Capacity, ? super Collection<ActionSignature>> func) {
    final LinkedList<InheritingElement> caps = new LinkedList<InheritingElement>();
    caps.add(capacity);
    boolean _isEmpty = caps.isEmpty();
    boolean _not = (!_isEmpty);
    boolean _while = _not;
    while (_while) {
      {
        InheritingElement cap = caps.removeFirst();
        if ((cap instanceof Capacity)) {
          EList<InheritingElement> _superTypes = ((Capacity)cap).getSuperTypes();
          caps.addAll(_superTypes);
          ArrayList<ActionSignature> list = new ArrayList<ActionSignature>();
          EList<EObject> _features = ((Capacity)cap).getFeatures();
          for (final EObject sig : _features) {
            list.add(((ActionSignature) sig));
          }
          func.apply(((Capacity)cap), list);
        }
      }
      boolean _isEmpty_1 = caps.isEmpty();
      boolean _not_1 = (!_isEmpty_1);
      _while = _not_1;
    }
  }
  
  private String secureTypeName(final NamedElement o) {
    QualifiedName name = this._iQualifiedNameProvider.getFullyQualifiedName(o);
    boolean _tripleNotEquals = (name != null);
    if (_tripleNotEquals) {
      return name.toString();
    }
    String sname = o.getName();
    boolean _tripleNotEquals_1 = (sname != null);
    if (_tripleNotEquals_1) {
      return sname;
    }
    this.log.finer(("Cannot determine the fully qualified name of: " + o));
    return o.toString();
  }
  
  protected void extractCapacityActions(final Capacity capacity, final Set<ActionSignature> functions, final Map<String, Collection<? extends ActionSignature>> functionsPerCapacity) {
    final Procedure2<Capacity, Collection<? extends ActionSignature>> _function = new Procedure2<Capacity, Collection<? extends ActionSignature>>() {
      public void apply(final Capacity c, final Collection<? extends ActionSignature> l) {
        boolean _tripleNotEquals = (functions != null);
        if (_tripleNotEquals) {
          functions.addAll(l);
        }
        boolean _tripleNotEquals_1 = (functionsPerCapacity != null);
        if (_tripleNotEquals_1) {
          String _secureTypeName = SARLJvmModelInferrer.this.secureTypeName(c);
          functionsPerCapacity.put(_secureTypeName, l);
        }
      }
    };
    this.iterateOnActions(capacity, _function);
  }
  
  protected void generateCapacityDelegatorMethods(final JvmGenericType owner, final InheritingElement context, final Capacity capacity) {
    ActionSignatureComparator _actionSignatureComparator = new ActionSignatureComparator();
    final TreeSet<ActionSignature> functions = new TreeSet<ActionSignature>(_actionSignatureComparator);
    final TreeMap<String, Collection<? extends ActionSignature>> functionsPerCapacity = new TreeMap<String, Collection<? extends ActionSignature>>();
    this.extractCapacityActions(capacity, functions, functionsPerCapacity);
    EList<InheritingElement> _superTypes = context.getSuperTypes();
    final LinkedList<InheritingElement> classes = new LinkedList<InheritingElement>(_superTypes);
    boolean _isEmpty = classes.isEmpty();
    boolean _not = (!_isEmpty);
    boolean _while = _not;
    while (_while) {
      {
        final InheritingElement superClass = classes.removeFirst();
        EList<InheritingElement> _superTypes_1 = superClass.getSuperTypes();
        classes.addAll(_superTypes_1);
        EList<EObject> _features = superClass.getFeatures();
        for (final EObject feature : _features) {
          if ((feature instanceof ActionSignature)) {
            functions.remove(feature);
          } else {
            if ((feature instanceof CapacityUses)) {
              final LinkedList<Capacity> caps = new LinkedList<Capacity>();
              EList<Capacity> _capacitiesUsed = ((CapacityUses)feature).getCapacitiesUsed();
              caps.addAll(_capacitiesUsed);
              boolean _isEmpty_1 = caps.isEmpty();
              boolean _not_1 = (!_isEmpty_1);
              boolean _while_1 = _not_1;
              while (_while_1) {
                {
                  final Capacity cap = caps.removeFirst();
                  EList<InheritingElement> _superTypes_2 = cap.getSuperTypes();
                  for (final InheritingElement s : _superTypes_2) {
                    if ((s instanceof Capacity)) {
                      caps.add(((Capacity)s));
                    }
                  }
                  String _secureTypeName = this.secureTypeName(cap);
                  Collection<? extends ActionSignature> list = functionsPerCapacity.get(_secureTypeName);
                  boolean _tripleEquals = (list == null);
                  if (_tripleEquals) {
                    final Procedure2<Capacity, Collection<? extends ActionSignature>> _function = new Procedure2<Capacity, Collection<? extends ActionSignature>>() {
                      public void apply(final Capacity c, final Collection<? extends ActionSignature> l) {
                        String _secureTypeName = SARLJvmModelInferrer.this.secureTypeName(c);
                        functionsPerCapacity.put(_secureTypeName, l);
                        functions.removeAll(l);
                      }
                    };
                    this.iterateOnActions(cap, _function);
                  } else {
                    functions.removeAll(list);
                  }
                }
                boolean _isEmpty_2 = caps.isEmpty();
                boolean _not_2 = (!_isEmpty_2);
                _while_1 = _not_2;
              }
            }
          }
        }
      }
      boolean _isEmpty_1 = classes.isEmpty();
      boolean _not_1 = (!_isEmpty_1);
      _while = _not_1;
    }
    for (final ActionSignature signature : functions) {
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
          _builder_1.append("getSkill(");
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
          EList<FormalParameter> _params = signature.getParams();
          final Function1<FormalParameter, String> _function = new Function1<FormalParameter, String>() {
            public String apply(final FormalParameter it) {
              return it.getName();
            }
          };
          String _join = IterableExtensions.<FormalParameter>join(_params, ", ", _function);
          it.append(_join);
          it.append(");");
        }
      };
      this._jvmTypesBuilder.setBody(_generateAction, _function);
    }
  }
  
  protected JvmOperation generateBehaviorUnit(final JvmGenericType owner, final BehaviorUnit unit, final int index) {
    Event _event = unit.getEvent();
    final String eventName = _event.getName();
    boolean _and = false;
    boolean _tripleNotEquals = (eventName != null);
    if (!_tripleNotEquals) {
      _and = false;
    } else {
      boolean _isEmpty = eventName.isEmpty();
      boolean _not = (!_isEmpty);
      _and = _not;
    }
    if (_and) {
      Event _event_1 = unit.getEvent();
      String _name = _event_1.getName();
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
          JvmFormalParameter _parameter = SARLJvmModelInferrer.this._jvmTypesBuilder.toParameter(_event, SARLKeywords.OCCURRENCE, _newTypeRef);
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
            JvmFormalParameter _parameter = SARLJvmModelInferrer.this._jvmTypesBuilder.toParameter(_event, SARLKeywords.OCCURRENCE, _newTypeRef);
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
            _builder.append(SARLKeywords.OCCURRENCE, "");
            _builder.append(")) { ");
            it.append(_builder);
            XExpression _body = unit.getBody();
            JvmTypeReference _newTypeRef = SARLJvmModelInferrer.this._jvmTypesBuilder.newTypeRef(behaviorMethod, Void.TYPE);
            LightweightTypeReference _lightweightTypeReference = SARLJvmModelInferrer.this.toLightweightTypeReference(_newTypeRef);
            SARLJvmModelInferrer.this.xbaseCompiler.compile(_body, it, _lightweightTypeReference);
            it.append("}");
          }
        };
        this._jvmTypesBuilder.setBody(behaviorMethod, _function_2);
        EList<JvmMember> _members = owner.getMembers();
        this._jvmTypesBuilder.<JvmOperation>operator_add(_members, guardMethod);
      }
      return behaviorMethod;
    }
    this.log.fine("Unable to resolve the event for a behavior unit");
    return null;
  }
  
  protected List<String> generateFormalParametersWithoutDefaultValue(final JvmExecutable owner, final boolean varargs, final List<FormalParameter> params) {
    ArrayList<String> parameterTypes = new ArrayList<String>();
    JvmFormalParameter lastParam = null;
    for (final FormalParameter param : params) {
      {
        String _name = param.getName();
        JvmTypeReference _parameterType = param.getParameterType();
        JvmFormalParameter _parameter = this._jvmTypesBuilder.toParameter(param, _name, _parameterType);
        lastParam = _parameter;
        EList<JvmFormalParameter> _parameters = owner.getParameters();
        this._jvmTypesBuilder.<JvmFormalParameter>operator_add(_parameters, lastParam);
        JvmTypeReference _parameterType_1 = param.getParameterType();
        String _identifier = _parameterType_1.getIdentifier();
        parameterTypes.add(_identifier);
      }
    }
    boolean _and = false;
    if (!varargs) {
      _and = false;
    } else {
      boolean _tripleNotEquals = (lastParam != null);
      _and = _tripleNotEquals;
    }
    if (_and) {
      JvmTypeReference _parameterType = lastParam.getParameterType();
      JvmTypeReference _addArrayTypeDimension = this._jvmTypesBuilder.addArrayTypeDimension(_parameterType);
      lastParam.setParameterType(_addArrayTypeDimension);
    }
    return parameterTypes;
  }
  
  protected List<String> generateFormalParametersWithDefaultValue(final JvmExecutable owner, final boolean varargs, final List<InferredStandardParameter> signature) {
    JvmFormalParameter lastParam = null;
    final ArrayList<String> arguments = new ArrayList<String>();
    for (final InferredStandardParameter parameterSpec : signature) {
      if ((parameterSpec instanceof InferredValuedParameter)) {
        boolean treated = false;
        XExpression expr = ((InferredValuedParameter)parameterSpec).getExpr();
        if ((expr instanceof XStringLiteral)) {
          JvmTypeReference _type = ((InferredValuedParameter)parameterSpec).getType();
          final String id = _type.getIdentifier();
          boolean _or = false;
          boolean _equals = Objects.equal(id, "char");
          if (_equals) {
            _or = true;
          } else {
            boolean _equals_1 = Objects.equal(id, "java.lang.Character");
            _or = _equals_1;
          }
          if (_or) {
            treated = true;
            String str = ((XStringLiteral)expr).getValue();
            int _length = str.length();
            boolean _greaterThan = (_length > 0);
            if (_greaterThan) {
              char _charAt = str.charAt(0);
              String _plus = ("\'" + Character.valueOf(_charAt));
              String _plus_1 = (_plus + "\'");
              arguments.add(_plus_1);
            } else {
              arguments.add("\\0");
            }
          }
        }
        final FakeTreeAppendable jExpr = new FakeTreeAppendable();
        XExpression _expr = ((InferredValuedParameter)parameterSpec).getExpr();
        JvmTypeReference _type_1 = ((InferredValuedParameter)parameterSpec).getType();
        this.xbaseCompiler.compileAsJavaExpression(_expr, jExpr, _type_1);
        if ((!treated)) {
          String _content = jExpr.getContent();
          arguments.add(_content);
        }
        XExpression _expr_1 = ((InferredValuedParameter)parameterSpec).getExpr();
        this._jvmTypesBuilder.<JvmExecutable>associate(_expr_1, owner);
      } else {
        final FormalParameter param = parameterSpec.getParameter();
        String _name = param.getName();
        JvmTypeReference _parameterType = param.getParameterType();
        JvmFormalParameter _parameter = this._jvmTypesBuilder.toParameter(param, _name, _parameterType);
        lastParam = _parameter;
        EList<JvmFormalParameter> _parameters = owner.getParameters();
        this._jvmTypesBuilder.<JvmFormalParameter>operator_add(_parameters, lastParam);
        String _name_1 = param.getName();
        arguments.add(_name_1);
      }
    }
    boolean _and = false;
    if (!varargs) {
      _and = false;
    } else {
      boolean _tripleNotEquals = (lastParam != null);
      _and = _tripleNotEquals;
    }
    if (_and) {
      JvmTypeReference _parameterType_1 = lastParam.getParameterType();
      JvmTypeReference _addArrayTypeDimension = this._jvmTypesBuilder.addArrayTypeDimension(_parameterType_1);
      lastParam.setParameterType(_addArrayTypeDimension);
    }
    return arguments;
  }
  
  protected JvmOperation generateAction(final JvmGenericType owner, final ActionSignature signature, final XExpression operationBody) {
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
        boolean _isVarargs = signature.isVarargs();
        it.setVarArgs(_isVarargs);
        boolean _isVarargs_1 = signature.isVarargs();
        EList<FormalParameter> _params = signature.getParams();
        SARLJvmModelInferrer.this.generateFormalParametersWithoutDefaultValue(it, _isVarargs_1, _params);
        SARLJvmModelInferrer.this._jvmTypesBuilder.setBody(it, operationBody);
      }
    };
    JvmOperation op = this._jvmTypesBuilder.toMethod(owner, _name, returnType, _function);
    EList<JvmMember> _members = owner.getMembers();
    this._jvmTypesBuilder.<JvmOperation>operator_add(_members, op);
    String _name_1 = signature.getName();
    ActionNameKey _createFunctionID = this.sarlSignatureProvider.createFunctionID(owner, _name_1);
    boolean _isVarargs = signature.isVarargs();
    EList<FormalParameter> _params = signature.getParams();
    final InferredActionSignature otherSignatures = this.sarlSignatureProvider.createSignature(_createFunctionID, _isVarargs, _params);
    for (final EList<InferredStandardParameter> otherSignature : otherSignatures) {
      {
        String _name_2 = signature.getName();
        final Procedure1<JvmOperation> _function_1 = new Procedure1<JvmOperation>() {
          public void apply(final JvmOperation it) {
            String _documentation = SARLJvmModelInferrer.this._jvmTypesBuilder.getDocumentation(signature);
            SARLJvmModelInferrer.this._jvmTypesBuilder.setDocumentation(it, _documentation);
            boolean _isVarargs = signature.isVarargs();
            it.setVarArgs(_isVarargs);
            it.setFinal(true);
            boolean _isVarargs_1 = signature.isVarargs();
            final List<String> args = SARLJvmModelInferrer.this.generateFormalParametersWithDefaultValue(it, _isVarargs_1, otherSignature);
            final Procedure1<ITreeAppendable> _function = new Procedure1<ITreeAppendable>() {
              public void apply(final ITreeAppendable it) {
                String _name = signature.getName();
                it.append(_name);
                it.append("(");
                String _join = IterableExtensions.join(args, ", ");
                it.append(_join);
                it.append(");");
              }
            };
            SARLJvmModelInferrer.this._jvmTypesBuilder.setBody(it, _function);
          }
        };
        JvmOperation _method = this._jvmTypesBuilder.toMethod(owner, _name_2, returnType, _function_1);
        op = _method;
        EList<JvmMember> _members_1 = owner.getMembers();
        this._jvmTypesBuilder.<JvmOperation>operator_add(_members_1, op);
      }
    }
    return op;
  }
  
  protected void generateConstructor(final JvmGenericType owner, final TopElement context, final Constructor constructor) {
    EList<JvmMember> _members = owner.getMembers();
    final Procedure1<JvmConstructor> _function = new Procedure1<JvmConstructor>() {
      public void apply(final JvmConstructor it) {
        String _documentation = SARLJvmModelInferrer.this._jvmTypesBuilder.getDocumentation(constructor);
        SARLJvmModelInferrer.this._jvmTypesBuilder.setDocumentation(it, _documentation);
        boolean _isVarargs = constructor.isVarargs();
        it.setVarArgs(_isVarargs);
        boolean _isVarargs_1 = constructor.isVarargs();
        EList<FormalParameter> _params = constructor.getParams();
        SARLJvmModelInferrer.this.generateFormalParametersWithoutDefaultValue(it, _isVarargs_1, _params);
        XExpression _body = constructor.getBody();
        SARLJvmModelInferrer.this._jvmTypesBuilder.setBody(it, _body);
      }
    };
    JvmConstructor _constructor = this._jvmTypesBuilder.toConstructor(context, _function);
    this._jvmTypesBuilder.<JvmConstructor>operator_add(_members, _constructor);
    ActionNameKey _createConstructorID = this.sarlSignatureProvider.createConstructorID(owner);
    boolean _isVarargs = constructor.isVarargs();
    EList<FormalParameter> _params = constructor.getParams();
    final InferredActionSignature otherSignatures = this.sarlSignatureProvider.createSignature(_createConstructorID, _isVarargs, _params);
    for (final EList<InferredStandardParameter> otherSignature : otherSignatures) {
      EList<JvmMember> _members_1 = owner.getMembers();
      final Procedure1<JvmConstructor> _function_1 = new Procedure1<JvmConstructor>() {
        public void apply(final JvmConstructor it) {
          String _documentation = SARLJvmModelInferrer.this._jvmTypesBuilder.getDocumentation(constructor);
          SARLJvmModelInferrer.this._jvmTypesBuilder.setDocumentation(it, _documentation);
          boolean _isVarargs = constructor.isVarargs();
          it.setVarArgs(_isVarargs);
          boolean _isVarargs_1 = constructor.isVarargs();
          final List<String> args = SARLJvmModelInferrer.this.generateFormalParametersWithDefaultValue(it, _isVarargs_1, otherSignature);
          final Procedure1<ITreeAppendable> _function = new Procedure1<ITreeAppendable>() {
            public void apply(final ITreeAppendable it) {
              it.append("this(");
              String _join = IterableExtensions.join(args, ", ");
              it.append(_join);
              it.append(");");
            }
          };
          SARLJvmModelInferrer.this._jvmTypesBuilder.setBody(it, _function);
        }
      };
      JvmConstructor _constructor_1 = this._jvmTypesBuilder.toConstructor(owner, _function_1);
      this._jvmTypesBuilder.<JvmConstructor>operator_add(_members_1, _constructor_1);
    }
  }
  
  /**
   * FIXME: Remove this function if it is fixed in Xtext: https://bugs.eclipse.org/bugs/show_bug.cgi?id=392440
   * 
   * Copied/pasted from {@link JvmTypesBuilder#toHashCodeMethod(EObject, boolean, JvmField...)}.
   * Updated for fixing the issue {@link "https://bugs.eclipse.org/bugs/show_bug.cgi?id=392440"}
   * 
   * @param owner
   * @param sourceElement
   * @param extendsSomethingWithProperHashCode
   * @param jvmFields
   * @return the operation.
   */
  protected JvmOperation toHashCodeMethod_Bug392440(final JvmGenericType owner, final EObject sourceElement, final boolean extendsSomethingWithProperHashCode, final JvmField... jvmFields) {
    boolean _tripleEquals = (sourceElement == null);
    if (_tripleEquals) {
      return null;
    }
    JvmTypeReference _newTypeRef = this._jvmTypesBuilder.newTypeRef(sourceElement, Integer.TYPE);
    JvmOperation result = this._jvmTypesBuilder.toMethod(sourceElement, "hashCode", _newTypeRef, null);
    boolean _tripleEquals_1 = (result == null);
    if (_tripleEquals_1) {
      return null;
    }
    EList<JvmAnnotationReference> _annotations = result.getAnnotations();
    JvmAnnotationReference _annotation = this._jvmTypesBuilder.toAnnotation(sourceElement, Override.class);
    _annotations.add(_annotation);
    final Procedure1<ITreeAppendable> _function = new Procedure1<ITreeAppendable>() {
      public void apply(final ITreeAppendable it) {
        it.append("final int prime = 31;");
        if (extendsSomethingWithProperHashCode) {
          ITreeAppendable _newLine = it.newLine();
          _newLine.append("int result = super.hashCode();");
        } else {
          ITreeAppendable _newLine_1 = it.newLine();
          _newLine_1.append("int result = 1;");
        }
        for (final JvmField field : jvmFields) {
          {
            JvmTypeReference _type = field.getType();
            String typeName = _type.getIdentifier();
            String _name = Boolean.TYPE.getName();
            boolean _equals = Objects.equal(_name, typeName);
            if (_equals) {
              ITreeAppendable _newLine_2 = it.newLine();
              String _simpleName = field.getSimpleName();
              String _plus = ("result = prime * result + (this." + _simpleName);
              String _plus_1 = (_plus + " ? 1231 : 1237);");
              _newLine_2.append(_plus_1);
            } else {
              boolean _or = false;
              boolean _or_1 = false;
              boolean _or_2 = false;
              String _name_1 = Integer.TYPE.getName();
              boolean _equals_1 = Objects.equal(_name_1, typeName);
              if (_equals_1) {
                _or_2 = true;
              } else {
                String _name_2 = Character.TYPE.getName();
                boolean _equals_2 = Objects.equal(_name_2, typeName);
                _or_2 = _equals_2;
              }
              if (_or_2) {
                _or_1 = true;
              } else {
                String _name_3 = Byte.TYPE.getName();
                boolean _equals_3 = Objects.equal(_name_3, typeName);
                _or_1 = _equals_3;
              }
              if (_or_1) {
                _or = true;
              } else {
                String _name_4 = Short.TYPE.getName();
                boolean _equals_4 = Objects.equal(_name_4, typeName);
                _or = _equals_4;
              }
              if (_or) {
                ITreeAppendable _newLine_3 = it.newLine();
                String _simpleName_1 = field.getSimpleName();
                String _plus_2 = ("result = prime * result + this." + _simpleName_1);
                String _plus_3 = (_plus_2 + ";");
                _newLine_3.append(_plus_3);
              } else {
                String _name_5 = Long.TYPE.getName();
                boolean _equals_5 = Objects.equal(_name_5, typeName);
                if (_equals_5) {
                  ITreeAppendable _newLine_4 = it.newLine();
                  String _simpleName_2 = field.getSimpleName();
                  String _plus_4 = ("result = prime * result + (int) (this." + _simpleName_2);
                  String _plus_5 = (_plus_4 + " ^ (this.");
                  String _simpleName_3 = field.getSimpleName();
                  String _plus_6 = (_plus_5 + _simpleName_3);
                  String _plus_7 = (_plus_6 + " >>> 32));");
                  _newLine_4.append(_plus_7);
                } else {
                  String _name_6 = Float.TYPE.getName();
                  boolean _equals_6 = Objects.equal(_name_6, typeName);
                  if (_equals_6) {
                    ITreeAppendable _newLine_5 = it.newLine();
                    String _simpleName_4 = field.getSimpleName();
                    String _plus_8 = ("result = prime * result + Float.floatToIntBits(this." + _simpleName_4);
                    String _plus_9 = (_plus_8 + ");");
                    _newLine_5.append(_plus_9);
                  } else {
                    String _name_7 = Double.TYPE.getName();
                    boolean _equals_7 = Objects.equal(_name_7, typeName);
                    if (_equals_7) {
                      ITreeAppendable _newLine_6 = it.newLine();
                      String _simpleName_5 = field.getSimpleName();
                      String _plus_10 = ("result = prime * result + (int) (Double.doubleToLongBits(this." + _simpleName_5);
                      String _plus_11 = (_plus_10 + ") ^ (Double.doubleToLongBits(this.");
                      String _simpleName_6 = field.getSimpleName();
                      String _plus_12 = (_plus_11 + _simpleName_6);
                      String _plus_13 = (_plus_12 + ") >>> 32));");
                      _newLine_6.append(_plus_13);
                    } else {
                      ITreeAppendable _newLine_7 = it.newLine();
                      String _simpleName_7 = field.getSimpleName();
                      String _plus_14 = ("result = prime * result + ((this." + _simpleName_7);
                      String _plus_15 = (_plus_14 + "== null) ? 0 : this.");
                      String _simpleName_8 = field.getSimpleName();
                      String _plus_16 = (_plus_15 + _simpleName_8);
                      String _plus_17 = (_plus_16 + ".hashCode());");
                      _newLine_7.append(_plus_17);
                    }
                  }
                }
              }
            }
          }
        }
        ITreeAppendable _newLine_2 = it.newLine();
        _newLine_2.append("return result;");
      }
    };
    this._jvmTypesBuilder.setBody(result, _function);
    return result;
  }
  
  /**
   * FIXME: Remove this function if it is fixed in Xtext: https://bugs.eclipse.org/bugs/show_bug.cgi?id=434912
   * 
   * Copied/pasted from {@link JvmTypesBuilder#toEquals}.
   * Updated for fixing the issue {@link "https://bugs.eclipse.org/bugs/show_bug.cgi?id=434912"}
   * 
   * @param owner
   * @param sourceElement
   * @param declaredType
   * @param isDelegateToSuperEquals
   * @param jvmFields
   * @return the operation.
   */
  protected JvmOperation toEqualsMethod_Bug434912(final JvmGenericType owner, final EObject sourceElement, final JvmDeclaredType declaredType, final boolean isDelegateToSuperEquals, final JvmField... jvmFields) {
    JvmTypeReference _newTypeRef = this._jvmTypesBuilder.newTypeRef(sourceElement, Boolean.TYPE);
    JvmOperation result = this._jvmTypesBuilder.toMethod(sourceElement, "equals", _newTypeRef, null);
    EList<JvmAnnotationReference> _annotations = result.getAnnotations();
    JvmAnnotationReference _annotation = this._jvmTypesBuilder.toAnnotation(sourceElement, Override.class);
    _annotations.add(_annotation);
    EList<JvmFormalParameter> _parameters = result.getParameters();
    JvmTypeReference _newTypeRef_1 = this._jvmTypesBuilder.newTypeRef(sourceElement, Object.class);
    JvmFormalParameter _parameter = this._jvmTypesBuilder.toParameter(sourceElement, "obj", _newTypeRef_1);
    _parameters.add(_parameter);
    final Procedure1<ITreeAppendable> _function = new Procedure1<ITreeAppendable>() {
      public void apply(final ITreeAppendable it) {
        ITreeAppendable _append = it.append("if (this == obj)");
        _append.increaseIndentation();
        ITreeAppendable _newLine = it.newLine();
        ITreeAppendable _append_1 = _newLine.append("return true;");
        _append_1.decreaseIndentation();
        ITreeAppendable _newLine_1 = it.newLine();
        ITreeAppendable _append_2 = _newLine_1.append("if (obj == null)");
        _append_2.increaseIndentation();
        ITreeAppendable _newLine_2 = it.newLine();
        ITreeAppendable _append_3 = _newLine_2.append("return false;");
        _append_3.decreaseIndentation();
        ITreeAppendable _newLine_3 = it.newLine();
        ITreeAppendable _append_4 = _newLine_3.append("if (getClass() != obj.getClass())");
        _append_4.increaseIndentation();
        ITreeAppendable _newLine_4 = it.newLine();
        ITreeAppendable _append_5 = _newLine_4.append("return false;");
        _append_5.decreaseIndentation();
        if (isDelegateToSuperEquals) {
          ITreeAppendable _newLine_5 = it.newLine();
          ITreeAppendable _append_6 = _newLine_5.append("if (!super.equals(obj))");
          _append_6.increaseIndentation();
          ITreeAppendable _newLine_6 = it.newLine();
          ITreeAppendable _append_7 = _newLine_6.append("return false;");
          _append_7.decreaseIndentation();
        }
        ITreeAppendable _newLine_7 = it.newLine();
        String _simpleName = declaredType.getSimpleName();
        String _plus = (_simpleName + " other = (");
        String _simpleName_1 = declaredType.getSimpleName();
        String _plus_1 = (_plus + _simpleName_1);
        String _plus_2 = (_plus_1 + ") obj;");
        _newLine_7.append(_plus_2);
        for (final JvmField field : jvmFields) {
          {
            JvmTypeReference _type = field.getType();
            String typeName = _type.getIdentifier();
            boolean _or = false;
            boolean _or_1 = false;
            boolean _or_2 = false;
            boolean _or_3 = false;
            boolean _or_4 = false;
            String _name = Boolean.TYPE.getName();
            boolean _equals = Objects.equal(_name, typeName);
            if (_equals) {
              _or_4 = true;
            } else {
              String _name_1 = Integer.TYPE.getName();
              boolean _equals_1 = Objects.equal(_name_1, typeName);
              _or_4 = _equals_1;
            }
            if (_or_4) {
              _or_3 = true;
            } else {
              String _name_2 = Long.TYPE.getName();
              boolean _equals_2 = Objects.equal(_name_2, typeName);
              _or_3 = _equals_2;
            }
            if (_or_3) {
              _or_2 = true;
            } else {
              String _name_3 = Character.TYPE.getName();
              boolean _equals_3 = Objects.equal(_name_3, typeName);
              _or_2 = _equals_3;
            }
            if (_or_2) {
              _or_1 = true;
            } else {
              String _name_4 = Byte.TYPE.getName();
              boolean _equals_4 = Objects.equal(_name_4, typeName);
              _or_1 = _equals_4;
            }
            if (_or_1) {
              _or = true;
            } else {
              String _name_5 = Short.TYPE.getName();
              boolean _equals_5 = Objects.equal(_name_5, typeName);
              _or = _equals_5;
            }
            if (_or) {
              ITreeAppendable _newLine_8 = it.newLine();
              String _simpleName_2 = field.getSimpleName();
              String _plus_3 = ("if (other." + _simpleName_2);
              String _plus_4 = (_plus_3 + " != this.");
              String _simpleName_3 = field.getSimpleName();
              String _plus_5 = (_plus_4 + _simpleName_3);
              String _plus_6 = (_plus_5 + ")");
              ITreeAppendable _append_8 = _newLine_8.append(_plus_6);
              _append_8.increaseIndentation();
              ITreeAppendable _newLine_9 = it.newLine();
              ITreeAppendable _append_9 = _newLine_9.append("return false;");
              _append_9.decreaseIndentation();
            } else {
              String _name_6 = Double.TYPE.getName();
              boolean _equals_6 = Objects.equal(_name_6, typeName);
              if (_equals_6) {
                ITreeAppendable _newLine_10 = it.newLine();
                String _simpleName_4 = field.getSimpleName();
                String _plus_7 = ("if (Double.doubleToLongBits(other." + _simpleName_4);
                String _plus_8 = (_plus_7 + ") != Double.doubleToLongBits(this.");
                String _simpleName_5 = field.getSimpleName();
                String _plus_9 = (_plus_8 + _simpleName_5);
                String _plus_10 = (_plus_9 + "))");
                ITreeAppendable _append_10 = _newLine_10.append(_plus_10);
                _append_10.increaseIndentation();
                ITreeAppendable _newLine_11 = it.newLine();
                ITreeAppendable _append_11 = _newLine_11.append("return false;");
                _append_11.decreaseIndentation();
              } else {
                String _name_7 = Float.TYPE.getName();
                boolean _equals_7 = Objects.equal(_name_7, typeName);
                if (_equals_7) {
                  ITreeAppendable _newLine_12 = it.newLine();
                  String _simpleName_6 = field.getSimpleName();
                  String _plus_11 = ("if (Float.floatToIntBits(other." + _simpleName_6);
                  String _plus_12 = (_plus_11 + ") != Float.floatToIntBits(this.");
                  String _simpleName_7 = field.getSimpleName();
                  String _plus_13 = (_plus_12 + _simpleName_7);
                  String _plus_14 = (_plus_13 + "))");
                  ITreeAppendable _append_12 = _newLine_12.append(_plus_14);
                  _append_12.increaseIndentation();
                  ITreeAppendable _newLine_13 = it.newLine();
                  ITreeAppendable _append_13 = _newLine_13.append("return false;");
                  _append_13.decreaseIndentation();
                } else {
                  ITreeAppendable _newLine_14 = it.newLine();
                  String _simpleName_8 = field.getSimpleName();
                  String _plus_15 = ("if (this." + _simpleName_8);
                  String _plus_16 = (_plus_15 + " == null) {");
                  ITreeAppendable _append_14 = _newLine_14.append(_plus_16);
                  _append_14.increaseIndentation();
                  ITreeAppendable _newLine_15 = it.newLine();
                  String _simpleName_9 = field.getSimpleName();
                  String _plus_17 = ("if (other." + _simpleName_9);
                  String _plus_18 = (_plus_17 + " != null)");
                  ITreeAppendable _append_15 = _newLine_15.append(_plus_18);
                  _append_15.increaseIndentation();
                  ITreeAppendable _newLine_16 = it.newLine();
                  ITreeAppendable _append_16 = _newLine_16.append("return false;");
                  _append_16.decreaseIndentation();
                  it.decreaseIndentation();
                  ITreeAppendable _newLine_17 = it.newLine();
                  String _simpleName_10 = field.getSimpleName();
                  String _plus_19 = ("} else if (!this." + _simpleName_10);
                  String _plus_20 = (_plus_19 + ".equals(other.");
                  String _simpleName_11 = field.getSimpleName();
                  String _plus_21 = (_plus_20 + _simpleName_11);
                  String _plus_22 = (_plus_21 + "))");
                  ITreeAppendable _append_17 = _newLine_17.append(_plus_22);
                  _append_17.increaseIndentation();
                  ITreeAppendable _newLine_18 = it.newLine();
                  ITreeAppendable _append_18 = _newLine_18.append("return false;");
                  _append_18.decreaseIndentation();
                }
              }
            }
          }
        }
        ITreeAppendable _newLine_8 = it.newLine();
        _newLine_8.append("return true;");
      }
    };
    this._jvmTypesBuilder.setBody(result, _function);
    return result;
  }
  
  protected LightweightTypeReference toLightweightTypeReference(final JvmTypeReference typeRef) {
    return this.toLightweightTypeReference(typeRef, false);
  }
  
  protected LightweightTypeReference toLightweightTypeReference(final JvmTypeReference typeRef, final boolean keepUnboundWildcardInformation) {
    StandardTypeReferenceOwner _standardTypeReferenceOwner = new StandardTypeReferenceOwner(this.services, typeRef);
    OwnedConverter converter = new OwnedConverter(_standardTypeReferenceOwner, keepUnboundWildcardInformation);
    LightweightTypeReference reference = converter.toLightweightReference(typeRef);
    return reference;
  }
  
  public void infer(final EObject element, final IJvmDeclaredTypeAcceptor acceptor, final boolean isPreIndexingPhase) {
    if (element instanceof Skill) {
      _infer((Skill)element, acceptor, isPreIndexingPhase);
      return;
    } else if (element instanceof Agent) {
      _infer((Agent)element, acceptor, isPreIndexingPhase);
      return;
    } else if (element instanceof Behavior) {
      _infer((Behavior)element, acceptor, isPreIndexingPhase);
      return;
    } else if (element instanceof Capacity) {
      _infer((Capacity)element, acceptor, isPreIndexingPhase);
      return;
    } else if (element instanceof Event) {
      _infer((Event)element, acceptor, isPreIndexingPhase);
      return;
    } else if (element != null) {
      _infer(element, acceptor, isPreIndexingPhase);
      return;
    } else {
      throw new IllegalArgumentException("Unhandled parameter types: " +
        Arrays.<Object>asList(element, acceptor, isPreIndexingPhase).toString());
    }
  }
}
