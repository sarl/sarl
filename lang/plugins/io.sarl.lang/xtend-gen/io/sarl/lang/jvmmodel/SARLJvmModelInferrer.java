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
import io.sarl.lang.annotation.DefaultValue;
import io.sarl.lang.annotation.DefaultValueSource;
import io.sarl.lang.annotation.DefaultValueUse;
import io.sarl.lang.annotation.Generated;
import io.sarl.lang.bugfixes.XtendBug392440;
import io.sarl.lang.bugfixes.XtendBug434912;
import io.sarl.lang.core.Address;
import io.sarl.lang.core.Agent;
import io.sarl.lang.core.Percept;
import io.sarl.lang.sarl.Action;
import io.sarl.lang.sarl.ActionSignature;
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
import io.sarl.lang.signature.ActionKey;
import io.sarl.lang.signature.ActionNameKey;
import io.sarl.lang.signature.ActionSignatureComparator;
import io.sarl.lang.signature.ActionSignatureProvider;
import io.sarl.lang.signature.InferredActionSignature;
import io.sarl.lang.signature.InferredStandardParameter;
import io.sarl.lang.signature.InferredValuedParameter;
import io.sarl.lang.signature.SignatureKey;
import io.sarl.lang.util.ModelUtil;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.Iterator;
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
import org.eclipse.xtext.common.types.JvmAnnotationTarget;
import org.eclipse.xtext.common.types.JvmAnnotationType;
import org.eclipse.xtext.common.types.JvmAnnotationValue;
import org.eclipse.xtext.common.types.JvmConstructor;
import org.eclipse.xtext.common.types.JvmDeclaredType;
import org.eclipse.xtext.common.types.JvmExecutable;
import org.eclipse.xtext.common.types.JvmField;
import org.eclipse.xtext.common.types.JvmFormalParameter;
import org.eclipse.xtext.common.types.JvmGenericType;
import org.eclipse.xtext.common.types.JvmMember;
import org.eclipse.xtext.common.types.JvmOperation;
import org.eclipse.xtext.common.types.JvmStringAnnotationValue;
import org.eclipse.xtext.common.types.JvmType;
import org.eclipse.xtext.common.types.JvmTypeReference;
import org.eclipse.xtext.common.types.JvmVisibility;
import org.eclipse.xtext.naming.IQualifiedNameProvider;
import org.eclipse.xtext.naming.QualifiedName;
import org.eclipse.xtext.xbase.XExpression;
import org.eclipse.xtext.xbase.compiler.XbaseCompiler;
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
import org.eclipse.xtext.xbase.validation.ReadAndWriteTracking;

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
  private ReadAndWriteTracking readAndWriteTracking;
  
  @Inject
  private CommonTypeComputationServices services;
  
  private XtendBug392440 hashCodeBugFix;
  
  private XtendBug434912 toEqualsBugFix;
  
  @Inject
  public void setTypesBuilder(final JvmTypesBuilder typesBuilder) {
    XtendBug392440 _xtendBug392440 = new XtendBug392440(typesBuilder);
    this.hashCodeBugFix = _xtendBug392440;
    XtendBug434912 _xtendBug434912 = new XtendBug434912(typesBuilder);
    this.toEqualsBugFix = _xtendBug434912;
  }
  
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
  protected void _infer(final Event event, final IJvmDeclaredTypeAcceptor acceptor, final boolean isPreIndexingPhase) {
    QualifiedName _fullyQualifiedName = this._iQualifiedNameProvider.getFullyQualifiedName(event);
    JvmGenericType _class = this._jvmTypesBuilder.toClass(event, _fullyQualifiedName);
    IJvmDeclaredTypeAcceptor.IPostIndexingInitializing<JvmGenericType> _accept = acceptor.<JvmGenericType>accept(_class);
    final Procedure1<JvmGenericType> _function = new Procedure1<JvmGenericType>() {
      public void apply(final JvmGenericType it) {
        SARLJvmModelInferrer.this.sarlSignatureProvider.resetSignatures(it);
        SARLJvmModelInferrer.this._jvmTypesBuilder.copyDocumentationTo(event, it);
        long serial = 1L;
        long _generateSuperTypes = SARLJvmModelInferrer.this.generateSuperTypes(it, event, io.sarl.lang.core.Event.class);
        long _plus = (serial + _generateSuperTypes);
        serial = _plus;
        JvmField jvmField = null;
        List<JvmField> jvmFields = new ArrayList<JvmField>();
        int actionIndex = 0;
        boolean hasConstructor = false;
        EList<EObject> _features = event.getFeatures();
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
              SARLJvmModelInferrer.this.generateConstructor(it, event, ((Constructor)feature), actionIndex);
              QualifiedName _fullyQualifiedName = SARLJvmModelInferrer.this._iQualifiedNameProvider.getFullyQualifiedName(event);
              int _hashCode = _fullyQualifiedName.hashCode();
              long _plus_1 = (serial + _hashCode);
              serial = _plus_1;
              actionIndex++;
              hasConstructor = true;
            }
          }
        }
        if ((!hasConstructor)) {
          final Procedure1<JvmConstructor> _function = new Procedure1<JvmConstructor>() {
            public void apply(final JvmConstructor it) {
              StringConcatenation _builder = new StringConcatenation();
              _builder.append("Construct an event. The source of the event is unknown.");
              _builder.newLine();
              SARLJvmModelInferrer.this._jvmTypesBuilder.setDocumentation(it, _builder.toString());
              StringConcatenationClient _client = new StringConcatenationClient() {
                @Override
                protected void appendTo(StringConcatenationClient.TargetStringConcatenation _builder) {
                  _builder.append("super();");
                  _builder.newLine();
                }
              };
              SARLJvmModelInferrer.this._jvmTypesBuilder.setBody(it, _client);
            }
          };
          JvmConstructor op = SARLJvmModelInferrer.this._jvmTypesBuilder.toConstructor(event, _function);
          EList<JvmAnnotationReference> _annotations = op.getAnnotations();
          JvmAnnotationReference _annotation = SARLJvmModelInferrer.this._jvmTypesBuilder.toAnnotation(it, Generated.class);
          SARLJvmModelInferrer.this._jvmTypesBuilder.<JvmAnnotationReference>operator_add(_annotations, _annotation);
          EList<JvmMember> _members = it.getMembers();
          SARLJvmModelInferrer.this._jvmTypesBuilder.<JvmConstructor>operator_add(_members, op);
          final Procedure1<JvmConstructor> _function_1 = new Procedure1<JvmConstructor>() {
            public void apply(final JvmConstructor it) {
              StringConcatenation _builder = new StringConcatenation();
              _builder.append("Construct an event.");
              _builder.newLine();
              _builder.append("@param source - address of the agent that is emitting this event.");
              _builder.newLine();
              SARLJvmModelInferrer.this._jvmTypesBuilder.setDocumentation(it, _builder.toString());
              EList<JvmFormalParameter> _parameters = it.getParameters();
              JvmTypeReference _newTypeRef = SARLJvmModelInferrer.this._jvmTypesBuilder.newTypeRef(it, Address.class);
              JvmFormalParameter _parameter = SARLJvmModelInferrer.this._jvmTypesBuilder.toParameter(event, "source", _newTypeRef);
              SARLJvmModelInferrer.this._jvmTypesBuilder.<JvmFormalParameter>operator_add(_parameters, _parameter);
              StringConcatenationClient _client = new StringConcatenationClient() {
                @Override
                protected void appendTo(StringConcatenationClient.TargetStringConcatenation _builder) {
                  _builder.append("super(source);");
                  _builder.newLine();
                }
              };
              SARLJvmModelInferrer.this._jvmTypesBuilder.setBody(it, _client);
            }
          };
          JvmConstructor _constructor = SARLJvmModelInferrer.this._jvmTypesBuilder.toConstructor(event, _function_1);
          op = _constructor;
          EList<JvmAnnotationReference> _annotations_1 = op.getAnnotations();
          JvmAnnotationReference _annotation_1 = SARLJvmModelInferrer.this._jvmTypesBuilder.toAnnotation(it, Generated.class);
          SARLJvmModelInferrer.this._jvmTypesBuilder.<JvmAnnotationReference>operator_add(_annotations_1, _annotation_1);
          EList<JvmMember> _members_1 = it.getMembers();
          SARLJvmModelInferrer.this._jvmTypesBuilder.<JvmConstructor>operator_add(_members_1, op);
        }
        boolean _isEmpty = jvmFields.isEmpty();
        boolean _not = (!_isEmpty);
        if (_not) {
          final JvmField[] tab = ((JvmField[])Conversions.unwrapArray(jvmFields, JvmField.class));
          QualifiedName _fullyQualifiedName = SARLJvmModelInferrer.this._iQualifiedNameProvider.getFullyQualifiedName(event);
          JvmGenericType elementType = SARLJvmModelInferrer.this._jvmTypesBuilder.toClass(event, _fullyQualifiedName);
          JvmOperation op_1 = SARLJvmModelInferrer.this.toEqualsBugFix.toEqualsMethod(it, event, elementType, true, tab);
          EList<JvmAnnotationReference> _annotations_2 = op_1.getAnnotations();
          JvmAnnotationReference _annotation_2 = SARLJvmModelInferrer.this._jvmTypesBuilder.toAnnotation(it, Generated.class);
          SARLJvmModelInferrer.this._jvmTypesBuilder.<JvmAnnotationReference>operator_add(_annotations_2, _annotation_2);
          EList<JvmMember> _members_2 = it.getMembers();
          SARLJvmModelInferrer.this._jvmTypesBuilder.<JvmOperation>operator_add(_members_2, op_1);
          JvmOperation _hashCodeMethod = SARLJvmModelInferrer.this.hashCodeBugFix.toHashCodeMethod(it, event, true, tab);
          op_1 = _hashCodeMethod;
          EList<JvmAnnotationReference> _annotations_3 = op_1.getAnnotations();
          JvmAnnotationReference _annotation_3 = SARLJvmModelInferrer.this._jvmTypesBuilder.toAnnotation(it, Generated.class);
          SARLJvmModelInferrer.this._jvmTypesBuilder.<JvmAnnotationReference>operator_add(_annotations_3, _annotation_3);
          EList<JvmMember> _members_3 = it.getMembers();
          SARLJvmModelInferrer.this._jvmTypesBuilder.<JvmOperation>operator_add(_members_3, op_1);
          JvmTypeReference _newTypeRef = SARLJvmModelInferrer.this._jvmTypesBuilder.newTypeRef(it, String.class);
          final Procedure1<JvmOperation> _function_2 = new Procedure1<JvmOperation>() {
            public void apply(final JvmOperation it) {
              it.setVisibility(JvmVisibility.PROTECTED);
              StringConcatenation _builder = new StringConcatenation();
              _builder.append("Returns a String representation of the Event ");
              String _name = event.getName();
              _builder.append(_name, "");
              _builder.append(" attributes only.");
              SARLJvmModelInferrer.this._jvmTypesBuilder.setDocumentation(it, _builder.toString());
              final Procedure1<ITreeAppendable> _function = new Procedure1<ITreeAppendable>() {
                public void apply(final ITreeAppendable it) {
                  StringConcatenation _builder = new StringConcatenation();
                  _builder.append("StringBuilder result = new StringBuilder(super.attributesToString());");
                  _builder.newLine();
                  {
                    EList<EObject> _features = event.getFeatures();
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
          JvmOperation _method = SARLJvmModelInferrer.this._jvmTypesBuilder.toMethod(event, "attributesToString", _newTypeRef, _function_2);
          op_1 = _method;
          EList<JvmAnnotationReference> _annotations_4 = op_1.getAnnotations();
          JvmAnnotationReference _annotation_4 = SARLJvmModelInferrer.this._jvmTypesBuilder.toAnnotation(it, Generated.class);
          SARLJvmModelInferrer.this._jvmTypesBuilder.<JvmAnnotationReference>operator_add(_annotations_4, _annotation_4);
          EList<JvmMember> _members_4 = it.getMembers();
          SARLJvmModelInferrer.this._jvmTypesBuilder.<JvmOperation>operator_add(_members_4, op_1);
        }
        final long serialValue = serial;
        JvmTypeReference _newTypeRef_1 = SARLJvmModelInferrer.this._jvmTypesBuilder.newTypeRef(it, long.class);
        final Procedure1<JvmField> _function_3 = new Procedure1<JvmField>() {
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
        final JvmField serialField = SARLJvmModelInferrer.this._jvmTypesBuilder.toField(event, "serialVersionUID", _newTypeRef_1, _function_3);
        EList<JvmAnnotationReference> _annotations_5 = serialField.getAnnotations();
        JvmAnnotationReference _annotation_5 = SARLJvmModelInferrer.this._jvmTypesBuilder.toAnnotation(it, Generated.class);
        SARLJvmModelInferrer.this._jvmTypesBuilder.<JvmAnnotationReference>operator_add(_annotations_5, _annotation_5);
        EList<JvmMember> _members_5 = it.getMembers();
        SARLJvmModelInferrer.this._jvmTypesBuilder.<JvmField>operator_add(_members_5, serialField);
        SARLJvmModelInferrer.this.readAndWriteTracking.markInitialized(serialField);
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
        SARLJvmModelInferrer.this._jvmTypesBuilder.copyDocumentationTo(capacity, it);
        SARLJvmModelInferrer.this.generateSuperTypes(it, capacity, io.sarl.lang.core.Capacity.class);
        int actionIndex = 0;
        EList<EObject> _features = capacity.getFeatures();
        for (final EObject feature : _features) {
          {
            SARLJvmModelInferrer.this.generateAction(it, ((ActionSignature) feature), null, actionIndex);
            actionIndex++;
          }
        }
      }
    };
    _accept.initializeLater(_function);
  }
  
  protected void _infer(final Skill skill, final IJvmDeclaredTypeAcceptor acceptor, final boolean isPreIndexingPhase) {
    QualifiedName _fullyQualifiedName = this._iQualifiedNameProvider.getFullyQualifiedName(skill);
    JvmGenericType _class = this._jvmTypesBuilder.toClass(skill, _fullyQualifiedName);
    IJvmDeclaredTypeAcceptor.IPostIndexingInitializing<JvmGenericType> _accept = acceptor.<JvmGenericType>accept(_class);
    final Procedure1<JvmGenericType> _function = new Procedure1<JvmGenericType>() {
      public void apply(final JvmGenericType it) {
        SARLJvmModelInferrer.this.sarlSignatureProvider.resetSignatures(it);
        SARLJvmModelInferrer.this._jvmTypesBuilder.copyDocumentationTo(skill, it);
        SARLJvmModelInferrer.this.generateSuperTypes(it, skill, io.sarl.lang.core.Skill.class);
        EList<InheritingElement> _implementedTypes = skill.getImplementedTypes();
        for (final InheritingElement cap : _implementedTypes) {
          boolean _tripleNotEquals = (cap != null);
          if (_tripleNotEquals) {
            QualifiedName capName = SARLJvmModelInferrer.this._iQualifiedNameProvider.getFullyQualifiedName(cap);
            boolean _tripleNotEquals_1 = (capName != null);
            if (_tripleNotEquals_1) {
              EList<JvmTypeReference> _superTypes = it.getSuperTypes();
              String _string = capName.toString();
              JvmTypeReference _newTypeRef = SARLJvmModelInferrer.this._jvmTypesBuilder.newTypeRef(skill, _string);
              SARLJvmModelInferrer.this._jvmTypesBuilder.<JvmTypeReference>operator_add(_superTypes, _newTypeRef);
            }
          }
        }
        final Map<ActionKey, JvmOperation> finalOperations = new TreeMap<ActionKey, JvmOperation>();
        final Map<ActionKey, JvmOperation> overridableOperations = new TreeMap<ActionKey, JvmOperation>();
        final Map<ActionKey, JvmOperation> operationsToImplement = new TreeMap<ActionKey, JvmOperation>();
        ModelUtil.populateInheritanceContext(it, finalOperations, overridableOperations, 
          null, operationsToImplement, 
          null, SARLJvmModelInferrer.this.sarlSignatureProvider);
        int actionIndex = 0;
        boolean hasConstructor = false;
        EList<EObject> _features = skill.getFeatures();
        for (final EObject feature : _features) {
          boolean _matched = false;
          if (!_matched) {
            if (feature instanceof Action) {
              _matched=true;
              ParameterizedFeature _signature = ((Action)feature).getSignature();
              ActionSignature sig = ((ActionSignature) _signature);
              XExpression _body = ((Action)feature).getBody();
              final Function1<ActionKey, Boolean> _function = new Function1<ActionKey, Boolean>() {
                public Boolean apply(final ActionKey it) {
                  boolean _and = false;
                  boolean _containsKey = finalOperations.containsKey(it);
                  boolean _not = (!_containsKey);
                  if (!_not) {
                    _and = false;
                  } else {
                    boolean _containsKey_1 = overridableOperations.containsKey(it);
                    boolean _not_1 = (!_containsKey_1);
                    _and = _not_1;
                  }
                  return _and;
                }
              };
              SARLJvmModelInferrer.this.generateAction(it, sig, _body, actionIndex, false, operationsToImplement, overridableOperations, _function);
              actionIndex++;
            }
          }
          if (!_matched) {
            if (feature instanceof Constructor) {
              _matched=true;
              SARLJvmModelInferrer.this.generateConstructor(it, skill, ((Constructor)feature), actionIndex);
              actionIndex++;
              hasConstructor = true;
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
                int _generateCapacityDelegatorMethods = SARLJvmModelInferrer.this.generateCapacityDelegatorMethods(it, skill, used, actionIndex);
                actionIndex = _generateCapacityDelegatorMethods;
              }
            }
          }
        }
        String currentKeyStr = null;
        JvmOperation originalOperation = null;
        SignatureKey sigKey = null;
        Set<Map.Entry<ActionKey, JvmOperation>> _entrySet = operationsToImplement.entrySet();
        for (final Map.Entry<ActionKey, JvmOperation> missedOperation : _entrySet) {
          {
            JvmOperation _value = missedOperation.getValue();
            String originalSignature = SARLJvmModelInferrer.this.annotationString(_value, DefaultValueUse.class);
            boolean _tripleNotEquals_2 = (originalSignature != null);
            if (_tripleNotEquals_2) {
              boolean _notEquals = (!Objects.equal(originalSignature, currentKeyStr));
              if (_notEquals) {
                currentKeyStr = originalSignature;
                SignatureKey _createSignatureIDFromString = SARLJvmModelInferrer.this.sarlSignatureProvider.createSignatureIDFromString(originalSignature);
                sigKey = _createSignatureIDFromString;
                ActionKey _key = missedOperation.getKey();
                String _functionName = _key.getFunctionName();
                ActionKey key = SARLJvmModelInferrer.this.sarlSignatureProvider.createActionID(_functionName, sigKey);
                JvmOperation _get = overridableOperations.get(key);
                originalOperation = _get;
              }
              boolean _tripleNotEquals_3 = (originalOperation != null);
              if (_tripleNotEquals_3) {
                String _simpleName = originalOperation.getSimpleName();
                JvmTypeReference _returnType = originalOperation.getReturnType();
                JvmOperation op = SARLJvmModelInferrer.this._jvmTypesBuilder.toMethod(skill, _simpleName, _returnType, null);
                boolean _isVarArgs = originalOperation.isVarArgs();
                op.setVarArgs(_isVarArgs);
                op.setFinal(true);
                ArrayList<String> args = new ArrayList<String>();
                JvmOperation _value_1 = missedOperation.getValue();
                EList<JvmFormalParameter> _parameters = _value_1.getParameters();
                Iterator<JvmFormalParameter> it1 = _parameters.iterator();
                EList<JvmFormalParameter> _parameters_1 = originalOperation.getParameters();
                Iterator<JvmFormalParameter> it2 = _parameters_1.iterator();
                JvmFormalParameter oparam = null;
                boolean _hasNext = it2.hasNext();
                boolean _while = _hasNext;
                while (_while) {
                  {
                    JvmFormalParameter param = it2.next();
                    String vId = SARLJvmModelInferrer.this.annotationString(param, DefaultValue.class);
                    boolean _and = false;
                    boolean _equals = Objects.equal(oparam, null);
                    if (!_equals) {
                      _and = false;
                    } else {
                      boolean _hasNext_1 = it1.hasNext();
                      _and = _hasNext_1;
                    }
                    if (_and) {
                      JvmFormalParameter _next = it1.next();
                      oparam = _next;
                    }
                    boolean _and_1 = false;
                    boolean _tripleNotEquals_4 = (oparam != null);
                    if (!_tripleNotEquals_4) {
                      _and_1 = false;
                    } else {
                      String _simpleName_1 = oparam.getSimpleName();
                      String _simpleName_2 = param.getSimpleName();
                      boolean _equals_1 = Objects.equal(_simpleName_1, _simpleName_2);
                      _and_1 = _equals_1;
                    }
                    if (_and_1) {
                      String _simpleName_3 = oparam.getSimpleName();
                      args.add(_simpleName_3);
                      EList<JvmFormalParameter> _parameters_2 = op.getParameters();
                      String _simpleName_4 = oparam.getSimpleName();
                      JvmTypeReference _parameterType = oparam.getParameterType();
                      JvmFormalParameter _parameter = SARLJvmModelInferrer.this._jvmTypesBuilder.toParameter(param, _simpleName_4, _parameterType);
                      SARLJvmModelInferrer.this._jvmTypesBuilder.<JvmFormalParameter>operator_add(_parameters_2, _parameter);
                      oparam = null;
                    } else {
                      boolean _and_2 = false;
                      boolean _tripleNotEquals_5 = (vId != null);
                      if (!_tripleNotEquals_5) {
                        _and_2 = false;
                      } else {
                        boolean _isEmpty = vId.isEmpty();
                        boolean _not = (!_isEmpty);
                        _and_2 = _not;
                      }
                      if (_and_2) {
                        JvmDeclaredType _declaringType = originalOperation.getDeclaringType();
                        String _qualifiedName = _declaringType.getQualifiedName();
                        String _plus = (_qualifiedName + ".___FORMAL_PARAMETER_DEFAULT_VALUE_");
                        String _plus_1 = (_plus + vId);
                        args.add(_plus_1);
                      } else {
                        throw new IllegalStateException("Invalid generation of the default-valued formal parameters");
                      }
                    }
                  }
                  boolean _hasNext_1 = it2.hasNext();
                  _while = _hasNext_1;
                }
                {
                  final String tmpName = originalOperation.getSimpleName();
                  final ArrayList<String> tmpArgs = args;
                  final Procedure1<ITreeAppendable> _function = new Procedure1<ITreeAppendable>() {
                    public void apply(final ITreeAppendable it) {
                      it.append(tmpName);
                      it.append("(");
                      String _join = IterableExtensions.join(tmpArgs, ", ");
                      it.append(_join);
                      it.append(");");
                    }
                  };
                  SARLJvmModelInferrer.this._jvmTypesBuilder.setBody(op, _function);
                }
                EList<JvmAnnotationReference> _annotations = op.getAnnotations();
                JvmAnnotationReference _annotation = SARLJvmModelInferrer.this._jvmTypesBuilder.toAnnotation(skill, DefaultValueUse.class, originalSignature);
                SARLJvmModelInferrer.this._jvmTypesBuilder.<JvmAnnotationReference>operator_add(_annotations, _annotation);
                EList<JvmMember> _members = it.getMembers();
                SARLJvmModelInferrer.this._jvmTypesBuilder.<JvmOperation>operator_add(_members, op);
                actionIndex++;
              }
            }
          }
        }
        if ((!hasConstructor)) {
          EList<JvmMember> _members = it.getMembers();
          final Procedure1<JvmConstructor> _function = new Procedure1<JvmConstructor>() {
            public void apply(final JvmConstructor it) {
              StringConcatenation _builder = new StringConcatenation();
              _builder.append("Construct a skill.");
              _builder.newLine();
              _builder.append("@param owner - agent that is owning this skill. ");
              _builder.newLine();
              SARLJvmModelInferrer.this._jvmTypesBuilder.setDocumentation(it, _builder.toString());
              EList<JvmFormalParameter> _parameters = it.getParameters();
              JvmTypeReference _newTypeRef = SARLJvmModelInferrer.this._jvmTypesBuilder.newTypeRef(it, Agent.class);
              JvmFormalParameter _parameter = SARLJvmModelInferrer.this._jvmTypesBuilder.toParameter(skill, "owner", _newTypeRef);
              SARLJvmModelInferrer.this._jvmTypesBuilder.<JvmFormalParameter>operator_add(_parameters, _parameter);
              StringConcatenationClient _client = new StringConcatenationClient() {
                @Override
                protected void appendTo(StringConcatenationClient.TargetStringConcatenation _builder) {
                  _builder.append("super(owner);");
                  _builder.newLine();
                }
              };
              SARLJvmModelInferrer.this._jvmTypesBuilder.setBody(it, _client);
            }
          };
          JvmConstructor _constructor = SARLJvmModelInferrer.this._jvmTypesBuilder.toConstructor(skill, _function);
          SARLJvmModelInferrer.this._jvmTypesBuilder.<JvmConstructor>operator_add(_members, _constructor);
          EList<JvmMember> _members_1 = it.getMembers();
          final Procedure1<JvmConstructor> _function_1 = new Procedure1<JvmConstructor>() {
            public void apply(final JvmConstructor it) {
              StringConcatenation _builder = new StringConcatenation();
              _builder.append("Construct a skill. The owning agent is unknown. ");
              _builder.newLine();
              SARLJvmModelInferrer.this._jvmTypesBuilder.setDocumentation(it, _builder.toString());
              StringConcatenationClient _client = new StringConcatenationClient() {
                @Override
                protected void appendTo(StringConcatenationClient.TargetStringConcatenation _builder) {
                  _builder.append("super();");
                  _builder.newLine();
                }
              };
              SARLJvmModelInferrer.this._jvmTypesBuilder.setBody(it, _client);
            }
          };
          JvmConstructor _constructor_1 = SARLJvmModelInferrer.this._jvmTypesBuilder.toConstructor(skill, _function_1);
          SARLJvmModelInferrer.this._jvmTypesBuilder.<JvmConstructor>operator_add(_members_1, _constructor_1);
        }
      }
    };
    _accept.initializeLater(_function);
  }
  
  protected void _infer(final Behavior behavior, final IJvmDeclaredTypeAcceptor acceptor, final boolean isPreIndexingPhase) {
    QualifiedName _fullyQualifiedName = this._iQualifiedNameProvider.getFullyQualifiedName(behavior);
    JvmGenericType _class = this._jvmTypesBuilder.toClass(behavior, _fullyQualifiedName);
    IJvmDeclaredTypeAcceptor.IPostIndexingInitializing<JvmGenericType> _accept = acceptor.<JvmGenericType>accept(_class);
    final Procedure1<JvmGenericType> _function = new Procedure1<JvmGenericType>() {
      public void apply(final JvmGenericType it) {
        SARLJvmModelInferrer.this.sarlSignatureProvider.resetSignatures(it);
        SARLJvmModelInferrer.this._jvmTypesBuilder.copyDocumentationTo(behavior, it);
        SARLJvmModelInferrer.this.generateSuperTypes(it, behavior, io.sarl.lang.core.Behavior.class);
        int behaviorUnitIndex = 1;
        int actionIndex = 1;
        boolean hasConstructor = false;
        EList<EObject> _features = behavior.getFeatures();
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
              final JvmOperation bMethod = SARLJvmModelInferrer.this.generateBehaviorUnit(it, ((BehaviorUnit)feature), behaviorUnitIndex);
              boolean _tripleNotEquals = (bMethod != null);
              if (_tripleNotEquals) {
                behaviorUnitIndex++;
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
              SARLJvmModelInferrer.this.generateAction(it, ((ActionSignature) _signature), _body, actionIndex);
              actionIndex++;
            }
          }
          if (!_matched) {
            if (feature instanceof CapacityUses) {
              _matched=true;
              EList<Capacity> _capacitiesUsed = ((CapacityUses)feature).getCapacitiesUsed();
              for (final Capacity used : _capacitiesUsed) {
                int _generateCapacityDelegatorMethods = SARLJvmModelInferrer.this.generateCapacityDelegatorMethods(it, behavior, used, actionIndex);
                actionIndex = _generateCapacityDelegatorMethods;
              }
            }
          }
          if (!_matched) {
            if (feature instanceof Constructor) {
              _matched=true;
              SARLJvmModelInferrer.this.generateConstructor(it, behavior, ((Constructor)feature), actionIndex);
              actionIndex++;
              hasConstructor = true;
            }
          }
          if (!_matched) {
            if (feature instanceof Attribute) {
              _matched=true;
              SARLJvmModelInferrer.this.generateAttribute(it, ((Attribute)feature), JvmVisibility.PROTECTED);
            }
          }
        }
        if ((!hasConstructor)) {
          EList<JvmMember> _members = it.getMembers();
          final Procedure1<JvmConstructor> _function = new Procedure1<JvmConstructor>() {
            public void apply(final JvmConstructor it) {
              StringConcatenation _builder = new StringConcatenation();
              _builder.append("Construct a behavior.");
              _builder.newLine();
              _builder.append("@param owner - reference to the agent that is owning this behavior.");
              _builder.newLine();
              SARLJvmModelInferrer.this._jvmTypesBuilder.setDocumentation(it, _builder.toString());
              EList<JvmFormalParameter> _parameters = it.getParameters();
              JvmTypeReference _newTypeRef = SARLJvmModelInferrer.this._jvmTypesBuilder.newTypeRef(it, Agent.class);
              JvmFormalParameter _parameter = SARLJvmModelInferrer.this._jvmTypesBuilder.toParameter(behavior, "owner", _newTypeRef);
              SARLJvmModelInferrer.this._jvmTypesBuilder.<JvmFormalParameter>operator_add(_parameters, _parameter);
              StringConcatenationClient _client = new StringConcatenationClient() {
                @Override
                protected void appendTo(StringConcatenationClient.TargetStringConcatenation _builder) {
                  _builder.append("super(owner);");
                  _builder.newLine();
                }
              };
              SARLJvmModelInferrer.this._jvmTypesBuilder.setBody(it, _client);
            }
          };
          JvmConstructor _constructor = SARLJvmModelInferrer.this._jvmTypesBuilder.toConstructor(behavior, _function);
          SARLJvmModelInferrer.this._jvmTypesBuilder.<JvmConstructor>operator_add(_members, _constructor);
        }
      }
    };
    _accept.initializeLater(_function);
  }
  
  protected void _infer(final io.sarl.lang.sarl.Agent agent, final IJvmDeclaredTypeAcceptor acceptor, final boolean isPreIndexingPhase) {
    QualifiedName _fullyQualifiedName = this._iQualifiedNameProvider.getFullyQualifiedName(agent);
    JvmGenericType _class = this._jvmTypesBuilder.toClass(agent, _fullyQualifiedName);
    IJvmDeclaredTypeAcceptor.IPostIndexingInitializing<JvmGenericType> _accept = acceptor.<JvmGenericType>accept(_class);
    final Procedure1<JvmGenericType> _function = new Procedure1<JvmGenericType>() {
      public void apply(final JvmGenericType it) {
        SARLJvmModelInferrer.this.sarlSignatureProvider.resetSignatures(it);
        SARLJvmModelInferrer.this._jvmTypesBuilder.copyDocumentationTo(agent, it);
        SARLJvmModelInferrer.this.generateSuperTypes(it, agent, Agent.class);
        final Procedure1<JvmConstructor> _function = new Procedure1<JvmConstructor>() {
          public void apply(final JvmConstructor it) {
            StringConcatenation _builder = new StringConcatenation();
            _builder.append("Construct an agent.");
            _builder.newLine();
            _builder.append("@param parentID - identifier of the parent. It is the identifer");
            _builder.newLine();
            _builder.append("of the parent agent and the enclosing contect, at the same time.");
            _builder.newLine();
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
        JvmConstructor cons = SARLJvmModelInferrer.this._jvmTypesBuilder.toConstructor(agent, _function);
        EList<JvmAnnotationReference> _annotations = cons.getAnnotations();
        JvmAnnotationReference _annotation = SARLJvmModelInferrer.this._jvmTypesBuilder.toAnnotation(agent, 
          Generated.class);
        SARLJvmModelInferrer.this._jvmTypesBuilder.<JvmAnnotationReference>operator_add(_annotations, _annotation);
        EList<JvmMember> _members = it.getMembers();
        SARLJvmModelInferrer.this._jvmTypesBuilder.<JvmConstructor>operator_add(_members, cons);
        int behaviorUnitIndex = 1;
        int actionIndex = 1;
        EList<EObject> _features = agent.getFeatures();
        for (final EObject feature : _features) {
          boolean _matched = false;
          if (!_matched) {
            if (feature instanceof BehaviorUnit) {
              _matched=true;
              final JvmOperation bMethod = SARLJvmModelInferrer.this.generateBehaviorUnit(it, ((BehaviorUnit)feature), behaviorUnitIndex);
              boolean _tripleNotEquals = (bMethod != null);
              if (_tripleNotEquals) {
                behaviorUnitIndex++;
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
              SARLJvmModelInferrer.this.generateAction(it, ((ActionSignature) _signature), _body, actionIndex);
              actionIndex++;
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
                int _generateCapacityDelegatorMethods = SARLJvmModelInferrer.this.generateCapacityDelegatorMethods(it, agent, used, actionIndex);
                actionIndex = _generateCapacityDelegatorMethods;
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
        SARLJvmModelInferrer.this._jvmTypesBuilder.copyDocumentationTo(attr, it);
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
    XExpression _initialValue = attr.getInitialValue();
    boolean _tripleNotEquals = (_initialValue != null);
    if (_tripleNotEquals) {
      this.readAndWriteTracking.markInitialized(field);
    }
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
  
  protected int generateCapacityDelegatorMethods(final JvmGenericType owner, final InheritingElement context, final Capacity capacity, final int index) {
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
    int actionIndex = index;
    for (final ActionSignature signature : functions) {
      {
        JvmOperation _generateAction = this.generateAction(owner, signature, null, actionIndex, false, null, null, null);
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
        actionIndex++;
      }
    }
    return actionIndex;
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
          SARLJvmModelInferrer.this._jvmTypesBuilder.copyDocumentationTo(unit, it);
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
  
  protected List<String> generateFormalParametersAndDefaultValueFields(final JvmExecutable owner, final JvmGenericType actionContainer, final EObject sourceElement, final boolean varargs, final List<FormalParameter> params, final boolean isForInterface, final int actionIndex) {
    ArrayList<String> parameterTypes = new ArrayList<String>();
    JvmFormalParameter lastParam = null;
    int paramIndex = 0;
    boolean hasDefaultValue = false;
    for (final FormalParameter param : params) {
      {
        String _name = param.getName();
        JvmTypeReference _parameterType = param.getParameterType();
        JvmFormalParameter _parameter = this._jvmTypesBuilder.toParameter(param, _name, _parameterType);
        lastParam = _parameter;
        XExpression _defaultValue = param.getDefaultValue();
        boolean _tripleNotEquals = (_defaultValue != null);
        if (_tripleNotEquals) {
          hasDefaultValue = true;
          String _plus = (Integer.valueOf(actionIndex) + "_");
          String namePostPart = (_plus + Integer.valueOf(paramIndex));
          String name = ("___FORMAL_PARAMETER_DEFAULT_VALUE_" + namePostPart);
          XExpression _defaultValue_1 = param.getDefaultValue();
          JvmTypeReference _parameterType_1 = param.getParameterType();
          final Procedure1<JvmField> _function = new Procedure1<JvmField>() {
            public void apply(final JvmField it) {
              String _name = param.getName();
              String _plus = ("Default value for the parameter " + _name);
              SARLJvmModelInferrer.this._jvmTypesBuilder.setDocumentation(it, _plus);
              it.setStatic(true);
              it.setFinal(true);
              if (isForInterface) {
                it.setVisibility(JvmVisibility.PUBLIC);
              } else {
                it.setVisibility(JvmVisibility.PRIVATE);
              }
              XExpression _defaultValue = param.getDefaultValue();
              SARLJvmModelInferrer.this._jvmTypesBuilder.setInitializer(it, _defaultValue);
            }
          };
          JvmField field = this._jvmTypesBuilder.toField(_defaultValue_1, name, _parameterType_1, _function);
          EList<JvmAnnotationReference> _annotations = field.getAnnotations();
          JvmAnnotationReference _annotation = this._jvmTypesBuilder.toAnnotation(param, Generated.class);
          this._jvmTypesBuilder.<JvmAnnotationReference>operator_add(_annotations, _annotation);
          EList<JvmMember> _members = actionContainer.getMembers();
          this._jvmTypesBuilder.<JvmField>operator_add(_members, field);
          this.readAndWriteTracking.markInitialized(field);
          JvmAnnotationReference annot = this._jvmTypesBuilder.toAnnotation(param, DefaultValue.class, namePostPart);
          EList<JvmAnnotationReference> _annotations_1 = lastParam.getAnnotations();
          this._jvmTypesBuilder.<JvmAnnotationReference>operator_add(_annotations_1, annot);
        }
        EList<JvmFormalParameter> _parameters = owner.getParameters();
        this._jvmTypesBuilder.<JvmFormalParameter>operator_add(_parameters, lastParam);
        JvmTypeReference _parameterType_2 = param.getParameterType();
        String _identifier = _parameterType_2.getIdentifier();
        parameterTypes.add(_identifier);
        paramIndex++;
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
    if (hasDefaultValue) {
      EList<JvmAnnotationReference> _annotations = owner.getAnnotations();
      JvmAnnotationReference _annotation = this._jvmTypesBuilder.toAnnotation(sourceElement, DefaultValueSource.class);
      this._jvmTypesBuilder.<JvmAnnotationReference>operator_add(_annotations, _annotation);
    }
    return parameterTypes;
  }
  
  protected List<String> generateFormalParametersWithDefaultValue(final JvmExecutable owner, final JvmGenericType actionContainer, final boolean varargs, final List<InferredStandardParameter> signature, final int actionIndex) {
    JvmFormalParameter lastParam = null;
    final ArrayList<String> arguments = new ArrayList<String>();
    int paramIndex = 0;
    for (final InferredStandardParameter parameterSpec : signature) {
      {
        if ((parameterSpec instanceof InferredValuedParameter)) {
          arguments.add(((("___FORMAL_PARAMETER_DEFAULT_VALUE_" + Integer.valueOf(actionIndex)) + "_") + Integer.valueOf(paramIndex)));
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
        paramIndex++;
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
    return arguments;
  }
  
  protected final JvmOperation generateAction(final JvmGenericType owner, final ActionSignature signature, final XExpression operationBody, final int index) {
    boolean _tripleEquals = (operationBody == null);
    return this.generateAction(owner, signature, operationBody, index, _tripleEquals, null, 
      null, null);
  }
  
  protected JvmOperation generateAction(final JvmGenericType owner, final ActionSignature signature, final XExpression operationBody, final int index, final boolean isAbstract, final Map<ActionKey, JvmOperation> operationsToImplement, final Map<ActionKey, JvmOperation> implementedOperations, final Function1<? super ActionKey, ? extends Boolean> inheritedOperation) {
    JvmTypeReference returnType = signature.getType();
    boolean _equals = Objects.equal(returnType, null);
    if (_equals) {
      JvmTypeReference _newTypeRef = this._jvmTypesBuilder.newTypeRef(signature, Void.TYPE);
      returnType = _newTypeRef;
    }
    String _name = signature.getName();
    final ActionNameKey actionKey = this.sarlSignatureProvider.createFunctionID(owner, _name);
    String _name_1 = signature.getName();
    final Procedure1<JvmOperation> _function = new Procedure1<JvmOperation>() {
      public void apply(final JvmOperation it) {
        SARLJvmModelInferrer.this._jvmTypesBuilder.copyDocumentationTo(signature, it);
        boolean _isVarargs = signature.isVarargs();
        it.setVarArgs(_isVarargs);
        it.setAbstract(isAbstract);
        boolean _isVarargs_1 = signature.isVarargs();
        EList<FormalParameter> _params = signature.getParams();
        SARLJvmModelInferrer.this.generateFormalParametersAndDefaultValueFields(it, owner, signature, _isVarargs_1, _params, isAbstract, index);
        SARLJvmModelInferrer.this._jvmTypesBuilder.setBody(it, operationBody);
      }
    };
    JvmOperation mainOp = this._jvmTypesBuilder.toMethod(signature, _name_1, returnType, _function);
    EList<JvmMember> _members = owner.getMembers();
    this._jvmTypesBuilder.<JvmOperation>operator_add(_members, mainOp);
    boolean _isVarargs = signature.isVarargs();
    EList<FormalParameter> _params = signature.getParams();
    final InferredActionSignature otherSignatures = this.sarlSignatureProvider.createSignature(actionKey, _isVarargs, _params);
    String _name_2 = signature.getName();
    SignatureKey _formalParameterKey = otherSignatures.getFormalParameterKey();
    ActionKey actSigKey = this.sarlSignatureProvider.createActionID(_name_2, _formalParameterKey);
    boolean _tripleNotEquals = (operationsToImplement != null);
    if (_tripleNotEquals) {
      JvmOperation removedOp = operationsToImplement.remove(actSigKey);
      boolean _and = false;
      boolean _tripleNotEquals_1 = (removedOp != null);
      if (!_tripleNotEquals_1) {
        _and = false;
      } else {
        boolean _tripleNotEquals_2 = (implementedOperations != null);
        _and = _tripleNotEquals_2;
      }
      if (_and) {
        implementedOperations.put(actSigKey, removedOp);
      }
    }
    Map<SignatureKey, EList<InferredStandardParameter>> _inferredSignatures = otherSignatures.getInferredSignatures();
    Set<Map.Entry<SignatureKey, EList<InferredStandardParameter>>> _entrySet = _inferredSignatures.entrySet();
    for (final Map.Entry<SignatureKey, EList<InferredStandardParameter>> otherSignature : _entrySet) {
      {
        String _name_3 = signature.getName();
        SignatureKey _key = otherSignature.getKey();
        ActionKey ak = this.sarlSignatureProvider.createActionID(_name_3, _key);
        boolean _or = false;
        boolean _equals_1 = Objects.equal(inheritedOperation, null);
        if (_equals_1) {
          _or = true;
        } else {
          Boolean _apply = inheritedOperation.apply(ak);
          _or = (_apply).booleanValue();
        }
        if (_or) {
          String _name_4 = signature.getName();
          final Procedure1<JvmOperation> _function_1 = new Procedure1<JvmOperation>() {
            public void apply(final JvmOperation it) {
              SARLJvmModelInferrer.this._jvmTypesBuilder.copyDocumentationTo(signature, it);
              boolean _isVarargs = signature.isVarargs();
              it.setVarArgs(_isVarargs);
              it.setFinal((!isAbstract));
              it.setAbstract(isAbstract);
              boolean _isVarargs_1 = signature.isVarargs();
              EList<InferredStandardParameter> _value = otherSignature.getValue();
              final List<String> args = SARLJvmModelInferrer.this.generateFormalParametersWithDefaultValue(it, owner, _isVarargs_1, _value, index);
              if ((!isAbstract)) {
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
              EList<JvmAnnotationReference> _annotations = it.getAnnotations();
              SignatureKey _formalParameterKey = otherSignatures.getFormalParameterKey();
              String _string = _formalParameterKey.toString();
              JvmAnnotationReference _annotation = SARLJvmModelInferrer.this._jvmTypesBuilder.toAnnotation(signature, 
                DefaultValueUse.class, _string);
              SARLJvmModelInferrer.this._jvmTypesBuilder.<JvmAnnotationReference>operator_add(_annotations, _annotation);
            }
          };
          JvmOperation additionalOp = this._jvmTypesBuilder.toMethod(signature, _name_4, returnType, _function_1);
          EList<JvmMember> _members_1 = owner.getMembers();
          this._jvmTypesBuilder.<JvmOperation>operator_add(_members_1, additionalOp);
          boolean _tripleNotEquals_3 = (operationsToImplement != null);
          if (_tripleNotEquals_3) {
            JvmOperation removedOp_1 = operationsToImplement.remove(ak);
            boolean _and_1 = false;
            boolean _tripleNotEquals_4 = (removedOp_1 != null);
            if (!_tripleNotEquals_4) {
              _and_1 = false;
            } else {
              boolean _tripleNotEquals_5 = (implementedOperations != null);
              _and_1 = _tripleNotEquals_5;
            }
            if (_and_1) {
              implementedOperations.put(ak, removedOp_1);
            }
          }
        }
      }
    }
    return mainOp;
  }
  
  protected SignatureKey generateConstructor(final JvmGenericType owner, final TopElement context, final Constructor constructor, final int index) {
    final ActionNameKey actionKey = this.sarlSignatureProvider.createConstructorID(owner);
    EList<JvmMember> _members = owner.getMembers();
    final Procedure1<JvmConstructor> _function = new Procedure1<JvmConstructor>() {
      public void apply(final JvmConstructor it) {
        SARLJvmModelInferrer.this._jvmTypesBuilder.copyDocumentationTo(constructor, it);
        boolean _isVarargs = constructor.isVarargs();
        it.setVarArgs(_isVarargs);
        boolean _isVarargs_1 = constructor.isVarargs();
        EList<FormalParameter> _params = constructor.getParams();
        SARLJvmModelInferrer.this.generateFormalParametersAndDefaultValueFields(it, owner, constructor, _isVarargs_1, _params, false, index);
        XExpression _body = constructor.getBody();
        SARLJvmModelInferrer.this._jvmTypesBuilder.setBody(it, _body);
      }
    };
    JvmConstructor _constructor = this._jvmTypesBuilder.toConstructor(context, _function);
    this._jvmTypesBuilder.<JvmConstructor>operator_add(_members, _constructor);
    boolean _isVarargs = constructor.isVarargs();
    EList<FormalParameter> _params = constructor.getParams();
    final InferredActionSignature otherSignatures = this.sarlSignatureProvider.createSignature(actionKey, _isVarargs, _params);
    for (final EList<InferredStandardParameter> otherSignature : otherSignatures) {
      {
        final Procedure1<JvmConstructor> _function_1 = new Procedure1<JvmConstructor>() {
          public void apply(final JvmConstructor it) {
            SARLJvmModelInferrer.this._jvmTypesBuilder.copyDocumentationTo(constructor, it);
            boolean _isVarargs = constructor.isVarargs();
            it.setVarArgs(_isVarargs);
            boolean _isVarargs_1 = constructor.isVarargs();
            final List<String> args = SARLJvmModelInferrer.this.generateFormalParametersWithDefaultValue(it, owner, _isVarargs_1, otherSignature, index);
            final Procedure1<ITreeAppendable> _function = new Procedure1<ITreeAppendable>() {
              public void apply(final ITreeAppendable it) {
                it.append("this(");
                String _join = IterableExtensions.join(args, ", ");
                it.append(_join);
                it.append(");");
              }
            };
            SARLJvmModelInferrer.this._jvmTypesBuilder.setBody(it, _function);
            EList<JvmAnnotationReference> _annotations = it.getAnnotations();
            SignatureKey _formalParameterKey = otherSignatures.getFormalParameterKey();
            String _string = _formalParameterKey.toString();
            JvmAnnotationReference _annotation = SARLJvmModelInferrer.this._jvmTypesBuilder.toAnnotation(owner, 
              DefaultValueUse.class, _string);
            SARLJvmModelInferrer.this._jvmTypesBuilder.<JvmAnnotationReference>operator_add(_annotations, _annotation);
          }
        };
        JvmConstructor op = this._jvmTypesBuilder.toConstructor(owner, _function_1);
        EList<JvmMember> _members_1 = owner.getMembers();
        this._jvmTypesBuilder.<JvmConstructor>operator_add(_members_1, op);
      }
    }
    return otherSignatures.getFormalParameterKey();
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
  
  protected String annotationString(final JvmAnnotationTarget op, final Class<?> annotationType) {
    final String n = annotationType.getName();
    EList<JvmAnnotationReference> _annotations = op.getAnnotations();
    for (final JvmAnnotationReference aref : _annotations) {
      {
        JvmAnnotationType an = aref.getAnnotation();
        String _qualifiedName = an.getQualifiedName();
        boolean _equals = Objects.equal(n, _qualifiedName);
        if (_equals) {
          EList<JvmAnnotationValue> _values = aref.getValues();
          for (final JvmAnnotationValue value : _values) {
            if ((value instanceof JvmStringAnnotationValue)) {
              EList<String> _values_1 = ((JvmStringAnnotationValue)value).getValues();
              for (final String sValue : _values_1) {
                boolean _tripleNotEquals = (value != null);
                if (_tripleNotEquals) {
                  return sValue;
                }
              }
            }
          }
        }
      }
    }
    return null;
  }
  
  public void infer(final EObject skill, final IJvmDeclaredTypeAcceptor acceptor, final boolean isPreIndexingPhase) {
    if (skill instanceof Skill) {
      _infer((Skill)skill, acceptor, isPreIndexingPhase);
      return;
    } else if (skill instanceof io.sarl.lang.sarl.Agent) {
      _infer((io.sarl.lang.sarl.Agent)skill, acceptor, isPreIndexingPhase);
      return;
    } else if (skill instanceof Behavior) {
      _infer((Behavior)skill, acceptor, isPreIndexingPhase);
      return;
    } else if (skill instanceof Capacity) {
      _infer((Capacity)skill, acceptor, isPreIndexingPhase);
      return;
    } else if (skill instanceof Event) {
      _infer((Event)skill, acceptor, isPreIndexingPhase);
      return;
    } else if (skill != null) {
      _infer(skill, acceptor, isPreIndexingPhase);
      return;
    } else {
      throw new IllegalArgumentException("Unhandled parameter types: " +
        Arrays.<Object>asList(skill, acceptor, isPreIndexingPhase).toString());
    }
  }
}
