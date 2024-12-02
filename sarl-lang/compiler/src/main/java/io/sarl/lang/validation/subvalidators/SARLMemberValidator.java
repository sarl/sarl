/*
 * $Id$
 *
 * SARL is an general-purpose agent programming language.
 * More details on http://www.sarl.io
 *
 * Copyright (C) 2014-2024 SARL.io, the Original Authors and Main Authors
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *      http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package io.sarl.lang.validation.subvalidators;

import static io.sarl.lang.sarl.SarlPackage.Literals.SARL_BEHAVIOR_UNIT__NAME;
import static io.sarl.lang.sarl.SarlPackage.Literals.SARL_CAPACITY_USES__CAPACITIES;
import static io.sarl.lang.sarl.SarlPackage.Literals.SARL_FORMAL_PARAMETER__DEFAULT_VALUE;
import static io.sarl.lang.validation.IssueCodes.ILLEGAL_PARAMETER_DEFAULT_VALUE_REDEFINITION;
import static io.sarl.lang.validation.IssueCodes.INVALID_CAPACITY_TYPE;
import static io.sarl.lang.validation.IssueCodes.INVALID_FIRING_EVENT_TYPE;
import static io.sarl.lang.validation.IssueCodes.MISSING_BODY;
import static io.sarl.lang.validation.IssueCodes.PARAMETER_DEFAULT_VALUE_REDFINITION;
import static io.sarl.lang.validation.IssueCodes.REDUNDANT_CAPACITY_USE;
import static io.sarl.lang.validation.IssueCodes.UNEXPECTED_EXCEPTION_THROW;
import static io.sarl.lang.validation.IssueCodes.UNEXPECTED_FORMAL_PARAMETER;
import static io.sarl.lang.validation.IssueCodes.UNUSED_AGENT_CAPACITY;
import static org.eclipse.xtend.core.validation.IssueCodes.ABSTRACT_METHOD_WITH_BODY;
import static org.eclipse.xtend.core.validation.IssueCodes.ANNOTATION_WRONG_TARGET;
import static org.eclipse.xtend.core.validation.IssueCodes.CONSTRUCTOR_TYPE_PARAMS_NOT_SUPPORTED;
import static org.eclipse.xtend.core.validation.IssueCodes.CREATE_FUNCTIONS_MUST_NOT_BE_ABSTRACT;
import static org.eclipse.xtend.core.validation.IssueCodes.DISPATCH_FUNCTIONS_MUST_NOT_BE_ABSTRACT;
import static org.eclipse.xtend.core.validation.IssueCodes.DISPATCH_FUNC_NAME_STARTS_WITH_UNDERSCORE;
import static org.eclipse.xtend.core.validation.IssueCodes.DISPATCH_FUNC_WITHOUT_PARAMS;
import static org.eclipse.xtend.core.validation.IssueCodes.DISPATCH_FUNC_WITH_TYPE_PARAMS;
import static org.eclipse.xtend.core.validation.IssueCodes.IMPLICIT_RETURN;
import static org.eclipse.xtend.core.validation.IssueCodes.INVALID_EXTENSION_TYPE;
import static org.eclipse.xtend.core.validation.IssueCodes.INVALID_USE_OF_STATIC;
import static org.eclipse.xtend.core.validation.IssueCodes.INVALID_USE_OF_VAR_ARG;
import static org.eclipse.xtend.core.validation.IssueCodes.MISSING_ABSTRACT;
import static org.eclipse.xtend.core.validation.IssueCodes.MISSING_ABSTRACT_IN_ANONYMOUS;
import static org.eclipse.xtend.core.validation.IssueCodes.UNUSED_PRIVATE_MEMBER;
import static org.eclipse.xtend.core.xtend.XtendPackage.Literals.CREATE_EXTENSION_INFO__NAME;
import static org.eclipse.xtend.core.xtend.XtendPackage.Literals.XTEND_EXECUTABLE__PARAMETERS;
import static org.eclipse.xtend.core.xtend.XtendPackage.Literals.XTEND_FIELD__NAME;
import static org.eclipse.xtend.core.xtend.XtendPackage.Literals.XTEND_FUNCTION__NAME;
import static org.eclipse.xtend.core.xtend.XtendPackage.Literals.XTEND_MEMBER__MODIFIERS;
import static org.eclipse.xtend.core.xtend.XtendPackage.Literals.XTEND_PARAMETER__EXTENSION;
import static org.eclipse.xtend.core.xtend.XtendPackage.Literals.XTEND_PARAMETER__PARAMETER_TYPE;
import static org.eclipse.xtend.core.xtend.XtendPackage.Literals.XTEND_PARAMETER__VAR_ARG;
import static org.eclipse.xtend.core.xtend.XtendPackage.Literals.XTEND_TYPE_DECLARATION__NAME;
import static org.eclipse.xtext.xbase.validation.IssueCodes.DUPLICATE_PARAMETER_NAME;
import static org.eclipse.xtext.xbase.validation.IssueCodes.FORBIDDEN_REFERENCE;
import static org.eclipse.xtext.xbase.validation.IssueCodes.INCOMPATIBLE_TYPES;
import static org.eclipse.xtext.xbase.validation.IssueCodes.INVALID_EARLY_EXIT;
import static org.eclipse.xtext.xbase.validation.IssueCodes.INVALID_TYPE;
import static org.eclipse.xtext.xbase.validation.IssueCodes.INVALID_USE_OF_TYPE;
import static org.eclipse.xtext.xbase.validation.IssueCodes.MUST_INVOKE_SUPER_CONSTRUCTOR;
import static org.eclipse.xtext.xbase.validation.IssueCodes.TOO_LITTLE_TYPE_INFORMATION;
import static org.eclipse.xtext.xbase.validation.IssueCodes.VARIABLE_NAME_DISCOURAGED;

import java.text.MessageFormat;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Comparator;
import java.util.Iterator;
import java.util.LinkedList;
import java.util.List;
import java.util.Objects;
import java.util.Set;
import java.util.TreeMap;

import com.google.common.collect.Iterables;
import com.google.inject.Inject;
import org.eclipse.emf.common.notify.Notifier;
import org.eclipse.emf.ecore.EAttribute;
import org.eclipse.emf.ecore.EObject;
import org.eclipse.emf.ecore.EStructuralFeature;
import org.eclipse.xtend.core.typesystem.LocalClassAwareTypeNames;
import org.eclipse.xtend.core.xtend.XtendAnnotationType;
import org.eclipse.xtend.core.xtend.XtendClass;
import org.eclipse.xtend.core.xtend.XtendConstructor;
import org.eclipse.xtend.core.xtend.XtendField;
import org.eclipse.xtend.core.xtend.XtendFormalParameter;
import org.eclipse.xtend.core.xtend.XtendFunction;
import org.eclipse.xtend.core.xtend.XtendInterface;
import org.eclipse.xtend.core.xtend.XtendMember;
import org.eclipse.xtend.core.xtend.XtendPackage;
import org.eclipse.xtend.core.xtend.XtendParameter;
import org.eclipse.xtend.core.xtend.XtendTypeDeclaration;
import org.eclipse.xtend.core.xtend.XtendVariableDeclaration;
import org.eclipse.xtext.EcoreUtil2;
import org.eclipse.xtext.common.types.JvmConstructor;
import org.eclipse.xtext.common.types.JvmDeclaredType;
import org.eclipse.xtext.common.types.JvmExecutable;
import org.eclipse.xtext.common.types.JvmField;
import org.eclipse.xtext.common.types.JvmGenericType;
import org.eclipse.xtext.common.types.JvmIdentifiableElement;
import org.eclipse.xtext.common.types.JvmOperation;
import org.eclipse.xtext.common.types.JvmParameterizedTypeReference;
import org.eclipse.xtext.common.types.JvmTypeReference;
import org.eclipse.xtext.common.types.JvmVisibility;
import org.eclipse.xtext.common.types.TypesPackage;
import org.eclipse.xtext.util.Strings;
import org.eclipse.xtext.validation.Check;
import org.eclipse.xtext.validation.CheckType;
import org.eclipse.xtext.validation.ValidationMessageAcceptor;
import org.eclipse.xtext.xbase.XAbstractFeatureCall;
import org.eclipse.xtext.xbase.XBlockExpression;
import org.eclipse.xtext.xbase.XCatchClause;
import org.eclipse.xtext.xbase.XClosure;
import org.eclipse.xtext.xbase.XConstructorCall;
import org.eclipse.xtext.xbase.XFeatureCall;
import org.eclipse.xtext.xbase.XReturnExpression;
import org.eclipse.xtext.xbase.XbasePackage;
import org.eclipse.xtext.xbase.lib.CollectionLiterals;
import org.eclipse.xtext.xbase.typesystem.IBatchTypeResolver;
import org.eclipse.xtext.xbase.typesystem.references.LightweightTypeReference;
import org.eclipse.xtext.xbase.validation.ImplicitReturnFinder;
import org.eclipse.xtext.xbase.validation.ProxyAwareUIStrings;
import org.eclipse.xtext.xbase.validation.UIStrings;
import org.eclipse.xtext.xtype.XComputedTypeReference;

import io.sarl.lang.controlflow.ISarlEarlyExitComputer;
import io.sarl.lang.core.Agent;
import io.sarl.lang.core.Behavior;
import io.sarl.lang.core.Event;
import io.sarl.lang.core.Skill;
import io.sarl.lang.sarl.SarlAction;
import io.sarl.lang.sarl.SarlAgent;
import io.sarl.lang.sarl.SarlBehavior;
import io.sarl.lang.sarl.SarlBehaviorUnit;
import io.sarl.lang.sarl.SarlCapacity;
import io.sarl.lang.sarl.SarlCapacityUses;
import io.sarl.lang.sarl.SarlConstructor;
import io.sarl.lang.sarl.SarlEvent;
import io.sarl.lang.sarl.SarlFormalParameter;
import io.sarl.lang.sarl.SarlPackage;
import io.sarl.lang.sarl.SarlRequiredCapacity;
import io.sarl.lang.sarl.SarlSkill;
import io.sarl.lang.sarl.actionprototype.ActionParameterTypes;
import io.sarl.lang.sarl.actionprototype.ActionPrototype;
import io.sarl.lang.sarl.actionprototype.InferredPrototype;
import io.sarl.lang.util.Utils;
import io.sarl.lang.validation.IssueCodes;

/**
 * A specialized validator to deal with members in SARL types.
 *
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 * @since 0.14
 */
public class SARLMemberValidator extends AbstractSARLSubValidatorWithParentLink {

	@Inject
	private ISarlEarlyExitComputer earlyExitComputer;

	@Inject
	private UIStrings uiStrings;

	@Inject
	private ProxyAwareUIStrings proxyAwareUIStrings;

	@Inject
	private LocalClassAwareTypeNames localClassAwareTypeNames;

	@Inject
	private IBatchTypeResolver batchTypeResolver;

	@Inject
	private ImplicitReturnFinder implicitReturnFinder;

	/** Emit a warning when the events after the "fires" keyword are not early-exit events.
	 *
	 * @param action the action to check.
	 */
	@Check(CheckType.FAST)
	public void checkEarlyExitEventInFires(SarlAction action) {
		var i = 0;
		for (final var event : action.getFiredEvents()) {
			if (!this.earlyExitComputer.isEarlyExitEvent(event)) {
				info(MessageFormat.format(
						Messages.SARLMemberValidator_2,
						event.getSimpleName()),
						action,
						SarlPackage.eINSTANCE.getSarlAction_FiredEvents(),
						i,
						IssueCodes.UNNECESSARY_FIRED_EVENT);
			}
			++i;
		}
	}

	/** Emit a warning when the "requires" keyword is used.
	 *
	 * @param statement the statement to check.
	 */
	@Check(CheckType.FAST)
	public void checkRequiredCapacityUse(SarlRequiredCapacity statement) {
		warning(MessageFormat.format(
				Messages.SARLMemberValidator_1,
				getGrammarAccess().getRequiresKeyword()),
				statement,
				null,
				INSIGNIFICANT_INDEX,
				IssueCodes.UNSUPPORTED_STATEMENT);
	}

	/** Check the type of the capacity uses.
	 *
	 * @param uses the capacity uses.
	 */
	@Check(CheckType.FAST)
	public void checkCapacityTypeForUses(SarlCapacityUses uses) {
		for (final var usedType : uses.getCapacities()) {
			final var ref = toLightweightTypeReference(usedType);
			if (ref != null && !getInheritanceHelper().isSarlCapacity(ref)) {
				error(MessageFormat.format(
						Messages.SARLMemberValidator_3,
						usedType.getQualifiedName(),
						Messages.SARLMemberValidator_4,
						getGrammarAccess().getUsesKeyword()),
						usedType,
						null,
						ValidationMessageAcceptor.INSIGNIFICANT_INDEX,
						INVALID_CAPACITY_TYPE,
						usedType.getSimpleName());
			}
		}
	}

	/** Check the types of the parameters of the "fires" statement.
	 *
	 * @param action the signature that contains the "fires" statement.
	 */
	@Check(CheckType.FAST)
	public void checkActionFires(SarlAction action) {
		for (final var event : action.getFiredEvents()) {
			final var ref = toLightweightTypeReference(event);
			if (ref != null && !getInheritanceHelper().isSarlEvent(ref)) {
				error(MessageFormat.format(
						Messages.SARLMemberValidator_3,
						event.getQualifiedName(),
						Messages.SARLMemberValidator_5,
						getGrammarAccess().getFiresKeyword()),
						event,
						null,
						ValidationMessageAcceptor.INSIGNIFICANT_INDEX,
						INVALID_FIRING_EVENT_TYPE,
						event.getSimpleName());
			}
		}
	}

	/** Check if all the fields are initialized in a SARL event.
	 *
	 * @param event the event.
	 */
	@Check(CheckType.EXPENSIVE)
	public void checkFinalFieldInitialization(SarlEvent event) {
		final var inferredType = getAssociations().getInferredType(event);
		if (inferredType != null) {
			getParentValidator().doCheckFinalFieldInitialization(inferredType, getMessageAcceptor());
		}
	}

	/** Check if all the fields are initialized in a SARL behavior.
	 *
	 * @param behavior the behavior.
	 */
	@Check(CheckType.EXPENSIVE)
	public void checkFinalFieldInitialization(SarlBehavior behavior) {
		final var inferredType = getAssociations().getInferredType(behavior);
		if (inferredType != null) {
			getParentValidator().doCheckFinalFieldInitialization(inferredType, getMessageAcceptor());
		}
	}

	/** Check if all the fields are initialized in a SARL skill.
	 *
	 * @param skill the skill.
	 */
	@Check(CheckType.EXPENSIVE)
	public void checkFinalFieldInitialization(SarlSkill skill) {
		final var inferredType = getAssociations().getInferredType(skill);
		if (inferredType != null) {
			getParentValidator().doCheckFinalFieldInitialization(inferredType, getMessageAcceptor());
		}
	}

	/** Check if all the fields are initialized in a SARL agent.
	 *
	 * @param agent the agent.
	 */
	@Check(CheckType.EXPENSIVE)
	public void checkFinalFieldInitialization(SarlAgent agent) {
		final var inferredType = getAssociations().getInferredType(agent);
		if (inferredType != null) {
			getParentValidator().doCheckFinalFieldInitialization(inferredType, getMessageAcceptor());
		}
	}

	/** Check if all the fields are initialized in a class.
	 *
	 * @param clazz the class.
	 */
	@Check(CheckType.EXPENSIVE)
	public void checkFinalFieldInitialization(XtendClass clazz) {
		final var inferredType = getAssociations().getInferredType(clazz);
		if (inferredType != null) {
			getParentValidator().doCheckFinalFieldInitialization(inferredType, getMessageAcceptor());
		}
	}

	/** Check if all the fields are initialized in an interface.
	 *
	 * @param oopInterface the interface.
	 */
	@Check(CheckType.EXPENSIVE)
	public void checkFinalFieldInitialization(XtendInterface oopInterface) {
		final var inferredType = getAssociations().getInferredType(oopInterface);
		if (inferredType != null) {
			getParentValidator().doCheckFinalFieldInitialization(inferredType, getMessageAcceptor());
		}
	}

	/** Check if the super default constructor is correctly invoked.
	 *
	 * @param agent the SARL element.
	 */
	@Check(CheckType.NORMAL)
	public void checkSuperConstructor(SarlAgent agent) {
		checkSuperConstructor(
				agent,
				XTEND_TYPE_DECLARATION__NAME,
				doGetConstructorParameterTypes(Agent.class, agent));
	}

	/** Check if the super default constructor is correctly invoked.
	 *
	 * @param behavior the SARL element.
	 */
	@Check(CheckType.NORMAL)
	public void checkSuperConstructor(SarlBehavior behavior) {
		checkSuperConstructor(
				behavior,
				XTEND_TYPE_DECLARATION__NAME,
				doGetConstructorParameterTypes(Behavior.class, behavior));
	}

	/** Check if the super default constructor is correctly invoked.
	 *
	 * @param skill the SARL element.
	 */
	@Check(CheckType.NORMAL)
	public void checkSuperConstructor(SarlSkill skill) {
		checkSuperConstructor(
				skill,
				XTEND_TYPE_DECLARATION__NAME,
				doGetConstructorParameterTypes(Skill.class, skill));
	}

	/** Check if the super default constructor is correctly invoked.
	 *
	 * @param event the SARL element.
	 */
	@Check(CheckType.NORMAL)
	public void checkSuperConstructor(SarlEvent event) {
		checkSuperConstructor(
				event,
				XTEND_TYPE_DECLARATION__NAME,
				doGetConstructorParameterTypes(Event.class, event));
	}

	/** Check if the super default constructor is correctly invoked.
	 *
	 * @param xtendClass the Xtend element.
	 */
	@Check(CheckType.NORMAL)
	public void checkDefaultSuperConstructor(XtendClass xtendClass) {
		checkSuperConstructor(
				xtendClass,
				XTEND_TYPE_DECLARATION__NAME,
				doGetConstructorParameterTypes(Object.class, xtendClass));
	}

	/** Check the super constructors.
	 *
	 * @param container the container.
	 * @param feature the syntactic feature related to the supertypes.
	 * @param defaultSignatures the signatures of the default constructors for the given container.
	 */
	protected void checkSuperConstructor(
			XtendTypeDeclaration container,
			EStructuralFeature feature,
			Collection<ActionParameterTypes> defaultSignatures) {
		final var jvmElement = getAssociations().getInferredType(container);
		if (jvmElement != null) {
			final var superConstructors =
					CollectionLiterals.<ActionParameterTypes, JvmConstructor>newTreeMap((Comparator<ActionParameterTypes>) null);
			final var typeRef = jvmElement.getExtendedClass();
			final var supertype = (typeRef == null) ? null : typeRef.getType();
			if (supertype instanceof JvmGenericType jvmSuperElement) {
				for (final var superConstructor : jvmSuperElement.getDeclaredConstructors()) {
					final var sig = getSarlActionSignatures().createParameterTypesFromJvmModel(
							superConstructor.isVarArgs(), superConstructor.getParameters());
					superConstructors.put(sig, superConstructor);
				}
			}

			final var voidKey = getSarlActionSignatures().createParameterTypesForVoid();

			for (final var member : container.getMembers()) {
				if (member instanceof SarlConstructor constructor) {
					if (!constructor.isStatic()) {
						var invokeDefaultConstructor = true;
						final var body = constructor.getExpression();
						if (body instanceof XBlockExpression block) {
							if (!block.getExpressions().isEmpty()) {
								final var firstStatement = block.getExpressions().get(0);
								if (firstStatement instanceof XConstructorCall) {
									invokeDefaultConstructor = false;
								} else if (firstStatement instanceof XFeatureCall cvalue) {
									final var calledFeature = cvalue.getFeature();
									if (calledFeature instanceof JvmConstructor) {
										invokeDefaultConstructor = false;
									}
								}
							}
						} else if (body instanceof XConstructorCall) {
							invokeDefaultConstructor = false;
						} else if (body instanceof XFeatureCall cvalue) {
							final var calledFeature = cvalue.getFeature();
							if (calledFeature instanceof JvmConstructor) {
								invokeDefaultConstructor = false;
							}
						}
						if (invokeDefaultConstructor && !superConstructors.containsKey(voidKey)) {
							final var issueData = new ArrayList<String>();
							for (final var defaultSignature : defaultSignatures) {
								issueData.add(defaultSignature.toString());
							}
							assert supertype != null;
							error(MessageFormat.format(Messages.SARLMemberValidator_6, supertype.getSimpleName()),
									member,
									null,
									ValidationMessageAcceptor.INSIGNIFICANT_INDEX,
									MUST_INVOKE_SUPER_CONSTRUCTOR,
									issueData.toArray(new String[issueData.size()]));
						}
					}
				}
			}
		}
	}

	private Collection<ActionParameterTypes> doGetConstructorParameterTypes(Class<?> type, Notifier context) {
		final var parameters = new ArrayList<ActionParameterTypes>();
		final var typeReference = getTypeReferences().getTypeForName(type, context);
		final var jvmType = typeReference.getType();
		if (jvmType instanceof JvmDeclaredType declaredType) {
			for (final var constructor : declaredType.getDeclaredConstructors()) {
				final var types = getSarlActionSignatures().createParameterTypesFromJvmModel(
						constructor.isVarArgs(), constructor.getParameters());
				if (types != null) {
					parameters.add(types);
				}
			}
		}
		if (parameters.isEmpty()) {
			parameters.add(getSarlActionSignatures().createParameterTypesForVoid());
		}
		return parameters;
	}

	/** Check if the default values of the formal parameters have a compatible type with the formal parameter.
	 *
	 * @param param the formal parameter to check.
	 */
	@Check(CheckType.NORMAL)
	public void checkDefaultValueTypeCompatibleWithParameterType(SarlFormalParameter param) {
		final var defaultValue = param.getDefaultValue();
		if (defaultValue != null) {
			final var rawType = param.getParameterType();
			assert rawType != null;
			final var toType = toLightweightTypeReference(rawType, true);
			if (toType == null) {
				error(MessageFormat.format(
						Messages.SARLMemberValidator_7,
						param.getName()),
						param,
						XTEND_PARAMETER__PARAMETER_TYPE,
						ValidationMessageAcceptor.INSIGNIFICANT_INDEX,
						INVALID_TYPE);
				return;
			}
			LightweightTypeReference fromType;
			if (isTypeFreeExpression(defaultValue)) {
				if (toType.isPrimitive()) {
					fromType = toLightweightTypeReference(getTypeReferences().getTypeForName(Object.class, param), true);
				} else {
					fromType = toType;
				}
			} else {
				fromType = getExpectedType(defaultValue);
				if (fromType == null) {
					fromType = getActualType(defaultValue);
					if (fromType == null) {
						// When the type of the default value cannot be inferred, it means that the default value
						// expression is not stored into a JVM element.
						// This case occurs when the function overrides another function with the default parameter.
						// In this case, the JVM model inferrer do not generate locally an hidden method with the default
						// value expression. Indeed, this hidden function is generated into the super type.
						// The following code tests if the validator is facing this limitation of the Xtext
						// API related to the computation of expressions types (i.e. a type could be computed only
						// if the expression is associated to a generated JVM element).
						final var jvmParam = getAssociations().getJvmParameter(param);
						var generateError = true;
						if (jvmParam != null) {
							final var defaultValueId = getSarlActionSignatures().extractDefaultValueString(jvmParam);
							generateError = Strings.isEmpty(defaultValueId);
						}
						if (generateError) {
							error(MessageFormat.format(
									Messages.SARLMemberValidator_8,
									param.getName()),
									param,
									SARL_FORMAL_PARAMETER__DEFAULT_VALUE,
									ValidationMessageAcceptor.INSIGNIFICANT_INDEX,
									INVALID_TYPE);
						}
						return;
					}
				}
			}
			if (fromType != toType && !Utils.canCast(fromType, toType, true, false, true)) {
				error(MessageFormat.format(
						Messages.SARLMemberValidator_9,
						getNameOfTypes(fromType), canonicalName(toType)),
						param,
						SARL_FORMAL_PARAMETER__DEFAULT_VALUE,
						ValidationMessageAcceptor.INSIGNIFICANT_INDEX,
						INCOMPATIBLE_TYPES,
						canonicalName(fromType),
						canonicalName(toType));
				return;
			}
		}
	}

	/** Check if the default values have not a reference to a not-pure operation.
	 *
	 * @param param the formal parameter to check.
	 */
	@Check(CheckType.NORMAL)
	public void checkDefaultValuePureExpression(SarlFormalParameter param) {
		final var defaultValue = param.getDefaultValue();
		if (defaultValue != null) {
			final var container = getLogicalContainerProvider().getNearestLogicalContainer(param);
			final InferredPrototype prototype;
			if (container instanceof JvmOperation cvalue) {
				prototype = getOperationHelper().getInferredPrototype(cvalue);
			} else if (container instanceof JvmConstructor cvalue) {
				prototype = getOperationHelper().getInferredPrototype(cvalue);
			} else {
				throw new Error("internal error: not an operation or a constructor"); //$NON-NLS-1$
			}
			final var sideEffects = getOperationHelper().getSideEffectExpressions(prototype, defaultValue);
			for (final var call : sideEffects) {
				final String code;
				if (call instanceof XAbstractFeatureCall acall) {
					final var element = acall.getFeature();
					if (element instanceof JvmExecutable cvalue) {
						code = this.uiStrings.signature(cvalue);
					} else {
						code = Utils.getSarlCodeFor(call);
					}
				} else if (call instanceof XConstructorCall cons) {
					code = getGrammarAccess().getNewKeyword() + this.uiStrings.arguments(cons);
				} else {
					code = Utils.getSarlCodeFor(call);
				}
				error(MessageFormat.format(Messages.SARLMemberValidator_10, code),
						call,
						null,
						ValidationMessageAcceptor.INSIGNIFICANT_INDEX,
						FORBIDDEN_REFERENCE);
			}
		}
	}

	/** Check if the default values have valid references to read-only fields from constructors.
	 *
	 * @param param the formal parameter to check.
	 */
	@Check(CheckType.EXPENSIVE)
	public void checkDefaultValueFinalFieldReferenceInConstructor(SarlFormalParameter param) {
		final var defaultValue = param.getDefaultValue();
		if (defaultValue != null) {
			final var container = getLogicalContainerProvider().getNearestLogicalContainer(param);
			if (container instanceof JvmConstructor) {
				final var iter = getAllFeatureCalls(defaultValue);
				while (iter.hasNext()) {
					final var call = iter.next();
					final var feature = call.getFeature();
					if (feature instanceof JvmField field) {
						if (!field.isFinal()) {
							error(MessageFormat.format(
									Messages.SARLMemberValidator_11,
									field.getSimpleName()),
									call,
									XbasePackage.Literals.XABSTRACT_FEATURE_CALL__FEATURE,
									ValidationMessageAcceptor.INSIGNIFICANT_INDEX,
									FORBIDDEN_REFERENCE);
						}
					}
				}
			}
		}
	}

	/** Check if the default value expression is redefined from an inherited definition.
	 *
	 * @param param the formal parameter to check.
	 */
	@Check(CheckType.EXPENSIVE)
	public void checkDefaultValueRedefinition(SarlFormalParameter param) {
		final var defaultValue = param.getDefaultValue();
		if (defaultValue != null) {
			final var container = getLogicalContainerProvider().getNearestLogicalContainer(param);
			if (container instanceof JvmOperation operation) {
				final var overridableOperations = new TreeMap<ActionPrototype, JvmOperation>();
				final var operationsToImplement = new TreeMap<ActionPrototype, JvmOperation>();
				Utils.populateInheritanceContext(
						operation.getDeclaringType(),
						null,
						overridableOperations,
						null,
						operationsToImplement,
						null,
						getSarlActionSignatures());
				final var inferredPrototype = getOperationHelper().getInferredPrototype(operation);
				final var actionPrototype = getSarlActionSignatures().createActionPrototype(
						operation.getSimpleName(),
						inferredPrototype.getFormalParameterTypes());
				var inheritedOperation = overridableOperations.get(actionPrototype);
				if (inheritedOperation == null) {
					inheritedOperation = operationsToImplement.get(actionPrototype);
				}
				if (inheritedOperation != null) {
					final var currentParam = getAssociations().getJvmParameter(param);
					final var referencedCode = getSarlActionSignatures().extractDefaultValueString(currentParam);
					final var currentCode = Utils.getSarlCodeFor(defaultValue);
					if (Strings.equal(currentCode, referencedCode)) {
						if (!isIgnored(PARAMETER_DEFAULT_VALUE_REDFINITION)) {
							addIssue(MessageFormat.format(Messages.SARLMemberValidator_12, param.getName()),
									param,
									SarlPackage.Literals.SARL_FORMAL_PARAMETER__DEFAULT_VALUE,
									ValidationMessageAcceptor.INSIGNIFICANT_INDEX,
									PARAMETER_DEFAULT_VALUE_REDFINITION);
						}
					} else {
						error(MessageFormat.format(Messages.SARLMemberValidator_13, param.getName(),
								Utils.toReadableString(referencedCode), Utils.toReadableString(currentCode)),
								param,
								SarlPackage.Literals.SARL_FORMAL_PARAMETER__DEFAULT_VALUE,
								ValidationMessageAcceptor.INSIGNIFICANT_INDEX,
								ILLEGAL_PARAMETER_DEFAULT_VALUE_REDEFINITION);
					}
				}
			}
		}
	}

	/** Check for unused capacities.
	 *
	 * @param uses the capacity use declaration.
	 */
	@Check(CheckType.EXPENSIVE)
	public void checkUnusedCapacities(SarlCapacityUses uses) {
		if (!isIgnored(UNUSED_AGENT_CAPACITY)) {
			final var container = uses.getDeclaringType();
			final var jvmContainer = (JvmDeclaredType) getAssociations().getPrimaryJvmElement(container);
			final var importedFeatures = CollectionLiterals.<String, JvmOperation>newHashMap();
			for (final var operation : jvmContainer.getDeclaredOperations()) {
				if (Utils.isNameForHiddenCapacityImplementationCallingMethod(operation.getSimpleName())) {
					importedFeatures.put(operation.getSimpleName(), operation);
				}
			}

			final var isSkill = container instanceof SarlSkill;
			var index = 0;
			for (final var capacity : uses.getCapacities()) {
				final var lreference = toLightweightTypeReference(capacity);
				if (isSkill && lreference.isAssignableFrom(jvmContainer)) {
					addIssue(MessageFormat.format(
							Messages.SARLMemberValidator_14,
							capacity.getSimpleName()),
							uses,
							SARL_CAPACITY_USES__CAPACITIES,
							index, UNUSED_AGENT_CAPACITY,
							capacity.getSimpleName());
				} else {
					final var fieldName = Utils.createNameForHiddenCapacityImplementationAttribute(capacity.getIdentifier());
					final var operationName = Utils.createNameForHiddenCapacityImplementationCallingMethodFromFieldName(fieldName);
					final var operation = importedFeatures.get(operationName);
					if (operation != null && !getParentValidator().isLocallyUsed(operation, container)) {
						addIssue(MessageFormat.format(
								Messages.SARLMemberValidator_15,
								capacity.getSimpleName()),
								uses,
								SARL_CAPACITY_USES__CAPACITIES,
								index, UNUSED_AGENT_CAPACITY,
								capacity.getSimpleName());
					}
				}
				++index;
			}
		}
	}

	/** Check for multiple capacity use declaration.
	 *
	 * @param uses the capacity use declaration.
	 */
	@Check(CheckType.EXPENSIVE)
	public void checkMultipleCapacityUses(SarlCapacityUses uses) {
		if (!isIgnored(REDUNDANT_CAPACITY_USE)) {
			final var declaringType = uses.getDeclaringType();
			if (declaringType != null) {
				final var previousCapacityUses = doGetPreviousCapacities(uses,
						declaringType.getMembers().iterator());
				var index = 0;
				for (final var capacity : uses.getCapacities()) {
					if (previousCapacityUses.contains(capacity.getIdentifier())) {
						addIssue(MessageFormat.format(
								Messages.SARLMemberValidator_16,
								capacity.getSimpleName()),
								uses,
								SARL_CAPACITY_USES__CAPACITIES,
								index,
								REDUNDANT_CAPACITY_USE,
								capacity.getSimpleName());
					} else {
						previousCapacityUses.add(capacity.getIdentifier());
					}
					++index;
				}
			}
		}
	}

	private static Set<String> doGetPreviousCapacities(SarlCapacityUses uses, Iterator<XtendMember> iterator) {
		var continueToFill = true;
		final var capacityUses = CollectionLiterals.<String>newTreeSet((Comparator<String>) null);
		while (continueToFill && iterator.hasNext()) {
			final var elt = iterator.next();
			if (elt instanceof SarlCapacityUses usesElt) {
				if (usesElt == uses) {
					continueToFill = false;
				} else {
					for (final var use : usesElt.getCapacities()) {
						capacityUses.add(use.getIdentifier());
					}
				}
			}
		}
		return capacityUses;
	}

	/** Check the local usage of the functions.
	 *
	 * @param function the function.
	 */
	@Check(CheckType.EXPENSIVE)
	public void checkLocalUsageOfDeclaredXtendFunction(XtendFunction function) {
		if (doCheckValidMemberName(function) && !isIgnored(UNUSED_PRIVATE_MEMBER)) {
			final JvmOperation mainOperation;
			if (function.isDispatch()) {
				mainOperation = getAssociations().getDispatchOperation(function);
			} else {
				mainOperation = getAssociations().getDirectlyInferredOperation(function);
			}
			if (mainOperation != null && mainOperation.getVisibility() == JvmVisibility.PRIVATE) {
				final var outerType = getOutermostType(function);
				var isUsed = getParentValidator().isLocallyUsed(mainOperation, outerType);
				if (!isUsed && isDefaultValuedParameterFunction(function)) {
					for (final var jvmElement : getAssociations().getJvmElements(function)) {
						if (jvmElement != mainOperation && jvmElement instanceof JvmOperation
								&& getParentValidator().isLocallyUsed(jvmElement, outerType)) {
							isUsed = true;
							// break the loop
							break;
						}
					}
				}
				if (!isUsed) {
					final var message = MessageFormat.format(Messages.SARLMemberValidator_17,
							mainOperation.getSimpleName(), this.uiStrings.parameters(mainOperation),
							getDeclaratorName(mainOperation));
					addIssueToState(UNUSED_PRIVATE_MEMBER, message, XtendPackage.Literals.XTEND_FUNCTION__NAME);
				}
			}
		}
	}

	/** Replies if the given function has a default value for one of its parameters.
	 *
	 * @param function the function to test.
	 * @return {@code true} if one parameter has a default value.
	 */
	@SuppressWarnings("static-method")
	protected boolean isDefaultValuedParameterFunction(XtendFunction function) {
		for (final var parameter : function.getParameters()) {
			if (parameter instanceof SarlFormalParameter sarlParameter) {
				if (sarlParameter.getDefaultValue() != null) {
					return true;
				}
			}
		}
		return false;
	}

	/** Check for a valid prototype of a static constructor.
	 *
	 * @param constructor the constructor to analyze.
	 */
	@Check(CheckType.FAST)
	public void checkStaticConstructorPrototype(XtendConstructor constructor) {
		if (constructor.isStatic()) {
			if (!constructor.getAnnotations().isEmpty()) {
				error(Messages.SARLMemberValidator_18,
						constructor,
						XtendPackage.eINSTANCE.getXtendAnnotationTarget_Annotations(),
						ValidationMessageAcceptor.INSIGNIFICANT_INDEX,
						ANNOTATION_WRONG_TARGET);
			}
			if (!constructor.getTypeParameters().isEmpty()) {
				error(Messages.SARLMemberValidator_19,
						constructor,
						XtendPackage.eINSTANCE.getXtendExecutable_TypeParameters(),
						ValidationMessageAcceptor.INSIGNIFICANT_INDEX,
						CONSTRUCTOR_TYPE_PARAMS_NOT_SUPPORTED);
			}
			if (!constructor.getParameters().isEmpty()) {
				error(Messages.SARLMemberValidator_20,
						constructor,
						XtendPackage.eINSTANCE.getXtendExecutable_Parameters(),
						ValidationMessageAcceptor.INSIGNIFICANT_INDEX,
						UNEXPECTED_FORMAL_PARAMETER);
			}
			if (!constructor.getExceptions().isEmpty()) {
				error(Messages.SARLMemberValidator_21,
						constructor,
						XtendPackage.eINSTANCE.getXtendExecutable_Parameters(),
						ValidationMessageAcceptor.INSIGNIFICANT_INDEX,
						UNEXPECTED_EXCEPTION_THROW);
			}
			if (constructor.getExpression() == null) {
				error(Messages.SARLMemberValidator_22,
						constructor,
						XtendPackage.eINSTANCE.getXtendExecutable_Expression(),
						ValidationMessageAcceptor.INSIGNIFICANT_INDEX,
						MISSING_BODY);
			}
		}
	}

	/** Check for abstract methods.
	 *
	 * <p>Override the Xtend behavior for: <ul>
	 * <li>not generating an error when a return type is missed. Indeed, the return type is "void" by default.</li>
	 * <li>generating a warning when "abstract" is missed.</li>
	 * </ul>
	 *
	 * @param function the function to check.
	 */
	@Check(CheckType.FAST)
	public void checkAbstract(XtendFunction function) {
		final var declarator = function.getDeclaringType();
		if (function.getExpression() == null || function.isAbstract()) {
			if (declarator instanceof XtendClass || declarator.isAnonymous()
					|| declarator instanceof SarlAgent || declarator instanceof SarlBehavior
					|| declarator instanceof SarlSkill) {
				if (function.isDispatch()) {
					error(MessageFormat.format(
							Messages.SARLMemberValidator_27,
							function.getName(), this.localClassAwareTypeNames.getReadableName(declarator)),
							XTEND_FUNCTION__NAME, -1, DISPATCH_FUNCTIONS_MUST_NOT_BE_ABSTRACT);
					return;
				}
				if (function.getCreateExtensionInfo() != null) {
					error(MessageFormat.format(
							Messages.SARLMemberValidator_28,
							function.getName(), this.localClassAwareTypeNames.getReadableName(declarator)),
							XTEND_FUNCTION__NAME, -1, CREATE_FUNCTIONS_MUST_NOT_BE_ABSTRACT);
					return;
				}
				if (declarator.isAnonymous()) {
					error(MessageFormat.format(
							Messages.SARLMemberValidator_26,
							function.getName(), this.localClassAwareTypeNames.getReadableName(declarator)),
							XTEND_FUNCTION__NAME, -1, MISSING_ABSTRACT_IN_ANONYMOUS);
				} else {
					final boolean isAbstract;
					if (declarator instanceof XtendClass cvalue) {
						isAbstract = cvalue.isAbstract();
					} else if (declarator instanceof SarlAgent cvalue) {
						isAbstract = cvalue.isAbstract();
					} else if (declarator instanceof SarlBehavior cvalue) {
						isAbstract = cvalue.isAbstract();
					} else if (declarator instanceof SarlSkill cvalue) {
						isAbstract = cvalue.isAbstract();
					} else {
						return;
					}
					if (!isAbstract && !function.isNative()) {
						error(MessageFormat.format(
								Messages.SARLMemberValidator_26,
								function.getName(), this.localClassAwareTypeNames.getReadableName(declarator)),
								XTEND_FUNCTION__NAME, -1, MISSING_ABSTRACT);
						return;
					}
				}

				if (!function.getModifiers().contains(getGrammarAccess().getAbstractKeyword())) {
					warning(MessageFormat.format(
							Messages.SARLMemberValidator_25,
							function.getName(), this.localClassAwareTypeNames.getReadableName(declarator)),
							XTEND_FUNCTION__NAME, -1, MISSING_ABSTRACT,
							function.getName(),
							this.localClassAwareTypeNames.getReadableName(declarator));
				}

			} else if (declarator instanceof XtendInterface || declarator instanceof SarlCapacity) {
				if (function.getCreateExtensionInfo() != null) {
					error(MessageFormat.format(
							Messages.SARLMemberValidator_24,
							function.getName()),
							XTEND_FUNCTION__NAME, -1, CREATE_FUNCTIONS_MUST_NOT_BE_ABSTRACT);
					return;
				}
			}
		} else if (declarator instanceof SarlCapacity) {
			// Interface is not considered for the following error because a body for a function
			// is assumed to be the default code for the interface's function
			error(Messages.SARLMemberValidator_23, XTEND_FUNCTION__NAME, -1, ABSTRACT_METHOD_WITH_BODY);
		}
	}

	protected void checkValidExtensionType(JvmIdentifiableElement identifiable, EObject source, EStructuralFeature feature) {
		LightweightTypeReference type = getActualType(identifiable);
		if (type != null && type.isPrimitive()) {
			error(MessageFormat.format(Messages.SARLMemberValidator_29, type.getHumanReadableName()), source, feature, INVALID_EXTENSION_TYPE);
		}
	}

	/** Check if the given field has a valid type for extension mechanism.
	 * 
	 * @param field the field to check.
	 */
	@Check(CheckType.FAST)
	public void checkValidExtension(XtendField field) {
		if (field.isExtension()) {
			final var jvmField = getAssociations().getJvmField(field);
			if (jvmField != null) {
				checkValidExtensionType(jvmField, field, XtendPackage.Literals.XTEND_FIELD__TYPE);
			}
		}
	}

	/** Check if the given formal parameter has a valid type for extension mechanism.
	 * 
	 * @param parameter the parameter to check.
	 */
	@Check(CheckType.FAST)
	public void checkValidExtension(XtendFormalParameter parameter) {
		// catch clauses validate their types against java.lang.Throwable
		if (parameter.isExtension() && !(parameter.eContainer() instanceof XCatchClause)) {
			checkValidExtensionType(parameter, parameter, TypesPackage.Literals.JVM_FORMAL_PARAMETER__PARAMETER_TYPE);
		}
	}

	/** Check if the given variable has a valid type for extension mechanism.
	 * 
	 * @param variableDeclaration the variable to check.
	 */
	@Check(CheckType.FAST)
	public void checkValidExtension(XtendVariableDeclaration variableDeclaration) {
		if (variableDeclaration.isExtension()) {
			checkValidExtensionType(variableDeclaration, variableDeclaration, XbasePackage.Literals.XVARIABLE_DECLARATION__NAME);
		}
	}

	/** Check if the given parameter has a valid type for extension mechanism.
	 * 
	 * @param parameter the parameter to check.
	 */
	@Check(CheckType.FAST)
	public void checkValidExtension(XtendParameter parameter) {
		if (parameter.isExtension()) {
			final var jvmParameter = getAssociations().getJvmParameter(parameter);
			if (jvmParameter != null) {
				checkValidExtensionType(jvmParameter, parameter, XtendPackage.Literals.XTEND_PARAMETER__PARAMETER_TYPE);
			}
		}
	}

	private void validateInferredType(JvmTypeReference inferredType, XtendMember member, String messagePrefix, EAttribute location) {
		if (inferredType != null) {
			final var iterator = EcoreUtil2.eAll(inferredType);
			while (iterator.hasNext()) {
				final var next = iterator.next();
				if (next instanceof JvmParameterizedTypeReference candidate) {
					final var type = candidate.getType();
					if (type instanceof JvmGenericType cvalue && !cvalue.getTypeParameters().isEmpty()) {
						if (candidate.getArguments().isEmpty()) {
							var message = new StringBuilder(messagePrefix);
							message = this.proxyAwareUIStrings.visit(inferredType, message);
							if (message != null && !isIgnored(org.eclipse.xtext.xbase.validation.IssueCodes.RAW_TYPE)) {
								var msg2 = new StringBuilder();
								msg2 = this.proxyAwareUIStrings.appendTypeSignature(type, msg2);
								final var m = MessageFormat.format(Messages.SARLMemberValidator_30,
										message.toString(),
										type.getSimpleName(),
										msg2.toString());
								addIssue(m.toString(), member, location, org.eclipse.xtext.xbase.validation.IssueCodes.RAW_TYPE);
							}
							return;
						}
					}
				} else if (next instanceof XComputedTypeReference cvalue) {
					validateInferredType(cvalue.getEquivalent(), member, messagePrefix, location);
					iterator.prune();
				}
			}
		}
	}

	/** Check if the given field has an inferred type that is not raw.
	 * 
	 * @param field the field to check.
	 */
	@Check
	public void checkNonRawTypeInferred(XtendField field) {
		if (field.getType() == null) {
			final var jvmField = getAssociations().getJvmField(field);
			// field could have been removed by AA, thus the resource is possibly null
			if (jvmField != null && jvmField.eResource() != null) {
				final var fieldType = jvmField.getType();
				validateInferredType(fieldType, field, Messages.SARLMemberValidator_31, XTEND_FIELD__NAME);
			}
		}
	}

	/** Check if the given function has an inferred return type that is not raw.
	 * 
	 * @param function the function to check.
	 */
	@Check(CheckType.EXPENSIVE)
	public void checkNonRawTypeInferred(XtendFunction function) {
		if (function.getReturnType() == null) {
			final var operation = getAssociations().getDirectlyInferredOperation(function);
			// operation could have been removed by AA, thus the resource is possibly null
			if (operation != null && operation.eResource() != null) {
				JvmTypeReference returnType = operation.getReturnType();
				validateInferredType(returnType, function, Messages.SARLMemberValidator_32, XTEND_FUNCTION__NAME);
			}
		}
	}

	/** Check if the parameter is not marked as extension and variadic at the same time.
	 * 
	 * @param param the parameter to check.
	 */
	@Check(CheckType.FAST)
	public void checkVarArgIsNotExtension(XtendParameter param) {
		if (param.isVarArg() && param.isExtension()) {
			error(Messages.SARLMemberValidator_33, param, XTEND_PARAMETER__EXTENSION, INVALID_USE_OF_VAR_ARG);
		}
	}

	/** Check if the variadic parameter is the last parameter of the enclosing function.
	 * 
	 * @param param the parameter to check.
	 */
	@SuppressWarnings("unchecked")
	@Check(CheckType.FAST)
	public void checkVarArgComesLast(XtendParameter param) {
		if (param.isVarArg()) {
			final var params = (List<XtendParameter>) param.eContainer().eGet(param.eContainingFeature());
			if (param != Iterables.getLast(params)) {
				error(Messages.SARLMemberValidator_34, param, XTEND_PARAMETER__VAR_ARG, INVALID_USE_OF_VAR_ARG);
			}
		}
	}

	/** Check if the parameters' names are valid.
	 * 
	 * @param function the function for which the parameters' names must be checked.
	 */
	@Check(CheckType.FAST)
	public void checkParameterNames(XtendFunction function) {
		for (int i = 0; i < function.getParameters().size(); ++i) {
			String leftParameterName = function.getParameters().get(i).getName();
			// standard parameter name check is done in JvmGenericTypeValidator
			if (function.getCreateExtensionInfo() != null) {
				if (Objects.equals(leftParameterName, function.getCreateExtensionInfo().getName())) {
					error(MessageFormat.format(Messages.SARLMemberValidator_35, leftParameterName),
							XTEND_EXECUTABLE__PARAMETERS, i, DUPLICATE_PARAMETER_NAME);
					if (function.getCreateExtensionInfo().eIsSet(CREATE_EXTENSION_INFO__NAME)) {
						error(MessageFormat.format(Messages.SARLMemberValidator_35, leftParameterName),
								function.getCreateExtensionInfo(),
								CREATE_EXTENSION_INFO__NAME, DUPLICATE_PARAMETER_NAME);
					} else {
						error(MessageFormat.format(Messages.SARLMemberValidator_36,
								getGrammarAccess().getItKeyword()),
								function.getCreateExtensionInfo(),
								CREATE_EXTENSION_INFO__NAME, DUPLICATE_PARAMETER_NAME);
					}
				}
			}
		}
	}

	/** Check if the given dispatch function has a valid set of formal parameters and type parameters and has
	 * a valid function name.
	 * 
	 * @param function the dispatch function to check.
	 */
	@Check(CheckType.FAST)
	public void dispatchFuncWithTypeParams(XtendFunction function) {
		if (function.isDispatch()) {
			final var dkw = getGrammarAccess().getDispatchKeyword();
			if (function.getParameters().isEmpty()) {
				error(Messages.SARLMemberValidator_37, function, 
						XTEND_MEMBER__MODIFIERS, function.getModifiers().indexOf(dkw),
						DISPATCH_FUNC_WITHOUT_PARAMS);
			}
			if (!function.getTypeParameters().isEmpty()) {
				error(Messages.SARLMemberValidator_38, function,
						XTEND_MEMBER__MODIFIERS, function.getModifiers().indexOf(dkw),
						DISPATCH_FUNC_WITH_TYPE_PARAMS);
			}
			if (function.getName().startsWith("_")) { //$NON-NLS-1$
				error(Messages.SARLMemberValidator_39, function, XTEND_FUNCTION__NAME,
						DISPATCH_FUNC_NAME_STARTS_WITH_UNDERSCORE);
			}
		}
	}

	/** Check if there is no {@code return} statement in a creation extension.
	 * 
	 * @param function the function to check.
	 */
	@Check(CheckType.EXPENSIVE)
	public void checkNoReturnsInCreateExtensions(XtendFunction function) {
		if (function.getCreateExtensionInfo() == null) {
			return;
		}
		final var found = new ArrayList<XReturnExpression>();
		collectReturnExpressions(function.getCreateExtensionInfo().getCreateExpression(), found);
		for (final var xReturnExpression : found) {
			error(Messages.SARLMemberValidator_40, xReturnExpression, null, INVALID_EARLY_EXIT);
		}
	}

	private static void collectReturnExpressions(EObject expr, List<XReturnExpression> found) {
		final var candidates = new LinkedList<EObject>();
		candidates.add(expr);
		while (!candidates.isEmpty()) {
			final var candidate = candidates.removeFirst();
			if (candidate instanceof XReturnExpression cvalue) {
				found.add(cvalue);
			} else if (expr instanceof XClosure) {
				continue;
			}
			candidates.addAll(expr.eContents());
		}
	}

	/** Check if a creation function is not of type {@code void}.
	 * 
	 * @param function the function to check.
	 */
	@Check(CheckType.FAST)
	public void checkCreateFunctionIsNotTypeVoid(XtendFunction function) {
		if (function.getCreateExtensionInfo() == null) {
			return;
		}
		if (function.getReturnType() == null) {
			final var operation = getAssociations().getDirectlyInferredOperation(function);
			if (operation != null && getParentValidator().isPrimitiveVoid(operation.getReturnType())) {
				error(MessageFormat.format(Messages.SARLMemberValidator_41, function.getName()), function,
						XtendPackage.Literals.XTEND_FUNCTION__NAME, INVALID_USE_OF_TYPE);
			}
		} else if (getParentValidator().isPrimitiveVoid(function.getReturnType())) {
			if (function.getReturnType() != null) {
				error(MessageFormat.format(Messages.SARLMemberValidator_42, function.getName()), function.getReturnType(),
						null, INVALID_USE_OF_TYPE);
			} else {
				error(MessageFormat.format(Messages.SARLMemberValidator_43, function.getName()),
						function.getReturnType(), null, INVALID_USE_OF_TYPE);
			}
		}
	}

	/** Check if a creation function is not of defined with type parameters.
	 * 
	 * @param function the function to check.
	 */
	@Check(CheckType.FAST)
	public void checkCreateFunctionIsNotGeneric(XtendFunction function) {
		if (function.getCreateExtensionInfo() != null && !function.getTypeParameters().isEmpty()) {
			error(Messages.SARLMemberValidator_44, function, 
					XTEND_MEMBER__MODIFIERS, function.getModifiers().indexOf(
							getGrammarAccess().getStaticStaticKeyword()),
					INVALID_USE_OF_STATIC);
		}
	}

	/** Check if there is a creation function is defined as static.
	 * 
	 * @param function the function to check.
	 */
	@Check(CheckType.FAST)
	public void checkCreateFunctionIsNotStatic(XtendFunction function) {
		if (function.getCreateExtensionInfo() != null && function.isStatic()) {
			error(Messages.SARLMemberValidator_45, function, 
					XTEND_MEMBER__MODIFIERS, function.getModifiers().indexOf(
							getGrammarAccess().getStaticStaticKeyword()),
					INVALID_USE_OF_STATIC);
		}
	}

	/** Check the usage of local fields.
	 * 
	 * @param field the field to check.
	 */
	@Check(CheckType.EXPENSIVE)
	public void checkLocalUsageOfDeclaredFields(XtendField field){
		if (doCheckValidMemberName(field) && !isIgnored(UNUSED_PRIVATE_MEMBER)) {
			JvmField jvmField = getAssociations().getJvmField(field);
			if (jvmField == null || jvmField.getVisibility() != JvmVisibility.PRIVATE || jvmField.eContainer() == null) {
				return;
			}
			if (getParentValidator().isLocallyUsed(jvmField, getOutermostType(field))) { 
				return;
			}
			final String message;
			if (field.isExtension()) {
				if (field.getName() == null && jvmField.getType() != null) {
					message = MessageFormat.format(Messages.SARLMemberValidator_46, 
							jvmField.getType().getIdentifier(),
							getDeclaratorName(jvmField));
				} else {
					message = MessageFormat.format(Messages.SARLMemberValidator_47, getDeclaratorName(jvmField),
							jvmField.getSimpleName());
				}
			} else {
				message = MessageFormat.format(Messages.SARLMemberValidator_48,  getDeclaratorName(jvmField),
						jvmField.getSimpleName());
			}
			addIssueToState(UNUSED_PRIVATE_MEMBER, message, XtendPackage.Literals.XTEND_FIELD__NAME);
		}
	}

	/** Check the type parameters for a function.
	 *
	 * @param xtendFunction the function to check.
	 */
	@Check(CheckType.NORMAL)
	public void checkTypeParameterForwardReferences(XtendFunction xtendFunction) {
		getParentValidator().doCheckTypeParameterForwardReference(xtendFunction.getTypeParameters());
	}

	/** Check that the given constructor has no type parameter declared.
	 * 
	 * @param constructor the constructor to check.
	 */
	@Check(CheckType.FAST)
	public void checkTypeParametersAreUnsupported(XtendConstructor constructor){
		if (!constructor.getTypeParameters().isEmpty()) {
			error(Messages.SARLMemberValidator_49,
					XtendPackage.Literals.XTEND_EXECUTABLE__TYPE_PARAMETERS,
					INSIGNIFICANT_INDEX, CONSTRUCTOR_TYPE_PARAMS_NOT_SUPPORTED);
		}
	}

	/** Check if the given field has a name that is not conflicting a keyword of Java.
	 * 
	 * @param field the field to check.
	 */
	@Check(CheckType.FAST)
	public void checkJavaKeywordConflict(XtendField field) {
		checkNoJavaKeyword(field, XtendPackage.Literals.XTEND_FIELD__NAME);
	}

	/** Check if the given function and its type parameters have names that are not conflicting a keyword of Java.
	 * 
	 * @param function the function to check.
	 */
	@Check(CheckType.FAST)
	public void checkJavaKeywordConflict(XtendFunction function) {
		if (function.eContainer() instanceof XtendAnnotationType
				&& getGrammarAccess().getDoKeyword().equals(function.getName())) {
			return;
		}
		checkNoJavaKeyword(function, XtendPackage.Literals.XTEND_FUNCTION__NAME);
		for (final var p : function.getTypeParameters()) {
			checkNoJavaKeyword(p, TypesPackage.Literals.JVM_TYPE_PARAMETER__NAME);
		}
	}

	/** Check if the type parameters of given constructor have names that are not conflicting a keyword of Java.
	 * 
	 * @param constructor the constructor to check.
	 */
	@Check(CheckType.FAST)
	public void checkJavaKeywordConflict(XtendConstructor constructor) {
		for (final var p : constructor.getTypeParameters()) {
			checkNoJavaKeyword(p, TypesPackage.Literals.JVM_TYPE_PARAMETER__NAME);
		}
	}

	/** Check if the field is initialized or has a declared type.
	 * 
	 * @param field the field to check.
	 */
	@Check(CheckType.FAST)
	public void checkNonInitializedFieldsHaveAType(XtendField field) {
		if (field.getType() == null && field.getInitialValue() == null) {
			error(MessageFormat.format(Messages.SARLMemberValidator_50,  field.getName()),
					field, XTEND_FIELD__NAME, TOO_LITTLE_TYPE_INFORMATION);
		}
	}

	/** Check if the field has a name that is not {@code "self"}.
	 * 
	 * @param field the field to check.
	 */
	@Check(CheckType.FAST)
	public void checkFieldsAreCalledSelf(XtendField field) {
		if ("self".equals(field.getName())) { //$NON-NLS-1$
			addIssue(Messages.SARLMemberValidator_51, field, XTEND_FIELD__NAME, VARIABLE_NAME_DISCOURAGED);
		}
	}

	/** Check if the function has an implicit return statement.
	 * 
	 * @param function the function to check.
	 */
	@Check(CheckType.EXPENSIVE)
	public void checkImplicitReturn(final XtendFunction function) {
		if (!isIgnored(IMPLICIT_RETURN)) {
			final var jvmOperation = getAssociations().getDirectlyInferredOperation(function);
			final var types = this.batchTypeResolver.resolveTypes(function);
			if (jvmOperation == null || !types.getActualType(jvmOperation).isPrimitiveVoid()) { 
				this.implicitReturnFinder.findImplicitReturns(function.getExpression(), implicitReturn -> {
					if (function.getExpression() != implicitReturn) {
						addIssue(Messages.SARLMemberValidator_52, implicitReturn, IMPLICIT_RETURN);
					}
				});
			}
		}
	}

	/** Check if the given behavior unit has an event specification that is conform to the event's declaration.
	 * 
	 * @param unit the event handler to check.
	 */
	@Check(CheckType.FAST)
	public void checkBehaviourUnitEventTypeConformance(final SarlBehaviorUnit unit) {
		final var event = unit.getName();
		if (event != null) {
			final var lref = getParentValidator().toLightweightTypeReference(event);
			getParentValidator().doCheckValidSuperTypeArgumentDefinition(
					lref, unit, SARL_BEHAVIOR_UNIT__NAME, INSIGNIFICANT_INDEX, true, false,
					getMessageAcceptor());
		}
	}

}
