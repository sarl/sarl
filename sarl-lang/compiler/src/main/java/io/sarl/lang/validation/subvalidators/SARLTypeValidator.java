/*
 * $Id$
 *
 * SARL is an general-purpose agent programming language.
 * More details on http://www.sarl.io
 *
 * Copyright (C) 2014-2025 SARL.io, the Original Authors and Main Authors.
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

import static com.google.common.collect.Iterables.filter;
import static io.sarl.lang.sarl.SarlPackage.Literals.SARL_EVENT__TYPE_PARAMETERS;
import static io.sarl.lang.validation.IssueCodes.DISCOURAGED_CAPACITY_DEFINITION;
import static io.sarl.lang.validation.IssueCodes.INVALID_NESTED_DEFINITION;
import static io.sarl.lang.validation.IssueCodes.UNUSED_TYPE_PARAMETER;
import static org.eclipse.xtend.core.validation.IssueCodes.DISPATCH_FUNCTIONS_DIFFERENT_PRIMITIVE_ARGS;
import static org.eclipse.xtend.core.validation.IssueCodes.DISPATCH_FUNCTIONS_MIXED_STATIC_AND_NON_STATIC;
import static org.eclipse.xtend.core.validation.IssueCodes.DISPATCH_FUNCTIONS_NON_STATIC_EXPECTED;
import static org.eclipse.xtend.core.validation.IssueCodes.DISPATCH_FUNCTIONS_STATIC_EXPECTED;
import static org.eclipse.xtend.core.validation.IssueCodes.DISPATCH_FUNCTIONS_WITH_DIFFERENT_VISIBILITY;
import static org.eclipse.xtend.core.validation.IssueCodes.DISPATCH_PLAIN_FUNCTION_NAME_CLASH;
import static org.eclipse.xtend.core.validation.IssueCodes.SINGLE_DISPATCH_FUNCTION;
import static org.eclipse.xtend.core.xtend.XtendPackage.Literals.XTEND_EXECUTABLE__PARAMETERS;
import static org.eclipse.xtend.core.xtend.XtendPackage.Literals.XTEND_FUNCTION__NAME;
import static org.eclipse.xtend.core.xtend.XtendPackage.Literals.XTEND_MEMBER__MODIFIERS;
import static org.eclipse.xtext.xbase.validation.IssueCodes.DUPLICATE_METHOD;
import static org.eclipse.xtext.xbase.validation.IssueCodes.INCOMPATIBLE_RETURN_TYPE;
import static org.eclipse.xtext.xbase.validation.IssueCodes.OVERRIDE_REDUCES_VISIBILITY;

import java.text.MessageFormat;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.List;

import org.eclipse.xtend.core.jvmmodel.DispatchHelper;
import org.eclipse.xtend.core.xtend.XtendClass;
import org.eclipse.xtend.core.xtend.XtendFunction;
import org.eclipse.xtend.core.xtend.XtendInterface;
import org.eclipse.xtend.core.xtend.XtendPackage;
import org.eclipse.xtend.core.xtend.XtendTypeDeclaration;
import org.eclipse.xtext.common.types.JvmGenericType;
import org.eclipse.xtext.common.types.JvmOperation;
import org.eclipse.xtext.common.types.JvmPrimitiveType;
import org.eclipse.xtext.common.types.JvmType;
import org.eclipse.xtext.common.types.JvmVisibility;
import org.eclipse.xtext.common.types.TypesPackage;
import org.eclipse.xtext.validation.Check;
import org.eclipse.xtext.validation.CheckType;
import org.eclipse.xtext.validation.ValidationMessageAcceptor;
import org.eclipse.xtext.xbase.typesystem.override.OverrideHelper;

import com.google.common.collect.HashMultimap;
import com.google.common.collect.Iterables;
import com.google.common.collect.Multimap;
import com.google.inject.Inject;

import io.sarl.lang.sarl.SarlAgent;
import io.sarl.lang.sarl.SarlArtifact;
import io.sarl.lang.sarl.SarlBehavior;
import io.sarl.lang.sarl.SarlCapacity;
import io.sarl.lang.sarl.SarlEvent;
import io.sarl.lang.sarl.SarlSkill;
import io.sarl.lang.sarl.SarlSpace;
import io.sarl.lang.validation.IssueCodes;

/**
 * A specialized validator to deal with SARL types.
 *
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 * @since 0.14
 */
public class SARLTypeValidator extends AbstractSARLSubValidatorWithParentLink {

	@Inject
	private DispatchHelper dispatchHelper;

	@Inject
	private OverrideHelper overrideHelper;

	/** Space keyword is reserved.
	 *
	 * @param space the space to check.
	 */
	@Check
	public void checkSpaceUse(SarlSpace space) {
		warning(MessageFormat.format(
				Messages.SARLTypeValidator_1,
				getGrammarAccess().getSpaceKeyword()),
				space,
				null,
				INSIGNIFICANT_INDEX,
				IssueCodes.UNSUPPORTED_STATEMENT);
	}

	/** Artifact keyword is reserved.
	 *
	 * @param artifact the artifact to check.
	 */
	@Check(CheckType.FAST)
	public void checkArtifactUse(SarlArtifact artifact) {
		error(MessageFormat.format(
				Messages.SARLTypeValidator_1,
				getGrammarAccess().getSpaceKeyword()),
				artifact,
				null);
	}

	/** Check the container for the SARL agents.
	 *
	 * @param agent the agent.
	 */
	@Check(CheckType.FAST)
	public void checkContainerType(SarlAgent agent) {
		final var declaringType = agent.getDeclaringType();
		if (declaringType != null) {
			final var name = canonicalName(declaringType);
			assert name != null;
			error(MessageFormat.format(Messages.SARLTypeValidator_2, name),
					agent,
					null,
					INVALID_NESTED_DEFINITION);
		}
	}

	/** Check the container for the SARL behaviors.
	 *
	 * @param behavior the behavior.
	 */
	@Check(CheckType.FAST)
	public void checkContainerType(SarlBehavior behavior) {
		final var declaringType = behavior.getDeclaringType();
		if (declaringType != null) {
			final var name = canonicalName(declaringType);
			assert name != null;
			error(MessageFormat.format(Messages.SARLTypeValidator_3, name),
					behavior,
					null,
					INVALID_NESTED_DEFINITION);
		}
	}

	/** Check the container for the SARL capacities.
	 *
	 * @param capacity the capacity.
	 */
	@Check(CheckType.FAST)
	public void checkContainerType(SarlCapacity capacity) {
		final var declaringType = capacity.getDeclaringType();
		if (declaringType != null) {
			final var name = canonicalName(declaringType);
			assert name != null;
			error(MessageFormat.format(Messages.SARLTypeValidator_4, name),
					capacity,
					null,
					INVALID_NESTED_DEFINITION);
		}
	}

	/** Check the container for the SARL skills.
	 *
	 * @param skill the skill.
	 */
	@Check(CheckType.FAST)
	public void checkContainerType(SarlSkill skill) {
		final var declaringType = skill.getDeclaringType();
		if (declaringType != null) {
			final var name = canonicalName(declaringType);
			assert name != null;
			error(MessageFormat.format(Messages.SARLTypeValidator_5, name),
					skill,
					null,
					INVALID_NESTED_DEFINITION);
		}
	}

	/** Check if the modifiers for the SARL events.
	 *
	 * @param event the event.
	 */
	@Check(CheckType.FAST)
	public void checkContainerType(SarlEvent event) {
		final var declaringType = event.getDeclaringType();
		if (declaringType != null) {
			final var name = canonicalName(declaringType);
			assert name != null;
			error(MessageFormat.format(Messages.SARLTypeValidator_6, name),
					event,
					null,
					INVALID_NESTED_DEFINITION);
		}
	}

	/** Check if a capacity has a feature defined inside.
	 *
	 * @param capacity the capacity to test.
	 */
	@Check(CheckType.FAST)
	public void checkCapacityFeatures(SarlCapacity capacity) {
		if (capacity.getMembers().isEmpty() && !isIgnored(DISCOURAGED_CAPACITY_DEFINITION)) {
			addIssue(Messages.SARLTypeValidator_7,
					capacity,
					null,
					ValidationMessageAcceptor.INSIGNIFICANT_INDEX,
					DISCOURAGED_CAPACITY_DEFINITION,
					capacity.getName(),
					"aFunction"); //$NON-NLS-1$
		}
	}

	/** Check if the dispatch functions in the given class.
	 * 
	 * @param clazz the class to test.
	 */
	@Check(CheckType.NORMAL)
	public void checkDispatchFunctions(XtendClass clazz) {
		final var type = getAssociations().getInferredType(clazz);
		if (type != null) {
			final var dispatchMethods = this.dispatchHelper.getDeclaredOrEnhancedDispatchMethods(type);
			checkDispatchNonDispatchConflict(clazz, dispatchMethods);
			for (final var signature : dispatchMethods.keySet()) {
				final var dispatchOperations = dispatchMethods.get(signature);
				final var syntheticDispatchMethod = this.dispatchHelper.getDispatcherOperation(type, signature);
				if (syntheticDispatchMethod != null) {
					final var overriddenOperation = this.overrideHelper.findOverriddenOperation(syntheticDispatchMethod);
					Boolean expectStatic = null;
					if (overriddenOperation != null) { 
						if (isMorePrivateThan(syntheticDispatchMethod.getVisibility(), overriddenOperation.getVisibility())) {
							final var msg = MessageFormat.format(Messages.SARLTypeValidator_10, overriddenOperation.getIdentifier());
							addDispatchError(type, dispatchOperations, msg, null, OVERRIDE_REDUCES_VISIBILITY);
						}
						expectStatic = Boolean.valueOf(overriddenOperation.isStatic());
					} 
					final var dispatchMethodReturnType = getActualType(clazz, syntheticDispatchMethod);
					if (dispatchOperations.size() == 1) {
						if (!isIgnored(SINGLE_DISPATCH_FUNCTION)) {
							final var singleOp = dispatchOperations.iterator().next();
							final var function = getAssociations().getXtendFunction(singleOp);
							addIssue(Messages.SARLTypeValidator_11, function, XTEND_MEMBER__MODIFIERS,
									function.getModifiers().indexOf(getGrammarAccess().getDispatchKeyword()),
									SINGLE_DISPATCH_FUNCTION);
						}
					} else {
						final var signatures = HashMultimap.<List<JvmType>, JvmOperation>create();
						final var allPrimitive = new boolean[signature.getArity()];
						Arrays.fill(allPrimitive, true);
						var isFirstLocalOperation = true;
						JvmVisibility commonVisibility = null;
						Boolean commonStatic = null;
						for (final var jvmOperation : dispatchOperations) {
							signatures.put(getParamTypes(jvmOperation, true), jvmOperation);
							for (var i = 0; i < jvmOperation.getParameters().size(); i++) {
								final var parameter = jvmOperation.getParameters().get(i);
								if (!(parameter.getParameterType().getType() instanceof JvmPrimitiveType)) {
									allPrimitive[i] = false;
								}
							}
							if (jvmOperation.getDeclaringType() == type) {
								if (expectStatic != null) {
									if (expectStatic.booleanValue() && !jvmOperation.isStatic()) {
										addDispatchError(jvmOperation, Messages.SARLTypeValidator_12,
												getGrammarAccess().getStaticStaticKeyword(), DISPATCH_FUNCTIONS_STATIC_EXPECTED);
									}
									if (!expectStatic.booleanValue() && jvmOperation.isStatic()) {
										addDispatchError(jvmOperation, Messages.SARLTypeValidator_13,
												getGrammarAccess().getStaticStaticKeyword(), DISPATCH_FUNCTIONS_NON_STATIC_EXPECTED);
									}
								}
								if (isFirstLocalOperation) {
									commonVisibility = jvmOperation.getVisibility();
									commonStatic = Boolean.valueOf(jvmOperation.isStatic());
									isFirstLocalOperation = false;
								} else {
									if (jvmOperation.getVisibility() != commonVisibility) {
										commonVisibility = null;
									}
									if (commonStatic != null && commonStatic.booleanValue() != jvmOperation.isStatic()) {
										commonStatic = null;
									}
								}
								if (dispatchMethodReturnType != null) {
									final var function = getAssociations().getXtendFunction(jvmOperation);
									if (function != null) {
										final var operationType = getActualType(function.getExpression(), jvmOperation);
										if (!dispatchMethodReturnType.isAssignableFrom(operationType)) {
											error(MessageFormat.format(Messages.SARLTypeValidator_14,
													dispatchMethodReturnType.getHumanReadableName(),
													operationType.getHumanReadableName()), function,
													XtendPackage.Literals.XTEND_FUNCTION__RETURN_TYPE,
													ValidationMessageAcceptor.INSIGNIFICANT_INDEX, INCOMPATIBLE_RETURN_TYPE);
										}
									}
								}
							}
						}
						if (commonVisibility == null) {
							addDispatchError(type, dispatchOperations, Messages.SARLTypeValidator_15, 
									null, DISPATCH_FUNCTIONS_WITH_DIFFERENT_VISIBILITY);
						}
						if (expectStatic == null && commonStatic == null) {
							addDispatchError(type, dispatchOperations, Messages.SARLTypeValidator_16, 
									getGrammarAccess().getStaticStaticKeyword(),
									DISPATCH_FUNCTIONS_MIXED_STATIC_AND_NON_STATIC);
						}
						for (final List<JvmType> paramTypes : signatures.keySet()) {
							Collection<JvmOperation> ops = signatures.get(paramTypes);
							if (ops.size() > 1) {
								if (Iterables.any(ops, input -> !getParamTypes(input, false).equals(paramTypes))) {
									for (final var jvmOperation : ops) {
										final var function = getAssociations().getXtendFunction(jvmOperation);
										error(Messages.SARLTypeValidator_18,
												function, null, DUPLICATE_METHOD);
									}
								}
							}
						}
						if (!isIgnored(DISPATCH_FUNCTIONS_DIFFERENT_PRIMITIVE_ARGS)) {
							for (var i = 0; i < allPrimitive.length; ++i) {
								if (allPrimitive[i]) {
									final var operationIter = dispatchOperations.iterator();
									final var paramType1 = operationIter.next().getParameters().get(i).getParameterType().getType();
									while (operationIter.hasNext()) {
										final var paramType2 = operationIter.next().getParameters().get(i).getParameterType().getType();
										if (!paramType2.equals(paramType1)) {
											for (final var jvmOperation : dispatchOperations) {
												final var function = getAssociations().getXtendFunction(jvmOperation);
												addIssue(Messages.SARLTypeValidator_17,
														function, XTEND_EXECUTABLE__PARAMETERS, i,
														DISPATCH_FUNCTIONS_DIFFERENT_PRIMITIVE_ARGS);
											}
											break;
										}
									}
								}
							}
						}
					}
				}
			}
		}
	}

	private void addDispatchError(JvmGenericType type, Iterable<JvmOperation> operations, String message, String modifier, String ISSUE_ID) {
		for (final var jvmOperation : operations) {
			if (jvmOperation.getDeclaringType() == type) {
				addDispatchError(jvmOperation, message, modifier, ISSUE_ID);
			}
		}
	}
	
	private void addDispatchError(JvmOperation jvmOperation, String message, String modifier, String ISSUE_ID) {
		final var function = getAssociations().getXtendFunction(jvmOperation);
		if (function != null) {
			var modifierIndex = -1;
			if(modifier != null) {
				modifierIndex = function.getModifiers().indexOf(modifier);
			} else {
				for (var i = 0; i < function.getModifiers().size(); ++i) {
					if (getVisibilityModifiers().contains(function.getModifiers().get(i))) {
						modifierIndex = i;
						break;
					}
				}
			}
			if(modifierIndex == -1) {
				modifierIndex = function.getModifiers().indexOf(getGrammarAccess().getDispatchKeyword());
			}
			error(message, function, XTEND_MEMBER__MODIFIERS, modifierIndex, ISSUE_ID);
		}
	}

	private List<JvmType> getParamTypes(JvmOperation jvmOperation, boolean wrapPrimitives) {
		final var types = new ArrayList<JvmType>();
		for (final var p : jvmOperation.getParameters()) {
			var typeReference = toLightweightTypeReference(p.getParameterType());
			if (wrapPrimitives) {
				typeReference = typeReference.getWrapperTypeIfPrimitive();
			}
			types.add(typeReference.getType());
		}
		return types;
	}

	private static boolean isMorePrivateThan(JvmVisibility o1, JvmVisibility o2) {
		if (o1 == o2) {
			return false;
		}
		switch (o1) {
		case DEFAULT:
			return o2 != JvmVisibility.PRIVATE;
		case PRIVATE:
			return true;
		case PROTECTED:
			return o2 == JvmVisibility.PUBLIC;
		case PUBLIC:
			return false;
		default:
			throw new IllegalArgumentException("Unknown JvmVisibility " + o1); //$NON-NLS-1$
		}
	}

	private void checkDispatchNonDispatchConflict(XtendClass clazz,
			Multimap<DispatchHelper.DispatchSignature, JvmOperation> dispatchMethods) {
		if(isIgnored(DISPATCH_PLAIN_FUNCTION_NAME_CLASH)) {
			return;
		}
		final var  nonDispatchMethods = HashMultimap.<DispatchHelper.DispatchSignature, XtendFunction>create();
		for (final var method: filter(clazz.getMembers(), XtendFunction.class)) {
			if (!method.isDispatch()) {
				nonDispatchMethods.put(new DispatchHelper.DispatchSignature(method.getName(),
						method.getParameters().size()), method);
			}
		}
		for (final var dispatchSignature : dispatchMethods.keySet()) {
			if (nonDispatchMethods.containsKey(dispatchSignature)) {
				for (final var function : nonDispatchMethods.get(dispatchSignature)) {
					addIssue(Messages.SARLTypeValidator_8, function,
							XTEND_FUNCTION__NAME,
							DISPATCH_PLAIN_FUNCTION_NAME_CLASH);
				}
				for (final var operation : dispatchMethods.get(dispatchSignature)) {
					final var function = getAssociations().getXtendFunction(operation);
					if (function.eResource() == clazz.eResource()) {
						addIssue(Messages.SARLTypeValidator_9, function,
								XTEND_FUNCTION__NAME,
								DISPATCH_PLAIN_FUNCTION_NAME_CLASH);
					}
				}
			}
		}
	}

	/** Check the type parameters for a class.
	 *
	 * @param xtendClass the type to check.
	 */
	@Check(CheckType.NORMAL)
	public void checkTypeParameterForwardReferences(XtendClass xtendClass) {
		getParentValidator().doCheckTypeParameterForwardReference(xtendClass.getTypeParameters());
	}

	/** Check the type parameters for an interface.
	 *
	 * @param xtendInterface the type to check.
	 */
	@Check(CheckType.NORMAL)
	public void checkTypeParameterForwardReferences(XtendInterface xtendInterface) {
		getParentValidator().doCheckTypeParameterForwardReference(xtendInterface.getTypeParameters());
	}

	/** Check the usage of JAva keywords as names.
	 *
	 * @param member the member to check.
	 */
	@Check(CheckType.FAST)
	public void checkJavaKeywordConflict(XtendTypeDeclaration member) {
		checkNoJavaKeyword(member, XtendPackage.Literals.XTEND_TYPE_DECLARATION__NAME);
	}

	/** Check the usage of JAva keywords as names.
	 *
	 * @param member the member to check.
	 */
	@Check(CheckType.FAST)
	public void checkJavaKeywordConflict(XtendClass member) {
		for (final var p : member.getTypeParameters()) {
			checkNoJavaKeyword(p, TypesPackage.Literals.JVM_TYPE_PARAMETER__NAME);
		}
	}

	/** Check the usage of JAva keywords as names..
	 *
	 * @param member the member to check.
	 */
	@Check(CheckType.FAST)
	public void checkJavaKeywordConflict(XtendInterface member) {
		for (final var p : member.getTypeParameters()) {
			checkNoJavaKeyword(p, TypesPackage.Literals.JVM_TYPE_PARAMETER__NAME);
		}
	}

	/** Check the type parameters of the event.
	 *
	 * @param event the event to check.
	 */
	@Check(CheckType.FAST)
	public void checkEventTypeParameters(SarlEvent event) {
		if (!isIgnored(UNUSED_TYPE_PARAMETER)) {
			final var parameters = event.getTypeParameters();
			if (!parameters.isEmpty()) {
				var  i = 0;
				for (final var parameter : parameters) {
					if (!getParentValidator().isTypeParameterLocallyUsedInEvent(parameter, event)) { 
						addIssue(MessageFormat.format(Messages.SARLTypeValidator_19, parameter.getSimpleName()),
								event,
								SARL_EVENT__TYPE_PARAMETERS,
								i,
								UNUSED_TYPE_PARAMETER);
					}
					++i;
				}
			}
		}
	}


}
