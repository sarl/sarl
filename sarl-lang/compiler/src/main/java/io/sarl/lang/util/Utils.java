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

package io.sarl.lang.util;

import static io.sarl.lang.core.util.SarlUtils.HIDDEN_MEMBER_CHARACTER;
import static io.sarl.lang.core.util.SarlUtils.isHiddenMember;

import java.lang.annotation.Annotation;
import java.lang.reflect.Modifier;
import java.security.MessageDigest;
import java.security.NoSuchAlgorithmException;
import java.util.ArrayList;
import java.util.BitSet;
import java.util.Collections;
import java.util.HashMap;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;
import java.util.TreeMap;
import java.util.function.BiConsumer;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import org.eclipse.emf.common.notify.Notifier;
import org.eclipse.emf.ecore.EObject;
import org.eclipse.emf.ecore.EStructuralFeature;
import org.eclipse.emf.ecore.resource.ResourceSet;
import org.eclipse.emf.ecore.util.EcoreUtil;
import org.eclipse.xtend.core.xtend.XtendFunction;
import org.eclipse.xtend.core.xtend.XtendParameter;
import org.eclipse.xtend.core.xtend.XtendTypeDeclaration;
import org.eclipse.xtext.EcoreUtil2;
import org.eclipse.xtext.common.types.JvmConstraintOwner;
import org.eclipse.xtext.common.types.JvmConstructor;
import org.eclipse.xtext.common.types.JvmDeclaredType;
import org.eclipse.xtext.common.types.JvmField;
import org.eclipse.xtext.common.types.JvmFormalParameter;
import org.eclipse.xtext.common.types.JvmGenericType;
import org.eclipse.xtext.common.types.JvmIdentifiableElement;
import org.eclipse.xtext.common.types.JvmLowerBound;
import org.eclipse.xtext.common.types.JvmMember;
import org.eclipse.xtext.common.types.JvmOperation;
import org.eclipse.xtext.common.types.JvmParameterizedTypeReference;
import org.eclipse.xtext.common.types.JvmType;
import org.eclipse.xtext.common.types.JvmTypeConstraint;
import org.eclipse.xtext.common.types.JvmTypeParameter;
import org.eclipse.xtext.common.types.JvmTypeParameterDeclarator;
import org.eclipse.xtext.common.types.JvmTypeReference;
import org.eclipse.xtext.common.types.JvmUpperBound;
import org.eclipse.xtext.common.types.JvmVisibility;
import org.eclipse.xtext.common.types.JvmWildcardTypeReference;
import org.eclipse.xtext.common.types.TypesFactory;
import org.eclipse.xtext.common.types.util.TypeReferences;
import org.eclipse.xtext.naming.QualifiedName;
import org.eclipse.xtext.nodemodel.util.NodeModelUtils;
import org.eclipse.xtext.serializer.ISerializer;
import org.eclipse.xtext.util.EmfFormatter;
import org.eclipse.xtext.util.XtextVersion;
import org.eclipse.xtext.xbase.XAbstractFeatureCall;
import org.eclipse.xtext.xbase.XExpression;
import org.eclipse.xtext.xbase.XFeatureCall;
import org.eclipse.xtext.xbase.XMemberFeatureCall;
import org.eclipse.xtext.xbase.XVariableDeclaration;
import org.eclipse.xtext.xbase.annotations.xAnnotations.XAnnotation;
import org.eclipse.xtext.xbase.compiler.ImportManager;
import org.eclipse.xtext.xbase.jvmmodel.JvmTypeReferenceBuilder;
import org.eclipse.xtext.xbase.jvmmodel.JvmTypesBuilder;
import org.eclipse.xtext.xbase.lib.CollectionLiterals;
import org.eclipse.xtext.xbase.lib.Functions.Function1;
import org.eclipse.xtext.xbase.lib.Pure;
import org.eclipse.xtext.xbase.typesystem.conformance.TypeConformanceComputationArgument;
import org.eclipse.xtext.xbase.typesystem.references.LightweightTypeReference;
import org.eclipse.xtext.xbase.typesystem.references.LightweightTypeReferenceFactory;
import org.eclipse.xtext.xbase.typesystem.references.StandardTypeReferenceOwner;
import org.eclipse.xtext.xbase.typesystem.util.CommonTypeComputationServices;
import org.eclipse.xtext.xtype.XFunctionTypeRef;
import org.eclipse.xtext.xtype.XtypeFactory;
import org.osgi.framework.Version;

import com.google.common.base.Objects;
import com.google.common.base.Strings;
import com.google.common.collect.Iterables;

import io.sarl.lang.core.SARLVersion;
import io.sarl.lang.core.annotation.EarlyExit;
import io.sarl.lang.core.util.OutParameter;
import io.sarl.lang.sarl.SarlAction;
import io.sarl.lang.sarl.SarlFormalParameter;
import io.sarl.lang.sarl.actionprototype.ActionParameterTypes;
import io.sarl.lang.sarl.actionprototype.ActionPrototype;
import io.sarl.lang.sarl.actionprototype.IActionPrototypeProvider;
import io.sarl.lang.services.SARLGrammarKeywordAccess;

/**
 * Utilities functions on JvmElements.
 *
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 */
public final class Utils {

	/** Name of a static initializer.
	 *
	 * @since 0.6
	 */
	private static final String STATIC_CONSTRUCTOR_NAME = "static$new"; //$NON-NLS-1$

	private static final String PREFIX_DEFAULT_VALUE_FUNCTION = HIDDEN_MEMBER_CHARACTER + "DEFAULT_VALUE" //$NON-NLS-1$
			+ HIDDEN_MEMBER_CHARACTER;

	private static final String PREFIX_CAPACITY_IMPLEMENTATION = HIDDEN_MEMBER_CHARACTER + "CAPACITY_USE" //$NON-NLS-1$
			+ HIDDEN_MEMBER_CHARACTER;

	private static final String POSTFIX_CAPACITY_IMPLEMENTATION_CALLER = HIDDEN_MEMBER_CHARACTER + "CALLER"; //$NON-NLS-1$

	private static final String PREFIX_GUARD_EVALUATOR = HIDDEN_MEMBER_CHARACTER + "guardEvaluator" //$NON-NLS-1$
			+ HIDDEN_MEMBER_CHARACTER;

	private static final String PREFIX_GUARD = HIDDEN_MEMBER_CHARACTER + "behaviorUnitGuard" //$NON-NLS-1$
			+ HIDDEN_MEMBER_CHARACTER;

	private static final String PREFIX_EVENT_HANDLER = HIDDEN_MEMBER_CHARACTER + "behaviorUnit" //$NON-NLS-1$
			+ HIDDEN_MEMBER_CHARACTER;

	private static final String PREFIX_LOCAL_VARIABLE = "___SARLlocal_"; //$NON-NLS-1$

	private static final String HIDDEN_MEMBER_REPLACEMENT_CHARACTER = "_"; //$NON-NLS-1$

	private static final String SARL_PACKAGE_PREFIX;

	private static final String SARL_VERSION_FIELD_NAME_STR = "SPECIFICATION_RELEASE_VERSION_STRING"; //$NON-NLS-1$

	private static boolean checkSarlVersionClass = true;

	private static final Pattern IMPLICIT_LAMBDA_PARAMETER_PATTERN = Pattern.compile("^\\$[0-9]+$"); //$NON-NLS-1$

	/** @since 0.15
	 */
	private static final String JAVA_MAIN_FUNCTION_NAME = "main"; //$NON-NLS-1$

	static {
		final var name = new StringBuilder();
		final var components = EarlyExit.class.getPackage().getName().split("\\."); //$NON-NLS-1$
		final var len = Math.min(3, components.length);
		for (var i = 0; i < len; ++i) {
			name.append(components[i]);
			name.append("."); //$NON-NLS-1$
		}
		SARL_PACKAGE_PREFIX = name.toString();
	}

	private Utils() {
		//
	}

	/** Replies if the given name is for a function that could be considered as the main (Java) entry point of the program.
	 *
	 * @param name the name of the function to test.
	 * @return {@code true} if the given name is for a main Java function.
	 * @since 0.15
	 */
	public static boolean isNameForJavaMainFunction(String name) {
		return Objects.equal(JAVA_MAIN_FUNCTION_NAME, name);
	}

	/** Replies the name for a function that could be considered as the main (Java) entry point of the program.
	 *
	 * @return the function simple name.
	 * @since 0.15
	 */
	public static String getNameForJavaMainFunction() {
		return JAVA_MAIN_FUNCTION_NAME;
	}

	/** Replies if the given function is a main function.
	 *
	 * @param function the function.
	 * @param services the services for comparing the types.
	 * @return {@code true} if the function is a main function.
	 * @since 0.15
	 */
	public static boolean isMainFunctionDeclaration(XtendFunction function, CommonTypeComputationServices services) {
		if (isMainFunctionDeclarationExcludingReturnType(function, services)) {
			var retType = toLightweightTypeReference(function.getReturnType(), services);
			if (retType == null || retType.isPrimitiveVoid()) {
				return true;
			}
		}
		return false;
	}

	/** Replies if the given function is a main function without considering the return type.
	 *
	 * @param function the function.
	 * @param services the services for comparing the types.
	 * @return {@code true} if the function is a main function.
	 * @since 0.15
	 */
	public static boolean isMainFunctionDeclarationExcludingReturnType(XtendFunction function, CommonTypeComputationServices services) {
		if (isNameForJavaMainFunction(function.getName()) && function.isStatic() && !function.isDispatch()) {
			if (function.getParameters().size() == 1) {
				final var providedArg = function.getParameters().get(0);
				var argType = toLightweightTypeReference(providedArg.getParameterType(), services);
				final var isVarArgsFromSource = isVarArg(function.getParameters());
				if (!isVarArgsFromSource && argType.isArray()) {
					argType = argType.getComponentType();
				}
				return argType.isType(String.class);
			}
		}
		return false;
	}

	/** Replies if the given name is the hidden name for a parameter's default value.
	 *
	 * @param name the name to test.
	 * @return {@code true} if the given name is the one for a default value.
	 * @since 0.12
	 */
	@Pure
	public static boolean isDynamicDefaultValueFunctionName(String name) {
		return name != null && name.startsWith(PREFIX_DEFAULT_VALUE_FUNCTION);
	}

	/** Replies if the given name is the hidden name for a static constructor.
	 *
	 * @param name the name to test.
	 * @return {@code true} if the given name is the one for a static constructor.
	 * @since 0.12
	 */
	@Pure
	public static boolean isStaticConstructorName(String name) {
		return STATIC_CONSTRUCTOR_NAME.equals(name);
	}

	/** Replies the name given to the static constructors.
	 *
	 * @return the hidden name for static constructors.
	 * @since 0.12
	 */
	@Pure
	public static String getStaticConstructorName() {
		return STATIC_CONSTRUCTOR_NAME;
	}

	/** Analyzing the type hierarchy of the given interface and
	 * extract hierarchy information.
	 *
	 * @param jvmElement - the element to analyze
	 * @param operations - filled with the operations inside and inherited by the element.
	 * @param fields - filled with the fields inside and inherited by the element.
	 * @param sarlSignatureProvider - provider of tools related to action signatures.
	 * @see org.eclipse.xtext.xbase.typesystem.override.OverrideHelper
	 */
	public static void populateInterfaceElements(
			JvmDeclaredType jvmElement,
			Map<ActionPrototype, JvmOperation> operations,
			Map<String, JvmField> fields,
			IActionPrototypeProvider sarlSignatureProvider) {
		for (final var feature : jvmElement.getAllFeatures()) {
			if (!"java.lang.Object".equals(feature.getDeclaringType().getQualifiedName())) { //$NON-NLS-1$
				if (operations != null && feature instanceof JvmOperation operation) {
					final var sig = sarlSignatureProvider.createParameterTypesFromJvmModel(
							operation.isVarArgs(), operation.getParameters());
					final var actionKey = sarlSignatureProvider.createActionPrototype(
							operation.getSimpleName(), sig);
					operations.put(actionKey, operation);
				} else if (fields != null && feature instanceof JvmField) {
					fields.put(feature.getSimpleName(), (JvmField) feature);
				}
			}
		}
	}

	/** Analyzing the type hierarchy of the given element, and
	 * extract any type-related information.
	 *
	 * @param jvmElement - the element to analyze
	 * @param finalOperations - filled with the final operations inherited by the element.
	 * @param overridableOperations - filled with the oervrideable operations inherited by the element.
	 * @param inheritedFields - filled with the fields inherited by the element.
	 * @param operationsToImplement - filled with the abstract operations inherited by the element.
	 * @param superConstructors - filled with the construstors of the super type.
	 * @param sarlSignatureProvider - provider of tools related to action signatures.
	 * @see org.eclipse.xtext.xbase.typesystem.override.OverrideHelper
	 */
	public static void populateInheritanceContext(
			JvmDeclaredType jvmElement,
			Map<ActionPrototype, JvmOperation> finalOperations,
			Map<ActionPrototype, JvmOperation> overridableOperations,
			Map<String, JvmField> inheritedFields,
			Map<ActionPrototype, JvmOperation> operationsToImplement,
			Map<ActionParameterTypes, JvmConstructor> superConstructors,
			IActionPrototypeProvider sarlSignatureProvider) {
		populateInheritanceContext(
				jvmElement,
				jvmElement.getExtendedClass(),
				jvmElement.getExtendedInterfaces(),
				finalOperations, overridableOperations, inheritedFields,
				operationsToImplement, superConstructors, sarlSignatureProvider);
	}

	/** Analyzing the type hierarchy of the given element, and
	 * extract any type-related information.
	 *
	 * @param jvmElement - the element to analyze
	 * @param extendedClass the extended class.
	 * @param extendedInterfaces the extended interfaces.
	 * @param finalOperations - filled with the final operations inherited by the element.
	 * @param overridableOperations - filled with the oervrideable operations inherited by the element.
	 * @param inheritedFields - filled with the fields inherited by the element.
	 * @param operationsToImplement - filled with the abstract operations inherited by the element.
	 * @param superConstructors - filled with the construstors of the super type.
	 * @param sarlSignatureProvider - provider of tools related to action signatures.
	 * @see org.eclipse.xtext.xbase.typesystem.override.OverrideHelper
	 */
	public static void populateInheritanceContext(
			JvmDeclaredType jvmElement,
			JvmTypeReference extendedClass,
			Iterable<? extends JvmTypeReference> extendedInterfaces,
			Map<ActionPrototype, JvmOperation> finalOperations,
			Map<ActionPrototype, JvmOperation> overridableOperations,
			Map<String, JvmField> inheritedFields,
			Map<ActionPrototype, JvmOperation> operationsToImplement,
			Map<ActionParameterTypes, JvmConstructor> superConstructors,
			IActionPrototypeProvider sarlSignatureProvider) {
		// Get the operations that must be implemented
		if (operationsToImplement != null && extendedInterfaces != null) {
			for (final var interfaceReference : extendedInterfaces) {
				final var interfaceReferenceType = interfaceReference.getType();
				if (interfaceReferenceType instanceof JvmGenericType cvalue) {
					for (final var feature : cvalue.getAllFeatures()) {
						if (!"java.lang.Object".equals(//$NON-NLS-1$
								feature.getDeclaringType().getQualifiedName())) {
							if (feature instanceof JvmOperation operation) {
								final var sig = sarlSignatureProvider.createParameterTypesFromJvmModel(
										operation.isVarArgs(), operation.getParameters());
								final var actionKey = sarlSignatureProvider.createActionPrototype(
										operation.getSimpleName(), sig);
								if (operation.isDefault()) {
									if (overridableOperations != null) {
										overridableOperations.put(actionKey, operation);
									}
								} else {
									if (operationsToImplement != null) {
										operationsToImplement.put(actionKey, operation);
									}
								}
							}
						}
					}
				}
			}
		}

		// Check on the implemented features, inherited from the super type
		if (extendedClass != null) {
			final var extendedClassJvmType = extendedClass.getType();
			if (extendedClassJvmType instanceof JvmGenericType parentType) {
				for (final var feature : parentType.getAllFeatures()) {
					if (!"java.lang.Object".equals(feature.getDeclaringType().getQualifiedName()) //$NON-NLS-1$
							&& isVisible(jvmElement, feature)
							&& !isHiddenMember(feature.getSimpleName())) {
						if (feature instanceof JvmOperation operation) {
							if (!feature.isStatic()) {
								final var sig = sarlSignatureProvider.createParameterTypesFromJvmModel(
										operation.isVarArgs(), operation.getParameters());
								final var actionKey = sarlSignatureProvider.createActionPrototype(
										feature.getSimpleName(), sig);
								if (operation.isAbstract() && !operation.isDefault()) {
									if (operationsToImplement != null) {
										operationsToImplement.put(actionKey, operation);
									}
								} else if (operation.isFinal()) {
									if (finalOperations != null) {
										finalOperations.put(actionKey, operation);
									}
									if (operationsToImplement != null) {
										operationsToImplement.remove(actionKey);
									}
								} else {
									if (overridableOperations != null) {
										overridableOperations.put(actionKey, operation);
									}
									if (operationsToImplement != null) {
										operationsToImplement.remove(actionKey);
									}
								}
							}
						} else if (feature instanceof JvmField cvalue && inheritedFields != null) {
							inheritedFields.put(feature.getSimpleName(), cvalue);
						}
					}
				}

				if (superConstructors != null) {
					for (final var cons : parentType.getDeclaredConstructors()) {
						final var sig = sarlSignatureProvider.createParameterTypesFromJvmModel(
								cons.isVarArgs(), cons.getParameters());
						superConstructors.put(sig,  cons);
					}
				}
			}
		}
	}

	/** Replies if the target feature is visible from the type.
	 *
	 * @param fromType - the type from which the feature visibility is tested.
	 * @param target - the feature to test for the visibility.
	 * @return {@code true} if the given type can see the target feature.
	 */
	public static boolean isVisible(JvmDeclaredType fromType, JvmMember target) {
		switch (target.getVisibility()) {
		case DEFAULT:
			return target.getDeclaringType().getPackageName().equals(fromType.getPackageName());
		case PROTECTED:
		case PUBLIC:
			return true;
		case PRIVATE:
		default:
		}
		return false;
	}

	/** Replies if the last parameter is a variadic parameter.
	 *
	 * @param params - parameters.
	 * @return {@code true} if the late parameter is variadic.
	 */
	public static boolean isVarArg(List<? extends XtendParameter> params) {
		assert params != null;
		if (params.size() > 0) {
			final var param = params.get(params.size() - 1);
			assert param != null;
			return param.isVarArg();
		}
		return false;
	}

	/** Replies if the given name is related to an hidden parameter for closures.
	 *
	 * <p>An hidden parameter for closures is the name assigned to a formal parameter
	 * for a closure when no name is explicitly provided.
	 *
	 * @param name - the name to test.
	 * @return {@code true} if the given name is reserved by SARL.
	 */
	public static boolean isImplicitLambdaParameterName(String name) {
		return IMPLICIT_LAMBDA_PARAMETER_PATTERN.matcher(name).matches();
	}

	/** Replies a fixed version of the given name assuming
	 * that it is an hidden action, and reformating
	 * the reserved text.
	 *
	 * <p>An hidden action is an action that is generated by the SARL
	 * compiler, and that cannot be defined by the SARL user.
	 *
	 * @param name - the name to fix.
	 * @return the fixed name.
	 */
	public static String fixHiddenMember(String name) {
		return name.replaceAll(Pattern.quote(HIDDEN_MEMBER_CHARACTER),
				Matcher.quoteReplacement(HIDDEN_MEMBER_REPLACEMENT_CHARACTER));
	}

	/** Create the name of the hidden function that is containing a parameter's default value.
	 *
	 * @param id the id of the default value.
	 * @return the method name.
	 * @since 0.12
	 */
	public static String createNameForHiddenDefaultValueFunction(String id) {
		return PREFIX_DEFAULT_VALUE_FUNCTION + fixHiddenMember(id.toUpperCase());
	}

	/** Create the name of the hidden field that is containing a capacity implementation.
	 *
	 * @param id the id of the capacity.
	 * @return the field name.
	 */
	public static String createNameForHiddenCapacityImplementationAttribute(String id) {
		return PREFIX_CAPACITY_IMPLEMENTATION + fixHiddenMember(id.toUpperCase()).replace(".", //$NON-NLS-1$
				HIDDEN_MEMBER_REPLACEMENT_CHARACTER);
	}

	/** Create the name of the hidden method that is calling a capacity implementation.
	 *
	 * @param capacityImplementationFieldName the name of the extension field.
	 * @return the method name.
	 */
	public static String createNameForHiddenCapacityImplementationCallingMethodFromFieldName(
			String capacityImplementationFieldName) {
		return capacityImplementationFieldName + POSTFIX_CAPACITY_IMPLEMENTATION_CALLER;
	}

	/** Replies if the given simple name is the name of the hidden method that is calling a capacity implementation.
	 *
	 * @param simpleName the simple name.
	 * @return {@code true} if the given simple name if for the hidden method for capacuty uses.
	 */
	public static boolean isNameForHiddenCapacityImplementationCallingMethod(String simpleName) {
		return simpleName != null && simpleName.startsWith(PREFIX_CAPACITY_IMPLEMENTATION)
				&& simpleName.endsWith(POSTFIX_CAPACITY_IMPLEMENTATION_CALLER);
	}

	/** Create the name of the hidden local variable.
	 *
	 * @param id the name of the local variable.
	 * @return the variable name.
	 */
	public static String createNameForHiddenLocalVariable(String id) {
		return PREFIX_LOCAL_VARIABLE + fixHiddenMember(id);
	}

	private static StringBuilder buildNameForHiddenEventMethod(JvmParameterizedTypeReference eventId,
			String prefix, boolean appendTypeSimpleName, boolean forceTypeParameterBoundsInName) {
		final var fullName = new StringBuilder(prefix);
		final var fullName0 = new StringBuilder();
		final var result = forEachTypeParameterName(eventId, (name, i) -> {
			fullName0.append(HIDDEN_MEMBER_CHARACTER).append(HIDDEN_MEMBER_CHARACTER);
			if (name == null) {
				fullName0.append(fixHiddenMember(Object.class.getSimpleName()));
			} else {
				fullName0.append(fixHiddenMember(name.getSimpleName()));
			}
		});
		if (result == null) {
			if (appendTypeSimpleName) {
				fullName.append(fixHiddenMember(eventId.getSimpleName()));
			}
		} else {
			if (appendTypeSimpleName) {
				fullName.append(fixHiddenMember(eventId.getType().getSimpleName()));
			}
			if (forceTypeParameterBoundsInName || result.isBoundInFunctionName()) {
				fullName.append(fullName0);
			}
		}
		return fullName;
	}
	
	/** Create the name of the hidden method that is containing the evaluation of all the guards for a given event.
	 *
	 * @param eventId the id of the event.
	 * @return the method name.
	 * @since 0.14
	 */
	public static String createNameForHiddenGuardGeneralEvaluatorMethod(JvmParameterizedTypeReference eventId) {
		return buildNameForHiddenEventMethod(eventId, PREFIX_GUARD_EVALUATOR, true, true).toString();
	}

	/** Create the name of the hidden method that is containing the event guard evaluation.
	 *
	 * @param eventId the id of the event.
	 * @param handlerIndex the index of the handler in the container type.
	 * @return the method name.
	 * @since 0.14
	 */
	public static String createNameForHiddenGuardEvaluatorMethod(JvmParameterizedTypeReference eventId, int handlerIndex) {
		final var fullName = buildNameForHiddenEventMethod(eventId, PREFIX_GUARD, true, false);
		fullName.append(HIDDEN_MEMBER_CHARACTER).append(handlerIndex);
		return fullName.toString();
	}

	/** Create the name of the hidden method that is containing the event handler code.
	 *
	 * @param eventId the id of the event.
	 * @param handlerIndex the index of the handler in the container type.
	 * @return the attribute name.
	 * @since 0.14
	 */
	public static String createNameForHiddenEventHandlerMethod(JvmParameterizedTypeReference eventId, int handlerIndex) {
		final var fullName = buildNameForHiddenEventMethod(eventId, PREFIX_EVENT_HANDLER, true, false);
		fullName.append(HIDDEN_MEMBER_CHARACTER).append(handlerIndex);
		return fullName.toString();
	}

	/** Replies if the given reference is pointing to a class type.
	 *
	 * @param typeRef - the type reference to test.
	 * @return {@code true} if the pointed element is a class type.
	 */
	public static boolean isClass(LightweightTypeReference typeRef) {
		final var t = typeRef.getType();
		if (t instanceof JvmGenericType cvalue) {
			return !cvalue.isInterface();
		}
		return false;
	}

	/** Replies if the given type is a class type.
	 *
	 * @param type - the type to test.
	 * @return {@code true} if the element is a class type.
	 */
	public static boolean isClass(Class<?> type) {
		return !type.isInterface();
	}

	/** Replies if the given reference is referencing a final type.
	 *
	 * @param expressionTypeRef - the type reference to test.
	 * @return {@code true} if the given type is final.
	 */
	public static boolean isFinal(LightweightTypeReference expressionTypeRef) {
		if (expressionTypeRef.isArray()) {
			return isFinal(expressionTypeRef.getComponentType());
		}
		if (expressionTypeRef.isPrimitive()) {
			return true;
		}
		return expressionTypeRef.getType() instanceof JvmDeclaredType cvalue
				&& cvalue.isFinal();
	}

	/** Replies if the given type is a final type.
	 *
	 * @param expressionType - the type to test.
	 * @return {@code true} if the given type is final.
	 */
	public static boolean isFinal(Class<?> expressionType) {
		if (expressionType.isArray()) {
			return isFinal(expressionType.getComponentType());
		}
		if (expressionType.isPrimitive()) {
			return true;
		}
		return expressionType.isEnum()
				|| Modifier.isFinal(expressionType.getModifiers());
	}

	/** Replies if the given type is an interface.
	 *
	 * @param type - the type to test.
	 * @return {@code true} if the given type is an interface.
	 */
	public static boolean isInterface(LightweightTypeReference type) {
		return type.getType() instanceof JvmGenericType cvalue
				&& cvalue.isInterface();
	}

	/** Replies if it is allowed to cast between the given types.
	 *
	 * @param fromType - source type
	 * @param toType - target type
	 * @param enablePrimitiveWidening - indicates if the widening of the primitive types is allowed.
	 * @param enableVoidMatchingNull - indicates if the {@code null} is matching {@code void}.
	 * @param allowSynonyms - indicates if the synonyms are allowed.
	 * @return the state of the cast.
	 */
	public static boolean canCast(
			LightweightTypeReference fromType, LightweightTypeReference toType,
			boolean enablePrimitiveWidening, boolean enableVoidMatchingNull,
			boolean allowSynonyms) {
		if (enableVoidMatchingNull) {
			final var fromVoid = fromType == null || fromType.isPrimitiveVoid();
			final var toVoid = toType == null || toType.isPrimitiveVoid();
			if (fromVoid) {
				return toVoid;
			}
			if (toVoid) {
				return fromVoid;
			}
			assert fromType != null;
			assert toType != null;
		} else if ((fromType == null || toType == null)
				|| (fromType.isPrimitiveVoid() != toType.isPrimitiveVoid())) {
			return false;
		}
		final var conform = new TypeConformanceComputationArgument(
				false, false, true, enablePrimitiveWidening, false, allowSynonyms);
		assert fromType != null;
		if (((fromType.getType() instanceof JvmDeclaredType || fromType.isPrimitive())
				// if one of the types is an interface and the other is a non final class
				// (or interface) there always can be a subtype
				&& (!isInterface(fromType) || isFinal(toType))
				&& (!isInterface(toType) || isFinal(fromType))
				&& (!toType.isAssignableFrom(fromType, conform))
				&& (isFinal(fromType) || isFinal(toType)
						|| isClass(fromType) && isClass(toType))
				// no upcast
				&& (!fromType.isAssignableFrom(toType, conform)))
				|| (toType.isPrimitive() && !(fromType.isPrimitive() || fromType.isWrapper()))) {
			return false;
		}
		return true;
	}

	private static ResourceSet getResoutceSet(EObject object) {
		if (object == null) {
			return null;
		}
		final var r = object.eResource();
		if (r == null) {
			return null;
		}
		return r.getResourceSet();
	}

	/** Convert a type reference to a lightweight type reference.
	 *
	 * @param typeRef - reference to convert.
	 * @param services - services used for the conversion
	 * @return the lightweight type reference.
	 */
	public static LightweightTypeReference toLightweightTypeReference(
			JvmTypeReference typeRef, CommonTypeComputationServices services) {
		return toLightweightTypeReference(typeRef, services, getResoutceSet(typeRef), false);
	}

	/** Convert a type reference to a lightweight type reference.
	 *
	 * @param typeRef - reference to convert.
	 * @param services - services used for the conversion
	 * @param context the context of the reference.
	 * @return the lightweight type reference.
	 * @since 0.12
	 */
	public static LightweightTypeReference toLightweightTypeReference(
			JvmTypeReference typeRef, CommonTypeComputationServices services, ResourceSet context) {
		return toLightweightTypeReference(typeRef, services, context, false);
	}

	/** Convert a type reference to a lightweight type reference.
	 *
	 * @param typeRef - reference to convert.
	 * @param services - services used for the conversion
	 * @param keepUnboundWildcardInformation - indicates if the unbound wild card
	 *        information must be keeped in the lightweight reference.
	 * @return the lightweight type reference.
	 */
	public static LightweightTypeReference toLightweightTypeReference(
			JvmTypeReference typeRef, CommonTypeComputationServices services,
			boolean keepUnboundWildcardInformation) {
		return toLightweightTypeReference(typeRef, services, getResoutceSet(typeRef), keepUnboundWildcardInformation);
	}

	/** Convert a type reference to a lightweight type reference.
	 *
	 * @param typeRef - reference to convert.
	 * @param services - services used for the conversion
	 * @param context the context of the reference.
	 * @param keepUnboundWildcardInformation - indicates if the unbound wild card
	 *        information must be keeped in the lightweight reference.
	 * @return the lightweight type reference.
	 * @since 0.12
	 */
	public static LightweightTypeReference toLightweightTypeReference(
			JvmTypeReference typeRef, CommonTypeComputationServices services,
			ResourceSet context, boolean keepUnboundWildcardInformation) {
		if (typeRef == null) {
			return null;
		}
		final var owner = new StandardTypeReferenceOwner(services, context);
		final var factory = new LightweightTypeReferenceFactory(owner,
				keepUnboundWildcardInformation);
		final var reference = factory.toLightweightReference(typeRef);
		return reference;
	}

	/** Convert a type reference to a lightweight type reference.
	 *
	 * @param type - type to convert.
	 * @param services - services used for the conversion
	 * @return the lightweight type reference.
	 */
	public static LightweightTypeReference toLightweightTypeReference(
			JvmType type, CommonTypeComputationServices services) {
		return toLightweightTypeReference(type, services, false);
	}

	/** Convert a type to a lightweight type reference.
	 *
	 * @param type - type to convert.
	 * @param services - services used for the conversion
	 * @param keepUnboundWildcardInformation - indicates if the unbound wild card
	 *        information must be keeped in the lightweight reference.
	 * @return the lightweight type reference.
	 */
	public static LightweightTypeReference toLightweightTypeReference(
			JvmType type, CommonTypeComputationServices services,
			boolean keepUnboundWildcardInformation) {
		if (type == null) {
			return null;
		}
		final var owner = new StandardTypeReferenceOwner(services, type);
		final var factory = new LightweightTypeReferenceFactory(owner,
				keepUnboundWildcardInformation);
		final var reference = factory.toLightweightReference(type);
		return reference;
	}

	/** Compare the two strings as they are version numbers.
	 *
	 * @param v1 - first version to compare.
	 * @param v2 - second version to compare.
	 * @return Negative integer of {@code v1} is lower than {@code v2};
	 *     positive integer of {@code v1} is greater than {@code v2};
	 *     {@code 0} if they are strictly equal.
	 * @deprecated see {@link #compareMajorMinorVersions(String, String)}
	 */
	@Deprecated(since = "0.13", forRemoval = true)
	public static int compareVersions(String v1, String v2) {
		// Remove the SNAPSHOT version.
		//final String fixedv1 = v1.replaceFirst("-SNAPSHOT$", ""); //$NON-NLS-1$ //$NON-NLS-2$
		//final String fixedv2 = v2.replaceFirst("-SNAPSHOT$", ""); //$NON-NLS-1$ //$NON-NLS-2$
		//final Version vobject1 = parseVersion(fixedv1);
		//final Version vobject2 = parseVersion(fixedv2);
		final var vobject1 = parseVersion(v1);
		final var vobject2 = parseVersion(v2);
		return vobject1.compareTo(vobject2);
	}

	/** Compare the two strings as they are version numbers with only major and minor components as beeing considered.
	 *
	 * @param v1 - first version to compare.
	 * @param v2 - second version to compare.
	 * @return Negative integer of {@code v1} is lower than {@code v2};
	 *     positive integer of {@code v1} is greater than {@code v2};
	 *     {@code 0} if they are strictly equal.
	 * @since 0.13
	 */
	public static int compareMajorMinorVersions(String v1, String v2) {
		final var vobject1 = parseVersion(v1);
		final var vo1 = new Version(vobject1.getMajor(), vobject1.getMinor(), 0);
		final var vobject2 = parseVersion(v2);
		final var vo2 = new Version(vobject2.getMajor(), vobject2.getMinor(), 0);
		return vo1.compareTo(vo2);
	}

	private static void addAnnotationToSignature(StringBuilder textRepresentation, SARLGrammarKeywordAccess elements,
			ISerializer serializer, ImportManager importManager, XAnnotation annotation) {
		textRepresentation.append(elements.getCommercialAtKeyword());
		textRepresentation.append(getSignatureType(annotation.getAnnotationType(), importManager));
		final var value = annotation.getValue();
		if (value != null) {
			textRepresentation.append(elements.getLeftParenthesisKeyword());
			textRepresentation.append(serializer.serialize(value).trim());
			textRepresentation.append(elements.getRightParenthesisKeyword());
		} else if (!annotation.getElementValuePairs().isEmpty()) {
			textRepresentation.append(elements.getLeftParenthesisKeyword());
			var addComa = false;
			for (final var pair : annotation.getElementValuePairs()) {
				if (addComa) {
					textRepresentation.append(elements.getCommaKeyword());
				} else {
					addComa = true;
				}
				textRepresentation.append(elements.getEqualsSignKeyword());
				textRepresentation.append(serializer.serialize(pair.getValue()).trim());
			}
			textRepresentation.append(elements.getRightParenthesisKeyword());
		}
	}

	/** Replies the original code for the given Ecore object.
	 *
	 * <p>The replied code is the SARL code within the source file. It's format depends on
	 * how the developer has input it.
	 *
	 * <p>This function does:<ul>
	 * <li>replace any sequence of "new line" characters by a space character, and</li>
	 * <li>trim the whitespaces.</li>
	 * </ul>
	 *
	 * @param object the object to search the code for.
	 * @return the SARL code for the given object, or {@code null} if no code was found.
	 * @since 0.7
	 */
	public static String getSarlCodeFor(EObject object) {
		final var node = NodeModelUtils.getNode(object);
		if (node != null) {
			var text = node.getText();
			if (text != null) {
				text = text.trim();
				text = text.replaceAll("[\n\r\f]+", " "); //$NON-NLS-1$//$NON-NLS-2$
			}
			return Strings.emptyToNull(text);
		}
		return null;
	}

	/** This is a context-safe serializer of a signature.
	 *
	 * @param signature - the signature to serialize.
	 * @param serializer - the Xtext serializer
	 * @param grammarAccess - the accessor to the SARL grammar.
	 * @param importManager - used to collect the types to import.
	 *     If {@code null}, the qualified names of the types with be put in the signature.
	 * @return the string representation of the signature.
	 */
	public static String getActionSignatureString(SarlAction signature, ISerializer serializer,
			SARLGrammarKeywordAccess grammarAccess, ImportManager importManager) {
		// Try the serializer
		try {
			return serializer.serialize(signature);
		} catch (Throwable exception) {
			// No working, perhaps the context's of the signature is unknown
		}
		final var textRepresentation = new StringBuilder();
		// Annotations
		for (final var annotation : signature.getAnnotations()) {
			addAnnotationToSignature(textRepresentation, grammarAccess, serializer, importManager, annotation);
		}
		// Modifiers
		for (final var modifier : signature.getModifiers()) {
			textRepresentation.append(modifier);
			textRepresentation.append(' ');
		}
		// Generic type
		if (!signature.getTypeParameters().isEmpty()) {
			var addComa = false;
			textRepresentation.append(grammarAccess.getLessThanSignKeyword());
			for (final var typeParameter : signature.getTypeParameters()) {
				if (addComa) {
					textRepresentation.append(grammarAccess.getCommaKeyword());
					textRepresentation.append(' ');
				} else {
					addComa = true;
				}
				textRepresentation.append(getSignatureType(typeParameter, importManager));
			}
			textRepresentation.append(grammarAccess.getLessThanSignKeyword());
			textRepresentation.append(' ');
		}
		// Name
		textRepresentation.append(signature.getName());
		// Parameters
		if (!signature.getParameters().isEmpty()) {
			textRepresentation.append(grammarAccess.getLeftParenthesisKeyword());
			final var idx = signature.getParameters().size() - 1;
			for (var i = 0; i < idx; ++i) {
				addParamToSignature(textRepresentation, signature.getParameters().get(i), grammarAccess,
						importManager, serializer);
				textRepresentation.append(grammarAccess.getCommaKeyword());
				textRepresentation.append(' ');
			}
			addParamToSignature(textRepresentation, signature.getParameters().get(idx), grammarAccess,
					importManager, serializer);
			textRepresentation.append(grammarAccess.getRightParenthesisKeyword());
		}
		// Return type
		final var returnType = signature.getReturnType();
		if (returnType != null && !"void".equals(returnType.getIdentifier())) { //$NON-NLS-1$
			textRepresentation.append(' ');
			textRepresentation.append(grammarAccess.getColonKeyword());
			textRepresentation.append(' ');
			textRepresentation.append(getSignatureType(returnType.getType(), importManager));
		}
		// Throws
		if (!signature.getExceptions().isEmpty()) {
			textRepresentation.append(' ');
			textRepresentation.append(grammarAccess.getThrowsKeyword());
			textRepresentation.append(' ');
			var addComa = false;
			for (final var eventType : signature.getExceptions()) {
				if (addComa) {
					textRepresentation.append(grammarAccess.getCommaKeyword());
					textRepresentation.append(' ');
				} else {
					addComa = true;
				}
				textRepresentation.append(getSignatureType(eventType.getType(), importManager));
			}
		}
		// Fires
		if (!signature.getFiredEvents().isEmpty()) {
			textRepresentation.append(' ');
			textRepresentation.append(grammarAccess.getFiresKeyword());
			textRepresentation.append(' ');
			var addComa = false;
			for (final var eventType : signature.getFiredEvents()) {
				if (addComa) {
					textRepresentation.append(grammarAccess.getCommaKeyword());
					textRepresentation.append(' ');
				} else {
					addComa = true;
				}
				textRepresentation.append(getSignatureType(eventType.getType(), importManager));
			}
		}
		return textRepresentation.toString();
	}

	private static void addParamToSignature(StringBuilder signature, XtendParameter parameter,
			SARLGrammarKeywordAccess grammarAccess, ImportManager importManager, ISerializer serializer) {
		signature.append(parameter.getName());
		signature.append(' ');
		signature.append(grammarAccess.getColonKeyword());
		signature.append(' ');
		signature.append(getSignatureType(parameter.getParameterType().getType(), importManager));
		if (parameter.isVarArg()) {
			signature.append(grammarAccess.getWildcardAsteriskKeyword());
		} else if (parameter instanceof SarlFormalParameter sarlParameter) {
			if (sarlParameter.getDefaultValue() != null) {
				signature.append(' ');
				signature.append(grammarAccess.getEqualsSignKeyword());
				signature.append(' ');
				signature.append(serializer.serialize(sarlParameter.getDefaultValue()).trim());
			}
		}
	}

	private static String getSignatureType(JvmType type, ImportManager importManager) {
		if (importManager != null) {
			importManager.addImportFor(type);
			return type.getSimpleName();
		}
		return type.getIdentifier();
	}

	/** Compute the serial version UID for the given JVM element.
	 *
	 * @param jvm - the JVM element.
	 * @return the serial version UID.
	 * @see "http://docs.oracle.com/javase/8/docs/platform/serialization/spec/class.html#a4100"
	 */
	public static long computeSerialVersionUID(JvmGenericType jvm) {
		final var serialVersionUIDBuffer = new StringBuilder();

		serialVersionUIDBuffer.append(jvm.getQualifiedName());

		var bitset = new BitSet(32);
		bitset.set(jvm.getVisibility().getValue());
		if (jvm.isFinal()) {
			bitset.set(4);
		}
		if (jvm.isAbstract()) {
			bitset.set(5);
		}
		if (jvm.isInterface()) {
			bitset.set(6);
		}
		serialVersionUIDBuffer.append(bitset.toByteArray());

		final var superTypes = CollectionLiterals.<JvmTypeReference>newTreeSet(new JvmTypeReferenceComparator());
		superTypes.addAll(jvm.getSuperTypes());

		final var fields = CollectionLiterals.<JvmField>newTreeSet(new JvmIdentifiableComparator());
		final var constructors = CollectionLiterals.<JvmConstructor>newTreeSet(new JvmIdentifiableComparator());
		final var operations = CollectionLiterals.<JvmOperation>newTreeSet(new JvmIdentifiableComparator());
		for (final var member : jvm.getMembers()) {
			if (member instanceof JvmField field) {
				if ((field.getVisibility() != JvmVisibility.PRIVATE)
						|| (!field.isStatic() && !field.isTransient())) {
					fields.add(field);
				}
			} else if (member instanceof JvmConstructor constructor) {
				if (constructor.getVisibility() != JvmVisibility.PRIVATE) {
					constructors.add(constructor);
				}
			} else if (member instanceof JvmOperation operation) {
				if (operation.getVisibility() != JvmVisibility.PRIVATE) {
					operations.add(operation);
				}
			}
		}

		for (final var superType : superTypes) {
			serialVersionUIDBuffer.append(superType.getQualifiedName());
		}

		for (final var field : fields) {
			serialVersionUIDBuffer.append(field.getSimpleName());
			bitset = new BitSet(32);
			bitset.set(field.getVisibility().getValue());
			if (field.isStatic()) {
				bitset.set(4);
			}
			if (field.isFinal()) {
				bitset.set(5);
			}
			if (field.isVolatile()) {
				bitset.set(6);
			}
			if (field.isTransient()) {
				bitset.set(7);
			}
			serialVersionUIDBuffer.append(bitset.toByteArray());
			serialVersionUIDBuffer.append(field.getType().getIdentifier());
		}

		for (final var constructor : constructors) {
			bitset = new BitSet(32);
			bitset.set(constructor.getVisibility().getValue());
			if (constructor.isStatic()) {
				bitset.set(4);
			}
			if (constructor.isVarArgs()) {
				bitset.set(5);
			}
			serialVersionUIDBuffer.append(bitset.toByteArray());
			for (final var parameter : constructor.getParameters()) {
				serialVersionUIDBuffer.append(parameter.getParameterType().getIdentifier());
			}
		}

		for (final var operation : operations) {
			bitset = new BitSet(32);
			bitset.set(operation.getVisibility().getValue());
			if (operation.isStatic()) {
				bitset.set(4);
			}
			if (operation.isFinal()) {
				bitset.set(5);
			}
			if (operation.isSynchronized()) {
				bitset.set(6);
			}
			if (operation.isNative()) {
				bitset.set(7);
			}
			if (operation.isAbstract()) {
				bitset.set(8);
			}
			if (operation.isStrictFloatingPoint()) {
				bitset.set(9);
			}
			if (operation.isVarArgs()) {
				bitset.set(10);
			}
			serialVersionUIDBuffer.append(bitset.toByteArray());
			for (final var parameter : operation.getParameters()) {
				serialVersionUIDBuffer.append(parameter.getParameterType().getIdentifier());
			}
		}

		var key = 1L;
		try {
			final var uniqueKey = serialVersionUIDBuffer.toString().getBytes();
			final var sha = MessageDigest.getInstance("SHA").digest(uniqueKey); //$NON-NLS-1$
			key = ((sha[0] >>> 24) & 0xFF)
					| ((sha[0] >>> 16) & 0xFF) << 8
					| ((sha[0] >>> 8) & 0xFF) << 16
					| ((sha[0] >>> 0) & 0xFF) << 24
					| ((sha[1] >>> 24) & 0xFF) << 32
					| ((sha[1] >>> 16) & 0xFF) << 40
					| ((sha[1] >>> 8) & 0xFF) << 48
					| ((sha[1] >>> 0) & 0xFF) << 56;
		} catch (NoSuchAlgorithmException e) {
			//
		}

		return key;
	}

	/** Replies a singleton list with the given element, or the empty list if
	 * the element is {@code null}.
	 *
	 * @param <T> the type of the element in the list.
	 * @param element the element.
	 * @return the list with the element, or the empty list.
	 */
	public static <T> List<T> singletonList(T element) {
		if (element == null) {
			return Collections.emptyList();
		}
		return Collections.singletonList(element);
	}

	/** Replies the qualified name of the given element.
	 *
	 * @param element the element.
	 * @return the qualified name of the element.
	 */
	public static QualifiedName getQualifiedName(JvmIdentifiableElement element) {
		if (element == null) {
			return null;
		}
		return QualifiedName.create(element.getQualifiedName('.').split("\\.")); //$NON-NLS-1$
	}

	/** Replies if the given declaration has an abstract member.
	 *
	 * @param declaration - the declaration.
	 * @return {@code true} if the given type has an abstract function.
	 */
	public static boolean hasAbstractMember(XtendTypeDeclaration declaration) {
		if (declaration != null) {
			for (final var member : declaration.getMembers()) {
				if (member instanceof XtendFunction cvalue) {
					if (cvalue.isAbstract()) {
						return true;
					}
				}
			}
		}
		return false;
	}

	/** Check if a compatible SARL library is available on the classpath.
	 *
	 * @param typeReferences - the accessor to the types.
	 * @param context - the context that is providing the access to the classpath.
	 * @return {@code true} if a compatible SARL library was found.
	 *     Otherwise {@code false}.
	 */
	public static boolean isCompatibleSARLLibraryOnClasspath(TypeReferences typeReferences, Notifier context) {
		final var version = new OutParameter<String>();
		final var code = getSARLLibraryVersionOnClasspath(typeReferences, context, version);
		if (code == SarlLibraryErrorCode.SARL_FOUND) {
			return isCompatibleSARLLibraryVersion(version.get());
		}
		return false;
	}

	/** Check if a version is compatible with the expected SARL library.
	 *
	 * @param version - the version to test.
	 * @return {@code true} if a compatible SARL library was found.
	 *     Otherwise {@code false}.
	 */
	public static boolean isCompatibleSARLLibraryVersion(String version) {
		if (version != null) {
			final var currentVersion = parseVersion(SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING);
			assert currentVersion != null;
			final var paramVersion = parseVersion(version);
			if (paramVersion != null) {
				return currentVersion.getMajor() == paramVersion.getMajor()
						&& currentVersion.getMinor() == paramVersion.getMinor();
			}
		}
		return false;
	}

	/** Parse the given string to extract a version number.
	 * This function is safer than {@link Version#parseVersion(String)} because it automatically
	 * adds the suffix {@code .0} to the provided version if it is not recognized as-is.
	 * 
	 * @param version the string to parse.
	 * @return the version number or {@code null} if there is no valid version number in the string.
	 * @since 0.15
	 */
	public static Version parseVersion(String version) {
		if (version != null) {
			try {
				return Version.parseVersion(version);
			} catch (Throwable ex0) {
				try {
					return Version.parseVersion(new StringBuilder(version).append(".0").toString()); //$NON-NLS-1$
				} catch (Throwable ex1) {
					//
				}
			}
		}
		return null;
	}

	/** Check if a version of the JDK is compatible with the SARL compilation environment.
	 * In other words, check if the SARL compiler is able to be run without issue
	 * with a JDK at the given version.
	 *
	 * @param version - the version to test.
	 * @return {@code true} if this version is for a compatible JDK.
	 *     Otherwise {@code false}.
	 * @since 0.10
	 * @see io.sarl.lang.core.SARLVersion#MINIMAL_JDK_VERSION_FOR_SARL_COMPILATION_ENVIRONMENT
	 * @see io.sarl.lang.core.SARLVersion#INCOMPATIBLE_JDK_VERSION_FOR_SARL_COMPILATION_ENVIRONMENT
	 */
	public static boolean isCompatibleJDKVersionWithSARLCompilationEnvironment(String version) {
		if (version != null) {
			final var current = parseVersion(version);
			if (current != null) {
				final var minJdk = parseVersion(SARLVersion.MINIMAL_JDK_VERSION_FOR_SARL_COMPILATION_ENVIRONMENT);
				assert minJdk != null;
				if (current.compareTo(minJdk) >= 0) {
					final var maxJdk = parseVersion(SARLVersion.INCOMPATIBLE_JDK_VERSION_FOR_SARL_COMPILATION_ENVIRONMENT);
					assert maxJdk != null;
					return current.compareTo(maxJdk) < 0;
				}
			}
		}
		return false;
	}

	/** Check if a version of the JDK is compatible with the SARL compilation environment.
	 * In other words, check if the SARL compiler is able to be run without issue
	 * with the current JDK.
	 *
	 * @return {@code true} if this version is for a compatible JDK.
	 *     Otherwise {@code false}.
	 * @since 0.10
	 * @see io.sarl.lang.core.SARLVersion#MINIMAL_JDK_VERSION_FOR_SARL_COMPILATION_ENVIRONMENT
	 * @see io.sarl.lang.core.SARLVersion#INCOMPATIBLE_JDK_VERSION_FOR_SARL_COMPILATION_ENVIRONMENT
	 */
	public static boolean isCompatibleJDKVersionWithSARLCompilationEnvironment() {
		return isCompatibleJDKVersionWithSARLCompilationEnvironment(System.getProperty("java.specification.version")); //$NON-NLS-1$
	}

	/** Check if a version of the JDK is compatible with the SARL libraries that is specified in the SARL project classpath.
	 * In other words, check if the SARL libraries in compiled SARL project are
	 * compatible with the JDK at the given version.
	 *
	 * @param version - the version to test.
	 * @return {@code true} if this version is for a compatible JDK.
	 *     Otherwise {@code false}.
	 * @since 0.10
	 * @see io.sarl.lang.core.SARLVersion#MINIMAL_JDK_VERSION_IN_SARL_PROJECT_CLASSPATH
	 * @see io.sarl.lang.core.SARLVersion#INCOMPATIBLE_JDK_VERSION_IN_SARL_PROJECT_CLASSPATH
	 */
	public static boolean isCompatibleJDKVersionWhenInSARLProjectClasspath(String version) {
		if (version != null && !version.isEmpty()) {
			final var current = parseVersion(version);
			if (current != null) {
				final var minJdk = parseVersion(SARLVersion.MINIMAL_JDK_VERSION_IN_SARL_PROJECT_CLASSPATH);
				assert minJdk != null;
				if (current.compareTo(minJdk) >= 0) {
					final var maxJdk = parseVersion(SARLVersion.INCOMPATIBLE_JDK_VERSION_IN_SARL_PROJECT_CLASSPATH);
					assert maxJdk != null;
					return current.compareTo(maxJdk) < 0;
				}
			}
		}
		return false;
	}

	/** Check if a version of Xtext is compatible with the SARL library.
	 *
	 * @param version - the version to test.
	 * @return {@code true} if this version is for a compatible Xtext.
	 *     Otherwise {@code false}.
	 */
	public static boolean isCompatibleXtextVersion(String version) {
		return version != null && !version.isEmpty()
				&& compareVersions(version, SARLVersion.MINIMAL_XTEXT_VERSION) >= 0;
	}

	/** Check if a version of the current Xtext is compatible with the SARL library.
	 *
	 * @return {@code true} if this version is for a compatible Xtext.
	 *     Otherwise {@code false}.
	 */
	public static boolean isCompatibleXtextVersion() {
		final var xtextVersion = XtextVersion.getCurrent();
		if (xtextVersion != null && !Strings.isNullOrEmpty(xtextVersion.getVersion())) {
			return isCompatibleXtextVersion(xtextVersion.getVersion());
		}
		return false;
	}

	/** Replies the version of the SARL library on the classpath.
	 *
	 * @param typeReferences - the accessor to the types.
	 * @param context - the context that is providing the access to the classpath.
	 * @return the version, or {@code null} if the SARL library cannot be found or
	 *     is too old.
	 * @deprecated see {@link #getSARLLibraryVersionOnClasspath(TypeReferences, Notifier, OutParameter)}
	 */
	@Deprecated(forRemoval = true, since = "0.10")
	public static String getSARLLibraryVersionOnClasspath(TypeReferences typeReferences, Notifier context) {
		final var version = new OutParameter<String>();
		final var code = getSARLLibraryVersionOnClasspath(typeReferences, context, version);
		if (code == SarlLibraryErrorCode.SARL_FOUND) {
			return version.get();
		}
		return null;
	}

	/** Replies the version of the SARL library on the classpath.
	 *
	 * @param typeReferences - the accessor to the types.
	 * @param context the context that is providing the access to the classpath.
	 * @param version the version of the SARL library that was found, according to the returned error code.
	 * @return the version, or {@code null} if the SARL library cannot be found or
	 *     is too old.
	 */
	public static SarlLibraryErrorCode getSARLLibraryVersionOnClasspath(TypeReferences typeReferences, Notifier context,
			OutParameter<String> version) {
		if (checkSarlVersionClass) {
			checkSarlVersionClass = false;
			try {
				final var v = SARLVersion.class.getDeclaredField(SARL_VERSION_FIELD_NAME_STR);
				if (v == null) {
					return SarlLibraryErrorCode.INVALID_SARL_VERSION_BYTECODE;
				}
			} catch (Throwable e) {
				return SarlLibraryErrorCode.INVALID_SARL_VERSION_BYTECODE;
			}
		}
		final JvmType type;
		try {
			type = typeReferences.findDeclaredType(SARLVersion.class, context);
		} catch (Throwable exception) {
			return SarlLibraryErrorCode.NO_SARL_VERSION_CLASS;
		}
		if (type == null) {
			return SarlLibraryErrorCode.NO_SARL_VERSION_CLASS;
		}
		if (type instanceof JvmDeclaredType sarlVersionType) {
			JvmField versionField = null;
			final var iterator = sarlVersionType.getDeclaredFields().iterator();
			while (versionField == null && iterator.hasNext()) {
				final var field = iterator.next();
				if (SARL_VERSION_FIELD_NAME_STR.equals(field.getSimpleName())) {
					versionField = field;
				}
			}
			if (versionField == null) {
				return SarlLibraryErrorCode.NO_SARL_VERSION_FIELD;
			}
			final var value = versionField.getConstantValueAsString();
			if (Strings.isNullOrEmpty(value)) {
				return SarlLibraryErrorCode.NO_SARL_VERSION_VALUE;
			}
			if (version != null) {
				version.set(value);
			}
			return SarlLibraryErrorCode.SARL_FOUND;
		}
		return SarlLibraryErrorCode.NO_SARL_VERSION_DECLARED_TYPE;
	}

	/** Replies if the given annotation is an annotation from the SARL core library.
	 *
	 * @param type the type of the annotation
	 * @return {@code true} if the given type is a SARL annotation.
	 */
	public static boolean isSARLAnnotation(Class<?> type) {
		return (type != null && Annotation.class.isAssignableFrom(type))
				&& isSARLAnnotation(type.getPackage().getName());
	}

	/** Replies if the given annotation is an annotation from the SARL core library.
	 *
	 * @param qualifiedName the qualified name of the annotation type.
	 * @return {@code true} if the given type is a SARL annotation.
	 */
	public static boolean isSARLAnnotation(String qualifiedName) {
		return qualifiedName != null && qualifiedName.startsWith(SARL_PACKAGE_PREFIX);
	}

	/** Replies if the given type is a functional interface.
	 *
	 * <p>This function does not test if the {@code @FunctionalInterface} is attached to the type.
	 * The function counts the number of operations.
	 *
	 * @param type the type to test.
	 * @param sarlSignatureProvider the provider of SARL operation signatures.
	 * @return {@code true} if the given type is final.
	 */
	public static boolean isFunctionalInterface(JvmGenericType type, IActionPrototypeProvider sarlSignatureProvider) {
		if (type != null && type.isInterface()) {
			final var operations = new HashMap<ActionPrototype, JvmOperation>();
			populateInterfaceElements(type, operations, null, sarlSignatureProvider);
			if (operations.size() == 1) {
				final var op = operations.values().iterator().next();
				return !op.isStatic() && !op.isDefault();
			}
		}
		return false;
	}

	/**
	 * Returns the closest {@link EObject#eContainer() container object} that is not of the requested type.
	 *
	 * @param element the element to start from.
	 * @param type the unexpected type.
	 * @param container the container.
	 * @param directContainerChild the child of the container that is or contains the given element.
	 * @return {@code true} if the container was found.
	 * @since 0.5
	 * @see org.eclipse.xtext.EcoreUtil2#getContainerOfType(EObject, Class)
	 */
	public static boolean getContainerNotOfType(EObject element, Class<? extends EObject> type,
			OutParameter<EObject> container, OutParameter<EObject> directContainerChild) {
		var previous = element;
		var elt = element.eContainer();
		while (elt != null) {
			if (!type.isInstance(elt)) {
				if (directContainerChild != null) {
					directContainerChild.set(previous);
				}
				if (container != null) {
					container.set(elt);
				}
				return true;
			}
			previous = elt;
			elt = elt.eContainer();
		}
		return false;
	}

	/**
	 * Returns the closest {@link EObject#eContainer() container object} that is of one of the requested type.
	 *
	 * @param element the element to start from.
	 * @param container the container.
	 * @param directContainerChild the child of the container that is or contains the given element.
	 * @param types the unexpected types.
	 * @return {@code true} if the container was found.
	 * @since 0.8
	 * @see org.eclipse.xtext.EcoreUtil2#getContainerOfType(EObject, Class)
	 */
	@SafeVarargs
	public static boolean getContainerOfType(EObject element,
			OutParameter<EObject> container, OutParameter<EObject> directContainerChild,
			Class<? extends EObject>... types) {
		var previous = element;
		var elt = element.eContainer();
		while (elt != null) {
			if (isInstance(types, elt)) {
				if (directContainerChild != null) {
					directContainerChild.set(previous);
				}
				if (container != null) {
					container.set(elt);
				}
				return true;
			}
			previous = elt;
			elt = elt.eContainer();
		}
		return false;
	}

	private static boolean isInstance(Class<? extends EObject>[] types, Object element) {
		for (final var type : types) {
			if (type.isInstance(element)) {
				return true;
			}
		}
		return false;
	}

	/**
	 * Returns the closest {@link EObject#eContainer() container object} that is validating the predicate.
	 *
	 * @param element the element to start from.
	 * @param predicate the predicate to test.
	 * @return the container or {@code null}.
	 * @since 0.5
	 * @see org.eclipse.xtext.EcoreUtil2#getContainerOfType(EObject, Class)
	 */
	public static EObject getFirstContainerForPredicate(EObject element, Function1<? super EObject, ? extends Boolean> predicate) {
		if (predicate == null || element == null) {
			return null;
		}
		var elt = element.eContainer();
		while (elt != null) {
			if (predicate.apply(elt).booleanValue()) {
				return elt;
			}
			elt = elt.eContainer();
		}
		return null;
	}

	/** Error code for the
	 * {@link Utils#getSARLLibraryVersionOnClasspath(TypeReferences, Notifier, OutParameter)}
	 * function.
	 *
	 * @author $Author: sgalland$
	 * @version $FullVersion$
	 * @mavengroupid $GroupId$
	 * @mavenartifactid $ArtifactId$
	 * @since 0.5
	 */
	public enum SarlLibraryErrorCode {
		/** SARL Library was found.
		 */
		SARL_FOUND,
		/** SARL version class not found.
		 */
		NO_SARL_VERSION_CLASS,
		/** SARL version class is not a Xtext declared type.
		 */
		NO_SARL_VERSION_DECLARED_TYPE,
		/** SARL version field not found.
		 */
		NO_SARL_VERSION_FIELD,
		/** SARL version value not found.
		 */
		NO_SARL_VERSION_VALUE,
		/** The byte code (the class) of {@link io.sarl.lang.core.SARLVersion} does not contains the expected field.
		 */
		INVALID_SARL_VERSION_BYTECODE,
	}

	/** Dump the object.
	 *
	 * @param object the object.
	 * @return the string representation of the object.
	 * @since 0.7
	 */
	public static String dump(EObject object) {
		return EmfFormatter.objToStr(object);
	}

	/** Dump the object.
	 *
	 * @param object the object.
	 * @param includeStaticField indicates if the static fields should be included.
	 * @return the string representation of the object.
	 * @since 0.6
	 */
	public static String dump(Object object, boolean includeStaticField) {
		if (object == null) {
			return new String();
		}
		final var buffer = new StringBuilder();
		final var types = new LinkedList<Class<?>>();
		types.add(object.getClass());
		while (!types.isEmpty()) {
			final var type = types.removeFirst();

			final var supertype = type.getSuperclass();
			if (supertype != null && !supertype.equals(Object.class)) {
				types.add(supertype);
			}

			if (buffer.length() > 0) {
				buffer.append("\n"); //$NON-NLS-1$
			}
			final var fields = type.getDeclaredFields();
			buffer.append(type.getSimpleName()).append(" {\n"); //$NON-NLS-1$

			var firstRound = true;

			for (final var field : fields) {
				if (!includeStaticField && Modifier.isStatic(field.getModifiers())) {
					continue;
				}
				try {
					field.setAccessible(true);
					if (firstRound) {
						firstRound = false;
					} else {
						buffer.append(",\n"); //$NON-NLS-1$
					}
					try {
						final var fieldObj = field.get(object);
						final String value;
						if (null == fieldObj) {
							value = "null"; //$NON-NLS-1$
						} else {
							value = fieldObj.toString();
						}
						buffer.append('\t').append(field.getName()).append('=').append('"');
						buffer.append(org.eclipse.xtext.util.Strings.convertToJavaString(value));
						buffer.append("\""); //$NON-NLS-1$
					} catch (IllegalAccessException ignore) {
						//this should never happen
					}
				} catch (Throwable ex) {
					// Ignore this because the field cannot be accessed due to reflection restriction
				}

			}

			buffer.append("\n}"); //$NON-NLS-1$
		}
		return buffer.toString();
	}

	/** Clone the given type reference that for being link to the given operation.
	 *
	 * <p>The proxies are not resolved, and the type parameters are clone when they are
	 * related to the type parameter of the type container.
	 *
	 * @param type the source type.
	 * @param executableTypeParameters the type parameters of the executable component that will contain the result type.
	 * @param superTypeParameterMapping the mapping from the type parameters inherited from the super types.
	 * @param typeParameterBuilder the builder if type parameter.
	 * @param typeBuilder the builder of type.
	 * @param typeReferences the builder of type references.
	 * @param jvmTypesFactory the factory of Jvm types.
	 * @return the result type, i.e. a copy of the source type.
	 * @since 0.6
	 */
	public static JvmTypeReference cloneWithTypeParametersAndProxies(
			JvmTypeReference type,
			Iterable<JvmTypeParameter> executableTypeParameters,
			Map<String, JvmTypeReference> superTypeParameterMapping,
			JvmTypeReferenceBuilder typeParameterBuilder, JvmTypesBuilder typeBuilder,
			TypeReferences typeReferences, TypesFactory jvmTypesFactory) {
		if (type == null) {
			return typeParameterBuilder.typeRef(Object.class);
		}

		var cloneType = true;
		var typeCandidate = type;

		// Use also cloneType as a flag that indicates if the type was already found in type parameters.
		if ((executableTypeParameters.iterator().hasNext() || !superTypeParameterMapping.isEmpty()) && cloneType) {
			final var typeParameterIdentifiers = new TreeMap<String, JvmTypeParameter>();
			for (final var typeParameter : executableTypeParameters) {
				typeParameterIdentifiers.put(typeParameter.getIdentifier(), typeParameter);
			}

			if (type instanceof JvmParameterizedTypeReference) {
				// Try to clone the type parameters.
				cloneType = false;
				typeCandidate = cloneAndAssociate(type, typeParameterIdentifiers, superTypeParameterMapping,
						typeParameterBuilder, typeReferences, jvmTypesFactory);
			} else if (type instanceof XFunctionTypeRef functionRef) {
				// Try to clone the function reference.
				cloneType = false;
				final var cloneReference = XtypeFactory.eINSTANCE.createXFunctionTypeRef();
				for (final var paramType : functionRef.getParamTypes()) {
					cloneReference.getParamTypes().add(cloneAndAssociate(
							paramType, typeParameterIdentifiers, superTypeParameterMapping,
							typeParameterBuilder, typeReferences, jvmTypesFactory));
				}
				cloneReference.setReturnType(cloneAndAssociate(
						functionRef.getReturnType(), typeParameterIdentifiers, superTypeParameterMapping,
						typeParameterBuilder, typeReferences, jvmTypesFactory));
				cloneReference.setInstanceContext(functionRef.isInstanceContext());
				typeCandidate = cloneReference;
			}
		}

		// Do the clone according to the type of the entity.
		assert typeCandidate != null;
		final JvmTypeReference returnType;
		if (!cloneType) {
			returnType = typeCandidate;
		} else {
			returnType = typeBuilder.cloneWithProxies(typeCandidate);
		}
		return returnType;
	}

	private static JvmTypeReference cloneAndAssociate(
			final JvmTypeReference type,
			final Map<String, JvmTypeParameter> typeParameterIdentifiers,
			Map<String, JvmTypeReference> superTypeParameterMapping,
			JvmTypeReferenceBuilder typeParameterBuilder,
			TypeReferences typeReferences,
			TypesFactory jvmTypesFactory) {
		final var copier = new EcoreUtil.Copier(false) {
			private static final long serialVersionUID = 698510355384773254L;

			@Override
			public EObject copy(EObject eobject) {
				final String id;
				// Try to override the type parameters
				if (eobject instanceof JvmTypeReference cvalue) {
					id = cvalue.getIdentifier();
				} else if (eobject instanceof JvmIdentifiableElement cvalue) {
					id = cvalue.getIdentifier();
				} else {
					id = null;
				}
				if (id != null) {
					final var param = typeParameterIdentifiers.get(id);
					if (param != null) {
						return typeReferences.createTypeRef(param);
					}
					final var superTypeReference = superTypeParameterMapping.get(id);
					if (superTypeReference != null) {
						return typeReferences.createDelegateTypeReference(superTypeReference);
					}
				}
				final var result = super.copy(eobject);
				if (result instanceof JvmWildcardTypeReference wildcardType) {
					var upperBoundSeen = false;
					for (final var constraint : wildcardType.getConstraints()) {
						if (constraint instanceof JvmUpperBound) {
							upperBoundSeen = true;
							break;
						}
					}
					if (!upperBoundSeen) {
						// no upper bound found - seems to be an invalid - assume object as upper bound
						final var object = typeParameterBuilder.typeRef(Object.class);
						final var upperBound = jvmTypesFactory.createJvmUpperBound();
						upperBound.setTypeReference(object);
						wildcardType.getConstraints().add(0, upperBound);
					}
				}
				return result;
			}
		};
		final var copy = (JvmTypeReference) copier.copy(type);
		copier.copyReferences();
		return copy;
	}

	/** Extract the mapping between the type parameters declared within the super types and the
	 * type parameters arguments that are declared within the given type.
	 *
	 * <p>For example, consider the following code:
	 * <pre>{@code 
	 * interface X&lt;T&gt; {
	 *   def a(p1 : T, p2 : U) with U
	 * }
	 * interface Y&lt;T&gt; {
	 * }
	 * class Z&lt;TT&gt; implements X&lt;TT&gt;, Y&lt;TT&gt; {
	 *   def a(p1 : TT, p2 : W) with W { }
	 * }
	 * }</pre>
	 * The mapping is:
	 * <pre>{@code 
	 * X.T =&gt; TT
	 * Y.T =&gt; TT
	 * }</pre>
	 *
	 * @param type the type to analyze.
	 * @param mapping the map to fill with the mapping.
	 * @since 0.7
	 */
	public static void getSuperTypeParameterMap(JvmDeclaredType type, Map<String, JvmTypeReference> mapping) {
		for (final var superTypeReference : type.getSuperTypes()) {
			if (superTypeReference instanceof JvmParameterizedTypeReference parameterizedTypeReference) {
				final var st = superTypeReference.getType();
				if (st instanceof JvmTypeParameterDeclarator superType) {
					int i = 0;
					for (final var typeParameter : superType.getTypeParameters()) {
						if (i < parameterizedTypeReference.getArguments().size()) {
							mapping.put(typeParameter.getIdentifier(), parameterizedTypeReference.getArguments().get(i));
						} else {
							mapping.put(typeParameter.getIdentifier(), null);
						}
						++i;
					}
				}
			}
		}
	}

	/** Copy the type parameters from a JvmOperation.
	 *
	 * <p>This function differs from {@link org.eclipse.xtend.core.jvmmodel.XtendJvmModelInferrer XtendJvmModelInferrer.copyAndFixTypeParameters(List, JvmTypeParameterDeclarator)}
	 * and {@link org.eclipse.xtend.core.jvmmodel.XtendJvmModelInferrer XtendJvmModelInferrer.copyTypeParameters(List, JvmTypeParameterDeclarator)}
	 * in the fact that the type parameters were already generated and fixed. The current function supper generic types by
	 * clone the types references with {@link #cloneWithTypeParametersAndProxies(JvmTypeReference, Iterable, Map, JvmTypeReferenceBuilder, JvmTypesBuilder, TypeReferences, TypesFactory)}.
	 *
	 * @param fromOperation the operation from which the type parameters are copied.
	 * @param toOperation the operation that will receives the new type parameters.
	 * @param typeParameterBuilder the builder if type parameter.
	 * @param typeBuilder the builder of type.
	 * @param typeReferences the builder of type references.
	 * @param jvmTypesFactory the factory of Jvm types.
	 * @since 0.6
	 */
	public static void copyTypeParametersFromJvmOperation(JvmOperation fromOperation, JvmOperation toOperation,
			JvmTypeReferenceBuilder typeParameterBuilder, JvmTypesBuilder typeBuilder,
			TypeReferences typeReferences, TypesFactory jvmTypesFactory) {
		// Get the type parameter mapping that is a consequence of the super type extension within the container.
		final var superTypeParameterMapping = new HashMap<String, JvmTypeReference>();
		Utils.getSuperTypeParameterMap(toOperation.getDeclaringType(), superTypeParameterMapping);
		copyTypeParametersFromJvmOperation(
				fromOperation.getTypeParameters(),
				toOperation.getTypeParameters(),
				superTypeParameterMapping,
				typeParameterBuilder, typeBuilder, typeReferences, jvmTypesFactory);
	}

	/** Copy the type parameters from a JvmOperation.
	 *
	 * <p>This function differs from {@link org.eclipse.xtend.core.jvmmodel.XtendJvmModelInferrer XtendJvmModelInferrer.copyAndFixTypeParameters(List, JvmTypeParameterDeclarator)}
	 * and {@link org.eclipse.xtend.core.jvmmodel.XtendJvmModelInferrer XtendJvmModelInferrer.copyTypeParameters(List, JvmTypeParameterDeclarator)}
	 * in the fact that the type parameters were already generated and fixed. The current function supper generic types by
	 * clone the types references with {@link #cloneWithTypeParametersAndProxies(JvmTypeReference, Iterable, Map, JvmTypeReferenceBuilder, JvmTypesBuilder, TypeReferences, TypesFactory)}.
	 *
	 * @param inputParameters the type parameters in the source operation.
	 * @param outputParameters the list of type parameters to be filled out.
	 * @param superTypeParameterMapping the mapping from the type parameters inherited from the super types.
	 * @param typeParameterBuilder the builder if type parameter.
	 * @param typeBuilder the builder of type.
	 * @param typeReferences the builder of type references.
	 * @param jvmTypesFactory the factory of Jvm types.
	 * @since 0.6
	 */
	public static void copyTypeParametersFromJvmOperation(List<JvmTypeParameter> inputParameters,
			List<JvmTypeParameter> outputParameters,
			Map<String, JvmTypeReference> superTypeParameterMapping,
			JvmTypeReferenceBuilder typeParameterBuilder, JvmTypesBuilder typeBuilder,
			TypeReferences typeReferences, TypesFactory jvmTypesFactory) {
		// Copy the generic types in two steps: first step is the name's copy.
		for (final var typeParameter : inputParameters) {
			final var typeParameterCopy = jvmTypesFactory.createJvmTypeParameter();
			typeParameterCopy.setName(typeParameter.getName());
			outputParameters.add(typeParameterCopy);
		}
		// Second step is the constraints' copy
		for (var i = 0; i < inputParameters.size(); ++i) {
			final var typeParameter = inputParameters.get(i);
			final var typeParameterCopy = outputParameters.get(i);
			for (final var constraint : typeParameter.getConstraints()) {
				JvmTypeConstraint cst = null;
				if (constraint instanceof JvmLowerBound) {
					cst = jvmTypesFactory.createJvmLowerBound();
				} else if (constraint instanceof JvmUpperBound) {
					cst = jvmTypesFactory.createJvmUpperBound();
				}
				if (cst != null) {
					typeParameterCopy.getConstraints().add(cst);
					cst.setTypeReference(cloneWithTypeParametersAndProxies(
							constraint.getTypeReference(),
							outputParameters,
							superTypeParameterMapping,
							typeParameterBuilder, typeBuilder, typeReferences, jvmTypesFactory));
				}
			}
		}
	}

	/** Set the given structure feature with the given value.
	 *
	 * @param object the object that contains the feature.
	 * @param property the feature to change.
	 * @param value the value of the feature.
	 */
	public static void setStructuralFeature(EObject object, EStructuralFeature property, Object value) {
		assert object != null;
		assert property != null;
		if (value == null) {
			object.eUnset(property);
		} else {
			object.eSet(property, value);
		}
	}

	/** Replies the root feature call into a sequence of feature calls.
	 *
	 * @param featureCall the leaf feature call.
	 * @return the root feature call.
	 * @since 0.8.6
	 * @see #getRootFeatureCall(XAbstractFeatureCall, XExpression, List)
	 */
	public static XAbstractFeatureCall getRootFeatureCall(XAbstractFeatureCall featureCall) {
		final var container = featureCall.eContainer();
		final XAbstractFeatureCall rootFeatureCall;
		if (container instanceof XMemberFeatureCall || container instanceof XFeatureCall) {
			rootFeatureCall = (XAbstractFeatureCall) getFirstContainerForPredicate(featureCall,
					it -> Boolean.valueOf(it.eContainer() != null && !(it.eContainer() instanceof XMemberFeatureCall || it.eContainer() instanceof XFeatureCall)));
		} else {
			rootFeatureCall = featureCall;
		}
		return rootFeatureCall;
	}

	/** Replies the root feature call into a sequence of feature calls that has
	 * not reference to elements declared within the container, and that are
	 * not one of the container's parameters.
	 *
	 * @param featureCall the leaf feature call.
	 * @param container the container of the feature call.
	 * @param containerParameters the parameters of the container.
	 * @return the root feature call, or {@code null} if there is no root feature call.
	 * @since 0.8.6
	 * @see #getRootFeatureCall(XAbstractFeatureCall)
	 */
	public static XAbstractFeatureCall getRootFeatureCall(XAbstractFeatureCall featureCall,
			XExpression container, List<JvmFormalParameter> containerParameters) {
		if (hasLocalParameters(featureCall, container, containerParameters)
				|| !(featureCall instanceof XMemberFeatureCall || featureCall instanceof XFeatureCall)) {
			return null;
		}
		var current = featureCall;
		var currentContainer = current.eContainer();
		while (currentContainer != null) {
			if (currentContainer instanceof XMemberFeatureCall || currentContainer instanceof XFeatureCall) {
				final var c = (XAbstractFeatureCall) currentContainer;
				if (hasLocalParameters(c, container, containerParameters)) {
					return current;
				}
				current = c;
				currentContainer = current.eContainer();
			} else {
				return current;
			}
		}
		return current;
	}

	private static boolean hasLocalParameters(EObject current, XExpression container, List<JvmFormalParameter> containerParameters) {
		if (current instanceof XAbstractFeatureCall featureCall) {
			if (isLocalEntity(featureCall, container, containerParameters)) {
				return true;
			}
			for (final var argument : featureCall.getActualArguments()) {
				final Iterable<XAbstractFeatureCall> iterable;
				if (argument instanceof XAbstractFeatureCall cvalue) {
					iterable = Iterables.concat(
							Collections.singletonList(cvalue),
							EcoreUtil2.getAllContentsOfType(argument, XAbstractFeatureCall.class));
				} else {
					iterable = EcoreUtil2.getAllContentsOfType(argument, XAbstractFeatureCall.class);
				}
				for (final var c : iterable) {
					if (isLocalEntity(c, container, containerParameters)) {
						return true;
					}
				}
			}
		}
		return false;
	}

	private static boolean isLocalEntity(XAbstractFeatureCall featureCall, XExpression container, List<JvmFormalParameter> containerParameters) {
		final var feature = featureCall.getFeature();
		if (feature instanceof JvmFormalParameter) {
			if (containerParameters.contains(feature)) {
				return true;
			}
		} else if (feature instanceof XVariableDeclaration) {
			if (EcoreUtil.isAncestor(container, feature)) {
				return true;
			}
		}
		return false;
	}

	/** Replies if a generic type parameter is declared into the given type.
	 *
	 * @param type is the type to explore.
	 * @return {@code code} if the {@code type} contains a generic type.
	 */
	public static boolean containsGenericType(LightweightTypeReference type) {
		if (type == null) {
			return false;
		}
		final var jtype = type.getType();
		if (jtype instanceof JvmTypeParameter) {
			return true;
		}
		for (final var atype : type.getTypeArguments()) {
			if (containsGenericType(atype)) {
				return true;
			}
		}
		switch (type.getKind()) {
		case LightweightTypeReference.KIND_WILDCARD_TYPE_REFERENCE:
			if (containsGenericType(type.getLowerBoundSubstitute()) || containsGenericType(type.getUpperBoundSubstitute())) {
				return true;
			}
			break;
		case LightweightTypeReference.KIND_ARRAY_TYPE_REFERENCE:
			if (containsGenericType(type.getComponentType())) {
				return true;
			}
			break;
		default:
			break;
		}
		return false;
	}

	/** Convert the given string of characters in order to be readable from a user's message.
	 *
	 * @param text the text to convert.
	 * @return the readable text.
	 * @since 0.12
	 */
	@Pure
	public static Object toReadableString(String text) {
		final var str = Strings.emptyToNull(text);
		if (str == null) {
			return Messages.Utils_0;
		}
		return text;
	}

	/** Replies the human-readable types arguments without showing the upper and lower bounds.
	 *
	 * @param arguments the list of type arguments.
	 * @param numberOfArgumentsIfNotProvider indicates the number of arguments that are expected if the given type does not provide them.
	 * @param grammarAccess the accessor to the grammar.
	 * @return the string representation of the type arguments.
	 * @since 0.15
	 */
	@Pure
	public static String getHumanReadableTypeArgumentsWithoutBounds(List<LightweightTypeReference> arguments, int numberOfArgumentsIfNotProvider, SARLGrammarKeywordAccess grammarAccess) {
		final var buffer = new StringBuilder();
		if (arguments != null && !arguments.isEmpty()) {
			buffer.append(grammarAccess.getLessThanSignKeyword());
			var first = true;
			for (final var argument : arguments) {
				if (first) {
					first = false;
				} else {
					buffer.append(grammarAccess.getCommaKeyword()).append(" "); //$NON-NLS-1$
				}
				buffer.append(argument.getHumanReadableName());
			}
			buffer.append(grammarAccess.getGreaterThanSignKeyword());
		} else {
			buffer.append(grammarAccess.getLessThanSignKeyword());
			for (var i = 0; i < numberOfArgumentsIfNotProvider; ++i) {
				if (i > 0) {
					buffer.append(grammarAccess.getCommaKeyword());
				}
				buffer.append(grammarAccess.getQuestionMarkKeyword());
			}
			buffer.append(grammarAccess.getGreaterThanSignKeyword());
		}
		return buffer.toString();
	}

	/** Replies the human-readable types arguments without showing the upper and lower bounds.
	 *
	 * @param type the type for which the type arguments must be extracted.
	 * @param numberOfArgumentsIfNotProvider indicates the number of arguments that are expected if the given type does not provide them.
	 * @param grammarAccess the accessor to the grammar.
	 * @return the string representation of the type arguments.
	 * @since 0.15
	 */
	@Pure
	public static String getHumanReadableTypeArgumentsWithoutBounds(LightweightTypeReference type, int numberOfArgumentsIfNotProvider, SARLGrammarKeywordAccess grammarAccess) {
		return getHumanReadableTypeArgumentsWithoutBounds(type.getTypeArguments(), numberOfArgumentsIfNotProvider, grammarAccess);
	}

	/** Replies the human-readable types arguments with showing the upper and lower bounds.
	 *
	 * @param arguments the list of type arguments.
	 * @param grammarAccess the accessor to the grammar.
	 * @return the string representation of the type arguments.
	 * @since 0.14
	 */
	@Pure
	public static String getHumanReadableTypeParametersWithBounds(List<JvmTypeParameter> arguments, SARLGrammarKeywordAccess grammarAccess) {
		final var buffer = new StringBuilder();
		if (arguments != null && !arguments.isEmpty()) {
			buffer.append(grammarAccess.getLessThanSignKeyword());
			var first = true;
			for (final var argument : arguments) {
				if (first) {
					first = false;
				} else {
					buffer.append(grammarAccess.getCommaKeyword()).append(" "); //$NON-NLS-1$
				}
				buffer.append(argument.getSimpleName());
				for (final var constraint : argument.getConstraints()) {
					if (constraint instanceof JvmUpperBound upper) {
						buffer.append(" ").append(grammarAccess.getExtendsKeyword()); //$NON-NLS-1$
						buffer.append(" ").append(upper.getTypeReference().getSimpleName()); //$NON-NLS-1$
					} else if (constraint instanceof JvmLowerBound lower) {
						buffer.append(" ").append(grammarAccess.getSuperKeyword()); //$NON-NLS-1$
						buffer.append(" ").append(lower.getTypeReference().getSimpleName()); //$NON-NLS-1$
					}
				}
			}
			buffer.append(grammarAccess.getGreaterThanSignKeyword());
		}
		return buffer.toString();
	}

	/** Replies the human-readable types arguments with showing the upper and lower bounds.
	 *
	 * @param arguments the list of type arguments.
	 * @param grammarAccess the accessor to the grammar.
	 * @return the string representation of the type arguments.
	 * @since 0.14
	 */
	@Pure
	public static String getHumanReadableTypeArgumentsWithBounds(List<LightweightTypeReference> arguments, SARLGrammarKeywordAccess grammarAccess) {
		final var buffer = new StringBuilder();
		if (arguments != null && arguments.isEmpty()) {
			buffer.append(grammarAccess.getLessThanSignKeyword());
			var first = true;
			for (final var argument : arguments) {
				if (first) {
					first = false;
				} else {
					buffer.append(grammarAccess.getCommaKeyword()).append(" "); //$NON-NLS-1$
				}
				buffer.append(argument.getHumanReadableName());
				buffer.append(" ").append(grammarAccess.getExtendsKeyword()).append(" "); //$NON-NLS-1$ //$NON-NLS-2$
				buffer.append(argument.getConstraintSubstitute().getHumanReadableName());
			}
			buffer.append(grammarAccess.getGreaterThanSignKeyword());
		}
		return buffer.toString();
	}

	/** Replies the human-readable types arguments with showing the upper and lower bounds.
	 *
	 * @param type the type for which the type arguments must be extracted.
	 * @param grammarAccess the accessor to the grammar.
	 * @return the string representation of the type arguments.
	 * @since 0.14
	 */
	@Pure
	public static String getHumanReadableTypeArgumentsWithBounds(LightweightTypeReference type, SARLGrammarKeywordAccess grammarAccess) {
		return getHumanReadableTypeArgumentsWithBounds(type.getTypeArguments(), grammarAccess);
	}

	/** Replies the upper bounds from the constraints of the given generic type parameter.
	 * This function may reply {@code null} if there is not specified bound.
	 *
	 * @param parameter the parameter to analyze.
	 * @return the upper bound, or {@code null} if no bound is specified.
	 * @since 0.14
	 */
	public static JvmTypeReference getUpperBoundFromConstraints(JvmConstraintOwner parameter) {
		final var cst = parameter.getConstraints();
		if (!cst.isEmpty()) {
			return cst.get(0).getTypeReference();
		}
		return null;
	}

	/** Run the specified lambda to each of the type parameters.
	 *
	 * @param type the type to analyze.
	 * @param consumer the consumer of the type parameter names.
	 * @return the status of the parsing, never {@code null}.
	 * @since 0.14
	 */
	public static TypeParameterStatus forEachTypeParameterName(JvmParameterizedTypeReference type, BiConsumer<JvmTypeReference, Integer> consumer) {
		var status = TypeParameterStatus.NO_TYPE_PARAMETER;
		if (type != null && type.getType() instanceof JvmTypeParameterDeclarator gtype) {
			final var parameters = gtype.getTypeParameters();
			final var arguments = type.getArguments();
			var i = 0;
			for (final var parameter : parameters) {
				final var argument = i < arguments.size() ? arguments.get(i) : null;
				if (argument == null) {
					try {
						consumer.accept(getUpperBoundFromConstraints(parameter), Integer.valueOf(i));
					} catch (Throwable ex) {
						//
					}
					status = status.or(TypeParameterStatus.DEFINITION_ROOT_BOUND);
				} else if (argument instanceof JvmWildcardTypeReference wargument) {
					JvmTypeReference name = getUpperBoundFromConstraints(wargument);
					if (name == null) {
						try {
							name = getUpperBoundFromConstraints(parameter);
						} catch (Throwable ex) {
							//
						}
						status = status.or(TypeParameterStatus.DEFINITION_ROOT_BOUND);
					} else {
						status = status.or(TypeParameterStatus.EXPLICIT_BOUNDED_WILDCARD);
					}
					consumer.accept(name, Integer.valueOf(i));
				} else {
					consumer.accept(argument, Integer.valueOf(i));
					status = status.or(TypeParameterStatus.EXPLICIT_DIRECT_TYPE);
				}
				++i;
			}
		}
		return status;
	}

	/** Build and reply an identifier for the behavior unit dedicated to the given event.
	 *
	 * @param eventType the type of event to consider.
	 * @return the identifier.
	 * @since 0.14
	 */
	public static String createBehaviorUnitEventId(JvmParameterizedTypeReference eventType) {
		final var id = eventType.getType().getIdentifier();
		return buildNameForHiddenEventMethod(eventType, id, false, true).toString();
	}

	/** Replies an array that contains the types of the type parameter bound for the given event.
	 *
	 * @param eventType the type of event to consider.
	 * @param typeReferences the tool for building the type references.
	 * @return the bounds' types, or {@code null} if there is no generic type.
	 * @since 0.14
	 */
	public static List<JvmTypeReference> getTypeParameterBoundsFor(JvmParameterizedTypeReference eventType, TypeReferences typeReferences) {
		final var bounds = new ArrayList<JvmTypeReference>();
		final var result = forEachTypeParameterName(eventType, (name, i) -> {
			final JvmTypeReference ref;
			if (name == null) {
				ref = typeReferences.createTypeRef(typeReferences.findDeclaredType(Object.class, eventType));
			} else {
				ref = name;
			}
			bounds.add(ref);
		});
		if (result != TypeParameterStatus.NO_TYPE_PARAMETER) {
			return bounds;
		}
		return null;
	}

	/**
	 * Status of parsing the type parameters.
	 *
	 * @author $Author: sgalland$
	 * @version $FullVersion$
	 * @mavengroupid $GroupId$
	 * @mavenartifactid $ArtifactId$
	 * @since 0.14
	 */
	public enum TypeParameterStatus {

		/** No type parameter in the given type.
		 */
		NO_TYPE_PARAMETER {

			@Override
			public boolean isBoundInFunctionName() {
				return false;
			}
		},

		/** A type parameter is not explicitly specified and the root bound type from
		 * the original type declaration is kept.
		 */
		DEFINITION_ROOT_BOUND {

			@Override
			public boolean isBoundInFunctionName() {
				return true;
			}
		},

		/** A type parameter is explicitly provided as a wild card with bound.
		 */
		EXPLICIT_BOUNDED_WILDCARD {

			@Override
			public boolean isBoundInFunctionName() {
				return true;
			}
		},

		/** A type parameter is specified with a direct type, without wildcard.
		 */
		EXPLICIT_DIRECT_TYPE {

			@Override
			public boolean isBoundInFunctionName() {
				return true;
			}
		};

		/** Replies the more precise status between the current and given statuses.
		 *
		 * @param status the second status.
		 * @return the more precise status.
		 */
		public TypeParameterStatus or(TypeParameterStatus status) {
			return status.ordinal() > ordinal() ? status : this;
		}

		/** Replies if this status enables to put the type parameter bounds in the generated function names.
		 *
		 * @return {@code true} if the bounds should be included in the function names.
		 */
		public abstract boolean isBoundInFunctionName();

	}

}

