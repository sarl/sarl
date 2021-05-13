/*
 * $Id$
 *
 * SARL is an general-purpose agent programming language.
 * More details on http://www.sarl.io
 *
 * Copyright (C) 2014-2021 the original authors or authors.
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

import static io.sarl.lang.util.SarlUtils.HIDDEN_MEMBER_CHARACTER;
import static io.sarl.lang.util.SarlUtils.isHiddenMember;

import java.lang.annotation.Annotation;
import java.lang.reflect.Field;
import java.lang.reflect.Modifier;
import java.security.MessageDigest;
import java.security.NoSuchAlgorithmException;
import java.util.BitSet;
import java.util.Collections;
import java.util.HashMap;
import java.util.Iterator;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;
import java.util.SortedSet;
import java.util.TreeMap;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import com.google.common.base.Strings;
import com.google.common.collect.Iterables;
import org.eclipse.emf.common.notify.Notifier;
import org.eclipse.emf.ecore.EObject;
import org.eclipse.emf.ecore.EStructuralFeature;
import org.eclipse.emf.ecore.resource.Resource;
import org.eclipse.emf.ecore.resource.ResourceSet;
import org.eclipse.emf.ecore.util.EcoreUtil;
import org.eclipse.xtend.core.jvmmodel.XtendJvmModelInferrer;
import org.eclipse.xtend.core.xtend.XtendFunction;
import org.eclipse.xtend.core.xtend.XtendMember;
import org.eclipse.xtend.core.xtend.XtendParameter;
import org.eclipse.xtend.core.xtend.XtendTypeDeclaration;
import org.eclipse.xtext.EcoreUtil2;
import org.eclipse.xtext.common.types.JvmConstructor;
import org.eclipse.xtext.common.types.JvmDeclaredType;
import org.eclipse.xtext.common.types.JvmFeature;
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
import org.eclipse.xtext.nodemodel.ICompositeNode;
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
import org.eclipse.xtext.xbase.annotations.xAnnotations.XAnnotationElementValuePair;
import org.eclipse.xtext.xbase.compiler.ImportManager;
import org.eclipse.xtext.xbase.jvmmodel.JvmTypeReferenceBuilder;
import org.eclipse.xtext.xbase.jvmmodel.JvmTypesBuilder;
import org.eclipse.xtext.xbase.lib.CollectionLiterals;
import org.eclipse.xtext.xbase.lib.Functions.Function1;
import org.eclipse.xtext.xbase.lib.Pure;
import org.eclipse.xtext.xbase.typesystem.conformance.TypeConformanceComputationArgument;
import org.eclipse.xtext.xbase.typesystem.override.OverrideHelper;
import org.eclipse.xtext.xbase.typesystem.references.LightweightTypeReference;
import org.eclipse.xtext.xbase.typesystem.references.LightweightTypeReferenceFactory;
import org.eclipse.xtext.xbase.typesystem.references.StandardTypeReferenceOwner;
import org.eclipse.xtext.xbase.typesystem.util.CommonTypeComputationServices;
import org.eclipse.xtext.xtype.XFunctionTypeRef;
import org.eclipse.xtext.xtype.XtypeFactory;
import org.osgi.framework.Version;

import io.sarl.lang.SARLVersion;
import io.sarl.lang.annotation.EarlyExit;
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
@SuppressWarnings({"checkstyle:classfanoutcomplexity", "checkstyle:methodcount"})
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

	static {
		final StringBuilder name = new StringBuilder();
		final String[] components = EarlyExit.class.getPackage().getName().split("\\."); //$NON-NLS-1$
		final int len = Math.min(3, components.length);
		for (int i = 0; i < len; ++i) {
			name.append(components[i]);
			name.append("."); //$NON-NLS-1$
		}
		SARL_PACKAGE_PREFIX = name.toString();
	}

	private Utils() {
		//
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
	 * @see OverrideHelper
	 */
	public static void populateInterfaceElements(
			JvmDeclaredType jvmElement,
			Map<ActionPrototype, JvmOperation> operations,
			Map<String, JvmField> fields,
			IActionPrototypeProvider sarlSignatureProvider) {
		for (final JvmFeature feature : jvmElement.getAllFeatures()) {
			if (!"java.lang.Object".equals(feature.getDeclaringType().getQualifiedName())) { //$NON-NLS-1$
				if (operations != null && feature instanceof JvmOperation) {
					final JvmOperation operation = (JvmOperation) feature;
					final ActionParameterTypes sig = sarlSignatureProvider.createParameterTypesFromJvmModel(
							operation.isVarArgs(), operation.getParameters());
					final ActionPrototype actionKey = sarlSignatureProvider.createActionPrototype(
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
	 * @see OverrideHelper
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
	 * @see OverrideHelper
	 */
	@SuppressWarnings({
		"checkstyle:cyclomaticcomplexity",
		"checkstyle:npathcomplexity",
		"checkstyle:nestedifdepth",
		"checkstyle:parameternumber"})
	public static void populateInheritanceContext(
			JvmDeclaredType jvmElement,
			JvmTypeReference extendedClass,
			Iterable<JvmTypeReference> extendedInterfaces,
			Map<ActionPrototype, JvmOperation> finalOperations,
			Map<ActionPrototype, JvmOperation> overridableOperations,
			Map<String, JvmField> inheritedFields,
			Map<ActionPrototype, JvmOperation> operationsToImplement,
			Map<ActionParameterTypes, JvmConstructor> superConstructors,
			IActionPrototypeProvider sarlSignatureProvider) {
		// Get the operations that must be implemented
		if (operationsToImplement != null && extendedInterfaces != null) {
			for (final JvmTypeReference interfaceReference : extendedInterfaces) {
				for (final JvmFeature feature : ((JvmGenericType) interfaceReference.getType()).getAllFeatures()) {
					if (!"java.lang.Object".equals(//$NON-NLS-1$
							feature.getDeclaringType().getQualifiedName())) {
						if (feature instanceof JvmOperation) {
							final JvmOperation operation = (JvmOperation) feature;
							final ActionParameterTypes sig = sarlSignatureProvider.createParameterTypesFromJvmModel(
									operation.isVarArgs(), operation.getParameters());
							final ActionPrototype actionKey = sarlSignatureProvider.createActionPrototype(
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

		// Check on the implemented features, inherited from the super type
		if (extendedClass != null) {
			final JvmGenericType parentType = (JvmGenericType) extendedClass.getType();
			for (final JvmFeature feature : parentType.getAllFeatures()) {
				if (!"java.lang.Object".equals(feature.getDeclaringType().getQualifiedName()) //$NON-NLS-1$
						&& isVisible(jvmElement, feature)
						&& !isHiddenMember(feature.getSimpleName())) {
					if (feature instanceof JvmOperation) {
						if (!feature.isStatic()) {
							final JvmOperation operation = (JvmOperation) feature;
							final ActionParameterTypes sig = sarlSignatureProvider.createParameterTypesFromJvmModel(
									operation.isVarArgs(), operation.getParameters());
							final ActionPrototype actionKey = sarlSignatureProvider.createActionPrototype(
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
					} else if (feature instanceof JvmField && inheritedFields != null) {
						inheritedFields.put(feature.getSimpleName(), (JvmField) feature);
					}
				}
			}

			if (superConstructors != null) {
				for (final JvmConstructor cons : parentType.getDeclaredConstructors()) {
					final ActionParameterTypes sig = sarlSignatureProvider.createParameterTypesFromJvmModel(
							cons.isVarArgs(), cons.getParameters());
					superConstructors.put(sig,  cons);
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
			final XtendParameter param = params.get(params.size() - 1);
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

	/** Create the name of the hidden method that is containing the evaluation of all the guards for a given event.
	 *
	 * @param eventId the id of the event.
	 * @return the method name.
	 */
	public static String createNameForHiddenGuardGeneralEvaluatorMethod(String eventId) {
		return PREFIX_GUARD_EVALUATOR + fixHiddenMember(eventId);
	}

	/** Create the name of the hidden method that is containing the event guard evaluation.
	 *
	 * @param eventId the id of the event.
	 * @param handlerIndex the index of the handler in the container type.
	 * @return the method name.
	 */
	public static String createNameForHiddenGuardEvaluatorMethod(String eventId, int handlerIndex) {
		return PREFIX_GUARD + fixHiddenMember(eventId)
			+ HIDDEN_MEMBER_CHARACTER + handlerIndex;
	}

	/** Create the name of the hidden method that is containing the event handler code.
	 *
	 * @param eventId the id of the event.
	 * @param handlerIndex the index of the handler in the container type.
	 * @return the attribute name.
	 */
	public static String createNameForHiddenEventHandlerMethod(String eventId, int handlerIndex) {
		return PREFIX_EVENT_HANDLER + fixHiddenMember(eventId) + HIDDEN_MEMBER_CHARACTER + handlerIndex;
	}

	/** Replies if the given reference is pointing to a class type.
	 *
	 * @param typeRef - the type reference to test.
	 * @return {@code true} if the pointed element is a class type.
	 */
	public static boolean isClass(LightweightTypeReference typeRef) {
		final JvmType t = typeRef.getType();
		if (t instanceof JvmGenericType) {
			return !((JvmGenericType) t).isInterface();
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
		return expressionTypeRef.getType() instanceof JvmDeclaredType
				&& ((JvmDeclaredType) expressionTypeRef.getType()).isFinal();
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
		return type.getType() instanceof JvmGenericType
				&& ((JvmGenericType) type.getType()).isInterface();
	}

	/** Replies if it is allowed to cast between the given types.
	 *
	 * @param fromType - source type
	 * @param toType - target type
	 * @param enablePrimitiveWidening - indicates if the widening of the primitive types is allowed.
	 * @param enableVoidMatchingNull - indicates if the {@code null} is matching <code>void</code>.
	 * @param allowSynonyms - indicates if the synonyms are allowed.
	 * @return the state of the cast.
	 */
	@SuppressWarnings({"checkstyle:cyclomaticcomplexity", "checkstyle:booleanexpressioncomplexity"})
	public static boolean canCast(
			LightweightTypeReference fromType, LightweightTypeReference toType,
			boolean enablePrimitiveWidening, boolean enableVoidMatchingNull,
			boolean allowSynonyms) {
		if (enableVoidMatchingNull) {
			final boolean fromVoid = fromType == null || fromType.isPrimitiveVoid();
			final boolean toVoid = toType == null || toType.isPrimitiveVoid();
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
		final TypeConformanceComputationArgument conform = new TypeConformanceComputationArgument(
				false, false, true, enablePrimitiveWidening, false, allowSynonyms);
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
		final Resource r = object.eResource();
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
		final StandardTypeReferenceOwner owner = new StandardTypeReferenceOwner(services, context);
		final LightweightTypeReferenceFactory factory = new LightweightTypeReferenceFactory(owner,
				keepUnboundWildcardInformation);
		final LightweightTypeReference reference = factory.toLightweightReference(typeRef);
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
		final StandardTypeReferenceOwner owner = new StandardTypeReferenceOwner(services, type);
		final LightweightTypeReferenceFactory factory = new LightweightTypeReferenceFactory(owner,
				keepUnboundWildcardInformation);
		final LightweightTypeReference reference = factory.toLightweightReference(type);
		return reference;
	}

	/** Compare the two strings as they are version numbers.
	 *
	 * @param v1 - first version to compare.
	 * @param v2 - second version to compare.
	 * @return Negative integer of <code>v1</code> is lower than <code>v2</code>;
	 *     positive integer of <code>v1</code> is greater than <code>v2</code>;
	 *     {@code 0} if they are strictly equal.
	 */
	public static int compareVersions(String v1, String v2) {
		// Remove the SNAPSHOT version.
		//final String fixedv1 = v1.replaceFirst("-SNAPSHOT$", ""); //$NON-NLS-1$ //$NON-NLS-2$
		//final String fixedv2 = v2.replaceFirst("-SNAPSHOT$", ""); //$NON-NLS-1$ //$NON-NLS-2$
		//final Version vobject1 = Version.parseVersion(fixedv1);
		//final Version vobject2 = Version.parseVersion(fixedv2);
		final Version vobject1 = Version.parseVersion(v1);
		final Version vobject2 = Version.parseVersion(v2);
		return vobject1.compareTo(vobject2);
	}

	private static void addAnnotationToSignature(StringBuilder textRepresentation, SARLGrammarKeywordAccess elements,
			ISerializer serializer, ImportManager importManager, XAnnotation annotation) {
		textRepresentation.append(elements.getCommercialAtKeyword());
		textRepresentation.append(getSignatureType(annotation.getAnnotationType(), importManager));
		final XExpression value = annotation.getValue();
		if (value != null) {
			textRepresentation.append(elements.getLeftParenthesisKeyword());
			textRepresentation.append(serializer.serialize(value).trim());
			textRepresentation.append(elements.getRightParenthesisKeyword());
		} else if (!annotation.getElementValuePairs().isEmpty()) {
			textRepresentation.append(elements.getLeftParenthesisKeyword());
			boolean addComa = false;
			for (final XAnnotationElementValuePair pair : annotation.getElementValuePairs()) {
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
		final ICompositeNode node = NodeModelUtils.getNode(object);
		if (node != null) {
			String text = node.getText();
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
	@SuppressWarnings({"checkstyle:cyclomaticcomplexity", "checkstyle:npathcomplexity"})
	public static String getActionSignatureString(SarlAction signature, ISerializer serializer,
			SARLGrammarKeywordAccess grammarAccess, ImportManager importManager) {
		// Try the serializer
		try {
			return serializer.serialize(signature);
		} catch (Throwable exception) {
			// No working, perhaps the context's of the signature is unknown
		}
		final StringBuilder textRepresentation = new StringBuilder();
		// Annotations
		for (final XAnnotation annotation : signature.getAnnotations()) {
			addAnnotationToSignature(textRepresentation, grammarAccess, serializer, importManager, annotation);
		}
		// Modifiers
		for (final String modifier : signature.getModifiers()) {
			textRepresentation.append(modifier);
			textRepresentation.append(' ');
		}
		// Generic type
		if (!signature.getTypeParameters().isEmpty()) {
			boolean addComa = false;
			textRepresentation.append(grammarAccess.getLessThanSignKeyword());
			for (final JvmTypeParameter typeParameter : signature.getTypeParameters()) {
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
			final int idx = signature.getParameters().size() - 1;
			for (int i = 0; i < idx; ++i) {
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
		final JvmTypeReference returnType = signature.getReturnType();
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
			boolean addComa = false;
			for (final JvmTypeReference eventType : signature.getExceptions()) {
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
			boolean addComa = false;
			for (final JvmTypeReference eventType : signature.getFiredEvents()) {
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
		} else if (parameter instanceof SarlFormalParameter) {
			final SarlFormalParameter sarlParameter = (SarlFormalParameter) parameter;
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
	@SuppressWarnings({
		"checkstyle:cyclomaticcomplexity",
		"checkstyle:npathcomplexity",
		"checkstyle:magicnumber",
		"checkstyle:booleanexpressioncomplexity"})
	public static long computeSerialVersionUID(JvmGenericType jvm) {
		final StringBuilder serialVersionUIDBuffer = new StringBuilder();

		serialVersionUIDBuffer.append(jvm.getQualifiedName());

		BitSet bitset = new BitSet(32);
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

		final SortedSet<JvmTypeReference> superTypes = CollectionLiterals.newTreeSet(new JvmTypeReferenceComparator());
		superTypes.addAll(jvm.getSuperTypes());

		final SortedSet<JvmField> fields = CollectionLiterals.newTreeSet(new JvmIdentifiableComparator());
		final SortedSet<JvmConstructor> constructors = CollectionLiterals.newTreeSet(new JvmIdentifiableComparator());
		final SortedSet<JvmOperation> operations = CollectionLiterals.newTreeSet(new JvmIdentifiableComparator());
		for (final JvmMember member : jvm.getMembers()) {
			if (member instanceof JvmField) {
				final JvmField field = (JvmField) member;
				if ((field.getVisibility() != JvmVisibility.PRIVATE)
						|| (!field.isStatic() && !field.isTransient())) {
					fields.add(field);
				}
			} else if (member instanceof JvmConstructor) {
				final JvmConstructor constructor = (JvmConstructor) member;
				if (constructor.getVisibility() != JvmVisibility.PRIVATE) {
					constructors.add(constructor);
				}
			} else if (member instanceof JvmOperation) {
				final JvmOperation operation = (JvmOperation) member;
				if (operation.getVisibility() != JvmVisibility.PRIVATE) {
					operations.add(operation);
				}
			}
		}

		for (final JvmTypeReference superType : superTypes) {
			serialVersionUIDBuffer.append(superType.getQualifiedName());
		}

		for (final JvmField field : fields) {
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

		for (final JvmConstructor constructor : constructors) {
			bitset = new BitSet(32);
			bitset.set(constructor.getVisibility().getValue());
			if (constructor.isStatic()) {
				bitset.set(4);
			}
			if (constructor.isVarArgs()) {
				bitset.set(5);
			}
			serialVersionUIDBuffer.append(bitset.toByteArray());
			for (final JvmFormalParameter parameter : constructor.getParameters()) {
				serialVersionUIDBuffer.append(parameter.getParameterType().getIdentifier());
			}
		}

		for (final JvmOperation operation : operations) {
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
			for (final JvmFormalParameter parameter : operation.getParameters()) {
				serialVersionUIDBuffer.append(parameter.getParameterType().getIdentifier());
			}
		}

		long key = 1L;
		try {
			final byte[] uniqueKey = serialVersionUIDBuffer.toString().getBytes();
			final byte[] sha = MessageDigest.getInstance("SHA").digest(uniqueKey); //$NON-NLS-1$
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
		return QualifiedName.create(element.getQualifiedName('.').split("\\.")); //$NON-NLS-1$
	}

	/** Replies if the given declaration has an abstract member.
	 *
	 * @param declaration - the declaration.
	 * @return {@code true} if the given type has an abstract function.
	 */
	public static boolean hasAbstractMember(XtendTypeDeclaration declaration) {
		if (declaration != null) {
			for (final XtendMember member : declaration.getMembers()) {
				if (member instanceof XtendFunction) {
					if (((XtendFunction) member).isAbstract()) {
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
		final OutParameter<String> version = new OutParameter<>();
		final SarlLibraryErrorCode code = getSARLLibraryVersionOnClasspath(typeReferences, context, version);
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
			final Version currentVersion = Version.parseVersion(SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING);
			final Version paramVersion = Version.parseVersion(version);
			return currentVersion.getMajor() == paramVersion.getMajor()
					&& currentVersion.getMinor() == paramVersion.getMinor();
		}
		return false;
	}

	/** Check if a version of the JDK is compatible with the SARL compilation environment.
	 * In other words, check if the SARL compiler is able to be run without issue
	 * with a JDK at the given version.
	 *
	 * @param version - the version to test.
	 * @return {@code true} if this version is for a compatible JDK.
	 *     Otherwise {@code false}.
	 * @since 0.10
	 * @see SARLVersion#MINIMAL_JDK_VERSION_FOR_SARL_COMPILATION_ENVIRONMENT
	 * @see SARLVersion#INCOMPATIBLE_JDK_VERSION_FOR_SARL_COMPILATION_ENVIRONMENT
	 */
	public static boolean isCompatibleJDKVersionWithSARLCompilationEnvironment(String version) {
		if (version != null && !version.isEmpty()) {
			final Version current = Version.parseVersion(version);
			if (current != null) {
				final Version minJdk = Version.parseVersion(SARLVersion.MINIMAL_JDK_VERSION_FOR_SARL_COMPILATION_ENVIRONMENT);
				assert minJdk != null;
				if (current.compareTo(minJdk) >= 0) {
					final Version maxJdk = Version.parseVersion(SARLVersion.INCOMPATIBLE_JDK_VERSION_FOR_SARL_COMPILATION_ENVIRONMENT);
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
	 * @see SARLVersion#MINIMAL_JDK_VERSION_FOR_SARL_COMPILATION_ENVIRONMENT
	 * @see SARLVersion#INCOMPATIBLE_JDK_VERSION_FOR_SARL_COMPILATION_ENVIRONMENT
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
	 * @see SARLVersion#MINIMAL_JDK_VERSION_IN_SARL_PROJECT_CLASSPATH
	 * @see SARLVersion#INCOMPATIBLE_JDK_VERSION_IN_SARL_PROJECT_CLASSPATH
	 */
	public static boolean isCompatibleJDKVersionWhenInSARLProjectClasspath(String version) {
		if (version != null && !version.isEmpty()) {
			final Version current = Version.parseVersion(version);
			if (current != null) {
				final Version minJdk = Version.parseVersion(SARLVersion.MINIMAL_JDK_VERSION_IN_SARL_PROJECT_CLASSPATH);
				assert minJdk != null;
				if (current.compareTo(minJdk) >= 0) {
					final Version maxJdk = Version.parseVersion(SARLVersion.INCOMPATIBLE_JDK_VERSION_IN_SARL_PROJECT_CLASSPATH);
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
		final XtextVersion xtextVersion = XtextVersion.getCurrent();
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
	@Deprecated
	public static String getSARLLibraryVersionOnClasspath(TypeReferences typeReferences, Notifier context) {
		final OutParameter<String> version = new OutParameter<>();
		final SarlLibraryErrorCode code = getSARLLibraryVersionOnClasspath(typeReferences, context, version);
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
	@SuppressWarnings("checkstyle:npathcomplexity")
	public static SarlLibraryErrorCode getSARLLibraryVersionOnClasspath(TypeReferences typeReferences, Notifier context,
			OutParameter<String> version) {
		if (checkSarlVersionClass) {
			checkSarlVersionClass = false;
			try {
				final Object v = SARLVersion.class.getDeclaredField(SARL_VERSION_FIELD_NAME_STR);
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
		if (!(type instanceof JvmDeclaredType)) {
			return SarlLibraryErrorCode.NO_SARL_VERSION_DECLARED_TYPE;
		}
		final JvmDeclaredType sarlVersionType = (JvmDeclaredType) type;
		JvmField versionField = null;
		final Iterator<JvmField> iterator = sarlVersionType.getDeclaredFields().iterator();
		while (versionField == null && iterator.hasNext()) {
			final JvmField field = iterator.next();
			if (SARL_VERSION_FIELD_NAME_STR.equals(field.getSimpleName())) {
				versionField = field;
			}
		}
		if (versionField == null) {
			return SarlLibraryErrorCode.NO_SARL_VERSION_FIELD;
		}
		final String value = versionField.getConstantValueAsString();
		if (Strings.isNullOrEmpty(value)) {
			return SarlLibraryErrorCode.NO_SARL_VERSION_VALUE;
		}
		if (version != null) {
			version.set(value);
		}
		return SarlLibraryErrorCode.SARL_FOUND;
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
			final Map<ActionPrototype, JvmOperation> operations = new HashMap<>();
			populateInterfaceElements(type, operations, null, sarlSignatureProvider);
			if (operations.size() == 1) {
				final JvmOperation op = operations.values().iterator().next();
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
	 * @see EcoreUtil2#getContainerOfType(EObject, Class)
	 */
	public static boolean getContainerNotOfType(EObject element, Class<? extends EObject> type,
			OutParameter<EObject> container, OutParameter<EObject> directContainerChild) {
		EObject previous = element;
		EObject elt = element.eContainer();
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
	 * @see EcoreUtil2#getContainerOfType(EObject, Class)
	 */
	@SafeVarargs
	public static boolean getContainerOfType(EObject element,
			OutParameter<EObject> container, OutParameter<EObject> directContainerChild,
			Class<? extends EObject>... types) {
		EObject previous = element;
		EObject elt = element.eContainer();
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
		for (final Class<? extends EObject> type : types) {
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
	 * @see EcoreUtil2#getContainerOfType(EObject, Class)
	 */
	public static EObject getFirstContainerForPredicate(EObject element, Function1<? super EObject, ? extends Boolean> predicate) {
		if (predicate == null || element == null) {
			return null;
		}
		EObject elt = element.eContainer();
		while (elt != null) {
			if (predicate.apply(elt)) {
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
		/** The byte code (the class) of {@link SARLVersion} does not contains the expected field.
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
	@SuppressWarnings("checkstyle:npathcomplexity")
	public static String dump(Object object, boolean includeStaticField) {
		if (object == null) {
			return new String();
		}
		final StringBuilder buffer = new StringBuilder();
		final LinkedList<Class<?>> types = new LinkedList<>();
		types.add(object.getClass());
		while (!types.isEmpty()) {
			final Class<?> type = types.removeFirst();

			final Class<?> supertype = type.getSuperclass();
			if (supertype != null && !supertype.equals(Object.class)) {
				types.add(supertype);
			}

			if (buffer.length() > 0) {
				buffer.append("\n"); //$NON-NLS-1$
			}
			final Field[] fields = type.getDeclaredFields();
			buffer.append(type.getSimpleName()).append(" {\n"); //$NON-NLS-1$

			boolean firstRound = true;

			for (final Field field : fields) {
				if (!includeStaticField && Modifier.isStatic(field.getModifiers())) {
					continue;
				}
				if (!firstRound) {
					buffer.append(",\n"); //$NON-NLS-1$
				}
				firstRound = false;
				field.setAccessible(true);
				try {
					final Object fieldObj = field.get(object);
					final String value;
					if (null == fieldObj) {
						value = "null"; //$NON-NLS-1$
					} else {
						value = fieldObj.toString();
					}
					buffer.append('\t').append(field.getName()).append('=').append('"');
					buffer.append(org.eclipse.xtext.util.Strings.convertToJavaString(value));
					buffer.append("\"\n"); //$NON-NLS-1$
				} catch (IllegalAccessException ignore) {
					//this should never happen
				}

			}

			buffer.append('}');
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

		boolean cloneType = true;
		JvmTypeReference typeCandidate = type;

		// Use also cloneType as a flag that indicates if the type was already found in type parameters.
		if ((executableTypeParameters.iterator().hasNext() || !superTypeParameterMapping.isEmpty()) && cloneType) {
			final Map<String, JvmTypeParameter> typeParameterIdentifiers = new TreeMap<>();
			for (final JvmTypeParameter typeParameter : executableTypeParameters) {
				typeParameterIdentifiers.put(typeParameter.getIdentifier(), typeParameter);
			}

			if (type instanceof JvmParameterizedTypeReference) {
				// Try to clone the type parameters.
				cloneType = false;
				typeCandidate = cloneAndAssociate(type, typeParameterIdentifiers, superTypeParameterMapping,
						typeParameterBuilder, typeReferences, jvmTypesFactory);
			} else if (type instanceof XFunctionTypeRef) {
				// Try to clone the function reference.
				final XFunctionTypeRef functionRef = (XFunctionTypeRef) type;
				cloneType = false;
				final XFunctionTypeRef cloneReference = XtypeFactory.eINSTANCE.createXFunctionTypeRef();
				for (final JvmTypeReference paramType : functionRef.getParamTypes()) {
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
		final EcoreUtil.Copier copier = new EcoreUtil.Copier(false) {
			private static final long serialVersionUID = 698510355384773254L;

			@Override
			public EObject copy(EObject eobject) {
				final String id;
				// Try to override the type parameters
				if (eobject instanceof JvmTypeReference) {
					id = ((JvmTypeReference) eobject).getIdentifier();
				} else if (eobject instanceof JvmIdentifiableElement) {
					id = ((JvmIdentifiableElement) eobject).getIdentifier();
				} else {
					id = null;
				}
				if (id != null) {
					final JvmTypeParameter param = typeParameterIdentifiers.get(id);
					if (param != null) {
						return typeReferences.createTypeRef(param);
					}
					final JvmTypeReference superTypeReference = superTypeParameterMapping.get(id);
					if (superTypeReference != null) {
						return typeReferences.createDelegateTypeReference(superTypeReference);
					}
				}
				final EObject result = super.copy(eobject);
				if (result instanceof JvmWildcardTypeReference) {
					final JvmWildcardTypeReference wildcardType = (JvmWildcardTypeReference) result;
					boolean upperBoundSeen = false;
					for (final JvmTypeConstraint constraint : wildcardType.getConstraints()) {
						if (constraint instanceof JvmUpperBound) {
							upperBoundSeen = true;
							break;
						}
					}
					if (!upperBoundSeen) {
						// no upper bound found - seems to be an invalid - assume object as upper bound
						final JvmTypeReference object = typeParameterBuilder.typeRef(Object.class);
						final JvmUpperBound upperBound = jvmTypesFactory.createJvmUpperBound();
						upperBound.setTypeReference(object);
						wildcardType.getConstraints().add(0, upperBound);
					}
				}
				return result;
			}
		};
		final JvmTypeReference copy = (JvmTypeReference) copier.copy(type);
		copier.copyReferences();
		return copy;
	}

	/** Extract the mapping between the type parameters declared within the super types and the
	 * type parameters arguments that are declared within the given type.
	 *
	 * <p>For example, consider the following code:
	 * <pre><code>
	 * interface X&lt;T&gt; {
	 *   def a(p1 : T, p2 : U) with U
	 * }
	 * interface Y&lt;T&gt; {
	 * }
	 * class Z&lt;TT&gt; implements X&lt;TT&gt;, Y&lt;TT&gt; {
	 *   def a(p1 : TT, p2 : W) with W { }
	 * }
	 * </code></pre>
	 * The mapping is:
	 * <pre><code>
	 * X.T =&gt; TT
	 * Y.T =&gt; TT
	 * </code></pre>
	 *
	 * @param type the type to analyze.
	 * @param mapping the map to fill with the mapping.
	 * @since 0.7
	 */
	public static void getSuperTypeParameterMap(JvmDeclaredType type, Map<String, JvmTypeReference> mapping) {
		for (final JvmTypeReference superTypeReference : type.getSuperTypes()) {
			if (superTypeReference instanceof JvmParameterizedTypeReference) {
				final JvmParameterizedTypeReference parameterizedTypeReference = (JvmParameterizedTypeReference) superTypeReference;
				final JvmType st = superTypeReference.getType();
				if (st instanceof JvmTypeParameterDeclarator) {
					final JvmTypeParameterDeclarator superType = (JvmTypeParameterDeclarator) st;
					int i = 0;
					for (final JvmTypeParameter typeParameter : superType.getTypeParameters()) {
						mapping.put(typeParameter.getIdentifier(), parameterizedTypeReference.getArguments().get(i));
						++i;
					}
				}
			}
		}
	}

	/** Copy the type parameters from a JvmOperation.
	 *
	 * <p>This function differs from {@link XtendJvmModelInferrer#copyAndFixTypeParameters(List,
	 * org.eclipse.xtext.common.types.JvmTypeParameterDeclarator)}
	 * and {@link XtendJvmModelInferrer#copyTypeParameters(List, org.eclipse.xtext.common.types.JvmTypeParameterDeclarator)}
	 * in the fact that the type parameters were already generated and fixed. The current function supper generic types by
	 * clone the types references with {@link #cloneWithTypeParametersAndProxies(JvmTypeReference, Iterable, Map, JvmTypeReferenceBuilder,
	 * JvmTypesBuilder, TypeReferences, TypesFactory)}.
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
		final Map<String, JvmTypeReference> superTypeParameterMapping = new HashMap<>();
		Utils.getSuperTypeParameterMap(toOperation.getDeclaringType(), superTypeParameterMapping);
		copyTypeParametersFromJvmOperation(
				fromOperation.getTypeParameters(),
				toOperation.getTypeParameters(),
				superTypeParameterMapping,
				typeParameterBuilder, typeBuilder, typeReferences, jvmTypesFactory);
	}

	/** Copy the type parameters from a JvmOperation.
	 *
	 * <p>This function differs from {@link XtendJvmModelInferrer#copyAndFixTypeParameters(List,
	 * org.eclipse.xtext.common.types.JvmTypeParameterDeclarator)}
	 * and {@link XtendJvmModelInferrer#copyTypeParameters(List, org.eclipse.xtext.common.types.JvmTypeParameterDeclarator)}
	 * in the fact that the type parameters were already generated and fixed. The current function supper generic types by
	 * clone the types references with {@link #cloneWithTypeParametersAndProxies(JvmTypeReference, Iterable, Map, JvmTypeReferenceBuilder,
	 * JvmTypesBuilder, TypeReferences, TypesFactory)}.
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
		for (final JvmTypeParameter typeParameter : inputParameters) {
			final JvmTypeParameter typeParameterCopy = jvmTypesFactory.createJvmTypeParameter();
			typeParameterCopy.setName(typeParameter.getName());
			outputParameters.add(typeParameterCopy);
		}
		// Second step is the constraints' copy
		for (int i = 0; i < inputParameters.size(); ++i) {
			final JvmTypeParameter typeParameter = inputParameters.get(i);
			final JvmTypeParameter typeParameterCopy = outputParameters.get(i);
			for (final JvmTypeConstraint constraint : typeParameter.getConstraints()) {
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
		final EObject container = featureCall.eContainer();
		final XAbstractFeatureCall rootFeatureCall;
		if (container instanceof XMemberFeatureCall || container instanceof XFeatureCall) {
			rootFeatureCall = (XAbstractFeatureCall) getFirstContainerForPredicate(featureCall,
				it -> it.eContainer() != null && !(it.eContainer() instanceof XMemberFeatureCall || it.eContainer() instanceof XFeatureCall));
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
		XAbstractFeatureCall current = featureCall;
		EObject currentContainer = current.eContainer();
		while (currentContainer != null) {
			if (currentContainer instanceof XMemberFeatureCall || currentContainer instanceof XFeatureCall) {
				final XAbstractFeatureCall c = (XAbstractFeatureCall) currentContainer;
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
		if (current instanceof XAbstractFeatureCall) {
			final XAbstractFeatureCall featureCall = (XAbstractFeatureCall) current;
			if (isLocalEntity(featureCall, container, containerParameters)) {
				return true;
			}
			for (final XExpression argument : featureCall.getActualArguments()) {
				final Iterable<XAbstractFeatureCall> iterable;
				if (argument instanceof XAbstractFeatureCall) {
					iterable = Iterables.concat(
							Collections.singletonList((XAbstractFeatureCall) argument),
							EcoreUtil2.getAllContentsOfType(argument, XAbstractFeatureCall.class));
				} else {
					iterable = EcoreUtil2.getAllContentsOfType(argument, XAbstractFeatureCall.class);
				}
				for (final XAbstractFeatureCall c : iterable) {
					if (isLocalEntity(c, container, containerParameters)) {
						return true;
					}
				}
			}
		}
		return false;
	}

	private static boolean isLocalEntity(XAbstractFeatureCall featureCall, XExpression container, List<JvmFormalParameter> containerParameters) {
		final JvmIdentifiableElement feature = featureCall.getFeature();
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
		final JvmType jtype = type.getType();
		if (jtype instanceof JvmTypeParameter) {
			return true;
		}
		for (final LightweightTypeReference atype : type.getTypeArguments()) {
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
		final String str = Strings.emptyToNull(text);
		if (str == null) {
			return Messages.Utils_0;
		}
		return text;
	}

}

