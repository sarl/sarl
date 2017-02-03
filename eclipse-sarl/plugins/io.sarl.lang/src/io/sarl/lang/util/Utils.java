/*
 * $Id$
 *
 * SARL is an general-purpose agent programming language.
 * More details on http://www.sarl.io
 *
 * Copyright (C) 2014-2017 the original authors or authors.
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

import java.lang.annotation.Annotation;
import java.lang.reflect.Modifier;
import java.security.MessageDigest;
import java.security.NoSuchAlgorithmException;
import java.util.BitSet;
import java.util.Collections;
import java.util.HashMap;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.SortedSet;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import com.google.common.base.Strings;
import org.eclipse.emf.common.notify.Notifier;
import org.eclipse.xtend.core.xtend.XtendFunction;
import org.eclipse.xtend.core.xtend.XtendMember;
import org.eclipse.xtend.core.xtend.XtendParameter;
import org.eclipse.xtend.core.xtend.XtendTypeDeclaration;
import org.eclipse.xtext.common.types.JvmConstructor;
import org.eclipse.xtext.common.types.JvmDeclaredType;
import org.eclipse.xtext.common.types.JvmFeature;
import org.eclipse.xtext.common.types.JvmField;
import org.eclipse.xtext.common.types.JvmFormalParameter;
import org.eclipse.xtext.common.types.JvmGenericType;
import org.eclipse.xtext.common.types.JvmIdentifiableElement;
import org.eclipse.xtext.common.types.JvmMember;
import org.eclipse.xtext.common.types.JvmOperation;
import org.eclipse.xtext.common.types.JvmType;
import org.eclipse.xtext.common.types.JvmTypeParameter;
import org.eclipse.xtext.common.types.JvmTypeReference;
import org.eclipse.xtext.common.types.JvmVisibility;
import org.eclipse.xtext.common.types.TypesPackage;
import org.eclipse.xtext.common.types.util.TypeReferences;
import org.eclipse.xtext.naming.QualifiedName;
import org.eclipse.xtext.serializer.ISerializer;
import org.eclipse.xtext.util.XtextVersion;
import org.eclipse.xtext.xbase.XExpression;
import org.eclipse.xtext.xbase.annotations.xAnnotations.XAnnotation;
import org.eclipse.xtext.xbase.annotations.xAnnotations.XAnnotationElementValuePair;
import org.eclipse.xtext.xbase.compiler.ImportManager;
import org.eclipse.xtext.xbase.lib.CollectionLiterals;
import org.eclipse.xtext.xbase.lib.Inline;
import org.eclipse.xtext.xbase.typesystem.conformance.TypeConformanceComputationArgument;
import org.eclipse.xtext.xbase.typesystem.references.LightweightTypeReference;
import org.eclipse.xtext.xbase.typesystem.references.LightweightTypeReferenceFactory;
import org.eclipse.xtext.xbase.typesystem.references.StandardTypeReferenceOwner;
import org.eclipse.xtext.xbase.typesystem.util.CommonTypeComputationServices;
import org.osgi.framework.Version;

import io.sarl.lang.SARLVersion;
import io.sarl.lang.actionprototype.ActionParameterTypes;
import io.sarl.lang.actionprototype.ActionPrototype;
import io.sarl.lang.actionprototype.IActionPrototypeProvider;
import io.sarl.lang.annotation.EarlyExit;
import io.sarl.lang.sarl.SarlAction;
import io.sarl.lang.sarl.SarlFormalParameter;
import io.sarl.lang.services.SARLGrammarKeywordAccess;

/**
 * Utilities functions on JvmElements.
 *
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 */
@SuppressWarnings("checkstyle:classfanoutcomplexity")
public final class Utils {

	/** Character for hidden features.
	 */
	public static final String HIDDEN_MEMBER_CHARACTER = "$"; //$NON-NLS-1$

	private static final String PREFIX_DEFAULT_VALUE = HIDDEN_MEMBER_CHARACTER + "DEFAULT_VALUE" //$NON-NLS-1$
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

	/** Analyzing the type hierarchy of the given interface and
	 * extract hierarchy information.
	 *
	 * @param jvmElement - the element to analyze
	 * @param operations - filled with the operations inside and inherited by the element.
	 * @param fields - filled with the fields inside and inherited by the element.
	 * @param sarlSignatureProvider - provider of tools related to action signatures.
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
	 */
	@SuppressWarnings({
		"checkstyle:cyclomaticcomplexity",
		"checkstyle:npathcomplexity",
		"checkstyle:nestedifdepth"})
	public static void populateInheritanceContext(
			JvmDeclaredType jvmElement,
			Map<ActionPrototype, JvmOperation> finalOperations,
			Map<ActionPrototype, JvmOperation> overridableOperations,
			Map<String, JvmField> inheritedFields,
			Map<ActionPrototype, JvmOperation> operationsToImplement,
			Map<ActionParameterTypes, JvmConstructor> superConstructors,
			IActionPrototypeProvider sarlSignatureProvider) {
		// Get the operations that must be implemented
		if (operationsToImplement != null) {
			for (final JvmTypeReference interfaceReference : jvmElement.getExtendedInterfaces()) {
				for (final JvmFeature feature : ((JvmGenericType) interfaceReference.getType()).getAllFeatures()) {
					if (!"java.lang.Object".equals(//$NON-NLS-1$
							feature.getDeclaringType().getQualifiedName())) {
						if (feature instanceof JvmOperation) {
							final JvmOperation operation = (JvmOperation) feature;
							final ActionParameterTypes sig = sarlSignatureProvider.createParameterTypesFromJvmModel(
									operation.isVarArgs(), operation.getParameters());
							final ActionPrototype actionKey = sarlSignatureProvider.createActionPrototype(
									operation.getSimpleName(), sig);
							operationsToImplement.put(actionKey, operation);
						}
					}
				}
			}
		}

		// Check on the implemented features, inherited from the super type
		if (jvmElement.getExtendedClass() != null) {
			final JvmGenericType parentType = (JvmGenericType) jvmElement.getExtendedClass().getType();
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
							if (operation.isAbstract()) {
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
	 * @return <code>true</code> if the given type can see the target feature.
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
	 * @return <code>true</code> if the late parameter is variadic.
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

	/** Replies if the given name is related to an hidden action.
	 *
	 * <p>An hidden action is an action that is generated by the SARL
	 * compiler, and that cannot be defined by the SARL user.
	 *
	 * @param name - the name to test.
	 * @return <code>true</code> if the given name is reserved by SARL.
	 */
	public static boolean isHiddenMember(String name) {
		return name.contains(HIDDEN_MEMBER_CHARACTER);
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

	/** Create the name of the hidden attribute that is containing a parameter's default value.
	 *
	 * @param id the id of the default value.
	 * @return the method name.
	 */
	public static String createNameForHiddenDefaultValueAttribute(String id) {
		return PREFIX_DEFAULT_VALUE + fixHiddenMember(id.toUpperCase());
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
	 * @return <code>true</code> if the given simple name if for the hidden method for capacuty uses.
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
	 * @return <code>true</code> if the pointed element is a class type.
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
	 * @return <code>true</code> if the element is a class type.
	 */
	public static boolean isClass(Class<?> type) {
		return !type.isInterface();
	}

	/** Replies if the given reference is referencing a final type.
	 *
	 * @param expressionTypeRef - the type reference to test.
	 * @return <code>true</code> if the given type is final.
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
	 * @return <code>true</code> if the given type is final.
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
	 * @return <code>true</code> if the given type is an interface.
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
	 * @param enableVoidMatchingNull - indicates if the <code>null</code> is matching <code>void</code>.
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

	/** Convert a type reference to a lightweight type reference.
	 *
	 * @param typeRef - reference to convert.
	 * @param services - services used for the conversion
	 * @return the lightweight type reference.
	 */
	public static LightweightTypeReference toLightweightTypeReference(
			JvmTypeReference typeRef, CommonTypeComputationServices services) {
		return toLightweightTypeReference(typeRef, services, false);
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
		if (typeRef == null) {
			return null;
		}
		final StandardTypeReferenceOwner owner = new StandardTypeReferenceOwner(services, typeRef);
		final LightweightTypeReferenceFactory factory = new LightweightTypeReferenceFactory(owner,
				keepUnboundWildcardInformation);
		final LightweightTypeReference reference = factory.toLightweightReference(typeRef);
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
	@Inline(value = "Version.parseVersion($1).compareTo(Version.parseVersion($2))",
			imported = {Version.class})
	public static int compareVersions(String v1, String v2) {
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

	/** This is a context-safe serializer of a signature.
	 *
	 * @param signature - the signature to serialize.
	 * @param serializer - the Xtext serializer
	 * @param grammarAccess - the accessor to the SARL grammar.
	 * @param importManager - used to collect the types to import.
	 *     If <code>null</code>, the qualified names of the types with be put in the signature.
	 * @return the string representation of the signature.
	 */
	@SuppressWarnings({"checkstyle:cyclomaticcomplexity", "checkstyle:npathcomplexity"})
	public static String getActionSignatureString(SarlAction signature, ISerializer serializer,
			SARLGrammarKeywordAccess grammarAccess, ImportManager importManager) {
		// Try the serializer
		try {
			//TODO: Check if there is a way to serialize without context
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
	 * the element is <code>null</code>.
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
	 * @return <code>true</code> if the given type has an abstract function.
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
	 * @return <code>true</code> if a compatible SARL library was found.
	 *     Otherwise <code>false</code>.
	 */
	public static boolean isCompatibleSARLLibraryOnClasspath(TypeReferences typeReferences, Notifier context) {
		return isCompatibleSARLLibraryVersion(getSARLLibraryVersionOnClasspath(typeReferences, context));
	}

	/** Check if a version is compatible with the expected SARL library.
	 *
	 * @param version - the version to test.
	 * @return <code>true</code> if a compatible SARL library was found.
	 *     Otherwise <code>false</code>.
	 */
	public static boolean isCompatibleSARLLibraryVersion(String version) {
		return org.eclipse.xtext.util.Strings.equal(SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING, version);
	}

	/** Check if a version of the JRE is compatible with the SARL library.
	 *
	 * @param version - the version to test.
	 * @return <code>true</code> if this version is for a compatible JRE.
	 *     Otherwise <code>false</code>.
	 */
	public static boolean isCompatibleJREVersion(String version) {
		return version != null && !version.isEmpty()
				&& compareVersions(version, SARLVersion.MINIMAL_JDK_VERSION) >= 0;
	}

	/** Check if a version of the current JRE is compatible with the SARL library.
	 *
	 * @return <code>true</code> if this version is for a compatible JRE.
	 *     Otherwise <code>false</code>.
	 */
	public static boolean isCompatibleJREVersion() {
		return isCompatibleJREVersion(System.getProperty("java.specification.version")); //$NON-NLS-1$
	}

	/** Check if a version of Xtext is compatible with the SARL library.
	 *
	 * @param version - the version to test.
	 * @return <code>true</code> if this version is for a compatible Xtext.
	 *     Otherwise <code>false</code>.
	 */
	public static boolean isCompatibleXtextVersion(String version) {
		return version != null && !version.isEmpty()
				&& compareVersions(version, SARLVersion.MINIMAL_XTEXT_VERSION) >= 0;
	}

	/** Check if a version of the current Xtext is compatible with the SARL library.
	 *
	 * @return <code>true</code> if this version is for a compatible Xtext.
	 *     Otherwise <code>false</code>.
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
	 * @return the version, or <code>null</code> if the SARL library cannot be found or
	 *     is too old.
	 */
	public static String getSARLLibraryVersionOnClasspath(TypeReferences typeReferences, Notifier context) {
		try {
			final JvmType type = typeReferences.findDeclaredType(SARLVersion.class.getName(), context);
			if (type instanceof JvmDeclaredType) {
				JvmField versionField = null;
				final Iterator<JvmField> iterator = ((JvmDeclaredType) type).getDeclaredFields().iterator();
				while (versionField == null && iterator.hasNext()) {
					final JvmField field = iterator.next();
					if ("SPECIFICATION_RELEASE_VERSION_STRING".equals(field.getSimpleName())) { //$NON-NLS-1$
						versionField = field;
					}
				}
				if (versionField != null) {
					return versionField.getConstantValueAsString();
				}
			}
		} catch (Throwable exception) {
			//
		}
		return null;
	}

	/** Replies if the given annotation is an annotation from the SARL core library.
	 *
	 * @param type the type of the annotation
	 * @return <code>true</code> if the given type is a SARL annotation.
	 */
	public static boolean isSARLAnnotation(Class<?> type) {
		return (type != null && Annotation.class.isAssignableFrom(type))
				&& isSARLAnnotation(type.getPackage().getName());
	}

	/** Replies if the given annotation is an annotation from the SARL core library.
	 *
	 * @param qualifiedName the qualified name of the annotation type.
	 * @return <code>true</code> if the given type is a SARL annotation.
	 */
	public static boolean isSARLAnnotation(String qualifiedName) {
		return qualifiedName != null && qualifiedName.startsWith(SARL_PACKAGE_PREFIX);
	}

	/** Replies if the given type is a primitive "void".
	 *
	 * <p>If the given parametr is <code>null</code>, this function returns <code>true</code>.
	 *
	 * @param type the type to test.
	 * @return <code>true</code> if the type is void or <code>null</code>.
	 */
	public static boolean isPrimitiveVoid(JvmType type) {
		// TODO: Is a utility class from Xbase providing this feature that is different from LightweightTypeReference.
		return type == null || (type.eClass() == TypesPackage.Literals.JVM_VOID && !type.eIsProxy());
	}

	/** Replies if the given type is a primitive type.
	 *
	 * @param type the type to test.
	 * @return <code>true</code> if the type is primitive.
	 */
	public static boolean isPrimitive(JvmType type) {
		// TODO: Is a utility class from Xbase providing this feature that is different from LightweightTypeReference.
		return type != null && type.eClass() == TypesPackage.Literals.JVM_PRIMITIVE_TYPE;
	}

	/** Replies if the given type is a functional interface.
	 *
	 * <p>This function does not test if the {@code @FunctionalInterface} is attached to the type.
	 * The function counts the number of operations.
	 *
	 * @param type the type to test.
	 * @param sarlSignatureProvider the provider of SARL operation signatures.
	 * @return <code>true</code> if the given type is final.
	 */
	public static boolean isFunctionalInterface(JvmGenericType type, IActionPrototypeProvider sarlSignatureProvider) {
		if (type != null && type.isInterface()) {
			final Map<ActionPrototype, JvmOperation> operations = new HashMap<>();
			populateInterfaceElements(type, operations, null, sarlSignatureProvider);
			return operations.size() == 1;
		}
		return false;
	}

}
