/*
 * $Id$
 *
 * SARL is an general-purpose agent programming language.
 * More details on http://www.sarl.io
 *
 * Copyright (C) 2014-2015 Sebastian RODRIGUEZ, Nicolas GAUD, Stéphane GALLAND.
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

import io.sarl.lang.actionprototype.ActionParameterTypes;
import io.sarl.lang.actionprototype.ActionPrototype;
import io.sarl.lang.actionprototype.ActionPrototypeProvider;
import io.sarl.lang.sarl.SarlAction;
import io.sarl.lang.sarl.SarlFormalParameter;
import io.sarl.lang.services.SARLGrammarAccess;
import io.sarl.lang.services.SARLGrammarAccess.ActionElements;
import io.sarl.lang.services.SARLGrammarAccess.ParameterElements;

import java.lang.reflect.Modifier;
import java.security.MessageDigest;
import java.security.NoSuchAlgorithmException;
import java.util.ArrayList;
import java.util.BitSet;
import java.util.Collections;
import java.util.List;
import java.util.Map;
import java.util.SortedSet;

import org.eclipse.xtend.core.xtend.XtendParameter;
import org.eclipse.xtext.common.types.JvmAnnotationReference;
import org.eclipse.xtext.common.types.JvmAnnotationTarget;
import org.eclipse.xtext.common.types.JvmAnnotationType;
import org.eclipse.xtext.common.types.JvmAnnotationValue;
import org.eclipse.xtext.common.types.JvmConstructor;
import org.eclipse.xtext.common.types.JvmDeclaredType;
import org.eclipse.xtext.common.types.JvmFeature;
import org.eclipse.xtext.common.types.JvmField;
import org.eclipse.xtext.common.types.JvmFormalParameter;
import org.eclipse.xtext.common.types.JvmGenericType;
import org.eclipse.xtext.common.types.JvmIdentifiableElement;
import org.eclipse.xtext.common.types.JvmMember;
import org.eclipse.xtext.common.types.JvmOperation;
import org.eclipse.xtext.common.types.JvmStringAnnotationValue;
import org.eclipse.xtext.common.types.JvmType;
import org.eclipse.xtext.common.types.JvmTypeAnnotationValue;
import org.eclipse.xtext.common.types.JvmTypeParameter;
import org.eclipse.xtext.common.types.JvmTypeReference;
import org.eclipse.xtext.common.types.JvmVisibility;
import org.eclipse.xtext.naming.QualifiedName;
import org.eclipse.xtext.serializer.ISerializer;
import org.eclipse.xtext.xbase.XExpression;
import org.eclipse.xtext.xbase.annotations.services.XbaseWithAnnotationsGrammarAccess.XAnnotationElements;
import org.eclipse.xtext.xbase.annotations.xAnnotations.XAnnotation;
import org.eclipse.xtext.xbase.annotations.xAnnotations.XAnnotationElementValuePair;
import org.eclipse.xtext.xbase.compiler.ImportManager;
import org.eclipse.xtext.xbase.lib.CollectionLiterals;
import org.eclipse.xtext.xbase.typesystem.conformance.TypeConformanceComputationArgument;
import org.eclipse.xtext.xbase.typesystem.references.LightweightTypeReference;
import org.eclipse.xtext.xbase.typesystem.references.LightweightTypeReferenceFactory;
import org.eclipse.xtext.xbase.typesystem.references.StandardTypeReferenceOwner;
import org.eclipse.xtext.xbase.typesystem.util.CommonTypeComputationServices;

import com.google.common.base.Strings;
import com.ibm.icu.util.VersionInfo;

/**
 * Utilities functions on JvmElements.
 *
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 */
public final class Utils {

	/** Prefix for the names of the hidden behavior units.
	 */
	public static final String PREFIX_ACTION_HANDLE = "_handle_"; //$NON-NLS-1$

	/** Prefix for the names of the hidden behavior units.
	 */
	public static final String PREFIX_HANDLE_GUARD = "_eventhandler_guard_"; //$NON-NLS-1$

	/** Prefix for the names of the hidden behavior units.
	 */
	public static final String PREFIX_HANDLE_BODY = "_eventhandler_body_"; //$NON-NLS-1$

	/** Préfix for the names of the hidden fields related to the default values.
	 */
	public static final String PREFIX_ATTRIBUTE_DEFAULT_VALUE = "___FORMAL_PARAMETER_DEFAULT_VALUE_"; //$NON-NLS-1$

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
			JvmGenericType jvmElement,
			Map<ActionPrototype, JvmOperation> operations,
			Map<String, JvmField> fields,
			ActionPrototypeProvider sarlSignatureProvider) {
		for (JvmFeature feature : jvmElement.getAllFeatures()) {
			if (!"java.lang.Object".equals(feature.getDeclaringType().getQualifiedName())) { //$NON-NLS-1$
				if (operations != null && feature instanceof JvmOperation) {
					JvmOperation operation = (JvmOperation) feature;
					ActionParameterTypes sig = sarlSignatureProvider.createParameterTypesFromJvmModel(
							operation.isVarArgs(), operation.getParameters());
					ActionPrototype actionKey = sarlSignatureProvider.createActionPrototype(
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
	public static void populateInheritanceContext(
			JvmGenericType jvmElement,
			Map<ActionPrototype, JvmOperation> finalOperations,
			Map<ActionPrototype, JvmOperation> overridableOperations,
			Map<String, JvmField> inheritedFields,
			Map<ActionPrototype, JvmOperation> operationsToImplement,
			Map<ActionParameterTypes, JvmConstructor> superConstructors,
			ActionPrototypeProvider sarlSignatureProvider) {
		// Get the operations that must be implemented
		if (operationsToImplement != null) {
			for (JvmTypeReference interfaceReference : jvmElement.getExtendedInterfaces()) {
				for (JvmFeature feature : ((JvmGenericType) interfaceReference.getType()).getAllFeatures()) {
					if (!"java.lang.Object".equals(//$NON-NLS-1$
							feature.getDeclaringType().getQualifiedName())) {
						if (feature instanceof JvmOperation) {
							JvmOperation operation = (JvmOperation) feature;
							ActionParameterTypes sig = sarlSignatureProvider.createParameterTypesFromJvmModel(
									operation.isVarArgs(), operation.getParameters());
							ActionPrototype actionKey = sarlSignatureProvider.createActionPrototype(
									operation.getSimpleName(), sig);
							operationsToImplement.put(actionKey, operation);
						}
					}
				}
			}
		}

		// Check on the implemented features, inherited from the super type
		if (jvmElement.getExtendedClass() != null) {
			JvmGenericType parentType = (JvmGenericType) jvmElement.getExtendedClass().getType();
			for (JvmFeature feature : parentType.getAllFeatures()) {
				if (!"java.lang.Object".equals(feature.getDeclaringType().getQualifiedName()) //$NON-NLS-1$
						&& isVisible(jvmElement, feature)
						&& !isHiddenAction(feature.getSimpleName())) {
					if (feature instanceof JvmOperation) {
						if (!feature.isStatic()) {
							JvmOperation operation = (JvmOperation) feature;
							ActionParameterTypes sig = sarlSignatureProvider.createParameterTypesFromJvmModel(
									operation.isVarArgs(), operation.getParameters());
							ActionPrototype actionKey = sarlSignatureProvider.createActionPrototype(
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
				for (JvmConstructor cons : parentType.getDeclaredConstructors()) {
					ActionParameterTypes sig = sarlSignatureProvider.createParameterTypesFromJvmModel(
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
		return params.size() > 0
				&& params.get(params.size() - 1).isVarArg();
	}

	/** Replies if the given name is related to an hidden action.
	 * <p>
	 * An hidden action is an action that is generated by the SARL
	 * compiler, and that cannot be defined by the SARL user.
	 *
	 * @param name - the name to test.
	 * @return <code>true</code> if the given name is reserved by
	 * SARL.
	 */
	public static boolean isHiddenAction(String name) {
		return name.startsWith(PREFIX_ACTION_HANDLE) || name.startsWith(PREFIX_HANDLE_GUARD)
				 || name.startsWith(PREFIX_HANDLE_BODY);
	}

	/** Replies a fixed version of the given name assuming
	 * that it is an hidden action, and reformating
	 * the reserved text.
	 * <p>
	 * An hidden action is an action that is generated by the SARL
	 * compiler, and that cannot be defined by the SARL user.
	 *
	 * @param name - the name to fix.
	 * @return the fixed name.
	 * @see #removeHiddenAction(String)
	 */
	public static String fixHiddenAction(String name) {
		if (name.startsWith(PREFIX_ACTION_HANDLE)) {
			if (name.length() > PREFIX_ACTION_HANDLE.length()) {
				return "handle" + name.substring(//$NON-NLS-1$
						PREFIX_ACTION_HANDLE.length(),
						PREFIX_ACTION_HANDLE.length() + 1).toUpperCase()
						+ name.substring(PREFIX_ACTION_HANDLE.length() + 1);
			}
			return "handle"; //$NON-NLS-1$
		}
		if (name.startsWith(PREFIX_HANDLE_GUARD)) {
			if (name.length() > PREFIX_HANDLE_GUARD.length()) {
				return "guard" + name.substring(//$NON-NLS-1$
						PREFIX_HANDLE_GUARD.length(),
						PREFIX_HANDLE_GUARD.length() + 1).toUpperCase()
						+ name.substring(PREFIX_HANDLE_GUARD.length() + 1);
			}
			return "guard"; //$NON-NLS-1$
		}
		if (name.startsWith(PREFIX_HANDLE_BODY)) {
			if (name.length() > PREFIX_HANDLE_BODY.length()) {
				return "handlebody" + name.substring(//$NON-NLS-1$
						PREFIX_HANDLE_BODY.length(),
						PREFIX_HANDLE_BODY.length() + 1).toUpperCase()
						+ name.substring(PREFIX_HANDLE_BODY.length() + 1);
			}
			return "guard"; //$NON-NLS-1$
		}
		return name;
	}

	/** Replies a fixed version of the given name assuming
	 * that it is an hidden action, and removing the reserved prefix.
	 * <p>
	 * An hidden action is an action that is generated by the SARL
	 * compiler, and that cannot be defined by the SARL user.
	 *
	 * @param name - the name to fix.
	 * @return the fixed name.
	 * @see #fixHiddenAction(String)
	 */
	public static String removeHiddenAction(String name) {
		if (name.startsWith(PREFIX_ACTION_HANDLE)) {
			if (name.length() > PREFIX_ACTION_HANDLE.length()) {
				return name.substring(
						PREFIX_ACTION_HANDLE.length(),
						PREFIX_ACTION_HANDLE.length() + 1).toLowerCase()
						+ name.substring(PREFIX_ACTION_HANDLE.length() + 1);
			}
			return "handle"; //$NON-NLS-1$
		}
		if (name.startsWith(PREFIX_HANDLE_GUARD)) {
			if (name.length() > PREFIX_HANDLE_GUARD.length()) {
				return name.substring(
						PREFIX_HANDLE_GUARD.length(),
						PREFIX_HANDLE_GUARD.length() + 1).toLowerCase()
						+ name.substring(PREFIX_HANDLE_GUARD.length() + 1);
			}
			return "guard"; //$NON-NLS-1$
		}
		if (name.startsWith(PREFIX_HANDLE_BODY)) {
			if (name.length() > PREFIX_HANDLE_BODY.length()) {
				return name.substring(
						PREFIX_HANDLE_BODY.length(),
						PREFIX_HANDLE_BODY.length() + 1).toLowerCase()
						+ name.substring(PREFIX_HANDLE_BODY.length() + 1);
			}
			return "handlebody"; //$NON-NLS-1$
		}
		return name;
	}

	/** Replies if the given name is related to an hidden attribute.
	 * <p>
	 * An hidden attribute is an attribute that is generated by the SARL
	 * compiler, and that cannot be defined by the SARL user.
	 *
	 * @param name - the name to test.
	 * @return <code>true</code> if the given name is reserved by
	 * SARL.
	 */
	public static boolean isHiddenAttribute(String name) {
		return name.startsWith(PREFIX_ATTRIBUTE_DEFAULT_VALUE);
	}

	/** Replies a fixed version of the given name assuming
	 * that it is an hidden attribute, and reformat the reserved
	 * prefix.
	 * <p>
	 * An hidden attribute is an attribute that is generated by the SARL
	 * compiler, and that cannot be defined by the SARL user.
	 *
	 * @param name - the name to fix.
	 * @return the fixed name.
	 */
	public static String fixHiddenAttribute(String name) {
		if (name.startsWith(PREFIX_ATTRIBUTE_DEFAULT_VALUE)) {
			if (name.length() > PREFIX_ATTRIBUTE_DEFAULT_VALUE.length()) {
				return name.substring(
						PREFIX_ATTRIBUTE_DEFAULT_VALUE.length(),
						PREFIX_ATTRIBUTE_DEFAULT_VALUE.length() + 1).toLowerCase()
						+ name.substring(PREFIX_ATTRIBUTE_DEFAULT_VALUE.length() + 1);
			}
			return "attr";  //$NON-NLS-1$
		}
		return name;
	}

	/** Replies if the given reference is pointing to a class type.
	 *
	 * @param typeRef - the type reference to test.
	 * @return <code>true</code> if the pointed element is a class type.
	 */
	public static boolean isClass(LightweightTypeReference typeRef) {
		JvmType t = typeRef.getType();
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
	public static boolean canCast(
			LightweightTypeReference fromType, LightweightTypeReference toType,
			boolean enablePrimitiveWidening, boolean enableVoidMatchingNull,
			boolean allowSynonyms) {
		if (enableVoidMatchingNull) {
			boolean fromVoid = (fromType == null) || (fromType.isPrimitiveVoid());
			boolean toVoid = (toType == null) || (toType.isPrimitiveVoid());
			if (fromVoid) {
				return toVoid;
			}
			if (toVoid) {
				return fromVoid;
			}
			assert (fromType != null);
			assert (toType != null);
		} else if ((fromType == null || toType == null)
				|| (fromType.isPrimitiveVoid() != toType.isPrimitiveVoid())) {
			return false;
		}
		TypeConformanceComputationArgument conform = new TypeConformanceComputationArgument(
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
		StandardTypeReferenceOwner owner = new StandardTypeReferenceOwner(services, typeRef);
		LightweightTypeReferenceFactory factory = new LightweightTypeReferenceFactory(owner, keepUnboundWildcardInformation);
		LightweightTypeReference reference = factory.toLightweightReference(typeRef);
		return reference;
	}

	/** Extract the string value of the given annotation, if it exists.
	 *
	 * @param op - the annoted element.
	 * @param annotationType - the type of the annotation to consider
	 * @return the value of the annotation, or <code>null</code> if no annotation or no
	 * value.
	 */
	public static String annotationString(JvmAnnotationTarget op, Class<?> annotationType) {
		String n = annotationType.getName();
		for (JvmAnnotationReference aref : op.getAnnotations()) {
			JvmAnnotationType an = aref.getAnnotation();
			if (n != null && n.equals(an.getQualifiedName())) {
				for (JvmAnnotationValue value : aref.getValues()) {
					if (value instanceof JvmStringAnnotationValue) {
						for (String sValue : ((JvmStringAnnotationValue) value).getValues()) {
							if (sValue != null) {
								return sValue;
							}
						}
					}
				}
			}
		}
		return null;
	}

	/** Extract the string values of the given annotation, if they exist.
	 *
	 * @param op - the annoted element.
	 * @param annotationType - the type of the annotation to consider
	 * @return the values of the annotation, never <code>null</code>.
	 */
	public static List<String> annotationStrings(JvmAnnotationTarget op, Class<?> annotationType) {
		List<String> values = new ArrayList<>();
		String n = annotationType.getName();
		for (JvmAnnotationReference aref : op.getAnnotations()) {
			JvmAnnotationType an = aref.getAnnotation();
			if (n != null && n.equals(an.getQualifiedName())) {
				for (JvmAnnotationValue value : aref.getValues()) {
					if (value instanceof JvmStringAnnotationValue) {
						for (String sValue : ((JvmStringAnnotationValue) value).getValues()) {
							if (sValue != null) {
								values.add(sValue);
							}
						}
					}
				}
			}
		}
		return values;
	}

	/** Extract the type values of the given annotation, if they exist.
	 *
	 * @param op - the annoted element.
	 * @param annotationType - the type of the annotation to consider
	 * @return the values of the annotation, never <code>null</code>.
	 */
	public static List<JvmTypeReference> annotationClasses(JvmAnnotationTarget op, Class<?> annotationType) {
		List<JvmTypeReference> values = new ArrayList<>();
		String n = annotationType.getName();
		for (JvmAnnotationReference aref : op.getAnnotations()) {
			JvmAnnotationType an = aref.getAnnotation();
			if (n != null && n.equals(an.getQualifiedName())) {
				for (JvmAnnotationValue value : aref.getValues()) {
					if (value instanceof JvmTypeAnnotationValue) {
						for (JvmTypeReference sValue : ((JvmTypeAnnotationValue) value).getValues()) {
							if (sValue != null) {
								values.add(sValue);
							}
						}
					}
				}
			}
		}
		return values;
	}

	/** Replies if the given target is annotated.
	 *
	 * @param op - the annoted element.
	 * @param annotationType - the type of the annotation to consider
	 * @return <code>true</code> if the op is annotated with the given annotation type,
	 * otherwise <code>false</code>.
	 */
	public static boolean hasAnnotation(JvmAnnotationTarget op, Class<?> annotationType) {
		String n = annotationType.getName();
		for (JvmAnnotationReference aref : op.getAnnotations()) {
			JvmAnnotationType an = aref.getAnnotation();
			if (n != null && n.equals(an.getQualifiedName())) {
				return true;
			}
		}
		return false;
	}

	/** Compare the two strings as they are version numbers.
	 *
	 * @param v1 - first version to compare.
	 * @param v2 - second version to compare.
	 * @return Negative integer of <code>v1</code> is lower than <code>v2</code>;
	 * positive integer of <code>v1</code> is greater than <code>v2</code>;
	 * {@code 0} if they are strictly equal.
	 */
	public static int compareVersions(String v1, String v2) {
		VersionInfo vi1 = VersionInfo.getInstance(v1);
		VersionInfo vi2 = VersionInfo.getInstance(v2);
		return vi1.compareTo(vi2);
	}

	/** Replies the default value for the given type.
	 *
	 * @param type - the type for which the default value should be determined.
	 * @return the default value.
	 */
	public static String getDefaultValueForType(LightweightTypeReference type) {
		if (type != null) {
			return getDefaultValueForType(type.getIdentifier());
		}
		return ""; //$NON-NLS-1$
	}

	/** Replies the default value for the given type.
	 *
	 * @param type - the type for which the default value should be determined.
	 * @return the default value.
	 */
	public static String getDefaultValueForType(String type) {
		//TODO: Check if a similar function exists in the Xbase library.
		String defaultValue = ""; //$NON-NLS-1$
		if (!Strings.isNullOrEmpty(type) && !"void".equals(type)) { //$NON-NLS-1$
			switch (type) {
			case "boolean":  //$NON-NLS-1$
				defaultValue = "true"; //$NON-NLS-1$
				break;
			case "double":  //$NON-NLS-1$
				defaultValue = "0.0"; //$NON-NLS-1$
				break;
			case "float":  //$NON-NLS-1$
				defaultValue = "0.0f"; //$NON-NLS-1$
				break;
			case "int":  //$NON-NLS-1$
				defaultValue = "0"; //$NON-NLS-1$
				break;
			case "long":  //$NON-NLS-1$
				defaultValue = "0"; //$NON-NLS-1$
				break;
			case "byte": //$NON-NLS-1$
				defaultValue = "(0 as byte)"; //$NON-NLS-1$
				break;
			case "short":  //$NON-NLS-1$
				defaultValue = "(0 as short)"; //$NON-NLS-1$
				break;
			case "char":  //$NON-NLS-1$
				defaultValue = "(0 as char)"; //$NON-NLS-1$
				break;
			default:
				defaultValue = "null"; //$NON-NLS-1$
			}
		}
		return defaultValue;
	}

	private static void addAnnotationToSignature(StringBuilder textRepresentation, SARLGrammarAccess elements,
			ISerializer serializer, ImportManager importManager, XAnnotation annotation) {
		XAnnotationElements annotationElements = elements.getXAnnotationAccess();
		textRepresentation.append(annotationElements.getCommercialAtKeyword_1());
		textRepresentation.append(getSignatureType(annotation.getAnnotationType(), importManager));
		XExpression value = annotation.getValue();
		if (value != null) {
			textRepresentation.append(annotationElements.getLeftParenthesisKeyword_3_0().getValue());
			textRepresentation.append(serializer.serialize(value).trim());
			textRepresentation.append(annotationElements.getRightParenthesisKeyword_3_2().getValue());
		} else if (!annotation.getElementValuePairs().isEmpty()){
			textRepresentation.append(annotationElements.getLeftParenthesisKeyword_3_0().getValue());
			boolean addComa = false;
			for (XAnnotationElementValuePair pair : annotation.getElementValuePairs()) {
				if (addComa) {
					textRepresentation.append(annotationElements.getCommaKeyword_3_1_0_1_0().getValue());
				} else {
					addComa = true;
				}
				textRepresentation.append(elements.getXAnnotationElementValuePairAccess()
						.getEqualsSignKeyword_0_0_1().getValue());
				textRepresentation.append(serializer.serialize(pair.getValue()).trim());
			}
			textRepresentation.append(annotationElements.getRightParenthesisKeyword_3_2());
		}
	}

	/** This is a context-safe serializer of a signature.
	 *
	 * @param signature - the signature to serialize.
	 * @param serializer - the Xtext serializer
	 * @param grammarAccess - the accessor to the SARL grammar.
	 * @param importManager - used to collect the types to import.
	 * If <code>null</code>, the qualified names of the types with be put in the signature.
	 * @return the string representation of the signature.
	 */
	public static String getActionSignatureString(SarlAction signature, ISerializer serializer,
			SARLGrammarAccess grammarAccess, ImportManager importManager) {
		// Try the serializer
		try {
			//TODO: Check if there is a way to serialize without context
			return serializer.serialize(signature);
		} catch (Throwable _) {
			// No working, perhaps the context's of the signature is unknown
		}
		ActionElements signatureElements = grammarAccess.getActionAccess();
		StringBuilder textRepresentation = new StringBuilder();
		// Annotations
		for(XAnnotation annotation : signature.getAnnotations()) {
			addAnnotationToSignature(textRepresentation, grammarAccess, serializer, importManager, annotation);
		}
		// Modifiers
		for(String modifier : signature.getModifiers()) {
			textRepresentation.append(modifier);
			textRepresentation.append(' ');
		}
		// Generic type
		if (!signature.getTypeParameters().isEmpty()) {
			boolean addComa = false;
			textRepresentation.append(signatureElements.getLessThanSignKeyword_5_0().getValue());
			for(JvmTypeParameter typeParameter : signature.getTypeParameters()) {
				if (addComa) {
					textRepresentation.append(signatureElements.getCommaKeyword_5_2_0().getValue());
					textRepresentation.append(' ');
				} else {
					addComa = true;
				}
				textRepresentation.append(getSignatureType(typeParameter, importManager));
			}
			textRepresentation.append(signatureElements.getLessThanSignKeyword_5_0().getValue());
			textRepresentation.append(' ');
		}
		// Name
		textRepresentation.append(signature.getName());
		// Parameters
		if (!signature.getParameters().isEmpty()) {
			textRepresentation.append(signatureElements.getLeftParenthesisKeyword_7_0().getValue());
			int idx = signature.getParameters().size() - 1;
			for (int i = 0; i < idx; ++i) {
				addParamToSignature(textRepresentation, signature.getParameters().get(i), grammarAccess,
						importManager, serializer);
				textRepresentation.append(signatureElements.getCommaKeyword_9_0_2_0().getValue());
				textRepresentation.append(' ');
			}
			addParamToSignature(textRepresentation, signature.getParameters().get(idx), grammarAccess,
					importManager, serializer);
			textRepresentation.append(signatureElements.getRightParenthesisKeyword_7_2().getValue());
		}
		// Return type
		JvmTypeReference returnType = signature.getReturnType();
		if (returnType != null && !"void".equals(returnType.getIdentifier())) { //$NON-NLS-1$
			textRepresentation.append(' ');
			textRepresentation.append(signatureElements.getColonKeyword_8_0().getValue());
			textRepresentation.append(' ');
			textRepresentation.append(getSignatureType(returnType.getType(), importManager));
		}
		// Throws
		if (!signature.getExceptions().isEmpty()) {
			textRepresentation.append(' ');
			textRepresentation.append(signatureElements.getThrowsKeyword_9_0_0().getValue());
			textRepresentation.append(' ');
			boolean addComa = false;
			for (JvmTypeReference eventType : signature.getExceptions()) {
				if (addComa) {
					textRepresentation.append(signatureElements.getCommaKeyword_9_0_2_0().getValue());
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
			textRepresentation.append(signatureElements.getFiresKeyword_9_1_0().getValue());
			textRepresentation.append(' ');
			boolean addComa = false;
			for (JvmTypeReference eventType : signature.getFiredEvents()) {
				if (addComa) {
					textRepresentation.append(signatureElements.getCommaKeyword_9_1_2_0().getValue());
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
			SARLGrammarAccess grammarAccess, ImportManager importManager, ISerializer serializer) {
		ParameterElements elements = grammarAccess.getParameterAccess();
		signature.append(parameter.getName());
		signature.append(' ');
		signature.append(elements.getColonKeyword_3().getValue());
		signature.append(' ');
		signature.append(getSignatureType(parameter.getParameterType().getType(), importManager));
		if (parameter.isVarArg()) {
			signature.append(grammarAccess.getVarArgTokenAccess().getAsteriskKeyword().getValue());
		} else if (parameter instanceof SarlFormalParameter) {
			SarlFormalParameter sarlParameter = (SarlFormalParameter) parameter;
			if (sarlParameter.getDefaultValue() != null) {
				signature.append(' ');
				signature.append(elements.getEqualsSignKeyword_5_1_0().getValue());
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
		StringBuilder serialVersionUIDBuffer = new StringBuilder();

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

		SortedSet<JvmTypeReference> superTypes = CollectionLiterals.newTreeSet(new JvmTypeReferenceComparator());
		superTypes.addAll(jvm.getSuperTypes());

		SortedSet<JvmField> fields = CollectionLiterals.newTreeSet(new JvmIdentifiableComparator());
		SortedSet<JvmConstructor> constructors = CollectionLiterals.newTreeSet(new JvmIdentifiableComparator());
		SortedSet<JvmOperation> operations = CollectionLiterals.newTreeSet(new JvmIdentifiableComparator());
		for (JvmMember member : jvm.getMembers()) {
			if (member instanceof JvmField) {
				JvmField field = (JvmField) member;
				if ((field.getVisibility() != JvmVisibility.PRIVATE)
						|| (!field.isStatic() && !field.isTransient())) {
					fields.add(field);
				}
			} else if (member instanceof JvmConstructor) {
				JvmConstructor constructor = (JvmConstructor) member;
				if (constructor.getVisibility() != JvmVisibility.PRIVATE) {
					constructors.add(constructor);
				}
			} else if (member instanceof JvmOperation) {
				JvmOperation operation = (JvmOperation) member;
				if (operation.getVisibility() != JvmVisibility.PRIVATE) {
					operations.add(operation);
				}
			}
		}

		for (JvmTypeReference superType : superTypes) {
			serialVersionUIDBuffer.append(superType.getQualifiedName());
		}

		for (JvmField field : fields) {
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

		for (JvmConstructor constructor : constructors) {
			bitset = new BitSet(32);
			bitset.set(constructor.getVisibility().getValue());
			if (constructor.isStatic()) {
				bitset.set(4);
			}
			if (constructor.isVarArgs()) {
				bitset.set(5);
			}
			serialVersionUIDBuffer.append(bitset.toByteArray());
			for (JvmFormalParameter parameter : constructor.getParameters()) {
				serialVersionUIDBuffer.append(parameter.getParameterType().getIdentifier());
			}
		}

		for (JvmOperation operation : operations) {
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
			for (JvmFormalParameter parameter : operation.getParameters()) {
				serialVersionUIDBuffer.append(parameter.getParameterType().getIdentifier());
			}
		}

		long key = 1L;
		try {
			byte[] uniqueKey = serialVersionUIDBuffer.toString().getBytes();
			byte[] sha = MessageDigest.getInstance("SHA").digest(uniqueKey); //$NON-NLS-1$
			key = ((sha[0] >>> 24) & 0xFF) |
					((sha[0] >>> 16) & 0xFF) << 8 |
					((sha[0] >>> 8) & 0xFF) << 16 |
					((sha[0] >>> 0) & 0xFF) << 24 |
					((sha[1] >>> 24) & 0xFF) << 32 |
					((sha[1] >>> 16) & 0xFF) << 40 |
					((sha[1] >>> 8) & 0xFF) << 48 |
					((sha[1] >>> 0) & 0xFF) << 56;		
		} catch (NoSuchAlgorithmException e) {
			// 
		}

		return key;
	}

	/** Replies a singleton list with the given element, or the empty list if
	 * the element is <code>null</code>.
	 *
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

}
