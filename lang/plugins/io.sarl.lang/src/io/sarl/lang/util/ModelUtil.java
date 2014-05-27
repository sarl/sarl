/*
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
package io.sarl.lang.util;

import io.sarl.lang.signature.ActionKey;
import io.sarl.lang.signature.ActionSignatureProvider;
import io.sarl.lang.signature.SignatureKey;

import java.util.Map;

import org.eclipse.emf.ecore.EObject;
import org.eclipse.xtext.common.types.JvmConstructor;
import org.eclipse.xtext.common.types.JvmDeclaredType;
import org.eclipse.xtext.common.types.JvmFeature;
import org.eclipse.xtext.common.types.JvmField;
import org.eclipse.xtext.common.types.JvmGenericType;
import org.eclipse.xtext.common.types.JvmMember;
import org.eclipse.xtext.common.types.JvmOperation;
import org.eclipse.xtext.common.types.JvmType;
import org.eclipse.xtext.common.types.JvmTypeReference;
import org.eclipse.xtext.xbase.jvmmodel.IJvmModelAssociations;
import org.eclipse.xtext.xbase.typesystem.conformance.TypeConformanceComputationArgument;
import org.eclipse.xtext.xbase.typesystem.references.LightweightTypeReference;

/**
 * Utilities functions on JvmElements.
 * 
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 */
public class ModelUtil {

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
			Map<ActionKey,JvmOperation> finalOperations,
			Map<ActionKey,JvmOperation> overridableOperations,
			Map<String,JvmField> inheritedFields,
			Map<ActionKey,JvmOperation> operationsToImplement,
			Map<SignatureKey,JvmConstructor> superConstructors,
			ActionSignatureProvider sarlSignatureProvider) {

		// Get the operations that must be implemented
		if (operationsToImplement!=null) {
			for(JvmTypeReference interfaceReference : jvmElement.getExtendedInterfaces()) {
				for(JvmFeature feature : ((JvmGenericType)interfaceReference.getType()).getAllFeatures()) {
					if (!"java.lang.Object".equals(feature.getDeclaringType().getQualifiedName())) { //$NON-NLS-1$
						if (feature instanceof JvmOperation) {
							JvmOperation operation = (JvmOperation)feature;
							SignatureKey sig = sarlSignatureProvider.createSignatureIDFromJvmModel(operation.isVarArgs(), operation.getParameters());
							ActionKey actionKey = sarlSignatureProvider.createActionID(operation.getSimpleName(), sig);
							operationsToImplement.put(actionKey, operation);
						}
					}
				}
			}
		}

		// Check on the implemented features, inherited from the super type			
		if (jvmElement.getExtendedClass()!=null) {
			JvmGenericType parentType = (JvmGenericType)jvmElement.getExtendedClass().getType();
			for(JvmFeature feature : parentType.getAllFeatures()) {
				if (!"java.lang.Object".equals(feature.getDeclaringType().getQualifiedName()) //$NON-NLS-1$
						&& isVisible(jvmElement, feature)
						&& !isHiddenAction(feature.getSimpleName())) {
					if (feature instanceof JvmOperation) {
						if (!feature.isStatic()) {
							JvmOperation operation = (JvmOperation)feature;
							SignatureKey sig = sarlSignatureProvider.createSignatureIDFromJvmModel(operation.isVarArgs(), operation.getParameters());
							ActionKey actionKey = sarlSignatureProvider.createActionID(feature.getSimpleName(), sig);
							if (operation.isAbstract()) {
								if (operationsToImplement!=null) operationsToImplement.put(actionKey, operation);
							}
							else if (operation.isFinal()) {
								if (finalOperations!=null) finalOperations.put(actionKey, operation);
								if (operationsToImplement!=null) operationsToImplement.remove(actionKey);
							}
							else {
								if (overridableOperations!=null) overridableOperations.put(actionKey, operation);
								if (operationsToImplement!=null) operationsToImplement.remove(actionKey);
							}
						} 
					}
					else if (feature instanceof JvmField && inheritedFields!=null) {
						inheritedFields.put(feature.getSimpleName(), (JvmField)feature);
					}
				}
			}
			
			if (superConstructors!=null) {
				for(JvmConstructor cons : parentType.getDeclaredConstructors()) {
					SignatureKey sig = sarlSignatureProvider.createSignatureIDFromJvmModel(cons.isVarArgs(), cons.getParameters());
					superConstructors.put(sig,  cons);
				}
			}
		}
	}

	/** Replies if the target feature is visible from the type.
	 * 
	 * @param fromType
	 * @param target
	 * @return <code>true</code> if the given type can see the target feature.
	 */
	public static boolean isVisible(JvmDeclaredType fromType, JvmMember target) {
		switch(target.getVisibility()) {
		case DEFAULT: {
			return target.getDeclaringType().getPackageName().equals(fromType.getPackageName());
		}
		case PROTECTED:
		case PUBLIC:
			return true;
		case PRIVATE:
		default:
		}
		return false;
	}

	/** Replies if the given name is related to an hidden action.
	 * <p>
	 * An hidden action is an action that is generated by the SARL
	 * compiler, and that cannot be defined by the SARL user.
	 * 
	 * @param name
	 * @return <code>true</code> if the given name is reserved by
	 * SARL.
	 */
	public static boolean isHiddenAction(String name) {
		return name.startsWith("_handle_"); //$NON-NLS-1$
	}

	/** Replies if the given name is related to an hidden attribute.
	 * <p>
	 * An hidden attribute is an attribute that is generated by the SARL
	 * compiler, and that cannot be defined by the SARL user.
	 * 
	 * @param name
	 * @return <code>true</code> if the given name is reserved by
	 * SARL.
	 */
	public static boolean isHiddenAttribute(String name) {
		return name.startsWith("___FORMAL_PARAMETER_DEFAULT_VALUE_"); //$NON-NLS-1$
	}

	/** Replies the JVM generic type of the given object.
	 * 
	 * @param element
	 * @param associations - manager of the associations between the SARL objects and the JVM objects.
	 * @return the generic type, or <code>null</code> if none.
	 */
	public static JvmGenericType getJvmGenericType(EObject element, IJvmModelAssociations associations) {
		for(EObject obj : associations.getJvmElements(element)) {
			if (obj instanceof JvmGenericType) {
				return (JvmGenericType)obj;
			}
		}
		return null;
	}

	/** Replies if the given reference is pointing to a class type.
	 * 
	 * @param typeRef
	 * @return <code>true</code> if the pointed element is a class type.
	 */
	public static boolean isClass(LightweightTypeReference typeRef) {
		JvmType t = typeRef.getType();
		if (t instanceof JvmGenericType) {
			return !((JvmGenericType)t).isInterface();
		}
		return false;
	}	

	/** Replies if the given reference is referencing a final type.
	 * 
	 * @param expressionTypeRef
	 * @return <code>true</code> if the given type is final.
	 */
	public static boolean isFinal(LightweightTypeReference expressionTypeRef) {
		if (expressionTypeRef.isArray()) {
			return isFinal(expressionTypeRef.getComponentType());
		}
		if (expressionTypeRef.isPrimitive())
			return true;
		return expressionTypeRef.getType() instanceof JvmDeclaredType
				&& ((JvmDeclaredType) expressionTypeRef.getType()).isFinal();
	}

	/** Replies if the given type is an interface.
	 * 
	 * @param type
	 * @return <code>true</code> if the given type is an interface.
	 */
	public static boolean isInterface(LightweightTypeReference type) {
		return type.getType() instanceof JvmGenericType && ((JvmGenericType)type.getType()).isInterface();
	}

	/** Replies if it is allowed to cast between the given types.
	 * 
	 * @param fromType - source type
	 * @param toType - target type
	 * @param enablePrimitiveWidening - indicates if the widening of the primitive types is allowed.
	 * @return the state of the cast.
	 */
	public static boolean canCast(LightweightTypeReference fromType, LightweightTypeReference toType, boolean enablePrimitiveWidening) {
		if (fromType==null || toType==null) return false;
		if (fromType.isPrimitiveVoid()!=toType.isPrimitiveVoid()) {
			return false;
		}
		TypeConformanceComputationArgument conform = new TypeConformanceComputationArgument(
				false, false, true, enablePrimitiveWidening, false, true);
		if (fromType.getType() instanceof JvmDeclaredType || fromType.isPrimitive()) {
			// if one of the types is an interface and the other is a non final class (or interface) there always can be a subtype
			if ((!isInterface(fromType) || isFinal(toType)) && (!isInterface(toType) || isFinal(fromType))) { 
				if (!toType.isAssignableFrom(fromType, conform)) {
					if (   isFinal(fromType) || isFinal(toType)
							|| isClass(fromType) && isClass(toType)) {
						if (!fromType.isAssignableFrom(toType, conform)) { // no upcast
							return false;
						}
					}
				}
			}
		}
		if(toType.isPrimitive() && !(fromType.isPrimitive() || fromType.isWrapper())) {
			return false;
		}
		return true;
	}
		
}
