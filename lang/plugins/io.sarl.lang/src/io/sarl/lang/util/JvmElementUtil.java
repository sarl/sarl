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
import org.eclipse.xtext.common.types.JvmDeclaredType;
import org.eclipse.xtext.common.types.JvmFeature;
import org.eclipse.xtext.common.types.JvmField;
import org.eclipse.xtext.common.types.JvmGenericType;
import org.eclipse.xtext.common.types.JvmMember;
import org.eclipse.xtext.common.types.JvmOperation;
import org.eclipse.xtext.common.types.JvmType;
import org.eclipse.xtext.common.types.JvmTypeReference;
import org.eclipse.xtext.xbase.jvmmodel.IJvmModelAssociations;
import org.eclipse.xtext.xbase.typesystem.references.LightweightTypeReference;

/**
 * Utilities functions on JvmElements.
 * 
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 */
public class JvmElementUtil {

	/** Analyzing the type hierarchy of the given element, and
	 * extract any type-related information.
	 * 
	 * @param jvmElement - the element to analyze
	 * @param finalOperations - filled with the final operations inherited by the element.
	 * @param overridableOperations - filled with the oervrideable operations inherited by the element.
	 * @param inheritedFields - filled with the fields inherited by the element.
	 * @param operationsToImplement - filled with the abstract operations inherited by the element.
	 * @param sarlSignatureProvider - provider of tools related to action signatures.
	 */
	public static void populateInheritanceContext(
			JvmGenericType jvmElement,
			Map<ActionKey,JvmOperation> finalOperations,
			Map<ActionKey,JvmOperation> overridableOperations,
			Map<String,JvmField> inheritedFields,
			Map<ActionKey,JvmOperation> operationsToImplement,
			ActionSignatureProvider sarlSignatureProvider) {

		// Get the operations that must be implemented			
		for(JvmTypeReference interfaceReference : jvmElement.getExtendedInterfaces()) {
			for(JvmFeature feature : ((JvmGenericType)interfaceReference.getType()).getAllFeatures()) {
				if (!"java.lang.Object".equals(feature.getDeclaringType().getQualifiedName())) { //$NON-NLS-1$
					if (feature instanceof JvmOperation) {
						JvmOperation operation = (JvmOperation)feature;
						SignatureKey sig = sarlSignatureProvider.createSignatureIDFromJvmModel(operation.getParameters());
						ActionKey actionKey = sarlSignatureProvider.createActionID(operation.getSimpleName(), sig);
						operationsToImplement.put(actionKey, operation);
					}
				}
			}
		}

		// Check on the implemented features, inherited from the super type			
		if (jvmElement.getExtendedClass()!=null) {
			for(JvmFeature feature : ((JvmGenericType)jvmElement.getExtendedClass().getType()).getAllFeatures()) {
				if (!"java.lang.Object".equals(feature.getDeclaringType().getQualifiedName()) //$NON-NLS-1$
						&& isVisible(jvmElement, feature)
						&& !isHiddenAction(feature.getSimpleName())) {
					if (feature instanceof JvmOperation) {
						if (!feature.isStatic()) {
							JvmOperation operation = (JvmOperation)feature;
							SignatureKey sig = sarlSignatureProvider.createSignatureIDFromJvmModel(operation.getParameters());
							ActionKey actionKey = sarlSignatureProvider.createActionID(feature.getSimpleName(), sig);
							if (operation.isAbstract()) {
								operationsToImplement.put(actionKey, operation);
							}
							else if (operation.isFinal()) {
								finalOperations.put(actionKey, operation);
								operationsToImplement.remove(actionKey);
							}
							else {
								overridableOperations.put(actionKey, operation);
								operationsToImplement.remove(actionKey);
							}
						} 
					}
					else if (feature instanceof JvmField) {
						inheritedFields.put(feature.getSimpleName(), (JvmField)feature);
					}
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
	

}
