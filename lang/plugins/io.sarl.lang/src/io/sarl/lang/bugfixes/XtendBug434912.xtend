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
package io.sarl.lang.bugfixes

import org.eclipse.emf.ecore.EObject
import org.eclipse.xtext.common.types.JvmDeclaredType
import org.eclipse.xtext.common.types.JvmField
import org.eclipse.xtext.common.types.JvmGenericType
import org.eclipse.xtext.common.types.JvmOperation
import org.eclipse.xtext.xbase.jvmmodel.JvmTypesBuilder

/**
 * Patches for the bug 434912 in Xtend.
 * 
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 */
class XtendBug434912 {

	private val JvmTypesBuilder typesBuilder
	
	new(JvmTypesBuilder typesBuilder) {
		this.typesBuilder = typesBuilder
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
	public def JvmOperation toEqualsMethod(JvmGenericType owner, EObject sourceElement, JvmDeclaredType declaredType, boolean isDelegateToSuperEquals, JvmField... jvmFields) {
		var JvmOperation result = this.typesBuilder.toMethod(
			sourceElement, "equals", 
			this.typesBuilder.newTypeRef(sourceElement, Boolean.TYPE),
			null
		)
		result.annotations.add(this.typesBuilder.toAnnotation(sourceElement,Override))
		result.parameters.add(this.typesBuilder.toParameter(
			sourceElement, "obj", 
			this.typesBuilder.newTypeRef(sourceElement, Object)
		))
		this.typesBuilder.setBody(result, [
					append("if (this == obj)").increaseIndentation()
					newLine().append("return true;").decreaseIndentation()
					newLine().append("if (obj == null)").increaseIndentation()
					newLine().append("return false;").decreaseIndentation()
					newLine().append("if (getClass() != obj.getClass())").increaseIndentation()
					newLine().append("return false;").decreaseIndentation()
					if (isDelegateToSuperEquals) {
						newLine().append("if (!super.equals(obj))").increaseIndentation()
						newLine().append("return false;").decreaseIndentation()
					}
					newLine().append(declaredType.getSimpleName()+" other = (" + declaredType.getSimpleName() + ") obj;")
					for (JvmField field : jvmFields) {
						var String typeName = field.type.identifier
						if (Boolean.TYPE.name == typeName 
								|| Integer.TYPE.name == typeName
								|| Long.TYPE.name == typeName
								|| Character.TYPE.name == typeName
								|| Byte.TYPE.name == typeName
								|| Short.TYPE.name == typeName) {
							newLine().append("if (other." + field.getSimpleName() +" != this." + field.getSimpleName() + ")").increaseIndentation()
							newLine().append("return false;").decreaseIndentation()
							
						} else if (Double.TYPE.name == typeName) {
							newLine().append("if (Double.doubleToLongBits(other." + field.getSimpleName() +") != Double.doubleToLongBits(this." + field.getSimpleName() + "))").increaseIndentation()
							newLine().append("return false;").decreaseIndentation()
						} else if (Float.TYPE.name == typeName) {
							newLine().append("if (Float.floatToIntBits(other." + field.getSimpleName() +") != Float.floatToIntBits(this." + field.getSimpleName() + "))").increaseIndentation()
							newLine().append("return false;").decreaseIndentation()
						} else {
							newLine().append("if (this." + field.getSimpleName() +" == null) {").increaseIndentation()
							newLine().append("if (other." + field.getSimpleName() +" != null)").increaseIndentation()
							newLine().append("return false;").decreaseIndentation()
							decreaseIndentation()
							newLine().append("} else if (!this."+ field.getSimpleName() +".equals(other."+ field.getSimpleName() +"))").increaseIndentation()
							newLine().append("return false;").decreaseIndentation()
						}
					}
					newLine().append("return true;")
			])
		return result
	}

}
