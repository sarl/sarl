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
class XtendBug392440 {

	private val JvmTypesBuilder typesBuilder
	
	new(JvmTypesBuilder typesBuilder) {
		this.typesBuilder = typesBuilder
	}

	/** 
	 * FIXME: Remove this function if it is fixed in Xtext: https://bugs.eclipse.org/bugs/show_bug.cgi?id=392440
	 * 
	 * Copied/pasted from {@link JvmTypesBuilder#toHashCodeMethod(EObject, boolean, JvmField...)}.
	 * Updated for fixing the issue {@link "https://bugs.eclipse.org/bugs/show_bug.cgi?id=392440"}
	 *
	 * @param owner 
	 * @param sourceElement
	 * @param extendsSomethingWithProperHashCode
	 * @param jvmFields
	 * @return the operation.
	 */
	public def JvmOperation toHashCodeMethod(JvmGenericType owner, EObject sourceElement, boolean extendsSomethingWithProperHashCode, JvmField ...jvmFields) {
		if (sourceElement === null) return null
		var JvmOperation result = this.typesBuilder.toMethod(
			sourceElement, "hashCode", 
			this.typesBuilder.newTypeRef(sourceElement, Integer.TYPE), null
		)
		if (result === null) return null
		result.annotations.add(this.typesBuilder.toAnnotation(sourceElement, Override))
		this.typesBuilder.setBody(result, [
				append("final int prime = 31;")
				if (extendsSomethingWithProperHashCode) {
					newLine().append("int result = super.hashCode();")
				} else {
					newLine().append("int result = 1;")
				}
				for (JvmField field : jvmFields) {
					var String typeName = field.type.identifier
					if (Boolean.TYPE.name == typeName) {
						newLine().append("result = prime * result + (this." + field.getSimpleName() +" ? 1231 : 1237);")
					} else if (Integer.TYPE.name == typeName
							|| Character.TYPE.name == typeName
							|| Byte.TYPE.name == typeName
							|| Short.TYPE.name == typeName) {
						newLine().append("result = prime * result + this." + field.getSimpleName() +";")
					} else if (Long.TYPE.name == typeName) {
						newLine().append("result = prime * result + (int) (this." + field.getSimpleName() +" ^ (this." + field.getSimpleName() + " >>> 32));")
					} else if (Float.TYPE.name == typeName) {
						newLine().append("result = prime * result + Float.floatToIntBits(this." + field.getSimpleName() +");")
					} else if (Double.TYPE.name == typeName) {
						newLine().append("result = prime * result + (int) (Double.doubleToLongBits(this." + field.getSimpleName() +") ^ (Double.doubleToLongBits(this." + field.getSimpleName() + ") >>> 32));");
					} else {
						newLine().append("result = prime * result + ((this." + field.getSimpleName() +"== null) ? 0 : this."+field.getSimpleName()+".hashCode());");
					}
				}
				newLine().append("return result;");
		])
		return result
	}

}
