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
package io.sarl.lang.jvmmodel;

import io.sarl.lang.sarl.SarlAgent;
import io.sarl.lang.sarl.SarlBehavior;
import io.sarl.lang.sarl.SarlCapacity;
import io.sarl.lang.sarl.SarlEvent;
import io.sarl.lang.sarl.SarlSkill;

import org.eclipse.emf.ecore.EObject;
import org.eclipse.xtend.core.jvmmodel.IXtendJvmAssociations;
import org.eclipse.xtend.core.xtend.XtendClass;
import org.eclipse.xtext.common.types.JvmGenericType;

import com.google.inject.ImplementedBy;
import com.google.inject.Singleton;

/** Associations between the SARL elements and the JVM elements.
 *
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 */
@ImplementedBy(SarlJvmModelAssociations.Impl.class)
public interface SarlJvmModelAssociations extends IXtendJvmAssociations {

	/** Replies the inferrer type for the given event.
	 * @param obj the SARL object.
	 * @return the inferred type.
	 */
	JvmGenericType getInferredType(SarlEvent obj);
		
	/** Replies the SARL event associated to the given type.
	 *
	 * @param jvmType - the JVM type.
	 * @return the SARL element.
	 */
	SarlEvent getSarlEvent(JvmGenericType jvmType);
		
	/** Replies the inferrer type for the given agent.
	 * @param obj the SARL object.
	 * @return the inferred type.
	 */
	JvmGenericType getInferredType(SarlAgent obj);
		
	/** Replies the SARL agent associated to the given type.
	 *
	 * @param jvmType - the JVM type.
	 * @return the SARL element.
	 */
	SarlAgent getSarlAgent(JvmGenericType jvmType);

	/** Replies the inferrer type for the given behavior.
	 * @param obj the SARL object.
	 * @return the inferred type.
	 */
	JvmGenericType getInferredType(SarlBehavior obj);
		
	/** Replies the SARL behavior associated to the given type.
	 *
	 * @param jvmType - the JVM type.
	 * @return the SARL element.
	 */
	SarlBehavior getSarlBehavior(JvmGenericType jvmType);

	/** Replies the inferrer type for the given capacity.
	 * @param obj the SARL object.
	 * @return the inferred type.
	 */
	JvmGenericType getInferredType(SarlCapacity obj);
		
	/** Replies the SARL capacity associated to the given type.
	 *
	 * @param jvmType - the JVM type.
	 * @return the SARL element.
	 */
	SarlCapacity getSarlCapacity(JvmGenericType jvmType);

	/** Replies the inferrer type for the given skill.
	 * @param obj the SARL object.
	 * @return the inferred type.
	 */
	JvmGenericType getInferredType(SarlSkill obj);
		
	/** Replies the SARL skill associated to the given type.
	 *
	 * @param jvmType - the JVM type.
	 * @return the SARL element.
	 */
	SarlSkill getSarlSkill(JvmGenericType jvmType);

	/** Associations between the SARL elements and the JVM elements.
	 *
	 * @author $Author: sgalland$
	 * @version $FullVersion$
	 * @mavengroupid $GroupId$
	 * @mavenartifactid $ArtifactId$
	 */
	@Singleton
	public static class Impl extends IXtendJvmAssociations.Impl implements SarlJvmModelAssociations {

		@Override
		public JvmGenericType getInferredType(XtendClass xtendClass) {
			final JvmGenericType firstOrNull = getFirstOrNull(getJvmElements(xtendClass), JvmGenericType.class);
			return firstOrNull;
		}

		@Override
		public JvmGenericType getInferredType(SarlEvent obj) {
			return getFirstOrNull(getJvmElements(obj), JvmGenericType.class);
		}

		@Override
		public SarlEvent getSarlEvent(JvmGenericType jvmType) {
			EObject primarySourceElement = getPrimarySourceElement(jvmType);
			if (primarySourceElement instanceof SarlEvent) { 
				return (SarlEvent) primarySourceElement;
			}
			return null;
		}

		@Override
		public JvmGenericType getInferredType(SarlAgent obj) {
			return getFirstOrNull(getJvmElements(obj), JvmGenericType.class);
		}

		@Override
		public SarlAgent getSarlAgent(JvmGenericType jvmType) {
			EObject primarySourceElement = getPrimarySourceElement(jvmType);
			if (primarySourceElement instanceof SarlAgent) { 
				return (SarlAgent) primarySourceElement;
			}
			return null;
		}

		@Override
		public JvmGenericType getInferredType(SarlBehavior obj) {
			return getFirstOrNull(getJvmElements(obj), JvmGenericType.class);
		}

		@Override
		public SarlBehavior getSarlBehavior(JvmGenericType jvmType) {
			EObject primarySourceElement = getPrimarySourceElement(jvmType);
			if (primarySourceElement instanceof SarlBehavior) { 
				return (SarlBehavior) primarySourceElement;
			}
			return null;
		}

		@Override
		public JvmGenericType getInferredType(SarlCapacity obj) {
			return getFirstOrNull(getJvmElements(obj), JvmGenericType.class);
		}

		@Override
		public SarlCapacity getSarlCapacity(JvmGenericType jvmType) {
			EObject primarySourceElement = getPrimarySourceElement(jvmType);
			if (primarySourceElement instanceof SarlCapacity) { 
				return (SarlCapacity) primarySourceElement;
			}
			return null;
		}

		@Override
		public JvmGenericType getInferredType(SarlSkill obj) {
			return getFirstOrNull(getJvmElements(obj), JvmGenericType.class);
		}

		@Override
		public SarlSkill getSarlSkill(JvmGenericType jvmType) {
			EObject primarySourceElement = getPrimarySourceElement(jvmType);
			if (primarySourceElement instanceof SarlSkill) { 
				return (SarlSkill) primarySourceElement;
			}
			return null;
		}

	}
	
}
