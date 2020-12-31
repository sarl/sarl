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

package io.sarl.lang.jvmmodel;

import com.google.inject.ImplementedBy;
import com.google.inject.Singleton;
import org.eclipse.emf.ecore.EObject;
import org.eclipse.xtend.core.jvmmodel.IXtendJvmAssociations;
import org.eclipse.xtend.core.xtend.XtendClass;
import org.eclipse.xtend.core.xtend.XtendConstructor;
import org.eclipse.xtend.core.xtend.XtendField;
import org.eclipse.xtend.core.xtend.XtendFunction;
import org.eclipse.xtext.common.types.JvmConstructor;
import org.eclipse.xtext.common.types.JvmField;
import org.eclipse.xtext.common.types.JvmGenericType;
import org.eclipse.xtext.common.types.JvmOperation;
import org.eclipse.xtext.xbase.XExpression;

import io.sarl.lang.sarl.SarlAction;
import io.sarl.lang.sarl.SarlAgent;
import io.sarl.lang.sarl.SarlAnnotationType;
import io.sarl.lang.sarl.SarlArtifact;
import io.sarl.lang.sarl.SarlBehavior;
import io.sarl.lang.sarl.SarlBehaviorUnit;
import io.sarl.lang.sarl.SarlCapacity;
import io.sarl.lang.sarl.SarlClass;
import io.sarl.lang.sarl.SarlConstructor;
import io.sarl.lang.sarl.SarlEnumeration;
import io.sarl.lang.sarl.SarlEvent;
import io.sarl.lang.sarl.SarlField;
import io.sarl.lang.sarl.SarlInterface;
import io.sarl.lang.sarl.SarlSkill;
import io.sarl.lang.sarl.SarlSpace;

/** Associations between the SARL elements and the JVM elements.
 *
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 */
@ImplementedBy(SarlJvmModelAssociations.Impl.class)
public interface SarlJvmModelAssociations extends IXtendJvmAssociations {

	/** Replies the SARL event associated to the given type.
	 *
	 * @param jvmType the JVM type.
	 * @return the SARL element.
	 */
	SarlEvent getSarlEvent(JvmGenericType jvmType);

	/** Replies the SARL agent associated to the given type.
	 *
	 * @param jvmType the JVM type.
	 * @return the SARL element.
	 */
	SarlAgent getSarlAgent(JvmGenericType jvmType);

	/** Replies the SARL behavior associated to the given type.
	 *
	 * @param jvmType the JVM type.
	 * @return the SARL element.
	 */
	SarlBehavior getSarlBehavior(JvmGenericType jvmType);

	/** Replies the SARL capacity associated to the given type.
	 *
	 * @param jvmType the JVM type.
	 * @return the SARL element.
	 */
	SarlCapacity getSarlCapacity(JvmGenericType jvmType);

	/** Replies the SARL skill associated to the given type.
	 *
	 * @param jvmType the JVM type.
	 * @return the SARL element.
	 */
	SarlSkill getSarlSkill(JvmGenericType jvmType);

	/** Replies the SARL space associated to the given type.
	 *
	 * @param jvmType the JVM type.
	 * @return the SARL element.
	 * @since 0.6
	 */
	SarlSpace getSarlSpace(JvmGenericType jvmType);

	/** Replies the SARL artifact associated to the given type.
	 *
	 * @param jvmType the JVM type.
	 * @return the SARL element.
	 * @since 0.6
	 */
	SarlArtifact getSarlArtifact(JvmGenericType jvmType);

	/** Replies the SARL class associated to the given type.
	 *
	 * @param jvmType the JVM type.
	 * @return the SARL element.
	 * @since 0.6
	 */
	SarlClass getSarlClass(JvmGenericType jvmType);

	/** Replies the SARL interface associated to the given type.
	 *
	 * @param jvmType the JVM type.
	 * @return the SARL element.
	 * @since 0.6
	 */
	SarlInterface getSarlInterface(JvmGenericType jvmType);

	/** Replies the SARL enumeration associated to the given type.
	 *
	 * @param jvmType the JVM type.
	 * @return the SARL element.
	 * @since 0.6
	 */
	SarlEnumeration getSarlEnumeration(JvmGenericType jvmType);

	/** Replies the SARL annotation type associated to the given type.
	 *
	 * @param jvmType the JVM type.
	 * @return the SARL element.
	 * @since 0.6
	 */
	SarlAnnotationType getSarlAnnotationType(JvmGenericType jvmType);

	/** Replies the SARL constructor associated to the given JVM constructor.
	 *
	 * @param jvmConstructor the JVM constructor.
	 * @return the SARL element.
	 * @since 0.7
	 */
	SarlConstructor getSarlConstructor(JvmConstructor jvmConstructor);

	/** Replies the SARL action associated to the given JVM operation.
	 *
	 * @param jvmOperation the JVM operation.
	 * @return the SARL element.
	 * @since 0.7
	 */
	SarlAction getSarlAction(JvmOperation jvmOperation);

	/** Replies the SARL field associated to the given JVM operation.
	 *
	 * @param jvmfield the JVM field.
	 * @return the SARL element.
	 * @since 0.7
	 */
	SarlField getSarlField(JvmField jvmfield);

	/** Replies the inferrer type for the given event.
	 * @param obj the SARL object.
	 * @return the inferred type.
	 */
	JvmGenericType getInferredType(SarlEvent obj);

	/** Replies the inferrer type for the given agent.
	 * @param obj the SARL object.
	 * @return the inferred type.
	 */
	JvmGenericType getInferredType(SarlAgent obj);

	/** Replies the inferrer type for the given behavior.
	 * @param obj the SARL object.
	 * @return the inferred type.
	 */
	JvmGenericType getInferredType(SarlBehavior obj);

	/** Replies the inferrer type for the given capacity.
	 * @param obj the SARL object.
	 * @return the inferred type.
	 */
	JvmGenericType getInferredType(SarlCapacity obj);

	/** Replies the inferrer type for the given skill.
	 * @param obj the SARL object.
	 * @return the inferred type.
	 */
	JvmGenericType getInferredType(SarlSkill obj);

	/**
	 * Returns the inferred operation for the guard of the given behavior unit.
	 *
	 * @param behaviorUnit the behavior unit.
	 * @return the inferred operation.
	 */
	JvmOperation getInferredGuardOperation(SarlBehaviorUnit behaviorUnit);

	/**
	 * Returns the inferred operation for the body of the given behavior unit.
	 *
	 * @param behaviorUnit the behavior unit.
	 * @return the inferred operation.
	 */
	JvmOperation getInferredBodyOperation(SarlBehaviorUnit behaviorUnit);

	/** Associations between the SARL elements and the JVM elements.
	 *
	 * @author $Author: sgalland$
	 * @version $FullVersion$
	 * @mavengroupid $GroupId$
	 * @mavenartifactid $ArtifactId$
	 */
	@Singleton
	class Impl extends IXtendJvmAssociations.Impl implements SarlJvmModelAssociations {

		@Override
		public SarlEvent getSarlEvent(JvmGenericType jvmType) {
			final EObject primarySourceElement = getPrimarySourceElement(jvmType);
			if (primarySourceElement instanceof SarlEvent) {
				return (SarlEvent) primarySourceElement;
			}
			return null;
		}

		@Override
		public SarlAgent getSarlAgent(JvmGenericType jvmType) {
			final EObject primarySourceElement = getPrimarySourceElement(jvmType);
			if (primarySourceElement instanceof SarlAgent) {
				return (SarlAgent) primarySourceElement;
			}
			return null;
		}

		@Override
		public SarlBehavior getSarlBehavior(JvmGenericType jvmType) {
			final EObject primarySourceElement = getPrimarySourceElement(jvmType);
			if (primarySourceElement instanceof SarlBehavior) {
				return (SarlBehavior) primarySourceElement;
			}
			return null;
		}

		@Override
		public SarlCapacity getSarlCapacity(JvmGenericType jvmType) {
			final EObject primarySourceElement = getPrimarySourceElement(jvmType);
			if (primarySourceElement instanceof SarlCapacity) {
				return (SarlCapacity) primarySourceElement;
			}
			return null;
		}

		@Override
		public SarlSpace getSarlSpace(JvmGenericType jvmType) {
			final EObject primarySourceElement = getPrimarySourceElement(jvmType);
			if (primarySourceElement instanceof SarlSpace) {
				return (SarlSpace) primarySourceElement;
			}
			return null;
		}

		@Override
		public SarlArtifact getSarlArtifact(JvmGenericType jvmType) {
			final EObject primarySourceElement = getPrimarySourceElement(jvmType);
			if (primarySourceElement instanceof SarlArtifact) {
				return (SarlArtifact) primarySourceElement;
			}
			return null;
		}

		@Override
		public SarlClass getSarlClass(JvmGenericType jvmType) {
			final EObject primarySourceElement = getPrimarySourceElement(jvmType);
			if (primarySourceElement instanceof SarlClass) {
				return (SarlClass) primarySourceElement;
			}
			return null;
		}

		@Override
		public SarlInterface getSarlInterface(JvmGenericType jvmType) {
			final EObject primarySourceElement = getPrimarySourceElement(jvmType);
			if (primarySourceElement instanceof SarlInterface) {
				return (SarlInterface) primarySourceElement;
			}
			return null;
		}

		@Override
		public SarlEnumeration getSarlEnumeration(JvmGenericType jvmType) {
			final EObject primarySourceElement = getPrimarySourceElement(jvmType);
			if (primarySourceElement instanceof SarlEnumeration) {
				return (SarlEnumeration) primarySourceElement;
			}
			return null;
		}

		@Override
		public SarlAnnotationType getSarlAnnotationType(JvmGenericType jvmType) {
			final EObject primarySourceElement = getPrimarySourceElement(jvmType);
			if (primarySourceElement instanceof SarlAnnotationType) {
				return (SarlAnnotationType) primarySourceElement;
			}
			return null;
		}

		@Override
		public JvmGenericType getInferredType(SarlEvent obj) {
			return getFirstOrNull(getJvmElements(obj), JvmGenericType.class);
		}

		@Override
		public JvmGenericType getInferredType(XtendClass xtendClass) {
			return getFirstOrNull(getJvmElements(xtendClass), JvmGenericType.class);
		}

		@Override
		public JvmGenericType getInferredType(SarlAgent obj) {
			return getFirstOrNull(getJvmElements(obj), JvmGenericType.class);
		}

		@Override
		public JvmGenericType getInferredType(SarlBehavior obj) {
			return getFirstOrNull(getJvmElements(obj), JvmGenericType.class);
		}

		@Override
		public JvmGenericType getInferredType(SarlCapacity obj) {
			return getFirstOrNull(getJvmElements(obj), JvmGenericType.class);
		}

		@Override
		public JvmGenericType getInferredType(SarlSkill obj) {
			return getFirstOrNull(getJvmElements(obj), JvmGenericType.class);
		}

		@Override
		public SarlSkill getSarlSkill(JvmGenericType jvmType) {
			final EObject primarySourceElement = getPrimarySourceElement(jvmType);
			if (primarySourceElement instanceof SarlSkill) {
				return (SarlSkill) primarySourceElement;
			}
			return null;
		}

		@Override
		public JvmOperation getInferredGuardOperation(SarlBehaviorUnit behaviorUnit) {
			final XExpression guard = behaviorUnit.getGuard();
			if (guard != null) {
				final EObject primaryJvmElement = getPrimaryJvmElement(guard);
				if (primaryJvmElement instanceof JvmOperation) {
					return (JvmOperation) primaryJvmElement;
				}
			}
			return null;
		}

		@Override
		public JvmOperation getInferredBodyOperation(SarlBehaviorUnit behaviorUnit) {
			final EObject primaryJvmElement = getPrimaryJvmElement(behaviorUnit);
			if (primaryJvmElement instanceof JvmOperation) {
				return (JvmOperation) primaryJvmElement;
			}
			return null;
		}

		@Override
		public XtendFunction getXtendFunction(JvmOperation jvmOperation) {
			XtendFunction fct = null;
			try {
				fct = super.getXtendFunction(jvmOperation);
			} catch (Throwable exception) {
				fct = null;
			}
			if (fct == null) {
				for (final EObject obj : getSourceElements(jvmOperation)) {
					if (obj instanceof XtendFunction) {
						fct = (XtendFunction) obj;
						break;
					}
				}
			}
			return fct;
		}

		@Override
		public SarlAction getSarlAction(JvmOperation jvmOperation) {
			final XtendFunction fct = getXtendFunction(jvmOperation);
			return fct instanceof SarlAction ? (SarlAction) fct : null;
		}

		@Override
		public XtendField getXtendField(JvmField jvmField) {
			XtendField fld = null;
			try {
				fld = super.getXtendField(jvmField);
			} catch (Throwable exception) {
				fld = null;
			}
			if (fld == null) {
				for (final EObject obj : getSourceElements(jvmField)) {
					if (obj instanceof XtendField) {
						fld = (XtendField) obj;
						break;
					}
				}
			}
			return fld;
		}

		@Override
		public SarlField getSarlField(JvmField jvmField) {
			final XtendField fld = getXtendField(jvmField);
			return fld instanceof SarlField ? (SarlField) fld : null;
		}

		@Override
		public XtendConstructor getXtendConstructor(JvmConstructor jvmConstructor) {
			XtendConstructor cons = null;
			try {
				cons = super.getXtendConstructor(jvmConstructor);
			} catch (Throwable exception) {
				cons = null;
			}
			if (cons == null) {
				for (final EObject obj : getSourceElements(jvmConstructor)) {
					if (obj instanceof XtendConstructor) {
						cons = (XtendConstructor) obj;
						break;
					}
				}
			}
			return cons;
		}

		@Override
		public SarlConstructor getSarlConstructor(JvmConstructor jvmConstructor) {
			final XtendConstructor cons = getXtendConstructor(jvmConstructor);
			return cons instanceof SarlConstructor ? (SarlConstructor) cons : null;
		}

	}

}
