/*
 * $Id$
 *
 * SARL is an general-purpose agent programming language.
 * More details on http://www.sarl.io
 *
 * Copyright (C) 2014-2024 SARL.io, the Original Authors and Main Authors
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

package io.sarl.lang.validation.subvalidators;

import static io.sarl.lang.validation.IssueCodes.POTENTIAL_FIELD_SYNCHRONIZATION_PROBLEM;

import java.text.MessageFormat;
import java.util.ArrayList;
import java.util.HashSet;

import org.eclipse.emf.ecore.EStructuralFeature.Setting;
import org.eclipse.xtend.core.xtend.XtendField;
import org.eclipse.xtend.core.xtend.XtendFunction;
import org.eclipse.xtend.core.xtend.XtendMember;
import org.eclipse.xtext.EcoreUtil2;
import org.eclipse.xtext.common.types.JvmVisibility;
import org.eclipse.xtext.validation.Check;
import org.eclipse.xtext.validation.CheckType;
import org.eclipse.xtext.xbase.XSynchronizedExpression;
import org.eclipse.xtext.xbase.util.XbaseUsageCrossReferencer;
import org.eclipse.xtext.xbase.validation.JvmGenericTypeValidator;

import io.sarl.lang.sarl.SarlAgent;
import io.sarl.lang.sarl.SarlBehavior;
import io.sarl.lang.sarl.SarlBehaviorUnit;
import io.sarl.lang.sarl.SarlSkill;

/**
 * A specialization of {@link JvmGenericTypeValidator} to deal with synchronization issues.
 *
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 * @since 0.14
 */
public class SARLSynchronizationValidator extends AbstractSARLSubValidator {

	//@Inject
	//private ISynchronizedFieldDetector synchronizedFieldDetector;

	/** Check if a field needs to be synchronized.
	 *
	 * @param field the field.
	 * @since 0.7
	 */
	@Check(CheckType.EXPENSIVE)
	public void checkUnsynchronizedField(XtendField field) {
		if (doCheckValidMemberName(field) && !isIgnored(POTENTIAL_FIELD_SYNCHRONIZATION_PROBLEM)) {
			/*if (this.synchronizedFieldDetector.isSynchronizedField(field)) {
				return;
			}*/
			final var jvmField = getAssociations().getJvmField(field);
			if (jvmField == null || jvmField.eContainer() == null || jvmField.isConstant() || jvmField.isFinal()) {
				return;
			}
			final var scope = getOutermostType(field);
			if ((scope instanceof SarlAgent || scope instanceof SarlBehavior
					|| scope instanceof SarlSkill) && isLocallyAssigned(jvmField, scope)) {
				final var usages = XbaseUsageCrossReferencer.find(jvmField, scope);
				final var blocks = new HashSet<XtendMember>();
				var isAccessibleFromOutside = jvmField.getVisibility() != JvmVisibility.PRIVATE;
				final var pbUsages = new ArrayList<Setting>();
				for (final var usage : usages) {
					final var member = EcoreUtil2.getContainerOfType(usage.getEObject(), XtendMember.class);
					if (member instanceof XtendFunction fct) {
						blocks.add(member);
						if (member.getVisibility() != JvmVisibility.PRIVATE) {
							isAccessibleFromOutside = true;
						}
						if (!fct.isSynchonized()) {
							pbUsages.add(usage);
						}
					} else if (member instanceof SarlBehaviorUnit) {
						blocks.add(member);
						isAccessibleFromOutside = true;
						pbUsages.add(usage);
					}
				}
				for (final var usage : pbUsages) {
					var synchronizationIssue = false;
					if (isAccessibleFromOutside || blocks.size() > 1) {
						synchronizationIssue = true;
					} else {
						// TODO: Refine the function call detection
						synchronizationIssue = true;
					}
					// Check if the field is already locally synchronized
					if (synchronizationIssue) {
						final var syncExpr = EcoreUtil2.getContainerOfType(
								usage.getEObject(), XSynchronizedExpression.class);
						if (syncExpr != null) {
							synchronizationIssue = false;
						}
					}
					if (synchronizationIssue
							&& !isIgnored(POTENTIAL_FIELD_SYNCHRONIZATION_PROBLEM,
									usage.getEObject())) {
						addIssue(
								MessageFormat.format(Messages.SARLSynchronizationValidator_1, field.getName()),
								usage.getEObject(),
								usage.getEStructuralFeature(),
								POTENTIAL_FIELD_SYNCHRONIZATION_PROBLEM);
					}
				}
			}
		}
	}


}
