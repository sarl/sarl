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

package io.sarl.lang.validation;

import java.util.List;
import java.util.Map;

import com.google.inject.Inject;
import com.google.inject.Singleton;
import org.eclipse.emf.ecore.EObject;
import org.eclipse.xtend.core.validation.XtendEarlyExitValidator;
import org.eclipse.xtext.validation.Check;
import org.eclipse.xtext.validation.IssueSeverities;
import org.eclipse.xtext.xbase.XAbstractFeatureCall;
import org.eclipse.xtext.xbase.XBlockExpression;
import org.eclipse.xtext.xbase.XExpression;
import org.eclipse.xtext.xbase.validation.IssueCodes;

import io.sarl.lang.controlflow.ISarlEarlyExitComputer;

/** Validation of the early-exit control flow.
 *
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 */
@Singleton
public class SARLEarlyExitValidator extends XtendEarlyExitValidator {

	@Inject
	private ISarlEarlyExitComputer earlyExitComputer;

	@Inject
	private IProgrammaticWarningSuppressor warningSuppressor;

	@Override
	protected IssueSeverities getIssueSeverities(Map<Object, Object> context, EObject eObject) {
		final var severities = super.getIssueSeverities(context, eObject);
		return this.warningSuppressor.getIssueSeverities(context, eObject, severities);
	}

	/** {@inheritDoc}.
	 *
	 * <p>This function is overriden for:<ul>
	 * <li>The XAbstractFeatureCall statements are not considered as potential early exit causes.
	 *     in the super function. We need to mark the dead code for the XAbstractFeatureCall statements
	 *     which refer to a function with the {@link io.sarl.lang.core.annotation.EarlyExit} annotation.</li>
	 * <li>Mark as dead the code after a "break" statement.</li>
	 * </ul>
	 */
	@Override
	@Check
	public void checkDeadCode(XBlockExpression block) {
		final var expressions = block.getExpressions();
		final var size = expressions.size();
		for (var i = 0; i < size - 1; ++i) {
			final var expression = expressions.get(i);
			if (this.earlyExitComputer.isEarlyExit(expression)) {
				if (expression instanceof XAbstractFeatureCall cvalue) {
					if (this.earlyExitComputer.isEarlyExitAnnotatedElement(
							cvalue.getFeature())) {
						markAsDeadCode(expressions.get(i + 1));
					}
				} else {
					// XAbstractFeatureCall does already a decent job for its argument lists
					// no additional error necessary
					markAsDeadCode(expressions.get(i + 1));
				}
				return;
			} else if (this.earlyExitComputer.isEarlyExitLoop(expression)) {
				markAsDeadCode(expressions.get(i + 1));
			}
		}
	}

	@Override
	protected void collectExits(EObject expr, List<XExpression> found) {
		super.collectExits(expr, found);
		if (expr instanceof XAbstractFeatureCall cvalue) {
			final var element = cvalue.getFeature();
			if (this.earlyExitComputer.isEarlyExitAnnotatedElement(element)) {
				found.add((XExpression) expr);
			}
		}
	}

	// This code is copied from the super type
	private boolean markAsDeadCode(XExpression expression) {
		if (expression instanceof XBlockExpression block) {
			final var expressions = block.getExpressions();
			if (!expressions.isEmpty()) {
				markAsDeadCode(expressions.get(0));
				return true;
			}
		}
		if (expression != null) {
			error(Messages.SARLEarlyExitValidator_0, expression, null, IssueCodes.UNREACHABLE_CODE);
			return true;
		}
		return false;
	}

}

