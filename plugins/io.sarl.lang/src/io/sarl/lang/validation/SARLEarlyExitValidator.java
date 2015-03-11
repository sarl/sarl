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

package io.sarl.lang.validation;

import io.sarl.lang.controlflow.SARLEarlyExitComputerUtil;

import java.util.List;

import org.eclipse.emf.common.util.EList;
import org.eclipse.emf.ecore.EObject;
import org.eclipse.xtend.core.validation.XtendEarlyExitValidator;
import org.eclipse.xtext.common.types.JvmIdentifiableElement;
import org.eclipse.xtext.validation.Check;
import org.eclipse.xtext.xbase.XAbstractFeatureCall;
import org.eclipse.xtext.xbase.XBlockExpression;
import org.eclipse.xtext.xbase.XExpression;
import org.eclipse.xtext.xbase.XFeatureCall;
import org.eclipse.xtext.xbase.XMemberFeatureCall;
import org.eclipse.xtext.xbase.controlflow.IEarlyExitComputer;
import org.eclipse.xtext.xbase.validation.IssueCodes;

import com.google.inject.Inject;

/** Validation of the early-exit control flow.
 *
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 */
public class SARLEarlyExitValidator extends XtendEarlyExitValidator {

	@Inject
	private IEarlyExitComputer earlyExitComputer;

	@Override
	@Check
	public void checkDeadCode(XBlockExpression block) {
		EList<XExpression> expressions = block.getExpressions();
		for (int i = 0, size = expressions.size(); i < size - 1; ++i) {
			XExpression expression = expressions.get(i);
			if (this.earlyExitComputer.isEarlyExit(expression)) {
				if (!(expression instanceof XAbstractFeatureCall)) {
					// XAbstractFeatureCall does already a decent job for its argument lists
					// no additional error necessary
					markAsDeadCode(expressions.get(i + 1));
				} else if (expression instanceof XMemberFeatureCall || expression instanceof XFeatureCall) {
					JvmIdentifiableElement element = ((XAbstractFeatureCall) expression).getFeature();
					if (SARLEarlyExitComputerUtil.isEarlyExitAnnotatedElement(element)) {
						markAsDeadCode(expressions.get(i + 1));
					}
				}
				return;
			}
		}
	}

	@Override
	protected void collectExits(EObject expr, List<XExpression> found) {
		if (expr instanceof XMemberFeatureCall || expr instanceof XFeatureCall) {
			JvmIdentifiableElement element = ((XAbstractFeatureCall) expr).getFeature();
			if (SARLEarlyExitComputerUtil.isEarlyExitAnnotatedElement(element)) {
				found.add((XExpression) expr);
				return;
			}
		}
		super.collectExits(expr, found);
	}

	private boolean markAsDeadCode(XExpression expression) {
		if (expression instanceof XBlockExpression) {
			XBlockExpression block = (XBlockExpression) expression;
			EList<XExpression> expressions = block.getExpressions();
			if (markAsDeadCode(expressions)) {
				return true;
			}
		}
		if (expression != null) {
			error("Unreachable expression.", expression, null, IssueCodes.UNREACHABLE_CODE); //$NON-NLS-1$
			return true;
		}
		return false;
	}

	private boolean markAsDeadCode(List<XExpression> expressions) {
		if (!expressions.isEmpty()) {
			markAsDeadCode(expressions.get(0));
			return true;
		}
		return false;
	}

}

