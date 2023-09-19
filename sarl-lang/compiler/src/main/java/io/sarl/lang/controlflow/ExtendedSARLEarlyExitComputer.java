/*
 * $Id$
 *
 * SARL is an general-purpose agent programming language.
 * More details on http://www.sarl.io
 *
 * Copyright (C) 2014-2023 SARL.io, the Original Authors and Main Authors
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

package io.sarl.lang.controlflow;

import javax.inject.Inject;
import javax.inject.Singleton;

import org.eclipse.xtext.xbase.XAbstractFeatureCall;
import org.eclipse.xtext.xbase.XExpression;
import org.eclipse.xtext.xbase.XbasePackage;
import org.eclipse.xtext.xbase.typesystem.util.ExtendedEarlyExitComputer;

/** Compute the early-exit flag for the SARL statements.
 *
 * @author $Author: sgalland$
 * @version compiler 0.13.0 20230919-093056
 * @mavengroupid io.sarl.lang
 * @mavenartifactid compiler
 */
@Singleton
public class ExtendedSARLEarlyExitComputer extends ExtendedEarlyExitComputer {

	@Inject
	private ISarlEarlyExitComputer originalComputer;

	@Override
	public boolean isDefiniteEarlyExit(XExpression expression) {
		if (super.isDefiniteEarlyExit(expression)) {
			return true;
		}
		return isEarlyExitSARLStatement(expression);
	}

	@Override
	public boolean isIntentionalEarlyExit(XExpression expression) {
		if (super.isIntentionalEarlyExit(expression)) {
			return true;
		}
		return isEarlyExitSARLStatement(expression);
	}

	/** Replies if the given expression is a early-exit SARL statement.
	 *
	 * @param expression the expression to test.
	 * @return {@code true} if the given expression is a SARL early-exit
	 *     statement, {@code false} otherwise.
	 * @see ISarlEarlyExitComputer#isEarlyExitAnnotatedElement(Object)
	 */
	protected boolean isEarlyExitSARLStatement(XExpression expression) {
		if (expression instanceof XAbstractFeatureCall) {
			// Do not call expression.getFeature() since the feature may be unresolved.
			// The type resolution at this point causes exceptions in the reentrant type resolver.
			// The second parameter (false) forces to ignore feature resolution.
			final Object element = expression.eGet(
					XbasePackage.Literals.XABSTRACT_FEATURE_CALL__FEATURE,
					false);
			return this.originalComputer.isEarlyExitAnnotatedElement(element);
		}
		return false;
	}

}

