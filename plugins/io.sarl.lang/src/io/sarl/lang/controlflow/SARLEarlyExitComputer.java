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

package io.sarl.lang.controlflow;

import java.util.Collection;
import java.util.Collections;

import org.eclipse.xtext.common.types.JvmIdentifiableElement;
import org.eclipse.xtext.xbase.XAbstractFeatureCall;
import org.eclipse.xtext.xbase.XExpression;
import org.eclipse.xtext.xbase.XFeatureCall;
import org.eclipse.xtext.xbase.XMemberFeatureCall;
import org.eclipse.xtext.xbase.controlflow.DefaultEarlyExitComputer;

/** Compute the early-exit flag for the SARL statements.
 *
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 */
public class SARLEarlyExitComputer extends DefaultEarlyExitComputer {

	@Override
	protected Collection<ExitPoint> _exitPoints(XAbstractFeatureCall expression) {
		Collection<ExitPoint> exitPoints = super._exitPoints(expression);
		if (!isNotEmpty(exitPoints)
				&& (expression instanceof XMemberFeatureCall || expression instanceof XFeatureCall)) {
			JvmIdentifiableElement element = expression.getFeature();
			if (SARLEarlyExitComputerUtil.isEarlyExitAnnotatedElement(element)) {
				exitPoints = Collections.<ExitPoint>singletonList(new SARLExitPoint(expression, false));
			}
		}
		return exitPoints;
	}

	/** Exit point description.
	 * <p>
	 * This class is defined for helping to override the standard Xbase exit point.
	 *
	 * @author $Author: sgalland$
	 * @version $FullVersion$
	 * @mavengroupid $GroupId$
	 * @mavenartifactid $ArtifactId$
	 */
	protected static class SARLExitPoint extends ExitPoint {

		/**
		 * @param expression - the expressionthat causes the early exit.
		 * @param exceptionalExit - flag that indicates if this is an exceptional exit.
		 */
		public SARLExitPoint(XExpression expression, boolean exceptionalExit) {
			super(expression, exceptionalExit);
		}

	}

}

