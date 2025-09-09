/*
 * $Id$
 *
 * SARL is an general-purpose agent programming language.
 * More details on http://www.sarl.io
 *
 * Copyright (C) 2014-2025 SARL.io, the Original Authors and Main Authors.
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

import com.google.inject.Inject;
import com.google.inject.Singleton;
import org.eclipse.xtext.common.types.JvmAnnotationTarget;
import org.eclipse.xtext.common.types.JvmTypeReference;
import org.eclipse.xtext.common.types.util.AnnotationLookup;
import org.eclipse.xtext.xbase.XAbstractFeatureCall;
import org.eclipse.xtext.xbase.XExpression;
import org.eclipse.xtext.xbase.controlflow.DefaultEarlyExitComputer;

import io.sarl.lang.core.annotation.EarlyExit;
import io.sarl.lang.sarl.SarlAction;
import io.sarl.lang.sarl.SarlBreakExpression;
import io.sarl.lang.sarl.SarlContinueExpression;

/** Compute the early-exit flag for the SARL statements.
 *
 * @author <a href="http://www.ciad-lab.fr/stephane_galland">St&eacute;phane Galland</a>
 * @version compiler 0.15.0 20250909-115746
 * @mavengroupid io.sarl.lang
 * @mavenartifactid compiler
 */
@Singleton
public class SARLEarlyExitComputer extends DefaultEarlyExitComputer implements ISarlEarlyExitComputer {

	@Inject
	private AnnotationLookup annotations;

	@Override
	protected Collection<ExitPoint> _exitPoints(XAbstractFeatureCall expression) {
		final var exitPoints = super._exitPoints(expression);
		if (isNotEmpty(exitPoints)) {
			return exitPoints;
		}
		final var element = expression.getFeature();
		if (isEarlyExitAnnotatedElement(element)) {
			return Collections.<ExitPoint>singletonList(new SarlExitPoint(expression, false));
		}
		return Collections.emptyList();
	}

	@Override
	public boolean isEarlyExitEvent(JvmTypeReference reference) {
		if (reference != null && !reference.eIsProxy()) {
			final var type = reference.getType();
			return isEarlyExitAnnotatedElement(type);
		}
		return false;
	}

	@Override
	public boolean isEarlyExitAnnotatedElement(Object element) {
		return (element instanceof JvmAnnotationTarget cvalue)
				&& (this.annotations.findAnnotation(cvalue, EarlyExit.class) != null);
	}

	@Override
	public boolean isEarlyExitLoop(XExpression expression) {
		return expression instanceof SarlBreakExpression || expression instanceof SarlContinueExpression;
	}

	@Override
	public boolean isEarlyExitOperation(SarlAction operation) {
		if (operation != null) {
			final var eventIterator = operation.getFiredEvents().iterator();
			while (eventIterator.hasNext()) {
				if (isEarlyExitEvent(eventIterator.next())) {
					return true;
				}
			}
		}
		return false;
	}

	@Override
	public boolean isEarlyExitInJava(XExpression expression) {
		final var exitPoints = getExitPoints(expression);
		if (isNotEmpty(exitPoints)) {
			for (final var exitPoint : exitPoints) {
				if (exitPoint instanceof SarlExitPoint) {
					return false;
				}
			}
			return true;
		}
		return false;
	}

	/**
	 * Exit point that is specific to SARL, not Xbase/Java.
	 *
	 * @author <a href="http://www.ciad-lab.fr/stephane_galland">St&eacute;phane Galland</a>
	 * @version compiler 0.15.0 20250909-115746
	 * @mavengroupid io.sarl.lang
	 * @mavenartifactid compiler
	 * @since 0.8
	 */
	public static class SarlExitPoint extends ExitPoint {

		/**
		 * Constructor.
		 *
		 * @param expression the expression that is an exit point.
		 * @param exceptionalExit {@code true} if the exit is exceptional (exception, etc.).
		 */
		public SarlExitPoint(XExpression expression, boolean exceptionalExit) {
			super(expression, exceptionalExit);
		}

	}

}

