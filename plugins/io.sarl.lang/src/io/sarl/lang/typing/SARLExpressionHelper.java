/*
 * $Id$
 *
 * SARL is an general-purpose agent programming language.
 * More details on http://www.sarl.io
 *
 * Copyright (C) 2014-2015 the original authors or authors.
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

package io.sarl.lang.typing;

import java.util.regex.Pattern;

import org.eclipse.xtext.xbase.XExpression;

import org.eclipse.xtext.xbase.XBlockExpression;
import com.google.inject.Singleton;
import org.eclipse.xtend.core.typing.XtendExpressionHelper;
import org.eclipse.xtext.common.types.JvmIdentifiableElement;
import org.eclipse.xtext.common.types.JvmOperation;
import org.eclipse.xtext.xbase.XAbstractFeatureCall;

/**
 * Helper on expressions.
 *
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 * @see "http://www.eclipse.org/Xtext/documentation.html#validation"
 */
@Singleton
public class SARLExpressionHelper extends XtendExpressionHelper {

	private final Pattern pattern;

	/** Construct the helper.
	 */
	public SARLExpressionHelper() {
		this.pattern = Pattern.compile("^(((is)|(get)|(has))[A-Z].*)|(equals)|(hashCode)|(clone)|(toString)$"); //$NON-NLS-1$
	}

	@Override
	public boolean hasSideEffects(XAbstractFeatureCall featureCall, boolean inspectContents) {
		if (super.hasSideEffects(featureCall, inspectContents)) {
			JvmIdentifiableElement feature = featureCall.getFeature();
			if ((feature != null) && (!feature.eIsProxy()) && (feature instanceof JvmOperation)) {
				String name = ((JvmOperation) feature).getSimpleName();
				if (name != null && this.pattern.matcher(name).find()) {
					return false;
				}
			}
			return true;
		}
		return false;
	}
	
	/** Replies if the given expression may the root expression for a pure function.
	 *
	 * <p>A pure function causes no externally visible side-effects and does not mutate non-local state. 
	 * In other words: if the result of a pure function is not used, it is dead code and is supposed to
	 * be removeable without changing the behavior of the program.
	 *
	 * <p>This function extends the function {@link #hasSideEffects(org.eclipse.xtext.xbase.XExpression)}
	 * by assuming that {@link XBlockExpression} expressions do not cause side effects if its member
	 * expressions do not cause side effects.
	 * 
	 * <p><strong>This function is a recursive version of {@link #hasSideEffects(org.eclipse.xtext.xbase.XExpression)}.
	 * 
	 * @param expression - the expression to test.
	 * @return whether the expression itself (not its children) possibly causes a side-effect
	 */
	public boolean hasDeepSideEffects(XExpression expression) {
//		if (expression instanceof XBlockExpression) {
//			for (XExpression subexpression : ((XBlockExpression) expression).getExpressions()) {
//				if (hasDeepSideEffects(subexpression)) {
//					return true;
//				}
//			}
//			return false;
//		}
		return hasSideEffects(expression);
	}

}
