/*
 * $Id$
 *
 * SARL is an general-purpose agent programming language.
 * More details on http://www.sarl.io
 *
 * Copyright (C) 2014-2016 the original authors or authors.
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

import com.google.inject.Singleton;
import org.eclipse.xtend.core.typing.XtendExpressionHelper;
import org.eclipse.xtext.common.types.JvmIdentifiableElement;
import org.eclipse.xtext.common.types.JvmOperation;
import org.eclipse.xtext.xbase.XAbstractFeatureCall;
import org.eclipse.xtext.xbase.XExpression;
import org.eclipse.xtext.xbase.lib.Pure;

/**
 * Helper on expressions.
 *
 * <p>This implementation extends the Xtend expression helper by assuming that any function
 * with a name starting with "get", "is", "has" is a pure function.
 * It also assumes that "equals", "hashCode", "clone" and "toString" are also pure functions.
 *
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 * @see "http://www.eclipse.org/Xtext/documentation.html#validation"
 */
@Singleton
public class SARLExpressionHelper extends XtendExpressionHelper {

	/** Regular expression pattern that matches the names of functions usually
	 * considered as pure.
	 */
	public static final String SPECIAL_PURE_FUNCTION_NAME_PATTERN =
			"^(((is)|(get)|(has))[A-Z].*)|(equals)|(hashCode)|(clone)|(toString)$"; //$NON-NLS-1$;

	private final Pattern pattern;

	/** Construct the helper.
	 */
	public SARLExpressionHelper() {
		this.pattern = Pattern.compile(SPECIAL_PURE_FUNCTION_NAME_PATTERN);
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

	/** Check if the given operation could be annoted with "@Pure".
	 *
	 * @param operation - the operation to test.
	 * @param body - the body of the operation.
	 * @return <code>true</code> if one of the components of the given expression has a side effect;
	 *     otherwise <code>false</code>.
	 * @see Pure
	 */
	public boolean isPurableOperation(JvmOperation operation, XExpression body) {
		if (operation == null || operation.isAbstract() || body == null) {
			return false;
		}
		String name = operation.getSimpleName();
		return (name != null && this.pattern.matcher(name).find()) || !hasSideEffects(body);
	}

}
