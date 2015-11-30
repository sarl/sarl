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

import com.google.inject.Inject;
import com.google.inject.Singleton;
import org.eclipse.xtext.common.types.JvmOperation;
import org.eclipse.xtext.xbase.XExpression;
import org.eclipse.xtext.xbase.lib.Pure;
import org.eclipse.xtext.xbase.util.XExpressionHelper;

/**
 * Extended helper on expressions.
 *
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 * @see SARLXExpressionHelper
 */
@Singleton
public class ExtendedXExpressionHelper {

	@Inject
	private XExpressionHelper expressionHelper;

	private final Pattern pattern;

	/** Construct the helper.
	 */
	public ExtendedXExpressionHelper() {
		this.pattern = Pattern.compile(XExpressionConstants.SPECIAL_PURE_FUNCTION_NAME_PATTERN);
	}

	/** Check if the given operation could be annoted with "@Pure".
	 *
	 * @param operation - the operation to test.
	 * @param body - the body of the operation.
	 * @return <code>true</code> if one of the components of the given expression has a side effect;
	 *     otherwise <code>false</code>.
	 * @see Pure
	 */
	public boolean isPureOperation(JvmOperation operation, XExpression body) {
		if (operation == null || operation.isAbstract() || body == null) {
			return false;
		}
		String name = operation.getSimpleName();
		return (name != null && this.pattern.matcher(name).find()) || !this.expressionHelper.hasSideEffects(body);
	}

}
