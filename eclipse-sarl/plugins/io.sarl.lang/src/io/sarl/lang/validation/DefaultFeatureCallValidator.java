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

package io.sarl.lang.validation;

import org.eclipse.xtext.xbase.XAbstractFeatureCall;

import io.sarl.lang.util.Utils;

/** Validator of the feature calls.
 *
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 */
public class DefaultFeatureCallValidator implements FeatureCallValidator {

	/** Construct the validator.
	 */
	public DefaultFeatureCallValidator() {
		//
	}

	@Override
	public boolean isDisallowedCall(XAbstractFeatureCall call) {
		if (call != null && call.getFeature() != null) {
			final String id = call.getFeature().getQualifiedName();
			return "java.lang.System.exit".equals(id); //$NON-NLS-1$
		}
		return false;
	}

	@Override
	public boolean isDiscouragedCall(XAbstractFeatureCall call) {
		if (call != null && call.getFeature() != null) {
			final String id = call.getFeature().getQualifiedName();
			if (id != null) {
				switch (id) {
				case "java.lang.System.err": //$NON-NLS-1$
				case "java.lang.System.out": //$NON-NLS-1$
				case "java.lang.System.setErr": //$NON-NLS-1$
				case "java.lang.System.setOut": //$NON-NLS-1$
				case "java.lang.System.console": //$NON-NLS-1$
				case "java.lang.System.inheritedChannel": //$NON-NLS-1$
					return true;
				default:
					if (id.startsWith("org.eclipse.xtext.xbase.lib.InputOutput")) { //$NON-NLS-1$
						return true;
					}
					if (Utils.isHiddenMember(call.getFeature().getSimpleName())) {
						return true;
					}
				}
			}
		}
		return false;
	}

}
