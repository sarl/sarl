/*
 * $Id$
 * 
 * Janus platform is an open-source multiagent platform.
 * More details on http://www.janusproject.io
 *
 * Copyright (C) 2014 Sebastian RODRIGUEZ, Nicolas GAUD, Stéphane GALLAND.
 * 
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 * http://www.apache.org/licenses/LICENSE-2.0
 * 
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package io.sarl.lang.validation;

import io.sarl.lang.util.ModelUtil;

import org.eclipse.xtext.naming.QualifiedName;
import org.eclipse.xtext.xbase.validation.LogicalContainerAwareFeatureNameValidator;

/** Validator of the feature names.
 * 
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 */
public class SARLFeatureNameValidator extends LogicalContainerAwareFeatureNameValidator {

	/**
	 */
	public SARLFeatureNameValidator() {
		
	}

	/** {@inheritDoc}
	 */
	@Override
	public boolean isDisallowedName(QualifiedName name) {
		if (ModelUtil.isHiddenAction(name.getLastSegment())
			||ModelUtil.isHiddenAttribute(name.getLastSegment())) {
			return true;
		}
		return super.isDisallowedName(name);
	}
	
	/** {@inheritDoc}
	 */
	@Override
	public boolean isDiscouragedName(QualifiedName name) {
		String n = name.getLastSegment();
		if ("const".equals(n) //$NON-NLS-1$
			||"class".equals(n) //$NON-NLS-1$
			||"interface".equals(n) //$NON-NLS-1$
			||"annotation".equals(n) //$NON-NLS-1$
			||"enum".equals(n)) { //$NON-NLS-1$
			return true;
		}
		return super.isDiscouragedName(name);
	}
	
}
