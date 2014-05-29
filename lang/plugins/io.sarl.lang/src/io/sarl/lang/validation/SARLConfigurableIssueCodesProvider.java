/*
 * Copyright 2014 Sebastian RODRIGUEZ, Nicolas GAUD, Stéphane GALLAND.
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */
package io.sarl.lang.validation;

import org.eclipse.xtext.preferences.PreferenceKey;
import org.eclipse.xtext.util.IAcceptor;
import org.eclipse.xtext.validation.SeverityConverter;
import org.eclipse.xtext.xbase.validation.XbaseConfigurableIssueCodes;

/**
 * Provider of issues that could be configured by the user.
 * 
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 */
public class SARLConfigurableIssueCodesProvider extends XbaseConfigurableIssueCodes {

	/**
	 */
	public SARLConfigurableIssueCodesProvider() {
		//
	}
	
	@Override
	protected void initialize(IAcceptor<PreferenceKey> iAcceptor) {
		super.initialize(iAcceptor);
		
		// Override the Xbase configuration
		iAcceptor.accept(create(org.eclipse.xtext.xbase.validation.IssueCodes.UNREACHABLE_CODE, SeverityConverter.SEVERITY_ERROR));

		// Add warnings from SARL
		iAcceptor.accept(create(IssueCodes.FIELD_NAME_SHADOWING, SeverityConverter.SEVERITY_WARNING));
		iAcceptor.accept(create(IssueCodes.REDUNDANT_INTERFACE_IMPLEMENTATION, SeverityConverter.SEVERITY_ERROR));
		iAcceptor.accept(create(IssueCodes.DISCOURAGED_BOOLEAN_EXPRESSION, SeverityConverter.SEVERITY_ERROR));
	}

}
