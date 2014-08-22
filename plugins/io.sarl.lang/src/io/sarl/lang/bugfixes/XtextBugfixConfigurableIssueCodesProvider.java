/*
 * $Id$
 *
 * SARL is an general-purpose agent programming language.
 * More details on http://www.sarl.io
 *
 * Copyright (C) 2014 Sebastian RODRIGUEZ, Nicolas GAUD, Stéphane GALLAND.
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
package io.sarl.lang.bugfixes;

import org.eclipse.xtext.preferences.PreferenceKey;
import org.eclipse.xtext.util.IAcceptor;
import org.eclipse.xtext.xbase.validation.XbaseConfigurableIssueCodes;

/**
 * Provider of issues that could be configured by the user.
 * Implementation of a provider of issues that is fixing several bugs in the Xtext API.
 * The code defined in this class should be sent to the Xtext project as patches.
 * <p>
 * <ul>
 * <li>Deprecated: {@link "https://bugs.eclipse.org/bugs/show_bug.cgi?id=437689"}</li>
 * </ul>
 *
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 */
public class XtextBugfixConfigurableIssueCodesProvider extends XbaseConfigurableIssueCodes {

	/* Constants are copied from org.eclipse.jdt.core.JavaCore to solve the dependency to jdt.core*/
	private static final String COMPILER_PB_DEPRECATION = JDT_CORE_PLUGIN_ID
		+ ".compiler.problem.deprecation"; //$NON-NLS-1$
	private static final String COMPILER_PB_DEPRECATION_IN_DEPRECATED_CODE = JDT_CORE_PLUGIN_ID
		+ ".compiler.problem.deprecationInDeprecatedCode"; //$NON-NLS-1$

	/** Construct a provider of issue codes for Xtext tools.
	 */
	public XtextBugfixConfigurableIssueCodesProvider() {
		//
	}

	@Override
	protected void initialize(IAcceptor<PreferenceKey> iAcceptor) {
		super.initialize(iAcceptor);
		iAcceptor.accept(createDelegate(
				io.sarl.lang.bugfixes.IssueCodes.DEPRECATED_FEATURE,
				COMPILER_PB_DEPRECATION));
		iAcceptor.accept(createDelegate(
				io.sarl.lang.bugfixes.IssueCodes.DEPRECATION_IN_DEPRECATED_CODE,
				COMPILER_PB_DEPRECATION_IN_DEPRECATED_CODE));
	}

}
