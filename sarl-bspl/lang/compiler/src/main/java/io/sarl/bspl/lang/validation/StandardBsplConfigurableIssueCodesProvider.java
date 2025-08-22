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

package io.sarl.bspl.lang.validation;

import org.eclipse.xtext.preferences.PreferenceKey;
import org.eclipse.xtext.util.IAcceptor;
import org.eclipse.xtext.validation.SeverityConverter;
import org.eclipse.xtext.xbase.validation.XbaseConfigurableIssueCodes;

import com.google.inject.Singleton;

/**
 * Provider of issues that could be configured by the user.
 *
 * @author $Author: sgalland$
 * @author $Author: stedeschi$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 * @since 0.15
 */
@Singleton
public class StandardBsplConfigurableIssueCodesProvider extends XbaseConfigurableIssueCodes {

	/** Construct a provider of issue codes for Xtext tools.
	 */
	public StandardBsplConfigurableIssueCodesProvider() {
		//
	}

	@Override
	protected void initialize(IAcceptor<PreferenceKey> acceptor) {
		super.initialize(acceptor);

		// Override the Xbase configuration
		acceptor.accept(create(
				org.eclipse.xtext.xbase.validation.IssueCodes.VARIABLE_NAME_SHADOWING,
				SeverityConverter.SEVERITY_WARNING));
		acceptor.accept(create(
				org.eclipse.xtext.xbase.validation.IssueCodes.VARIABLE_NAME_DISCOURAGED,
				SeverityConverter.SEVERITY_WARNING));
		acceptor.accept(create(
				org.eclipse.xtext.xbase.validation.IssueCodes.RAW_TYPE,
				SeverityConverter.SEVERITY_WARNING));

		// Add warnings from SARL BSPL
		acceptor.accept(create(
				IssueCodes.DUPLICATE_PROTOCOL_MESSAGE,
				SeverityConverter.SEVERITY_WARNING));

		acceptor.accept(create(
				IssueCodes.DUPLICATE_PROTOCOL_ROLE,
				SeverityConverter.SEVERITY_WARNING));

		acceptor.accept(create(
				IssueCodes.MISSED_PARAMETER_TYPE,
				SeverityConverter.SEVERITY_WARNING));

		acceptor.accept(create(
				IssueCodes.EMPTY_PACKAGE_DECLARATION,
				SeverityConverter.SEVERITY_WARNING));

		acceptor.accept(create(
				IssueCodes.UNEXPECTED_PACKAGE_DECLARATION,
				SeverityConverter.SEVERITY_WARNING));

		acceptor.accept(create(
				IssueCodes.UNNECESSARY_ROLE_CARDINALITY,
				SeverityConverter.SEVERITY_WARNING));

		acceptor.accept(create(
				IssueCodes.INVALID_ROLE_CARDINALITY_ORDER,
				SeverityConverter.SEVERITY_WARNING));

		acceptor.accept(create(
				IssueCodes.MISSED_ARGUMENT_IN_MESSAGE,
				SeverityConverter.SEVERITY_WARNING));
	}

}
