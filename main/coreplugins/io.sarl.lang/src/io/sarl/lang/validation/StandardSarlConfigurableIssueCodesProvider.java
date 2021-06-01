/*
 * $Id$
 *
 * SARL is an general-purpose agent programming language.
 * More details on http://www.sarl.io
 *
 * Copyright (C) 2014-2021 the original authors or authors.
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

import javax.inject.Singleton;

import org.eclipse.xtend.core.validation.XtendConfigurableIssueCodes;
import org.eclipse.xtext.preferences.PreferenceKey;
import org.eclipse.xtext.util.IAcceptor;
import org.eclipse.xtext.validation.SeverityConverter;

/**
 * Provider of issues that could be configured by the user.
 *
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 */
@Singleton
public class StandardSarlConfigurableIssueCodesProvider extends XtendConfigurableIssueCodes {

	/** Construct a provider of issue codes for Xtext tools.
	 */
	public StandardSarlConfigurableIssueCodesProvider() {
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

		// Override the Xtend configuration
		acceptor.accept(create(
				org.eclipse.xtend.core.validation.IssueCodes.WRONG_PACKAGE,
				SeverityConverter.SEVERITY_WARNING));
		acceptor.accept(create(
				org.eclipse.xtend.core.validation.IssueCodes.MISSING_OVERRIDE,
				SeverityConverter.SEVERITY_IGNORE));
		acceptor.accept(create(
				org.eclipse.xtend.core.validation.IssueCodes.MISSING_ABSTRACT,
				SeverityConverter.SEVERITY_WARNING));

		// Add warnings from SARL
		acceptor.accept(create(
				IssueCodes.REDUNDANT_INTERFACE_IMPLEMENTATION,
				SeverityConverter.SEVERITY_WARNING));

		acceptor.accept(create(
				IssueCodes.DISCOURAGED_BOOLEAN_EXPRESSION,
				SeverityConverter.SEVERITY_WARNING));

		acceptor.accept(create(
				IssueCodes.DISCOURAGED_CAPACITY_DEFINITION,
				SeverityConverter.SEVERITY_WARNING));

		acceptor.accept(create(
				IssueCodes.DISCOURAGED_FUNCTION_NAME,
				SeverityConverter.SEVERITY_WARNING));

		acceptor.accept(create(
				IssueCodes.UNREACHABLE_BEHAVIOR_UNIT,
				SeverityConverter.SEVERITY_WARNING));

		acceptor.accept(create(
				IssueCodes.UNUSED_AGENT_CAPACITY,
				SeverityConverter.SEVERITY_WARNING));

		acceptor.accept(create(
				IssueCodes.REDUNDANT_CAPACITY_USE,
				SeverityConverter.SEVERITY_WARNING));

		acceptor.accept(create(
				IssueCodes.RETURN_TYPE_SPECIFICATION_IS_RECOMMENDED,
				SeverityConverter.SEVERITY_WARNING));

		acceptor.accept(create(
				IssueCodes.DISCOURAGED_OCCURRENCE_READONLY_USE,
				SeverityConverter.SEVERITY_WARNING));

		acceptor.accept(create(
				IssueCodes.USED_RESERVED_SARL_ANNOTATION,
				SeverityConverter.SEVERITY_WARNING));

		acceptor.accept(create(
				IssueCodes.MANUAL_INLINE_DEFINITION,
				SeverityConverter.SEVERITY_WARNING));

		acceptor.accept(create(
				IssueCodes.DISCOURAGED_LOOP_BREAKING_KEYWORD_USE,
				SeverityConverter.SEVERITY_WARNING));

		acceptor.accept(create(
				IssueCodes.POTENTIAL_FIELD_SYNCHRONIZATION_PROBLEM,
				SeverityConverter.SEVERITY_WARNING));

		acceptor.accept(create(
				IssueCodes.POTENTIAL_INEFFICIENT_VALUE_CONVERSION,
				SeverityConverter.SEVERITY_WARNING));

		acceptor.accept(create(
				IssueCodes.PARAMETER_DEFAULT_VALUE_REDFINITION,
				SeverityConverter.SEVERITY_WARNING));

		acceptor.accept(create(
				IssueCodes.POTENTIAL_MEMORY_SHARING_OUTSIDE_AGENT_CONTROL,
				SeverityConverter.SEVERITY_WARNING));
	}

}
