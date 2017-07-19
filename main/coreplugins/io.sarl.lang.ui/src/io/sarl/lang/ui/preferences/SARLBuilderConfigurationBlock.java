/*
 * $Id$
 *
 * SARL is an general-purpose agent programming language.
 * More details on http://www.sarl.io
 *
 * Copyright (C) 2014-2017 the original authors or authors.
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

package io.sarl.lang.ui.preferences;

import static io.sarl.lang.ui.preferences.SARLBuilderPreferenceAccess.PREF_GENERATE_INLINE;
import static io.sarl.lang.ui.preferences.SARLBuilderPreferenceAccess.PREF_GENERATE_PURE;
import static io.sarl.lang.ui.preferences.SARLBuilderPreferenceAccess.PREF_USE_EXPRESSION_INTERPRETER;

import java.util.Set;

import com.google.common.collect.Sets;
import com.google.inject.Inject;
import org.eclipse.core.resources.IProject;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.xtext.builder.EclipseOutputConfigurationProvider;
import org.eclipse.xtext.generator.OutputConfiguration;
import org.eclipse.xtext.xbase.ui.builder.XbaseBuilderConfigurationBlock;

import io.sarl.lang.generator.extra.ExtraLanguageOutputConfigurations;

/** Preference page that permits to configure the SARL builder.
 *
 * <p>This page extends the Xbase page.
 *
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 */
public class SARLBuilderConfigurationBlock extends XbaseBuilderConfigurationBlock {

	@Inject
	private EclipseOutputConfigurationProvider configurationProvider;

	@Override
	protected void createGeneralSectionItems(Composite composite) {
		super.createGeneralSectionItems(composite);

		final Button generateInlineButton = addCheckBox(composite, Messages.SARLBuilderConfigurationBlock_0,
				PREF_GENERATE_INLINE, BOOLEAN_VALUES, 0);

		final Button useExpressionInterpreter = addCheckBox(composite, Messages.SARLBuilderConfigurationBlock_1,
				PREF_USE_EXPRESSION_INTERPRETER, BOOLEAN_VALUES, INDENT_AMOUNT);
		useExpressionInterpreter.setEnabled(generateInlineButton.getSelection());

		addCheckBox(composite, Messages.SARLBuilderConfigurationBlock_2,
				PREF_GENERATE_PURE, BOOLEAN_VALUES, 0);

		generateInlineButton.addSelectionListener(new SelectionAdapter() {
			@Override
			public void widgetSelected(SelectionEvent event) {
				useExpressionInterpreter.setEnabled(generateInlineButton.getSelection());
			}
		});
	}

	/** Replies the output configurations for the given project.
	 *
	 * @param project the project.
	 * @return the output configurations associated to the given project.
	 */
	protected Set<OutputConfiguration> getOutputConfigurations(IProject project) {
		final Set<OutputConfiguration> original = this.configurationProvider.getOutputConfigurations(getProject());
		return Sets.filter(original, (it) -> !ExtraLanguageOutputConfigurations.isExtraLanguageOutputConfiguration(it.getName()));
	}

}
