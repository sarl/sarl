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

package io.sarl.lang.ui.bugfixes.pending.xtexteclipse282;

import java.util.Set;

import org.eclipse.jface.dialogs.IDialogSettings;
import org.eclipse.jface.layout.PixelConverter;
import org.eclipse.swt.SWT;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.ui.forms.widgets.ExpandableComposite;
import org.eclipse.xtext.builder.internal.Activator;
import org.eclipse.xtext.generator.OutputConfiguration;
import org.eclipse.xtext.ui.preferences.ScrolledPageContent;

import io.sarl.lang.ui.preferences.SARLBuilderConfigurationBlock;

/** See xtext-eclipse/#282: Add getOutputConfiguration function in BuilderConfigurationBlock.
 *
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 * @see "https://github.com/eclipse/xtext-eclipse/pull/282"
 */
public class Issue282BuilderConfigurationBlock extends SARLBuilderConfigurationBlock {

	@Override
	@SuppressWarnings("checkstyle:magicnumber")
	protected Control doCreateContents(Composite parent) {
		final PixelConverter pixelConverter = new PixelConverter(parent);
		setShell(parent.getShell());
		final Composite mainComp = new Composite(parent, SWT.NONE);
		mainComp.setFont(parent.getFont());
		final GridLayout layout = new GridLayout();
		layout.marginHeight = 0;
		layout.marginWidth = 0;
		mainComp.setLayout(layout);
		final Composite othersComposite = createBuildPathTabContent(mainComp);
		final GridData gridData = new GridData(GridData.FILL, GridData.FILL, true, true);
		gridData.heightHint = pixelConverter.convertHeightInCharsToPixels(20);
		othersComposite.setLayoutData(gridData);
		validateSettings(null, null, null);
		return mainComp;
	}

	private Composite createBuildPathTabContent(Composite parent) {
		final int columns = 3;
		final ScrolledPageContent pageContent = new ScrolledPageContent(parent);
		final GridLayout layout = new GridLayout();
		layout.numColumns = columns;
		layout.marginHeight = 0;
		layout.marginWidth = 0;

		final Composite composite = pageContent.getBody();
		composite.setLayout(layout);
		String label = org.eclipse.xtext.builder.preferences.Messages.BuilderConfigurationBlock_GeneralSection_Label;
		ExpandableComposite excomposite = createStyleSection(composite, label, columns);

		Composite othersComposite = new Composite(excomposite, SWT.NONE);
		excomposite.setClient(othersComposite);
		othersComposite.setLayout(new GridLayout(columns, false));

		createGeneralSectionItems(othersComposite);

		final Set<OutputConfiguration> outputConfigurations = getOutputConfigurations(getProject());

		for (final OutputConfiguration outputConfiguration : outputConfigurations) {
			label = outputConfiguration.getDescription();
			excomposite = createStyleSection(composite, label, columns);
			othersComposite = new Composite(excomposite, SWT.NONE);
			excomposite.setClient(othersComposite);
			othersComposite.setLayout(new GridLayout(columns, false));

			createOutputSectionItems(othersComposite, outputConfiguration);
		}
		registerKey(getIsProjectSpecificPropertyKey(getPropertyPrefix()));
		final IDialogSettings section = Activator.getDefault().getDialogSettings().getSection(SETTINGS_SECTION_NAME);
		restoreSectionExpansionStates(section);
		return pageContent;
	}

}
