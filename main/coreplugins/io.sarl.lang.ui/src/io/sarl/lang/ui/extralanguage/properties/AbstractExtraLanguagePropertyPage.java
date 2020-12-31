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

package io.sarl.lang.ui.extralanguage.properties;

import com.google.inject.Inject;
import com.google.inject.name.Named;
import org.eclipse.core.resources.IProject;
import org.eclipse.core.runtime.IAdaptable;
import org.eclipse.jface.preference.IPreferencePageContainer;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.ui.preferences.IWorkbenchPreferenceContainer;
import org.eclipse.xtext.Constants;
import org.eclipse.xtext.ui.preferences.PropertyAndPreferencePage;

/** Abstract property page for configuring an extra language generator.
 *
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 * @since 0.6
 */
public abstract class AbstractExtraLanguagePropertyPage extends PropertyAndPreferencePage {

	private AbstractGeneratorConfigurationBlock builderConfigurationBlock;

	private String languageName;

	/** Set the language name.
	 *
	 * @param languageName the name.
	 */
	@Inject
	public void setLanguageName(@Named(Constants.LANGUAGE_NAME) String languageName) {
		this.languageName = languageName;
	}

	/** Change the configuration block.
	 *
	 * @param block the block.
	 */
	protected final void setInternalConfigurationBlock(AbstractGeneratorConfigurationBlock block) {
		assert block != null;
		this.builderConfigurationBlock = block;
	}

	@Override
	public void createControl(Composite parent) {
		final IWorkbenchPreferenceContainer container = (IWorkbenchPreferenceContainer) getContainer();
		this.builderConfigurationBlock.setProject(getProject());
		this.builderConfigurationBlock.setWorkbenchPreferenceContainer(container);
		this.builderConfigurationBlock.setStatusChangeListener(getNewStatusChangedListener());
		super.createControl(parent);
	}

	@Override
	protected Control createPreferenceContent(Composite composite, IPreferencePageContainer preferencePageContainer) {
		return this.builderConfigurationBlock.createContents(composite);
	}

	@Override
	protected boolean hasProjectSpecificOptions(IProject project) {
		return this.builderConfigurationBlock.hasProjectSpecificOptions(project);
	}

	/** Replies the identifier of the page for a extra language generator.
	 *
	 * @return the identifier.
	 */
	protected abstract String getPreferenceContainerID();

	@Override
	protected String getPreferencePageID() {
		return this.languageName + ".compiler.extra." + getPreferenceContainerID() + ".preferencePage"; //$NON-NLS-1$//$NON-NLS-2$
	}

	@Override
	protected String getPropertyPageID() {
		return this.languageName + ".compiler.extra." + getPreferenceContainerID() + ".propertyPage"; //$NON-NLS-1$//$NON-NLS-2$
	}

	@Override
	public void dispose() {
		if (this.builderConfigurationBlock != null) {
			this.builderConfigurationBlock.dispose();
		}
		super.dispose();
	}

	@Override
	protected void enableProjectSpecificSettings(boolean useProjectSpecificSettings) {
		super.enableProjectSpecificSettings(useProjectSpecificSettings);
		if (this.builderConfigurationBlock != null) {
			this.builderConfigurationBlock.useProjectSpecificSettings(useProjectSpecificSettings);
		}
	}

	@Override
	protected void performDefaults() {
		super.performDefaults();
		if (this.builderConfigurationBlock != null) {
			this.builderConfigurationBlock.performDefaults();
		}
	}

	@Override
	public boolean performOk() {
		if (this.builderConfigurationBlock != null) {
			if (!this.builderConfigurationBlock.performOk()) {
				return false;
			}
		}
		return super.performOk();
	}

	@Override
	public void performApply() {
		if (this.builderConfigurationBlock != null) {
			this.builderConfigurationBlock.performApply();
		}
	}

	@Override
	public void setElement(IAdaptable element) {
		super.setElement(element);
		// no description for property page
		setDescription(null);
	}

}
