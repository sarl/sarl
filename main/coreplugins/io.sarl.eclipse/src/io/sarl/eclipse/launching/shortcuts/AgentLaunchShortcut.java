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

package io.sarl.eclipse.launching.shortcuts;

import javax.inject.Inject;

import com.google.common.base.Strings;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.debug.core.ILaunchConfiguration;
import org.eclipse.emf.common.util.URI;
import org.eclipse.swt.graphics.Image;
import org.eclipse.xtext.common.types.JvmVisibility;
import org.eclipse.xtext.naming.IQualifiedNameProvider;

import io.sarl.eclipse.SARLEclipsePlugin;
import io.sarl.eclipse.launching.config.ILaunchConfigurationAccessor;
import io.sarl.lang.core.Agent;
import io.sarl.lang.sarl.SarlAgent;
import io.sarl.lang.ui.labeling.SARLImages;

/** Shortcut for launching a SARL agent.
 *
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 */
@SuppressWarnings("checkstyle:classfanoutcomplexity")
public class AgentLaunchShortcut extends AbstractSarlLaunchShortcut<SarlAgent, Agent> {

	@Inject
	private IQualifiedNameProvider nameProvider;

	@Inject
	private ILaunchConfigurationAccessor accessor;

	@Inject
	private SARLImages images;

	@Override
	protected Class<SarlAgent> getValidEObjectType() {
		return SarlAgent.class;
	}

	@Override
	protected Class<Agent> getValidJavaType() {
		return Agent.class;
	}

	@Override
	protected String getConfigurationType() {
		return this.accessor.getAgentLaunchConfigurationType();
	}

	@Override
	protected String getElementQualifiedName(ILaunchConfiguration configuration) {
		return this.accessor.getAgent(configuration);
	}

	@Override
	protected URI getResourceURIForValidEObject(Object object) {
		if (object instanceof SarlAgent) {
			final SarlAgent agent = (SarlAgent) object;
			return agent.eResource().getURI();
		}
		return null;
	}

	@Override
	protected String getQualifiedNameFor(SarlAgent element) {
		return this.nameProvider.getFullyQualifiedName(element).toString();
	}

	@Override
	protected Image getElementImage(Object element) {
		return this.images.forAgent(JvmVisibility.PRIVATE, 0).createImage();
	}

	@Override
	protected ILaunchConfiguration createConfiguration(String projectName, String fullyQualifiedNameOfAgent) {
		try {
			return this.configurator.newAgentLaunchConfiguration(projectName, fullyQualifiedNameOfAgent);
		} catch (CoreException exception) {
			SARLEclipsePlugin.getDefault().openError(getShell(),
					io.sarl.eclipse.util.Messages.AbstractSarlScriptInteractiveSelector_1,
					exception.getStatus().getMessage(), null, exception);
			return null;
		}
	}

	@Override
	protected boolean isSelectableElement(SarlAgent element) {
		return element != null && !Strings.isNullOrEmpty(element.getName());
	}

	@Override
	protected String getElementLabel() {
		return Messages.AgentLaunchShortcut_0;
	}

	@Override
	protected String getElementsLabel() {
		return Messages.AgentLaunchShortcut_1;
	}

	@Override
	protected String getElementLongLabel() {
		return Messages.AgentLaunchShortcut_2;
	}

}
