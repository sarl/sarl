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
import org.eclipse.jdt.internal.ui.JavaPluginImages;
import org.eclipse.swt.graphics.Image;
import org.eclipse.xtext.naming.IQualifiedNameProvider;

import io.sarl.eclipse.SARLEclipsePlugin;
import io.sarl.eclipse.launching.config.ILaunchConfigurationAccessor;
import io.sarl.lang.sarl.SarlClass;

/** Shortcut for launching a SARL application.
 *
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 * @since 0.6
 */
public class ApplicationLaunchShortcut extends AbstractSarlLaunchShortcut<SarlClass, Object> {

	@Inject
	private IQualifiedNameProvider nameProvider;

	@Inject
	private ILaunchConfigurationAccessor accessor;

	@Override
	protected URI getResourceURIForValidEObject(Object object) {
		if (object instanceof SarlClass) {
			final SarlClass clazz = (SarlClass) object;
			return clazz.eResource().getURI();
		}
		return null;
	}

	@Override
	protected Image getElementImage(Object element) {
		return JavaPluginImages.DESC_OBJS_CLASS.createImage();
	}

	@Override
	protected Class<SarlClass> getValidEObjectType() {
		return SarlClass.class;
	}

	@Override
	protected Class<Object> getValidJavaType() {
		return Object.class;
	}

	@Override
	protected String getQualifiedNameFor(SarlClass element) {
		return this.nameProvider.getFullyQualifiedName(element).toString();
	}

	@Override
	protected String getConfigurationType() {
		return this.accessor.getApplicationLaunchConfigurationType();
	}

	@Override
	protected String getElementQualifiedName(ILaunchConfiguration configuration) {
		return this.accessor.getMain(configuration);
	}

	@Override
	protected ILaunchConfiguration createConfiguration(String projectName, String fullyQualifiedName) {
		try {
			return this.configurator.newApplicationLaunchConfiguration(projectName, fullyQualifiedName,
					SarlStandardClasspathProvider.class);
		} catch (CoreException exception) {
			SARLEclipsePlugin.getDefault().openError(getShell(),
					io.sarl.eclipse.util.Messages.AbstractSarlScriptInteractiveSelector_1,
					exception.getStatus().getMessage(), null, exception);
			return null;
		}
	}

	@Override
	protected boolean isSelectableElement(SarlClass element) {
		return element != null && !Strings.isNullOrEmpty(element.getName());
	}

	@Override
	protected String getElementLabel() {
		return Messages.ApplicationLaunchShortcut_0;
	}

	@Override
	protected String getElementsLabel() {
		return Messages.ApplicationLaunchShortcut_1;
	}

	@Override
	protected String getElementLongLabel() {
		return Messages.ApplicationLaunchShortcut_2;
	}

}
