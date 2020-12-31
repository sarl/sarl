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

package io.sarl.eclipse.wizards.elements.oop.newinterface;

import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.SubMonitor;
import org.eclipse.jdt.core.IType;
import org.eclipse.jdt.core.JavaModelException;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.xtext.common.types.access.IJvmTypeProvider;
import org.eclipse.xtext.xbase.compiler.ISourceAppender;

import io.sarl.eclipse.SARLEclipsePlugin;
import io.sarl.eclipse.wizards.elements.AbstractNewSarlElementWizardPage;
import io.sarl.lang.codebuilder.appenders.ScriptSourceAppender;
import io.sarl.lang.codebuilder.builders.ISarlInterfaceBuilder;

/**
 * Wizard page for creating a new SARL class.
 *
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 */
public class NewSarlInterfaceWizardPage extends AbstractNewSarlElementWizardPage {

	private static final String IMAGE_HEADER =
			"platform:/plugin/org.eclipse.jdt.ui/icons/full/wizban/newint_wiz.png"; //$NON-NLS-1$

	/** Construct a wizard page.
	 */
	public NewSarlInterfaceWizardPage() {
		super(INTERFACE_TYPE, Messages.NewSarlInterfaceWizard_0);
		setTitle(Messages.NewSarlInterfaceWizard_0);
		setDescription(Messages.NewSarlInterfaceWizardPage_0);
		setImageDescriptor(SARLEclipsePlugin.getDefault().getImageDescriptor(IMAGE_HEADER));
	}

	@Override
	public void createPageControls(Composite parent) {
		createSuperInterfacesControls(parent, COLUMNS);
	}

	@Override
	protected void doStatusUpdate() {
		final IStatus[] status = new IStatus[] {
			this.fContainerStatus,
			this.fPackageStatus,
			this.fTypeNameStatus,
			this.fSuperInterfacesStatus,
		};
		updateStatus(status);
	}

	@Override
	protected void generateTypeContent(ISourceAppender appender, IJvmTypeProvider typeProvider,
			String comment, IProgressMonitor monitor) throws Exception {
		final SubMonitor mon = SubMonitor.convert(monitor, 2);
		final ScriptSourceAppender scriptBuilder = this.codeBuilderFactory.buildScript(
				getPackageFragment().getElementName(), typeProvider);
		final ISarlInterfaceBuilder inter = scriptBuilder.addSarlInterface(getTypeName());
		for (final String type : getSuperInterfaces()) {
			inter.addExtends(type);
		}
		inter.setDocumentation(comment);
		mon.worked(1);
		scriptBuilder.build(appender);
		mon.done();
	}

	@Override
	protected String getExistingElementErrorMessage() {
		return Messages.NewSarlInterfaceWizardPage_1;
	}

	@Override
	protected String getInvalidSubtypeErrorMessage() {
		return null;
	}

	@Override
	protected IType getRootSuperInterface() throws JavaModelException {
		return getJavaProject().findType(Object.class.getName());
	}

}
