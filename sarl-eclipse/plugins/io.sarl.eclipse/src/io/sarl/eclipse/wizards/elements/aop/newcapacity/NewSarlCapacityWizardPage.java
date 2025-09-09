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

package io.sarl.eclipse.wizards.elements.aop.newcapacity;

import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.SubMonitor;
import org.eclipse.jdt.core.IJavaProject;
import org.eclipse.jdt.core.IType;
import org.eclipse.jdt.core.JavaModelException;
import org.eclipse.jface.operation.IRunnableContext;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.xtext.common.types.access.IJvmTypeProvider;
import org.eclipse.xtext.xbase.compiler.ISourceAppender;

import io.sarl.eclipse.SARLEclipseConfig;
import io.sarl.eclipse.SARLEclipsePlugin;
import io.sarl.eclipse.wizards.elements.AbstractNewSarlElementWizardPage;
import io.sarl.eclipse.wizards.elements.AbstractSuperTypeSelectionDialog;
import io.sarl.eclipse.wizards.elements.SarlSpecificTypeSelectionExtension;
import io.sarl.lang.core.Capacity;

/**
 * Wizard page for creating a new SARL behavior.
 *
 * @author $Author: sgalland$
 * @version io.sarl.eclipse 0.15.0 20250909-115751
 * @mavengroupid io.sarl.eclipse
 * @mavenartifactid io.sarl.eclipse
 */
@SuppressWarnings("restriction")
public class NewSarlCapacityWizardPage extends AbstractNewSarlElementWizardPage {

	/** Construct a wizard page.
	 */
	public NewSarlCapacityWizardPage() {
		super(INTERFACE_TYPE, Messages.NewSarlCapacity_0);
		setTitle(Messages.NewSarlCapacity_0);
		setDescription(Messages.NewSarlCapacityWizardPage_0);
		setImageDescriptor(SARLEclipsePlugin.getDefault().getImageDescriptor(SARLEclipseConfig.NEW_CAPACITY_WIZARD_DIALOG_IMAGE));
	}

	@Override
	protected String getSuperInterfacesLabel() {
		return Messages.NewSarlCapacityWizardPage_3;
	}

	@Override
	public void createPageControls(Composite parent) {
		createSuperInterfacesControls(parent, COLUMNS);
	}

	@Override
	protected void doStatusUpdate() {
		final var status = new IStatus[] {
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
		final var mon = SubMonitor.convert(monitor, 2);
		final var scriptBuilder = this.codeBuilderFactory.buildScript(
				getPackageFragment().getElementName(), typeProvider);
		final var behavior = scriptBuilder.addSarlCapacity(getTypeName());
		for (final var type : getSuperInterfaces()) {
			behavior.addExtends(type);
		}
		behavior.setDocumentation(comment);
		mon.worked(1);
		scriptBuilder.build(appender);
		mon.done();
	}

	@Override
	protected String getExistingElementErrorMessage() {
		return Messages.NewSarlCapacityWizardPage_1;
	}

	@Override
	protected String getInvalidInterfaceTypeErrorMessage() {
		return Messages.NewSarlCapacityWizardPage_2;
	}

	@Override
	protected IType getRootSuperInterface() throws JavaModelException {
		return getJavaProject().findType(Capacity.class.getName());
	}

	@Override
	protected AbstractSuperTypeSelectionDialog<?> createSuperInterfaceSelectionDialog(Shell parent,
			IRunnableContext context, IJavaProject project, SarlSpecificTypeSelectionExtension extension,
			boolean multi) {
		return new SuperCapacitySelectionDialog<>(parent, context, project, this, extension, multi);
	}

}
