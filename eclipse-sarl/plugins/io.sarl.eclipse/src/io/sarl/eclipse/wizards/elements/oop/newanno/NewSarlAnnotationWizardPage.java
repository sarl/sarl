/*
 * $Id$
 *
 * SARL is an general-purpose agent programming language.
 * More details on http://www.sarl.io
 *
 * Copyright (C) 2014-2016 the original authors or authors.
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

package io.sarl.eclipse.wizards.elements.oop.newanno;

import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.emf.common.notify.Notifier;
import org.eclipse.emf.ecore.resource.Resource;
import org.eclipse.jdt.core.IType;
import org.eclipse.jdt.core.JavaModelException;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.xtext.common.types.access.IJvmTypeProvider;
import org.eclipse.xtext.xbase.compiler.ISourceAppender;

import io.sarl.eclipse.SARLEclipsePlugin;
import io.sarl.eclipse.wizards.elements.AbstractNewSarlElementWizardPage;
import io.sarl.lang.codebuilder.builders.IScriptBuilder;

/**
 * Wizard page for creating a new SARL annotation.
 *
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 */
public class NewSarlAnnotationWizardPage extends AbstractNewSarlElementWizardPage {

	private static final String IMAGE_HEADER =
			"platform:/plugin/org.eclipse.jdt.ui/icons/full/wizban/newannotation_wiz.png"; //$NON-NLS-1$

	/** Construct a wizard page.
	 */
	public NewSarlAnnotationWizardPage() {
		super(CLASS_TYPE, Messages.NewSarlAnnotationWizard_0);
		setTitle(Messages.NewSarlAnnotationWizard_0);
		setDescription(Messages.NewSarlAnnotationWizardPage_0);
		setImageDescriptor(SARLEclipsePlugin.getDefault().getImageDescriptor(IMAGE_HEADER));
	}

	@Override
	public void createPageControls(Composite parent) {
		//
	}

	@Override
	protected void doStatusUpdate() {
		final IStatus[] status = new IStatus[] {
			this.fContainerStatus,
			this.fPackageStatus,
			this.fTypeNameStatus,
		};
		updateStatus(status);
	}

	@Override
	protected void generateTypeContent(ISourceAppender appender, IJvmTypeProvider typeProvider,
			IProgressMonitor monitor) throws CoreException {
//		final IScriptBuilder scriptBuilder = this.codeBuilderFactory.createScript(
//				getPackageFragment().getElementName(), ecoreResource);
//		scriptBuilder.addSarlAnnotationType(getTypeName());
//		scriptBuilder.finalizeScript();
	}

	@Override
	protected String getExistingElementErrorMessage() {
		return Messages.NewSarlAnnotationWizardPage_1;
	}

	@Override
	protected String getInvalidSubtypeErrorMessage() {
		return null;
	}

	@Override
	protected IType getRootSuperType() throws JavaModelException {
		return getJavaProject().findType(Object.class.getName());
	}

}
