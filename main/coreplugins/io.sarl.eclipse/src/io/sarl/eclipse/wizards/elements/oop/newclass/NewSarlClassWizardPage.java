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

package io.sarl.eclipse.wizards.elements.oop.newclass;

import com.google.common.base.Strings;
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
import io.sarl.lang.codebuilder.builders.ISarlClassBuilder;

/**
 * Wizard page for creating a new SARL class.
 *
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 */
public class NewSarlClassWizardPage extends AbstractNewSarlElementWizardPage {

	private static final String IMAGE_HEADER =
			"platform:/plugin/org.eclipse.jdt.ui/icons/full/wizban/newclass_wiz.png"; //$NON-NLS-1$

	/** Construct a wizard page.
	 */
	public NewSarlClassWizardPage() {
		super(CLASS_TYPE, Messages.NewSarlClassWizard_0);
		setTitle(Messages.NewSarlClassWizard_0);
		setDescription(Messages.NewSarlClassWizardPage_0);
		setImageDescriptor(SARLEclipsePlugin.getDefault().getImageDescriptor(IMAGE_HEADER));
	}

	@Override
	public void createPageControls(Composite parent) {
		createSuperClassControls(parent, COLUMNS);
		createSuperInterfacesControls(parent, COLUMNS);
		createSeparator(parent, COLUMNS);
		createMethodStubControls(parent, COLUMNS, true, true, false, false);
	}

	@Override
	protected void doStatusUpdate() {
		final IStatus[] status = new IStatus[] {
			this.fContainerStatus,
			this.fPackageStatus,
			this.fTypeNameStatus,
			this.fSuperClassStatus,
			this.fSuperInterfacesStatus,
		};
		updateStatus(status);
	}

	@Override
	protected void generateTypeContent(ISourceAppender appender, IJvmTypeProvider typeProvider,
			String comment, IProgressMonitor monitor) throws Exception {
		final SubMonitor mon = SubMonitor.convert(monitor, 3);
		final ScriptSourceAppender scriptBuilder = this.codeBuilderFactory.buildScript(
				getPackageFragment().getElementName(), typeProvider);
		final ISarlClassBuilder clazz = scriptBuilder.addSarlClass(getTypeName());
		final String superType = getSuperClass();
		// Do not add the "Object" type because it is the default.
		if (Strings.isNullOrEmpty(superType) || Object.class.getName().equals(superType)) {
			clazz.setExtends(null);
		} else {
			clazz.setExtends(superType);
		}
		for (final String type : getSuperInterfaces()) {
			clazz.addImplements(type);
		}
		clazz.setDocumentation(comment);
		mon.worked(1);
		createInheritedMembers(
			Object.class.getCanonicalName(),
			clazz.getSarlClass(),
			true,
			() -> clazz.addSarlConstructor(),
			name -> clazz.addOverrideSarlAction(name),
			superType,
			getSuperInterfaces());
		mon.worked(2);
		scriptBuilder.build(appender);
		mon.done();
	}

	@Override
	protected String getExistingElementErrorMessage() {
		return Messages.NewSarlClassWizardPage_1;
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
