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

package io.sarl.eclipse.wizards.elements.aop.newbehavior;

import java.io.IOException;
import java.util.Collections;
import java.util.Comparator;
import java.util.Map;

import com.google.common.collect.Maps;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.jdt.core.IJavaProject;
import org.eclipse.jdt.core.IMethod;
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
import io.sarl.lang.actionprototype.ActionPrototype;
import io.sarl.lang.codebuilder.appenders.ScriptSourceAppender;
import io.sarl.lang.codebuilder.builders.ISarlBehaviorBuilder;
import io.sarl.lang.core.Behavior;

/**
 * Wizard page for creating a new SARL behavior.
 *
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 */
public class NewSarlBehaviorWizardPage extends AbstractNewSarlElementWizardPage {

	/** Construct a wizard page.
	 */
	public NewSarlBehaviorWizardPage() {
		super(CLASS_TYPE, Messages.NewSarlBehavior_0);
		setTitle(Messages.NewSarlBehavior_0);
		setDescription(Messages.NewSarlBehaviorWizardPage_0);
		setImageDescriptor(SARLEclipsePlugin.getDefault().getImageDescriptor(SARLEclipseConfig.NEW_BEHAVIOR_WIZARD_DIALOG_IMAGE));
	}

	@Override
	public void createPageControls(Composite parent) {
		createSuperClassControls(parent, COLUMNS);
		createSeparator(parent, COLUMNS);
		createMethodStubControls(parent, COLUMNS, true, true);
	}

	@Override
	protected void doStatusUpdate() {
		final IStatus[] status = new IStatus[] {
			this.fContainerStatus,
			this.fPackageStatus,
			this.fTypeNameStatus,
			this.fSuperClassStatus,
		};
		updateStatus(status);
	}

	@Override
	protected void generateTypeContent(ISourceAppender appender, IJvmTypeProvider typeProvider,
			IProgressMonitor monitor) throws CoreException {
		final ScriptSourceAppender scriptBuilder = this.codeBuilderFactory.buildScript(
				getPackageFragment().getElementName(), typeProvider);
		final ISarlBehaviorBuilder behavior = scriptBuilder.addSarlBehavior(getTypeName());
		behavior.setExtends(getSuperClass());

		if (isCreateInherited()) {
			final Map<ActionPrototype, IMethod> operationsToImplement = Maps.newTreeMap((Comparator<ActionPrototype>) null);
			this.jdt2sarl.populateInheritanceContext(
					this.jdt2sarl.toTypeFinder(getJavaProject()),
					// Discarding final operation.
					null,
					// Discarding overridable operation.
					null,
					// Discarding inherited fields,
					null,
					operationsToImplement,
					// Discarding super constructors,
					null,
					getSuperClass(),
					Collections.<String>emptyList());
			this.jdt2sarl.createActions(behavior, operationsToImplement.values());
		}

		try {
			scriptBuilder.build(appender);
		} catch (IOException exception) {
			throw new CoreException(SARLEclipsePlugin.getDefault().createStatus(IStatus.ERROR, exception));
		}
		monitor.done();
	}

	@Override
	protected String getExistingElementErrorMessage() {
		return Messages.NewSarlBehaviorWizardPage_1;
	}

	@Override
	protected String getInvalidSubtypeErrorMessage() {
		return Messages.NewSarlBehaviorWizardPage_2;
	}

	@Override
	protected IType getRootSuperType() throws JavaModelException {
		return getJavaProject().findType(Behavior.class.getName());
	}

	@Override
	protected AbstractSuperTypeSelectionDialog<?> createSuperClassSelectionDialog(Shell parent,
			IRunnableContext context, IJavaProject project, SarlSpecificTypeSelectionExtension extension,
			boolean multi) {
		return new SuperBehaviorSelectionDialog(parent, context, project, this, extension, multi);
	}

}
