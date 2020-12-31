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

package io.sarl.eclipse.wizards.elements.aop.newskill;

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
import io.sarl.eclipse.wizards.elements.aop.newcapacity.SuperCapacitySelectionDialog;
import io.sarl.lang.codebuilder.appenders.ScriptSourceAppender;
import io.sarl.lang.codebuilder.builders.ISarlSkillBuilder;
import io.sarl.lang.core.Capacity;
import io.sarl.lang.core.Skill;

/**
 * Wizard page for creating a new SARL skill.
 *
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 */
public class NewSarlSkillWizardPage extends AbstractNewSarlElementWizardPage {

	/** Construct a wizard page for creating a SARL skill.
	 */
	public NewSarlSkillWizardPage() {
		super(CLASS_TYPE, Messages.NewSarlSkill_0);
		setTitle(Messages.NewSarlSkill_0);
		setDescription(Messages.NewSarlSkillPage_0);
		setImageDescriptor(SARLEclipsePlugin.getDefault().getImageDescriptor(SARLEclipseConfig.NEW_SKILL_WIZARD_DIALOG_IMAGE));
	}

	@Override
	protected String getSuperClassLabel() {
		return Messages.NewSarlSkillWizardPage_4;
	}

	@Override
	protected String getSuperInterfacesLabel() {
		return Messages.NewSarlSkillWizardPage_5;
	}

	@Override
	public void createPageControls(Composite parent) {
		createSuperClassControls(parent, COLUMNS);
		createSuperInterfacesControls(parent, COLUMNS);
		createSeparator(parent, COLUMNS);
		createMethodStubControls(parent, COLUMNS, true, true, false, true);
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

	@SuppressWarnings("all")
	@Override
	protected void generateTypeContent(ISourceAppender appender, IJvmTypeProvider typeProvider,
			String comment, IProgressMonitor monitor) throws Exception {
		final SubMonitor mon = SubMonitor.convert(monitor, 4);
		final ScriptSourceAppender scriptBuilder = this.codeBuilderFactory.buildScript(
				getPackageFragment().getElementName(), typeProvider);
		final ISarlSkillBuilder skill = scriptBuilder.addSarlSkill(getTypeName());
		skill.setExtends(getSuperClass());
		for (final String type : getSuperInterfaces()) {
			skill.addImplements(type);
		}
		skill.setDocumentation(comment);
		mon.worked(1);
		createStandardSARLLifecycleFunctionTemplates(
				"skill",
				(it) -> skill.addSarlAction(it),
				(it) -> skill.addSarlCapacityUses(it));
		mon.worked(2);
		createInheritedMembers(
				Skill.class.getCanonicalName(),
				skill.getSarlSkill(),
				true,
				() -> skill.addSarlConstructor(),
				(name) -> skill.addOverrideSarlAction(name),
				getSuperClass(),
				getSuperInterfaces());
		mon.worked(3);
		scriptBuilder.build(appender);
		mon.done();
	}

	@Override
	protected String getExistingElementErrorMessage() {
		return Messages.NewSarlSkillWizardPage_1;
	}

	@Override
	protected String getInvalidSubtypeErrorMessage() {
		return Messages.NewSarlSkillWizardPage_2;
	}

	@Override
	protected IType getRootSuperType() throws JavaModelException {
		return getJavaProject().findType(Skill.class.getName());
	}

	@Override
	protected IType getRootSuperInterface() throws JavaModelException {
		return getJavaProject().findType(Capacity.class.getName());
	}

	@Override
	protected String getInvalidInterfaceTypeErrorMessage() {
		return Messages.NewSarlSkillWizardPage_0;
	}

	@Override
	protected boolean isSuperInterfaceNeeded() {
		return true;
	}

	@Override
	protected String getMissedSuperInterfaceErrorMessage() {
		return Messages.NewSarlSkillWizardPage_3;
	}

	@Override
	protected AbstractSuperTypeSelectionDialog<?> createSuperClassSelectionDialog(Shell parent,
			IRunnableContext context, IJavaProject project, SarlSpecificTypeSelectionExtension extension,
			boolean multi) {
		return new SuperSkillSelectionDialog(parent, context, project, this, extension, multi);
	}

	@Override
	protected AbstractSuperTypeSelectionDialog<?> createSuperInterfaceSelectionDialog(Shell parent,
			IRunnableContext context, IJavaProject project, SarlSpecificTypeSelectionExtension extension,
			boolean multi) {
		return new SuperCapacitySelectionDialog<>(parent, context, project, this, extension, multi);
	}

}
