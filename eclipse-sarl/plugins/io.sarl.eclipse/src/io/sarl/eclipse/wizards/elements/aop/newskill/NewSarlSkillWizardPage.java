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

package io.sarl.eclipse.wizards.elements.aop.newskill;

import java.util.Comparator;
import java.util.Map;

import com.google.common.base.Strings;
import com.google.common.collect.Maps;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.emf.ecore.resource.Resource;
import org.eclipse.jdt.core.IMethod;
import org.eclipse.jdt.core.IType;
import org.eclipse.jdt.core.JavaModelException;
import org.eclipse.swt.widgets.Composite;

import io.sarl.eclipse.SARLEclipseConfig;
import io.sarl.eclipse.SARLEclipsePlugin;
import io.sarl.eclipse.wizards.elements.AbstractNewSarlElementWizardPage;
import io.sarl.lang.actionprototype.ActionParameterTypes;
import io.sarl.lang.actionprototype.ActionPrototype;
import io.sarl.lang.codebuilder.builders.ISarlSkillBuilder;
import io.sarl.lang.codebuilder.builders.IScriptBuilder;
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
	public void createPageControls(Composite parent) {
		createSuperClassControls(parent, COLUMNS);
		createSuperInterfacesControls(parent, COLUMNS);
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
			this.fSuperInterfacesStatus,
		};
		updateStatus(status);
	}

	@Override
	protected void getTypeContent(Resource ecoreResource, String typeComment) throws CoreException {
		final IScriptBuilder scriptBuilder = this.codeBuilderFactory.createScript(
				getPackageFragment().getElementName(), ecoreResource);
		final ISarlSkillBuilder skill = scriptBuilder.addSarlSkill(getTypeName());
		skill.setExtends(getSuperClass());
		for (final String implementedType : getSuperInterfaces()) {
			skill.addImplements(implementedType);
		}
		skill.setDocumentation(typeComment.trim());

		final Map<ActionPrototype, IMethod> operationsToImplement;
		final Map<ActionParameterTypes, IMethod> constructors;

		final String superClass = getSuperClass();
		if (Strings.isNullOrEmpty(superClass) || !isCreateConstructors()) {
			constructors = null;
		} else {
			constructors = Maps.newTreeMap((Comparator<ActionParameterTypes>) null);
		}

		if (isCreateInherited()) {
			operationsToImplement = Maps.newTreeMap((Comparator<ActionPrototype>) null);
		} else {
			operationsToImplement = null;
		}

		this.jdt2sarl.populateInheritanceContext(
				this.jdt2sarl.toTypeFinder(getJavaProject()),
				// Discarding final operation
				null,
				// Discarding overridable operation
				null,
				// Discarding inherited fields,
				null,
				operationsToImplement,
				constructors,
				getSuperClass(),
				getSuperInterfaces());

		if (constructors != null) {
			this.jdt2sarl.createStandardConstructors(skill, constructors.values(), skill.getSarlSkill());
		}

		if (operationsToImplement != null) {
			this.jdt2sarl.createActions(skill, operationsToImplement.values());
		}

		scriptBuilder.finalizeScript();
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

}
