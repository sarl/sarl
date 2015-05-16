/*
 * $Id$
 *
 * SARL is an general-purpose agent programming language.
 * More details on http://www.sarl.io
 *
 * Copyright (C) 2014-2015 Sebastian RODRIGUEZ, Nicolas GAUD, Stéphane GALLAND.
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

import static io.sarl.eclipse.util.Jdt2Ecore.populateInheritanceContext;
import io.sarl.eclipse.SARLEclipseConfig;
import io.sarl.eclipse.SARLEclipsePlugin;
import io.sarl.eclipse.util.Jdt2Ecore;
import io.sarl.eclipse.wizards.elements.AbstractNewSarlElementWizardPage;
import io.sarl.lang.actionprototype.ActionParameterTypes;
import io.sarl.lang.actionprototype.ActionPrototype;
import io.sarl.lang.core.Behavior;
import io.sarl.lang.generator.helper.SarlEcoreCode;
import io.sarl.lang.sarl.SarlBehavior;

import java.util.Collections;
import java.util.Comparator;
import java.util.Map;

import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.emf.ecore.resource.Resource;
import org.eclipse.jdt.core.IMethod;
import org.eclipse.jdt.core.IType;
import org.eclipse.jdt.core.JavaModelException;
import org.eclipse.swt.widgets.Composite;

import com.google.common.base.Strings;
import com.google.common.collect.Maps;

/**
 * Wizard page for creating a new SARL behavior.
 *
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 */
public class NewSarlBehaviorWizardPage extends AbstractNewSarlElementWizardPage {

	/**
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
		IStatus[] status = new IStatus[] {
				this.fContainerStatus,
				this.fPackageStatus,
				this.fTypeNameStatus,
				this.fSuperClassStatus,
		};
		updateStatus(status);
	}

	@Override
	protected void getTypeContent(Resource ecoreResource, String typeComment) throws CoreException {
		SarlEcoreCode code = this.sarlGenerator.createScript(ecoreResource, getPackageFragment().getElementName());
		SarlBehavior behavior = this.sarlGenerator.createBehavior(code, getTypeName(), getSuperClass());
		this.sarlGenerator.attachComment(code, behavior, typeComment);

		Map<ActionPrototype, IMethod> operationsToImplement;
		Map<ActionParameterTypes, IMethod> constructors;

		String superClass = getSuperClass();
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

		populateInheritanceContext(
				Jdt2Ecore.toTypeFinder(getJavaProject()),
				// Discarding final operation.
				null,
				// Discarding overridable operation.
				null,
				// Discarding inherited fields,
				null,
				operationsToImplement,
				constructors,
				code.getCodeGenerator().getActionSignatureProvider(),
				getSuperClass(),
				Collections.<String>emptyList());

		if (constructors != null) {
			Jdt2Ecore.createStandardConstructors(code, constructors.values(), behavior);
		}

		if (operationsToImplement != null) {
			Jdt2Ecore.createActions(code, operationsToImplement.values(), behavior);
		}

		code.finalizeScript();
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

}
