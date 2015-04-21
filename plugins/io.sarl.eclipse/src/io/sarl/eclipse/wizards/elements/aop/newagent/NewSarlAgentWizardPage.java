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
package io.sarl.eclipse.wizards.elements.aop.newagent;

import static io.sarl.eclipse.util.Jdt2Ecore.populateInheritanceContext;
import io.sarl.eclipse.SARLConfig;
import io.sarl.eclipse.SARLEclipsePlugin;
import io.sarl.eclipse.util.Jdt2Ecore;
import io.sarl.eclipse.wizards.elements.AbstractNewSarlElementWizardPage;
import io.sarl.lang.actionprototype.ActionPrototype;
import io.sarl.lang.core.Agent;
import io.sarl.lang.generator.helper.SarlEcoreCode;
import io.sarl.lang.sarl.SarlAgent;

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

import com.google.common.collect.Maps;

/**
 * Wizard page for creating a new SARL agent.
 *
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 */
public class NewSarlAgentWizardPage extends AbstractNewSarlElementWizardPage {

	/**
	 */
	public NewSarlAgentWizardPage() {
		super(CLASS_TYPE, Messages.NewSarlAgent_0);
		setTitle(Messages.NewSarlAgent_0);
		setDescription(Messages.NewSarlAgentPage_0);
		setImageDescriptor(SARLEclipsePlugin.getDefault().getImageDescriptor(SARLConfig.NEW_AGENT_WIZARD_DIALOG_IMAGE));
	}

	@Override
	public void createPageControls(Composite parent) {
		createSuperClassControls(parent, COLUMNS);
		createSeparator(parent, COLUMNS);
		createMethodStubControls(parent, COLUMNS, false, true);
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
		SarlAgent agent = this.sarlGenerator.createAgent(code, getTypeName(), getSuperClass());
		this.sarlGenerator.attachComment(code, agent, typeComment);

		Map<ActionPrototype, IMethod> operationsToImplement;

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
				// Discarding super constructors,
				null,
				code.getCodeGenerator().getActionSignatureProvider(),
				getSuperClass(),
				Collections.<String>emptyList());

		if (operationsToImplement != null) {
			Jdt2Ecore.createActions(code, operationsToImplement.values(), agent);
		}

		code.finalizeScript();
	}

	@Override
	protected String getExistingElementErrorMessage() {
		return Messages.NewSarlAgentWizardPage_1;
	}

	@Override
	protected String getInvalidSubtypeErrorMessage() {
		return Messages.NewSarlAgentWizardPage_2;
	}

	@Override
	protected IType getRootSuperType() throws JavaModelException {
		return getJavaProject().findType(Agent.class.getName());
	}

}
