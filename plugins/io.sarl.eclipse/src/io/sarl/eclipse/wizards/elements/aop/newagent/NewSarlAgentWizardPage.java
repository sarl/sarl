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

package io.sarl.eclipse.wizards.elements.aop.newagent;

import java.util.Collections;
import java.util.Comparator;
import java.util.Map;

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
import io.sarl.lang.actionprototype.ActionPrototype;
import io.sarl.lang.codebuilder.builders.IAgentBuilder;
import io.sarl.lang.codebuilder.builders.IScriptBuilder;
import io.sarl.lang.core.Agent;

/**
 * Wizard page for creating a new SARL agent.
 *
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 */
public class NewSarlAgentWizardPage extends AbstractNewSarlElementWizardPage {

	/** Construct a wizard page.
	 */
	public NewSarlAgentWizardPage() {
		super(CLASS_TYPE, Messages.NewSarlAgent_0);
		setTitle(Messages.NewSarlAgent_0);
		setDescription(Messages.NewSarlAgentPage_0);
		setImageDescriptor(SARLEclipsePlugin.getDefault().getImageDescriptor(SARLEclipseConfig.NEW_AGENT_WIZARD_DIALOG_IMAGE));
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
		IScriptBuilder scriptBuilder = this.codeBuilderFactory.createScript(getPackageFragment().getElementName(), ecoreResource);
		IAgentBuilder agent = scriptBuilder.addAgent(getTypeName());
		agent.setExtends(getSuperClass());
		agent.setDocumentation(typeComment.trim());

		Map<ActionPrototype, IMethod> operationsToImplement;

		if (isCreateInherited()) {
			operationsToImplement = Maps.newTreeMap((Comparator<ActionPrototype>) null);
		} else {
			operationsToImplement = null;
		}

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

		if (operationsToImplement != null) {
			this.jdt2sarl.createActions(agent, operationsToImplement.values());
		}

		scriptBuilder.finalizeScript();
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
