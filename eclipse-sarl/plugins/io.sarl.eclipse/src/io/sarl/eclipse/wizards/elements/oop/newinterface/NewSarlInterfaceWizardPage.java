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

package io.sarl.eclipse.wizards.elements.oop.newinterface;

import java.util.Comparator;
import java.util.Map;

import com.google.common.collect.Maps;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.emf.common.notify.Notifier;
import org.eclipse.emf.ecore.resource.Resource;
import org.eclipse.jdt.core.IMethod;
import org.eclipse.jdt.core.IType;
import org.eclipse.jdt.core.JavaModelException;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.xtext.common.types.access.IJvmTypeProvider;
import org.eclipse.xtext.xbase.compiler.ISourceAppender;

import io.sarl.eclipse.SARLEclipsePlugin;
import io.sarl.eclipse.wizards.elements.AbstractNewSarlElementWizardPage;
import io.sarl.lang.actionprototype.ActionPrototype;
import io.sarl.lang.codebuilder.builders.ISarlInterfaceBuilder;
import io.sarl.lang.codebuilder.builders.IScriptBuilder;

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
		super(CLASS_TYPE, Messages.NewSarlInterfaceWizard_0);
		setTitle(Messages.NewSarlInterfaceWizard_0);
		setDescription(Messages.NewSarlInterfaceWizardPage_0);
		setImageDescriptor(SARLEclipsePlugin.getDefault().getImageDescriptor(IMAGE_HEADER));
	}

	@Override
	public void createPageControls(Composite parent) {
		createSuperInterfacesControls(parent, COLUMNS);
		createSeparator(parent, COLUMNS);
		createMethodStubControls(parent, COLUMNS, false, true);
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
			IProgressMonitor monitor) throws CoreException {
//		final IScriptBuilder scriptBuilder = this.codeBuilderFactory.createScript(
//				getPackageFragment().getElementName(), ecoreResource);
//		final ISarlInterfaceBuilder classType = scriptBuilder.addSarlInterface(getTypeName());
//		for (final String implementedType : getSuperInterfaces()) {
//			classType.addExtends(implementedType);
//		}
//
//		final Map<ActionPrototype, IMethod> operationsToImplement;
//
//		if (isCreateInherited()) {
//			operationsToImplement = Maps.newTreeMap((Comparator<ActionPrototype>) null);
//		} else {
//			operationsToImplement = null;
//		}
//
//		this.jdt2sarl.populateInheritanceContext(
//				this.jdt2sarl.toTypeFinder(getJavaProject()),
//				// Discarding final operation.
//				null,
//				// Discarding overridable operation.
//				null,
//				// Discarding inherited fields,
//				null,
//				operationsToImplement,
//				// Discarding super constructors,
//				null,
//				getSuperClass(),
//				getSuperInterfaces());
//
//		if (operationsToImplement != null) {
//			this.jdt2sarl.createActions(classType, operationsToImplement.values());
//		}
//
//		scriptBuilder.finalizeScript();
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
