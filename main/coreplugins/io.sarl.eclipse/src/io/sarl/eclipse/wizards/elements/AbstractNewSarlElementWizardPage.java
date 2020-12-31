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

package io.sarl.eclipse.wizards.elements;

import java.io.ByteArrayInputStream;
import java.lang.reflect.InvocationTargetException;
import java.text.MessageFormat;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.Comparator;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;
import java.util.Objects;
import java.util.Set;
import javax.inject.Inject;
import javax.inject.Named;

import com.google.common.base.Strings;
import com.google.common.collect.Iterables;
import com.google.common.collect.Maps;
import com.google.inject.Injector;
import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.IFolder;
import org.eclipse.core.resources.IResource;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.NullProgressMonitor;
import org.eclipse.core.runtime.OperationCanceledException;
import org.eclipse.core.runtime.SubMonitor;
import org.eclipse.debug.internal.ui.SWTFactory;
import org.eclipse.emf.common.util.URI;
import org.eclipse.emf.ecore.resource.ResourceSet;
import org.eclipse.jdt.core.ICompilationUnit;
import org.eclipse.jdt.core.IJavaElement;
import org.eclipse.jdt.core.IJavaProject;
import org.eclipse.jdt.core.IMethod;
import org.eclipse.jdt.core.IPackageFragment;
import org.eclipse.jdt.core.IPackageFragmentRoot;
import org.eclipse.jdt.core.IType;
import org.eclipse.jdt.core.ITypeHierarchy;
import org.eclipse.jdt.core.JavaModelException;
import org.eclipse.jdt.internal.core.CompilationUnit;
import org.eclipse.jdt.internal.core.DefaultWorkingCopyOwner;
import org.eclipse.jdt.internal.core.PackageFragment;
import org.eclipse.jdt.internal.ui.util.CoreUtility;
import org.eclipse.jdt.internal.ui.wizards.NewWizardMessages;
import org.eclipse.jdt.internal.ui.wizards.dialogfields.DialogField;
import org.eclipse.jdt.internal.ui.wizards.dialogfields.LayoutUtil;
import org.eclipse.jdt.internal.ui.wizards.dialogfields.SelectionButtonDialogFieldGroup;
import org.eclipse.jdt.ui.wizards.NewTypeWizardPage;
import org.eclipse.jface.dialogs.IDialogSettings;
import org.eclipse.jface.operation.IRunnableContext;
import org.eclipse.jface.operation.IRunnableWithProgress;
import org.eclipse.jface.viewers.IStructuredSelection;
import org.eclipse.jface.window.Window;
import org.eclipse.swt.SWT;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.ui.actions.WorkspaceModifyOperation;
import org.eclipse.xtend.core.xtend.XtendTypeDeclaration;
import org.eclipse.xtext.Constants;
import org.eclipse.xtext.common.types.JvmDeclaredType;
import org.eclipse.xtext.common.types.JvmOperation;
import org.eclipse.xtext.common.types.JvmParameterizedTypeReference;
import org.eclipse.xtext.common.types.access.IJvmTypeProvider;
import org.eclipse.xtext.formatting.IWhitespaceInformationProvider;
import org.eclipse.xtext.ui.resource.IResourceSetProvider;
import org.eclipse.xtext.ui.resource.IStorage2UriMapper;
import org.eclipse.xtext.xbase.XFeatureCall;
import org.eclipse.xtext.xbase.compiler.ISourceAppender;
import org.eclipse.xtext.xbase.compiler.ImportManager;
import org.eclipse.xtext.xbase.compiler.output.FakeTreeAppendable;
import org.eclipse.xtext.xbase.lib.Functions.Function1;
import org.eclipse.xtext.xbase.lib.Procedures.Procedure1;

import io.sarl.eclipse.SARLEclipsePlugin;
import io.sarl.eclipse.util.Jdt2Ecore;
import io.sarl.eclipse.util.Jdt2Ecore.ActionBuilder;
import io.sarl.eclipse.util.Jdt2Ecore.ConstructorBuilder;
import io.sarl.eclipse.util.Jdt2Ecore.TypeFinder;
import io.sarl.lang.codebuilder.CodeBuilderFactory;
import io.sarl.lang.codebuilder.builders.IBlockExpressionBuilder;
import io.sarl.lang.codebuilder.builders.IExpressionBuilder;
import io.sarl.lang.codebuilder.builders.ISarlActionBuilder;
import io.sarl.lang.codebuilder.builders.ISarlBehaviorUnitBuilder;
import io.sarl.lang.formatting2.FormatterFacade;
import io.sarl.lang.sarl.actionprototype.ActionParameterTypes;
import io.sarl.lang.sarl.actionprototype.ActionPrototype;

/**
 * Abstract implementation of a wizard page for creating new SARL elements.
 *
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 */
@SuppressWarnings("checkstyle:classfanoutcomplexity")
public abstract class AbstractNewSarlElementWizardPage extends NewTypeWizardPage {

	/** filename extension for the Java code.
	 */
	protected static final String JAVA_FILE_EXTENSION = "java"; //$NON-NLS-1$

	/** Number of columns in the composite components.
	 */
	protected static final int COLUMNS = 4;

	/** Name of the SARL Initialize event.
	 */
	protected static final String INITIALIZE_EVENT_NAME = "io.sarl.core.Initialize"; //$NON-NLS-1$

	/** Name of the SARL Destroy event.
	 */
	protected static final String DESTROY_EVENT_NAME = "io.sarl.core.Destroy"; //$NON-NLS-1$

	/** Name of the SARL ContextJoined event.
	 */
	protected static final String CONTEXTJOINED_EVENT_NAME = "io.sarl.core.ContextJoined"; //$NON-NLS-1$

	/** Name of the SARL ContextLeft event.
	 */
	protected static final String CONTEXTLEFT_EVENT_NAME = "io.sarl.core.ContextLeft"; //$NON-NLS-1$

	/** Name of the SARL MemberJoined event.
	 */
	protected static final String MEMBERJOINED_EVENT_NAME = "io.sarl.core.MemberJoined"; //$NON-NLS-1$

	/** Name of the SARL MemberLeft event.
	 */
	protected static final String MEMBERLEFT_EVENT_NAME = "io.sarl.core.MemberLeft"; //$NON-NLS-1$

	/** Name of the SARL SpaceCreated event.
	 *
	 * @since 0.10
	 */
	protected static final String SPACECREATED_EVENT_NAME = "io.sarl.core.SpaceCreated"; //$NON-NLS-1$

	/** Name of the SARL SpaceDestroyed event.
	 *
	 * @since 0.10
	 */
	protected static final String SPACEDESTROYED_EVENT_NAME = "io.sarl.core.SpaceDestroyed"; //$NON-NLS-1$

	/** Name of the SARL ParticipantJoined event.
	 *
	 * @since 0.10
	 */
	protected static final String PARTICIPANTJOINED_EVENT_NAME = "io.sarl.core.ParticipantJoined"; //$NON-NLS-1$

	/** Name of the SARL ParticipantLeft event.
	 *
	 * @since 0.10
	 */
	protected static final String PARTICIPANTLEFT_EVENT_NAME = "io.sarl.core.ParticipantLeft"; //$NON-NLS-1$

	/** Name of the SARL AgentSpawned event.
	 */
	protected static final String AGENTSPAWNED_EVENT_NAME = "io.sarl.core.AgentSpawned"; //$NON-NLS-1$

	/** Name of the SARL AgentKilled event.
	 */
	protected static final String AGENTKILLED_EVENT_NAME = "io.sarl.core.AgentKilled"; //$NON-NLS-1$

	/** Name of the SARL Logging capacity.
	 */
	protected static final String LOGGING_CAPACITY_NAME = "io.sarl.core.Logging"; //$NON-NLS-1$

	/** Name of the SARL skill install function.
	 */
	protected static final String INSTALL_SKILL_NAME = "install"; //$NON-NLS-1$

	/** Name of the SARL skill uninstall function.
	 */
	protected static final String UNINSTALL_SKILL_NAME = "uninstall"; //$NON-NLS-1$

	/** Name of the SARL skill prepareUninstallation function.
	 */
	protected static final String PREPARE_UNINSTALL_SKILL_NAME = "prepareUninstallation"; //$NON-NLS-1$

	private static final int STEPS = 8;

	private static final String SETTINGS_CREATECONSTR = "create_constructor"; //$NON-NLS-1$

	private static final String SETTINGS_CREATEUNIMPLEMENTED = "create_unimplemented"; //$NON-NLS-1$

	private static final String SETTINGS_GENERATEEVENTHANDLERS = "generate_event_handlers"; //$NON-NLS-1$

	private static final String SETTINGS_GENERATELIFECYCLEFUNCTIONS = "generate_lifecycle_functions"; //$NON-NLS-1$

	/** A builder of code.
	 */
	@Inject
	protected CodeBuilderFactory codeBuilderFactory;

	/** Provider of the resource set associated to a project.
	 */
	@Inject
	protected IResourceSetProvider resourceSetFactory;

	@Inject
	private Jdt2Ecore jdt2sarl;

	@Inject
	private FieldInitializerUtil fieldInitializer;

	@Inject
	private IStorage2UriMapper storage2UriMapper;

	@Inject
	private IWhitespaceInformationProvider whitespaceInformationProvider;

	@Inject
	private Injector injector;

	private String sarlFileExtension;

	private IResource resource;

	private SelectionButtonDialogFieldGroup methodStubsButtons;

	private boolean isConstructorCreationEnabled;

	private boolean isInheritedCreationEnabled;

	private boolean isDefaultEventGenerated;

	private boolean isDefaultLifecycleFunctionsGenerated;

	@Inject
	private FormatterFacade formatterFacade;

	@Inject
	private IJvmTypeProvider.Factory jdtTypeProviderFactory;

	private boolean hasSuperTypeField;

	private boolean hasSuperInterfaceField;

	/** Constructor.
	 * @param typeKind Signals the kind of the type to be created. Valid kinds are
	 * {@link NewTypeWizardPage#CLASS_TYPE}, {@link NewTypeWizardPage#INTERFACE_TYPE},
	 * {@link NewTypeWizardPage#ENUM_TYPE} and {@link NewTypeWizardPage#ANNOTATION_TYPE}.
	 * @param title the title of the page.
	 */
	public AbstractNewSarlElementWizardPage(int typeKind, String title) {
		super(typeKind, title);
	}

	/** Replies if the super-type field is activiated.
	 *
	 * @return <code>true</code> if the super-type control were added.
	 */
	public boolean isSuperTypeActivated() {
		return this.hasSuperTypeField;
	}

	/** Replies if the super-interface field is activiated.
	 *
	 * @return <code>true</code> if the super-interface control were added.
	 */
	public boolean isSuperInterfaceActivated() {
		return this.hasSuperInterfaceField;
	}

	@Override
	protected void createSuperClassControls(Composite composite, int nColumns) {
		this.hasSuperTypeField = true;
		super.createSuperClassControls(composite, nColumns);
	}

	@Override
	protected void createSuperInterfacesControls(Composite composite, int nColumns) {
		this.hasSuperInterfaceField = true;
		super.createSuperInterfacesControls(composite, nColumns);
	}

	/** Change the file extension used by this page.
	 *
	 * @param fileExtension the file extension
	 */
	@Inject
	public void setFileExtension(@Named(Constants.FILE_EXTENSIONS) String fileExtension) {
		this.sarlFileExtension = fileExtension;
	}

	/** Update the status of the wizard.
	 */
	protected abstract void doStatusUpdate();

	/** Replies the resource that contains the created SARL element.
	 *
	 * @return the resource of the created SARL element.
	 */
	public IResource getResource() {
		return this.resource;
	}

	/** Change the resource where the created SARL element is located.
	 *
	 * @param resource the resource of the created SARL element.
	 */
	protected void setResource(IResource resource) {
		this.resource = resource;
	}

	@Override
	public void setVisible(boolean visible) {
		super.setVisible(visible);
		if (visible) {
			doStatusUpdate();
			setFocus();
		}
	}

	@Override
	protected void handleFieldChanged(String fieldName) {
		super.handleFieldChanged(fieldName);
		doStatusUpdate();
	}

	@Override
	protected IStatus typeNameChanged() {
		assert this.sarlFileExtension != null;
		final IPackageFragment packageFragment = getPackageFragment();
		final String typeName = getTypeName();
		if (packageFragment != null && !Strings.isNullOrEmpty(typeName)) {
			if (isSarlFile(packageFragment, typeName)) {
				String packageName = ""; //$NON-NLS-1$
				if (!packageFragment.isDefaultPackage()) {
					packageName = packageFragment.getElementName() + "."; //$NON-NLS-1$
				}
				return SARLEclipsePlugin.getDefault().createStatus(
						IStatus.ERROR,
						MessageFormat.format(
								getExistingElementErrorMessage(),
								packageName + getTypeName()));
			}
		}
		return super.typeNameChanged();
	}

	/** Replies if the given filename is a SARL script or a generated Java file.
	 *
	 * @param packageFragment the package in which the file should be search for.
	 * @param filename the filename to test.
	 * @return <code>true</code> if a file (SALR or Java) with the given name exists.
	 */
	protected boolean isSarlFile(IPackageFragment packageFragment, String filename) {
		if (isFileExists(packageFragment, filename, this.sarlFileExtension)) {
			return true;
		}
		final IJavaProject project = getPackageFragmentRoot().getJavaProject();
		if (project != null) {
			try {
				final String packageName = packageFragment.getElementName();
				for (final IPackageFragmentRoot root : project.getPackageFragmentRoots()) {
					final IPackageFragment fragment = root.getPackageFragment(packageName);
					if (isFileExists(fragment, filename, JAVA_FILE_EXTENSION)) {
						return true;
					}
				}
			} catch (JavaModelException exception) {
				// silent error
			}
		}
		return false;
	}

	/** Replies if the given filename is a SARL script in the given package.
	 *
	 * @param packageFragment the package in which the file should be search for.
	 * @param filename the filename to test.
	 * @param extension the filename extension to search for.
	 * @return <code>true</code> if a file (SARL or Java) with the given name exists.
	 */
	protected static boolean isFileExists(IPackageFragment packageFragment, String filename, String extension) {
		if (packageFragment != null) {
			final IResource resource = packageFragment.getResource();
			if (resource instanceof IFolder) {
				final IFolder folder = (IFolder) resource;
				if (folder.getFile(filename + "." + extension).exists()) { //$NON-NLS-1$
					return true;
				}
			}
		}
		return false;
	}

	/** Invoked for obtaining the error message related to an existing type.
	 *
	 * <p>The following parameters will be replaced in the error message according to
	 * the {@link MessageFormat} utility class: <ul>
	 * <li><code>{0}</code>: the name of the type.</li>
	 * </ul>
	 *
	 * @return the error message.
	 */
	protected abstract String getExistingElementErrorMessage();

	/** Invoked for obtaining the error message related to an invalid subtype for the new element.
	 *
	 * <p>The following parameters will be replaced in the error message according to
	 * the {@link MessageFormat} utility class: <ul>
	 * <li><code>{0}</code>: the name of the selected type.</li>
	 * </ul>
	 *
	 * @return the error message.
	 * @throws UnsupportedOperationException a runtime exception
	 */
	@SuppressWarnings("static-method")
	protected String getInvalidSubtypeErrorMessage() {
		throw new UnsupportedOperationException();
	}

	/** Invoked for obtaining the error message related to an invalid sub-interface for the new element.
	 *
	 * <p>The following parameters will be replaced in the error message according to
	 * the {@link MessageFormat} utility class: <ul>
	 * <li><code>{0}</code>: the name of the selected type.</li>
	 * </ul>
	 *
	 * @return the error message.
	 * @throws UnsupportedOperationException a runtime exception
	 */
	@SuppressWarnings("static-method")
	protected String getInvalidInterfaceTypeErrorMessage() {
		throw new UnsupportedOperationException();
	}

	/** Invoked for obtaining the error message related to a missed super-interface.
	 *
	 * <p>The following parameters will be replaced in the error message according to
	 * the {@link MessageFormat} utility class: <ul>
	 * <li><code>{0}</code>: the name of the missed type.</li>
	 * </ul>
	 *
	 * @return the error message.
	 * @throws UnsupportedOperationException a runtime exception
	 */
	@SuppressWarnings("static-method")
	protected String getMissedSuperInterfaceErrorMessage() {
		throw new UnsupportedOperationException();
	}

	/** Invoked by the wizard for initializing the page with the given selection.
	 *
	 * @param selection the current selection.
	 */
	protected void init(IStructuredSelection selection) {
		final IJavaElement elem = this.fieldInitializer.getSelectedResource(selection);
		initContainerPage(elem);
		initTypePage(elem);
		//
		try {
			getRootSuperType();
			reinitSuperClass();
		} catch (Throwable exception) {
			//
		}
		//
		try {
			getRootSuperInterface();
			reinitSuperInterfaces();
		} catch (Throwable exception) {
			//
		}
		//
		doStatusUpdate();
	}

	@Override
	public boolean isAddComments() {
		// Create the comments
		return true;
	}

	/** Replies if the given type is a subtype of the expected super-type.
	 * The expected super-type is replied by {@link #getRootSuperType()}.
	 *
	 * @param className the name of the class to be tested.
	 * @return <code>true</code> if the given name is the one of a subtype
	 *     of the expected root type.
	 * @throws JavaModelException if there is a problem for retreiving the Java information.
	 */
	protected boolean isValidExtendedType(String className) throws JavaModelException {
		// accept the empty field (stands for the default super type)
		if (!Strings.isNullOrEmpty(className)) {
			final IType rootType = getRootSuperType();
			if (rootType == null) {
				final IStatus status = SARLEclipsePlugin.getDefault().createStatus(IStatus.ERROR,
						Messages.AbstractNewSarlElementWizardPage_3);
				throw new JavaModelException(new CoreException(status));
			}
			final IType type = getJavaProject().findType(className);
			if (type == null) {
				final IStatus status = SARLEclipsePlugin.getDefault().createStatus(IStatus.ERROR,
						MessageFormat.format(Messages.AbstractNewSarlElementWizardPage_4, className));
				throw new JavaModelException(new CoreException(status));
			}
			final ITypeHierarchy hierarchy = type.newSupertypeHierarchy(new NullProgressMonitor());
			if (hierarchy == null || !hierarchy.contains(rootType)) {
				return false;
			}
		}
		return true;
	}

	/** Replies if the given type implements the expected super-interface.
	 * The expected super-interface is replied by {@link #getRootSuperInterface()}.
	 *
	 * @param className the name of the class to be tested.
	 * @return <code>true</code> if the given name implements a type
	 *     of the expected root type.
	 * @throws JavaModelException if there is a problem for retreiving the Java information.
	 */
	protected boolean isValidImplementedType(String className) throws JavaModelException {
		if (!Strings.isNullOrEmpty(className)) {
			final IType rootType = getRootSuperInterface();
			assert rootType != null;
			final IType type = getJavaProject().findType(className);
			assert type != null;
			final ITypeHierarchy hierarchy = type.newSupertypeHierarchy(new NullProgressMonitor());
			assert hierarchy != null;
			if (!hierarchy.contains(rootType)) {
				return false;
			}
		}
		return true;
	}

	private void reinitSuperClass() {
		final String className = getSuperClass();
		try {
			if (!isValidExtendedType(className)) {
				final IType rootType = getRootSuperType();
				assert rootType != null;
				setSuperClass(rootType.getFullyQualifiedName(), true);
			}
		} catch (JavaModelException ex) {
			this.fSuperClassStatus = SARLEclipsePlugin.getDefault().createStatus(IStatus.ERROR, ex);
		}
	}

	private void reinitSuperInterfaces() {
		final List<IStatus> status = new ArrayList<>();
		final Set<String> validInterfaces = new HashSet<>();
		for (final String interfaceName : getSuperInterfaces()) {
			try {
				if (!isValidImplementedType(interfaceName)) {
					final IType rootType = getRootSuperInterface();
					assert rootType != null;
					validInterfaces.add(rootType.getFullyQualifiedName());
				} else {
					validInterfaces.add(interfaceName);
				}
			} catch (JavaModelException ex) {
				status.add(SARLEclipsePlugin.getDefault().createStatus(IStatus.ERROR, ex));
			}
		}
		setSuperInterfaces(new ArrayList<>(validInterfaces), true);
		if (status.isEmpty() && isSuperInterfaceNeeded()) {
			try {
				status.add(SARLEclipsePlugin.getDefault().createStatus(
						IStatus.ERROR, MessageFormat.format(
						getMissedSuperInterfaceErrorMessage(),
						getRootSuperInterface().getFullyQualifiedName())));
			} catch (Throwable exception) {
				//
			}
		}
		if (status.isEmpty()) {
			this.fSuperInterfacesStatus = SARLEclipsePlugin.getDefault().createOkStatus();
		} else {
			final IStatus[] tab = new IStatus[status.size()];
			status.toArray(tab);
			this.fSuperInterfacesStatus = SARLEclipsePlugin.getDefault().createMultiStatus(tab);
		}
	}

	@Override
	protected IStatus superClassChanged() {
		IStatus status = super.superClassChanged();
		assert status != null;
		if (status.isOK() && isSuperTypeActivated()) {
			final String className = getSuperClass();
			try {
				if (!isValidExtendedType(className)) {
					status = SARLEclipsePlugin.getDefault().createStatus(
							IStatus.ERROR, MessageFormat.format(
									getInvalidSubtypeErrorMessage(),
									className));
				}
			} catch (JavaModelException ex) {
				status = ex.getJavaModelStatus();
			}
		}
		return status;
	}

	@Override
	protected IStatus superInterfacesChanged() {
		IStatus status = super.superInterfacesChanged();
		assert status != null;
		if (status.isOK() && isSuperInterfaceActivated()) {
			final List<IStatus> statusInfo = new ArrayList<>();
			boolean hasInterface = false;
			for (final String superInterface : getSuperInterfaces()) {
				try {
					if (!isValidImplementedType(superInterface)) {
						statusInfo.add(SARLEclipsePlugin.getDefault().createStatus(
								IStatus.ERROR, MessageFormat.format(
								getInvalidInterfaceTypeErrorMessage(),
								superInterface)));
					} else {
						hasInterface = true;
					}
				} catch (JavaModelException ex) {
					statusInfo.add(SARLEclipsePlugin.getDefault().createStatus(IStatus.ERROR, ex));
				}
			}
			if (!hasInterface && isSuperInterfaceNeeded()) {
				try {
					statusInfo.add(SARLEclipsePlugin.getDefault().createStatus(
							IStatus.ERROR, MessageFormat.format(
							getMissedSuperInterfaceErrorMessage(),
							getRootSuperInterface().getFullyQualifiedName())));
				} catch (Throwable exception) {
					//
				}
			}
			if (!statusInfo.isEmpty()) {
				final IStatus[] tab = new IStatus[statusInfo.size()];
				statusInfo.toArray(tab);
				status = SARLEclipsePlugin.getDefault().createMultiStatus(tab);
			}
		}
		return status;
	}

	/** Replies if the created type must have one super-interface.
	 *
	 * @return <code>true</code> if the type needs a super-interface;
	 * <code>false</code> if not.
	 */
	@SuppressWarnings("static-method")
	protected boolean isSuperInterfaceNeeded() {
		return false;
	}

	/** Replies the allowed root super type for the created type.
	 *
	 * @return the allowed root super type.
	 * @throws JavaModelException - when the Java model cannot enable to retrieve the root type.
	 * @throws UnsupportedOperationException a runtime exception
	 */
	@SuppressWarnings("static-method")
	protected IType getRootSuperType() throws JavaModelException {
		throw new UnsupportedOperationException();
	}

	/** Replies the allowed root super interface for the created type.
	 *
	 * @return the allowed root super interface.
	 * @throws JavaModelException - when the Java model cannot enable to retrieve the root type.
	 * @throws UnsupportedOperationException a runtime exception
	 */
	@SuppressWarnings("static-method")
	protected IType getRootSuperInterface() throws JavaModelException {
		throw new UnsupportedOperationException();
	}

	/** Create the components that are common to the creation of all
	 * the SARL elements.
	 *
	 * <p>You should invoke this from the {@link #createControl(Composite)} of
	 * the child class.
	 *
	 * @param parent the component in which the common controls are added.
	 * @return the created composite.
	 */
	protected Composite createCommonControls(Composite parent) {
		initializeDialogUnits(parent);
		final Composite composite = SWTFactory.createComposite(
				parent,
				parent.getFont(),
				COLUMNS, 1,
				GridData.FILL_HORIZONTAL);
		createContainerControls(composite, COLUMNS);
		createPackageControls(composite, COLUMNS);
		createSeparator(composite, COLUMNS);
		createTypeNameControls(composite, COLUMNS);
		return composite;
	}

	@Override
	public final void createControl(Composite parent) {
		final Composite composite = createCommonControls(parent);
		createPageControls(composite);
		setControl(composite);
		readSettings();
		doStatusUpdate();
	}

	/** Invoked to create the controls in the page.
	 *
	 * @param parent the container of the controls.
	 */
	protected abstract void createPageControls(Composite parent);

	/** Create the type from the data gathered in the wizard.
	 *
	 * @return the size of the created file.
	 */
	protected final int asyncCreateType() {
		final int[] size = {0};
		final IRunnableWithProgress op = new WorkspaceModifyOperation() {
			@Override
			protected void execute(IProgressMonitor monitor)
				throws CoreException, InvocationTargetException, InterruptedException {
				size[0] = createSARLType(monitor);
			}
		};
		try {
			getContainer().run(true, false, op);
		} catch (InterruptedException e) {
			// canceled by user
			return 0;
		} catch (InvocationTargetException e) {
			final Throwable realException = e.getTargetException();
			SARLEclipsePlugin.getDefault().openError(getShell(), getTitle(),
					realException.getMessage(), null, realException);
		}
		return size[0];
	}

	@Override
	public final void createType(IProgressMonitor monitor) throws CoreException, InterruptedException {
		// See createSARLType
		throw new UnsupportedOperationException();
	}

	private ICompilationUnit getCompilationUnitStub() {
		final String compilationUnitName = getCompilationUnitName(getTypeName());
		return new CompilationUnit((PackageFragment) getPackageFragment(), compilationUnitName, DefaultWorkingCopyOwner.PRIMARY);
	}

	/** Create the inherited members.
	 *
	 * @param defaultSuperTypeQualifiedName the qualified name of the default super type.
	 * @param context the context.
	 * @param generateActionBlocks indicates if the action blocks must be generated.
	 * @param constructorBuilder the code for adding a constructor.
	 * @param actionBuilder the code for adding an operation.
	 * @param superTypeQualifiedName the qualified name of the super type.
	 * @param superInterfaceQualifiedNames the qualified names of the super interfaces.
	 * @throws JavaModelException if the java model is invalid.
	 */
	protected void createInheritedMembers(
			String defaultSuperTypeQualifiedName,
			XtendTypeDeclaration context, boolean generateActionBlocks,
			ConstructorBuilder constructorBuilder, ActionBuilder actionBuilder,
			String superTypeQualifiedName, String... superInterfaceQualifiedNames)
					throws JavaModelException {
		createInheritedMembers(defaultSuperTypeQualifiedName, context,
				generateActionBlocks, constructorBuilder, actionBuilder,
				superTypeQualifiedName, Arrays.asList(superInterfaceQualifiedNames));
	}

	/** Create the inherited members.
	 *
	 * @param defaultSuperTypeQualifiedName the qualified name of the default super type.
	 * @param context the context.
	 * @param generateActionBlocks indicates if the action blocks must be generated.
	 * @param constructorBuilder the code for adding a constructor.
	 * @param actionBuilder the code for adding an operation.
	 * @param superTypeQualifiedName the qualified name of the super type.
	 * @param superInterfaceQualifiedNames the qualified names of the super interfaces.
	 * @throws JavaModelException if the java model is invalid.
	 */
	protected void createInheritedMembers(
			String defaultSuperTypeQualifiedName,
			XtendTypeDeclaration context, boolean generateActionBlocks,
			ConstructorBuilder constructorBuilder, ActionBuilder actionBuilder,
			String superTypeQualifiedName, List<String> superInterfaceQualifiedNames)
					throws JavaModelException {
		final TypeFinder typeFinder = getTypeFinder();

		final Map<ActionParameterTypes, IMethod> baseConstructors = Maps.newTreeMap((Comparator<ActionParameterTypes>) null);
		this.jdt2sarl.populateInheritanceContext(
				typeFinder,
				null, null, null, null,
				baseConstructors,
				defaultSuperTypeQualifiedName,
				Collections.<String>emptyList());

		final Map<ActionParameterTypes, IMethod> constructors;
		if (isCreateConstructors()) {
			constructors = Maps.newTreeMap((Comparator<ActionParameterTypes>) null);
		} else {
			constructors = null;
		}

		final Map<ActionPrototype, IMethod> operationsToImplement;
		if (isCreateInherited()) {
			operationsToImplement = Maps.newHashMap();
		} else {
			operationsToImplement = null;
		}

		this.jdt2sarl.populateInheritanceContext(
				typeFinder,
				null, null, null,
				operationsToImplement,
				constructors,
				superTypeQualifiedName,
				superInterfaceQualifiedNames);

		if (context != null) {
			if (constructors != null && constructorBuilder != null) {
				for (final Entry<ActionParameterTypes, IMethod> constructor : constructors.entrySet()) {
					if (!baseConstructors.containsKey(constructor.getKey())) {
						this.jdt2sarl.createStandardConstructorsWith(constructorBuilder,
								Collections.singletonList(constructor.getValue()),
								context);
						break;
					}
				}
			}

			if (operationsToImplement != null && actionBuilder != null) {
				this.jdt2sarl.createActionsWith(actionBuilder, operationsToImplement.values(),
						generateActionBlocks ? context : null);
			}
		}
	}

	/** Replies the type finder in the context of the current project.
	 *
	 * @return the type finder.
	 * @since 0.5
	 */
	protected final TypeFinder getTypeFinder() {
		return this.jdt2sarl.toTypeFinder(getJavaProject());
	}

	/** Create the SARL type.
	 *
	 * @param monitor the progression monitor.
	 * @return the size of the generated code.
	 * @throws CoreException when the creation failed.
	 * @throws InterruptedException when the operation was canceled.
	 */
	public int createSARLType(IProgressMonitor monitor) throws CoreException, InterruptedException {
		try {
			final SubMonitor mainmon = SubMonitor.convert(monitor, getTitle(), STEPS);
			// Create the package if not existing
			IPackageFragment packageFragment = getPackageFragment();
			if (!packageFragment.exists()) {
				packageFragment = getPackageFragmentRoot().createPackageFragment(
						getPackageFragment().getElementName(),
						true,
						mainmon.newChild(1));
			} else {
				mainmon.worked(1);
			}

			// Create the file
			final IFolder packageResource = (IFolder) packageFragment.getResource();
			if (!packageResource.exists()) {
				CoreUtility.createFolder(packageResource, true, true, mainmon.newChild(1));
			} else {
				mainmon.worked(1);
			}
			IFile sarlFile = packageResource.getFile(
					getTypeName() + "." //$NON-NLS-1$
					+ this.sarlFileExtension);
			int index = 1;
			while (sarlFile.exists()) {
				sarlFile = packageResource.getFile(
						getTypeName() + index + "." //$NON-NLS-1$
						+ this.sarlFileExtension);
				++index;
			}

			final URI sarlUri = this.storage2UriMapper.getUri(sarlFile);
			final ResourceSet resourceSet = this.resourceSetFactory.get(packageFragment.getJavaProject().getProject());
			final ICompilationUnit compilationUnit = getCompilationUnitStub();
			final String lineSeparator = this.whitespaceInformationProvider
					.getLineSeparatorInformation(sarlUri).getLineSeparator();
			mainmon.worked(1);

			// Create the type content
			final SubMonitor mon1 = mainmon.newChild(1);
			mon1.setTaskName(MessageFormat.format(Messages.AbstractNewSarlElementWizardPage_5, getTypeName()));
			final String typeComment = getTypeComment(compilationUnit, lineSeparator);
			final IJvmTypeProvider typeProvider = this.jdtTypeProviderFactory.findOrCreateTypeProvider(resourceSet);
			final ImportManager imports = new ImportManager(true);
			this.injector.injectMembers(imports);
			final FakeTreeAppendable appender = new FakeTreeAppendable(imports);
			this.injector.injectMembers(appender);
			generateTypeContent(appender, typeProvider, typeComment, mon1);
			mon1.done();

			// Build the full file content
			final SubMonitor mon2 = mainmon.newChild(1);
			mon2.setTaskName(MessageFormat.format(Messages.AbstractNewSarlElementWizardPage_6, getTypeName()));
			final String fileComment = getFileComment(compilationUnit, lineSeparator);
			final StringBuilder realContent = new StringBuilder();
			if (!Strings.isNullOrEmpty(fileComment)) {
				realContent.append(fileComment);
				realContent.append(lineSeparator);
				realContent.append(lineSeparator);
			}
			realContent.append(appender.getContent());
			realContent.append(lineSeparator);
			mon2.done();

			final SubMonitor mon3 = mainmon.newChild(1);
			mon3.setTaskName(MessageFormat.format(Messages.AbstractNewSarlElementWizardPage_7, getTypeName()));
			final String content = this.formatterFacade.format(realContent.toString());
			mon3.done();

			// Write the resource
			final SubMonitor mon4 = mainmon.newChild(1);
			mon4.setTaskName(MessageFormat.format(Messages.AbstractNewSarlElementWizardPage_8, getTypeName()));
			try (ByteArrayInputStream stream = new ByteArrayInputStream(content.getBytes())) {
				sarlFile.create(stream, true, mon4);
			}
			setResource(sarlFile);
			saveSettings();
			mon4.done();

			return content.length();
		} catch (OperationCanceledException e) {
			throw new InterruptedException();
		} catch (CoreException e) {
			throw e;
		} catch (Exception e) {
			throw new CoreException(SARLEclipsePlugin.getDefault().createStatus(IStatus.ERROR, e));
		}
	}

	/** Read the settings of the dialog box.
	 */
	protected void readSettings() {
		boolean createConstructors = false;
		boolean createUnimplemented = true;
		boolean createEventHandlers = true;
		boolean createLifecycleFunctions = true;
		final IDialogSettings dialogSettings = getDialogSettings();
		if (dialogSettings != null) {
			final IDialogSettings section = dialogSettings.getSection(getName());
			if (section != null) {
				createConstructors = section.getBoolean(SETTINGS_CREATECONSTR);
				createUnimplemented = section.getBoolean(SETTINGS_CREATEUNIMPLEMENTED);
				createEventHandlers = section.getBoolean(SETTINGS_GENERATEEVENTHANDLERS);
				createLifecycleFunctions = section.getBoolean(SETTINGS_GENERATELIFECYCLEFUNCTIONS);
			}
		}
		setMethodStubSelection(createConstructors, createUnimplemented, createEventHandlers,
				createLifecycleFunctions, true);
	}

	/** Save the settings of the dialog box.
	 */
	protected void saveSettings() {
		final IDialogSettings dialogSettings = getDialogSettings();
		if (dialogSettings != null) {
			IDialogSettings section = dialogSettings.getSection(getName());
			if (section == null) {
				section = dialogSettings.addNewSection(getName());
			}
			section.put(SETTINGS_CREATECONSTR, isCreateConstructors());
			section.put(SETTINGS_CREATEUNIMPLEMENTED, isCreateInherited());
			section.put(SETTINGS_GENERATEEVENTHANDLERS, isCreateStandardEventHandlers());
			section.put(SETTINGS_GENERATELIFECYCLEFUNCTIONS, isCreateStandardLifecycleFunctions());
		}
	}

	/** Invoked for retreiving the definition of the new type.
	 *
	 * @param appender the receiver of the code.
	 * @param typeProvider provides types.
	 * @param comment the comment of the element to generate.
	 * @param monitor the progression monitor.
	 * @throws Exception if an error occurs when creating the content.
	 */
	protected abstract void generateTypeContent(ISourceAppender appender, IJvmTypeProvider typeProvider,
			String comment, IProgressMonitor monitor) throws Exception;

	/** Create the controls related to the behavior units to generate.
	 *
	 * @param composite the container of the controls.
	 * @param columns the number of columns.
	 * @param enableConstructors indicates if the constructor creation is enable.
	 * @param enableInherited indicates if the inherited operation creation is enable.
	 * @param defaultEvents indicates if the default events will be generated.
	 * @param lifecycleFunctions indicates if the default lifecycle functions will be generated.
	 */
	protected void createMethodStubControls(Composite composite, int columns,
			boolean enableConstructors, boolean enableInherited, boolean defaultEvents,
			boolean lifecycleFunctions) {
		this.isConstructorCreationEnabled = enableConstructors;
		this.isInheritedCreationEnabled = enableInherited;
		this.isDefaultEventGenerated = defaultEvents;
		this.isDefaultLifecycleFunctionsGenerated = lifecycleFunctions;
		final List<String> nameList = new ArrayList<>(4);
		if (enableConstructors) {
			nameList.add(Messages.AbstractNewSarlElementWizardPage_0);
		}
		if (enableInherited) {
			nameList.add(Messages.AbstractNewSarlElementWizardPage_1);
		}
		if (defaultEvents) {
			nameList.add(Messages.AbstractNewSarlElementWizardPage_17);
		}
		if (lifecycleFunctions) {
			nameList.add(Messages.AbstractNewSarlElementWizardPage_18);
		}
		if (nameList.isEmpty()) {
			return;
		}
		final String[] buttonNames = new String[nameList.size()];
		nameList.toArray(buttonNames);

		this.methodStubsButtons = new SelectionButtonDialogFieldGroup(SWT.CHECK, buttonNames, 1);
		this.methodStubsButtons.setLabelText(Messages.AbstractNewSarlElementWizardPage_2);

		final Control labelControl = this.methodStubsButtons.getLabelControl(composite);
		LayoutUtil.setHorizontalSpan(labelControl, columns);

		DialogField.createEmptySpace(composite);

		final Control buttonGroup = this.methodStubsButtons.getSelectionButtonsGroup(composite);
		LayoutUtil.setHorizontalSpan(buttonGroup, columns - 1);
	}

	/**
	 * Returns the current selection state of the 'Create Constructors' checkbox.
	 *
	 * @return the selection state of the 'Create Constructors' checkbox
	 */
	protected boolean isCreateConstructors() {
		return this.isConstructorCreationEnabled && this.methodStubsButtons.isSelected(0);
	}

	/**
	 * Returns the current selection state of the 'Create inherited abstract methods'
	 * checkbox.
	 *
	 * @return the selection state of the 'Create inherited abstract methods' checkbox
	 */
	protected boolean isCreateInherited() {
		int idx = 0;
		if (this.isConstructorCreationEnabled) {
			++idx;
		}
		return this.isInheritedCreationEnabled && this.methodStubsButtons.isSelected(idx);
	}

	/**
	 * Returns the current selection state of the 'Create standard event handlers'
	 * checkbox.
	 *
	 * @return the selection state of the 'Create standard event handlers' checkbox
	 */
	protected boolean isCreateStandardEventHandlers() {
		int idx = 0;
		if (this.isConstructorCreationEnabled) {
			++idx;
		}
		if (this.isInheritedCreationEnabled) {
			++idx;
		}
		return this.isDefaultEventGenerated && this.methodStubsButtons.isSelected(idx);
	}

	/**
	 * Returns the current selection state of the 'Create standard lifecycle functions'
	 * checkbox.
	 *
	 * @return the selection state of the 'Create standard lifecycle functions' checkbox
	 */
	protected boolean isCreateStandardLifecycleFunctions() {
		int idx = 0;
		if (this.isConstructorCreationEnabled) {
			++idx;
		}
		if (this.isInheritedCreationEnabled) {
			++idx;
		}
		if (this.isDefaultEventGenerated) {
			++idx;
		}
		return this.isDefaultLifecycleFunctionsGenerated && this.methodStubsButtons.isSelected(idx);
	}

	/**
	 * Sets the selection state of the method stub checkboxes.
	 *
	 * @param createConstructors initial selection state of the 'Create Constructors' checkbox.
	 * @param createInherited initial selection state of the 'Create inherited abstract methods' checkbox.
	 * @param createEventHandlers initial selection state of the 'Create standard event handlers' checkbox.
	 * @param createLifecycleFunctions initial selection state of the 'Create standard lifecycle functions' checkbox.
	 * @param canBeModified if <code>true</code> the method stub checkboxes can be changed by
	 *     the user. If <code>false</code> the buttons are "read-only"
	 */
	protected void setMethodStubSelection(boolean createConstructors, boolean createInherited,
			boolean createEventHandlers, boolean createLifecycleFunctions, boolean canBeModified) {
		if (this.methodStubsButtons != null) {
			int idx = 0;
			if (this.isConstructorCreationEnabled) {
				this.methodStubsButtons.setSelection(idx, createConstructors);
				++idx;
			}
			if (this.isInheritedCreationEnabled) {
				this.methodStubsButtons.setSelection(idx, createInherited);
				++idx;
			}
			if (this.isDefaultEventGenerated) {
				this.methodStubsButtons.setSelection(idx, createEventHandlers);
				++idx;
			}
			if (this.isDefaultLifecycleFunctionsGenerated) {
				this.methodStubsButtons.setSelection(idx, createLifecycleFunctions);
				++idx;
			}
			this.methodStubsButtons.setEnabled(canBeModified);
		}
	}

	/** Create an instanceof the super-class selection dialog.
	 *
	 * @param parent the parent.
	 * @param context the execution context.
	 * @param project the Java project.
	 * @param extension the extension to give to the dialog box.
	 * @param multi indicates if the selection could be done on multiple elements.
	 * @return the dialog, or {@code null} for using the default dialog box.
	 */
	@SuppressWarnings("static-method")
	protected AbstractSuperTypeSelectionDialog<?> createSuperClassSelectionDialog(
			Shell parent, IRunnableContext context, IJavaProject project, SarlSpecificTypeSelectionExtension extension,
			boolean multi) {
		return null;
	}

	@Override
	protected IType chooseSuperClass() {
		final IJavaProject project = getJavaProject();
		if (project == null) {
			return null;
		}
		final IJvmTypeProvider typeProvider = this.jdtTypeProviderFactory.findOrCreateTypeProvider(
				this.resourceSetFactory.get(project.getProject()));
		final SarlSpecificTypeSelectionExtension extension = new SarlSpecificTypeSelectionExtension(typeProvider);
		this.injector.injectMembers(extension);
		final AbstractSuperTypeSelectionDialog<?> dialog = createSuperClassSelectionDialog(getShell(),
				getWizard().getContainer(), project, extension, false);
		if (dialog == null) {
			return super.chooseSuperClass();
		}

		this.injector.injectMembers(dialog);
		dialog.setTitle(NewWizardMessages.NewTypeWizardPage_SuperClassDialog_title);
		dialog.setMessage(NewWizardMessages.NewTypeWizardPage_SuperClassDialog_message);
		dialog.setInitialPattern(getSuperClass());

		if (dialog.open() == Window.OK) {
			return (IType) dialog.getFirstResult();
		}
		return null;
	}

	/** Create an instanceof the super-interface selection dialog.
	 *
	 * @param parent the parent.
	 * @param context the execution context.
	 * @param project the Java project.
	 * @param extension the extension to give to the dialog box.
	 * @param multi indicates if the selection could be done on multiple elements.
	 * @return the dialog, or {@code null} for using the default dialog box.
	 */
	@SuppressWarnings("static-method")
	protected AbstractSuperTypeSelectionDialog<?> createSuperInterfaceSelectionDialog(
			Shell parent, IRunnableContext context, IJavaProject project, SarlSpecificTypeSelectionExtension extension,
			boolean multi) {
		return null;
	}

	private static void createInfoCall(IExpressionBuilder builder, String message) {
		final JvmParameterizedTypeReference capacity = builder.newTypeRef(null, LOGGING_CAPACITY_NAME);
		final String objectType = Object.class.getName();
		final String objectArrayType = objectType + "[]"; //$NON-NLS-1$
		final JvmOperation infoMethod = Iterables.find(
				((JvmDeclaredType) capacity.getType()).getDeclaredOperations(), it -> {
				if (Objects.equals(it.getSimpleName(), "info") //$NON-NLS-1$
						&& it.getParameters().size() == 2) {
					final String type1 = it.getParameters().get(0).getParameterType().getIdentifier();
					final String type2 = it.getParameters().get(1).getParameterType().getIdentifier();
					return Objects.equals(objectType, type1) && Objects.equals(objectArrayType, type2);
				}
				return false;
			},
			null);
		if (infoMethod != null) {
			builder.setExpression("info(\"" + message + "\")"); //$NON-NLS-1$ //$NON-NLS-2$
			((XFeatureCall) builder.getXExpression()).setFeature(infoMethod);
		}
	}

	/** Create the default standard SARL event templates.
	 *
	 * @param elementTypeName the name of the element type.
	 * @param behaviorUnitAdder the adder of behavior unit.
	 * @param usesAdder the adder of uses statement.
	 * @return {@code true} if the units are added; {@code false} otherwise.
	 * @since 0.5
	 */
	protected boolean createStandardSARLEventTemplates(String elementTypeName,
			Function1<? super String, ? extends ISarlBehaviorUnitBuilder> behaviorUnitAdder,
			Procedure1<? super String> usesAdder) {
		if (!isCreateStandardEventHandlers()) {
			return false;
		}
		Object type;
		try {
			type = getTypeFinder().findType(INITIALIZE_EVENT_NAME);
		} catch (JavaModelException e) {
			type = null;
		}
		if (type != null) {
			// SARL Libraries are on the classpath

			usesAdder.apply(LOGGING_CAPACITY_NAME);

			ISarlBehaviorUnitBuilder unit = behaviorUnitAdder.apply(INITIALIZE_EVENT_NAME);
			IBlockExpressionBuilder block = unit.getExpression();
			block.setInnerDocumentation(MessageFormat.format(
					Messages.AbstractNewSarlElementWizardPage_9,
					elementTypeName));
			IExpressionBuilder expr = block.addExpression();
			createInfoCall(expr, "The " + elementTypeName + " was started.");  //$NON-NLS-1$ //$NON-NLS-2$

			unit = behaviorUnitAdder.apply(DESTROY_EVENT_NAME);
			block = unit.getExpression();
			block.setInnerDocumentation(MessageFormat.format(
					Messages.AbstractNewSarlElementWizardPage_10,
					elementTypeName));
			expr = block.addExpression();
			createInfoCall(expr, "The " + elementTypeName + " was stopped.");  //$NON-NLS-1$ //$NON-NLS-2$

			unit = behaviorUnitAdder.apply(AGENTSPAWNED_EVENT_NAME);
			block = unit.getExpression();
			block.setInnerDocumentation(MessageFormat.format(
					Messages.AbstractNewSarlElementWizardPage_11,
					elementTypeName));

			unit = behaviorUnitAdder.apply(AGENTKILLED_EVENT_NAME);
			block = unit.getExpression();
			block.setInnerDocumentation(MessageFormat.format(
					Messages.AbstractNewSarlElementWizardPage_12,
					elementTypeName));

			unit = behaviorUnitAdder.apply(CONTEXTJOINED_EVENT_NAME);
			block = unit.getExpression();
			block.setInnerDocumentation(MessageFormat.format(
					Messages.AbstractNewSarlElementWizardPage_13,
					elementTypeName));

			unit = behaviorUnitAdder.apply(CONTEXTLEFT_EVENT_NAME);
			block = unit.getExpression();
			block.setInnerDocumentation(MessageFormat.format(
					Messages.AbstractNewSarlElementWizardPage_14,
					elementTypeName));

			unit = behaviorUnitAdder.apply(MEMBERJOINED_EVENT_NAME);
			block = unit.getExpression();
			block.setInnerDocumentation(MessageFormat.format(
					Messages.AbstractNewSarlElementWizardPage_15,
					elementTypeName));

			unit = behaviorUnitAdder.apply(MEMBERLEFT_EVENT_NAME);
			block = unit.getExpression();
			block.setInnerDocumentation(MessageFormat.format(
					Messages.AbstractNewSarlElementWizardPage_16,
					elementTypeName));

			unit = behaviorUnitAdder.apply(MEMBERLEFT_EVENT_NAME);
			block = unit.getExpression();
			block.setInnerDocumentation(MessageFormat.format(
					Messages.AbstractNewSarlElementWizardPage_16,
					elementTypeName));

			unit = behaviorUnitAdder.apply(SPACECREATED_EVENT_NAME);
			block = unit.getExpression();
			block.setInnerDocumentation(MessageFormat.format(
					Messages.AbstractNewSarlElementWizardPage_21,
					elementTypeName));

			unit = behaviorUnitAdder.apply(SPACEDESTROYED_EVENT_NAME);
			block = unit.getExpression();
			block.setInnerDocumentation(MessageFormat.format(
					Messages.AbstractNewSarlElementWizardPage_22,
					elementTypeName));

			unit = behaviorUnitAdder.apply(PARTICIPANTJOINED_EVENT_NAME);
			block = unit.getExpression();
			block.setInnerDocumentation(MessageFormat.format(
					Messages.AbstractNewSarlElementWizardPage_23,
					elementTypeName));

			unit = behaviorUnitAdder.apply(PARTICIPANTLEFT_EVENT_NAME);
			block = unit.getExpression();
			block.setInnerDocumentation(MessageFormat.format(
					Messages.AbstractNewSarlElementWizardPage_24,
					elementTypeName));

			return true;
		}
		return false;
	}

	/** Create the default standard lifecycle function templates.
	 *
	 * @param elementTypeName the name of the element type.
	 * @param actionAdder the adder of actions.
	 * @param usesAdder the adder of uses statement.
	 * @return {@code true} if the units are added; {@code false} otherwise.
	 * @since 0.5
	 */
	protected boolean createStandardSARLLifecycleFunctionTemplates(String elementTypeName,
			Function1<? super String, ? extends ISarlActionBuilder> actionAdder,
			Procedure1<? super String> usesAdder) {
		if (!isCreateStandardLifecycleFunctions()) {
			return false;
		}

		usesAdder.apply(LOGGING_CAPACITY_NAME);

		ISarlActionBuilder action = actionAdder.apply(INSTALL_SKILL_NAME);
		IBlockExpressionBuilder block = action.getExpression();
		block.setInnerDocumentation(MessageFormat.format(
				Messages.AbstractNewSarlElementWizardPage_19,
				elementTypeName));
		IExpressionBuilder expr = block.addExpression();
		createInfoCall(expr, "Installing the " + elementTypeName);  //$NON-NLS-1$

		action = actionAdder.apply(PREPARE_UNINSTALL_SKILL_NAME);
		block = action.getExpression();
		block.setInnerDocumentation(MessageFormat.format(
				Messages.AbstractNewSarlElementWizardPage_25,
				elementTypeName));
		expr = block.addExpression();
		createInfoCall(expr, "Preparing the uninstallation of the " + elementTypeName);  //$NON-NLS-1$

		action = actionAdder.apply(UNINSTALL_SKILL_NAME);
		block = action.getExpression();
		block.setInnerDocumentation(MessageFormat.format(
				Messages.AbstractNewSarlElementWizardPage_20,
				elementTypeName));
		expr = block.addExpression();
		createInfoCall(expr, "Uninstalling the " + elementTypeName);  //$NON-NLS-1$

		return true;
	}

	@Override
	protected void chooseSuperInterfaces() {
		final IJavaProject project = getJavaProject();
		if (project == null) {
			return;
		}
		final IJvmTypeProvider typeProvider = this.jdtTypeProviderFactory.findOrCreateTypeProvider(
				this.resourceSetFactory.get(project.getProject()));
		final SarlSpecificTypeSelectionExtension extension = new SarlSpecificTypeSelectionExtension(typeProvider);
		this.injector.injectMembers(extension);
		final AbstractSuperTypeSelectionDialog<?> dialog = createSuperInterfaceSelectionDialog(getShell(),
				getWizard().getContainer(), project, extension, true);
		if (dialog != null) {
			this.injector.injectMembers(dialog);
			dialog.setTitle(NewWizardMessages.NewTypeWizardPage_InterfacesDialog_interface_title);
			dialog.setMessage(NewWizardMessages.NewTypeWizardPage_InterfacesDialog_message);
			try {
				dialog.setInitialPattern(getRootSuperInterface().getFullyQualifiedName());
			} catch (JavaModelException exception) {
				SARLEclipsePlugin.getDefault().log(exception);
			}

			if (dialog.open() == Window.OK) {
				final Object[] tab = dialog.getResult();
				if (tab != null) {
					final List<String> list = new ArrayList<>(tab.length);
					for (final Object obj : tab) {
						if (obj instanceof IType) {
							final IType type = (IType) obj;
							list.add(type.getFullyQualifiedName());
						}
					}
					setSuperInterfaces(list, true);
				}
			}
		} else {
			super.chooseSuperInterfaces();
		}
	}

}
