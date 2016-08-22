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

package io.sarl.eclipse.wizards.elements;

import java.io.ByteArrayOutputStream;
import java.lang.reflect.InvocationTargetException;
import java.text.MessageFormat;
import java.util.ArrayList;
import java.util.HashSet;
import java.util.List;
import java.util.Set;

import javax.inject.Inject;
import javax.inject.Named;

import com.google.common.base.Strings;
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
import org.eclipse.emf.ecore.resource.Resource;
import org.eclipse.emf.ecore.resource.ResourceSet;
import org.eclipse.jdt.core.ICompilationUnit;
import org.eclipse.jdt.core.IJavaElement;
import org.eclipse.jdt.core.IJavaProject;
import org.eclipse.jdt.core.IPackageFragment;
import org.eclipse.jdt.core.IPackageFragmentRoot;
import org.eclipse.jdt.core.IType;
import org.eclipse.jdt.core.ITypeHierarchy;
import org.eclipse.jdt.core.JavaModelException;
import org.eclipse.jdt.internal.core.CompilationUnit;
import org.eclipse.jdt.internal.core.DefaultWorkingCopyOwner;
import org.eclipse.jdt.internal.core.PackageFragment;
import org.eclipse.jdt.internal.ui.wizards.NewWizardMessages;
import org.eclipse.jdt.internal.ui.wizards.dialogfields.DialogField;
import org.eclipse.jdt.internal.ui.wizards.dialogfields.LayoutUtil;
import org.eclipse.jdt.internal.ui.wizards.dialogfields.SelectionButtonDialogFieldGroup;
import org.eclipse.jdt.ui.wizards.NewTypeWizardPage;
import org.eclipse.jface.dialogs.IDialogSettings;
import org.eclipse.jface.dialogs.MessageDialog;
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
import org.eclipse.xtext.Constants;
import org.eclipse.xtext.common.types.access.IJvmTypeProvider;
import org.eclipse.xtext.common.types.access.jdt.JdtTypeProviderFactory;
import org.eclipse.xtext.formatting.IWhitespaceInformationProvider;
import org.eclipse.xtext.ui.resource.IResourceSetProvider;
import org.eclipse.xtext.ui.resource.IStorage2UriMapper;
import org.eclipse.xtext.util.StringInputStream;

import io.sarl.eclipse.SARLEclipsePlugin;
import io.sarl.eclipse.util.Jdt2Ecore;
import io.sarl.lang.codebuilder.CodeBuilderFactory;
import io.sarl.lang.formatting2.FormatterFacade;

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

	private static final int STEPS = 5;

	private static final String SETTINGS_CREATECONSTR = "create_constructor"; //$NON-NLS-1$

	private static final String SETTINGS_CREATEUNIMPLEMENTED = "create_unimplemented"; //$NON-NLS-1$

	/** Creator of SARL elements.
	 */
	@Inject
	protected CodeBuilderFactory codeBuilderFactory;

	/** The trnaslator from JDT to SARL Ecore.
	 */
	@Inject
	protected Jdt2Ecore jdt2sarl;

	@Inject
	private FieldInitializerUtil fieldInitializer;

	@Inject
	private IStorage2UriMapper storage2UriMapper;

	@Inject
	private IResourceSetProvider resourceSetFactory;

	@Inject
	private IWhitespaceInformationProvider whitespaceInformationProvider;

	@Inject
	private Injector injector;

	private String sarlFileExtension;

	private IResource resource;

	private SelectionButtonDialogFieldGroup methodStubsButtons;

	private boolean isConstructorCreationEnabled;

	private boolean isInheritedCreationEnabled;

	@Inject
	private FormatterFacade formatterFacade;

	@Inject
	private JdtTypeProviderFactory jdtTypeProviderFactory;

	/**
	 * @param typeKind - Signals the kind of the type to be created. Valid kinds are
	 * {@link NewTypeWizardPage#CLASS_TYPE}, {@link NewTypeWizardPage#INTERFACE_TYPE},
	 * {@link NewTypeWizardPage#ENUM_TYPE} and {@link NewTypeWizardPage#ANNOTATION_TYPE}.
	 * @param title - the title of the page.
	 */
	public AbstractNewSarlElementWizardPage(int typeKind, String title) {
		super(typeKind, title);
	}

	/** Change the file extension used by this page.
	 *
	 * @param fileExtension - the file extension
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
	 * @param resource - the resource of the created SARL element.
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
	 * @param packageFragment - the package in which the file should be search for.
	 * @param filename - the filename to test.
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
	 * @param packageFragment - the package in which the file should be search for.
	 * @param filename - the filename to test.
	 * @param extension - the filename extension to search for.
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
	 */
	@SuppressWarnings("static-method")
	protected String getMissedSuperInterfaceErrorMessage() {
		throw new UnsupportedOperationException();
	}

	/** Invoked by the wizard for initializing the page with the given selection.
	 *
	 * @param selection - the current selection.
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
	 * @param className - the name of the class to be tested.
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
	 * @param className - the name of the class to be tested.
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
		if (status.isOK()) {
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
		if (status.isOK()) {
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
	 * @throws JavaModelException - when the Java model cannot enable to retreive the root type.
	 */
	protected abstract IType getRootSuperType() throws JavaModelException;

	/** Replies the allowed root super interface for the created type.
	 *
	 * @return the allowed root super interface.
	 * @throws JavaModelException - when the Java model cannot enable to retreive the root type.
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
	 * @param parent - the component in which the common controls are added.
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
	 * @param parent - the container of the controls.
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
			// cancelled by user
			return 0;
		} catch (InvocationTargetException e) {
			final Throwable realException = e.getTargetException();
			SARLEclipsePlugin.getDefault().log(realException);
			MessageDialog.openError(getShell(), getTitle(), realException.getMessage());
		}
		return size[0];
	}

	@Override
	public final void createType(IProgressMonitor monitor) throws CoreException, InterruptedException {
		createSARLType(monitor);
	}

	/** Create the SARL type.
	 *
	 * @param monitor - the progression monitor.
	 * @return the size of the generated code.
	 * @throws CoreException when the creation failed.
	 * @throws InterruptedException when the operation was canceled.
	 */
	public int createSARLType(IProgressMonitor monitor) throws CoreException, InterruptedException {
		final SubMonitor mon = SubMonitor.convert(monitor, STEPS);
		try {
			// Create the package if not existing
			if (!getPackageFragment().exists()) {
				getPackageFragmentRoot().createPackageFragment(
						getPackageFragment().getElementName(),
						true,
						mon.newChild(1));
			} else {
				mon.worked(1);
			}
			// Create the file
			final IResource packageResource = getPackageFragment().getResource();
			final IFolder folder = (IFolder) packageResource;
			final IFile sarlFile = folder.getFile(
					getTypeName() + "." //$NON-NLS-1$
					+ AbstractNewSarlElementWizardPage.this.sarlFileExtension);
			final URI sarlUri = AbstractNewSarlElementWizardPage.this.storage2UriMapper.getUri(sarlFile);
			final ResourceSet resourceSet = AbstractNewSarlElementWizardPage.this.resourceSetFactory.get(
					getJavaProject().getProject());
			final Resource ecoreResource = resourceSet.createResource(sarlUri);
			mon.worked(1);

			// Create the file content
			final ICompilationUnit compilationUnit = getCompilationUnitStub();
			final String lineSeparator = AbstractNewSarlElementWizardPage.this.whitespaceInformationProvider
					.getLineSeparatorInformation(sarlUri).getLineSeparator();
			final String fileComment = getFileComment(compilationUnit, lineSeparator);
			final String typeComment = getTypeComment(compilationUnit, lineSeparator);
			getTypeContent(ecoreResource, typeComment);
			String content;
			try (ByteArrayOutputStream baos = new ByteArrayOutputStream()) {
				if (!Strings.isNullOrEmpty(fileComment)) {
					baos.write(fileComment.getBytes());
					baos.write(lineSeparator.getBytes());
				}
				ecoreResource.save(baos, null);
				content = baos.toString();
			}
			mon.worked(1);

			content = this.formatterFacade.format(content);
			mon.worked(1);

			try (StringInputStream stringInputStream = new StringInputStream(content)) {
				sarlFile.create(stringInputStream, true, mon.newChild(1));
			}
			setResource(sarlFile);

			saveSettings();

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
		final IDialogSettings dialogSettings = getDialogSettings();
		if (dialogSettings != null) {
			final IDialogSettings section = dialogSettings.getSection(getName());
			if (section != null) {
				createConstructors = section.getBoolean(SETTINGS_CREATECONSTR);
				createUnimplemented = section.getBoolean(SETTINGS_CREATEUNIMPLEMENTED);
			}
		}
		setMethodStubSelection(createConstructors, createUnimplemented, true);
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
		}
	}

	private ICompilationUnit getCompilationUnitStub() {
		final String compilationUnitName = getCompilationUnitName(getTypeName());
		return new CompilationUnit((PackageFragment) getPackageFragment(), compilationUnitName,
				DefaultWorkingCopyOwner.PRIMARY);
	}

	/** Invoked for retreiving the definition of the new type.
	 *
	 * @param ecoreResource - the Ecore resource of the script.
	 * @param typeComment - the comment for the type.
	 * @throws CoreException if an error occurs when creating the content.
	 */
	protected abstract void getTypeContent(Resource ecoreResource, String typeComment) throws CoreException;

	/** Create the controls related to the behavior units to generate.
	 *
	 * @param composite - the container of the controls.
	 * @param columns - the number of columns.
	 * @param enableConstructors - indicates if the constructor creation is enable.
	 * @param enableInherited - indicates if the inherited operation creation is enable.
	 */
	protected void createMethodStubControls(Composite composite, int columns,
			boolean enableConstructors, boolean enableInherited) {
		this.isConstructorCreationEnabled = enableConstructors;
		this.isInheritedCreationEnabled = enableInherited;
		final String[] buttonNames;
		if (enableConstructors && enableInherited) {
			buttonNames = new String[] {
				Messages.AbstractNewSarlElementWizardPage_0,
				Messages.AbstractNewSarlElementWizardPage_1,
			};
		} else if (enableInherited && !enableConstructors) {
			buttonNames = new String[] {
				Messages.AbstractNewSarlElementWizardPage_1,
			};
		} else {
			assert enableConstructors;
			buttonNames = new String[] {
				Messages.AbstractNewSarlElementWizardPage_0,
			};
		}
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
		return this.isInheritedCreationEnabled && this.methodStubsButtons.isSelected(
				this.isConstructorCreationEnabled ? 1 : 0);
	}

	/**
	 * Sets the selection state of the method stub checkboxes.
	 *
	 * @param createConstructors initial selection state of the 'Create Constructors' checkbox.
	 * @param createInherited initial selection state of the 'Create inherited abstract methods' checkbox.
	 * @param canBeModified if <code>true</code> the method stub checkboxes can be changed by
	 *     the user. If <code>false</code> the buttons are "read-only"
	 */
	protected void setMethodStubSelection(boolean createConstructors, boolean createInherited, boolean canBeModified) {
		if (this.methodStubsButtons != null) {
			if (this.isConstructorCreationEnabled && this.isInheritedCreationEnabled) {
				this.methodStubsButtons.setSelection(0, createConstructors);
				this.methodStubsButtons.setSelection(1, createInherited);
			} else if (this.isInheritedCreationEnabled && !this.isConstructorCreationEnabled) {
				this.methodStubsButtons.setSelection(0, createInherited);
			} else {
				this.methodStubsButtons.setSelection(0, createConstructors);
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
	 * @return the dialog.
	 */
	protected abstract AbstractSuperTypeSelectionDialog<?> createSuperClassSelectionDialog(
			Shell parent, IRunnableContext context, IJavaProject project, SarlSpecificTypeSelectionExtension extension);

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
				getWizard().getContainer(), project, extension);
		this.injector.injectMembers(dialog);
		dialog.setTitle(NewWizardMessages.NewTypeWizardPage_SuperClassDialog_title);
		dialog.setMessage(NewWizardMessages.NewTypeWizardPage_SuperClassDialog_message);
		dialog.setInitialPattern(getSuperClass());

		if (dialog.open() == Window.OK) {
			return (IType) dialog.getFirstResult();
		}
		return null;
	}

}
