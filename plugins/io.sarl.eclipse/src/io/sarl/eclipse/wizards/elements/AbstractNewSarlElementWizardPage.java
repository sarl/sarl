/*
 * $Id$
 *
 * SARL is an general-purpose agent programming language.
 * More details on http://www.sarl.io
 *
 * Copyright (C) 2014 Sebastian RODRIGUEZ, Nicolas GAUD, Stéphane GALLAND.
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

import io.sarl.eclipse.SARLEclipsePlugin;
import io.sarl.lang.SARLKeywords;

import java.io.ByteArrayInputStream;
import java.lang.reflect.InvocationTargetException;
import java.text.MessageFormat;
import java.util.ArrayList;
import java.util.HashSet;
import java.util.List;
import java.util.Set;
import java.util.TreeSet;

import javax.inject.Inject;
import javax.inject.Named;

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
import org.eclipse.jdt.internal.corext.codemanipulation.StubUtility;
import org.eclipse.jdt.ui.wizards.NewTypeWizardPage;
import org.eclipse.jface.dialogs.MessageDialog;
import org.eclipse.jface.operation.IRunnableWithProgress;
import org.eclipse.jface.viewers.IStructuredSelection;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.ui.actions.WorkspaceModifyOperation;
import org.eclipse.xtext.Constants;
import org.eclipse.xtext.formatting.IWhitespaceInformationProvider;
import org.eclipse.xtext.ui.resource.IStorage2UriMapper;

import com.google.common.base.Strings;

/**
 * Abstract implementation of a wizard page for creating new SARL elements.
 *
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 */
public abstract class AbstractNewSarlElementWizardPage extends NewTypeWizardPage {

	/** filename extension for the Java code.
	 */
	protected static final String JAVA_FILE_EXTENSION = "java"; //$NON-NLS-1$

	/** Number of columns in the composite components.
	 */
	protected static final int COLUMNS = 4;

	private IResource resource;

	@Inject
	private FieldInitializerUtil util;

	private String sarlFileExtension;

	@Inject
	private IWhitespaceInformationProvider whitespaceInformationProvider;

	@Inject
	private IStorage2UriMapper storage2UriMapper;

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
	public final void createType(IProgressMonitor monitor) throws CoreException, InterruptedException {
		throw new UnsupportedOperationException();
	}

	@Override
	protected void handleFieldChanged(String fieldName) {
		super.handleFieldChanged(fieldName);
		doStatusUpdate();
	}

	@Override
	protected IStatus typeNameChanged() {
		assert (this.sarlFileExtension != null);
		IPackageFragment packageFragment = getPackageFragment();
		String typeName = getTypeName();
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
		IJavaProject project = getPackageFragmentRoot().getJavaProject();
		if (project != null) {
			try {
				String packageName = packageFragment.getElementName();
				for (IPackageFragmentRoot root : project.getPackageFragmentRoots()) {
					IPackageFragment fragment = root.getPackageFragment(packageName);
					if (isFileExists(fragment, filename, JAVA_FILE_EXTENSION)) {
						return true;
					}
				}
			} catch (JavaModelException _) {
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
			IResource resource = packageFragment.getResource();
			if (resource instanceof IFolder) {
				IFolder folder = (IFolder) resource;
				if (folder.getFile(filename + "." + extension).exists()) { //$NON-NLS-1$
					return true;
				}
			}
		}
		return false;
	}

	/** Invoked for obtaining the error message related to an existing type.
	 *
	 * The following parameters will be replaced in the error message according to
	 * the {@link MessageFormat} utility class: <ul>
	 * <li><code>{0}</code>: the name of the type.</li>
	 * </ul>
	 *
	 * @return the error message.
	 */
	protected abstract String getExistingElementErrorMessage();

	/** Invoked for obtaining the error message related to an invalid subtype for the new element.
	 *
	 * The following parameters will be replaced in the error message according to
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
	 * The following parameters will be replaced in the error message according to
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
	 * The following parameters will be replaced in the error message according to
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
		IJavaElement elem = this.util.getSelectedResource(selection);
		initContainerPage(elem);
		initTypePage(elem);
		//
		try {
			getRootSuperType();
			reinitSuperClass();
		} catch (Throwable _) {
			//
		}
		//
		try {
			getRootSuperInterface();
			reinitSuperInterfaces();
		} catch (Throwable _) {
			//
		}
		//
		doStatusUpdate();
	}

	/** Replies if the given type is a subtype of the expected super-type.
	 * The expected super-type is replied by {@link #getRootSuperType()}.
	 *
	 * @param className - the name of the class to be tested.
	 * @return <code>true</code> if the given name is the one of a subtype
	 * of the expected root type.
	 * @throws JavaModelException if there is a problem for retreiving the Java information.
	 */
	protected boolean isValidExtendedType(String className) throws JavaModelException {
		// accept the empty field (stands for java.lang.Object)
		if (!Strings.isNullOrEmpty(className)) {
			IType rootType = getRootSuperType();
			assert (rootType != null);
			IType type = findType(getJavaProject(), className);
			assert (type != null);
			ITypeHierarchy hierarchy = type.newSupertypeHierarchy(new NullProgressMonitor());
			assert (hierarchy != null);
			if (!hierarchy.contains(rootType)) {
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
	 * of the expected root type.
	 * @throws JavaModelException if there is a problem for retreiving the Java information.
	 */
	protected boolean isValidImplementedType(String className) throws JavaModelException {
		if (!Strings.isNullOrEmpty(className)) {
			IType rootType = getRootSuperInterface();
			assert (rootType != null);
			IType type = findType(getJavaProject(), className);
			assert (type != null);
			ITypeHierarchy hierarchy = type.newSupertypeHierarchy(new NullProgressMonitor());
			assert (hierarchy != null);
			if (!hierarchy.contains(rootType)) {
				return false;
			}
		}
		return true;
	}

	private void reinitSuperClass() {
		String className = getSuperClass();
		try {
			if (!isValidExtendedType(className)) {
				IType rootType = getRootSuperType();
				assert (rootType != null);
				setSuperClass(rootType.getFullyQualifiedName(), true);
			}
		} catch (JavaModelException ex) {
			this.fSuperClassStatus = SARLEclipsePlugin.getDefault().createStatus(IStatus.ERROR, ex);
		}
	}

	private void reinitSuperInterfaces() {
		List<IStatus> status = new ArrayList<>();
		Set<String> validInterfaces = new HashSet<>();
		for (String interfaceName : getSuperInterfaces()) {
			try {
				if (!isValidImplementedType(interfaceName)) {
					IType rootType = getRootSuperInterface();
					assert (rootType != null);
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
			} catch (Throwable _) {
				//
			}
		}
		if (status.isEmpty()) {
			this.fSuperInterfacesStatus = SARLEclipsePlugin.getDefault().createOkStatus();
		} else {
			IStatus[] tab = new IStatus[status.size()];
			status.toArray(tab);
			this.fSuperInterfacesStatus = SARLEclipsePlugin.getDefault().createMultiStatus(tab);
		}
	}

	/** Find a type in the context of the given project.
	 *
	 * @param project - the context of the search.
	 * @param typeName - the name of the type to search.
	 * @return the type or <code>null</code>.
	 * @throws JavaModelException if it is not possible to retreive the type.
	 */
	@SuppressWarnings("static-method")
	protected IType findType(IJavaProject project, String typeName) throws JavaModelException {
		if (project.exists()) {
			return project.findType(typeName);
		}
		return null;
	}

	@Override
	protected IStatus superClassChanged() {
		IStatus status = super.superClassChanged();
		assert (status != null);
		if (status.isOK()) {
			String className = getSuperClass();
			try {
				if (!isValidExtendedType(className)) {
					status = SARLEclipsePlugin.getDefault().createStatus(
							IStatus.ERROR, MessageFormat.format(
									getInvalidSubtypeErrorMessage(),
									className));
				}
			} catch (JavaModelException ex) {
				status = SARLEclipsePlugin.getDefault().createStatus(IStatus.ERROR, ex);
			}
		}
		return status;
	}

	@Override
	protected IStatus superInterfacesChanged() {
		IStatus status = super.superInterfacesChanged();
		assert (status != null);
		if (status.isOK()) {
			List<IStatus> statusInfo = new ArrayList<>();
			boolean hasInterface = false;
			for (String superInterface : getSuperInterfaces()) {
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
				} catch (Throwable _) {
					//
				}
			}
			if (!statusInfo.isEmpty()) {
				IStatus[] tab = new IStatus[statusInfo.size()];
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
	 * You should invoke this from the {@link #createControl(Composite)} of
	 * the child class.
	 *
	 * @param parent - the component in which the common controls are added.
	 * @return the created composite.
	 */
	protected Composite createCommonControls(Composite parent) {
		initializeDialogUnits(parent);
		Composite composite = SWTFactory.createComposite(
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

	/** Create the type from the data gathered in the wizard.
	 *
	 * @return the size of the created file.
	 */
	protected int createType() {
		final int[] size = {0};
		IRunnableWithProgress op = new WorkspaceModifyOperation() {
			private static final int STEPS = 4;
			@SuppressWarnings("synthetic-access")
			@Override
			protected void execute(IProgressMonitor monitor) throws CoreException, InvocationTargetException,
			InterruptedException {
				SubMonitor mon = SubMonitor.convert(monitor, STEPS);
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
					IResource packageResource = getPackageFragment().getResource();
					IFile sarlFile = ((IFolder) packageResource).getFile(
							getTypeName() + "." //$NON-NLS-1$
							+ AbstractNewSarlElementWizardPage.this.sarlFileExtension);
					URI sarlUri = AbstractNewSarlElementWizardPage.this.storage2UriMapper.getUri(sarlFile);
					mon.worked(1);
					// Create the file content
					String content = createContent(mon.newChild(1), sarlFile,
							AbstractNewSarlElementWizardPage.this.whitespaceInformationProvider.
							getIndentationInformation(sarlUri).getIndentString(),
							AbstractNewSarlElementWizardPage.this.whitespaceInformationProvider.
							getLineSeparatorInformation(sarlUri).getLineSeparator());
					size[0] = content.length();
					sarlFile.create(new ByteArrayInputStream(content.getBytes()), true, mon.newChild(1));
					setResource(sarlFile);
				} catch (OperationCanceledException e) {
					throw new InterruptedException();
				} catch (Exception e) {
					throw new InvocationTargetException(e);
				}
			}
		};
		try {
			getContainer().run(true, false, op);
		} catch (InterruptedException e) {
			// cancelled by user
			return 0;
		} catch (InvocationTargetException e) {
			Throwable realException = e.getTargetException();
			MessageDialog.openError(getShell(), getTitle(), realException.getMessage());
		}
		return size[0];
	}

	/** Invoked to create a stub to a JDT compilation unit.
	 * This stub could be used to access to the
	 *
	 * @return the stub.
	 */
	private ICompilationUnit getCompilationUnitStub() {
		String compilationUnitName = getCompilationUnitName(getTypeName());
		return new CompilationUnit((PackageFragment) getPackageFragment(),
				compilationUnitName,
				DefaultWorkingCopyOwner.PRIMARY);
	}

	/** Invoked for creating the content of the generated file.
	 *
	 * @param monitor - the progression monitor.
	 * @param sarlFile - the filename of the generated file.
	 * @param indentation - the identation.
	 * @param lineSeparator - the line separator.
	 * @return the content of the file.
	 * @throws CoreException when the content cannot be generated.
	 */
	private String createContent(IProgressMonitor monitor, IFile sarlFile,
			String indentation, String lineSeparator) throws CoreException {
		ICompilationUnit compilationUnit = getCompilationUnitStub();
		String fileComment = getFileComment(compilationUnit, lineSeparator);
		String typeComment = getTypeComment(compilationUnit, lineSeparator);

		IPackageFragment packageFragment = getPackageFragment();

		StringBuilder typeContent = new StringBuilder();
		Set<String> imports = new TreeSet<>();
		getTypeContent(packageFragment, typeContent, imports, indentation, lineSeparator);

		String packageDeclaration = SarlTypeCreatorUtil.createPackageDeclaration(packageFragment, lineSeparator);

		StringBuilder head = new StringBuilder();
		head.append(packageDeclaration);
		head.append(lineSeparator);
		if (!imports.isEmpty()) {
			for (String importElement : imports) {
				if (!Strings.isNullOrEmpty(importElement)
						&& !importElement.equals(packageFragment.getElementName())) {
					head.append(SARLKeywords.IMPORT);
					head.append(" "); //$NON-NLS-1$
					head.append(importElement);
					head.append(lineSeparator);
				}
			}
		}
		return StubUtility.getCompilationUnitContent(compilationUnit,
				head.toString(),
				fileComment, typeComment, typeContent.toString(), lineSeparator);
	}

	/** Invoked for retreiving the definition of the new type.
	 *
	 * @param packageFragment - the definition of the package fragment in which the content should be generated.
	 * @param content - the content of the file. This parameter is filled with the content.
	 * @param imports - the list of the imports to inject into the file.
	 * @param indentation - the identation.
	 * @param lineSeparator - the line separator.
	 */
	protected abstract void getTypeContent(
			IPackageFragment packageFragment,
			StringBuilder content,
			Set<String> imports,
			String indentation, String lineSeparator);

}
