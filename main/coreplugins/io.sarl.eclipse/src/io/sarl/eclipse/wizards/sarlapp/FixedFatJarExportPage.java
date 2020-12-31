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

package io.sarl.eclipse.wizards.sarlapp;

import java.io.File;
import java.lang.reflect.InvocationTargetException;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Iterator;
import java.util.List;

import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.IProject;
import org.eclipse.core.resources.IResource;
import org.eclipse.core.resources.IWorkspaceRoot;
import org.eclipse.core.resources.ResourcesPlugin;
import org.eclipse.core.runtime.Assert;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IPath;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.MultiStatus;
import org.eclipse.core.runtime.Path;
import org.eclipse.core.runtime.Status;
import org.eclipse.debug.core.DebugPlugin;
import org.eclipse.debug.core.ILaunchConfiguration;
import org.eclipse.debug.core.ILaunchConfigurationType;
import org.eclipse.debug.core.ILaunchManager;
import org.eclipse.debug.ui.IDebugUIConstants;
import org.eclipse.jdt.core.IClasspathEntry;
import org.eclipse.jdt.core.IJavaProject;
import org.eclipse.jdt.core.IPackageFragmentRoot;
import org.eclipse.jdt.core.IType;
import org.eclipse.jdt.core.JavaCore;
import org.eclipse.jdt.core.JavaModelException;
import org.eclipse.jdt.core.search.IJavaSearchScope;
import org.eclipse.jdt.internal.ui.IJavaHelpContextIds;
import org.eclipse.jdt.internal.ui.JavaPlugin;
import org.eclipse.jdt.internal.ui.jarpackager.AbstractJarDestinationWizardPage;
import org.eclipse.jdt.internal.ui.jarpackager.JarPackagerUtil;
import org.eclipse.jdt.internal.ui.jarpackagerfat.FatJarAntExporter;
import org.eclipse.jdt.internal.ui.jarpackagerfat.FatJarBuilder;
import org.eclipse.jdt.internal.ui.jarpackagerfat.FatJarPackageWizardPage;
import org.eclipse.jdt.internal.ui.jarpackagerfat.FatJarPackagerMessages;
import org.eclipse.jdt.internal.ui.jarpackagerfat.FatJarPackagerUtil;
import org.eclipse.jdt.internal.ui.jarpackagerfat.FatJarRsrcUrlAntExporter;
import org.eclipse.jdt.internal.ui.jarpackagerfat.FatJarRsrcUrlBuilder;
import org.eclipse.jdt.internal.ui.jarpackagerfat.UnpackFatJarAntExporter;
import org.eclipse.jdt.internal.ui.jarpackagerfat.UnpackFatJarBuilder;
import org.eclipse.jdt.internal.ui.jarpackagerfat.UnpackJarAntExporter;
import org.eclipse.jdt.internal.ui.jarpackagerfat.UnpackJarBuilder;
import org.eclipse.jdt.internal.ui.search.JavaSearchScopeFactory;
import org.eclipse.jdt.internal.ui.util.MainMethodSearchEngine;
import org.eclipse.jdt.internal.ui.util.SWTUtil;
import org.eclipse.jdt.internal.ui.wizards.JavaProjectWizard;
import org.eclipse.jdt.launching.IJavaLaunchConfigurationConstants;
import org.eclipse.jdt.launching.IRuntimeClasspathEntry;
import org.eclipse.jdt.launching.JavaRuntime;
import org.eclipse.jdt.ui.JavaUI;
import org.eclipse.jdt.ui.jarpackager.JarPackageData;
import org.eclipse.jface.dialogs.Dialog;
import org.eclipse.jface.dialogs.IDialogSettings;
import org.eclipse.jface.dialogs.IMessageProvider;
import org.eclipse.jface.operation.IRunnableContext;
import org.eclipse.jface.resource.JFaceResources;
import org.eclipse.jface.viewers.IStructuredSelection;
import org.eclipse.swt.SWT;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Combo;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Event;
import org.eclipse.swt.widgets.FileDialog;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Listener;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.ui.PlatformUI;

/**
 * SARL wizard page for exporting a SARL application into a single Jar file.
 * Most part of the code of this class comes from {@link JavaProjectWizard}.
 *
 * <p>This page is copied and adapted from {@link FatJarPackageWizardPage}.
 *
 * <p>TODO: The code of JDt should be changed for applying the updates within this file.
 *
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 * @since 0.8
 */
@SuppressWarnings("all")
public class FixedFatJarExportPage extends AbstractJarDestinationWizardPage  {

	private static final String PAGE_NAME = "ExportSarlApplicationPage"; //$NON-NLS-1$

	private static final String STORE_LAUNCH_CONFIGURATION_SELECTION_NAME = PAGE_NAME + ".LAUNCH_CONFIGURATION_SELECTION_NAME"; //$NON-NLS-1$

	private static final String STORE_DESTINATION_ELEMENT = PAGE_NAME + ".DESTINATION_PATH_SELECTION"; //$NON-NLS-1$

	private static final String STORE_ANTSCRIPT_SAVE = PAGE_NAME + ".ANTSCRIPT_SAVE"; //$NON-NLS-1$

	private static final String STORE_ANTSCRIPT_LOCATION = PAGE_NAME + ".ANTSCRIPT_LOCATION"; //$NON-NLS-1$

	private static final String STORE_ANTSCRIPT_LOCATION_HISTORY = PAGE_NAME + ".ANTSCRIPT_LOCATION_HISTORY"; //$NON-NLS-1$

	private static final String STORE_LIBRARY_HANDLING = PAGE_NAME + ".LIBRARY_HANDLING"; //$NON-NLS-1$

	private static final String ANTSCRIPT_EXTENSION = "xml"; //$NON-NLS-1$

	private final JarPackageData fJarPackage;

	private LibraryHandler fLibraryHandler;

	/**
	 * Model for the launch combo box. Element: {@link LaunchConfigurationElement}
	 */
	private final List<LaunchConfigurationElement> fLauchConfigurationModel;

	private Combo fLaunchConfigurationCombo;

	private Button fAntScriptSaveCheckbox;

	private Label fAntScriptLabel;

	private Combo fAntScriptNamesCombo;

	private Button fAntScriptBrowseButton;

	private IPath fAntScriptLocation;

	private Composite fLibraryHandlingGroup;

	private Button fExtractJarsRadioButton;

	private Button fPackageJarsRadioButton;

	private Button fCopyJarFilesRadioButton;

	/** Construct a wizard page for exporting a SARL application within a Jar file.
	 *
	 * @param jarPackage description of the package.
	 * @param selection the current selection.
	 */
	public FixedFatJarExportPage(JarPackageData jarPackage, IStructuredSelection selection) {
		super(PAGE_NAME, selection, jarPackage);
		setTitle(FatJarPackagerMessages.JarPackageWizardPage_title);
		setDescription(FatJarPackagerMessages.FatJarPackageWizardPage_description);
		this.fJarPackage = jarPackage;
		this.fLauchConfigurationModel = new ArrayList<>();
	}

	@Override
	public void createControl(Composite parent) {
		Composite composite= new Composite(parent, SWT.NONE);
		composite.setLayoutData(new GridData(SWT.FILL, SWT.FILL, true, true));
		GridLayout layout= new GridLayout(1, false);
		layout.marginHeight= 0;
		layout.marginWidth= 0;
		composite.setLayout(layout);

		createContentGroup(composite);

		createLibraryHandlingGroup(composite);

		Label seperator= new Label(composite, SWT.SEPARATOR | SWT.HORIZONTAL);
		seperator.setLayoutData(new GridData(SWT.FILL, SWT.TOP, true, false));

		createAntScriptGroup(composite);

		restoreWidgetValues();

		update();

		Dialog.applyDialogFont(composite);
		setControl(composite);
		PlatformUI.getWorkbench().getHelpSystem().setHelp(composite, IJavaHelpContextIds.FATJARPACKAGER_WIZARD_PAGE);
	}

	private void createContentGroup(Composite parent) {
		Composite composite= new Composite(parent, SWT.NONE);
		composite.setLayoutData(new GridData(SWT.FILL, SWT.TOP, true, false));
		composite.setLayout(new GridLayout(1, false));

		createLabel(composite, FatJarPackagerMessages.FatJarPackageWizardPage_launchConfigGroupTitle, false);
		createLaunchConfigSelectionGroup(composite);

		createLabel(composite, FatJarPackagerMessages.FatJarPackageWizardPage_destinationGroupTitle, false);
		createDestinationGroup(composite);
	}

	@Override
	protected String getDestinationLabel() {
		return null;
	}

	private void createLaunchConfigSelectionGroup(Composite parent) {
		fLaunchConfigurationCombo= new Combo(parent, SWT.DROP_DOWN | SWT.READ_ONLY);
		SWTUtil.setDefaultVisibleItemCount(fLaunchConfigurationCombo);
		fLaunchConfigurationCombo.setLayoutData(new GridData(SWT.FILL, SWT.CENTER, true, false));

		fLauchConfigurationModel.addAll(Arrays.asList(getLaunchConfigurations()));
		String[] names= new String[fLauchConfigurationModel.size()];
		for (int i= 0, size= fLauchConfigurationModel.size(); i < size; i++) {
			LaunchConfigurationElement element= fLauchConfigurationModel.get(i);
			names[i]= element.getLaunchConfigurationName();
		}
		fLaunchConfigurationCombo.setItems(names);

		fLaunchConfigurationCombo.addListener(SWT.Selection, this);
		fLaunchConfigurationCombo.addListener(SWT.Modify, this);
	}

	private void createAntScriptGroup(Composite parent) {
		Composite composite= new Composite(parent, SWT.NONE);
		GridData layoutData= new GridData(SWT.FILL, SWT.TOP, true, false);
		composite.setLayoutData(layoutData);
		GridLayout layout= new GridLayout(3, false);
		composite.setLayout(layout);

		fAntScriptSaveCheckbox= new Button(composite, SWT.CHECK | SWT.LEFT);
		fAntScriptSaveCheckbox.setText(FatJarPackagerMessages.FatJarPackageWizardPage_saveAntScript_text);
		fAntScriptSaveCheckbox.addListener(SWT.Selection, this);
		GridData data= new GridData(SWT.BEGINNING);
		data.horizontalSpan= 3;
		fAntScriptSaveCheckbox.setLayoutData(data);

		// ant script name entry field
		fAntScriptLabel= createLabel(composite, FatJarPackagerMessages.FatJarPackageWizardPage_antScriptLocation_text, false);

		fAntScriptNamesCombo= new Combo(composite, SWT.SINGLE | SWT.BORDER);
		SWTUtil.setDefaultVisibleItemCount(fAntScriptNamesCombo);
		fAntScriptNamesCombo.addListener(SWT.Modify, this);
		fAntScriptNamesCombo.addListener(SWT.Selection, this);
		data= new GridData(GridData.HORIZONTAL_ALIGN_FILL | GridData.GRAB_HORIZONTAL);
		data.widthHint= SIZING_TEXT_FIELD_WIDTH;
		data.horizontalSpan= 1;
		fAntScriptNamesCombo.setLayoutData(data);

		// ant script browse button
		fAntScriptBrowseButton= new Button(composite, SWT.PUSH);
		fAntScriptBrowseButton.setText(FatJarPackagerMessages.FatJarPackageWizardPage_antScriptLocationBrowse_text);
		fAntScriptBrowseButton.setLayoutData(new GridData(GridData.HORIZONTAL_ALIGN_FILL));
		SWTUtil.setButtonDimensionHint(fAntScriptBrowseButton);
		fAntScriptBrowseButton.addSelectionListener(new SelectionAdapter() {
			@Override
			public void widgetSelected(SelectionEvent e) {
				handleAntScriptBrowseButtonPressed();
			}
		});
	}

	/**
	 *	Open an appropriate ant script browser so that the user can specify a source
	 *	to import from
	 */
	private void handleAntScriptBrowseButtonPressed() {
		FileDialog dialog= new FileDialog(getContainer().getShell(), SWT.SAVE);
		dialog.setFilterExtensions(new String[] { "*." + ANTSCRIPT_EXTENSION }); //$NON-NLS-1$

		String currentSourceString= getAntScriptValue();
		int lastSeparatorIndex= currentSourceString.lastIndexOf(File.separator);
		if (lastSeparatorIndex != -1) {
			dialog.setFilterPath(currentSourceString.substring(0, lastSeparatorIndex));
			dialog.setFileName(currentSourceString.substring(lastSeparatorIndex + 1, currentSourceString.length()));
		}
		String selectedFileName= dialog.open();
		if (selectedFileName != null)
			fAntScriptNamesCombo.setText(selectedFileName);
	}

	/**
	 * Answer the contents of the ant script specification widget. If this
	 * value does not have the required suffix then add it first.
	 *
	 * @return java.lang.String
	 */
	private String getAntScriptValue() {
		String antScriptText= fAntScriptNamesCombo.getText().trim();
		if (antScriptText.indexOf('.') < 0)
			antScriptText+= "." + ANTSCRIPT_EXTENSION; //$NON-NLS-1$
		return antScriptText;
	}

	/**
	 * Creates a new label with an optional bold font.
	 * 
	 * @param parent the parent control
	 * @param text the label text
	 * @param bold bold or not
	 * @return the new label control
	 */
	protected Label createLabel(Composite parent, String text, boolean bold) {
		Label label= new Label(parent, SWT.NONE);
		if (bold)
			label.setFont(JFaceResources.getBannerFont());
		label.setText(text);
		GridData gridData= new GridData(SWT.BEGINNING, SWT.CENTER, false, false);
		label.setLayoutData(gridData);
		return label;
	}

	/**
	 * Create the export options specification widgets.
	 * 
	 * @param parent org.eclipse.swt.widgets.Composite
	 */
	protected void createLibraryHandlingGroup(Composite parent) {
		fLibraryHandlingGroup= new Composite(parent, SWT.NONE);
		GridLayout layout= new GridLayout();
		fLibraryHandlingGroup.setLayout(layout);
		fLibraryHandlingGroup.setLayoutData(new GridData(GridData.HORIZONTAL_ALIGN_FILL | GridData.VERTICAL_ALIGN_FILL | GridData.GRAB_HORIZONTAL));

		createLabel(fLibraryHandlingGroup, FatJarPackagerMessages.FatJarPackageWizardPage_libraryHandlingGroupTitle, false);

		fExtractJarsRadioButton= new Button(fLibraryHandlingGroup, SWT.RADIO | SWT.LEFT);
		fExtractJarsRadioButton.setText(FatJarPackagerMessages.FatJarPackageWizardPage_extractJars_text);
		fExtractJarsRadioButton.setLayoutData(new GridData(GridData.FILL_HORIZONTAL));
		fExtractJarsRadioButton.addListener(SWT.Selection, new Listener() {
			@Override
			public void handleEvent(Event event) {
				if (((Button)event.widget).getSelection())
					fLibraryHandler= new ExtractLibraryHandler();
			}
		});

		fPackageJarsRadioButton= new Button(fLibraryHandlingGroup, SWT.RADIO | SWT.LEFT);
		fPackageJarsRadioButton.setText(FatJarPackagerMessages.FatJarPackageWizardPage_packageJars_text);
		fPackageJarsRadioButton.setLayoutData(new GridData(GridData.FILL_HORIZONTAL));
		fPackageJarsRadioButton.addListener(SWT.Selection, new Listener() {
			@Override
			public void handleEvent(Event event) {
				if (((Button)event.widget).getSelection())
					fLibraryHandler= new PackageLibraryHandler();
			}
		});

		fCopyJarFilesRadioButton= new Button(fLibraryHandlingGroup, SWT.RADIO | SWT.LEFT);
		fCopyJarFilesRadioButton.setText(FatJarPackagerMessages.FatJarPackageWizardPage_copyJarFiles_text);
		fCopyJarFilesRadioButton.setLayoutData(new GridData(GridData.FILL_HORIZONTAL));
		fCopyJarFilesRadioButton.addListener(SWT.Selection, new Listener() {
			@Override
			public void handleEvent(Event event) {
				if (((Button)event.widget).getSelection())
					fLibraryHandler= new CopyLibraryHandler();
			}
		});

		// set default for first selection (no previous widget settings to restore)
		setLibraryHandler(new ExtractLibraryHandler());
	}

	public LibraryHandler getLibraryHandler() {
		return fLibraryHandler;
	}

	public void setLibraryHandler(LibraryHandler libraryHandler) {
		fLibraryHandler= libraryHandler;
		fExtractJarsRadioButton.setSelection(libraryHandler.getID() == ExtractLibraryHandler.ID);
		fPackageJarsRadioButton.setSelection(libraryHandler.getID() == PackageLibraryHandler.ID);
		fCopyJarFilesRadioButton.setSelection(libraryHandler.getID() == CopyLibraryHandler.ID);
	}

	LibraryHandler createLibraryHandlerById(int handlerId) {
		if (handlerId == PackageLibraryHandler.ID)
			return new PackageLibraryHandler();
		if (handlerId == CopyLibraryHandler.ID)
			return new CopyLibraryHandler();
		return new ExtractLibraryHandler();
	}

	/**
	 * Stores the widget values in the JAR package.
	 */
	@Override
	protected void updateModel() {
		super.updateModel();

		String comboText= fAntScriptNamesCombo.getText();
		IPath path= Path.fromOSString(comboText);
		if (path.segmentCount() > 0 && ensureAntScriptFileIsValid(path.toFile()) && path.getFileExtension() == null)
			path= path.addFileExtension(ANTSCRIPT_EXTENSION);

		fAntScriptLocation= getAbsoluteLocation(path);
	}

	@Override
	protected void updateWidgetEnablements() {
		boolean antScriptSave= fAntScriptSaveCheckbox.getSelection();
		fAntScriptLabel.setEnabled(antScriptSave);
		fAntScriptNamesCombo.setEnabled(antScriptSave);
		fAntScriptBrowseButton.setEnabled(antScriptSave);
	}

	@Override
	public boolean isPageComplete() {
		clearMessages();
		boolean complete= validateDestinationGroup();
		complete= validateLaunchConfigurationGroup() && complete;
		complete= validateAntScriptGroup() && complete;
		return complete;
	}

	private boolean validateLaunchConfigurationGroup() {
		int index= fLaunchConfigurationCombo.getSelectionIndex();
		if (index == -1)
			return false;

		LaunchConfigurationElement element= fLauchConfigurationModel.get(index);
		if (element.hasProgramArguments())
			setWarningMessage(FatJarPackagerMessages.FatJarPackageWizardPage_warning_launchConfigContainsProgramArgs);

		if (element.hasVMArguments())
			setWarningMessage(FatJarPackagerMessages.FatJarPackageWizardPage_warning_launchConfigContainsVMArgs);

		return true;
	}

	private boolean validateAntScriptGroup() {
		if (!fAntScriptSaveCheckbox.getSelection())
			// save as ant not selected
			return true;

		if (fAntScriptNamesCombo.getText().length() == 0) {
			setErrorMessage(FatJarPackagerMessages.FatJarPackageWizardPage_error_antScriptLocationMissing);
			return false;
		}

		if (fAntScriptLocation.toString().endsWith("/")) { //$NON-NLS-1$
			setErrorMessage(FatJarPackagerMessages.FatJarPackageWizardPage_error_antScriptLocationIsDir);
			fAntScriptNamesCombo.setFocus();
			return false;
		}

		// Inform user about relative directory
		if (!(new File(fAntScriptNamesCombo.getText()).isAbsolute()))
			setInfoMessage(FatJarPackagerMessages.FatJarPackageWizardPage_info_antScriptLocationRelative);

		return ensureAntScriptFileIsValid(fAntScriptLocation.toFile());
	}

	/**
	 * Gets the absolute location relative to the workspace.
	 * 
	 * @param location the location
	 * @return the absolute path for the location of the file
	 * 
	 * @since 3.8
	 */
	private IPath getAbsoluteLocation(IPath location) {
		if (location.isAbsolute())
			return location;

		IWorkspaceRoot root= ResourcesPlugin.getWorkspace().getRoot();
		if (location.segmentCount() >= 2 && !"..".equals(location.segment(0))) { //$NON-NLS-1$
			IFile file= root.getFile(location);
			IPath absolutePath= file.getLocation();
			if (absolutePath != null) {
				return absolutePath;
			}
		}
		// The path does not exist in the workspace (e.g. because there's no such project).
		// Fallback is to just append the path to the workspace root.
		return root.getLocation().append(location);

	}

	/**
	 * Returns a boolean indicating whether the passed File handle is is valid and available for
	 * use.
	 * 
	 * @param antScriptFile the ant script
	 * @return boolean
	 */
	private boolean ensureAntScriptFileIsValid(File antScriptFile) {
		if (antScriptFile.exists() && antScriptFile.isDirectory() && fAntScriptNamesCombo.getText().length() > 0) {
			setErrorMessage(FatJarPackagerMessages.FatJarPackageWizardPage_error_antScriptLocationIsDir);
			fAntScriptNamesCombo.setFocus();
			return false;
		}
		if (antScriptFile.exists()) {
			if (!antScriptFile.canWrite()) {
				setErrorMessage(FatJarPackagerMessages.FatJarPackageWizardPage_error_antScriptLocationUnwritable);
				fAntScriptNamesCombo.setFocus();
				return false;
			}
		}
		return true;
	}

	/**
	 * clear all previously set messages and error-messages
	 */
	private void clearMessages() {
		if (getErrorMessage() != null)
			setErrorMessage(null);
		if (getMessage() != null)
			setMessage(null);
	}

	/**
	 * set message to newMessage with severity WARNING.
	 * overwrite existing message only if it is beyond severity WARNING
	 * @param newMessage the warning to be set
	 */
	private void setWarningMessage(String newMessage) {
		if (getMessage() == null || getMessageType() < IMessageProvider.WARNING)
			setMessage(newMessage, IMessageProvider.WARNING);
	}

	/**
	 * set message to newMessage with severity WARNING.
	 * overwrite existing message only if it is beyond severity WARNING
	 * @param newMessage the warning to be set
	 */
	private void setInfoMessage(String newMessage) {
		if (getMessage() == null || getMessageType() < IMessageProvider.INFORMATION)
			setMessage(newMessage, IMessageProvider.INFORMATION);
	}

	//TODO: Replace "private" by "protected" within JDT
	protected LaunchConfigurationElement[] getLaunchConfigurations() {
		ArrayList<ExistingLaunchConfigurationElement> result= new ArrayList<>();

		try {
			ILaunchManager manager= DebugPlugin.getDefault().getLaunchManager();
			ILaunchConfigurationType type= manager.getLaunchConfigurationType(IJavaLaunchConfigurationConstants.ID_JAVA_APPLICATION);
			ILaunchConfiguration[] launchconfigs= manager.getLaunchConfigurations(type);

			for (int i= 0; i < launchconfigs.length; i++) {
				ILaunchConfiguration launchconfig= launchconfigs[i];
				if (!launchconfig.getAttribute(IDebugUIConstants.ATTR_PRIVATE, false)) {
					String projectName= launchconfig.getAttribute(IJavaLaunchConfigurationConstants.ATTR_PROJECT_NAME, ""); //$NON-NLS-1$
					result.add(new ExistingLaunchConfigurationElement(launchconfig, projectName));
				}
			}
		} catch (CoreException e) {
			JavaPlugin.log(e);
		}

		return result.toArray(new LaunchConfigurationElement[result.size()]);
	}

	public Object[] getSelectedElementsWithoutContainedChildren(MultiStatus status) {
		try {
			LaunchConfigurationElement element= fLauchConfigurationModel.get(fLaunchConfigurationCombo.getSelectionIndex());
			ILaunchConfiguration launchconfig= element.getLaunchConfiguration();
			fJarPackage.setLaunchConfigurationName(element.getLaunchConfigurationName());

			return getSelectedElementsWithoutContainedChildren(launchconfig, fJarPackage, getContainer(), status);
		} catch (CoreException e) {
			JavaPlugin.log(e);
			return new Object[0];
		}
	}

	//TODO: Replace "private" by "protected within JDT
	protected static IJavaProject[] getProjectSearchOrder(String projectName) {

		ArrayList<String> projectNames= new ArrayList<>();
		projectNames.add(projectName);

		int nextProject= 0;
		while (nextProject < projectNames.size()) {
			String nextProjectName= projectNames.get(nextProject);
			IJavaProject jproject= getJavaProject(nextProjectName);

			if (jproject != null) {
				try {
					String[] childProjectNames= jproject.getRequiredProjectNames();
					for (int i= 0; i < childProjectNames.length; i++) {
						if (!projectNames.contains(childProjectNames[i])) {
							projectNames.add(childProjectNames[i]);
						}
					}
				} catch (JavaModelException e) {
					JavaPlugin.log(e);
				}
			}
			nextProject+= 1;
		}

		ArrayList<IJavaProject> result= new ArrayList<>();
		for (int i= 0, size= projectNames.size(); i < size; i++) {
			String name= projectNames.get(i);
			IJavaProject project= getJavaProject(name);
			if (project != null)
				result.add(project);
		}

		return result.toArray(new IJavaProject[result.size()]);
	}

	//TODO: Replace "private" by "protected within JDT
	protected static IJavaProject getJavaProject(String projectName) {
		IProject project= ResourcesPlugin.getWorkspace().getRoot().getProject(projectName);
		if (project == null)
			return null;

		IJavaProject result= JavaCore.create(project);
		if (result == null)
			return null;

		if (!result.exists())
			return null;

		return result;
	}

	// TODO: Remove static modifier from JDT
	// TODO: Replace "private" by "protected" within JDT
	protected IPath[] getClasspath(ILaunchConfiguration configuration) throws CoreException {
		IRuntimeClasspathEntry[] entries= JavaRuntime.computeUnresolvedRuntimeClasspath(configuration);
		entries= JavaRuntime.resolveRuntimeClasspath(entries, configuration);

		boolean isModularConfig= JavaRuntime.isModularConfiguration(configuration);
		ArrayList<IPath> userEntries= new ArrayList<>(entries.length);
		for (int i= 0; i < entries.length; i++) {
			int classPathProperty= entries[i].getClasspathProperty();
			if ((!isModularConfig && classPathProperty == IRuntimeClasspathEntry.USER_CLASSES)
					|| (isModularConfig && (classPathProperty == IRuntimeClasspathEntry.CLASS_PATH || classPathProperty == IRuntimeClasspathEntry.MODULE_PATH))) {

				String location= entries[i].getLocation();
				if (location != null) {
					IPath entry= Path.fromOSString(location);
					if (!userEntries.contains(entry)) {
						userEntries.add(entry);
					}
				}
			}
		}
		return userEntries.toArray(new IPath[userEntries.size()]);
	}

	private static String getMainClass(ILaunchConfiguration launchConfig, MultiStatus status) {
		String result= null;
		if (launchConfig != null) {
			try {
				result= launchConfig.getAttribute(IJavaLaunchConfigurationConstants.ATTR_MAIN_TYPE_NAME, (String) null);
			} catch (CoreException e) {
				JavaPlugin.log(e);
			}
		}
		if (result == null) {
			status.add(new Status(IStatus.WARNING, JavaUI.ID_PLUGIN, FatJarPackagerMessages.FatJarPackageWizardPage_LaunchConfigurationWithoutMainType_warning));
			result= ""; //$NON-NLS-1$
		}
		return result;
	}

	/**
	 * @param classpathEntries the path to the package fragment roots
	 * @param projectName the root of the project dependency tree
	 * @param status a status to report problems to
	 * @return all package fragment roots corresponding to each classpath entry start the search at project with projectName
	 */
	// TODO: Remove "static" modifier from JDT
	// TODO: Replace "private" by "protected" within JDT
	protected IPackageFragmentRoot[] getRequiredPackageFragmentRoots(IPath[] classpathEntries, final String projectName, MultiStatus status) {
		ArrayList<IPackageFragmentRoot> result= new ArrayList<>();

		IJavaProject[] searchOrder= getProjectSearchOrder(projectName);
		
		for (int i= 0; i < classpathEntries.length; i++) {
			IPath entry= classpathEntries[i];
			IPackageFragmentRoot[] elements= findRootsForClasspath(entry, searchOrder);
			if (elements == null) {
				status.add(new Status(IStatus.WARNING, JavaUI.ID_PLUGIN,
						org.eclipse.jdt.internal.corext.util.Messages.format(
								FatJarPackagerMessages.FatJarPackageWizardPage_error_missingClassFile,
								getPathLabel(entry, false))));
			} else {
				for (int j= 0; j < elements.length; j++) {
					result.add(elements[j]);
				}
			}
		}

		return result.toArray(new IPackageFragmentRoot[result.size()]);
	}

	/**
	 * Returns the label of a path.
	 *
	 * @param path the path
	 * @param isOSPath if <code>true</code>, the path represents an OS path, if <code>false</code> it is a workspace path.
	 * @return the label of the path to be used in the UI.
	 */
	protected static String getPathLabel(IPath path, boolean isOSPath) {
		String label;
		if (isOSPath) {
			label= path.toOSString();
		} else {
			label= path.makeRelative().toString();
		}
		return label;
	}
	
	//TODO: Replace "private" by "protected within JDT
	protected static IPackageFragmentRoot[] findRootsForClasspath(IPath entry, IJavaProject[] searchOrder) {
		for (int i= 0; i < searchOrder.length; i++) {
			IPackageFragmentRoot[] elements= findRootsInProject(entry, searchOrder[i]);
			if (elements.length != 0) {
				return elements;
			}
		}
		return null;
	}

	private static IPackageFragmentRoot[] findRootsInProject(IPath entry, IJavaProject project) {
		ArrayList<IPackageFragmentRoot> result= new ArrayList<>();

		try {
			IPackageFragmentRoot[] roots= project.getPackageFragmentRoots();
			for (int i= 0; i < roots.length; i++) {
				IPackageFragmentRoot packageFragmentRoot= roots[i];
				if (isRootAt(packageFragmentRoot, entry))
					result.add(packageFragmentRoot);
			}
		} catch (Exception e) {
			JavaPlugin.log(e);
		}

		return result.toArray(new IPackageFragmentRoot[result.size()]);
	}

	private static boolean isRootAt(IPackageFragmentRoot root, IPath entry) {
		try {
			IClasspathEntry cpe= root.getRawClasspathEntry();
			if (cpe.getEntryKind() == IClasspathEntry.CPE_SOURCE) {
				IPath outputLocation= cpe.getOutputLocation();
				if (outputLocation == null)
					outputLocation= root.getJavaProject().getOutputLocation();

				IPath location= ResourcesPlugin.getWorkspace().getRoot().findMember(outputLocation).getLocation();
				if (entry.equals(location))
					return true;
			}
		} catch (JavaModelException e) {
			JavaPlugin.log(e);
		}

		IResource resource= root.getResource();
		if (resource != null && entry.equals(resource.getLocation()))
			return true;

		IPath path= root.getPath();
		if (path != null && entry.equals(path))
			return true;

		return false;
	}

	// TODO Replace "private" by "protected into the JDT.
	protected static IType findMainMethodByName(String name, IPackageFragmentRoot[] classpathResources, IRunnableContext context) {

		List<IResource> resources= JarPackagerUtil.asResources(classpathResources);
		if (resources == null) {
			return null;
		}

		for (Iterator<IResource> iterator= resources.iterator(); iterator.hasNext();) {
			IResource element= iterator.next();
			if (element == null)
				iterator.remove();
		}

		IJavaSearchScope searchScope= JavaSearchScopeFactory.getInstance().createJavaSearchScope(resources.toArray(new IResource[resources.size()]), true);
		MainMethodSearchEngine engine= new MainMethodSearchEngine();
		try {
			IType[] mainTypes= engine.searchMainMethods(context, searchScope, 0);
			for (int i= 0; i < mainTypes.length; i++) {
				if (mainTypes[i].getFullyQualifiedName().equals(name))
					return mainTypes[i];
			}
		} catch (InvocationTargetException ex) {
			JavaPlugin.log(ex);
		} catch (InterruptedException e) {
			// null
		}

		return null;
	}

	@Override
	public void dispose() {
		super.dispose();
		if (fLauchConfigurationModel != null) {
			for (int i= 0, size= fLauchConfigurationModel.size(); i < size; i++) {
				LaunchConfigurationElement element= fLauchConfigurationModel.get(i);
				element.dispose();
			}
		}
	}

	@Override
	protected void restoreWidgetValues() {

		// restore JARPACKAGEDATA from SETTINGS and set widgets
		IDialogSettings settings= getDialogSettings();
		if (settings != null) {
			// SAVE ANT SCRIPT
			fAntScriptSaveCheckbox.setSelection(settings.getBoolean(STORE_ANTSCRIPT_SAVE));

			// ANT SCRIPT LOCATION
			String antScriptLocation= settings.get(STORE_ANTSCRIPT_LOCATION);
			if (antScriptLocation != null) {
				fAntScriptLocation= Path.fromOSString(antScriptLocation);
				if (fAntScriptLocation.isEmpty()) {
					fAntScriptNamesCombo.setText(""); //$NON-NLS-1$
				} else {
					fAntScriptNamesCombo.setText(fAntScriptLocation.toOSString());
				}
			}

			// ANT SCRIPT LOCATION HISTORY
			String[] directoryNames= settings.getArray(STORE_ANTSCRIPT_LOCATION_HISTORY);
			if (directoryNames != null) {
				if (!fAntScriptNamesCombo.getText().equals(directoryNames[0]))
					fAntScriptNamesCombo.add(fAntScriptNamesCombo.getText());
				for (int i= 0; i < directoryNames.length; i++)
					fAntScriptNamesCombo.add(directoryNames[i]);
			}

			// LIBRARY HANDLING
			int libraryHandling= ExtractLibraryHandler.ID; // default value for migrated (Eclipse 3.4) settings
			try {
				libraryHandling= settings.getInt(STORE_LIBRARY_HANDLING);
			} catch (NumberFormatException ignore) { // also thrown if no value was stored (null)
			}
			setLibraryHandler(createLibraryHandlerById(libraryHandling));

			// LAUNCH CONFIG
			String name= settings.get(STORE_LAUNCH_CONFIGURATION_SELECTION_NAME);
			if (name != null) {
				String[] items= fLaunchConfigurationCombo.getItems();
				for (int i= 0; i < items.length; i++) {
					if (name.equals(items[i])) {
						fLaunchConfigurationCombo.select(i);
					}
				}
			}

			// DESTINATION
			String destinationPath= settings.get(STORE_DESTINATION_ELEMENT);
			if (destinationPath != null && destinationPath.length() > 0) {
				fJarPackage.setJarLocation(Path.fromOSString(destinationPath));
			}
		}

		super.restoreWidgetValues();

	}

	@Override
	protected void saveWidgetValues() {
		super.saveWidgetValues();

		IDialogSettings settings= getDialogSettings();
		if (settings != null) {
			// ANT SCRIPT SAVE
			settings.put(STORE_ANTSCRIPT_SAVE, fAntScriptSaveCheckbox.getSelection());

			// ANT SCRIPT LOCATION
			IPath antScriptLocation= fAntScriptLocation;
			if (antScriptLocation == null) {
				settings.put(STORE_ANTSCRIPT_LOCATION, ""); //$NON-NLS-1$
			} else {
				settings.put(STORE_ANTSCRIPT_LOCATION, antScriptLocation.toOSString());
			}

			// ANT SCRIPT LOCATION HISTORY
			String[] directoryNames= settings.getArray(STORE_ANTSCRIPT_LOCATION_HISTORY);
			if (directoryNames == null)
				directoryNames= new String[0];
			directoryNames= addToHistory(directoryNames, getAntScriptValue());
			settings.put(STORE_ANTSCRIPT_LOCATION_HISTORY, directoryNames);

			// LIBRARY HANDLING
			settings.put(STORE_LIBRARY_HANDLING, getLibraryHandler().getID());

			// LAUNCH CONFIG
			int index= fLaunchConfigurationCombo.getSelectionIndex();
			if (index == -1) {
				settings.put(STORE_LAUNCH_CONFIGURATION_SELECTION_NAME, ""); //$NON-NLS-1$
			} else {
				String selectedItem= fLaunchConfigurationCombo.getItem(index);
				settings.put(STORE_LAUNCH_CONFIGURATION_SELECTION_NAME, selectedItem);
			}

			// DESTINATION
			IPath location= fJarPackage.getJarLocation();
			if (location == null) {
				settings.put(STORE_DESTINATION_ELEMENT, ""); //$NON-NLS-1$
			} else {
				settings.put(STORE_DESTINATION_ELEMENT, location.toOSString());
			}
		}
	}

	/*
	 * For internal use only (testing), clients must not call.
	 */
	// TODO Remove "static" modifier from JDT
	public Object[] getSelectedElementsWithoutContainedChildren(ILaunchConfiguration launchconfig, JarPackageData data, IRunnableContext context, MultiStatus status) throws CoreException {
		if (launchconfig == null)
			return new Object[0];

		String projectName= launchconfig.getAttribute(IJavaLaunchConfigurationConstants.ATTR_PROJECT_NAME, ""); //$NON-NLS-1$

		IPath[] classpath= getClasspath(launchconfig);
		IPackageFragmentRoot[] classpathResources= getRequiredPackageFragmentRoots(classpath, projectName, status);

		String mainClass= getMainClass(launchconfig, status);
		IType mainType= findMainMethodByName(mainClass, classpathResources, context);
		if (mainType == null) {
			status.add(new Status(IStatus.ERROR, JavaUI.ID_PLUGIN, FatJarPackagerMessages.FatJarPackageWizardPage_error_noMainMethod));
		}
		data.setManifestMainClass(mainType);

		return classpathResources;
	}

	public void exportAntScript(MultiStatus status) {
		if (!fAntScriptSaveCheckbox.getSelection())
			return;

		if (canCreateAntScript(getShell())) {
			LaunchConfigurationElement element= fLauchConfigurationModel.get(fLaunchConfigurationCombo.getSelectionIndex());
			Assert.isNotNull(element);
			FatJarAntExporter antExporter= getLibraryHandler().getAntExporter(fAntScriptLocation, fJarPackage.getAbsoluteJarLocation(), element.getLaunchConfiguration());
			try {
				antExporter.run(status);
			} catch (CoreException e) {
				status.add(new Status(IStatus.ERROR, JavaUI.ID_PLUGIN, FatJarPackagerMessages.FatJarPackageWizardPage_error_ant_script_generation_failed, e));
			}
		}
	}

	/**
	 * Checks if the ANT script file can be overwritten.
	 * If the JAR package setting does not allow to overwrite the JAR
	 * then a dialog will ask the user again.
	 *
	 * @param	parent	the parent for the dialog,
	 * 			or {@code null} if no dialog should be presented
	 * @return	<code>true</code> if it is OK to create the JAR
	 */
	private boolean canCreateAntScript(Shell parent) {

		File file= fAntScriptLocation.toFile();
		if (file.exists()) {
			if (!file.canWrite())
				return false;

			if (fJarPackage.allowOverwrite())
				return true;

			return parent != null && JarPackagerUtil.askForOverwritePermission(parent, fAntScriptLocation, true);
		}

		// Test if directory exists
		String path = file.getAbsolutePath();
		int separatorIndex= path.lastIndexOf(File.separator);
		if (separatorIndex == -1) // i.e.- default directory, which is fine
			return true;

		File directory= new File(path.substring(0, separatorIndex));
		if (!directory.exists()) {
			if (FatJarPackagerUtil.askToCreateAntScriptDirectory(parent, directory))
				return directory.mkdirs();
			else
				return false;
		}

		return true;
	}

	public static abstract class LibraryHandler {
		public abstract FatJarAntExporter getAntExporter(IPath antScriptLocation, IPath jarLocation, ILaunchConfiguration launchConfiguration);

		public abstract FatJarBuilder getBuilder(JarPackageData jarPackageData);

		public abstract int getID();

		public abstract boolean isShowWarning();
	}

	public static class ExtractLibraryHandler extends LibraryHandler {

		public final static int ID= 1;

		public ExtractLibraryHandler() {
		}

		@Override
		public FatJarAntExporter getAntExporter(IPath antScriptLocation, IPath jarLocation, ILaunchConfiguration launchConfiguration) {
			return new UnpackFatJarAntExporter(antScriptLocation, jarLocation, launchConfiguration);
		}

		@Override
		public FatJarBuilder getBuilder(JarPackageData jarPackageData) {
			return new UnpackFatJarBuilder();
		}

		@Override
		public int getID() {
			return ID;
		}

		@Override
		public boolean isShowWarning() {
			return true;
		}
	}

	public static class PackageLibraryHandler extends LibraryHandler {

		public final static int ID= 2;

		public PackageLibraryHandler() {
		}

		@Override
		public FatJarAntExporter getAntExporter(IPath antScriptLocation, IPath jarLocation, ILaunchConfiguration launchConfiguration) {
			return new FatJarRsrcUrlAntExporter(antScriptLocation, jarLocation, launchConfiguration);
		}

		@Override
		public FatJarBuilder getBuilder(JarPackageData jarPackageData) {
			return new FatJarRsrcUrlBuilder();
		}

		@Override
		public int getID() {
			return ID;
		}

		@Override
		public boolean isShowWarning() {
			return false;
		}
	}

	public static class CopyLibraryHandler extends LibraryHandler {

		public final static int ID= 3;

		public CopyLibraryHandler() {
		}

		@Override
		public FatJarAntExporter getAntExporter(IPath antScriptLocation, IPath jarLocation, ILaunchConfiguration launchConfiguration) {
			return new UnpackJarAntExporter(antScriptLocation, jarLocation, launchConfiguration);
		}

		@Override
		public FatJarBuilder getBuilder(JarPackageData jarPackageData) {
			return new UnpackJarBuilder(jarPackageData);
		}

		@Override
		public int getID() {
			return ID;
		}

		@Override
		public boolean isShowWarning() {
			return false;
		}
	}

	//TODO: Replace "private" by "protected" within JDT
	protected abstract static class LaunchConfigurationElement {

		public abstract ILaunchConfiguration getLaunchConfiguration();

		public abstract String getLaunchConfigurationName();

		public abstract boolean hasProgramArguments();

		public abstract boolean hasVMArguments();

		public void dispose() {
			//do nothing
		}
	}

	//TODO: Replace "private" by "protected" within JDT
	protected static class ExistingLaunchConfigurationElement extends LaunchConfigurationElement {

		private final ILaunchConfiguration fLaunchConfiguration;
		private final String fProjectName;

		public ExistingLaunchConfigurationElement(ILaunchConfiguration launchConfiguration, String projectName) {
			fLaunchConfiguration= launchConfiguration;
			fProjectName= projectName;
		}

		@Override
		public ILaunchConfiguration getLaunchConfiguration() {
			return fLaunchConfiguration;
		}

		@Override
		public String getLaunchConfigurationName() {
			StringBuffer result= new StringBuffer();

			result.append(fLaunchConfiguration.getName());
			result.append(" - "); //$NON-NLS-1$
			result.append(fProjectName);

			return result.toString();
		}

		@Override
		public boolean hasProgramArguments() {
			try {
				return fLaunchConfiguration.getAttribute(IJavaLaunchConfigurationConstants.ATTR_PROGRAM_ARGUMENTS, (String) null) != null;
			} catch (CoreException e) {
				JavaPlugin.log(e);
				return false;
			}
		}

		@Override
		public boolean hasVMArguments() {
			try {
				return fLaunchConfiguration.getAttribute(IJavaLaunchConfigurationConstants.ATTR_VM_ARGUMENTS, (String) null) != null;
			} catch (CoreException e) {
				JavaPlugin.log(e);
				return false;
			}
		}

	}

}
