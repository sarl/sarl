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
package io.sarl.eclipse.wizards.newproject;

import io.sarl.eclipse.SARLConfig;
import io.sarl.eclipse.SARLEclipsePlugin;
import io.sarl.eclipse.buildpath.SARLClasspathContainerInitializer;
import io.sarl.eclipse.runtime.ISREInstall;
import io.sarl.eclipse.runtime.SREConfigurationBlock;

import java.io.File;
import java.io.IOException;
import java.net.URI;
import java.text.MessageFormat;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.Comparator;
import java.util.HashMap;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.Observable;
import java.util.Observer;

import org.eclipse.core.filesystem.URIUtil;
import org.eclipse.core.resources.IProject;
import org.eclipse.core.resources.IResource;
import org.eclipse.core.resources.IWorkspace;
import org.eclipse.core.resources.ResourcesPlugin;
import org.eclipse.core.runtime.IPath;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Path;
import org.eclipse.core.runtime.Platform;
import org.eclipse.equinox.bidi.StructuredTextTypeHandlerFactory;
import org.eclipse.jdt.core.IAccessRule;
import org.eclipse.jdt.core.IClasspathAttribute;
import org.eclipse.jdt.core.IClasspathEntry;
import org.eclipse.jdt.core.JavaCore;
import org.eclipse.jdt.internal.core.ClasspathEntry;
import org.eclipse.jdt.internal.corext.util.JavaModelUtil;
import org.eclipse.jdt.internal.ui.IJavaHelpContextIds;
import org.eclipse.jdt.internal.ui.JavaPlugin;
import org.eclipse.jdt.internal.ui.packageview.PackageExplorerPart;
import org.eclipse.jdt.internal.ui.preferences.CompliancePreferencePage;
import org.eclipse.jdt.internal.ui.preferences.PropertyAndPreferencePage;
import org.eclipse.jdt.internal.ui.viewsupport.BasicElementLabels;
import org.eclipse.jdt.internal.ui.wizards.NewWizardMessages;
import org.eclipse.jdt.internal.ui.wizards.buildpaths.BuildPathSupport;
import org.eclipse.jdt.internal.ui.wizards.dialogfields.ComboDialogField;
import org.eclipse.jdt.internal.ui.wizards.dialogfields.DialogField;
import org.eclipse.jdt.internal.ui.wizards.dialogfields.IDialogFieldListener;
import org.eclipse.jdt.internal.ui.wizards.dialogfields.IStringButtonAdapter;
import org.eclipse.jdt.internal.ui.wizards.dialogfields.LayoutUtil;
import org.eclipse.jdt.internal.ui.wizards.dialogfields.SelectionButtonDialogField;
import org.eclipse.jdt.internal.ui.wizards.dialogfields.StringButtonDialogField;
import org.eclipse.jdt.internal.ui.wizards.dialogfields.StringDialogField;
import org.eclipse.jdt.internal.ui.workingsets.IWorkingSetIDs;
import org.eclipse.jdt.launching.IVMInstall;
import org.eclipse.jdt.launching.IVMInstall2;
import org.eclipse.jdt.launching.IVMInstallType;
import org.eclipse.jdt.launching.JavaRuntime;
import org.eclipse.jdt.launching.VMStandin;
import org.eclipse.jdt.launching.environments.IExecutionEnvironment;
import org.eclipse.jdt.ui.JavaUI;
import org.eclipse.jdt.ui.PreferenceConstants;
import org.eclipse.jdt.ui.wizards.NewJavaProjectWizardPageOne;
import org.eclipse.jface.dialogs.Dialog;
import org.eclipse.jface.dialogs.IDialogConstants;
import org.eclipse.jface.dialogs.IDialogSettings;
import org.eclipse.jface.util.BidiUtils;
import org.eclipse.jface.util.Policy;
import org.eclipse.jface.viewers.IStructuredSelection;
import org.eclipse.jface.viewers.ITreeSelection;
import org.eclipse.jface.viewers.TreePath;
import org.eclipse.jface.wizard.WizardPage;
import org.eclipse.swt.SWT;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.events.SelectionListener;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Combo;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.DirectoryDialog;
import org.eclipse.swt.widgets.Group;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Link;
import org.eclipse.ui.IWorkbenchPart;
import org.eclipse.ui.IWorkingSet;
import org.eclipse.ui.PlatformUI;
import org.eclipse.ui.dialogs.PreferencesUtil;
import org.eclipse.ui.dialogs.WorkingSetConfigurationBlock;

/**
 * The first page of the SARL new project wizard. Most part of the code of this class
 * is copy/paste from {@link NewJavaProjectWizardPageOne}
 *
 * This version removes the choice of the project structure and update the structure of
 * the source folder of the project
 * {@link MainProjectWizardPage#getSourceClasspathEntries()}
 *
 * @author $Author: ngaud$
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 */
public class MainProjectWizardPage extends WizardPage {

	private static final IWorkingSet[] EMPTY_WORKING_SET_ARRAY = new IWorkingSet[0];

	private final NameGroup fNameGroup;
	private final LocationGroup fLocationGroup;
	private final SREConfigurationBlock fSREGroup;
	private final JREGroup fJREGroup;
	private final DetectGroup fDetectGroup;
	private final Validator fValidator;
	private final WorkingSetGroup fWorkingSetGroup;

	/**
	 * Creates a new {@link MainProjectWizardPage}.
	 */
	@SuppressWarnings("synthetic-access")
	public MainProjectWizardPage() {
		super(Messages.SARLProjectNewWizard_3);
		setPageComplete(false);

		this.fNameGroup = new NameGroup();
		this.fLocationGroup = new LocationGroup();
		this.fSREGroup = new SREConfigurationBlock(Messages.MainProjectPage_0, true, null, null);
		this.fJREGroup = new JREGroup();
		this.fWorkingSetGroup = new WorkingSetGroup();
		this.fDetectGroup = new DetectGroup();

		// establish connections
		this.fNameGroup.addObserver(this.fLocationGroup);
		this.fDetectGroup.addObserver(this.fJREGroup);
		this.fLocationGroup.addObserver(this.fDetectGroup);

		// initialize all elements
		this.fNameGroup.notifyObservers();

		// create and connect validator
		this.fValidator = new Validator();
		this.fNameGroup.addObserver(this.fValidator);
		this.fLocationGroup.addObserver(this.fValidator);

		// initialize defaults
		setProjectName(""); //$NON-NLS-1$
		setProjectLocationURI(null);
		setWorkingSets(new IWorkingSet[0]);

		initializeDefaultVM();

		setTitle(Messages.SARLProjectNewWizard_3);
		setDescription(Messages.SARLProjectNewWizard_1);
		setImageDescriptor(SARLEclipsePlugin.getDefault().getImageDescriptor(
				SARLConfig.NEW_PROJECT_WIZARD_DIALOG_IMAGE));
	}

	/**
	 * The wizard owning this page can call this method to initialize the fields from the current selection and active part.
	 *
	 * @param selection used to initialize the fields
	 * @param activePart the (typically active) part to initialize the fields or <code>null</code>
	 */
	public void init(IStructuredSelection selection, IWorkbenchPart activePart) {
		setWorkingSets(getSelectedWorkingSet(selection, activePart));
	}

	private static void initializeDefaultVM() {
		JavaRuntime.getDefaultVMInstall();
	}

	@Override
	public void createControl(Composite parent) {
		initializeDialogUnits(parent);

		final Composite composite = new Composite(parent, SWT.NULL);
		composite.setFont(parent.getFont());
		composite.setLayout(initGridLayout(new GridLayout(1, false), true));
		composite.setLayoutData(new GridData(GridData.HORIZONTAL_ALIGN_FILL));

		// create UI elements
		Control nameControl = createNameControl(composite);
		nameControl.setLayoutData(new GridData(GridData.FILL_HORIZONTAL));

		Control locationControl = createLocationControl(composite);
		locationControl.setLayoutData(new GridData(GridData.FILL_HORIZONTAL));

		Control sreControl = createSRESelectionControl(composite);
		sreControl.setLayoutData(new GridData(GridData.FILL_HORIZONTAL));

		Control jreControl = createJRESelectionControl(composite);
		jreControl.setLayoutData(new GridData(GridData.FILL_HORIZONTAL));

		Control workingSetControl = createWorkingSetControl(composite);
		workingSetControl.setLayoutData(new GridData(GridData.FILL_HORIZONTAL));

		Control infoControl = createInfoControl(composite);
		infoControl.setLayoutData(new GridData(GridData.FILL_HORIZONTAL));

		setControl(composite);

		this.fSREGroup.initialize();
		this.fSREGroup.selectSpecificSRE(null);
		this.fSREGroup.selectSystemWideSRE();
	}

	@Override
	protected void setControl(Control newControl) {
		Dialog.applyDialogFont(newControl);

		PlatformUI.getWorkbench().getHelpSystem().setHelp(newControl, IJavaHelpContextIds.NEW_JAVAPROJECT_WIZARD_PAGE);

		super.setControl(newControl);
	}

	/**
	 * Creates the controls for the name field.
	 *
	 * @param composite the parent composite
	 * @return the created control
	 */
	protected Control createNameControl(Composite composite) {
		return this.fNameGroup.createControl(composite);
	}

	/**
	 * Creates the controls for the location field.
	 *
	 * @param composite the parent composite
	 * @return the created control
	 */
	protected Control createLocationControl(Composite composite) {
		return this.fLocationGroup.createControl(composite);
	}

	/**
	 * Creates the controls for the JRE selection.
	 *
	 * @param composite the parent composite
	 * @return the created control
	 */
	protected Control createJRESelectionControl(Composite composite) {
		return this.fJREGroup.createControl(composite);
	}

	/**
	 * Creates the controls for the SRE selection.
	 *
	 * @param composite the parent composite
	 * @return the created control
	 */
	protected Control createSRESelectionControl(Composite composite) {
		return this.fSREGroup.createControl(composite);
	}

	/**
	 * Creates the controls for the working set selection.
	 *
	 * @param composite the parent composite
	 * @return the created control
	 */
	protected Control createWorkingSetControl(Composite composite) {
		return this.fWorkingSetGroup.createControl(composite);
	}

	/**
	 * Creates the controls for the info section.
	 *
	 * @param composite the parent composite
	 * @return the created control
	 */
	protected Control createInfoControl(Composite composite) {
		return this.fDetectGroup.createControl(composite);
	}

	/**
	 * Gets a project name for the new project.
	 *
	 * @return the new project resource handle
	 */
	public String getProjectName() {
		return this.fNameGroup.getName();
	}

	/**
	 * Sets the name of the new project.
	 *
	 * @param name the new name
	 */
	public void setProjectName(String name) {
		if (name == null) {
			throw new IllegalArgumentException();
		}

		this.fNameGroup.setName(name);
	}

	/** Replies if the system default SRE must be used for the new project.
	 *
	 * @return <code>true</code> if the system default must be used.
	 */
	public boolean isSystemDefaultSRE() {
		return this.fSREGroup.isSystemWideDefaultSRE();
	}

	/** Replies the selected SRE.
	 *
	 * @return the SRE.
	 */
	public ISREInstall getSRE() {
		return this.fSREGroup.getSelectedSRE();
	}

	/**
	 * Returns the current project location path as entered by the user, or <code>null</code> if
	 * the project should be created in the workspace.
	 *
	 * @return the project location path or its anticipated initial value.
	 */
	public URI getProjectLocationURI() {
		if (this.fLocationGroup.isUseDefaultSelected()) {
			return null;
		}
		return URIUtil.toURI(this.fLocationGroup.getLocation());
	}

	/**
	 * Sets the project location of the new project or <code>null</code> if the project should be created in the workspace.
	 *
	 * @param uri the new project location.
	 */
	public void setProjectLocationURI(URI uri) {
		IPath path = uri != null ? URIUtil.toPath(uri) : null;
		this.fLocationGroup.setLocation(path);
	}

	/**
	 * Returns the compiler compliance to be used for the project, or <code>null</code> to use the workspace compiler compliance.
	 *
	 * @return compiler compliance to be used for the project or <code>null</code>
	 */
	public String getCompilerCompliance() {
		return this.fJREGroup.getSelectedCompilerCompliance();
	}

	/**
	 * Returns the default class path entries to be added on new projects.
	 * By default this is the JRE container as selected by the user.
	 *
	 * @param classpathEntries - the collection in which the classpath entries will be added.
	 */
	public void putDefaultClasspathEntriesIn(Collection<IClasspathEntry> classpathEntries) {
		IPath newPath = this.fJREGroup.getJREContainerPath();
		if (newPath != null) {
			classpathEntries.add(JavaCore.newContainerEntry(newPath));
		} else {
			IClasspathEntry[] entries = PreferenceConstants.getDefaultJRELibrary();
			classpathEntries.addAll(Arrays.asList(entries));
		}

		IClasspathEntry sarlClasspathEntry = JavaCore.newContainerEntry(
				SARLClasspathContainerInitializer.CONTAINER_ID,
				new IAccessRule[0],
				new IClasspathAttribute[0],
				true);
		classpathEntries.add(sarlClasspathEntry);
	}

	/**
	 * Returns the source class path entries to be added on new projects. The underlying resources may not
	 * exist. All entries that are returned must be of kind {@link IClasspathEntry#CPE_SOURCE}.
	 *
	 * @return returns the source class path entries for the new project
	 */
	public Iterable<IClasspathEntry> getSourceClasspathEntries() {
		IPath sourceFolderPath = new Path(getProjectName()).makeAbsolute();

		IPath srcJava = sourceFolderPath.append(
				Path.fromPortableString(SARLConfig.FOLDER_SOURCE_JAVA));
		IClasspathEntry srcJavaEntry = JavaCore.newSourceEntry(srcJava.makeAbsolute());

		IPath srcSarl = sourceFolderPath.append(
				Path.fromPortableString(SARLConfig.FOLDER_SOURCE_SARL));
		IClasspathEntry srcSarlEntry = JavaCore.newSourceEntry(srcSarl.makeAbsolute());

		IPath srcGeneratedSources = sourceFolderPath.append(
				Path.fromPortableString(SARLConfig.FOLDER_SOURCE_GENERATED));
		IClasspathAttribute attr = JavaCore.newClasspathAttribute(
				IClasspathAttribute.IGNORE_OPTIONAL_PROBLEMS,
				Boolean.TRUE.toString());
		IClasspathEntry srcGeneratedSourcesEntry = JavaCore.newSourceEntry(
				srcGeneratedSources.makeAbsolute(),
				ClasspathEntry.INCLUDE_ALL,
				ClasspathEntry.EXCLUDE_NONE,
				null /*output location*/,
				new IClasspathAttribute[] {attr});

		IPath srcResources = sourceFolderPath.append(
				Path.fromPortableString(SARLConfig.FOLDER_RESOURCES));
		IClasspathEntry srcResourcesEntry = JavaCore.newSourceEntry(srcResources.makeAbsolute());

		return Arrays.asList(
				srcJavaEntry,
				srcSarlEntry,
				srcGeneratedSourcesEntry,
				srcResourcesEntry
				);
	}

	/**
	 * Returns the source class path entries to be added on new projects.
	 * The underlying resource may not exist.
	 *
	 * @return returns the default class path entries
	 */
	public IPath getOutputLocation() {
		IPath outputLocationPath = new Path(getProjectName()).makeAbsolute();

		outputLocationPath = outputLocationPath.append(
				Path.fromPortableString(SARLConfig.FOLDER_BIN));

		return outputLocationPath;
	}

	/**
	 * Returns the working sets to which the new project should be added.
	 *
	 * @return the selected working sets to which the new project should be added
	 */
	public IWorkingSet[] getWorkingSets() {
		return this.fWorkingSetGroup.getSelectedWorkingSets();
	}

	/**
	 * Sets the working sets to which the new project should be added.
	 *
	 * @param workingSets the initial selected working sets
	 */
	public void setWorkingSets(IWorkingSet[] workingSets) {
		if (workingSets == null) {
			throw new IllegalArgumentException();
		}
		this.fWorkingSetGroup.setWorkingSets(workingSets);
	}

	@Override
	public void setVisible(boolean visible) {
		super.setVisible(visible);
		if (visible) {
			this.fNameGroup.postSetFocus();
		}
	}

	private GridLayout initGridLayout(GridLayout layout, boolean margins) {
		layout.horizontalSpacing = convertHorizontalDLUsToPixels(IDialogConstants.HORIZONTAL_SPACING);
		layout.verticalSpacing = convertVerticalDLUsToPixels(IDialogConstants.VERTICAL_SPACING);
		if (margins) {
			layout.marginWidth = convertHorizontalDLUsToPixels(IDialogConstants.HORIZONTAL_MARGIN);
			layout.marginHeight = convertVerticalDLUsToPixels(IDialogConstants.VERTICAL_MARGIN);
		} else {
			layout.marginWidth = 0;
			layout.marginHeight = 0;
		}
		return layout;
	}

	private static IWorkingSet[] getSelectedWorkingSet(IStructuredSelection selection, IWorkbenchPart activePart) {
		IWorkingSet[] selected = getSelectedWorkingSet(selection);
		assert (selected != null);
		if (selected.length > 0) {
			for (int i = 0; i < selected.length; ++i) {
				if (!isValidWorkingSet(selected[i])) {
					return EMPTY_WORKING_SET_ARRAY;
				}
			}
			return selected;
		}

		selected = EMPTY_WORKING_SET_ARRAY;

		if (activePart instanceof PackageExplorerPart) {
			PackageExplorerPart explorerPart = (PackageExplorerPart) activePart;
			if (explorerPart.getRootMode() == PackageExplorerPart.PROJECTS_AS_ROOTS) {
				// Get active filter
				IWorkingSet filterWorkingSet = explorerPart.getFilterWorkingSet();
				if (filterWorkingSet != null && isValidWorkingSet(filterWorkingSet)) {
					selected = new IWorkingSet[] {filterWorkingSet};
				}
			} else {
				// If we have been gone into a working set return the working set
				Object input = explorerPart.getViewPartInput();
				if (input instanceof IWorkingSet
						&& isValidWorkingSet((IWorkingSet) input)) {
					selected = new IWorkingSet[] {(IWorkingSet) input};
				}
			}
		}

		return selected;
	}

	private static IWorkingSet[] getSelectedWorkingSet(IStructuredSelection selection) {
		IWorkingSet[] workingSet = EMPTY_WORKING_SET_ARRAY;

		if (selection instanceof ITreeSelection) {
			ITreeSelection treeSelection = (ITreeSelection) selection;
			if (!treeSelection.isEmpty()) {
				workingSet = getSelectedWorkingSet(treeSelection);
			}
		}
		return workingSet;
	}

	private static IWorkingSet[] getSelectedWorkingSet(ITreeSelection treeSelection) {
		assert (!treeSelection.isEmpty());
		List<?> elements = treeSelection.toList();
		if (elements.size() == 1) {
			Object element = elements.get(0);
			TreePath[] paths = treeSelection.getPathsFor(element);
			if (paths.length == 1
					&& paths[0].getSegmentCount() != 0) {
				Object candidate = paths[0].getSegment(0);
				if (candidate instanceof IWorkingSet
						&& isValidWorkingSet((IWorkingSet) candidate)) {
					return new IWorkingSet[] {(IWorkingSet) candidate};
				}
			}
		} else {
			ArrayList<IWorkingSet> result = new ArrayList<>();
			for (Iterator<?> iterator = elements.iterator(); iterator.hasNext();) {
				Object element = iterator.next();
				if (element instanceof IWorkingSet && isValidWorkingSet((IWorkingSet) element)) {
					result.add((IWorkingSet) element);
				}
			}
			return result.toArray(new IWorkingSet[result.size()]);
		}
		return EMPTY_WORKING_SET_ARRAY;
	}

	private static boolean isValidWorkingSet(IWorkingSet workingSet) {
		String id = workingSet.getId();
		if (!IWorkingSetIDs.JAVA.equals(id) && !IWorkingSetIDs.RESOURCE.equals(id)) {
			return false;
		}

		if (workingSet.isAggregateWorkingSet()) {
			return false;
		}

		return true;
	}

	/**
	 * Request a project name. Fires an event whenever the text field is changed, regardless of its content.
	 */
	private final class NameGroup extends Observable implements IDialogFieldListener {

		protected final StringDialogField fNameField;

		public NameGroup() {
			// text field for project name
			this.fNameField = new StringDialogField();
			this.fNameField.setLabelText(NewWizardMessages.NewJavaProjectWizardPageOne_NameGroup_label_text);
			this.fNameField.setDialogFieldListener(this);
		}

		public Control createControl(Composite composite) {
			Composite nameComposite = new Composite(composite, SWT.NONE);
			nameComposite.setFont(composite.getFont());
			nameComposite.setLayout(new GridLayout(2, false));

			this.fNameField.doFillIntoGrid(nameComposite, 2);
			LayoutUtil.setHorizontalGrabbing(this.fNameField.getTextControl(null));

			return nameComposite;
		}

		protected void fireEvent() {
			setChanged();
			notifyObservers();
		}

		public String getName() {
			return this.fNameField.getText().trim();
		}

		public void postSetFocus() {
			this.fNameField.postSetFocusOnDialogField(getShell().getDisplay());
		}

		public void setName(String name) {
			this.fNameField.setText(name);
		}

		@Override
		public void dialogFieldChanged(DialogField field) {
			fireEvent();
		}
	}

	/**
	 * Request a location. Fires an event whenever the checkbox or the location
	 * field is changed, regardless of whether the change originates from the user
	 * or has been invoked programmatically.
	 */
	private final class LocationGroup extends Observable implements Observer, IStringButtonAdapter, IDialogFieldListener {

		private static final String DIALOGSTORE_LAST_EXTERNAL_LOC = JavaUI.ID_PLUGIN + ".last.external.project"; //$NON-NLS-1$

		protected final SelectionButtonDialogField fUseDefaults;
		protected final StringButtonDialogField fLocation;

		private String fPreviousExternalLocation;

		public LocationGroup() {
			this.fUseDefaults = new SelectionButtonDialogField(SWT.CHECK);
			this.fUseDefaults.setDialogFieldListener(this);
			this.fUseDefaults.setLabelText(NewWizardMessages.NewJavaProjectWizardPageOne_LocationGroup_location_desc);

			this.fLocation = new StringButtonDialogField(this);
			this.fLocation.setDialogFieldListener(this);
			this.fLocation.setLabelText(NewWizardMessages.NewJavaProjectWizardPageOne_LocationGroup_locationLabel_desc);
			this.fLocation.setButtonLabel(NewWizardMessages.NewJavaProjectWizardPageOne_LocationGroup_browseButton_desc);

			this.fUseDefaults.setSelection(true);

			this.fPreviousExternalLocation = ""; //$NON-NLS-1$
		}

		public Control createControl(Composite composite) {
			final int numColumns = 4;

			final Composite locationComposite = new Composite(composite, SWT.NONE);
			locationComposite.setLayout(new GridLayout(numColumns, false));

			this.fUseDefaults.doFillIntoGrid(locationComposite, numColumns);
			this.fLocation.doFillIntoGrid(locationComposite, numColumns);
			LayoutUtil.setHorizontalGrabbing(this.fLocation.getTextControl(null));
			BidiUtils.applyBidiProcessing(this.fLocation.getTextControl(null), StructuredTextTypeHandlerFactory.FILE);

			return locationComposite;
		}

		protected void fireEvent() {
			setChanged();
			notifyObservers();
		}

		protected String getDefaultPath(String name) {
			final IPath path = Platform.getLocation().append(name);
			return path.toOSString();
		}

		@SuppressWarnings("synthetic-access")
		@Override
		public void update(Observable o, Object arg) {
			if (isUseDefaultSelected()) {
				this.fLocation.setText(getDefaultPath(MainProjectWizardPage.this.fNameGroup.getName()));
			}
			fireEvent();
		}

		public IPath getLocation() {
			if (isUseDefaultSelected()) {
				return Platform.getLocation();
			}
			return Path.fromOSString(this.fLocation.getText().trim());
		}

		public boolean isUseDefaultSelected() {
			return this.fUseDefaults.isSelected();
		}

		@SuppressWarnings("synthetic-access")
		public void setLocation(IPath path) {
			this.fUseDefaults.setSelection(path == null);
			if (path != null) {
				this.fLocation.setText(path.toOSString());
			} else {
				this.fLocation.setText(getDefaultPath(MainProjectWizardPage.this.fNameGroup.getName()));
			}
			fireEvent();
		}

		@SuppressWarnings("synthetic-access")
		@Override
		public void changeControlPressed(DialogField field) {
			final DirectoryDialog dialog = new DirectoryDialog(getShell());
			dialog.setMessage(NewWizardMessages.NewJavaProjectWizardPageOne_directory_message);
			String directoryName = this.fLocation.getText().trim();
			if (directoryName.length() == 0) {
				String prevLocation = JavaPlugin.getDefault().getDialogSettings().get(DIALOGSTORE_LAST_EXTERNAL_LOC);
				if (prevLocation != null) {
					directoryName = prevLocation;
				}
			}

			if (directoryName.length() > 0) {
				final File path = new File(directoryName);
				if (path.exists()) {
					dialog.setFilterPath(directoryName);
				}
			}
			final String selectedDirectory = dialog.open();
			if (selectedDirectory != null) {
				String oldDirectory = new Path(this.fLocation.getText().trim()).lastSegment();
				this.fLocation.setText(selectedDirectory);
				String lastSegment = new Path(selectedDirectory).lastSegment();
				if (lastSegment != null
						&& (MainProjectWizardPage.this.fNameGroup.getName().length() == 0
						|| MainProjectWizardPage.this.fNameGroup.getName().equals(oldDirectory))) {
					MainProjectWizardPage.this.fNameGroup.setName(lastSegment);
				}
				JavaPlugin.getDefault().getDialogSettings().put(DIALOGSTORE_LAST_EXTERNAL_LOC, selectedDirectory);
			}
		}

		@SuppressWarnings("synthetic-access")
		@Override
		public void dialogFieldChanged(DialogField field) {
			if (field == this.fUseDefaults) {
				final boolean checked = this.fUseDefaults.isSelected();
				if (checked) {
					this.fPreviousExternalLocation = this.fLocation.getText();
					this.fLocation.setText(getDefaultPath(MainProjectWizardPage.this.fNameGroup.getName()));
					this.fLocation.setEnabled(false);
				} else {
					this.fLocation.setText(this.fPreviousExternalLocation);
					this.fLocation.setEnabled(true);
				}
			}
			fireEvent();
		}
	}

	/**
	 * @author $Author: ngaud$
	 * @version $FullVersion$
	 * @mavengroupid $GroupId$
	 * @mavenartifactid $ArtifactId$
	 */
	private final class JREGroup implements Observer, SelectionListener, IDialogFieldListener {

		private static final String LAST_SELECTED_EE_SETTINGS_KEY =
				JavaUI.ID_PLUGIN + ".last.selected.execution.enviroment"; //$NON-NLS-1$
		private static final String LAST_SELECTED_JRE_SETTINGS_KEY =
				JavaUI.ID_PLUGIN + ".last.selected.project.jre"; //$NON-NLS-1$
		private static final String LAST_SELECTED_JRE_KIND2 =
				JavaUI.ID_PLUGIN + ".last.selected.jre.kind2"; //$NON-NLS-1$

		private static final int DEFAULT_JRE = 0;
		private static final int PROJECT_JRE = 1;
		private static final int EE_JRE = 2;

		private final SelectionButtonDialogField fUseDefaultJRE;
		private final SelectionButtonDialogField fUseProjectJRE;
		private final SelectionButtonDialogField fUseEEJRE;
		private final ComboDialogField fJRECombo;
		private final ComboDialogField fEECombo;
		private Group fGroup;
		private Link fPreferenceLink;
		private IVMInstall[] fInstalledJVMs;
		private String[] fJRECompliance;
		private IExecutionEnvironment[] fInstalledEEs;
		private String[] fEECompliance;

		public JREGroup() {
			this.fUseDefaultJRE = new SelectionButtonDialogField(SWT.RADIO);
			this.fUseDefaultJRE.setLabelText(getDefaultJVMLabel());

			this.fUseProjectJRE = new SelectionButtonDialogField(SWT.RADIO);
			this.fUseProjectJRE.setLabelText(NewWizardMessages.NewJavaProjectWizardPageOne_JREGroup_specific_compliance);

			this.fJRECombo = new ComboDialogField(SWT.READ_ONLY);
			fillInstalledJREs(this.fJRECombo);
			this.fJRECombo.setDialogFieldListener(this);

			this.fUseEEJRE = new SelectionButtonDialogField(SWT.RADIO);
			this.fUseEEJRE.setLabelText(NewWizardMessages.NewJavaProjectWizardPageOne_JREGroup_specific_EE);

			this.fEECombo = new ComboDialogField(SWT.READ_ONLY);
			fillExecutionEnvironments(this.fEECombo);
			this.fEECombo.setDialogFieldListener(this);

			switch (getLastSelectedJREKind()) {
			case DEFAULT_JRE:
				this.fUseDefaultJRE.setSelection(true);
				break;
			case PROJECT_JRE:
				this.fUseProjectJRE.setSelection(true);
				break;
			case EE_JRE:
				this.fUseEEJRE.setSelection(true);
				break;
			default:
			}

			this.fJRECombo.setEnabled(this.fUseProjectJRE.isSelected());
			this.fEECombo.setEnabled(this.fUseEEJRE.isSelected());

			this.fUseDefaultJRE.setDialogFieldListener(this);
			this.fUseProjectJRE.setDialogFieldListener(this);
			this.fUseEEJRE.setDialogFieldListener(this);
		}

		@SuppressWarnings("synthetic-access")
		public Control createControl(Composite composite) {
			this.fGroup = new Group(composite, SWT.NONE);
			this.fGroup.setFont(composite.getFont());
			this.fGroup.setLayout(initGridLayout(new GridLayout(2, false), true));
			this.fGroup.setText(NewWizardMessages.NewJavaProjectWizardPageOne_JREGroup_title);

			this.fUseEEJRE.doFillIntoGrid(this.fGroup, 1);
			Combo eeComboControl = this.fEECombo.getComboControl(this.fGroup);
			eeComboControl.setLayoutData(new GridData(GridData.FILL, GridData.CENTER, true, false));

			this.fUseProjectJRE.doFillIntoGrid(this.fGroup, 1);
			Combo comboControl = this.fJRECombo.getComboControl(this.fGroup);
			comboControl.setLayoutData(new GridData(GridData.FILL, GridData.CENTER, true, false));

			this.fUseDefaultJRE.doFillIntoGrid(this.fGroup, 1);

			this.fPreferenceLink = new Link(this.fGroup, SWT.NONE);
			this.fPreferenceLink.setFont(this.fGroup.getFont());
			this.fPreferenceLink.setText(NewWizardMessages.NewJavaProjectWizardPageOne_JREGroup_link_description);
			this.fPreferenceLink.setLayoutData(new GridData(GridData.END, GridData.CENTER, false, false));
			this.fPreferenceLink.addSelectionListener(this);

			updateEnableState();
			return this.fGroup;
		}

		private void fillInstalledJREs(ComboDialogField comboField) {
			String selectedItem = getLastSelectedJRE();
			int selectionIndex = -1;
			if (this.fUseProjectJRE.isSelected()) {
				selectionIndex = comboField.getSelectionIndex();
				if (selectionIndex != -1) {
					// paranoia
					selectedItem = comboField.getItems()[selectionIndex];
				}
			}

			this.fInstalledJVMs = getWorkspaceJREs();
			Arrays.sort(this.fInstalledJVMs, new Comparator<IVMInstall>() {

				@Override
				public int compare(IVMInstall i0, IVMInstall i1) {
					if (i1 instanceof IVMInstall2 && i0 instanceof IVMInstall2) {
						String cc0 = JavaModelUtil.getCompilerCompliance((IVMInstall2) i0, JavaCore.VERSION_1_4);
						String cc1 = JavaModelUtil.getCompilerCompliance((IVMInstall2) i1, JavaCore.VERSION_1_4);
						int result = cc1.compareTo(cc0);
						if (result != 0) {
							return result;
						}
					}
					return Policy.getComparator().compare(i0.getName(), i1.getName());
				}

			});
			// find new index
			selectionIndex = -1;
			String[] jreLabels = new String[this.fInstalledJVMs.length];
			this.fJRECompliance = new String[this.fInstalledJVMs.length];
			for (int i = 0; i < this.fInstalledJVMs.length; i++) {
				jreLabels[i] = this.fInstalledJVMs[i].getName();
				if (selectedItem != null && jreLabels[i].equals(selectedItem)) {
					selectionIndex = i;
				}
				if (this.fInstalledJVMs[i] instanceof IVMInstall2) {
					this.fJRECompliance[i] = JavaModelUtil.getCompilerCompliance(
							(IVMInstall2) this.fInstalledJVMs[i],
							JavaCore.VERSION_1_4);
				} else {
					this.fJRECompliance[i] = JavaCore.VERSION_1_4;
				}
			}
			comboField.setItems(jreLabels);
			if (selectionIndex == -1) {
				comboField.selectItem(getDefaultJVMName());
			} else {
				comboField.selectItem(selectedItem);
			}
		}

		private void fillExecutionEnvironments(ComboDialogField comboField) {
			String selectedItem = getLastSelectedEE();
			int selectionIndex = -1;
			if (this.fUseEEJRE.isSelected()) {
				selectionIndex = comboField.getSelectionIndex();
				if (selectionIndex != -1) {
					// paranoia
					selectedItem = comboField.getItems()[selectionIndex];
				}
			}

			this.fInstalledEEs = JavaRuntime.getExecutionEnvironmentsManager().getExecutionEnvironments();
			Arrays.sort(this.fInstalledEEs, new Comparator<IExecutionEnvironment>() {
				@Override
				public int compare(IExecutionEnvironment arg0, IExecutionEnvironment arg1) {
					return Policy.getComparator().compare(arg0.getId(), arg1.getId());
				}
			});
			// find new index
			selectionIndex = -1;
			String[] eeLabels = new String[this.fInstalledEEs.length];
			this.fEECompliance = new String[this.fInstalledEEs.length];
			for (int i = 0; i < this.fInstalledEEs.length; i++) {
				eeLabels[i] = this.fInstalledEEs[i].getId();
				if (selectedItem != null && eeLabels[i].equals(selectedItem)) {
					selectionIndex = i;
				}
				this.fEECompliance[i] = JavaModelUtil.getExecutionEnvironmentCompliance(this.fInstalledEEs[i]);
			}
			comboField.setItems(eeLabels);
			if (selectionIndex == -1) {
				comboField.selectItem(getDefaultEEName());
			} else {
				comboField.selectItem(selectedItem);
			}
		}

		private IVMInstall[] getWorkspaceJREs() {
			List<VMStandin> standins = new ArrayList<>();
			IVMInstallType[] types = JavaRuntime.getVMInstallTypes();
			for (int i = 0; i < types.length; i++) {
				IVMInstallType type = types[i];
				IVMInstall[] installs = type.getVMInstalls();
				for (int j = 0; j < installs.length; j++) {
					IVMInstall install = installs[j];
					standins.add(new VMStandin(install));
				}
			}
			return standins.toArray(new IVMInstall[standins.size()]);
		}

		private String getDefaultJVMName() {
			IVMInstall install = JavaRuntime.getDefaultVMInstall();
			if (install != null) {
				return install.getName();
			}
			return NewWizardMessages.NewJavaProjectWizardPageOne_UnknownDefaultJRE_name;
		}

		private String getDefaultEEName() {
			IVMInstall defaultVM = JavaRuntime.getDefaultVMInstall();

			IExecutionEnvironment[] environments = JavaRuntime.getExecutionEnvironmentsManager().getExecutionEnvironments();
			if (defaultVM != null) {
				for (int i = 0; i < environments.length; i++) {
					IVMInstall eeDefaultVM = environments[i].getDefaultVM();
					if (eeDefaultVM != null && defaultVM.getId().equals(eeDefaultVM.getId())) {
						return environments[i].getId();
					}
				}
			}

			String defaultCC;
			if (defaultVM instanceof IVMInstall2) {
				defaultCC = JavaModelUtil.getCompilerCompliance((IVMInstall2) defaultVM, JavaCore.VERSION_1_4);
			} else {
				defaultCC = JavaCore.VERSION_1_4;
			}

			for (int i = 0; i < environments.length; i++) {
				String eeCompliance = JavaModelUtil.getExecutionEnvironmentCompliance(environments[i]);
				if (defaultCC.endsWith(eeCompliance)) {
					return environments[i].getId();
				}
			}

			return "JavaSE-1.6"; //$NON-NLS-1$
		}

		private String getDefaultJVMLabel() {
			return MessageFormat.format(
					NewWizardMessages.NewJavaProjectWizardPageOne_JREGroup_default_compliance,
					getDefaultJVMName());
		}

		@Override
		public void update(Observable o, Object arg) {
			updateEnableState();
		}

		@SuppressWarnings("synthetic-access")
		private void updateEnableState() {
			final boolean detect = MainProjectWizardPage.this.fDetectGroup.mustDetect();
			this.fUseDefaultJRE.setEnabled(!detect);
			this.fUseProjectJRE.setEnabled(!detect);
			this.fUseEEJRE.setEnabled(!detect);
			this.fJRECombo.setEnabled(!detect && this.fUseProjectJRE.isSelected());
			this.fEECombo.setEnabled(!detect && this.fUseEEJRE.isSelected());
			if (this.fPreferenceLink != null) {
				this.fPreferenceLink.setEnabled(!detect);
			}
			if (this.fGroup != null) {
				this.fGroup.setEnabled(!detect);
			}
		}

		@Override
		public void widgetSelected(SelectionEvent e) {
			widgetDefaultSelected(e);
		}

		@SuppressWarnings("synthetic-access")
		@Override
		public void widgetDefaultSelected(SelectionEvent e) {
			String jreID = BuildPathSupport.JRE_PREF_PAGE_ID;
			String eeID = BuildPathSupport.EE_PREF_PAGE_ID;
			String complianceId = CompliancePreferencePage.PREF_ID;
			Map<String, Boolean> data = new HashMap<>();
			data.put(PropertyAndPreferencePage.DATA_NO_LINK, Boolean.TRUE);
			PreferencesUtil.createPreferenceDialogOn(
					getShell(), jreID,
					new String[] {jreID, complianceId, eeID},
					data).open();

			handlePossibleJVMChange();
			MainProjectWizardPage.this.fDetectGroup.handlePossibleJVMChange();
		}

		public void handlePossibleJVMChange() {
			this.fUseDefaultJRE.setLabelText(getDefaultJVMLabel());
			fillInstalledJREs(this.fJRECombo);
			fillExecutionEnvironments(this.fEECombo);
		}

		@SuppressWarnings("synthetic-access")
		@Override
		public void dialogFieldChanged(DialogField field) {
			updateEnableState();
			MainProjectWizardPage.this.fDetectGroup.handlePossibleJVMChange();
			if (field == this.fJRECombo) {
				if (this.fUseProjectJRE.isSelected()) {
					storeSelectionValue(this.fJRECombo, LAST_SELECTED_JRE_SETTINGS_KEY);
				}
			} else if (field == this.fEECombo) {
				if (this.fUseEEJRE.isSelected()) {
					storeSelectionValue(this.fEECombo, LAST_SELECTED_EE_SETTINGS_KEY);
				}
			} else if (field == this.fUseDefaultJRE) {
				if (this.fUseDefaultJRE.isSelected()) {
					JavaPlugin.getDefault().getDialogSettings().put(LAST_SELECTED_JRE_KIND2, DEFAULT_JRE);
					this.fUseProjectJRE.setSelection(false);
					this.fUseEEJRE.setSelection(false);
				}
			} else if (field == this.fUseProjectJRE) {
				if (this.fUseProjectJRE.isSelected()) {
					JavaPlugin.getDefault().getDialogSettings().put(LAST_SELECTED_JRE_KIND2, PROJECT_JRE);
					this.fUseDefaultJRE.setSelection(false);
					this.fUseEEJRE.setSelection(false);
				}
			} else if (field == this.fUseEEJRE) {
				if (this.fUseEEJRE.isSelected()) {
					JavaPlugin.getDefault().getDialogSettings().put(LAST_SELECTED_JRE_KIND2, EE_JRE);
					this.fUseDefaultJRE.setSelection(false);
					this.fUseProjectJRE.setSelection(false);
				}
			}
		}

		private void storeSelectionValue(ComboDialogField combo, String preferenceKey) {
			int index = combo.getSelectionIndex();
			if (index == -1) {
				return;
			}

			String item = combo.getItems()[index];
			JavaPlugin.getDefault().getDialogSettings().put(preferenceKey, item);
		}

		private int getLastSelectedJREKind() {
			IDialogSettings settings = JavaPlugin.getDefault().getDialogSettings();
			if (settings.get(LAST_SELECTED_JRE_KIND2) == null) {
				return EE_JRE;
			}

			return settings.getInt(LAST_SELECTED_JRE_KIND2);
		}

		private String getLastSelectedEE() {
			IDialogSettings settings = JavaPlugin.getDefault().getDialogSettings();
			return settings.get(LAST_SELECTED_EE_SETTINGS_KEY);
		}

		private String getLastSelectedJRE() {
			IDialogSettings settings = JavaPlugin.getDefault().getDialogSettings();
			return settings.get(LAST_SELECTED_JRE_SETTINGS_KEY);
		}

		public IVMInstall getSelectedJVM() {
			if (this.fUseProjectJRE.isSelected()) {
				int index = this.fJRECombo.getSelectionIndex();
				if (index >= 0 && index < this.fInstalledJVMs.length) {
					// paranoia
					return this.fInstalledJVMs[index];
				}
			} /*else if (this.fUseEEJRE.isSelected()) {

			}*/
			return null;
		}

		public IPath getJREContainerPath() {
			if (this.fUseProjectJRE.isSelected()) {
				int index = this.fJRECombo.getSelectionIndex();
				if (index >= 0 && index < this.fInstalledJVMs.length) {
					// paranoia
					return JavaRuntime.newJREContainerPath(this.fInstalledJVMs[index]);
				}
			} else if (this.fUseEEJRE.isSelected()) {
				int index = this.fEECombo.getSelectionIndex();
				if (index >= 0 && index < this.fInstalledEEs.length) {
					// paranoia
					return JavaRuntime.newJREContainerPath(this.fInstalledEEs[index]);
				}
			}
			return null;
		}

		public String getSelectedCompilerCompliance() {
			if (this.fUseProjectJRE.isSelected()) {
				int index = this.fJRECombo.getSelectionIndex();
				if (index >= 0 && index < this.fJRECompliance.length) {
					// paranoia
					return this.fJRECompliance[index];
				}
			} else if (this.fUseEEJRE.isSelected()) {
				int index = this.fEECombo.getSelectionIndex();
				if (index >= 0 && index < this.fEECompliance.length) {
					// paranoia
					return this.fEECompliance[index];
				}
			}
			return null;
		}
	}

	private final class WorkingSetGroup {

		private WorkingSetConfigurationBlock fWorkingSetBlock;

		public WorkingSetGroup() {
			String[] workingSetIds = new String[] {IWorkingSetIDs.JAVA, IWorkingSetIDs.RESOURCE};
			this.fWorkingSetBlock = new WorkingSetConfigurationBlock(workingSetIds, JavaPlugin.getDefault().getDialogSettings());
			// fWorkingSetBlock.setDialogMessage(NewWizardMessages.NewJavaProjectWizardPageOne_WorkingSetSelection_message);
		}

		public Control createControl(Composite composite) {
			Group workingSetGroup = new Group(composite, SWT.NONE);
			workingSetGroup.setFont(composite.getFont());
			workingSetGroup.setText(NewWizardMessages.NewJavaProjectWizardPageOne_WorkingSets_group);
			workingSetGroup.setLayout(new GridLayout(1, false));

			this.fWorkingSetBlock.createContent(workingSetGroup);

			return workingSetGroup;
		}

		public void setWorkingSets(IWorkingSet[] workingSets) {
			this.fWorkingSetBlock.setWorkingSets(workingSets);
		}

		public IWorkingSet[] getSelectedWorkingSets() {
			return this.fWorkingSetBlock.getSelectedWorkingSets();
		}
	}

	/**
	 * Show a warning when the project location contains files.
	 */
	private final class DetectGroup extends Observable implements Observer, SelectionListener {

		private Link fHintText;
		private Label fIcon;
		private boolean fDetect;

		public DetectGroup() {
			this.fDetect = false;
		}

		@SuppressWarnings("synthetic-access")
		public Control createControl(Composite parent) {

			Composite composite = new Composite(parent, SWT.NONE);
			composite.setLayoutData(new GridData(SWT.FILL, SWT.TOP, true, false));
			GridLayout layout = new GridLayout(2, false);
			layout.horizontalSpacing = 10;
			composite.setLayout(layout);

			this.fIcon = new Label(composite, SWT.LEFT);
			this.fIcon.setImage(Dialog.getImage(Dialog.DLG_IMG_MESSAGE_WARNING));
			GridData gridData = new GridData(SWT.LEFT, SWT.TOP, false, false);
			this.fIcon.setLayoutData(gridData);

			this.fHintText = new Link(composite, SWT.WRAP);
			this.fHintText.setFont(composite.getFont());
			this.fHintText.addSelectionListener(this);
			gridData = new GridData(GridData.FILL, SWT.FILL, true, true);
			gridData.widthHint = convertWidthInCharsToPixels(50);
			gridData.heightHint = convertHeightInCharsToPixels(3);
			this.fHintText.setLayoutData(gridData);

			handlePossibleJVMChange();
			return composite;
		}

		@SuppressWarnings("synthetic-access")
		public void handlePossibleJVMChange() {

			if (JavaRuntime.getDefaultVMInstall() == null) {
				this.fHintText.setText(NewWizardMessages.NewJavaProjectWizardPageOne_NoJREFound_link);
				this.fHintText.setVisible(true);
				this.fIcon.setImage(Dialog.getImage(Dialog.DLG_IMG_MESSAGE_WARNING));
				this.fIcon.setVisible(true);
				return;
			}

			String selectedCompliance = MainProjectWizardPage.this.fJREGroup.getSelectedCompilerCompliance();
			if (selectedCompliance != null) {
				String defaultCompliance = JavaCore.getOption(JavaCore.COMPILER_COMPLIANCE);
				if (selectedCompliance.equals(defaultCompliance)) {
					this.fHintText.setVisible(false);
					this.fIcon.setVisible(false);
				} else {
					this.fHintText.setText(MessageFormat.format(
							NewWizardMessages.NewJavaProjectWizardPageOne_DetectGroup_differendWorkspaceCC_message,
							BasicElementLabels.getVersionName(defaultCompliance),
							BasicElementLabels.getVersionName(selectedCompliance)));
					this.fHintText.setVisible(true);
					this.fIcon.setImage(Dialog.getImage(Dialog.DLG_IMG_MESSAGE_INFO));
					this.fIcon.setVisible(true);
				}
				return;
			}

			selectedCompliance = JavaCore.getOption(JavaCore.COMPILER_COMPLIANCE);
			IVMInstall selectedJVM = MainProjectWizardPage.this.fJREGroup.getSelectedJVM();
			if (selectedJVM == null) {
				selectedJVM = JavaRuntime.getDefaultVMInstall();
			}
			String jvmCompliance = JavaCore.VERSION_1_4;
			if (selectedJVM instanceof IVMInstall2) {
				jvmCompliance = JavaModelUtil.getCompilerCompliance((IVMInstall2) selectedJVM, JavaCore.VERSION_1_4);
			}
			if (!selectedCompliance.equals(jvmCompliance)
					&& (JavaModelUtil.is50OrHigher(selectedCompliance)
							|| JavaModelUtil.is50OrHigher(jvmCompliance))) {
				this.fHintText.setText(MessageFormat.format(
						NewWizardMessages.NewJavaProjectWizardPageOne_DetectGroup_jre_message,
						BasicElementLabels.getVersionName(selectedCompliance),
						BasicElementLabels.getVersionName(jvmCompliance)));
				this.fHintText.setVisible(true);
				this.fIcon.setImage(Dialog.getImage(Dialog.DLG_IMG_MESSAGE_WARNING));
				this.fIcon.setVisible(true);
			} else {
				this.fHintText.setVisible(false);
				this.fIcon.setVisible(false);
			}

		}

		@SuppressWarnings("synthetic-access")
		private boolean computeDetectState() {
			if (MainProjectWizardPage.this.fLocationGroup.isUseDefaultSelected()) {
				String name = MainProjectWizardPage.this.fNameGroup.getName();
				if (name.length() == 0 || JavaPlugin.getWorkspace().getRoot().findMember(name) != null) {
					return false;
				}

				File directory = MainProjectWizardPage.this.fLocationGroup.getLocation().append(name).toFile();
				return directory.isDirectory();
			}

			File directory = MainProjectWizardPage.this.fLocationGroup.getLocation().toFile();
			return directory.isDirectory();
		}

		@Override
		public void update(Observable o, Object arg) {
			if (o instanceof LocationGroup) {
				boolean oldDetectState = this.fDetect;
				this.fDetect = computeDetectState();

				if (oldDetectState != this.fDetect) {
					setChanged();
					notifyObservers();

					if (this.fDetect) {
						this.fHintText.setVisible(true);
						this.fHintText.setText(NewWizardMessages.NewJavaProjectWizardPageOne_DetectGroup_message);
						this.fIcon.setImage(Dialog.getImage(Dialog.DLG_IMG_MESSAGE_INFO));
						this.fIcon.setVisible(true);
					} else {
						handlePossibleJVMChange();
					}
				}
			}
		}

		public boolean mustDetect() {
			return this.fDetect;
		}

		@Override
		public void widgetSelected(SelectionEvent e) {
			widgetDefaultSelected(e);
		}

		@SuppressWarnings("synthetic-access")
		@Override
		public void widgetDefaultSelected(SelectionEvent e) {
			String jreID = BuildPathSupport.JRE_PREF_PAGE_ID;
			String eeID = BuildPathSupport.EE_PREF_PAGE_ID;
			String complianceId = CompliancePreferencePage.PREF_ID;
			Map<String, Boolean> data = new HashMap<>();
			data.put(PropertyAndPreferencePage.DATA_NO_LINK, Boolean.TRUE);
			String id = "JRE".equals(e.text) ? jreID : complianceId; //$NON-NLS-1$
			PreferencesUtil.createPreferenceDialogOn(getShell(), id,
					new String[] {jreID, complianceId, eeID},
					data).open();

			MainProjectWizardPage.this.fJREGroup.handlePossibleJVMChange();
			handlePossibleJVMChange();
		}
	}

	/**
	 * Validate this page and show appropriate warnings and error NewWizardMessages.
	 */
	private final class Validator implements Observer {

		@SuppressWarnings("synthetic-access")
		@Override
		public void update(Observable o, Object arg) {
			try {
				IWorkspace workspace = JavaPlugin.getWorkspace();
				String name = MainProjectWizardPage.this.fNameGroup.getName();
				checkProjectName(workspace, name);
				IProject handle = checkProjectExist(workspace, name);
				String location = MainProjectWizardPage.this.fLocationGroup.getLocation().toOSString();
				IPath projectPath = checkLocationSyntax(location);
				validateLocation(workspace, handle, projectPath);

				setErrorMessage(null);
				setMessage(null);
				setPageComplete(true);

			} catch (ValidationException e) {
				setMessage(e.getMessage());
				setErrorMessage(e.getErrorMessage());
				setPageComplete(false);
			}
		}

		/** Check the project name.
		 *
		 * @param workspace - the workspace.
		 * @param name - name of the project.
		 * @throws ValidationException if the name is invalid.
		 */
		private void checkProjectName(IWorkspace workspace, String name)  throws ValidationException {
			// check whether the project name field is empty
			if (name.length() == 0) {
				throw new ValidationException(
						NewWizardMessages.NewJavaProjectWizardPageOne_Message_enterProjectName,
						null);
			}

			// check whether the project name is valid
			IStatus nameStatus = workspace.validateName(name, IResource.PROJECT);
			if (!nameStatus.isOK()) {
				throw new ValidationException(
						null,
						nameStatus.getMessage());
			}
		}

		/** Check whether project already exists.
		 *
		 * @param workspace - the workspace.
		 * @param name - name of the project.
		 * @return the project.
		 * @throws ValidationException if the project exists.
		 */
		@SuppressWarnings("synthetic-access")
		private IProject checkProjectExist(IWorkspace workspace, String name) throws ValidationException {
			IProject handle = workspace.getRoot().getProject(name);
			if (handle.exists()) {
				throw new ValidationException(
						null,
						NewWizardMessages.NewJavaProjectWizardPageOne_Message_projectAlreadyExists);
			}

			IPath projectLocation = ResourcesPlugin.getWorkspace().getRoot().getLocation().append(name);
			if (projectLocation.toFile().exists()) {
				try {
					// correct casing
					String canonicalPath = projectLocation.toFile().getCanonicalPath();
					projectLocation = new Path(canonicalPath);
				} catch (IOException e) {
					SARLEclipsePlugin.getDefault().log(e);
				}

				String existingName = projectLocation.lastSegment();
				if (!existingName.equals(MainProjectWizardPage.this.fNameGroup.getName())) {
					throw new ValidationException(
							null,
							MessageFormat.format(
									NewWizardMessages.NewJavaProjectWizardPageOne_Message_invalidProjectNameForWorkspaceRoot,
									BasicElementLabels.getResourceName(existingName)));
				}
			}
			return handle;
		}

		/**
		 * Check whether the location is a syntactically correct path.
		 *
		 * @param location - the location to test.
		 * @return the project path.
		 * @throws ValidationException if the location has invalid syntax.
		 */
		@SuppressWarnings("synthetic-access")
		private IPath checkLocationSyntax(String location) throws ValidationException {
			// check whether location is empty
			if (location.length() == 0) {
				throw new ValidationException(
						NewWizardMessages.NewJavaProjectWizardPageOne_Message_enterLocation,
						null);
			}

			if (!Path.EMPTY.isValidPath(location)) {
				throw new ValidationException(
						null,
						NewWizardMessages.NewJavaProjectWizardPageOne_Message_invalidDirectory);
			}

			IPath projectPath = null;
			if (!MainProjectWizardPage.this.fLocationGroup.isUseDefaultSelected()) {
				projectPath = Path.fromOSString(location);
				if (!projectPath.toFile().exists()) {
					// check non-existing external location
					if (!canCreate(projectPath.toFile())) {
						throw new ValidationException(
								null,
								NewWizardMessages.NewJavaProjectWizardPageOne_Message_cannotCreateAtExternalLocation);
					}
				}
			}
			return projectPath;
		}

		/** Validate the project location.
		 *
		 * @param workspace - the workspace.
		 * @param handle - the project.
		 * @param projectPath - the project path.
		 * @throws ValidationException if the location has invalid syntax.
		 */
		private void validateLocation(
				IWorkspace workspace,
				IProject handle, IPath projectPath)  throws ValidationException {
			IStatus locationStatus = workspace.validateProjectLocation(handle, projectPath);
			if (!locationStatus.isOK()) {
				throw new ValidationException(
						null,
						locationStatus.getMessage());
			}
		}

		private boolean canCreate(File file) {
			File f = file;
			while (!f.exists()) {
				f = f.getParentFile();
				if (f == null) {
					return false;
				}
			}
			return f.canWrite();
		}

		/**
		 * @author $Author: sgalland$
		 * @version $FullVersion$
		 * @mavengroupid $GroupId$
		 * @mavenartifactid $ArtifactId$
		 */
		private class ValidationException extends Exception {

			private static final long serialVersionUID = 2953587040979817037L;

			private final String errorMessage;

			public ValidationException(String message, String errorMessage) {
				super(message);
				this.errorMessage = errorMessage;
			}

			public String getErrorMessage() {
				return this.errorMessage;
			}

		}
	}

}
