/*
 * $Id$
 *
 * SARL is an general-purpose agent programming language.
 * More details on http://www.sarl.io
 *
 * Copyright (C) 2014-2025 SARL.io, the Original Authors and Main Authors.
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

import java.io.File;
import java.io.IOException;
import java.net.URI;
import java.text.MessageFormat;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.Comparator;
import java.util.HashMap;
import java.util.Map;
import java.util.Observable;
import java.util.Observer;
import java.util.TreeMap;
import java.util.stream.Stream;

import org.eclipse.core.filesystem.URIUtil;
import org.eclipse.core.resources.IProject;
import org.eclipse.core.resources.IResource;
import org.eclipse.core.resources.IWorkspace;
import org.eclipse.core.resources.ResourcesPlugin;
import org.eclipse.core.runtime.IPath;
import org.eclipse.core.runtime.Path;
import org.eclipse.core.runtime.Platform;
import org.eclipse.equinox.bidi.StructuredTextTypeHandlerFactory;
import org.eclipse.jdt.core.IAccessRule;
import org.eclipse.jdt.core.IClasspathAttribute;
import org.eclipse.jdt.core.IClasspathEntry;
import org.eclipse.jdt.core.JavaCore;
import org.eclipse.jdt.internal.corext.util.JavaModelUtil;
import org.eclipse.jdt.internal.ui.IJavaHelpContextIds;
import org.eclipse.jdt.internal.ui.JavaPlugin;
import org.eclipse.jdt.internal.ui.packageview.PackageExplorerPart;
import org.eclipse.jdt.internal.ui.preferences.CompliancePreferencePage;
import org.eclipse.jdt.internal.ui.preferences.PropertyAndPreferencePage;
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
import org.eclipse.jface.wizard.WizardPage;
import org.eclipse.osgi.util.TextProcessor;
import org.eclipse.swt.SWT;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.events.SelectionListener;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
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

import io.sarl.apputils.eclipseextensions.projectconfig.ProjectConfigurationFragment;
import io.sarl.apputils.eclipseextensions.projectconfig.ProjectConfigurationFragments;
import io.sarl.apputils.eclipseextensions.sreprovider.ISREInstall;
import io.sarl.apputils.uiextensions.classpath.SarlDefaultClassPathProvider;
import io.sarl.eclipse.SARLEclipseConfig;
import io.sarl.eclipse.SARLEclipsePlugin;
import io.sarl.eclipse.buildpath.SARLClasspathContainerInitializer;
import io.sarl.eclipse.natures.SARLProjectConfigurator;
import io.sarl.eclipse.runtime.SREConfigurationBlock;
import io.sarl.lang.SARLConfig;
import io.sarl.lang.core.SARLVersion;

/**
 * The first page of the SARL new project wizard. Most part of the code of this class
 * is copy/paste from {@link NewJavaProjectWizardPageOne}
 *
 * <p>This version removes the choice of the project structure and update the structure of
 * the source folder of the project with
 * {@link SARLProjectConfigurator#getDefaultSourceClassPathEntries(IPath)}
 *
 * @author <a href="http://www.ciad-lab.fr/nicolas_gaud">Nicolas Gaud</a>
 * @author <a href="http://www.ciad-lab.fr/stephane_galland">St&eacute;phane Galland</a>
 * @version io.sarl.eclipse 0.15.0 20250909-115751
 * @mavengroupid io.sarl.eclipse
 * @mavenartifactid io.sarl.eclipse
 */
@SuppressWarnings({"deprecation", "restriction"})
public class MainProjectWizardPage extends WizardPage implements SarlDefaultClassPathProvider {

	private static final IWorkingSet[] EMPTY_WORKING_SET_ARRAY = new IWorkingSet[0];

	private final NameGroup nameGroup;

	private final LocationGroup locationGroup;

	private final SREConfigurationBlock sreGroup;

	private final JREGroup jreGroup;

	private final DetectGroup detectGroup;

	private final Validator validator;

	private final WorkingSetGroup workingSetGroup;

	private final ProjectConfiguratorsGroup configuratorGroup;

	/**
	 * Creates a new {@link MainProjectWizardPage}.
	 */
	public MainProjectWizardPage() {
		super(Messages.NewSarlProjectWizard_0);
		setPageComplete(false);

		this.nameGroup = new NameGroup();
		this.locationGroup = new LocationGroup();
		this.sreGroup = new SREConfigurationBlock(Messages.MainProjectWizardPage_0, true, null, null);
		this.jreGroup = new JREGroup();
		this.workingSetGroup = new WorkingSetGroup();
		this.detectGroup = new DetectGroup();
		this.configuratorGroup = new ProjectConfiguratorsGroup();

		// establish connections
		this.nameGroup.addObserver(this.locationGroup);
		this.detectGroup.addObserver(this.jreGroup);
		this.locationGroup.addObserver(this.detectGroup);
		this.configuratorGroup.addObserver(this.locationGroup);

		// initialize all elements
		this.nameGroup.notifyObservers();

		// create and connect validator
		this.validator = new Validator();
		this.nameGroup.addObserver(this.validator);
		this.locationGroup.addObserver(this.validator);
		this.configuratorGroup.addObserver(this.validator);

		// initialize defaults
		setProjectName(""); //$NON-NLS-1$
		setProjectLocationURI(null);
		setWorkingSets(new IWorkingSet[0]);

		initializeDefaultVM();

		setTitle(Messages.NewSarlProjectWizard_0);
		setDescription(Messages.MainProjectWizardPage_1);
		setImageDescriptor(SARLEclipsePlugin.getDefault().getImageDescriptor(
				SARLEclipseConfig.NEW_PROJECT_WIZARD_DIALOG_IMAGE));
	}

	/**
	 * The wizard owning this page can call this method to initialize the fields from the current selection and active part.
	 *
	 * @param selection used to initialize the fields
	 * @param activePart the (typically active) part to initialize the fields or {@code null}
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

		final var composite = new Composite(parent, SWT.NULL);
		composite.setFont(parent.getFont());
		composite.setLayout(initGridLayout(new GridLayout(1, false), true));
		composite.setLayoutData(new GridData(GridData.HORIZONTAL_ALIGN_FILL));

		// create UI elements
		final var nameControl = createNameControl(composite);
		nameControl.setLayoutData(new GridData(GridData.FILL_HORIZONTAL));

		final var locationControl = createLocationControl(composite);
		locationControl.setLayoutData(new GridData(GridData.FILL_HORIZONTAL));

		final var sreControl = createSRESelectionControl(composite);
		sreControl.setLayoutData(new GridData(GridData.FILL_HORIZONTAL));

		final var jreControl = createJRESelectionControl(composite);
		jreControl.setLayoutData(new GridData(GridData.FILL_HORIZONTAL));

		final var workingSetControl = createWorkingSetControl(composite);
		workingSetControl.setLayoutData(new GridData(GridData.FILL_HORIZONTAL));

		final var projectConfiguratorControl = createProjectConfiguratorControl(composite);
		projectConfiguratorControl.setLayoutData(new GridData(GridData.FILL_HORIZONTAL));

		final var infoControl = createInfoControl(composite);
		infoControl.setLayoutData(new GridData(GridData.FILL_HORIZONTAL));

		setControl(composite);

		this.sreGroup.initialize();
		this.sreGroup.selectSpecificSRE(null);
		this.sreGroup.selectSystemWideSRE();
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
		return this.nameGroup.createControl(composite);
	}

	/**
	 * Creates the controls for the location field.
	 *
	 * @param composite the parent composite
	 * @return the created control
	 */
	protected Control createLocationControl(Composite composite) {
		return this.locationGroup.createControl(composite);
	}

	/**
	 * Creates the controls for the JRE selection.
	 *
	 * @param composite the parent composite
	 * @return the created control
	 */
	protected Control createJRESelectionControl(Composite composite) {
		return this.jreGroup.createControl(composite);
	}

	/**
	 * Creates the controls for the SRE selection.
	 *
	 * @param composite the parent composite
	 * @return the created control
	 */
	protected Control createSRESelectionControl(Composite composite) {
		return this.sreGroup.createControl(composite);
	}

	/**
	 * Creates the controls for the working set selection.
	 *
	 * @param composite the parent composite
	 * @return the created control
	 */
	protected Control createWorkingSetControl(Composite composite) {
		return this.workingSetGroup.createControl(composite);
	}

	/**
	 * Creates the controls for the project configurator section.
	 *
	 * @param composite the parent composite.
	 * @return the created control.
	 * @since 0.15
	 */
	protected Control createProjectConfiguratorControl(Composite composite) {
		return this.configuratorGroup.createControl(composite);
	}

	/**
	 * Creates the controls for the info section.
	 *
	 * @param composite the parent composite
	 * @return the created control
	 */
	protected Control createInfoControl(Composite composite) {
		return this.detectGroup.createControl(composite);
	}

	/**
	 * Gets a project name for the new project.
	 *
	 * @return the new project resource handle
	 */
	public String getProjectName() {
		return this.nameGroup.getName();
	}

	/**
	 * Sets the name of the new project.
	 *
	 * @param name the new name
	 * @throws IllegalArgumentException a runtime exception
	 */
	public void setProjectName(String name) {
		if (name == null) {
			throw new IllegalArgumentException();
		}

		this.nameGroup.setName(name);
	}

	/** Replies if the system default SRE must be used for the new project.
	 *
	 * @return {@code true} if the system default must be used.
	 */
	public boolean isSystemDefaultSRE() {
		return this.sreGroup.isSystemWideDefaultSRE();
	}

	/** Replies the selected SRE.
	 *
	 * @return the SRE.
	 */
	public ISREInstall getSRE() {
		return this.sreGroup.getSelectedSRE();
	}

	/**
	 * Returns the current project location path as entered by the user, or {@code null} if
	 * the project should be created in the workspace.
	 *
	 * @return the project location path or its anticipated initial value.
	 */
	public URI getProjectLocationURI() {
		if (this.locationGroup.isUseDefaultSelected()) {
			return null;
		}
		return URIUtil.toURI(this.locationGroup.getLocation());
	}

	/**
	 * Sets the project location of the new project or {@code null} if the project should be created in the workspace.
	 *
	 * @param uri the new project location.
	 */
	public void setProjectLocationURI(URI uri) {
		final var path = uri != null ? URIUtil.toPath(uri) : null;
		this.locationGroup.setLocation(path);
	}

	/**
	 * Returns the compiler compliance to be used for the project, or {@code null} to use the workspace compiler compliance.
	 *
	 * @return compiler compliance to be used for the project or {@code null}
	 */
	public String getCompilerCompliance() {
		return this.jreGroup.getSelectedCompilerCompliance();
	}

	@Override
	public void putDefaultClasspathEntriesIn(Collection<IClasspathEntry> classpathEntries) {
		final var newPath = this.jreGroup.getJREContainerPath();
		if (newPath != null) {
			classpathEntries.add(JavaCore.newContainerEntry(newPath));
		} else {
			final var entries = PreferenceConstants.getDefaultJRELibrary();
			classpathEntries.addAll(Arrays.asList(entries));
		}

		final var sarlClasspathEntry = JavaCore.newContainerEntry(
				SARLClasspathContainerInitializer.CONTAINER_ID,
				new IAccessRule[0],
				new IClasspathAttribute[0],
				true);
		classpathEntries.add(sarlClasspathEntry);

		// Add libraries from the project configurators
		final var iterator = this.configuratorGroup.getActivatedProjectExtensions().iterator();
		while (iterator.hasNext()) {
			final var extension = iterator.next();
			final var binEntries = extension.getBinaryClassPathEntries(getOutputLocation());
			classpathEntries.addAll(binEntries);
		}
	}

	/**
	 * Returns the source class path entries to be added on new projects.
	 * The underlying resource may not exist.
	 *
	 * @return returns the default class path entries
	 */
	public IPath getOutputLocation() {
		var outputLocationPath = new Path(getProjectName()).makeAbsolute();

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
		return this.workingSetGroup.getSelectedWorkingSets();
	}

	/**
	 * Sets the working sets to which the new project should be added.
	 *
	 * @param workingSets the initial selected working sets
	 * @throws IllegalArgumentException a runtime exception
	 */
	public void setWorkingSets(IWorkingSet[] workingSets) {
		if (workingSets == null) {
			throw new IllegalArgumentException();
		}
		this.workingSetGroup.setWorkingSets(workingSets);
	}

	@Override
	public void setVisible(boolean visible) {
		super.setVisible(visible);
		if (visible) {
			this.nameGroup.postSetFocus();
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
		var selected = getSelectedWorkingSet(selection);
		assert selected != null;
		if (selected.length > 0) {
			for (var i = 0; i < selected.length; ++i) {
				if (!isValidWorkingSet(selected[i])) {
					return EMPTY_WORKING_SET_ARRAY;
				}
			}
			return selected;
		}

		selected = EMPTY_WORKING_SET_ARRAY;

		if (activePart instanceof PackageExplorerPart explorerPart) {
			if (explorerPart.getRootMode() == PackageExplorerPart.PROJECTS_AS_ROOTS) {
				// Get active filter
				final var filterWorkingSet = explorerPart.getFilterWorkingSet();
				if (filterWorkingSet != null && isValidWorkingSet(filterWorkingSet)) {
					selected = new IWorkingSet[] {filterWorkingSet};
				}
			} else {
				// If we have been gone into a working set return the working set
				final var input = explorerPart.getViewPartInput();
				if (input instanceof IWorkingSet cvalue
						&& isValidWorkingSet(cvalue)) {
					selected = new IWorkingSet[] {cvalue};
				}
			}
		}

		return selected;
	}

	private static IWorkingSet[] getSelectedWorkingSet(IStructuredSelection selection) {
		var workingSet = EMPTY_WORKING_SET_ARRAY;

		if (selection instanceof ITreeSelection treeSelection) {
			if (!treeSelection.isEmpty()) {
				workingSet = getSelectedWorkingSet(treeSelection);
			}
		}
		return workingSet;
	}

	private static IWorkingSet[] getSelectedWorkingSet(ITreeSelection treeSelection) {
		assert !treeSelection.isEmpty();
		final var elements = treeSelection.toList();
		if (elements.size() == 1) {
			final var element = elements.get(0);
			final var paths = treeSelection.getPathsFor(element);
			if (paths.length == 1
					&& paths[0].getSegmentCount() != 0) {
				final var candidate = paths[0].getSegment(0);
				if (candidate instanceof IWorkingSet cvalue && isValidWorkingSet(cvalue)) {
					return new IWorkingSet[] {cvalue};
				}
			}
		} else {
			final var result = new ArrayList<IWorkingSet>();
			for (var iterator = elements.iterator(); iterator.hasNext();) {
				final var element = iterator.next();
				if (element instanceof IWorkingSet cvalue && isValidWorkingSet(cvalue)) {
					result.add(cvalue);
				}
			}
			return result.toArray(new IWorkingSet[result.size()]);
		}
		return EMPTY_WORKING_SET_ARRAY;
	}

	private static boolean isValidWorkingSet(IWorkingSet workingSet) {
		final var id = workingSet.getId();
		if (!IWorkingSetIDs.JAVA.equals(id) && !IWorkingSetIDs.RESOURCE.equals(id)) {
			return false;
		}

		if (workingSet.isAggregateWorkingSet()) {
			return false;
		}

		return true;
	}

	/** Replies the project extensions that are activated.
	 *
	 * @return the activated extensions.
	 */
	public Stream<ProjectConfigurationFragment> getActivatedProjectExtensions() {
		return this.configuratorGroup.getActivatedProjectExtensions();
	}

	/**
	 * Request a project name. Fires an event whenever the text field is changed, regardless of its content.
	 *
	 * @mavengroupid io.sarl.eclipse
	 * @mavenartifactid io.sarl.eclipse
	 */
	private final class NameGroup extends Observable implements IDialogFieldListener {

		protected final StringDialogField nameField;

		NameGroup() {
			// text field for project name
			this.nameField = new StringDialogField();
			this.nameField.setLabelText(NewWizardMessages.NewJavaProjectWizardPageOne_NameGroup_label_text);
			this.nameField.setDialogFieldListener(this);
		}

		public Control createControl(Composite composite) {
			final var nameComposite = new Composite(composite, SWT.NONE);
			nameComposite.setFont(composite.getFont());
			nameComposite.setLayout(new GridLayout(2, false));

			this.nameField.doFillIntoGrid(nameComposite, 2);
			LayoutUtil.setHorizontalGrabbing(this.nameField.getTextControl(null));

			return nameComposite;
		}

		protected void fireEvent() {
			setChanged();
			notifyObservers();
		}

		public String getName() {
			return this.nameField.getText().trim();
		}

		public void postSetFocus() {
			this.nameField.postSetFocusOnDialogField(getShell().getDisplay());
		}

		public void setName(String name) {
			this.nameField.setText(name);
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
	 *
	 * @mavengroupid io.sarl.eclipse
	 * @mavenartifactid io.sarl.eclipse
	 */
	private final class LocationGroup extends Observable implements Observer, IStringButtonAdapter, IDialogFieldListener {

		private static final String DIALOGSTORE_LAST_EXTERNAL_LOC = JavaUI.ID_PLUGIN + ".last.external.project"; //$NON-NLS-1$

		protected final SelectionButtonDialogField useDefaults;

		protected final StringButtonDialogField location;

		private String previousExternalLocation;

		LocationGroup() {
			this.useDefaults = new SelectionButtonDialogField(SWT.CHECK);
			this.useDefaults.setDialogFieldListener(this);
			this.useDefaults.setLabelText(NewWizardMessages.NewJavaProjectWizardPageOne_LocationGroup_location_desc);

			this.location = new StringButtonDialogField(this);
			this.location.setDialogFieldListener(this);
			this.location.setLabelText(NewWizardMessages.NewJavaProjectWizardPageOne_LocationGroup_locationLabel_desc);
			this.location.setButtonLabel(NewWizardMessages.NewJavaProjectWizardPageOne_LocationGroup_browseButton_desc);

			this.useDefaults.setSelection(true);

			this.previousExternalLocation = ""; //$NON-NLS-1$
		}

		public Control createControl(Composite composite) {
			final var numColumns = 4;

			final var locationComposite = new Composite(composite, SWT.NONE);
			locationComposite.setLayout(new GridLayout(numColumns, false));

			this.useDefaults.doFillIntoGrid(locationComposite, numColumns);
			this.location.doFillIntoGrid(locationComposite, numColumns);
			LayoutUtil.setHorizontalGrabbing(this.location.getTextControl(null));
			BidiUtils.applyBidiProcessing(this.location.getTextControl(null), StructuredTextTypeHandlerFactory.FILE);

			return locationComposite;
		}

		protected void fireEvent() {
			setChanged();
			notifyObservers();
		}

		protected String getDefaultPath(String name) {
			final var path = Platform.getLocation().append(name);
			return path.toOSString();
		}

		@Override
		public void update(Observable observable, Object arg) {
			if (isUseDefaultSelected()) {
				this.location.setText(getDefaultPath(MainProjectWizardPage.this.nameGroup.getName()));
			}
			fireEvent();
		}

		public IPath getLocation() {
			if (isUseDefaultSelected()) {
				return Platform.getLocation();
			}
			return Path.fromOSString(this.location.getText().trim());
		}

		public boolean isUseDefaultSelected() {
			return this.useDefaults.isSelected();
		}

		public void setLocation(IPath path) {
			this.useDefaults.setSelection(path == null);
			if (path != null) {
				this.location.setText(path.toOSString());
			} else {
				this.location.setText(getDefaultPath(MainProjectWizardPage.this.nameGroup.getName()));
			}
			fireEvent();
		}

		@Override
		public void changeControlPressed(DialogField field) {
			final var dialog = new DirectoryDialog(getShell());
			dialog.setMessage(NewWizardMessages.NewJavaProjectWizardPageOne_directory_message);
			String directoryName = this.location.getText().trim();
			if (directoryName.length() == 0) {
				final var prevLocation = JavaPlugin.getDefault().getDialogSettings().get(DIALOGSTORE_LAST_EXTERNAL_LOC);
				if (prevLocation != null) {
					directoryName = prevLocation;
				}
			}

			if (directoryName.length() > 0) {
				final var path = new File(directoryName);
				if (path.exists()) {
					dialog.setFilterPath(directoryName);
				}
			}
			final String selectedDirectory = dialog.open();
			if (selectedDirectory != null) {
				final var oldDirectory = new Path(this.location.getText().trim()).lastSegment();
				this.location.setText(selectedDirectory);
				final var lastSegment = new Path(selectedDirectory).lastSegment();
				if (lastSegment != null
						&& (MainProjectWizardPage.this.nameGroup.getName().length() == 0
						|| MainProjectWizardPage.this.nameGroup.getName().equals(oldDirectory))) {
					MainProjectWizardPage.this.nameGroup.setName(lastSegment);
				}
				JavaPlugin.getDefault().getDialogSettings().put(DIALOGSTORE_LAST_EXTERNAL_LOC, selectedDirectory);
			}
		}

		@Override
		public void dialogFieldChanged(DialogField field) {
			if (field == this.useDefaults) {
				final var checked = this.useDefaults.isSelected();
				if (checked) {
					this.previousExternalLocation = this.location.getText();
					this.location.setText(getDefaultPath(MainProjectWizardPage.this.nameGroup.getName()));
					this.location.setEnabled(false);
				} else {
					this.location.setText(this.previousExternalLocation);
					this.location.setEnabled(true);
				}
			}
			fireEvent();
		}
	}

	/** Group that contains the configuration of the JRE.
	 *
	 * @author <a href="http://www.ciad-lab.fr/nicolas_gaud">Nicolas Gaud</a>
	 * @version io.sarl.eclipse 0.15.0 20250909-115751
	 * @mavengroupid io.sarl.eclipse
	 * @mavenartifactid io.sarl.eclipse
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

		private final SelectionButtonDialogField useDefaultJRE;

		private final SelectionButtonDialogField useProjectJRE;

		private final SelectionButtonDialogField useEEJRE;

		private final ComboDialogField jreCombo;

		private final ComboDialogField eeCombo;

		private Group group;

		private Link preferenceLink;

		private IVMInstall[] installedJVMs;

		private String[] jreCompliance;

		private IExecutionEnvironment[] installedEEs;

		private String[] eeCompliance;

		JREGroup() {
			this.useDefaultJRE = new SelectionButtonDialogField(SWT.RADIO);
			this.useDefaultJRE.setLabelText(getDefaultJVMLabel());

			this.useProjectJRE = new SelectionButtonDialogField(SWT.RADIO);
			this.useProjectJRE.setLabelText(NewWizardMessages.NewJavaProjectWizardPageOne_JREGroup_specific_compliance);

			this.jreCombo = new ComboDialogField(SWT.READ_ONLY);
			fillInstalledJREs(this.jreCombo);
			this.jreCombo.setDialogFieldListener(this);

			this.useEEJRE = new SelectionButtonDialogField(SWT.RADIO);
			this.useEEJRE.setLabelText(NewWizardMessages.NewJavaProjectWizardPageOne_JREGroup_specific_EE);

			this.eeCombo = new ComboDialogField(SWT.READ_ONLY);
			fillExecutionEnvironments(this.eeCombo);
			this.eeCombo.setDialogFieldListener(this);

			switch (getLastSelectedJREKind()) {
			case DEFAULT_JRE:
				this.useDefaultJRE.setSelection(true);
				break;
			case PROJECT_JRE:
				this.useProjectJRE.setSelection(true);
				break;
			case EE_JRE:
				this.useEEJRE.setSelection(true);
				break;
			default:
			}

			this.jreCombo.setEnabled(this.useProjectJRE.isSelected());
			this.eeCombo.setEnabled(this.useEEJRE.isSelected());

			this.useDefaultJRE.setDialogFieldListener(this);
			this.useProjectJRE.setDialogFieldListener(this);
			this.useEEJRE.setDialogFieldListener(this);
		}

		public Control createControl(Composite composite) {
			this.group = new Group(composite, SWT.NONE);
			this.group.setFont(composite.getFont());
			this.group.setLayout(initGridLayout(new GridLayout(2, false), true));
			this.group.setText(NewWizardMessages.NewJavaProjectWizardPageOne_JREGroup_title);

			this.useEEJRE.doFillIntoGrid(this.group, 1);
			final var eeComboControl = this.eeCombo.getComboControl(this.group);
			eeComboControl.setLayoutData(new GridData(GridData.FILL, GridData.CENTER, true, false));

			this.useProjectJRE.doFillIntoGrid(this.group, 1);
			final var comboControl = this.jreCombo.getComboControl(this.group);
			comboControl.setLayoutData(new GridData(GridData.FILL, GridData.CENTER, true, false));

			this.useDefaultJRE.doFillIntoGrid(this.group, 1);

			this.preferenceLink = new Link(this.group, SWT.NONE);
			this.preferenceLink.setFont(this.group.getFont());
			this.preferenceLink.setText(NewWizardMessages.NewJavaProjectWizardPageOne_JREGroup_link_description);
			this.preferenceLink.setLayoutData(new GridData(GridData.END, GridData.CENTER, false, false));
			this.preferenceLink.addSelectionListener(this);

			updateEnableState();
			return this.group;
		}

		private void fillInstalledJREs(ComboDialogField comboField) {
			var selectedItem = getLastSelectedJRE();
			var selectionIndex = comboField.getSelectionIndex();
			if (this.useProjectJRE.isSelected()) {
				if (selectionIndex != -1) {
					// paranoia
					selectedItem = comboField.getItems()[selectionIndex];
				}
			}

			this.installedJVMs = getWorkspaceJREs();
			Arrays.sort(this.installedJVMs, new Comparator<IVMInstall>() {

				@Override
				public int compare(IVMInstall i0, IVMInstall i1) {
					if (i1 instanceof IVMInstall2 cvalue1 && i0 instanceof IVMInstall2 cvalue0) {
						final var cc0 = JavaModelUtil.getCompilerCompliance(cvalue0,
								SARLVersion.MINIMAL_JDK_VERSION_IN_SARL_PROJECT_CLASSPATH);
						final var cc1 = JavaModelUtil.getCompilerCompliance(cvalue1,
								SARLVersion.MINIMAL_JDK_VERSION_IN_SARL_PROJECT_CLASSPATH);
						final var result = cc1.compareTo(cc0);
						if (result != 0) {
							return result;
						}
					}
					return Policy.getComparator().compare(i0.getName(), i1.getName());
				}

			});
			// find new index
			selectionIndex = -1;
			final var jreLabels = new String[this.installedJVMs.length];
			this.jreCompliance = new String[this.installedJVMs.length];
			for (var i = 0; i < this.installedJVMs.length; i++) {
				jreLabels[i] = this.installedJVMs[i].getName();
				if (selectedItem != null && jreLabels[i].equals(selectedItem)) {
					selectionIndex = i;
				}
				if (this.installedJVMs[i] instanceof IVMInstall2 cvalue) {
					this.jreCompliance[i] = JavaModelUtil.getCompilerCompliance(
							cvalue,
							SARLVersion.MINIMAL_JDK_VERSION_IN_SARL_PROJECT_CLASSPATH);
				} else {
					this.jreCompliance[i] = SARLVersion.MINIMAL_JDK_VERSION_IN_SARL_PROJECT_CLASSPATH;
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
			var selectedItem = getLastSelectedEE();
			var selectionIndex = comboField.getSelectionIndex();
			if (this.useEEJRE.isSelected()) {
				if (selectionIndex != -1) {
					// paranoia
					selectedItem = comboField.getItems()[selectionIndex];
				}
			}

			this.installedEEs = JavaRuntime.getExecutionEnvironmentsManager().getExecutionEnvironments();
			Arrays.sort(this.installedEEs, new Comparator<IExecutionEnvironment>() {
				@Override
				public int compare(IExecutionEnvironment arg0, IExecutionEnvironment arg1) {
					return Policy.getComparator().compare(arg0.getId(), arg1.getId());
				}
			});
			// find new index
			selectionIndex = -1;
			final var eeLabels = new String[this.installedEEs.length];
			this.eeCompliance = new String[this.installedEEs.length];
			for (var i = 0; i < this.installedEEs.length; i++) {
				eeLabels[i] = this.installedEEs[i].getId();
				if (selectedItem != null && eeLabels[i].equals(selectedItem)) {
					selectionIndex = i;
				}
				this.eeCompliance[i] = JavaModelUtil.getExecutionEnvironmentCompliance(this.installedEEs[i]);
			}
			comboField.setItems(eeLabels);
			if (selectionIndex == -1) {
				comboField.selectItem(getDefaultEEName());
			} else {
				comboField.selectItem(selectedItem);
			}
		}

		private IVMInstall[] getWorkspaceJREs() {
			final var standins = new ArrayList<VMStandin>();
			final var types = JavaRuntime.getVMInstallTypes();
			for (var i = 0; i < types.length; i++) {
				final var type = types[i];
				final var installs = type.getVMInstalls();
				for (var j = 0; j < installs.length; j++) {
					final var install = installs[j];
					standins.add(new VMStandin(install));
				}
			}
			return standins.toArray(new IVMInstall[standins.size()]);
		}

		private String getDefaultJVMName() {
			final var install = JavaRuntime.getDefaultVMInstall();
			if (install != null) {
				return install.getName();
			}
			return NewWizardMessages.NewJavaProjectWizardPageOne_UnknownDefaultJRE_name;
		}

		private String getDefaultEEName() {
			final var defaultVM = JavaRuntime.getDefaultVMInstall();

			final var environments = JavaRuntime.getExecutionEnvironmentsManager()
					.getExecutionEnvironments();
			if (defaultVM != null) {
				for (var i = 0; i < environments.length; i++) {
					final var eeDefaultVM = environments[i].getDefaultVM();
					if (eeDefaultVM != null && defaultVM.getId().equals(eeDefaultVM.getId())) {
						return environments[i].getId();
					}
				}
			}

			final String defaultCC;
			if (defaultVM instanceof IVMInstall2 cvalue) {
				defaultCC = JavaModelUtil.getCompilerCompliance(cvalue, SARLVersion.MINIMAL_JDK_VERSION_IN_SARL_PROJECT_CLASSPATH);
			} else {
				defaultCC = SARLVersion.MINIMAL_JDK_VERSION_IN_SARL_PROJECT_CLASSPATH;
			}

			for (var i = 0; i < environments.length; i++) {
				final var eeCompliance = JavaModelUtil.getExecutionEnvironmentCompliance(environments[i]);
				if (defaultCC.endsWith(eeCompliance)) {
					return environments[i].getId();
				}
			}

			return SARLVersion.MINIMAL_JDK_VERSION_IN_SARL_PROJECT_CLASSPATH;
		}

		private String getDefaultJVMLabel() {
			return MessageFormat.format(
					NewWizardMessages.NewJavaProjectWizardPageOne_JREGroup_default_compliance,
					getDefaultJVMName());
		}

		@Override
		public void update(Observable observable, Object arg) {
			updateEnableState();
		}

		private void updateEnableState() {
			final var detect = MainProjectWizardPage.this.detectGroup.mustDetect();
			this.useDefaultJRE.setEnabled(!detect);
			this.useProjectJRE.setEnabled(!detect);
			this.useEEJRE.setEnabled(!detect);
			this.jreCombo.setEnabled(!detect && this.useProjectJRE.isSelected());
			this.eeCombo.setEnabled(!detect && this.useEEJRE.isSelected());
			if (this.preferenceLink != null) {
				this.preferenceLink.setEnabled(!detect);
			}
			if (this.group != null) {
				this.group.setEnabled(!detect);
			}
		}

		@Override
		public void widgetSelected(SelectionEvent event) {
			widgetDefaultSelected(event);
		}

		@Override
		public void widgetDefaultSelected(SelectionEvent event) {
			final var jreID = BuildPathSupport.JRE_PREF_PAGE_ID;
			final var eeID = BuildPathSupport.EE_PREF_PAGE_ID;
			final var complianceId = CompliancePreferencePage.PREF_ID;
			final var data = new HashMap<>();
			data.put(PropertyAndPreferencePage.DATA_NO_LINK, Boolean.TRUE);
			PreferencesUtil.createPreferenceDialogOn(
					getShell(), jreID,
					new String[] {jreID, complianceId, eeID},
					data).open();

			handlePossibleJVMChange();
			MainProjectWizardPage.this.detectGroup.handlePossibleJVMChange();
		}

		public void handlePossibleJVMChange() {
			this.useDefaultJRE.setLabelText(getDefaultJVMLabel());
			fillInstalledJREs(this.jreCombo);
			fillExecutionEnvironments(this.eeCombo);
		}

		@Override
		public void dialogFieldChanged(DialogField field) {
			updateEnableState();
			MainProjectWizardPage.this.detectGroup.handlePossibleJVMChange();
			if (field == this.jreCombo) {
				if (this.useProjectJRE.isSelected()) {
					storeSelectionValue(this.jreCombo, LAST_SELECTED_JRE_SETTINGS_KEY);
				}
			} else if (field == this.eeCombo) {
				if (this.useEEJRE.isSelected()) {
					storeSelectionValue(this.eeCombo, LAST_SELECTED_EE_SETTINGS_KEY);
				}
			} else if (field == this.useDefaultJRE) {
				if (this.useDefaultJRE.isSelected()) {
					JavaPlugin.getDefault().getDialogSettings().put(LAST_SELECTED_JRE_KIND2, DEFAULT_JRE);
					this.useProjectJRE.setSelection(false);
					this.useEEJRE.setSelection(false);
				}
			} else if (field == this.useProjectJRE) {
				if (this.useProjectJRE.isSelected()) {
					JavaPlugin.getDefault().getDialogSettings().put(LAST_SELECTED_JRE_KIND2, PROJECT_JRE);
					this.useDefaultJRE.setSelection(false);
					this.useEEJRE.setSelection(false);
				}
			} else if (field == this.useEEJRE) {
				if (this.useEEJRE.isSelected()) {
					JavaPlugin.getDefault().getDialogSettings().put(LAST_SELECTED_JRE_KIND2, EE_JRE);
					this.useDefaultJRE.setSelection(false);
					this.useProjectJRE.setSelection(false);
				}
			}
		}

		private void storeSelectionValue(ComboDialogField combo, String preferenceKey) {
			final var index = combo.getSelectionIndex();
			if (index == -1) {
				return;
			}

			final var item = combo.getItems()[index];
			JavaPlugin.getDefault().getDialogSettings().put(preferenceKey, item);
		}

		private int getLastSelectedJREKind() {
			final var settings = JavaPlugin.getDefault().getDialogSettings();
			if (settings.get(LAST_SELECTED_JRE_KIND2) == null) {
				return EE_JRE;
			}

			return settings.getInt(LAST_SELECTED_JRE_KIND2);
		}

		private String getLastSelectedEE() {
			final var settings = JavaPlugin.getDefault().getDialogSettings();
			return settings.get(LAST_SELECTED_EE_SETTINGS_KEY);
		}

		private String getLastSelectedJRE() {
			final IDialogSettings settings = JavaPlugin.getDefault().getDialogSettings();
			return settings.get(LAST_SELECTED_JRE_SETTINGS_KEY);
		}

		public IVMInstall getSelectedJVM() {
			if (this.useProjectJRE.isSelected()) {
				final var index = this.jreCombo.getSelectionIndex();
				if (index >= 0 && index < this.installedJVMs.length) {
					// paranoia
					return this.installedJVMs[index];
				}
			}
			return null;
		}

		public IPath getJREContainerPath() {
			if (this.useProjectJRE.isSelected()) {
				final var index = this.jreCombo.getSelectionIndex();
				if (index >= 0 && index < this.installedJVMs.length) {
					// paranoia
					return JavaRuntime.newJREContainerPath(this.installedJVMs[index]);
				}
			} else if (this.useEEJRE.isSelected()) {
				final var index = this.eeCombo.getSelectionIndex();
				if (index >= 0 && index < this.installedEEs.length) {
					// paranoia
					return JavaRuntime.newJREContainerPath(this.installedEEs[index]);
				}
			}
			return null;
		}

		public String getSelectedCompilerCompliance() {
			if (this.useProjectJRE.isSelected()) {
				final var index = this.jreCombo.getSelectionIndex();
				if (index >= 0 && index < this.jreCompliance.length) {
					// paranoia
					return this.jreCompliance[index];
				}
			} else if (this.useEEJRE.isSelected()) {
				final var index = this.eeCombo.getSelectionIndex();
				if (index >= 0 && index < this.eeCompliance.length) {
					// paranoia
					return this.eeCompliance[index];
				}
			}
			return null;
		}
	}

	/** Group of configuration for the working set.
	 *
	 * @author <a href="http://www.ciad-lab.fr/nicolas_gaud">Nicolas Gaud</a>
	 * @author <a href="http://www.ciad-lab.fr/stephane_galland">St&eacute;phane Galland</a>
	 * @version io.sarl.eclipse 0.15.0 20250909-115751
	 * @mavengroupid io.sarl.eclipse
	 * @mavenartifactid io.sarl.eclipse
	 */
	private final class WorkingSetGroup {

		private WorkingSetConfigurationBlock workingSetBlock;

		WorkingSetGroup() {
			final var workingSetIds = new String[] {IWorkingSetIDs.JAVA, IWorkingSetIDs.RESOURCE};
			this.workingSetBlock = new WorkingSetConfigurationBlock(workingSetIds, JavaPlugin.getDefault().getDialogSettings());
			// fWorkingSetBlock.setDialogMessage(NewWizardMessages.NewJavaProjectWizardPageOne_WorkingSetSelection_message);
		}

		public Control createControl(Composite composite) {
			final var workingSetGroup = new Group(composite, SWT.NONE);
			workingSetGroup.setFont(composite.getFont());
			workingSetGroup.setText(NewWizardMessages.NewJavaProjectWizardPageOne_WorkingSets_group);
			workingSetGroup.setLayout(new GridLayout(1, false));

			this.workingSetBlock.createContent(workingSetGroup);

			return workingSetGroup;
		}

		public void setWorkingSets(IWorkingSet[] workingSets) {
			this.workingSetBlock.setWorkingSets(workingSets);
		}

		public IWorkingSet[] getSelectedWorkingSets() {
			return this.workingSetBlock.getSelectedWorkingSets();
		}
	}

	/**
	 * Show a warning when the project location contains files.
	 *
	 * @author <a href="http://www.ciad-lab.fr/nicolas_gaud">Nicolas Gaud</a>
	 * @author <a href="http://www.ciad-lab.fr/stephane_galland">St&eacute;phane Galland</a>
	 * @version io.sarl.eclipse 0.15.0 20250909-115751
	 * @mavengroupid io.sarl.eclipse
	 * @mavenartifactid io.sarl.eclipse
	 */
	private final class DetectGroup extends Observable implements Observer, SelectionListener {

		private static final int HORIZONTAL_SPACING = 10;

		private static final int WIDTH_HINT = 50;

		private static final int HEIGHT_HINT = 3;

		private Link fHintText;

		private Label icon;

		private boolean fDetect;

		DetectGroup() {
			this.fDetect = false;
		}

		@SuppressWarnings("synthetic-access")
		public Control createControl(Composite parent) {

			final var composite = new Composite(parent, SWT.NONE);
			composite.setLayoutData(new GridData(SWT.FILL, SWT.TOP, true, false));
			final var layout = new GridLayout(2, false);
			layout.horizontalSpacing = HORIZONTAL_SPACING;
			composite.setLayout(layout);

			this.icon = new Label(composite, SWT.LEFT);
			this.icon.setImage(Dialog.getImage(Dialog.DLG_IMG_MESSAGE_WARNING));
			var gridData = new GridData(SWT.LEFT, SWT.TOP, false, false);
			this.icon.setLayoutData(gridData);

			this.fHintText = new Link(composite, SWT.WRAP);
			this.fHintText.setFont(composite.getFont());
			this.fHintText.addSelectionListener(this);
			gridData = new GridData(GridData.FILL, SWT.FILL, true, true);
			gridData.widthHint = convertWidthInCharsToPixels(WIDTH_HINT);
			gridData.heightHint = convertHeightInCharsToPixels(HEIGHT_HINT);
			this.fHintText.setLayoutData(gridData);

			handlePossibleJVMChange();
			return composite;
		}

		public void handlePossibleJVMChange() {

			if (JavaRuntime.getDefaultVMInstall() == null) {
				this.fHintText.setText(NewWizardMessages.NewJavaProjectWizardPageOne_NoJREFound_link);
				this.fHintText.setVisible(true);
				this.icon.setImage(Dialog.getImage(Dialog.DLG_IMG_MESSAGE_WARNING));
				this.icon.setVisible(true);
				return;
			}

			var selectedCompliance = MainProjectWizardPage.this.jreGroup.getSelectedCompilerCompliance();
			if (selectedCompliance != null) {
				final var defaultCompliance = JavaCore.getOption(JavaCore.COMPILER_COMPLIANCE);
				if (selectedCompliance.equals(defaultCompliance)) {
					this.fHintText.setVisible(false);
					this.icon.setVisible(false);
				} else {
					this.fHintText.setText(MessageFormat.format(
							NewWizardMessages.NewJavaProjectWizardPageOne_DetectGroup_differendWorkspaceCC_message,
							TextProcessor.process(defaultCompliance),
							TextProcessor.process(selectedCompliance)));
					this.fHintText.setVisible(true);
					this.icon.setImage(Dialog.getImage(Dialog.DLG_IMG_MESSAGE_INFO));
					this.icon.setVisible(true);
				}
				return;
			}

			selectedCompliance = JavaCore.getOption(JavaCore.COMPILER_COMPLIANCE);
			var selectedJVM = MainProjectWizardPage.this.jreGroup.getSelectedJVM();
			if (selectedJVM == null) {
				selectedJVM = JavaRuntime.getDefaultVMInstall();
			}
			var jvmCompliance = SARLVersion.MINIMAL_JDK_VERSION_IN_SARL_PROJECT_CLASSPATH;
			if (selectedJVM instanceof IVMInstall2 cvalue) {
				jvmCompliance = JavaModelUtil.getCompilerCompliance(cvalue,
						SARLVersion.MINIMAL_JDK_VERSION_IN_SARL_PROJECT_CLASSPATH);
			}
			if (!selectedCompliance.equals(jvmCompliance)) {
				this.fHintText.setText(MessageFormat.format(
						NewWizardMessages.NewJavaProjectWizardPageOne_DetectGroup_jre_message,
						TextProcessor.process(selectedCompliance),
						TextProcessor.process(jvmCompliance)));
				this.fHintText.setVisible(true);
				this.icon.setImage(Dialog.getImage(Dialog.DLG_IMG_MESSAGE_WARNING));
				this.icon.setVisible(true);
			} else {
				this.fHintText.setVisible(false);
				this.icon.setVisible(false);
			}

		}

		private boolean computeDetectState() {
			if (MainProjectWizardPage.this.locationGroup.isUseDefaultSelected()) {
				final var name = MainProjectWizardPage.this.nameGroup.getName();
				if (name.length() == 0 || JavaPlugin.getWorkspace().getRoot().findMember(name) != null) {
					return false;
				}

				final var directory = MainProjectWizardPage.this.locationGroup.getLocation().append(name).toFile();
				return directory.isDirectory();
			}

			final var directory = MainProjectWizardPage.this.locationGroup.getLocation().toFile();
			return directory.isDirectory();
		}

		@Override
		public void update(Observable observable, Object arg) {
			if (observable instanceof LocationGroup) {
				final var oldDetectState = this.fDetect;
				this.fDetect = computeDetectState();

				if (oldDetectState != this.fDetect) {
					setChanged();
					notifyObservers();

					if (this.fDetect) {
						this.fHintText.setVisible(true);
						this.fHintText.setText(NewWizardMessages.NewJavaProjectWizardPageOne_DetectGroup_message);
						this.icon.setImage(Dialog.getImage(Dialog.DLG_IMG_MESSAGE_INFO));
						this.icon.setVisible(true);
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
		public void widgetSelected(SelectionEvent event) {
			widgetDefaultSelected(event);
		}

		@Override
		public void widgetDefaultSelected(SelectionEvent event) {
			final var jreID = BuildPathSupport.JRE_PREF_PAGE_ID;
			final var eeID = BuildPathSupport.EE_PREF_PAGE_ID;
			final var complianceId = CompliancePreferencePage.PREF_ID;
			final var data = new HashMap<String, Boolean>();
			data.put(PropertyAndPreferencePage.DATA_NO_LINK, Boolean.TRUE);
			final var id = "JRE".equals(event.text) ? jreID : complianceId; //$NON-NLS-1$
			PreferencesUtil.createPreferenceDialogOn(getShell(), id,
					new String[] {jreID, complianceId, eeID},
					data).open();

			MainProjectWizardPage.this.jreGroup.handlePossibleJVMChange();
			handlePossibleJVMChange();
		}
	}

	/**
	 * Validate this page and show appropriate warnings and error NewWizardMessages.
	 *
	 * @author <a href="http://www.ciad-lab.fr/nicolas_gaud">Nicolas Gaud</a>
	 * @author <a href="http://www.ciad-lab.fr/stephane_galland">St&eacute;phane Galland</a>
	 * @version io.sarl.eclipse 0.15.0 20250909-115751
	 * @mavengroupid io.sarl.eclipse
	 * @mavenartifactid io.sarl.eclipse
	 */
	private final class Validator implements Observer {

		@Override
		public void update(Observable observable, Object arg) {
			try {
				final var workspace = JavaPlugin.getWorkspace();
				final var name = MainProjectWizardPage.this.nameGroup.getName();
				checkProjectName(workspace, name);
				final var handle = checkProjectExist(workspace, name);
				final var location = MainProjectWizardPage.this.locationGroup.getLocation().toOSString();
				final var projectPath = checkLocationSyntax(location);
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
		 * @param workspace the workspace.
		 * @param name name of the project.
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
			final var nameStatus = workspace.validateName(name, IResource.PROJECT);
			if (!nameStatus.isOK()) {
				throw new ValidationException(
						null,
						nameStatus.getMessage());
			}
		}

		/** Check whether project already exists.
		 *
		 * @param workspace the workspace.
		 * @param name name of the project.
		 * @return the project.
		 * @throws ValidationException if the project exists.
		 */
		private IProject checkProjectExist(IWorkspace workspace, String name) throws ValidationException {
			final var handle = workspace.getRoot().getProject(name);
			if (handle.exists()) {
				throw new ValidationException(
						null,
						NewWizardMessages.NewJavaProjectWizardPageOne_Message_projectAlreadyExists);
			}

			var projectLocation = ResourcesPlugin.getWorkspace().getRoot().getLocation().append(name);
			if (projectLocation.toFile().exists()) {
				try {
					// correct casing
					final var canonicalPath = projectLocation.toFile().getCanonicalPath();
					projectLocation = new Path(canonicalPath);
				} catch (IOException e) {
					SARLEclipsePlugin.getDefault().log(e);
				}

				final var existingName = projectLocation.lastSegment();
				if (!existingName.equals(MainProjectWizardPage.this.nameGroup.getName())) {
					throw new ValidationException(
							null,
							MessageFormat.format(
									NewWizardMessages.NewJavaProjectWizardPageOne_Message_invalidProjectNameForWorkspaceRoot,
									TextProcessor.process(existingName)));
				}
			}
			return handle;
		}

		/**
		 * Check whether the location is a syntactically correct path.
		 *
		 * @param location the location to test.
		 * @return the project path.
		 * @throws ValidationException if the location has invalid syntax.
		 */
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
			if (!MainProjectWizardPage.this.locationGroup.isUseDefaultSelected()) {
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
		 * @param workspace the workspace.
		 * @param handle the project.
		 * @param projectPath the project path.
		 * @throws ValidationException if the location has invalid syntax.
		 */
		private void validateLocation(
				IWorkspace workspace,
				IProject handle, IPath projectPath)  throws ValidationException {
			final var locationStatus = workspace.validateProjectLocation(handle, projectPath);
			if (!locationStatus.isOK()) {
				throw new ValidationException(
						null,
						locationStatus.getMessage());
			}
		}

		private boolean canCreate(File file) {
			var fileInHierarchy = file;
			while (!fileInHierarchy.exists()) {
				fileInHierarchy = fileInHierarchy.getParentFile();
				if (fileInHierarchy == null) {
					return false;
				}
			}
			return fileInHierarchy.canWrite();
		}

		/** Exception that contains any validation error.
		 *
		 * @author <a href="http://www.ciad-lab.fr/stephane_galland">St&eacute;phane Galland</a>
		 * @version io.sarl.eclipse 0.15.0 20250909-115751
		 * @mavengroupid io.sarl.eclipse
		 * @mavenartifactid io.sarl.eclipse
		 */
		private class ValidationException extends Exception {

			private static final long serialVersionUID = 2953587040979817037L;

			private final String errorMessage;

			ValidationException(String message, String errorMessage) {
				super(message);
				this.errorMessage = errorMessage;
			}

			public String getErrorMessage() {
				return this.errorMessage;
			}

		}
	}

	/**
	 * Show the list of project configuration fragments that are available for project creation.
	 *
	 * @author <a href="http://www.ciad-lab.fr/stephane_galland">St&eacute;phane Galland</a>
	 * @version io.sarl.eclipse 0.15.0 20250909-115751
	 * @mavengroupid io.sarl.eclipse
	 * @mavenartifactid io.sarl.eclipse
	 * @since 0.15
	 */
	private final class ProjectConfiguratorsGroup extends Observable implements IDialogFieldListener {

		private static final String LAST_SELECTED_SETTINGS_KEY_PREFIX = SARLEclipsePlugin.PLUGIN_ID + ".last.selected.create.projectconfigurators."; //$NON-NLS-1$

		private final Map<SelectionButtonDialogField, ProjectConfigurationFragment> field2fragment = new TreeMap<>(
				(a, b) -> {
					final var ida = System.identityHashCode(a);
					final var idb = System.identityHashCode(b);
					return Integer.compare(ida, idb);
				});

		/** Constructor.
		 */
		ProjectConfiguratorsGroup() {
			for (final var configuration : ProjectConfigurationFragments.getConfigurationFragmentsFromExtension()) {
				final var checkbox = new SelectionButtonDialogField(SWT.CHECK);
				checkbox.setLabelText(configuration.getLabel());
				checkbox.setDialogFieldListener(this);
				checkbox.setEnabled(true);
				this.field2fragment.put(checkbox, configuration);
			}
		}

		/** Create the controls in this group.
		 * 
		 * @param composite the parent component.
		 * @return the created control.
		 */
		public Control createControl(Composite composite) {
			final var layout = new GridLayout();
			layout.numColumns = 2;
			final var configuratorGroup = new Group(composite, SWT.NONE);
			configuratorGroup.setFont(composite.getFont());
			configuratorGroup.setText(Messages.MainProjectWizardPage_2);
			configuratorGroup.setLayout(layout);

			for (final var entry : this.field2fragment.entrySet()) {
				final var fragment = entry.getValue();
				final var pref = JavaPlugin.getDefault().getDialogSettings().get(preferenceKey(fragment.getId()));
				final var checkbox = entry.getKey();
				checkbox.setSelection(pref == null ? fragment.isActiveByDefault(): Boolean.parseBoolean(pref));
				checkbox.doFillIntoGrid(configuratorGroup, 2);
			}

			return configuratorGroup;
		}

		private static String preferenceKey(String configurationId) {
			return LAST_SELECTED_SETTINGS_KEY_PREFIX + configurationId;
		}

		private void handlePossibleChange(SelectionButtonDialogField field, ProjectConfigurationFragment fragment) {
			final var pref = JavaPlugin.getDefault().getDialogSettings().get(preferenceKey(fragment.getId()));
			final var selectedOrig = pref == null ? true : Boolean.parseBoolean(pref);
			var selected = selectedOrig;
			if (field.isEnabled()) {
				selected = field.isSelected();
			}
			if (selectedOrig != selected) {
				final var preferences = JavaPlugin.getDefault().getDialogSettings();
				preferences.put(preferenceKey(fragment.getId()), field.isSelected());
			}
		}

		@Override
		public void dialogFieldChanged(DialogField field) {
			if (field instanceof SelectionButtonDialogField cfield) {
				final var configuration = this.field2fragment.get(cfield); 
				handlePossibleChange(cfield, configuration);
			}
		}

		/** Replies the project extensions that are activated.
		 *
		 * @return the activated extensions.
		 */
		Stream<ProjectConfigurationFragment> getActivatedProjectExtensions() {
			return this.field2fragment.entrySet().stream()
					.filter(it -> it.getKey().isEnabled() && it.getKey().isSelected())
					.map(it -> it.getValue());
		}

	}

}
