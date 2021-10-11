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
import org.eclipse.osgi.util.TextProcessor;
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

import io.sarl.eclipse.SARLEclipseConfig;
import io.sarl.eclipse.SARLEclipsePlugin;
import io.sarl.eclipse.buildpath.SARLClasspathContainerInitializer;
import io.sarl.eclipse.natures.SARLProjectConfigurator;
import io.sarl.eclipse.runtime.ISREInstall;
import io.sarl.eclipse.runtime.SREConfigurationBlock;
import io.sarl.eclipse.util.classpath.SarlDefaultClassPathProvider;
import io.sarl.lang.SARLConfig;
import io.sarl.lang.SARLVersion;

/**
 * The first page of the SARL new project wizard. Most part of the code of this class
 * is copy/paste from {@link NewJavaProjectWizardPageOne}
 *
 * <p>This version removes the choice of the project structure and update the structure of
 * the source folder of the project with
 * {@link SARLProjectConfigurator#getDefaultSourceClassPathEntries(IPath)}
 *
 * @author $Author: ngaud$
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 */
@SuppressWarnings("deprecation")
public class MainProjectWizardPage extends WizardPage implements SarlDefaultClassPathProvider {

	private static final IWorkingSet[] EMPTY_WORKING_SET_ARRAY = new IWorkingSet[0];

	private final NameGroup nameGroup;

	private final LocationGroup locationGroup;

	private final SREConfigurationBlock sreGroup;

	private final JREGroup jreGroup;

	private final DetectGroup detectGroup;

	private final Validator validator;

	private final WorkingSetGroup workingSetGroup;

	/**
	 * Creates a new {@link MainProjectWizardPage}.
	 */
	@SuppressWarnings({ "synthetic-access" })
	public MainProjectWizardPage() {
		super(Messages.SARLProjectNewWizard_3);
		setPageComplete(false);

		this.nameGroup = new NameGroup();
		this.locationGroup = new LocationGroup();
		this.sreGroup = new SREConfigurationBlock(Messages.MainProjectPage_0, true, null, null);
		this.jreGroup = new JREGroup();
		this.workingSetGroup = new WorkingSetGroup();
		this.detectGroup = new DetectGroup();

		// establish connections
		this.nameGroup.addObserver(this.locationGroup);
		this.detectGroup.addObserver(this.jreGroup);
		this.locationGroup.addObserver(this.detectGroup);

		// initialize all elements
		this.nameGroup.notifyObservers();

		// create and connect validator
		this.validator = new Validator();
		this.nameGroup.addObserver(this.validator);
		this.locationGroup.addObserver(this.validator);

		// initialize defaults
		setProjectName(""); //$NON-NLS-1$
		setProjectLocationURI(null);
		setWorkingSets(new IWorkingSet[0]);

		initializeDefaultVM();

		setTitle(Messages.SARLProjectNewWizard_3);
		setDescription(Messages.SARLProjectNewWizard_1);
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

		final Composite composite = new Composite(parent, SWT.NULL);
		composite.setFont(parent.getFont());
		composite.setLayout(initGridLayout(new GridLayout(1, false), true));
		composite.setLayoutData(new GridData(GridData.HORIZONTAL_ALIGN_FILL));

		// create UI elements
		final Control nameControl = createNameControl(composite);
		nameControl.setLayoutData(new GridData(GridData.FILL_HORIZONTAL));

		final Control locationControl = createLocationControl(composite);
		locationControl.setLayoutData(new GridData(GridData.FILL_HORIZONTAL));

		final Control sreControl = createSRESelectionControl(composite);
		sreControl.setLayoutData(new GridData(GridData.FILL_HORIZONTAL));

		final Control jreControl = createJRESelectionControl(composite);
		jreControl.setLayoutData(new GridData(GridData.FILL_HORIZONTAL));

		final Control workingSetControl = createWorkingSetControl(composite);
		workingSetControl.setLayoutData(new GridData(GridData.FILL_HORIZONTAL));

		final Control infoControl = createInfoControl(composite);
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
	 * @return <code>true</code> if the system default must be used.
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
		final IPath path = uri != null ? URIUtil.toPath(uri) : null;
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
		final IPath newPath = this.jreGroup.getJREContainerPath();
		if (newPath != null) {
			classpathEntries.add(JavaCore.newContainerEntry(newPath));
		} else {
			final IClasspathEntry[] entries = PreferenceConstants.getDefaultJRELibrary();
			classpathEntries.addAll(Arrays.asList(entries));
		}

		final IClasspathEntry sarlClasspathEntry = JavaCore.newContainerEntry(
				SARLClasspathContainerInitializer.CONTAINER_ID,
				new IAccessRule[0],
				new IClasspathAttribute[0],
				true);
		classpathEntries.add(sarlClasspathEntry);
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
		IWorkingSet[] selected = getSelectedWorkingSet(selection);
		assert selected != null;
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
			final PackageExplorerPart explorerPart = (PackageExplorerPart) activePart;
			if (explorerPart.getRootMode() == PackageExplorerPart.PROJECTS_AS_ROOTS) {
				// Get active filter
				final IWorkingSet filterWorkingSet = explorerPart.getFilterWorkingSet();
				if (filterWorkingSet != null && isValidWorkingSet(filterWorkingSet)) {
					selected = new IWorkingSet[] {filterWorkingSet};
				}
			} else {
				// If we have been gone into a working set return the working set
				final Object input = explorerPart.getViewPartInput();
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
			final ITreeSelection treeSelection = (ITreeSelection) selection;
			if (!treeSelection.isEmpty()) {
				workingSet = getSelectedWorkingSet(treeSelection);
			}
		}
		return workingSet;
	}

	private static IWorkingSet[] getSelectedWorkingSet(ITreeSelection treeSelection) {
		assert !treeSelection.isEmpty();
		final List<?> elements = treeSelection.toList();
		if (elements.size() == 1) {
			final Object element = elements.get(0);
			final TreePath[] paths = treeSelection.getPathsFor(element);
			if (paths.length == 1
					&& paths[0].getSegmentCount() != 0) {
				final Object candidate = paths[0].getSegment(0);
				if (candidate instanceof IWorkingSet
						&& isValidWorkingSet((IWorkingSet) candidate)) {
					return new IWorkingSet[] {(IWorkingSet) candidate};
				}
			}
		} else {
			final List<IWorkingSet> result = new ArrayList<>();
			for (Iterator<?> iterator = elements.iterator(); iterator.hasNext();) {
				final Object element = iterator.next();
				if (element instanceof IWorkingSet && isValidWorkingSet((IWorkingSet) element)) {
					result.add((IWorkingSet) element);
				}
			}
			return result.toArray(new IWorkingSet[result.size()]);
		}
		return EMPTY_WORKING_SET_ARRAY;
	}

	private static boolean isValidWorkingSet(IWorkingSet workingSet) {
		final String id = workingSet.getId();
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
	 *
	 * @mavengroupid $GroupId$
	 * @mavenartifactid $ArtifactId$
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
			final Composite nameComposite = new Composite(composite, SWT.NONE);
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
	 * @mavengroupid $GroupId$
	 * @mavenartifactid $ArtifactId$
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
			final int numColumns = 4;

			final Composite locationComposite = new Composite(composite, SWT.NONE);
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
			final IPath path = Platform.getLocation().append(name);
			return path.toOSString();
		}

		@SuppressWarnings("synthetic-access")
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

		@SuppressWarnings("synthetic-access")
		public void setLocation(IPath path) {
			this.useDefaults.setSelection(path == null);
			if (path != null) {
				this.location.setText(path.toOSString());
			} else {
				this.location.setText(getDefaultPath(MainProjectWizardPage.this.nameGroup.getName()));
			}
			fireEvent();
		}

		@SuppressWarnings("synthetic-access")
		@Override
		public void changeControlPressed(DialogField field) {
			final DirectoryDialog dialog = new DirectoryDialog(getShell());
			dialog.setMessage(NewWizardMessages.NewJavaProjectWizardPageOne_directory_message);
			String directoryName = this.location.getText().trim();
			if (directoryName.length() == 0) {
				final String prevLocation = JavaPlugin.getDefault().getDialogSettings().get(DIALOGSTORE_LAST_EXTERNAL_LOC);
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
				final String oldDirectory = new Path(this.location.getText().trim()).lastSegment();
				this.location.setText(selectedDirectory);
				final String lastSegment = new Path(selectedDirectory).lastSegment();
				if (lastSegment != null
						&& (MainProjectWizardPage.this.nameGroup.getName().length() == 0
						|| MainProjectWizardPage.this.nameGroup.getName().equals(oldDirectory))) {
					MainProjectWizardPage.this.nameGroup.setName(lastSegment);
				}
				JavaPlugin.getDefault().getDialogSettings().put(DIALOGSTORE_LAST_EXTERNAL_LOC, selectedDirectory);
			}
		}

		@SuppressWarnings("synthetic-access")
		@Override
		public void dialogFieldChanged(DialogField field) {
			if (field == this.useDefaults) {
				final boolean checked = this.useDefaults.isSelected();
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

		@SuppressWarnings("synthetic-access")
		public Control createControl(Composite composite) {
			this.group = new Group(composite, SWT.NONE);
			this.group.setFont(composite.getFont());
			this.group.setLayout(initGridLayout(new GridLayout(2, false), true));
			this.group.setText(NewWizardMessages.NewJavaProjectWizardPageOne_JREGroup_title);

			this.useEEJRE.doFillIntoGrid(this.group, 1);
			final Combo eeComboControl = this.eeCombo.getComboControl(this.group);
			eeComboControl.setLayoutData(new GridData(GridData.FILL, GridData.CENTER, true, false));

			this.useProjectJRE.doFillIntoGrid(this.group, 1);
			final Combo comboControl = this.jreCombo.getComboControl(this.group);
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
			String selectedItem = getLastSelectedJRE();
			int selectionIndex = comboField.getSelectionIndex();
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
					if (i1 instanceof IVMInstall2 && i0 instanceof IVMInstall2) {
						final String cc0 = JavaModelUtil.getCompilerCompliance((IVMInstall2) i0,
								SARLVersion.MINIMAL_JDK_VERSION_IN_SARL_PROJECT_CLASSPATH);
						final String cc1 = JavaModelUtil.getCompilerCompliance((IVMInstall2) i1,
								SARLVersion.MINIMAL_JDK_VERSION_IN_SARL_PROJECT_CLASSPATH);
						final int result = cc1.compareTo(cc0);
						if (result != 0) {
							return result;
						}
					}
					return Policy.getComparator().compare(i0.getName(), i1.getName());
				}

			});
			// find new index
			selectionIndex = -1;
			final String[] jreLabels = new String[this.installedJVMs.length];
			this.jreCompliance = new String[this.installedJVMs.length];
			for (int i = 0; i < this.installedJVMs.length; i++) {
				jreLabels[i] = this.installedJVMs[i].getName();
				if (selectedItem != null && jreLabels[i].equals(selectedItem)) {
					selectionIndex = i;
				}
				if (this.installedJVMs[i] instanceof IVMInstall2) {
					this.jreCompliance[i] = JavaModelUtil.getCompilerCompliance(
							(IVMInstall2) this.installedJVMs[i],
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
			String selectedItem = getLastSelectedEE();
			int selectionIndex = comboField.getSelectionIndex();
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
			final String[] eeLabels = new String[this.installedEEs.length];
			this.eeCompliance = new String[this.installedEEs.length];
			for (int i = 0; i < this.installedEEs.length; i++) {
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
			final List<VMStandin> standins = new ArrayList<>();
			final IVMInstallType[] types = JavaRuntime.getVMInstallTypes();
			for (int i = 0; i < types.length; i++) {
				final IVMInstallType type = types[i];
				final IVMInstall[] installs = type.getVMInstalls();
				for (int j = 0; j < installs.length; j++) {
					final IVMInstall install = installs[j];
					standins.add(new VMStandin(install));
				}
			}
			return standins.toArray(new IVMInstall[standins.size()]);
		}

		private String getDefaultJVMName() {
			final IVMInstall install = JavaRuntime.getDefaultVMInstall();
			if (install != null) {
				return install.getName();
			}
			return NewWizardMessages.NewJavaProjectWizardPageOne_UnknownDefaultJRE_name;
		}

		private String getDefaultEEName() {
			final IVMInstall defaultVM = JavaRuntime.getDefaultVMInstall();

			final IExecutionEnvironment[] environments = JavaRuntime.getExecutionEnvironmentsManager()
					.getExecutionEnvironments();
			if (defaultVM != null) {
				for (int i = 0; i < environments.length; i++) {
					final IVMInstall eeDefaultVM = environments[i].getDefaultVM();
					if (eeDefaultVM != null && defaultVM.getId().equals(eeDefaultVM.getId())) {
						return environments[i].getId();
					}
				}
			}

			final String defaultCC;
			if (defaultVM instanceof IVMInstall2) {
				defaultCC = JavaModelUtil.getCompilerCompliance((IVMInstall2) defaultVM, SARLVersion.MINIMAL_JDK_VERSION_IN_SARL_PROJECT_CLASSPATH);
			} else {
				defaultCC = SARLVersion.MINIMAL_JDK_VERSION_IN_SARL_PROJECT_CLASSPATH;
			}

			for (int i = 0; i < environments.length; i++) {
				final String eeCompliance = JavaModelUtil.getExecutionEnvironmentCompliance(environments[i]);
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

		@SuppressWarnings("synthetic-access")
		private void updateEnableState() {
			final boolean detect = MainProjectWizardPage.this.detectGroup.mustDetect();
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

		@SuppressWarnings("synthetic-access")
		@Override
		public void widgetDefaultSelected(SelectionEvent event) {
			final String jreID = BuildPathSupport.JRE_PREF_PAGE_ID;
			final String eeID = BuildPathSupport.EE_PREF_PAGE_ID;
			final String complianceId = CompliancePreferencePage.PREF_ID;
			final Map<String, Boolean> data = new HashMap<>();
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

		@SuppressWarnings("synthetic-access")
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
			final int index = combo.getSelectionIndex();
			if (index == -1) {
				return;
			}

			final String item = combo.getItems()[index];
			JavaPlugin.getDefault().getDialogSettings().put(preferenceKey, item);
		}

		private int getLastSelectedJREKind() {
			final IDialogSettings settings = JavaPlugin.getDefault().getDialogSettings();
			if (settings.get(LAST_SELECTED_JRE_KIND2) == null) {
				return EE_JRE;
			}

			return settings.getInt(LAST_SELECTED_JRE_KIND2);
		}

		private String getLastSelectedEE() {
			final IDialogSettings settings = JavaPlugin.getDefault().getDialogSettings();
			return settings.get(LAST_SELECTED_EE_SETTINGS_KEY);
		}

		private String getLastSelectedJRE() {
			final IDialogSettings settings = JavaPlugin.getDefault().getDialogSettings();
			return settings.get(LAST_SELECTED_JRE_SETTINGS_KEY);
		}

		public IVMInstall getSelectedJVM() {
			if (this.useProjectJRE.isSelected()) {
				final int index = this.jreCombo.getSelectionIndex();
				if (index >= 0 && index < this.installedJVMs.length) {
					// paranoia
					return this.installedJVMs[index];
				}
			}
			return null;
		}

		public IPath getJREContainerPath() {
			if (this.useProjectJRE.isSelected()) {
				final int index = this.jreCombo.getSelectionIndex();
				if (index >= 0 && index < this.installedJVMs.length) {
					// paranoia
					return JavaRuntime.newJREContainerPath(this.installedJVMs[index]);
				}
			} else if (this.useEEJRE.isSelected()) {
				final int index = this.eeCombo.getSelectionIndex();
				if (index >= 0 && index < this.installedEEs.length) {
					// paranoia
					return JavaRuntime.newJREContainerPath(this.installedEEs[index]);
				}
			}
			return null;
		}

		public String getSelectedCompilerCompliance() {
			if (this.useProjectJRE.isSelected()) {
				final int index = this.jreCombo.getSelectionIndex();
				if (index >= 0 && index < this.jreCompliance.length) {
					// paranoia
					return this.jreCompliance[index];
				}
			} else if (this.useEEJRE.isSelected()) {
				final int index = this.eeCombo.getSelectionIndex();
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
	 * @author $Author: ngaud$
	 * @author $Author: sgalland$
	 * @version $FullVersion$
	 * @mavengroupid $GroupId$
	 * @mavenartifactid $ArtifactId$
	 */
	private final class WorkingSetGroup {

		private WorkingSetConfigurationBlock workingSetBlock;

		WorkingSetGroup() {
			final String[] workingSetIds = new String[] {IWorkingSetIDs.JAVA, IWorkingSetIDs.RESOURCE};
			this.workingSetBlock = new WorkingSetConfigurationBlock(workingSetIds, JavaPlugin.getDefault().getDialogSettings());
			// fWorkingSetBlock.setDialogMessage(NewWizardMessages.NewJavaProjectWizardPageOne_WorkingSetSelection_message);
		}

		public Control createControl(Composite composite) {
			final Group workingSetGroup = new Group(composite, SWT.NONE);
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
	 * @author $Author: ngaud$
	 * @author $Author: sgalland$
	 * @version $FullVersion$
	 * @mavengroupid $GroupId$
	 * @mavenartifactid $ArtifactId$
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

			final Composite composite = new Composite(parent, SWT.NONE);
			composite.setLayoutData(new GridData(SWT.FILL, SWT.TOP, true, false));
			final GridLayout layout = new GridLayout(2, false);
			layout.horizontalSpacing = HORIZONTAL_SPACING;
			composite.setLayout(layout);

			this.icon = new Label(composite, SWT.LEFT);
			this.icon.setImage(Dialog.getImage(Dialog.DLG_IMG_MESSAGE_WARNING));
			GridData gridData = new GridData(SWT.LEFT, SWT.TOP, false, false);
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

		@SuppressWarnings("synthetic-access")
		public void handlePossibleJVMChange() {

			if (JavaRuntime.getDefaultVMInstall() == null) {
				this.fHintText.setText(NewWizardMessages.NewJavaProjectWizardPageOne_NoJREFound_link);
				this.fHintText.setVisible(true);
				this.icon.setImage(Dialog.getImage(Dialog.DLG_IMG_MESSAGE_WARNING));
				this.icon.setVisible(true);
				return;
			}

			String selectedCompliance = MainProjectWizardPage.this.jreGroup.getSelectedCompilerCompliance();
			if (selectedCompliance != null) {
				final String defaultCompliance = JavaCore.getOption(JavaCore.COMPILER_COMPLIANCE);
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
			IVMInstall selectedJVM = MainProjectWizardPage.this.jreGroup.getSelectedJVM();
			if (selectedJVM == null) {
				selectedJVM = JavaRuntime.getDefaultVMInstall();
			}
			String jvmCompliance = SARLVersion.MINIMAL_JDK_VERSION_IN_SARL_PROJECT_CLASSPATH;
			if (selectedJVM instanceof IVMInstall2) {
				jvmCompliance = JavaModelUtil.getCompilerCompliance((IVMInstall2) selectedJVM,
						SARLVersion.MINIMAL_JDK_VERSION_IN_SARL_PROJECT_CLASSPATH);
			}
			if (!selectedCompliance.equals(jvmCompliance)
					&& (JavaModelUtil.is50OrHigher(selectedCompliance)
					|| JavaModelUtil.is50OrHigher(jvmCompliance))) {
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

		@SuppressWarnings("synthetic-access")
		private boolean computeDetectState() {
			if (MainProjectWizardPage.this.locationGroup.isUseDefaultSelected()) {
				final String name = MainProjectWizardPage.this.nameGroup.getName();
				if (name.length() == 0 || JavaPlugin.getWorkspace().getRoot().findMember(name) != null) {
					return false;
				}

				final File directory = MainProjectWizardPage.this.locationGroup.getLocation().append(name).toFile();
				return directory.isDirectory();
			}

			final File directory = MainProjectWizardPage.this.locationGroup.getLocation().toFile();
			return directory.isDirectory();
		}

		@Override
		public void update(Observable observable, Object arg) {
			if (observable instanceof LocationGroup) {
				final boolean oldDetectState = this.fDetect;
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

		@SuppressWarnings("synthetic-access")
		@Override
		public void widgetDefaultSelected(SelectionEvent event) {
			final String jreID = BuildPathSupport.JRE_PREF_PAGE_ID;
			final String eeID = BuildPathSupport.EE_PREF_PAGE_ID;
			final String complianceId = CompliancePreferencePage.PREF_ID;
			final Map<String, Boolean> data = new HashMap<>();
			data.put(PropertyAndPreferencePage.DATA_NO_LINK, Boolean.TRUE);
			final String id = "JRE".equals(event.text) ? jreID : complianceId; //$NON-NLS-1$
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
	 * @author $Author: ngaud$
	 * @author $Author: sgalland$
	 * @version $FullVersion$
	 * @mavengroupid $GroupId$
	 * @mavenartifactid $ArtifactId$
	 */
	private final class Validator implements Observer {

		@SuppressWarnings("synthetic-access")
		@Override
		public void update(Observable observable, Object arg) {
			try {
				final IWorkspace workspace = JavaPlugin.getWorkspace();
				final String name = MainProjectWizardPage.this.nameGroup.getName();
				checkProjectName(workspace, name);
				final IProject handle = checkProjectExist(workspace, name);
				final String location = MainProjectWizardPage.this.locationGroup.getLocation().toOSString();
				final IPath projectPath = checkLocationSyntax(location);
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
			final IStatus nameStatus = workspace.validateName(name, IResource.PROJECT);
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
		@SuppressWarnings("synthetic-access")
		private IProject checkProjectExist(IWorkspace workspace, String name) throws ValidationException {
			final IProject handle = workspace.getRoot().getProject(name);
			if (handle.exists()) {
				throw new ValidationException(
						null,
						NewWizardMessages.NewJavaProjectWizardPageOne_Message_projectAlreadyExists);
			}

			IPath projectLocation = ResourcesPlugin.getWorkspace().getRoot().getLocation().append(name);
			if (projectLocation.toFile().exists()) {
				try {
					// correct casing
					final String canonicalPath = projectLocation.toFile().getCanonicalPath();
					projectLocation = new Path(canonicalPath);
				} catch (IOException e) {
					SARLEclipsePlugin.getDefault().log(e);
				}

				final String existingName = projectLocation.lastSegment();
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
			final IStatus locationStatus = workspace.validateProjectLocation(handle, projectPath);
			if (!locationStatus.isOK()) {
				throw new ValidationException(
						null,
						locationStatus.getMessage());
			}
		}

		private boolean canCreate(File file) {
			File fileInHierarchy = file;
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
		 * @author $Author: sgalland$
		 * @version $FullVersion$
		 * @mavengroupid $GroupId$
		 * @mavenartifactid $ArtifactId$
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

}
