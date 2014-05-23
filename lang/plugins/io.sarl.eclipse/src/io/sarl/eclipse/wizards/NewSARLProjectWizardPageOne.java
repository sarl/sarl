package io.sarl.eclipse.wizards;

import java.io.File;
import java.io.IOException;
import java.net.MalformedURLException;
import java.net.URI;
import java.net.URL;
import java.util.ArrayList;
import java.util.Arrays;
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
import org.eclipse.core.runtime.FileLocator;
import org.eclipse.core.runtime.IPath;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Path;
import org.eclipse.core.runtime.Platform;
import org.eclipse.equinox.bidi.StructuredTextTypeHandlerFactory;
import org.eclipse.jdt.core.IClasspathEntry;
import org.eclipse.jdt.core.JavaCore;
import org.eclipse.jdt.internal.corext.util.JavaModelUtil;
import org.eclipse.jdt.internal.corext.util.Messages;
import org.eclipse.jdt.internal.ui.IJavaHelpContextIds;
import org.eclipse.jdt.internal.ui.JavaPlugin;
import org.eclipse.jdt.internal.ui.packageview.PackageExplorerPart;
import org.eclipse.jdt.internal.ui.preferences.CompliancePreferencePage;
import org.eclipse.jdt.internal.ui.preferences.NewJavaProjectPreferencePage;
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
import org.osgi.framework.Bundle;

/**
 * The first page of the SARL new project wizard. Most part of the code of this class is copy/paste from {@link NewJavaProjectWizardPageOne}
 * 
 * This version removes the choice of the project structure And update the structure of the source folder of the project {@link NewSARLProjectWizardPageOne#getSourceClasspathEntries()} TODO and must add the selection of the JVM and proposes Janus as default Agent JVM.
 * 
 * @author $Author: ngaud$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 */
@SuppressWarnings("restriction")
public class NewSARLProjectWizardPageOne extends WizardPage {

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

		/*
		 * (non-Javadoc)
		 * 
		 * @see org.eclipse.jdt.internal.ui.wizards.dialogfields.IDialogFieldListener#dialogFieldChanged(org.eclipse.jdt.internal.ui.wizards.dialogfields.DialogField)
		 */
		public void dialogFieldChanged(DialogField field) {
			fireEvent();
		}
	}

	/**
	 * Request a location. Fires an event whenever the checkbox or the location field is changed, regardless of whether the change originates from the user or has been invoked programmatically.
	 */
	private final class LocationGroup extends Observable implements Observer, IStringButtonAdapter, IDialogFieldListener {

		protected final SelectionButtonDialogField fUseDefaults;
		protected final StringButtonDialogField fLocation;

		private String fPreviousExternalLocation;

		private static final String DIALOGSTORE_LAST_EXTERNAL_LOC = JavaUI.ID_PLUGIN + ".last.external.project"; //$NON-NLS-1$

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

		/*
		 * (non-Javadoc)
		 * 
		 * @see java.util.Observer#update(java.util.Observable, java.lang.Object)
		 */
		public void update(Observable o, Object arg) {
			if (isUseDefaultSelected()) {
				this.fLocation.setText(getDefaultPath(NewSARLProjectWizardPageOne.this.fNameGroup.getName()));
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

		public void setLocation(IPath path) {
			this.fUseDefaults.setSelection(path == null);
			if (path != null) {
				this.fLocation.setText(path.toOSString());
			} else {
				this.fLocation.setText(getDefaultPath(NewSARLProjectWizardPageOne.this.fNameGroup.getName()));
			}
			fireEvent();
		}

		/*
		 * (non-Javadoc)
		 * 
		 * @see org.eclipse.jdt.internal.ui.wizards.dialogfields.IStringButtonAdapter#changeControlPressed(org.eclipse.jdt.internal.ui.wizards.dialogfields.DialogField)
		 */
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
				if (path.exists())
					dialog.setFilterPath(directoryName);
			}
			final String selectedDirectory = dialog.open();
			if (selectedDirectory != null) {
				String oldDirectory = new Path(this.fLocation.getText().trim()).lastSegment();
				this.fLocation.setText(selectedDirectory);
				String lastSegment = new Path(selectedDirectory).lastSegment();
				if (lastSegment != null && (NewSARLProjectWizardPageOne.this.fNameGroup.getName().length() == 0 || NewSARLProjectWizardPageOne.this.fNameGroup.getName().equals(oldDirectory))) {
					NewSARLProjectWizardPageOne.this.fNameGroup.setName(lastSegment);
				}
				JavaPlugin.getDefault().getDialogSettings().put(DIALOGSTORE_LAST_EXTERNAL_LOC, selectedDirectory);
			}
		}

		/*
		 * (non-Javadoc)
		 * 
		 * @see org.eclipse.jdt.internal.ui.wizards.dialogfields.IDialogFieldListener#dialogFieldChanged(org.eclipse.jdt.internal.ui.wizards.dialogfields.DialogField)
		 */
		public void dialogFieldChanged(DialogField field) {
			if (field == this.fUseDefaults) {
				final boolean checked = this.fUseDefaults.isSelected();
				if (checked) {
					this.fPreviousExternalLocation = this.fLocation.getText();
					this.fLocation.setText(getDefaultPath(NewSARLProjectWizardPageOne.this.fNameGroup.getName()));
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
	 * Request a project layout.
	 */
	private final class LayoutGroup implements Observer, SelectionListener {

		private final SelectionButtonDialogField fStdRadio, fSrcBinRadio;
		private Group fGroup;
		private Link fPreferenceLink;

		public LayoutGroup() {
			this.fStdRadio = new SelectionButtonDialogField(SWT.RADIO);
			this.fStdRadio.setLabelText(NewWizardMessages.NewJavaProjectWizardPageOne_LayoutGroup_option_oneFolder);

			this.fSrcBinRadio = new SelectionButtonDialogField(SWT.RADIO);
			this.fSrcBinRadio.setLabelText(NewWizardMessages.NewJavaProjectWizardPageOne_LayoutGroup_option_separateFolders);

			boolean useSrcBin = PreferenceConstants.getPreferenceStore().getBoolean(PreferenceConstants.SRCBIN_FOLDERS_IN_NEWPROJ);
			this.fSrcBinRadio.setSelection(useSrcBin);
			this.fStdRadio.setSelection(!useSrcBin);
		}

		public Control createContent(Composite composite) {
			this.fGroup = new Group(composite, SWT.NONE);
			this.fGroup.setFont(composite.getFont());
			this.fGroup.setLayout(initGridLayout(new GridLayout(3, false), true));
			this.fGroup.setText(NewWizardMessages.NewJavaProjectWizardPageOne_LayoutGroup_title);

			this.fStdRadio.doFillIntoGrid(this.fGroup, 3);
			LayoutUtil.setHorizontalGrabbing(this.fStdRadio.getSelectionButton(null));

			this.fSrcBinRadio.doFillIntoGrid(this.fGroup, 2);

			this.fPreferenceLink = new Link(this.fGroup, SWT.NONE);
			this.fPreferenceLink.setText(NewWizardMessages.NewJavaProjectWizardPageOne_LayoutGroup_link_description);
			this.fPreferenceLink.setLayoutData(new GridData(GridData.END, GridData.END, false, false));
			this.fPreferenceLink.addSelectionListener(this);

			updateEnableState();
			return this.fGroup;
		}

		/*
		 * (non-Javadoc)
		 * 
		 * @see java.util.Observer#update(java.util.Observable, java.lang.Object)
		 */
		public void update(Observable o, Object arg) {
			updateEnableState();
		}

		private void updateEnableState() {
			if (NewSARLProjectWizardPageOne.this.fDetectGroup == null)
				return;

			final boolean detect = NewSARLProjectWizardPageOne.this.fDetectGroup.mustDetect();
			this.fStdRadio.setEnabled(!detect);
			this.fSrcBinRadio.setEnabled(!detect);
			if (this.fPreferenceLink != null) {
				this.fPreferenceLink.setEnabled(!detect);
			}
			if (this.fGroup != null) {
				this.fGroup.setEnabled(!detect);
			}
		}

		/**
		 * Return <code>true</code> if the user specified to create 'source' and 'bin' folders.
		 * 
		 * @return returns <code>true</code> if the user specified to create 'source' and 'bin' folders.
		 */
		public boolean isSrcBin() {
			return this.fSrcBinRadio.isSelected();
		}

		/*
		 * (non-Javadoc)
		 * 
		 * @see org.eclipse.swt.events.SelectionListener#widgetSelected(org.eclipse.swt.events.SelectionEvent)
		 */
		public void widgetSelected(SelectionEvent e) {
			widgetDefaultSelected(e);
		}

		/*
		 * (non-Javadoc)
		 * 
		 * @see org.eclipse.swt.events.SelectionListener#widgetDefaultSelected(org.eclipse.swt.events.SelectionEvent)
		 */
		public void widgetDefaultSelected(SelectionEvent e) {
			String id = NewJavaProjectPreferencePage.ID;
			PreferencesUtil.createPreferenceDialogOn(getShell(), id, new String[] { id }, null).open();
			NewSARLProjectWizardPageOne.this.fDetectGroup.handlePossibleJVMChange();
			NewSARLProjectWizardPageOne.this.fJREGroup.handlePossibleJVMChange();
		}
	}

	private final class JREGroup implements Observer, SelectionListener, IDialogFieldListener {

		private static final String LAST_SELECTED_EE_SETTINGS_KEY = JavaUI.ID_PLUGIN + ".last.selected.execution.enviroment"; //$NON-NLS-1$
		private static final String LAST_SELECTED_JRE_SETTINGS_KEY = JavaUI.ID_PLUGIN + ".last.selected.project.jre"; //$NON-NLS-1$
		// private static final String LAST_SELECTED_JRE_KIND= JavaUI.ID_PLUGIN + ".last.selected.jre.kind"; // used before EE became default
		private static final String LAST_SELECTED_JRE_KIND2 = JavaUI.ID_PLUGIN + ".last.selected.jre.kind2"; //$NON-NLS-1$

		private static final int DEFAULT_JRE = 0;
		private static final int PROJECT_JRE = 1;
		private static final int EE_JRE = 2;

		private final SelectionButtonDialogField fUseDefaultJRE, fUseProjectJRE, fUseEEJRE;
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
			}

			this.fJRECombo.setEnabled(this.fUseProjectJRE.isSelected());
			this.fEECombo.setEnabled(this.fUseEEJRE.isSelected());

			this.fUseDefaultJRE.setDialogFieldListener(this);
			this.fUseProjectJRE.setDialogFieldListener(this);
			this.fUseEEJRE.setDialogFieldListener(this);
		}

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
				if (selectionIndex != -1) {// paranoia
					selectedItem = comboField.getItems()[selectionIndex];
				}
			}

			this.fInstalledJVMs = getWorkspaceJREs();
			Arrays.sort(this.fInstalledJVMs, new Comparator<IVMInstall>() {

				public int compare(IVMInstall i0, IVMInstall i1) {
					if (i1 instanceof IVMInstall2 && i0 instanceof IVMInstall2) {
						String cc0 = JavaModelUtil.getCompilerCompliance((IVMInstall2) i0, JavaCore.VERSION_1_4);
						String cc1 = JavaModelUtil.getCompilerCompliance((IVMInstall2) i1, JavaCore.VERSION_1_4);
						int result = cc1.compareTo(cc0);
						if (result != 0)
							return result;
					}
					return Policy.getComparator().compare(i0.getName(), i1.getName());
				}

			});
			selectionIndex = -1;// find new index
			String[] jreLabels = new String[this.fInstalledJVMs.length];
			this.fJRECompliance = new String[this.fInstalledJVMs.length];
			for (int i = 0; i < this.fInstalledJVMs.length; i++) {
				jreLabels[i] = this.fInstalledJVMs[i].getName();
				if (selectedItem != null && jreLabels[i].equals(selectedItem)) {
					selectionIndex = i;
				}
				if (this.fInstalledJVMs[i] instanceof IVMInstall2) {
					this.fJRECompliance[i] = JavaModelUtil.getCompilerCompliance((IVMInstall2) this.fInstalledJVMs[i], JavaCore.VERSION_1_4);
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
				if (selectionIndex != -1) {// paranoia
					selectedItem = comboField.getItems()[selectionIndex];
				}
			}

			this.fInstalledEEs = JavaRuntime.getExecutionEnvironmentsManager().getExecutionEnvironments();
			Arrays.sort(this.fInstalledEEs, new Comparator<IExecutionEnvironment>() {
				public int compare(IExecutionEnvironment arg0, IExecutionEnvironment arg1) {
					return Policy.getComparator().compare(arg0.getId(), arg1.getId());
				}
			});
			selectionIndex = -1;// find new index
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
			List<VMStandin> standins = new ArrayList<VMStandin>();
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
			} else {
				return NewWizardMessages.NewJavaProjectWizardPageOne_UnknownDefaultJRE_name;
			}
		}

		private String getDefaultEEName() {
			IVMInstall defaultVM = JavaRuntime.getDefaultVMInstall();

			IExecutionEnvironment[] environments = JavaRuntime.getExecutionEnvironmentsManager().getExecutionEnvironments();
			if (defaultVM != null) {
				for (int i = 0; i < environments.length; i++) {
					IVMInstall eeDefaultVM = environments[i].getDefaultVM();
					if (eeDefaultVM != null && defaultVM.getId().equals(eeDefaultVM.getId()))
						return environments[i].getId();
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
				if (defaultCC.endsWith(eeCompliance))
					return environments[i].getId();
			}

			return "JavaSE-1.6"; //$NON-NLS-1$
		}

		private String getDefaultJVMLabel() {
			return Messages.format(NewWizardMessages.NewJavaProjectWizardPageOne_JREGroup_default_compliance, getDefaultJVMName());
		}

		public void update(Observable o, Object arg) {
			updateEnableState();
		}

		private void updateEnableState() {
			final boolean detect = NewSARLProjectWizardPageOne.this.fDetectGroup.mustDetect();
			this.fUseDefaultJRE.setEnabled(!detect);
			this.fUseProjectJRE.setEnabled(!detect);
			this.fUseEEJRE.setEnabled(!detect);
			this.fJRECombo.setEnabled(!detect && this.fUseProjectJRE.isSelected());
			this.fEECombo.setEnabled(!detect && this.fUseEEJRE.isSelected());
			if (this.fPreferenceLink != null) {
				this.fPreferenceLink.setEnabled(!detect);
			}
			if (this.fGroup != null) {
				fGroup.setEnabled(!detect);
			}
		}

		/*
		 * (non-Javadoc)
		 * 
		 * @see org.eclipse.swt.events.SelectionListener#widgetSelected(org.eclipse.swt.events.SelectionEvent)
		 */
		public void widgetSelected(SelectionEvent e) {
			widgetDefaultSelected(e);
		}

		/*
		 * (non-Javadoc)
		 * 
		 * @see org.eclipse.swt.events.SelectionListener#widgetDefaultSelected(org.eclipse.swt.events.SelectionEvent)
		 */
		public void widgetDefaultSelected(SelectionEvent e) {
			String jreID = BuildPathSupport.JRE_PREF_PAGE_ID;
			String eeID = BuildPathSupport.EE_PREF_PAGE_ID;
			String complianceId = CompliancePreferencePage.PREF_ID;
			Map<String, Boolean> data = new HashMap<String, Boolean>();
			data.put(PropertyAndPreferencePage.DATA_NO_LINK, Boolean.TRUE);
			PreferencesUtil.createPreferenceDialogOn(getShell(), jreID, new String[] { jreID, complianceId, eeID }, data).open();

			handlePossibleJVMChange();
			fDetectGroup.handlePossibleJVMChange();
		}

		public void handlePossibleJVMChange() {
			fUseDefaultJRE.setLabelText(getDefaultJVMLabel());
			fillInstalledJREs(fJRECombo);
			fillExecutionEnvironments(fEECombo);
		}

		/*
		 * (non-Javadoc)
		 * 
		 * @see org.eclipse.jdt.internal.ui.wizards.dialogfields.IDialogFieldListener#dialogFieldChanged(org.eclipse.jdt.internal.ui.wizards.dialogfields.DialogField)
		 */
		public void dialogFieldChanged(DialogField field) {
			updateEnableState();
			fDetectGroup.handlePossibleJVMChange();
			if (field == fJRECombo) {
				if (fUseProjectJRE.isSelected()) {
					storeSelectionValue(fJRECombo, LAST_SELECTED_JRE_SETTINGS_KEY);
				}
			} else if (field == fEECombo) {
				if (fUseEEJRE.isSelected()) {
					storeSelectionValue(fEECombo, LAST_SELECTED_EE_SETTINGS_KEY);
				}
			} else if (field == fUseDefaultJRE) {
				if (fUseDefaultJRE.isSelected()) {
					JavaPlugin.getDefault().getDialogSettings().put(LAST_SELECTED_JRE_KIND2, DEFAULT_JRE);
					fUseProjectJRE.setSelection(false);
					fUseEEJRE.setSelection(false);
				}
			} else if (field == fUseProjectJRE) {
				if (fUseProjectJRE.isSelected()) {
					JavaPlugin.getDefault().getDialogSettings().put(LAST_SELECTED_JRE_KIND2, PROJECT_JRE);
					fUseDefaultJRE.setSelection(false);
					fUseEEJRE.setSelection(false);
				}
			} else if (field == fUseEEJRE) {
				if (fUseEEJRE.isSelected()) {
					JavaPlugin.getDefault().getDialogSettings().put(LAST_SELECTED_JRE_KIND2, EE_JRE);
					fUseDefaultJRE.setSelection(false);
					fUseProjectJRE.setSelection(false);
				}
			}
		}

		private void storeSelectionValue(ComboDialogField combo, String preferenceKey) {
			int index = combo.getSelectionIndex();
			if (index == -1)
				return;

			String item = combo.getItems()[index];
			JavaPlugin.getDefault().getDialogSettings().put(preferenceKey, item);
		}

		private int getLastSelectedJREKind() {
			IDialogSettings settings = JavaPlugin.getDefault().getDialogSettings();
			if (settings.get(LAST_SELECTED_JRE_KIND2) == null)
				return EE_JRE;

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
			if (fUseProjectJRE.isSelected()) {
				int index = fJRECombo.getSelectionIndex();
				if (index >= 0 && index < fInstalledJVMs.length) { // paranoia
					return fInstalledJVMs[index];
				}
			} else if (fUseEEJRE.isSelected()) {

			}
			return null;
		}

		public IPath getJREContainerPath() {
			if (fUseProjectJRE.isSelected()) {
				int index = fJRECombo.getSelectionIndex();
				if (index >= 0 && index < fInstalledJVMs.length) { // paranoia
					return JavaRuntime.newJREContainerPath(fInstalledJVMs[index]);
				}
			} else if (fUseEEJRE.isSelected()) {
				int index = fEECombo.getSelectionIndex();
				if (index >= 0 && index < fInstalledEEs.length) { // paranoia
					return JavaRuntime.newJREContainerPath(fInstalledEEs[index]);
				}
			}
			return null;
		}

		public String getSelectedCompilerCompliance() {
			if (fUseProjectJRE.isSelected()) {
				int index = fJRECombo.getSelectionIndex();
				if (index >= 0 && index < fJRECompliance.length) { // paranoia
					return fJRECompliance[index];
				}
			} else if (fUseEEJRE.isSelected()) {
				int index = fEECombo.getSelectionIndex();
				if (index >= 0 && index < fEECompliance.length) { // paranoia
					return fEECompliance[index];
				}
			}
			return null;
		}
	}

	private final class WorkingSetGroup {

		private WorkingSetConfigurationBlock fWorkingSetBlock;

		public WorkingSetGroup() {
			String[] workingSetIds = new String[] { IWorkingSetIDs.JAVA, IWorkingSetIDs.RESOURCE };
			fWorkingSetBlock = new WorkingSetConfigurationBlock(workingSetIds, JavaPlugin.getDefault().getDialogSettings());
			// fWorkingSetBlock.setDialogMessage(NewWizardMessages.NewJavaProjectWizardPageOne_WorkingSetSelection_message);
		}

		public Control createControl(Composite composite) {
			Group workingSetGroup = new Group(composite, SWT.NONE);
			workingSetGroup.setFont(composite.getFont());
			workingSetGroup.setText(NewWizardMessages.NewJavaProjectWizardPageOne_WorkingSets_group);
			workingSetGroup.setLayout(new GridLayout(1, false));

			fWorkingSetBlock.createContent(workingSetGroup);

			return workingSetGroup;
		}

		public void setWorkingSets(IWorkingSet[] workingSets) {
			fWorkingSetBlock.setWorkingSets(workingSets);
		}

		public IWorkingSet[] getSelectedWorkingSets() {
			return fWorkingSetBlock.getSelectedWorkingSets();
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
			fDetect = false;
		}

		public Control createControl(Composite parent) {

			Composite composite = new Composite(parent, SWT.NONE);
			composite.setLayoutData(new GridData(SWT.FILL, SWT.TOP, true, false));
			GridLayout layout = new GridLayout(2, false);
			layout.horizontalSpacing = 10;
			composite.setLayout(layout);

			fIcon = new Label(composite, SWT.LEFT);
			fIcon.setImage(Dialog.getImage(Dialog.DLG_IMG_MESSAGE_WARNING));
			GridData gridData = new GridData(SWT.LEFT, SWT.TOP, false, false);
			fIcon.setLayoutData(gridData);

			fHintText = new Link(composite, SWT.WRAP);
			fHintText.setFont(composite.getFont());
			fHintText.addSelectionListener(this);
			gridData = new GridData(GridData.FILL, SWT.FILL, true, true);
			gridData.widthHint = convertWidthInCharsToPixels(50);
			gridData.heightHint = convertHeightInCharsToPixels(3);
			fHintText.setLayoutData(gridData);

			handlePossibleJVMChange();
			return composite;
		}

		public void handlePossibleJVMChange() {

			if (JavaRuntime.getDefaultVMInstall() == null) {
				fHintText.setText(NewWizardMessages.NewJavaProjectWizardPageOne_NoJREFound_link);
				fHintText.setVisible(true);
				fIcon.setImage(Dialog.getImage(Dialog.DLG_IMG_MESSAGE_WARNING));
				fIcon.setVisible(true);
				return;
			}

			String selectedCompliance = fJREGroup.getSelectedCompilerCompliance();
			if (selectedCompliance != null) {
				String defaultCompliance = JavaCore.getOption(JavaCore.COMPILER_COMPLIANCE);
				if (selectedCompliance.equals(defaultCompliance)) {
					fHintText.setVisible(false);
					fIcon.setVisible(false);
				} else {
					fHintText.setText(Messages.format(NewWizardMessages.NewJavaProjectWizardPageOne_DetectGroup_differendWorkspaceCC_message, new String[] { BasicElementLabels.getVersionName(defaultCompliance), BasicElementLabels.getVersionName(selectedCompliance) }));
					fHintText.setVisible(true);
					fIcon.setImage(Dialog.getImage(Dialog.DLG_IMG_MESSAGE_INFO));
					fIcon.setVisible(true);
				}
				return;
			}

			selectedCompliance = JavaCore.getOption(JavaCore.COMPILER_COMPLIANCE);
			IVMInstall selectedJVM = fJREGroup.getSelectedJVM();
			if (selectedJVM == null) {
				selectedJVM = JavaRuntime.getDefaultVMInstall();
			}
			String jvmCompliance = JavaCore.VERSION_1_4;
			if (selectedJVM instanceof IVMInstall2) {
				jvmCompliance = JavaModelUtil.getCompilerCompliance((IVMInstall2) selectedJVM, JavaCore.VERSION_1_4);
			}
			if (!selectedCompliance.equals(jvmCompliance) && (JavaModelUtil.is50OrHigher(selectedCompliance) || JavaModelUtil.is50OrHigher(jvmCompliance))) {
				fHintText.setText(Messages.format(NewWizardMessages.NewJavaProjectWizardPageOne_DetectGroup_jre_message, new String[] { BasicElementLabels.getVersionName(selectedCompliance), BasicElementLabels.getVersionName(jvmCompliance) }));
				fHintText.setVisible(true);
				fIcon.setImage(Dialog.getImage(Dialog.DLG_IMG_MESSAGE_WARNING));
				fIcon.setVisible(true);
			} else {
				fHintText.setVisible(false);
				fIcon.setVisible(false);
			}

		}

		private boolean computeDetectState() {
			if (fLocationGroup.isUseDefaultSelected()) {
				String name = fNameGroup.getName();
				if (name.length() == 0 || JavaPlugin.getWorkspace().getRoot().findMember(name) != null) {
					return false;
				} else {
					final File directory = fLocationGroup.getLocation().append(name).toFile();
					return directory.isDirectory();
				}
			} else {
				final File directory = fLocationGroup.getLocation().toFile();
				return directory.isDirectory();
			}
		}

		public void update(Observable o, Object arg) {
			if (o instanceof LocationGroup) {
				boolean oldDetectState = fDetect;
				fDetect = computeDetectState();

				if (oldDetectState != fDetect) {
					setChanged();
					notifyObservers();

					if (fDetect) {
						fHintText.setVisible(true);
						fHintText.setText(NewWizardMessages.NewJavaProjectWizardPageOne_DetectGroup_message);
						fIcon.setImage(Dialog.getImage(Dialog.DLG_IMG_MESSAGE_INFO));
						fIcon.setVisible(true);
					} else {
						handlePossibleJVMChange();
					}
				}
			}
		}

		public boolean mustDetect() {
			return fDetect;
		}

		/*
		 * (non-Javadoc)
		 * 
		 * @see org.eclipse.swt.events.SelectionListener#widgetSelected(org.eclipse.swt.events.SelectionEvent)
		 */
		public void widgetSelected(SelectionEvent e) {
			widgetDefaultSelected(e);
		}

		/*
		 * (non-Javadoc)
		 * 
		 * @see org.eclipse.swt.events.SelectionListener#widgetDefaultSelected(org.eclipse.swt.events.SelectionEvent)
		 */
		public void widgetDefaultSelected(SelectionEvent e) {
			String jreID = BuildPathSupport.JRE_PREF_PAGE_ID;
			String eeID = BuildPathSupport.EE_PREF_PAGE_ID;
			String complianceId = CompliancePreferencePage.PREF_ID;
			Map<String, Boolean> data = new HashMap<String, Boolean>();
			data.put(PropertyAndPreferencePage.DATA_NO_LINK, Boolean.TRUE);
			String id = "JRE".equals(e.text) ? jreID : complianceId; //$NON-NLS-1$
			PreferencesUtil.createPreferenceDialogOn(getShell(), id, new String[] { jreID, complianceId, eeID }, data).open();

			fJREGroup.handlePossibleJVMChange();
			handlePossibleJVMChange();
		}
	}

	/**
	 * Validate this page and show appropriate warnings and error NewWizardMessages.
	 */
	private final class Validator implements Observer {

		public void update(Observable o, Object arg) {

			final IWorkspace workspace = JavaPlugin.getWorkspace();

			final String name = fNameGroup.getName();

			// check whether the project name field is empty
			if (name.length() == 0) {
				setErrorMessage(null);
				setMessage(NewWizardMessages.NewJavaProjectWizardPageOne_Message_enterProjectName);
				setPageComplete(false);
				return;
			}

			// check whether the project name is valid
			final IStatus nameStatus = workspace.validateName(name, IResource.PROJECT);
			if (!nameStatus.isOK()) {
				setErrorMessage(nameStatus.getMessage());
				setPageComplete(false);
				return;
			}

			// check whether project already exists
			final IProject handle = workspace.getRoot().getProject(name);
			if (handle.exists()) {
				setErrorMessage(NewWizardMessages.NewJavaProjectWizardPageOne_Message_projectAlreadyExists);
				setPageComplete(false);
				return;
			}

			IPath projectLocation = ResourcesPlugin.getWorkspace().getRoot().getLocation().append(name);
			if (projectLocation.toFile().exists()) {
				try {
					// correct casing
					String canonicalPath = projectLocation.toFile().getCanonicalPath();
					projectLocation = new Path(canonicalPath);
				} catch (IOException e) {
					JavaPlugin.log(e);
				}

				String existingName = projectLocation.lastSegment();
				if (!existingName.equals(fNameGroup.getName())) {
					setErrorMessage(Messages.format(NewWizardMessages.NewJavaProjectWizardPageOne_Message_invalidProjectNameForWorkspaceRoot, BasicElementLabels.getResourceName(existingName)));
					setPageComplete(false);
					return;
				}

			}

			final String location = fLocationGroup.getLocation().toOSString();

			// check whether location is empty
			if (location.length() == 0) {
				setErrorMessage(null);
				setMessage(NewWizardMessages.NewJavaProjectWizardPageOne_Message_enterLocation);
				setPageComplete(false);
				return;
			}

			// check whether the location is a syntactically correct path
			if (!Path.EMPTY.isValidPath(location)) {
				setErrorMessage(NewWizardMessages.NewJavaProjectWizardPageOne_Message_invalidDirectory);
				setPageComplete(false);
				return;
			}

			IPath projectPath = null;
			if (!fLocationGroup.isUseDefaultSelected()) {
				projectPath = Path.fromOSString(location);
				if (!projectPath.toFile().exists()) {
					// check non-existing external location
					if (!canCreate(projectPath.toFile())) {
						setErrorMessage(NewWizardMessages.NewJavaProjectWizardPageOne_Message_cannotCreateAtExternalLocation);
						setPageComplete(false);
						return;
					}
				}
			}

			// validate the location
			final IStatus locationStatus = workspace.validateProjectLocation(handle, projectPath);
			if (!locationStatus.isOK()) {
				setErrorMessage(locationStatus.getMessage());
				setPageComplete(false);
				return;
			}

			setPageComplete(true);

			setErrorMessage(null);
			setMessage(null);
		}

		private boolean canCreate(File file) {
			while (!file.exists()) {
				file = file.getParentFile();
				if (file == null)
					return false;
			}

			return file.canWrite();
		}
	}

	private static final String PAGE_NAME = "NewJavaProjectWizardPageOne"; //$NON-NLS-1$

	private final NameGroup fNameGroup;
	private final LocationGroup fLocationGroup;
	private final LayoutGroup fLayoutGroup;
	private final JREGroup fJREGroup;
	private final DetectGroup fDetectGroup;
	private final Validator fValidator;
	private final WorkingSetGroup fWorkingSetGroup;

	/**
	 * Creates a new {@link NewSARLProjectWizardPageOne}.
	 */
	public NewSARLProjectWizardPageOne() {
		super(PAGE_NAME);
		setPageComplete(false);
		setTitle(NewWizardMessages.NewJavaProjectWizardPageOne_page_title);
		setDescription(NewWizardMessages.NewJavaProjectWizardPageOne_page_description);

		fNameGroup = new NameGroup();
		fLocationGroup = new LocationGroup();
		fJREGroup = new JREGroup();
		fLayoutGroup = new LayoutGroup();
		fWorkingSetGroup = new WorkingSetGroup();
		fDetectGroup = new DetectGroup();

		// establish connections
		fNameGroup.addObserver(fLocationGroup);
		fDetectGroup.addObserver(fLayoutGroup);
		fDetectGroup.addObserver(fJREGroup);
		fLocationGroup.addObserver(fDetectGroup);

		// initialize all elements
		fNameGroup.notifyObservers();

		// create and connect validator
		fValidator = new Validator();
		fNameGroup.addObserver(fValidator);
		fLocationGroup.addObserver(fValidator);

		// initialize defaults
		setProjectName(""); //$NON-NLS-1$
		setProjectLocationURI(null);
		setWorkingSets(new IWorkingSet[0]);

		initializeDefaultVM();
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

	private void initializeDefaultVM() {
		JavaRuntime.getDefaultVMInstall();
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see org.eclipse.jface.dialogs.IDialogPage#createControl(org.eclipse.swt.widgets.Composite)
	 */
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

		Control jreControl = createJRESelectionControl(composite);
		jreControl.setLayoutData(new GridData(GridData.FILL_HORIZONTAL));

		// not necessary
		// Control layoutControl= createProjectLayoutControl(composite);
		// layoutControl.setLayoutData(new GridData(GridData.FILL_HORIZONTAL));

		Control workingSetControl = createWorkingSetControl(composite);
		workingSetControl.setLayoutData(new GridData(GridData.FILL_HORIZONTAL));

		Control infoControl = createInfoControl(composite);
		infoControl.setLayoutData(new GridData(GridData.FILL_HORIZONTAL));

		setControl(composite);
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
		return fNameGroup.createControl(composite);
	}

	/**
	 * Creates the controls for the location field.
	 * 
	 * @param composite the parent composite
	 * @return the created control
	 */
	protected Control createLocationControl(Composite composite) {
		return fLocationGroup.createControl(composite);
	}

	/**
	 * Creates the controls for the JRE selection
	 * 
	 * @param composite the parent composite
	 * @return the created control
	 */
	protected Control createJRESelectionControl(Composite composite) {
		return fJREGroup.createControl(composite);
	}

	/**
	 * Creates the controls for the project layout selection.
	 * 
	 * @param composite the parent composite
	 * @return the created control
	 */
	protected Control createProjectLayoutControl(Composite composite) {
		return fLayoutGroup.createContent(composite);
	}

	/**
	 * Creates the controls for the working set selection.
	 * 
	 * @param composite the parent composite
	 * @return the created control
	 */
	protected Control createWorkingSetControl(Composite composite) {
		return fWorkingSetGroup.createControl(composite);
	}

	/**
	 * Creates the controls for the info section.
	 * 
	 * @param composite the parent composite
	 * @return the created control
	 */
	protected Control createInfoControl(Composite composite) {
		return fDetectGroup.createControl(composite);
	}

	/**
	 * Gets a project name for the new project.
	 * 
	 * @return the new project resource handle
	 */
	public String getProjectName() {
		return fNameGroup.getName();
	}

	/**
	 * Sets the name of the new project
	 * 
	 * @param name the new name
	 */
	public void setProjectName(String name) {
		if (name == null)
			throw new IllegalArgumentException();

		fNameGroup.setName(name);
	}

	/**
	 * Returns the current project location path as entered by the user, or <code>null</code> if the project should be created in the workspace.
	 * 
	 * @return the project location path or its anticipated initial value.
	 */
	public URI getProjectLocationURI() {
		if (fLocationGroup.isUseDefaultSelected()) {
			return null;
		}
		return URIUtil.toURI(fLocationGroup.getLocation());
	}

	/**
	 * Sets the project location of the new project or <code>null</code> if the project should be created in the workspace
	 * 
	 * @param uri the new project location
	 */
	public void setProjectLocationURI(URI uri) {
		IPath path = uri != null ? URIUtil.toPath(uri) : null;
		fLocationGroup.setLocation(path);
	}

	/**
	 * Returns the compiler compliance to be used for the project, or <code>null</code> to use the workspace compiler compliance.
	 * 
	 * @return compiler compliance to be used for the project or <code>null</code>
	 */
	public String getCompilerCompliance() {
		return fJREGroup.getSelectedCompilerCompliance();
	}

	/**
	 * Returns the default class path entries to be added on new projects. By default this is the JRE container as selected by the user.
	 * 
	 * @return returns the default class path entries
	 */
	public IClasspathEntry[] getDefaultClasspathEntries() {
		
		Bundle xBaseBundle = Platform.getBundle("org.eclipse.xtext.xbase");
		Bundle xTendBundle = Platform.getBundle("org.eclipse.xtend.lib");
		
		Bundle sarlCoreBundle = Platform.getBundle("io.sarl.bundle");
		//Bundle sarlLangCoreBundle = Platform.getBundle("io.sarl.lang.core");
		//Bundle sarlUtilBundle = Platform.getBundle("io.sarl.util");

		
		URL xBaseBundleInstallLocation = null;
		URL xTendBundleInstallLocation = null;
		URL sarlCoreBundleInstallLocation = null;
		//URL sarlLangCoreBundleInstallLocation = null;
		//URL sarlUtilBundleInstallLocation = null;
		
		try {
			xBaseBundleInstallLocation = new URL(xBaseBundle.getLocation());
			xTendBundleInstallLocation = new URL(xTendBundle.getLocation());
			sarlCoreBundleInstallLocation = new URL(sarlCoreBundle.getLocation());
			//sarlLangCoreBundleInstallLocation = new URL(sarlLangCoreBundle.getLocation());
			//sarlUtilBundleInstallLocation = new URL(sarlUtilBundle.getLocation());
		} catch (MalformedURLException e1) {
			e1.printStackTrace();
		}

		URL xBaseBundleInstallLocationLocal = null;
		URL xTendBundleInstallLocationLocal = null;
		URL sarlCoreBundleInstallLocationLocal = null;
		//URL sarlLangCoreBundleInstallLocationLocal = null;
		//URL sarlUtilBundleInstallLocationLocal = null;
		try {
			xBaseBundleInstallLocationLocal = FileLocator.toFileURL(xBaseBundleInstallLocation);
			xTendBundleInstallLocationLocal = FileLocator.toFileURL(xTendBundleInstallLocation);
			sarlCoreBundleInstallLocationLocal = FileLocator.toFileURL(sarlCoreBundleInstallLocation);
			//sarlLangCoreBundleInstallLocationLocal = FileLocator.toFileURL(sarlLangCoreBundleInstallLocation);
			//sarlUtilBundleInstallLocationLocal = FileLocator.toFileURL(sarlUtilBundleInstallLocation);
		} catch (IOException e) {
			e.printStackTrace();
		}
		String xBaseBundleFullPath = null;
		if (xBaseBundleInstallLocationLocal.getPath().startsWith("file:")) {
			try {
				xBaseBundleFullPath = new File(new URL(xBaseBundleInstallLocationLocal.getPath()).getPath()).getAbsolutePath();
			} catch (MalformedURLException e) {
				e.printStackTrace();
			}
		} else {
			xBaseBundleFullPath = new File(xBaseBundleInstallLocationLocal.getPath()).getAbsolutePath();
		}
		
		String xTendBundleFullPath = null;
		if (xTendBundleInstallLocationLocal.getPath().startsWith("file:")) {
			try {
				xTendBundleFullPath = new File(new URL(xTendBundleInstallLocationLocal.getPath()).getPath()).getAbsolutePath();
			} catch (MalformedURLException e) {
				e.printStackTrace();
			}
		} else {
			xTendBundleFullPath = new File(xTendBundleInstallLocationLocal.getPath()).getAbsolutePath();
		}
		
		String sarlCoreBundleFullPath = null;
		if (sarlCoreBundleInstallLocationLocal.getPath().startsWith("file:")) {
			try {
				sarlCoreBundleFullPath = new File(new URL(sarlCoreBundleInstallLocationLocal.getPath()).getPath()).getAbsolutePath();
			} catch (MalformedURLException e) {
				e.printStackTrace();
			}
		} else {
			sarlCoreBundleFullPath = new File(sarlCoreBundleInstallLocationLocal.getPath()).getAbsolutePath();
		}
		/*
		String sarlLangCoreBundleFullPath = null;
		if (sarlLangCoreBundleInstallLocationLocal.getPath().startsWith("file:")) {
			try {
				sarlLangCoreBundleFullPath = new File(new URL(sarlLangCoreBundleInstallLocationLocal.getPath()).getPath()).getAbsolutePath();
			} catch (MalformedURLException e) {
				e.printStackTrace();
			}
		} else {
			sarlLangCoreBundleFullPath = new File(sarlLangCoreBundleInstallLocationLocal.getPath()).getAbsolutePath();
		}
		
		String sarlUtilBundleFullPath = null;
		if (sarlUtilBundleInstallLocationLocal.getPath().startsWith("file:")) {
			try {
				sarlUtilBundleFullPath = new File(new URL(sarlUtilBundleInstallLocationLocal.getPath()).getPath()).getAbsolutePath();
			} catch (MalformedURLException e) {
				e.printStackTrace();
			}
		} else {
			sarlUtilBundleFullPath = new File(sarlUtilBundleInstallLocationLocal.getPath()).getAbsolutePath();
		}		
		*/
		IClasspathEntry xBase = JavaCore.newContainerEntry(Path.fromOSString(xBaseBundleFullPath));
		IClasspathEntry xTend = JavaCore.newContainerEntry(Path.fromOSString(xTendBundleFullPath));
		
		IClasspathEntry sarlLang = JavaCore.newContainerEntry(Path.fromOSString(sarlCoreBundleFullPath));
		//IClasspathEntry sarlLangCore = JavaCore.newContainerEntry(Path.fromOSString(sarlLangCoreBundleFullPath));
		//IClasspathEntry sarlUtil = JavaCore.newContainerEntry(Path.fromOSString(sarlUtilBundleFullPath));
		
		
		IPath newPath = fJREGroup.getJREContainerPath();
		if (newPath != null) {
			return new IClasspathEntry[] { JavaCore.newContainerEntry(newPath), xBase,xTend,sarlLang};
		}
		IClasspathEntry[] entries = PreferenceConstants.getDefaultJRELibrary();
		int oldLength = entries.length;
		entries = Arrays.copyOf(entries, oldLength+3);
		entries[oldLength] = xBase;
		entries[oldLength+1] = xTend;
		entries[oldLength+2] = sarlLang;
		/*entries[oldLength+3] = sarlLangCore;
		entries[oldLength+4] = sarlUtil;*/
		
		return entries;
		
		
		
		/*
		IPath newPath = fJREGroup.getJREContainerPath();
		if (newPath != null) {
			return new IClasspathEntry[] { JavaCore.newContainerEntry(newPath) };
		}
		return PreferenceConstants.getDefaultJRELibrary();
		*/
		
	}

	private static final String SOURCE_FOLDER_NAME = "src"; //$NON-NLS-1$
	private static final String MAIN_FOLDER_NAME = "main"; //$NON-NLS-1$
	private static final String JAVA_FOLDER_NAME = "java"; //$NON-NLS-1$
	private static final String SARL_FOLDER_NAME = "sarl"; //$NON-NLS-1$
	private static final String GENERATED_SOURCE_FOLDER_NAME = "generated-sources"; //$NON-NLS-1$
	private static final String XTEND_FOLDER_NAME = "xtend"; //$NON-NLS-1$	

	/**
	 * The different directories composing a SARL project
	 */
	public static final String[] PROJECT_STRUCTURE_PATH = { SOURCE_FOLDER_NAME, SOURCE_FOLDER_NAME + File.separator + MAIN_FOLDER_NAME,// src/main
			SOURCE_FOLDER_NAME + File.separator + MAIN_FOLDER_NAME + File.separator + JAVA_FOLDER_NAME,// src/main/java
			SOURCE_FOLDER_NAME + File.separator + MAIN_FOLDER_NAME + File.separator + SARL_FOLDER_NAME,// src/main/sarl
			SOURCE_FOLDER_NAME + File.separator + MAIN_FOLDER_NAME + File.separator + GENERATED_SOURCE_FOLDER_NAME,// src/main/generated-sources
			SOURCE_FOLDER_NAME + File.separator + MAIN_FOLDER_NAME + File.separator + GENERATED_SOURCE_FOLDER_NAME + File.separator + XTEND_FOLDER_NAME };// src/main/generated-sources/xtend

	/**
	 * Returns the source class path entries to be added on new projects. The underlying resources may not exist. All entries that are returned must be of kind {@link IClasspathEntry#CPE_SOURCE}.
	 * 
	 * @return returns the source class path entries for the new project
	 */
	public IClasspathEntry[] getSourceClasspathEntries() {
		IPath sourceFolderPath = new Path(getProjectName()).makeAbsolute();

		/*
		 * if (fLayoutGroup.isSrcBin()) { IPath srcPath= new Path(PreferenceConstants.getPreferenceStore().getString(PreferenceConstants.SRCBIN_SRCNAME)); if (srcPath.segmentCount() > 0) { sourceFolderPath= sourceFolderPath.append(srcPath); } }
		 */
		IPath srcMainJava = new Path(sourceFolderPath + File.separator + PROJECT_STRUCTURE_PATH[2]);
		IPath srcMainSarl = new Path(sourceFolderPath + File.separator + PROJECT_STRUCTURE_PATH[3]);
		IPath srcMainGeneratedSourcesXtend = new Path(sourceFolderPath + File.separator + PROJECT_STRUCTURE_PATH[5]);

		return new IClasspathEntry[] { JavaCore.newSourceEntry(srcMainJava.makeAbsolute()), JavaCore.newSourceEntry(srcMainSarl.makeAbsolute()), JavaCore.newSourceEntry(srcMainGeneratedSourcesXtend.makeAbsolute()) };
	}

	/**
	 * Returns the source class path entries to be added on new projects. The underlying resource may not exist.
	 * 
	 * @return returns the default class path entries
	 */
	public IPath getOutputLocation() {
		IPath outputLocationPath = new Path(getProjectName()).makeAbsolute();
		if (fLayoutGroup.isSrcBin()) {
			IPath binPath = new Path(PreferenceConstants.getPreferenceStore().getString(PreferenceConstants.SRCBIN_BINNAME));
			if (binPath.segmentCount() > 0) {
				outputLocationPath = outputLocationPath.append(binPath);
			}
		}
		return outputLocationPath;
	}

	/**
	 * Returns the working sets to which the new project should be added.
	 * 
	 * @return the selected working sets to which the new project should be added
	 */
	public IWorkingSet[] getWorkingSets() {
		return fWorkingSetGroup.getSelectedWorkingSets();
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
		fWorkingSetGroup.setWorkingSets(workingSets);
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see org.eclipse.jface.dialogs.DialogPage#setVisible(boolean)
	 */
	@Override
	public void setVisible(boolean visible) {
		super.setVisible(visible);
		if (visible) {
			fNameGroup.postSetFocus();
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

	private static final IWorkingSet[] EMPTY_WORKING_SET_ARRAY = new IWorkingSet[0];

	private IWorkingSet[] getSelectedWorkingSet(IStructuredSelection selection, IWorkbenchPart activePart) {
		IWorkingSet[] selected = getSelectedWorkingSet(selection);
		if (selected != null && selected.length > 0) {
			for (int i = 0; i < selected.length; i++) {
				if (!isValidWorkingSet(selected[i]))
					return EMPTY_WORKING_SET_ARRAY;
			}
			return selected;
		}

		if (!(activePart instanceof PackageExplorerPart))
			return EMPTY_WORKING_SET_ARRAY;

		PackageExplorerPart explorerPart = (PackageExplorerPart) activePart;
		if (explorerPart.getRootMode() == PackageExplorerPart.PROJECTS_AS_ROOTS) {
			// Get active filter
			IWorkingSet filterWorkingSet = explorerPart.getFilterWorkingSet();
			if (filterWorkingSet == null)
				return EMPTY_WORKING_SET_ARRAY;

			if (!isValidWorkingSet(filterWorkingSet))
				return EMPTY_WORKING_SET_ARRAY;

			return new IWorkingSet[] { filterWorkingSet };
		} else {
			// If we have been gone into a working set return the working set
			Object input = explorerPart.getViewPartInput();
			if (!(input instanceof IWorkingSet))
				return EMPTY_WORKING_SET_ARRAY;

			IWorkingSet workingSet = (IWorkingSet) input;
			if (!isValidWorkingSet(workingSet))
				return EMPTY_WORKING_SET_ARRAY;

			return new IWorkingSet[] { workingSet };
		}
	}

	private IWorkingSet[] getSelectedWorkingSet(IStructuredSelection selection) {
		if (!(selection instanceof ITreeSelection))
			return EMPTY_WORKING_SET_ARRAY;

		ITreeSelection treeSelection = (ITreeSelection) selection;
		if (treeSelection.isEmpty())
			return EMPTY_WORKING_SET_ARRAY;

		List<?> elements = treeSelection.toList();
		if (elements.size() == 1) {
			Object element = elements.get(0);
			TreePath[] paths = treeSelection.getPathsFor(element);
			if (paths.length != 1)
				return EMPTY_WORKING_SET_ARRAY;

			TreePath path = paths[0];
			if (path.getSegmentCount() == 0)
				return EMPTY_WORKING_SET_ARRAY;

			Object candidate = path.getSegment(0);
			if (!(candidate instanceof IWorkingSet))
				return EMPTY_WORKING_SET_ARRAY;

			IWorkingSet workingSetCandidate = (IWorkingSet) candidate;
			if (isValidWorkingSet(workingSetCandidate))
				return new IWorkingSet[] { workingSetCandidate };

			return EMPTY_WORKING_SET_ARRAY;
		}

		ArrayList<IWorkingSet> result = new ArrayList<IWorkingSet>();
		for (Iterator<?> iterator = elements.iterator(); iterator.hasNext();) {
			Object element = iterator.next();
			if (element instanceof IWorkingSet && isValidWorkingSet((IWorkingSet) element)) {
				result.add((IWorkingSet) element);
			}
		}
		return result.toArray(new IWorkingSet[result.size()]);
	}

	private static boolean isValidWorkingSet(IWorkingSet workingSet) {
		String id = workingSet.getId();
		if (!IWorkingSetIDs.JAVA.equals(id) && !IWorkingSetIDs.RESOURCE.equals(id))
			return false;

		if (workingSet.isAggregateWorkingSet())
			return false;

		return true;
	}

}
