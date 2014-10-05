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
package io.sarl.eclipse.preferences;

import io.sarl.eclipse.SARLEclipsePlugin;
import io.sarl.eclipse.runtime.ISREInstall;
import io.sarl.eclipse.runtime.ISREInstallChangedListener;
import io.sarl.eclipse.runtime.SARLRuntime;
import io.sarl.eclipse.wizards.sreinstall.AddSREInstallWizard;
import io.sarl.eclipse.wizards.sreinstall.EditSREInstallWizard;

import java.text.MessageFormat;
import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;

import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.ListenerList;
import org.eclipse.core.runtime.NullProgressMonitor;
import org.eclipse.core.runtime.Platform;
import org.eclipse.debug.internal.ui.SWTFactory;
import org.eclipse.jdt.internal.debug.ui.jres.JREMessages;
import org.eclipse.jdt.internal.ui.JavaPlugin;
import org.eclipse.jdt.launching.PropertyChangeEvent;
import org.eclipse.jdt.ui.ISharedImages;
import org.eclipse.jdt.ui.JavaUI;
import org.eclipse.jface.dialogs.IDialogSettings;
import org.eclipse.jface.preference.PreferencePage;
import org.eclipse.jface.viewers.BaseLabelProvider;
import org.eclipse.jface.viewers.CheckStateChangedEvent;
import org.eclipse.jface.viewers.CheckboxTableViewer;
import org.eclipse.jface.viewers.DoubleClickEvent;
import org.eclipse.jface.viewers.ICheckStateListener;
import org.eclipse.jface.viewers.IDoubleClickListener;
import org.eclipse.jface.viewers.ISelection;
import org.eclipse.jface.viewers.ISelectionChangedListener;
import org.eclipse.jface.viewers.ISelectionProvider;
import org.eclipse.jface.viewers.IStructuredContentProvider;
import org.eclipse.jface.viewers.IStructuredSelection;
import org.eclipse.jface.viewers.ITableColorProvider;
import org.eclipse.jface.viewers.ITableFontProvider;
import org.eclipse.jface.viewers.ITableLabelProvider;
import org.eclipse.jface.viewers.SelectionChangedEvent;
import org.eclipse.jface.viewers.StructuredSelection;
import org.eclipse.jface.viewers.Viewer;
import org.eclipse.jface.viewers.ViewerComparator;
import org.eclipse.jface.window.Window;
import org.eclipse.jface.wizard.WizardDialog;
import org.eclipse.swt.SWT;
import org.eclipse.swt.custom.BusyIndicator;
import org.eclipse.swt.events.KeyAdapter;
import org.eclipse.swt.events.KeyEvent;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.graphics.Color;
import org.eclipse.swt.graphics.Font;
import org.eclipse.swt.graphics.Image;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Display;
import org.eclipse.swt.widgets.Event;
import org.eclipse.swt.widgets.Listener;
import org.eclipse.swt.widgets.Table;
import org.eclipse.swt.widgets.TableColumn;
import org.eclipse.ui.IWorkbench;
import org.eclipse.ui.IWorkbenchPreferencePage;
import org.osgi.framework.Bundle;
import org.osgi.framework.Version;

import com.google.common.base.Strings;

/** Preference page for the SARL runtime environments.
 *
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 */
public class SREsPreferencePage extends PreferencePage implements IWorkbenchPreferencePage, ISelectionProvider {

	/**
	 * ID for the page.
	 */
	public static final String ID = "io.sarl.eclipse.preferences.SREsPreferencePage"; //$NON-NLS-1$

	private Table sreTable;
	private CheckboxTableViewer sresList;

	private final InstallListener installListener = new InstallListener();

	/**
	 * SREs being displayed.
	 */
	private final List<ISREInstall> sreArray = new ArrayList<>();

	/**
	 * Selection listeners (default SRE changes).
	 */
	private final ListenerList selectionListeners = new ListenerList();

	/**
	 * Previous selection.
	 */
	private ISelection prevSelection = new StructuredSelection();

	// Action buttons
	private Button addButton;
	private Button removeButton;
	private Button editButton;
	private Button copyButton;
	private Button resetButton;

	private Column sortColumn = Column.NAME;

	/**
	 * Constructor.
	 */
	public SREsPreferencePage() {
		//
	}

	/** Set the error message from the given exception.
	 *
	 * @param e - the exception to log.
	 */
	public void setErrorMessage(Throwable e) {
		if (e != null) {
			String m = e.getLocalizedMessage();
			if (Strings.isNullOrEmpty(m)) {
				m = e.getMessage();
			}
			if (Strings.isNullOrEmpty(m)) {
				m = MessageFormat.format(Messages.SREsPreferencePage_9, e.getClass().getName());
			}
			setErrorMessage(m);
		}
	}

	@Override
	public void init(IWorkbench workbench) {
		//
	}

	@Override
	protected Control createContents(Composite parent) {
		initializeDialogUnits(parent);

		noDefaultAndApplyButton();

		Font font = parent.getFont();

		GridLayout layout = new GridLayout();
		layout.numColumns = 1;
		layout.marginHeight = 2;
		layout.marginWidth = 2;
		parent.setLayout(layout);

		SWTFactory.createWrapLabel(parent,
				Messages.SREsPreferencePage_0,
				1, 300);
		SWTFactory.createVerticalSpacer(parent, 1);

		SWTFactory.createWrapLabel(parent,
				Messages.SREsPreferencePage_1,
				1, 300);
		Composite listComposite = SWTFactory.createComposite(parent, font, 2, 1, GridData.FILL_BOTH);

		this.sreTable = new Table(listComposite,
				SWT.CHECK | SWT.BORDER | SWT.MULTI | SWT.FULL_SELECTION);
		GridData gd = new GridData(GridData.FILL_BOTH);
		gd.heightHint = 250;
		gd.widthHint = 350;
		this.sreTable.setLayoutData(gd);
		this.sreTable.setFont(font);
		this.sreTable.setHeaderVisible(true);
		this.sreTable.setLinesVisible(true);

		TableColumn column = new TableColumn(this.sreTable, SWT.NULL);
		column.setText(Messages.SREsPreferencePage_2);
		column.addSelectionListener(new SelectionAdapter() {
			@SuppressWarnings("synthetic-access")
			@Override
			public void widgetSelected(SelectionEvent e) {
				sortByName();
				SREsPreferencePage.this.sresList.refresh(true);
			}
		});
		int defaultwidth = 350 / 3 + 1;
		column.setWidth(defaultwidth);

		column = new TableColumn(this.sreTable, SWT.NULL);
		column.setText(Messages.SREsPreferencePage_3);
		column.addSelectionListener(new SelectionAdapter() {
			@SuppressWarnings("synthetic-access")
			@Override
			public void widgetSelected(SelectionEvent e) {
				sortByLocation();
				SREsPreferencePage.this.sresList.refresh(true);
			}
		});
		column.setWidth(defaultwidth);

		this.sresList = new CheckboxTableViewer(this.sreTable);
		this.sresList.setLabelProvider(new SRELabelProvider());
		this.sresList.setContentProvider(new SREsContentProvider());
		this.sresList.setUseHashlookup(true);

		this.sresList.addSelectionChangedListener(new ISelectionChangedListener() {
			@SuppressWarnings("synthetic-access")
			@Override
			public void selectionChanged(SelectionChangedEvent evt) {
				enableButtons();
			}
		});

		this.sresList.addCheckStateListener(new ICheckStateListener() {
			@Override
			public void checkStateChanged(CheckStateChangedEvent event) {
				if (event.getChecked()) {
					setDefaultSRE((ISREInstall) event.getElement());
				} else {
					setDefaultSRE(null);
				}
			}
		});

		this.sresList.addDoubleClickListener(new IDoubleClickListener() {
			@SuppressWarnings("synthetic-access")
			@Override
			public void doubleClick(DoubleClickEvent e) {
				if (!SREsPreferencePage.this.sresList.getSelection().isEmpty()) {
					editSRE();
				}
			}
		});
		this.sreTable.addKeyListener(new KeyAdapter() {
			@SuppressWarnings("synthetic-access")
			@Override
			public void keyPressed(KeyEvent event) {
				if (event.character == SWT.DEL && event.stateMask == 0) {
					if (SREsPreferencePage.this.removeButton.isEnabled()) {
						removeSREs();
					}
				}
			}
		});

		Composite buttons = SWTFactory.createComposite(listComposite, font, 1, 1, GridData.VERTICAL_ALIGN_BEGINNING, 0, 0);

		this.addButton = SWTFactory.createPushButton(buttons, Messages.SREsPreferencePage_4, null);
		this.addButton.addListener(SWT.Selection, new Listener() {
			@Override
			public void handleEvent(Event evt) {
				addSRE();
			}
		});

		this.editButton = SWTFactory.createPushButton(buttons, Messages.SREsPreferencePage_5, null);
		this.editButton.addListener(SWT.Selection, new Listener() {
			@Override
			public void handleEvent(Event evt) {
				editSRE();
			}
		});

		this.copyButton = SWTFactory.createPushButton(buttons, Messages.SREsPreferencePage_6, null);
		this.copyButton.addListener(SWT.Selection, new Listener() {
			@Override
			public void handleEvent(Event evt) {
				copySRE();
			}
		});

		this.removeButton = SWTFactory.createPushButton(buttons, Messages.SREsPreferencePage_7, null);
		this.removeButton.addListener(SWT.Selection, new Listener() {
			@Override
			public void handleEvent(Event evt) {
				removeSREs();
			}
		});

		this.resetButton = SWTFactory.createPushButton(buttons, Messages.SREsPreferencePage_10, null);
		this.resetButton.addListener(SWT.Selection, new Listener() {
			@Override
			public void handleEvent(Event evt) {
				resetSREs();
			}
		});

		SWTFactory.createVerticalSpacer(listComposite, 1);

		// Populates the SRE table with existing SREs defined in the workspace.
		ISREInstall[] sres = SARLRuntime.getSREInstalls();
		for (ISREInstall sre : sres) {
			sre.revalidate();
		}
		setSREs(sres);
		setDefaultSRE(SARLRuntime.getDefaultSREInstall());

		// by default, sort by name
		restoreColumnSettings(JavaPlugin.getDefault().getDialogSettings());

		enableButtons();

		SARLRuntime.addSREInstallChangedListener(this.installListener);

		applyDialogFont(parent);
		return parent;
	}

	/**
	 * Sets the SREs to be displayed in this block.
	 *
	 * @param sres - SREs to be displayed
	 */
	protected void setSREs(ISREInstall[] sres) {
		this.sreArray.clear();
		for (ISREInstall sre : sres) {
			this.sreArray.add(sre);
		}
		this.sresList.setInput(this.sreArray);
		this.sresList.refresh();
		updateUI();
	}

	/**
	 * Returns the SREs currently being displayed in this block.
	 *
	 * @return the SREs currently being displayed in this block
	 */
	public ISREInstall[] getSREs() {
		return this.sreArray.toArray(new ISREInstall[this.sreArray.size()]);
	}

	private boolean isDuplicateName(String name) {
		String n = Strings.nullToEmpty(name);
		for (ISREInstall sre : this.sreArray) {
			if (n.equals(sre.getName())) {
				return true;
			}
		}
		return false;
	}

	private boolean isDuplicateId(String id) {
		String i = Strings.nullToEmpty(id);
		for (ISREInstall sre : this.sreArray) {
			if (i.equals(sre.getId())) {
				return true;
			}
		}
		return false;
	}

	private String createUniqueIdentifier() {
		String id = SARLRuntime.createUniqueIdentifier();
		while (isDuplicateId(id)) {
			id = SARLRuntime.createUniqueIdentifier();
		}
		return id;
	}

	/**
	 * Compares the given name against current names and adds the appropriate numerical
	 * suffix to ensure that it is unique.
	 *
	 * @param name - the name with which to ensure uniqueness.
	 * @return the unique version of the given name.
	 */
	public String createUniqueName(String name) {
		if (!isDuplicateName(name)) {
			return name;
		}
		if (name.matches(".*\\(\\d*\\)")) { //$NON-NLS-1$
			int start = name.lastIndexOf('(');
			int end = name.lastIndexOf(')');
			String stringInt = name.substring(start + 1, end);
			int numericValue = Integer.parseInt(stringInt);
			String newName = name.substring(0, start + 1) + (numericValue + 1) + ")"; //$NON-NLS-1$
			return createUniqueName(newName);
		}
		return createUniqueName(name + " (1)"); //$NON-NLS-1$
	}

	/** Add a SRE.
	 */
	protected void addSRE() {
		AddSREInstallWizard wizard = new AddSREInstallWizard(
				createUniqueIdentifier(),
				this.sreArray.toArray(new ISREInstall[this.sreArray.size()]));
		WizardDialog dialog = new WizardDialog(getShell(), wizard);
		if (dialog.open() == Window.OK) {
			ISREInstall result = wizard.getCreatedSRE();
			if (result != null) {
				this.sreArray.add(result);
				//refresh from model
				this.sresList.refresh();
				this.sresList.setSelection(new StructuredSelection(result));
				//ensure labels are updated
				this.sresList.refresh(true);
				updateUI();
			}
		}
	}

	/** Edit the selected SRE.
	 */
	protected void editSRE() {
		IStructuredSelection selection = (IStructuredSelection) this.sresList.getSelection();
		ISREInstall sre = (ISREInstall) selection.getFirstElement();
		if (sre == null) {
			return;
		}
		EditSREInstallWizard wizard = new EditSREInstallWizard(
				sre, this.sreArray.toArray(new ISREInstall[this.sreArray.size()]));
		WizardDialog dialog = new WizardDialog(getShell(), wizard);
		if (dialog.open() == Window.OK) {
			this.sresList.setSelection(new StructuredSelection(sre));
			this.sresList.refresh(true);
			updateUI();
		}
	}

	/** Copy the selected SRE.
	 */
	@SuppressWarnings("unchecked")
	protected void copySRE() {
		IStructuredSelection selection = (IStructuredSelection) this.sresList.getSelection();
		Iterator<ISREInstall> it = selection.iterator();

		ArrayList<ISREInstall> newEntries = new ArrayList<>();
		while (it.hasNext()) {
			ISREInstall selectedSRE = it.next();

			ISREInstall copy = selectedSRE.copy(createUniqueIdentifier());
			copy.setName(createUniqueName(selectedSRE.getName()));

			EditSREInstallWizard wizard = new EditSREInstallWizard(
					copy, this.sreArray.toArray(new ISREInstall[this.sreArray.size()]));
			WizardDialog dialog = new WizardDialog(getShell(), wizard);
			int dlgResult = dialog.open();
			if (dlgResult == Window.OK) {
				newEntries.add(copy);
			} else {
				assert (dlgResult == Window.CANCEL);
				// Canceling one wizard should cancel all subsequent wizards
				break;
			}
		}
		if (!newEntries.isEmpty()) {
			this.sreArray.addAll(newEntries);
			this.sresList.refresh();
			this.sresList.setSelection(new StructuredSelection(newEntries.toArray()));
		} else {
			this.sresList.setSelection(selection);
		}
		this.sresList.refresh(true);
		updateUI();
	}

	/** Remove the selected SREs.
	 */
	@SuppressWarnings("unchecked")
	protected void removeSREs() {
		IStructuredSelection selection = (IStructuredSelection) this.sresList.getSelection();
		ISREInstall[] vms = new ISREInstall[selection.size()];
		Iterator<ISREInstall> iter = selection.iterator();
		int i = 0;
		while (iter.hasNext()) {
			vms[i] = iter.next();
			i++;
		}
		removeSREs(vms);
	}

	/**
	 * Removes the given SREs from the table.
	 *
	 * @param sres - the SREs to remove.
	 */
	public void removeSREs(ISREInstall... sres) {
		ISREInstall defaultSRE = getDefaultSRE();
		String defaultId = defaultSRE == null ? null : defaultSRE.getId();
		int defaultIndex = -1;
		if (defaultId != null) {
			for (int i = 0; defaultIndex == -1 && i < this.sreTable.getItemCount(); ++i) {
				if (defaultId.equals(
						((ISREInstall) this.sreTable.getItem(i).getData()).getId())) {
					defaultIndex = i;
				}
			}
		}
		String normedDefaultId = Strings.nullToEmpty(defaultId);
		boolean defaultIsRemoved = false;
		for (ISREInstall sre : sres) {
			if (this.sreArray.remove(sre) && sre.getId().equals(normedDefaultId)) {
				defaultIsRemoved = true;
			}
		}
		this.sresList.refresh();
		// Update the default SRE
		if (defaultIsRemoved) {
			if (this.sreTable.getItemCount() == 0) {
				setSelection(null);
			} else {
				if (defaultIndex < 0) {
					defaultIndex = 0;
				} else if (defaultIndex >= this.sreTable.getItemCount()) {
					defaultIndex = this.sreTable.getItemCount() - 1;
				}
				setSelection(new StructuredSelection(
						this.sreTable.getItem(defaultIndex).getData()));
			}
		}
		this.sresList.refresh(true);
		if (defaultIsRemoved) {
			fireDefaultSREChanged();
		}
		updateUI();
	}

	/** Reset the list of the SREs to the platform elements.
	 */
	protected void resetSREs() {
		try {
			SARLRuntime.reset();
		} catch (CoreException e) {
			setErrorMessage(e);
		}
	}

	private boolean verifyValidity(ISREInstall sre, boolean errorMessages) {
		if (!sre.getValidity().isOK()) {
			if (errorMessages) {
				setErrorMessage(MessageFormat.format(
						io.sarl.eclipse.launching.Messages.RuntimeEnvironmentTab_5,
						sre.getName()));
			}
			return false;
		}
		// Check the SARL version.
		Bundle bundle = Platform.getBundle("io.sarl.lang"); //$NON-NLS-1$
		if (bundle != null) {
			Version sarlVersion = bundle.getVersion();
			Version minVersion = SARLEclipsePlugin.parseVersion(sre.getMinimalSARLVersion());
			Version maxVersion = SARLEclipsePlugin.parseVersion(sre.getMaximalSARLVersion());
			int cmp = SARLEclipsePlugin.compareVersionToRange(sarlVersion, minVersion, maxVersion);
			if (cmp < 0) {
				if (errorMessages) {
					setErrorMessage(MessageFormat.format(
							io.sarl.eclipse.runtime.Messages.AbstractSREInstall_0,
							sarlVersion.toString(),
							minVersion.toString()));
				}
				return false;
			} else if (cmp > 0) {
				if (errorMessages) {
					setErrorMessage(MessageFormat.format(
							io.sarl.eclipse.runtime.Messages.AbstractSREInstall_1,
							sarlVersion.toString(),
							maxVersion.toString()));
				}
				return false;
			}
		}
		return true;
	}

	@Override
	public boolean isValid() {
		setMessage(null);
		setErrorMessage((String) null);
		if (this.sreArray.isEmpty()) {
			setMessage(io.sarl.eclipse.launching.Messages.RuntimeEnvironmentTab_7);
		} else {
			ISREInstall defaultSRE = getDefaultSRE();
			if (defaultSRE == null) {
				setErrorMessage(Messages.SREsPreferencePage_8);
				return false;
			}
			if (!verifyValidity(defaultSRE, true)) {
				return false;
			}
		}
		return super.isValid();
	}

	@Override
	protected void performDefaults() {
		super.performDefaults();
	}

	@Override
	public boolean performOk() {
		final boolean[] canceled = new boolean[] {false};
		BusyIndicator.showWhile(null, new Runnable() {
			@Override
			public void run() {
				ISREInstall defaultSRE = getDefaultSRE();
				ISREInstall[] sres = getSREs();
				NullProgressMonitor monitor = new NullProgressMonitor();
				try {
					SARLRuntime.setSREInstalls(sres, monitor);
					SARLRuntime.setDefaultSREInstall(defaultSRE, monitor, false);
					SARLRuntime.saveSREConfiguration(monitor);
					canceled[0] = monitor.isCanceled();
				} catch (CoreException e) {
					setErrorMessage(e);
					canceled[0] = true;
				}
			}
		});

		if (canceled[0]) {
			return false;
		}

		// save column widths
		IDialogSettings settings = JavaPlugin.getDefault().getDialogSettings();
		int columnCount = this.sreTable.getColumnCount();
		for (int i = 0; i < columnCount; i++) {
			settings.put(ID + ".columnWidth" + i, //$NON-NLS-1$
					this.sreTable.getColumn(i).getWidth());
		}
		settings.put(ID + ".sortColumn", this.sortColumn.name()); //$NON-NLS-1$

		return super.performOk();
	}

	/**
	 * Restores the column widths from dialog settings.
	 *
	 * @param settings - the settings to read.
	 */
	private void restoreColumnWidths(IDialogSettings settings) {
		int columnCount = this.sreTable.getColumnCount();
		for (int i = 0; i < columnCount; i++) {
			int width = -1;
			try {
				width = settings.getInt(ID + ".columnWidth" + i); //$NON-NLS-1$
			} catch (NumberFormatException _) {
				//
			}

			if ((width <= 0) || (i == this.sreTable.getColumnCount() - 1)) {
				this.sreTable.getColumn(i).pack();
			} else {
				this.sreTable.getColumn(i).setWidth(width);
			}
		}
	}

	/**
	 * Restore table settings from the given dialog store using the
	 * given key.
	 *
	 * @param settings - dialog settings store
	 */
	private void restoreColumnSettings(IDialogSettings settings) {
		this.sresList.getTable().layout(true);
		restoreColumnWidths(settings);
		this.sortColumn = Column.NAME;
		try {
			String columnName = settings.get(ID + ".sortColumn"); //$NON-NLS-1$
			if (!Strings.isNullOrEmpty(columnName)) {
				this.sortColumn = Column.valueOf(columnName);
				if (this.sortColumn == null) {
					this.sortColumn = Column.NAME;
				}
			}
		} catch (Throwable _) {
			//
		}
		switch (this.sortColumn) {
		case NAME:
			sortByName();
			break;
		case LOCATION:
			sortByLocation();
			break;
		default:
		}
	}

	@Override
	public void dispose() {
		SARLRuntime.removeSREInstallChangedListener(this.installListener);
		super.dispose();
	}

	/**
	 * Sets the default SRE, possible <code>null</code>.
	 *
	 * @param sre - the SRE or <code>null</code>
	 */
	public void setDefaultSRE(ISREInstall sre) {
		if (sre == null) {
			setSelection(new StructuredSelection());
		} else {
			setSelection(new StructuredSelection(sre));
		}
	}

	private void updateUI() {
		if (getContainer() != null) {
			getContainer().updateButtons();
		}
		updateApplyButton();
	}

	@Override
	public void setSelection(ISelection selection) {
		if (selection instanceof IStructuredSelection
				&& !selection.equals(this.prevSelection)) {
			this.prevSelection = selection;
			Object sre = ((IStructuredSelection) selection).getFirstElement();
			if (sre == null) {
				this.sresList.setCheckedElements(new Object[0]);
				updateUI();
			} else {
				this.sresList.setCheckedElements(new Object[]{sre});
				this.sresList.reveal(sre);
				updateUI();
			}
			this.sresList.refresh(true);
			fireDefaultSREChanged();
			updateUI();
		}
	}

	@Override
	public ISelection getSelection() {
		return new StructuredSelection(this.sresList.getCheckedElements());
	}

	private void fireDefaultSREChanged() {
		SelectionChangedEvent event = new SelectionChangedEvent(this, getSelection());
		Object[] listeners = this.selectionListeners.getListeners();
		for (int i = 0; i < listeners.length; i++) {
			ISelectionChangedListener listener = (ISelectionChangedListener) listeners[i];
			listener.selectionChanged(event);
		}
	}

	@Override
	public void addSelectionChangedListener(ISelectionChangedListener listener) {
		this.selectionListeners.add(listener);
	}

	@Override
	public void removeSelectionChangedListener(ISelectionChangedListener listener) {
		this.selectionListeners.remove(listener);
	}

	/**
	 * Returns the default SRE or <code>null</code> if none.
	 *
	 * @return the default SRE or <code>null</code> if none
	 */
	public ISREInstall getDefaultSRE() {
		Object[] objects = this.sresList.getCheckedElements();
		if (objects.length == 0) {
			return null;
		}
		return (ISREInstall) objects[0];
	}

	/**
	 * Enables the buttons based on selected items counts in the viewer.
	 */
	@SuppressWarnings("unchecked")
	private void enableButtons() {
		IStructuredSelection selection = (IStructuredSelection) this.sresList.getSelection();
		int selectionCount = selection.size();
		this.editButton.setEnabled(selectionCount == 1);
		this.copyButton.setEnabled(selectionCount > 0);
		if (selectionCount > 0 && selectionCount <= this.sresList.getTable().getItemCount()) {
			Iterator<ISREInstall> iterator = selection.iterator();
			while (iterator.hasNext()) {
				ISREInstall install = iterator.next();
				if (SARLRuntime.isPlatformSRE(install)) {
					this.removeButton.setEnabled(false);
					return;
				}
			}
			this.removeButton.setEnabled(true);
		} else {
			this.removeButton.setEnabled(false);
		}
	}

	/**
	 * Sorts by SRE name.
	 */
	private void sortByName() {
		this.sresList.setComparator(new ViewerComparator() {
			@Override
			public int compare(Viewer viewer, Object e1, Object e2) {
				if ((e1 instanceof ISREInstall) && (e2 instanceof ISREInstall)) {
					ISREInstall left = (ISREInstall) e1;
					ISREInstall right = (ISREInstall) e2;
					return left.getName().compareToIgnoreCase(right.getName());
				}
				return super.compare(viewer, e1, e2);
			}

			@Override
			public boolean isSorterProperty(Object element, String property) {
				return true;
			}
		});
		this.sortColumn = Column.NAME;
	}

	/**
	 * Sorts by VM location.
	 */
	private void sortByLocation() {
		this.sresList.setComparator(new ViewerComparator() {
			@Override
			public int compare(Viewer viewer, Object e1, Object e2) {
				if ((e1 instanceof ISREInstall) && (e2 instanceof ISREInstall)) {
					ISREInstall left = (ISREInstall) e1;
					ISREInstall right = (ISREInstall) e2;
					return left.getLocation().compareToIgnoreCase(right.getLocation());
				}
				return super.compare(viewer, e1, e2);
			}

			@Override
			public boolean isSorterProperty(Object element, String property) {
				return true;
			}
		});
		this.sortColumn = Column.LOCATION;
	}

	/**
	 * Label provider for installed SREs table.
	 *
	 * @author $Author: sgalland$
	 * @version $FullVersion$
	 * @mavengroupid $GroupId$
	 * @mavenartifactid $ArtifactId$
	 */
	private class SRELabelProvider extends BaseLabelProvider
	implements ITableLabelProvider, ITableColorProvider, ITableFontProvider {

		/**
		 */
		public SRELabelProvider() {
			//
		}

		@Override
		public Font getFont(Object element, int columnIndex) {
			return null;
		}

		@SuppressWarnings("synthetic-access")
		private boolean isValid(Object element) {
			if (element instanceof ISREInstall) {
				ISREInstall sre = (ISREInstall) element;
				return verifyValidity(sre, false);
			}
			return true;
		}

		@Override
		public Color getForeground(Object element, int columnIndex) {
			if (isValid(element)) {
				return getControl().getDisplay().getSystemColor(SWT.COLOR_LIST_FOREGROUND);
			}
			return getControl().getDisplay().getSystemColor(SWT.COLOR_RED);
		}

		@Override
		public Color getBackground(Object element, int columnIndex) {
			return getControl().getDisplay().getSystemColor(SWT.COLOR_LIST_BACKGROUND);
		}

		/** {@inheritDoc}
		 */
		@Override
		public Image getColumnImage(Object element, int columnIndex) {
			if (columnIndex == 0) {
				return JavaUI.getSharedImages().getImage(ISharedImages.IMG_OBJS_LIBRARY);
			}
			return null;
		}

		/** {@inheritDoc}
		 */
		@Override
		public String getColumnText(Object element, int columnIndex) {
			if (element instanceof ISREInstall) {
				ISREInstall sre = (ISREInstall) element;
				switch(columnIndex) {
				case 0:
					if (getDefaultSRE() == element) {
						return MessageFormat.format(
								JREMessages.InstalledJREsBlock_7,
								sre.getName());
					}
					return sre.getName();
				case 1:
					return sre.getLocation();
				default:
				}
			}
			return element == null ? "" : element.toString(); //$NON-NLS-1$
		}

	} // class SRELabelProvider

	/**
	 * Content provider to show a list of JREs.
	 *
	 * @author $Author: sgalland$
	 * @version $FullVersion$
	 * @mavengroupid $GroupId$
	 * @mavenartifactid $ArtifactId$
	 */
	private class SREsContentProvider implements IStructuredContentProvider {

		/**
		 */
		public SREsContentProvider() {
			//
		}

		@SuppressWarnings("synthetic-access")
		@Override
		public Object[] getElements(Object input) {
			return SREsPreferencePage.this.sreArray.toArray();
		}

		@Override
		public void inputChanged(Viewer viewer, Object oldInput, Object newInput) {
			//
		}

		@Override
		public void dispose() {
			//
		}

	} // class SREsContentProvider

	/**
	 * @author $Author: sgalland$
	 * @version $FullVersion$
	 * @mavengroupid $GroupId$
	 * @mavenartifactid $ArtifactId$
	 */
	private class InstallListener implements ISREInstallChangedListener {

		/**
		 */
		public InstallListener() {
			//
		}

		@SuppressWarnings("synthetic-access")
		@Override
		public void sreAdded(ISREInstall sre) {
			if (!SREsPreferencePage.this.sreArray.contains(sre)) {
				SREsPreferencePage.this.sreArray.add(sre);
				// Refreshes the SRE listing after a SRE install notification, might not
				// happen on the UI thread.
				Display display = Display.getDefault();
				if (display.getThread().equals(Thread.currentThread())) {
					SREsPreferencePage.this.sresList.refresh();
				} else {
					display.syncExec(new Runnable() {
						@Override
						public void run() {
							SREsPreferencePage.this.sresList.refresh();
						}
					});
				}
			}
		}

		@SuppressWarnings("synthetic-access")
		@Override
		public void sreRemoved(ISREInstall sre) {
			if (SREsPreferencePage.this.sreArray.contains(sre)) {
				SREsPreferencePage.this.sreArray.remove(sre);
				// Refreshes the SRE listing after a SRE install notification, might not
				// happen on the UI thread.
				Display display = Display.getDefault();
				if (display.getThread().equals(Thread.currentThread())) {
					SREsPreferencePage.this.sresList.refresh();
				} else {
					display.syncExec(new Runnable() {
						@Override
						public void run() {
							SREsPreferencePage.this.sresList.refresh();
						}
					});
				}
			}
		}

		/** {@inheritDoc}
		 */
		@SuppressWarnings("synthetic-access")
		@Override
		public void sreChanged(PropertyChangeEvent event) {
			if (ISREInstallChangedListener.PROPERTY_NAME.equals(event.getProperty())) {
				ISREInstall sre = (ISREInstall) event.getSource();
				if (SREsPreferencePage.this.sreArray.contains(sre)) {
					// Refreshes the SRE listing after a SRE install notification, might not
					// happen on the UI thread.
					Display display = Display.getDefault();
					if (display.getThread().equals(Thread.currentThread())) {
						SREsPreferencePage.this.sresList.refresh();
					} else {
						display.syncExec(new Runnable() {
							@Override
							public void run() {
								SREsPreferencePage.this.sresList.refresh();
							}
						});
					}
				}
			}
		}

		/** {@inheritDoc}
		 */
		@Override
		public void defaultSREInstallChanged(ISREInstall previous, ISREInstall current) {
			setDefaultSRE(current);
		}

	} // class InstallListener

	/**
	 * @author $Author: sgalland$
	 * @version $FullVersion$
	 * @mavengroupid $GroupId$
	 * @mavenartifactid $ArtifactId$
	 */
	private static enum Column {

		/** Name column.
		 */
		NAME,

		/** Location column.
		 */
		LOCATION;

	} // enum Column

}

