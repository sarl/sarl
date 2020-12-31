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

package io.sarl.lang.ui.extralanguage.properties;

import java.util.List;
import java.util.Set;

import com.google.common.collect.Iterables;
import com.google.common.collect.Lists;
import com.google.inject.Inject;
import org.eclipse.core.resources.IProject;
import org.eclipse.core.resources.ResourcesPlugin;
import org.eclipse.core.runtime.jobs.Job;
import org.eclipse.debug.internal.ui.SWTFactory;
import org.eclipse.jface.dialogs.IDialogSettings;
import org.eclipse.jface.layout.PixelConverter;
import org.eclipse.jface.preference.IPreferenceStore;
import org.eclipse.jface.resource.ImageDescriptor;
import org.eclipse.jface.resource.ImageRegistry;
import org.eclipse.jface.resource.ResourceLocator;
import org.eclipse.swt.SWT;
import org.eclipse.swt.custom.TableEditor;
import org.eclipse.swt.events.FocusAdapter;
import org.eclipse.swt.events.FocusEvent;
import org.eclipse.swt.events.ModifyEvent;
import org.eclipse.swt.events.ModifyListener;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.graphics.Image;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Display;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Table;
import org.eclipse.swt.widgets.TableColumn;
import org.eclipse.swt.widgets.TableItem;
import org.eclipse.swt.widgets.Text;
import org.eclipse.swt.widgets.Widget;
import org.eclipse.ui.forms.widgets.ExpandableComposite;
import org.eclipse.xtext.builder.EclipseOutputConfigurationProvider;
import org.eclipse.xtext.builder.preferences.BuilderPreferenceAccess;
import org.eclipse.xtext.generator.OutputConfiguration;
import org.eclipse.xtext.ui.preferences.OptionsConfigurationBlock;
import org.eclipse.xtext.ui.preferences.ScrolledPageContent;

import io.sarl.lang.extralanguage.compiler.ExtraLanguageOutputConfigurations;
import io.sarl.lang.ui.extralanguage.preferences.ExtraLanguagePreferenceAccess;
import io.sarl.lang.ui.internal.LangActivator;

/** Abstract implementation for the configuration block dedicated to an extra language generator.
 *
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 * @since 0.6
 */
@SuppressWarnings({"checkstyle:classdataabstractioncoupling", "checkstyle:classfanoutcomplexity"})
public abstract class AbstractGeneratorConfigurationBlock extends OptionsConfigurationBlock implements IExtraControlController {

	/** Section name.
	 */
	public static final String SETTINGS_SECTION_NAME = "ExtraLanguageGeneratorConfigurationBlock"; //$NON-NLS-1$

	/** Boolean values for preferences.
	 */
	protected static final String[] BOOLEAN_VALUES = new String[] {IPreferenceStore.TRUE, IPreferenceStore.FALSE};

	private static final String IMAGE = "icons/experimental.png"; //$NON-NLS-1$

	private static final int HEIGHT = 20;

	private static final int INDENT_AMOUNT = 32;

	private final List<IExtraControl> extraControls = Lists.newArrayList();

	private final List<TableItem> tableItems = Lists.newArrayList();

	private final boolean isExperimental;

	@Inject
	private EclipseOutputConfigurationProvider configurationProvider;

	@Inject
	private ExtraLanguagePreferenceAccess preferenceStoreAccess;

	private IPreferenceStore projectPreferenceStore;

	private String propertyPrefix;

	/** Constructor.
	 *
	 * @param isExperimental indicates if the block should display experimental elements.
	 */
	public AbstractGeneratorConfigurationBlock(boolean isExperimental) {
		this.isExperimental = isExperimental;
	}

	@Override
	public final String getPropertyPrefix() {
		if (this.propertyPrefix == null) {
			this.propertyPrefix = ExtraLanguagePreferenceAccess.getPropertyPrefix(getPreferenceID());
		}
		return this.propertyPrefix;
	}

	/** Replies if the block should display experimental elements.
	 *
	 * @return {@code true} if the experimental elements should be shown.
	 */
	public boolean isExperimental() {
		return this.isExperimental;
	}

	/** Replies the preference  access.
	 *
	 * @return the access.
	 */
	public ExtraLanguagePreferenceAccess getPreferenceAccess() {
		return this.preferenceStoreAccess;
	}

	@Override
	public void setProject(IProject project) {
		super.setProject(project);
		setPreferenceStore(createPreferenceStoreInstance(project));
	}

	@Override
	public void setPreferenceStore(IPreferenceStore preferenceStore) {
		this.projectPreferenceStore = preferenceStore;
		super.setPreferenceStore(preferenceStore);
	}

	/** Create the instance of the preference store.
	 *
	 * @param project the project.
	 * @return the preference store.
	 */
	protected IPreferenceStore createPreferenceStoreInstance(IProject project) {
		return this.preferenceStoreAccess.getWritablePreferenceStore(getProject());
	}

	/** Replies the project preference store.
	 *
	 * @return the preference store.
	 */
	protected IPreferenceStore getPreferenceStore() {
		if (this.projectPreferenceStore == null) {
			this.projectPreferenceStore = createPreferenceStoreInstance(getProject());
		}
		return this.projectPreferenceStore;
	}

	/** Add an extra control in the block.
	 *
	 * @param control the control to add.
	 */
	protected void addExtraControl(IExtraControl control) {
		this.extraControls.add(control);
	}

	@Override
	protected Control doCreateContents(Composite parent) {
		final PixelConverter pixelConverter = new PixelConverter(parent);
		setShell(parent.getShell());
		final Composite mainComp = new Composite(parent, SWT.NONE);
		mainComp.setFont(parent.getFont());
		final GridLayout layout = new GridLayout();
		layout.marginHeight = 0;
		layout.marginWidth = 0;
		mainComp.setLayout(layout);
		final Composite othersComposite = createGeneratorContent(mainComp);
		final GridData gridData = new GridData(GridData.FILL, GridData.FILL, true, true);
		gridData.heightHint = pixelConverter.convertHeightInCharsToPixels(HEIGHT);
		othersComposite.setLayoutData(gridData);
		validateSettings(null, null, null);
		return mainComp;
	}

	private Composite createGeneratorContent(Composite parent) {
		final ScrolledPageContent pageContent = new ScrolledPageContent(parent);
		final int columns = 3;
		final GridLayout layout = new GridLayout();
		layout.numColumns = columns;
		layout.marginHeight = 0;
		layout.marginWidth = 0;

		final Composite composite = pageContent.getBody();
		composite.setLayout(layout);

		if (isExperimental()) {
			createExperimentalWarningMessage(composite);
		}

		ExpandableComposite excomposite = createStyleSection(composite, Messages.AbstractGeneratorConfigurationBlock_4, columns);
		Composite othersComposite = new Composite(excomposite, SWT.NONE);
		excomposite.setClient(othersComposite);
		othersComposite.setLayout(new GridLayout(columns, false));
		createGeneralSectionItems(othersComposite);

		createAdditionalSectionsBeforeOutputConfiguration(composite);

		excomposite = createStyleSection(composite, Messages.AbstractGeneratorConfigurationBlock_6, columns);
		othersComposite = new Composite(excomposite, SWT.NONE);
		excomposite.setClient(othersComposite);
		othersComposite.setLayout(new GridLayout(columns, false));
		createOutputSectionItems(othersComposite, getOutputConfiguration());

		createAdditionalSectionsAfterOutputConfiguration(composite);

		excomposite = createStyleSection(composite, Messages.AbstractGeneratorConfigurationBlock_5, columns);
		othersComposite = SWTFactory.createComposite(excomposite, parent.getFont(), 2, 1, GridData.FILL_BOTH);
		excomposite.setClient(othersComposite);
		createTypeConversionSectionItems(othersComposite);

		excomposite = createStyleSection(composite, Messages.AbstractGeneratorConfigurationBlock_7, columns);
		othersComposite = SWTFactory.createComposite(excomposite, parent.getFont(), 2, 1, GridData.FILL_BOTH);
		excomposite.setClient(othersComposite);
		createFeatureConversionSectionItems(othersComposite);

		createAdditionalSectionsAfterTypeConversionTables(composite);

		registerKey(getIsProjectSpecificPropertyKey(getPropertyPrefix()));
		IDialogSettings section = getDialogSettings().getSection(SETTINGS_SECTION_NAME);
		if (section == null) {
			section = getDialogSettings().addNewSection(SETTINGS_SECTION_NAME);
		}
		restoreSectionExpansionStates(section);
		return pageContent;
	}

	/** Replies the output configuration associated to the extra language generator.
	 *
	 * @return the output configuration.
	 */
	protected OutputConfiguration getOutputConfiguration() {
		final Set<OutputConfiguration> outputConfigurations = this.configurationProvider.getOutputConfigurations(getProject());
		final String expectedName = ExtraLanguageOutputConfigurations.createOutputConfigurationName(getPreferenceID());
		return Iterables.find(outputConfigurations, it -> expectedName.equals(it.getName()));
	}

	/**
	 * Returns the dialog settings for this UI plug-in.
	 * The dialog settings is used to hold persistent state data for the various
	 * wizards and dialogs of this plug-in in the context of a workbench.
	 *
	 * @return the dialog settings
	 */
	public abstract IDialogSettings getDialogSettings();

	/** Replies the identifier of the container of the generator's preferences.
	 *
	 * @return the identifier.
	 */
	public abstract String getPreferenceID();

	/** Add additional sections in the dialog before the controls for output configuration.
	 *
	 * @param composite the parent.
	 */
	protected void createAdditionalSectionsBeforeOutputConfiguration(Composite composite) {
		//
	}

	/** Add additional sections in the dialog between the controls for output configuration and
	 * the ones for type conversion.
	 *
	 * @param composite the parent.
	 */
	protected void createAdditionalSectionsAfterOutputConfiguration(Composite composite) {
		//
	}

	/** Add additional sections in the dialog after the controls for type conversions.
	 *
	 * @param composite the parent.
	 */
	protected void createAdditionalSectionsAfterTypeConversionTables(Composite composite) {
		//
	}

	/** Replies the image that represents the target language.
	 *
	 * @return the target language's image.
	 */
	public abstract Image getTargetLanguageImage();

	/** Create the items for the "Type Conversion" section.
	 *
	 * @param parentComposite the parent.
	 */
	protected void createTypeConversionSectionItems(Composite parentComposite) {
		final TypeConversionTable typeConversionTable = new TypeConversionTable(
				this, getTargetLanguageImage(),
				getPreferenceStore(), getPreferenceID());
		typeConversionTable.doCreate(parentComposite, getDialogSettings());
		makeScrollableCompositeAware(typeConversionTable.getControl());
		addExtraControl(typeConversionTable);
	}

	private static ScrolledPageContent getParentScrolledComposite(Control control) {
		Control parent = control.getParent();
		while (!(parent instanceof ScrolledPageContent) && parent != null) {
			parent = parent.getParent();
		}
		if (parent instanceof ScrolledPageContent) {
			return (ScrolledPageContent) parent;
		}
		return null;
	}

	/** Make the given control awaer of the scrolling events.
	 *
	 * @param control the control to adapt to.
	 */
	protected static void makeScrollableCompositeAware(Control control) {
		final ScrolledPageContent parentScrolledComposite = getParentScrolledComposite(control);
		if (parentScrolledComposite != null) {
			parentScrolledComposite.adaptChild(control);
		}
	}

	/** Create the items for the "Feature Conversion" section.
	 *
	 * @param parentComposite the parent.
	 */
	protected void createFeatureConversionSectionItems(Composite parentComposite) {
		final FeatureNameConversionTable typeConversionTable = new FeatureNameConversionTable(
				this, getTargetLanguageImage(),
				getPreferenceStore(), getPreferenceID());
		typeConversionTable.doCreate(parentComposite, getDialogSettings());
		makeScrollableCompositeAware(typeConversionTable.getControl());
		addExtraControl(typeConversionTable);
	}

	/** Replies the text for the "enabling/disabling" label.
	 *
	 * @return the text.
	 */
	protected abstract String getActivationText();

	/** Create the "experimental" warning message.
	 *
	 * @param composite the parent.
	 */
	protected void createExperimentalWarningMessage(Composite composite) {
		final Label dangerIcon = new Label(composite, SWT.WRAP);
		dangerIcon.setImage(getImage(IMAGE));
		final GridData labelLayoutData = new GridData();
		labelLayoutData.horizontalIndent = 0;
		dangerIcon.setLayoutData(labelLayoutData);
	}

	/** Replies the image.
	 *
	 * @param imagePath the image path.
	 * @return the image.
	 */
	protected final Image getImage(String imagePath) {
		final ImageDescriptor descriptor = getImageDescriptor(imagePath);
		if (descriptor == null) {
			return null;
		}
		return descriptor.createImage();
	}

	/** Replies the image descriptor.
	 *
	 * @param imagePath the image path.
	 * @return the image descriptor.
	 */
	@SuppressWarnings("static-method")
	protected ImageDescriptor getImageDescriptor(String imagePath) {
		final LangActivator activator = LangActivator.getInstance();
		final ImageRegistry registry = activator.getImageRegistry();
		ImageDescriptor descriptor = registry.getDescriptor(imagePath);
		if (descriptor == null) {
			descriptor = ResourceLocator.imageDescriptorFromBundle(activator.getBundle().getSymbolicName(), imagePath).orElse(null);
			if (descriptor != null) {
				registry.put(imagePath, descriptor);
			}
		}
		return descriptor;
	}

	/** Create the items for the "General" section.
	 *
	 * @param composite the parent.
	 */
	protected void createGeneralSectionItems(Composite composite) {
		addCheckBox(composite, getActivationText(),
				ExtraLanguagePreferenceAccess.getPrefixedKey(getPreferenceID(),
						ExtraLanguagePreferenceAccess.ENABLED_PROPERTY),
				BOOLEAN_VALUES, 0);
	}

	@Override
	protected Job getBuildJob(IProject project) {
		final Job buildJob = new OptionsConfigurationBlock.BuildJob(Messages.AbstractGeneratorConfigurationBlock_3, project);
		buildJob.setRule(ResourcesPlugin.getWorkspace().getRuleFactory().buildRule());
		buildJob.setUser(true);
		return buildJob;
	}

	@Override
	protected String[] getFullBuildDialogStrings(boolean workspaceSettings) {
		final String message;
		if (workspaceSettings) {
			message = Messages.AbstractGeneratorConfigurationBlock_1;
		} else {
			message = Messages.AbstractGeneratorConfigurationBlock_2;
		}
		return new String[] {Messages.AbstractGeneratorConfigurationBlock_0, message};
	}

	@Override
	protected void validateSettings(String changedKey, String oldValue, String newValue) {
		//
	}

	@Override
	public void dispose() {
		IDialogSettings section = getDialogSettings().getSection(SETTINGS_SECTION_NAME);
		if (section == null) {
			section = getDialogSettings().addNewSection(SETTINGS_SECTION_NAME);
		}
		storeSectionExpansionStates(section);
		super.dispose();
	}

	@Override
	public void controlChanged(Widget widget) {
		// Change the visibility
		super.controlChanged(widget);
	}

	@Override
	public void controlChanged(String preferenceKey, String preferenceValue) {
		final String oldValue = setValue(preferenceKey, preferenceValue);
		validateSettings(preferenceKey, oldValue, preferenceValue);
	}

	@Override
	public void textChanged(Text textControl) {
		// Change the visibility
		super.textChanged(textControl);
	}

	@Override
	public String getValue(String key) {
		// Change the visibility
		return super.getValue(key);
	}

	@Override
	public void registerKey(String key) {
		// Change the visibility
		super.registerKey(key);
	}

	@Override
	public void performDefaults() {
		super.performDefaults();
	}

	/** Create the items for the "Output" section.
	 *
	 * @param composite the parent.
	 * @param outputConfiguration the output configuration.
	 */
	protected void createOutputSectionItems(Composite composite, OutputConfiguration outputConfiguration) {
		final Text defaultDirectoryField = addTextField(composite,
				org.eclipse.xtext.builder.preferences.Messages.OutputConfigurationPage_Directory,
				BuilderPreferenceAccess.getKey(outputConfiguration,
						EclipseOutputConfigurationProvider.OUTPUT_DIRECTORY), 0, 0);
		addCheckBox(composite,
				org.eclipse.xtext.builder.preferences.Messages.OutputConfigurationPage_CreateDirectory,
				BuilderPreferenceAccess.getKey(outputConfiguration,
						EclipseOutputConfigurationProvider.OUTPUT_CREATE_DIRECTORY), BOOLEAN_VALUES, 0);
		addCheckBox(composite,
				org.eclipse.xtext.builder.preferences.Messages.OutputConfigurationPage_OverrideExistingResources,
				BuilderPreferenceAccess.getKey(outputConfiguration,
						EclipseOutputConfigurationProvider.OUTPUT_OVERRIDE), BOOLEAN_VALUES, 0);
		addCheckBox(composite,
				org.eclipse.xtext.builder.preferences.Messages.OutputConfigurationPage_CreatesDerivedResources,
				BuilderPreferenceAccess.getKey(outputConfiguration,
						EclipseOutputConfigurationProvider.OUTPUT_DERIVED), BOOLEAN_VALUES, 0);
		addCheckBox(composite,
				org.eclipse.xtext.builder.preferences.Messages.OutputConfigurationPage_CleanupDerivedResources,
				BuilderPreferenceAccess.getKey(outputConfiguration,
						EclipseOutputConfigurationProvider.OUTPUT_CLEANUP_DERIVED), BOOLEAN_VALUES, 0);
		addCheckBox(composite,
				org.eclipse.xtext.builder.preferences.Messages.OutputConfigurationPage_CleanDirectory,
				BuilderPreferenceAccess.getKey(outputConfiguration,
						EclipseOutputConfigurationProvider.OUTPUT_CLEAN_DIRECTORY), BOOLEAN_VALUES, 0);
		final Button installAsPrimaryButton = addCheckBox(composite,
				org.eclipse.xtext.builder.preferences.Messages.BuilderConfigurationBlock_InstallDslAsPrimarySource,
				BuilderPreferenceAccess.getKey(outputConfiguration,
						EclipseOutputConfigurationProvider.INSTALL_DSL_AS_PRIMARY_SOURCE), BOOLEAN_VALUES, 0);
		final Button hideLocalButton = addCheckBox(composite,
				org.eclipse.xtext.builder.preferences.Messages.BuilderConfigurationBlock_hideSyntheticLocalVariables,
				BuilderPreferenceAccess.getKey(outputConfiguration,
						EclipseOutputConfigurationProvider.HIDE_LOCAL_SYNTHETIC_VARIABLES), BOOLEAN_VALUES, 0);
		hideLocalButton.setEnabled(installAsPrimaryButton.getSelection());
		installAsPrimaryButton.addSelectionListener(new SelectionAdapter() {
			@Override
			public void widgetSelected(SelectionEvent exception) {
				hideLocalButton.setEnabled(installAsPrimaryButton.getSelection());
			}
		});
		final GridData hideLocalButtonData = new GridData();
		hideLocalButtonData.horizontalIndent = INDENT_AMOUNT;
		hideLocalButton.setLayoutData(hideLocalButtonData);
		addCheckBox(composite,
				org.eclipse.xtext.builder.preferences.Messages.OutputConfigurationPage_KeepLocalHistory,
				BuilderPreferenceAccess.getKey(outputConfiguration,
						EclipseOutputConfigurationProvider.OUTPUT_KEEP_LOCAL_HISTORY), BOOLEAN_VALUES, 0);

		if (getProject() != null && !outputConfiguration.getSourceFolders().isEmpty()) {
			final Button outputPerSourceButton = addCheckBox(composite,
					org.eclipse.xtext.builder.preferences.Messages.OutputConfigurationPage_UseOutputPerSourceFolder,
					BuilderPreferenceAccess.getKey(outputConfiguration,
							EclipseOutputConfigurationProvider.USE_OUTPUT_PER_SOURCE_FOLDER), BOOLEAN_VALUES, 0);
			final Table table = createOutputFolderTable(composite, outputConfiguration, defaultDirectoryField);
			table.setVisible(outputPerSourceButton.getSelection());
			outputPerSourceButton.addSelectionListener(new SelectionAdapter() {
				@Override
				public void widgetSelected(SelectionEvent exception) {
					table.setVisible(outputPerSourceButton.getSelection());
				}
			});
		}
	}

	@SuppressWarnings("checkstyle:magicnumber")
	private Table createOutputFolderTable(Composite othersComposite, OutputConfiguration outputConfiguration,
			Text defaultDirectoryField) {
		final Table table = new Table(othersComposite, SWT.BORDER | SWT.FULL_SELECTION);
		new TableColumn(table, SWT.NONE).setText(
				org.eclipse.xtext.builder.preferences.Messages.OutputConfigurationPage_IgnoreSourceFolder);
		new TableColumn(table, SWT.NONE).setText(
				org.eclipse.xtext.builder.preferences.Messages.OutputConfigurationPage_SourceFolder);
		new TableColumn(table, SWT.NONE).setText(
				org.eclipse.xtext.builder.preferences.Messages.OutputConfigurationPage_OutputDirectory);
		table.getColumn(0).setWidth(75);
		table.getColumn(1).setWidth(200);
		table.getColumn(2).setWidth(200);
		table.pack();
		table.setHeaderVisible(true);
		table.setLinesVisible(true);

		for (final String source : outputConfiguration.getSourceFolders()) {
			final String outputForSourceFolderKey = BuilderPreferenceAccess.getOutputForSourceFolderKey(outputConfiguration, source);
			registerKey(outputForSourceFolderKey);
			final String ignoreSourceFolderKey = BuilderPreferenceAccess.getIgnoreSourceFolderKey(outputConfiguration, source);
			registerKey(ignoreSourceFolderKey);
			final String defaultOutputDirectoryKey = BuilderPreferenceAccess.getKey(outputConfiguration,
					EclipseOutputConfigurationProvider.OUTPUT_DIRECTORY);
			final TableItemData data = new TableItemData(source, outputForSourceFolderKey, ignoreSourceFolderKey,
					defaultOutputDirectoryKey);

			final TableItem item = new TableItem(table, SWT.NONE, 0);
			this.tableItems.add(item);
			item.setData(data);
			refreshItem(item);

			final TableEditor directoryEditor = new TableEditor(table);
			directoryEditor.grabHorizontal = true;
			table.addSelectionListener(new SelectionAdapter() {
				@SuppressWarnings("synthetic-access")
				@Override
				public void widgetSelected(SelectionEvent exception) {
					if (exception.item != item) {
						return;
					}
					if (isIgnored(item)) {
						return;
					}
					final Control oldDirectoryField = directoryEditor.getEditor();
					if (oldDirectoryField != null) {
						oldDirectoryField.dispose();
					}

					final Text directoryField = new Text(table, SWT.NONE);
					directoryField.setText(getOutputDirectory(item));
					directoryField.addModifyListener(new ModifyListener() {
						@Override
						public void modifyText(ModifyEvent me) {
							setValue(data.getOutputDirectoryKey(), directoryField.getText());
							refreshItem(item);
						}
					});
					directoryField.addFocusListener(new FocusAdapter() {
						@Override
						public void focusLost(FocusEvent exception) {
							directoryField.dispose();
						}
					});
					directoryField.selectAll();
					directoryField.setFocus();
					directoryEditor.setEditor(directoryField, item, 2);
				}
			});
			final TableEditor ignoreEditor = new TableEditor(table);
			ignoreEditor.grabHorizontal = true;
			final Button ignoreField = new Button(table, SWT.CHECK);
			ignoreEditor.setEditor(ignoreField, item, 0);
			ignoreField.setSelection(isIgnored(item));
			ignoreField.addSelectionListener(new SelectionAdapter() {
				@SuppressWarnings("synthetic-access")
				@Override
				public void widgetSelected(SelectionEvent exception) {
					setValue(data.getIgnoreKey(),
							String.valueOf(ignoreField.getSelection()));
					refreshItem(item);
				}
			});
			defaultDirectoryField.addModifyListener(new ModifyListener() {
				@SuppressWarnings("synthetic-access")
				@Override
				public void modifyText(ModifyEvent exception) {
					refreshItem(item);
				}
			});
		}
		return table;
	}

	private void refreshItem(final TableItem item) {
		final TableItemData data = (TableItemData) item.getData();
		item.setText(1, data.getSourceFolder());
		final String outputDirectory = getOutputDirectory(item);
		if ("".equals(outputDirectory)) { //$NON-NLS-1$
			item.setForeground(2, Display.getCurrent().getSystemColor(SWT.COLOR_GRAY));
			item.setText(2, getValue(data.getDefaultOutputDirectoryKey()));
		} else {
			item.setForeground(2, null);
			item.setText(2, outputDirectory);
		}
		if (isIgnored(item)) {
			item.setForeground(Display.getCurrent().getSystemColor(SWT.COLOR_GRAY));
		} else {
			item.setForeground(null);
		}
	}

	private String getOutputDirectory(TableItem item) {
		final TableItemData data = (TableItemData) item.getData();
		return getValue(data.getOutputDirectoryKey());
	}

	private boolean isIgnored(TableItem item)  {
		final TableItemData data = (TableItemData) item.getData();
		return Boolean.parseBoolean(getValue(data.getIgnoreKey()));
	}

	@Override
	protected void updateControls() {
		super.updateControls();
		for (final TableItem item : this.tableItems) {
			refreshItem(item);
		}
		for (final IExtraControl control : this.extraControls) {
			control.updateControls();
		}
	}

	/** Internal data.
	 *
	 * @author $Author: sgalland$
	 * @version $FullVersion$
	 * @mavengroupid $GroupId$
	 * @mavenartifactid $ArtifactId$
	 */
	private static class TableItemData {
		private String sourceFolder;

		private String outputDirectoryKey;

		private String ignoreKey;

		private String defaultOutputDirectoryKey;

		/** Constructor.
		 *
		 * @param sourceFolder the source folder.
		 * @param outputDirectoryKey the output folder key.
		 * @param ignoreKey the ignore flag key.
		 * @param defaultOutputDirectoryKey the default output key.
		 */
		TableItemData(String sourceFolder, String outputDirectoryKey, String ignoreKey,
				String defaultOutputDirectoryKey) {
			this.sourceFolder = sourceFolder;
			this.outputDirectoryKey = outputDirectoryKey;
			this.ignoreKey = ignoreKey;
			this.defaultOutputDirectoryKey = defaultOutputDirectoryKey;
		}

		public String getSourceFolder() {
			return this.sourceFolder;
		}

		public String getOutputDirectoryKey() {
			return this.outputDirectoryKey;
		}

		public String getIgnoreKey() {
			return this.ignoreKey;
		}

		public String getDefaultOutputDirectoryKey() {
			return this.defaultOutputDirectoryKey;
		}

	}

}
