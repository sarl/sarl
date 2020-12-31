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

import java.lang.ref.WeakReference;
import java.util.ArrayList;
import java.util.Iterator;
import java.util.LinkedList;
import java.util.List;
import java.util.Objects;

import org.eclipse.debug.internal.ui.SWTFactory;
import org.eclipse.jdt.core.IType;
import org.eclipse.jdt.core.search.IJavaSearchConstants;
import org.eclipse.jdt.internal.ui.JavaUIMessages;
import org.eclipse.jdt.internal.ui.dialogs.OpenTypeSelectionDialog;
import org.eclipse.jdt.ui.ISharedImages;
import org.eclipse.jdt.ui.JavaUI;
import org.eclipse.jface.dialogs.IDialogConstants;
import org.eclipse.jface.dialogs.IDialogSettings;
import org.eclipse.jface.preference.IPreferenceStore;
import org.eclipse.jface.viewers.BaseLabelProvider;
import org.eclipse.jface.viewers.CellEditor;
import org.eclipse.jface.viewers.DialogCellEditor;
import org.eclipse.jface.viewers.ICellModifier;
import org.eclipse.jface.viewers.IStructuredContentProvider;
import org.eclipse.jface.viewers.IStructuredSelection;
import org.eclipse.jface.viewers.ITableLabelProvider;
import org.eclipse.jface.viewers.StructuredSelection;
import org.eclipse.jface.viewers.TableViewer;
import org.eclipse.jface.viewers.TextCellEditor;
import org.eclipse.jface.viewers.Viewer;
import org.eclipse.jface.viewers.ViewerComparator;
import org.eclipse.swt.SWT;
import org.eclipse.swt.events.KeyAdapter;
import org.eclipse.swt.events.KeyEvent;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.graphics.Image;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Display;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Table;
import org.eclipse.swt.widgets.TableColumn;
import org.eclipse.swt.widgets.TableItem;
import org.eclipse.ui.PlatformUI;
import org.eclipse.xtext.util.Strings;
import org.eclipse.xtext.xbase.lib.Pair;

import io.sarl.lang.ui.extralanguage.preferences.ExtraLanguagePreferenceAccess;

/** Abstract implementation of a table for conversion definition.
 *
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 * @since 0.6
 */
@SuppressWarnings({"checkstyle:classdataabstractioncoupling", "checkstyle:classfanoutcomplexity"})
public abstract class AbstractConversionTable extends AbstractExtraControl {

	/** Propery name for the source column.
	 */
	protected static final String SOURCE_COLUMN_PROPERTY = "source"; //$NON-NLS-1$

	/** Propery name for the target column.
	 */
	protected static final String TARGET_COLUMN_PROPERTY = "target"; //$NON-NLS-1$

	private static final int WIDTH_HINT = 350;

	private static final int HEIGHT_HINT = 250;

	private static final int DEFAULT_WIDTH = WIDTH_HINT / 2;

	private final boolean isSortedElements;

	private final LinkedList<ConversionMapping> conversions = new LinkedList<>();

	private Table table;

	private TableViewer list;

	private CellEditor[] editors;

	private ICellModifier cellModifier;

	private Column sort = Column.SOURCE;

	private Button removeButton;

	private Button clearButton;

	private Button moveTopButton;

	private Button moveUpButton;

	private Button moveDownButton;

	private Button moveBottomButton;

	/** Constructor.
	 *
	 * @param controller the controller.
	 * @param languageImage for the target language (16x16).
	 * @param preferenceStore the preference store to be used.
	 * @param preferenceContainerID the identifier of the generator's preference container.
	 * @param sortedElements indicates if the elements in the table should be sorted.
	 */
	AbstractConversionTable(IExtraControlController controller, Image languageImage,
			IPreferenceStore preferenceStore, String preferenceContainerID, boolean sortedElements) {
		super(controller, languageImage, preferenceStore, preferenceContainerID);
		this.isSortedElements = sortedElements;
	}

	/** Replies the label of the source column.
	 *
	 * @return the label.
	 */
	protected abstract String getSourceColumnLabel();

	/** Replies the label of the target column.
	 *
	 * @return the label.
	 */
	protected abstract String getTargetColumnLabel();

	/** Replies the editor for the source column.
	 *
	 * @return the editor, or {@code null}.
	 */
	protected abstract CellEditor createSourceColumnEditor();

	/** Replies the editor for the target column.
	 *
	 * @return the editor, or {@code null}.
	 */
	protected abstract CellEditor createTargetColumnEditor();

	/** Replies the cell modifier for all the columns.
	 *
	 * @return the cell modifier, or {@code null}.
	 */
	protected ICellModifier createCellModifier() {
		return new CellModifier(this, SOURCE_COLUMN_PROPERTY, TARGET_COLUMN_PROPERTY);
	}

	/** Replies the content provider for all the cells.
	 *
	 * @return the cell content provider, never {@code null}.
	 */
	protected IStructuredContentProvider createContentProvider() {
		return new ContentProvider();
	}

	/** Replies the label provider for all the cells.
	 *
	 * @return the label content provider, never {@code null}.
	 */
	protected ITableLabelProvider createLabelProvider() {
		return new LabelProvider(getLanguageImage());
	}

	/** Replies the message to display as introduction.
	 *
	 * @return the introduction message, or {@code null} if there is no introduction message.
	 */
	@SuppressWarnings("static-method")
	protected String getIntroductionLabel() {
		return null;
	}

	/** Create the table.
	 *
	 * @param parentComposite the parent.
	 * @param settings the dialog settings.
	 */
	public void doCreate(Composite parentComposite, IDialogSettings settings) {
		// Introduction
		final String introductionMessage = getIntroductionLabel();
		if (!Strings.isEmpty(introductionMessage)) {
			final GridData gd = new GridData();
			gd.grabExcessHorizontalSpace = true;
			gd.horizontalAlignment = GridData.FILL_HORIZONTAL;
			gd.horizontalSpan = 2;
			final Label textWidget = new Label(parentComposite, SWT.WRAP);
			textWidget.setLayoutData(gd);
			textWidget.setText(introductionMessage);
			textWidget.setFont(parentComposite.getFont());
		}
		//
		final GridData gd = new GridData(GridData.FILL_BOTH);
		gd.heightHint = HEIGHT_HINT;
		gd.widthHint = WIDTH_HINT;
		this.table = new Table(parentComposite, SWT.BORDER | SWT.MULTI | SWT.FULL_SELECTION);
		this.table.setLayoutData(gd);
		this.table.setFont(parentComposite.getFont());
		this.table.setHeaderVisible(true);
		this.table.setLinesVisible(true);
		this.table.setLinesVisible(true);

		TableColumn column = new TableColumn(this.table, SWT.NULL);
		column.setText(getSourceColumnLabel());
		if (!this.isSortedElements) {
			column.addSelectionListener(new SelectionAdapter() {
				@SuppressWarnings("synthetic-access")
				@Override
				public void widgetSelected(SelectionEvent event) {
					sortBySourceColumn();
					AbstractConversionTable.this.list.refresh(true);
				}
			});
		}
		column.setWidth(DEFAULT_WIDTH);

		column = new TableColumn(this.table, SWT.NULL);
		column.setText(getTargetColumnLabel());
		if (!this.isSortedElements) {
			column.addSelectionListener(new SelectionAdapter() {
				@SuppressWarnings("synthetic-access")
				@Override
				public void widgetSelected(SelectionEvent event) {
					sortByTargetColumn();
					AbstractConversionTable.this.list.refresh(true);
				}
			});
		}
		column.setWidth(DEFAULT_WIDTH);

		this.list = new TableViewer(this.table);
		this.list.setLabelProvider(createLabelProvider());
		this.list.setContentProvider(createContentProvider());
		this.list.setUseHashlookup(true);

		this.list.addSelectionChangedListener(evt -> enableButtons());

		this.table.addKeyListener(new KeyAdapter() {
			@SuppressWarnings("synthetic-access")
			@Override
			public void keyPressed(KeyEvent event) {
				if (event.character == SWT.DEL && event.stateMask == 0) {
					if (AbstractConversionTable.this.removeButton.isEnabled()) {
						removeCurrentTypeConversion();
					}
				}
			}
		});

		this.editors = new CellEditor[2];
		this.editors[0] = createSourceColumnEditor();
		this.editors[1] = createTargetColumnEditor();
		this.list.setCellEditors(this.editors);

		this.cellModifier = createCellModifier();
		this.list.setColumnProperties(new String[] {SOURCE_COLUMN_PROPERTY, TARGET_COLUMN_PROPERTY});
		this.list.setCellModifier(this.cellModifier);

		final Composite buttons = SWTFactory.createComposite(parentComposite, parentComposite.getFont(), 1, 1,
				GridData.VERTICAL_ALIGN_BEGINNING, 0, 0);

		final Button addButton = SWTFactory.createPushButton(buttons, Messages.AbstractConversionTable_0, null);
		addButton.addListener(SWT.Selection, evt -> addTypeConversion(null, null, true));

		this.removeButton = SWTFactory.createPushButton(buttons, Messages.AbstractConversionTable_1, null);
		this.removeButton.addListener(SWT.Selection, evt -> removeCurrentTypeConversion());

		this.clearButton = SWTFactory.createPushButton(buttons, Messages.AbstractConversionTable_2, null);
		this.clearButton.addListener(SWT.Selection, evt -> removeAllTypeConversions());

		if (this.isSortedElements) {
			this.moveTopButton = SWTFactory.createPushButton(buttons, Messages.AbstractConversionTable_3, null);
			this.moveTopButton.addListener(SWT.Selection, evt -> moveSelectionTop());

			this.moveUpButton = SWTFactory.createPushButton(buttons, Messages.AbstractConversionTable_4, null);
			this.moveUpButton.addListener(SWT.Selection, evt -> moveSelectionUp());

			this.moveDownButton = SWTFactory.createPushButton(buttons, Messages.AbstractConversionTable_5, null);
			this.moveDownButton.addListener(SWT.Selection, evt -> moveSelectionDown());

			this.moveBottomButton = SWTFactory.createPushButton(buttons, Messages.AbstractConversionTable_6, null);
			this.moveBottomButton.addListener(SWT.Selection, evt -> moveSelectionBottom());
		}

		SWTFactory.createVerticalSpacer(parentComposite, 1);

		// Register the preference key(s)
		registerKey(getPreferenceKey());

		// Fill the content of the table
		updateControls();

		// Enable/disable the buttons
		enableButtons();

		// By default, sort by name
		restoreColumnSettings(settings);
	}

	/** Replies the UI control that is embedded in this object.
	 *
	 * @return the control widget.
	 */
	public Table getControl() {
		return this.table;
	}

	/** Create a cell editor that enables to select a class.
	 *
	 * @return the cell editor.
	 */
	protected CellEditor createClassCellEditor() {
		return new DialogCellEditor(getControl()) {
			@Override
			protected Object openDialogBox(Control cellEditorWindow) {
				final OpenTypeSelectionDialog dialog = new OpenTypeSelectionDialog(
						getControl().getShell(),
						false,
						PlatformUI.getWorkbench().getProgressService(),
						null,
						IJavaSearchConstants.TYPE);
				dialog.setTitle(JavaUIMessages.OpenTypeAction_dialogTitle);
				dialog.setMessage(JavaUIMessages.OpenTypeAction_dialogMessage);
				final int result = dialog.open();
				if (result != IDialogConstants.OK_ID) {
					return null;
				}
				final Object[] types = dialog.getResult();
				if (types == null || types.length != 1 || !(types[0] instanceof IType)) {
					return null;
				}
				final IType type = (IType) types[0];
				final String name = type.getFullyQualifiedName();
				return Strings.emptyIfNull(name);
			}
		};
	}

	/** Create a cell editor that enables to type text.
	 *
	 * @return the cell editor.
	 */
	protected CellEditor createTextCellEditor() {
		return new TextCellEditor(getControl());
	}

	/** Replies the key for saving the conversions into the preferences.
	 *
	 * @return the key.
	 */
	protected abstract String getPreferenceKey();

	@Override
	public void updateControls() {
		final IExtraControlController ctrl = getController();
		final String preferenceName = getPreferenceKey();
		final String rawValue = Strings.emptyIfNull(ctrl.getValue(preferenceName));
		final List<Pair<String, String>> conversions = new ArrayList<>();
		ExtraLanguagePreferenceAccess.parseConverterPreferenceValue(rawValue,
			(source, target) -> conversions.add(new Pair<>(source, target)));
		setTypeConversions(conversions, false);
	}

	/**
	 * Enables the type conversion buttons based on selected items counts in the viewer.
	 */
	private void enableButtons() {
		final int itemCount = this.list.getTable().getItemCount();
		final boolean hasElement = itemCount > 0;
		IStructuredSelection selection;
		if (hasElement) {
			selection = this.list.getStructuredSelection();
			final int selectionCount = selection.size();
			if (selectionCount <= 0 || selectionCount > itemCount) {
				selection = null;
			}
		} else {
			selection = null;
		}
		this.removeButton.setEnabled(selection != null);
		this.clearButton.setEnabled(hasElement);
		if (this.isSortedElements) {
			final Object firstElement = selection != null ? this.list.getTable().getItem(0).getData() : null;
			final Object lastElement = selection != null ? this.list.getTable().getItem(this.list.getTable().getItemCount() - 1).getData() : null;
			final boolean isNotFirst = firstElement != null && selection != null && firstElement != selection.getFirstElement();
			final boolean isNotLast = lastElement != null && selection != null && lastElement != selection.getFirstElement();
			this.moveTopButton.setEnabled(isNotFirst);
			this.moveUpButton.setEnabled(isNotFirst);
			this.moveDownButton.setEnabled(isNotLast);
			this.moveBottomButton.setEnabled(isNotLast);
		}
	}

	/**
	 * Sets the type conversions to be displayed in this block.
	 *
	 * @param typeConversions the type conversions.
	 * @param notifyController indicates if the controller should be notified.
	 */
	protected void setTypeConversions(List<Pair<String, String>> typeConversions, boolean notifyController) {
		this.conversions.clear();
		if (typeConversions != null) {
			for (final Pair<String, String> entry : typeConversions) {
				this.conversions.add(new ConversionMapping(entry.getKey(), entry.getValue()));
			}
		}
		this.list.setInput(this.conversions);
		refreshListUI();
		if (notifyController) {
			preferenceValueChanged();
		}
	}

	/** Add a type conversion.
	 *
	 * @param javaType the name of the java type.
	 * @param targetType the name of the target type.
	 * @param updateSelection indicates if the selection should be updated.
	 */
	protected void addTypeConversion(String javaType, String targetType, boolean updateSelection) {
		final ConversionMapping entry = new ConversionMapping(javaType, targetType);
		this.conversions.add(entry);
		//refresh from model
		refreshListUI();
		if (updateSelection) {
			this.list.setSelection(new StructuredSelection(entry));
		}
		//ensure labels are updated
		if (!this.list.isBusy()) {
			this.list.refresh(true);
		}
		enableButtons();
		preferenceValueChanged();
	}

	private void preferenceValueChanged() {
		final String preferenceValue = ExtraLanguagePreferenceAccess.toConverterPreferenceValue(
				new TypeConversionIterator());
		getController().controlChanged(
				getPreferenceKey(),
				preferenceValue);
	}

	/** Remove the current type conversion.
	 */
	@SuppressWarnings("unchecked")
	protected void removeCurrentTypeConversion() {
		final IStructuredSelection selection = this.list.getStructuredSelection();
		final String[] types = new String[selection.size()];
		final Iterator<ConversionMapping> iter = selection.iterator();
		int i = 0;
		while (iter.hasNext()) {
			types[i] = iter.next().getSource();
			i++;
		}
		removeTypeConversions(types);
	}

	/** Move the selection at the top.
	 */
	protected void moveSelectionTop() {
		final IStructuredSelection selection = this.list.getStructuredSelection();
		final int index = this.conversions.indexOf(selection.getFirstElement());
		if (index > 0) {
			final int endIndex = index + selection.size() - 1;
			for (int i = 0; i < selection.size(); ++i) {
				final ConversionMapping next = this.conversions.remove(endIndex);
				this.conversions.addFirst(next);
			}
			refreshListUI();
			this.list.refresh(true);
			enableButtons();
			preferenceValueChanged();
		}
	}

	/** Move the selection up.
	 */
	protected void moveSelectionUp() {
		final IStructuredSelection selection = this.list.getStructuredSelection();
		final int index = this.conversions.indexOf(selection.getFirstElement());
		if (index > 0) {
			final ConversionMapping previous = this.conversions.remove(index - 1);
			this.conversions.add(index + selection.size() - 1, previous);
			refreshListUI();
			this.list.refresh(true);
			enableButtons();
			preferenceValueChanged();
		}
	}

	/** Move the selection down.
	 */
	protected void moveSelectionDown() {
		final IStructuredSelection selection = this.list.getStructuredSelection();
		final int index = this.conversions.indexOf(selection.getFirstElement());
		if (index >= 0 && (index + selection.size()) < this.conversions.size()) {
			final ConversionMapping next = this.conversions.remove(index + selection.size());
			this.conversions.add(index, next);
			refreshListUI();
			this.list.refresh(true);
			enableButtons();
			preferenceValueChanged();
		}
	}

	/** Move the selection at the bottom.
	 */
	protected void moveSelectionBottom() {
		final IStructuredSelection selection = this.list.getStructuredSelection();
		final int index = this.conversions.indexOf(selection.getFirstElement());
		if (index >= 0 && (index + selection.size()) < this.conversions.size()) {
			for (int i = 0; i < selection.size(); ++i) {
				final ConversionMapping previous = this.conversions.remove(index);
				this.conversions.addLast(previous);
			}
			refreshListUI();
			this.list.refresh(true);
			enableButtons();
			preferenceValueChanged();
		}
	}

	/** Remove the given type conversions.
	 *
	 * @param types the type conversions to be removed.
	 */
	protected void removeTypeConversions(String... types) {
		for (final String type : types) {
			final Iterator<ConversionMapping> iterator = this.conversions.iterator();
			while (iterator.hasNext()) {
				final ConversionMapping pair = iterator.next();
				if (Strings.equal(pair.getSource(), type)) {
					iterator.remove();
					break;
				}
			}
		}
		refreshListUI();
		this.list.refresh(true);
		enableButtons();
		preferenceValueChanged();
	}

	/** Remove all the current type conversions.
	 */
	protected void removeAllTypeConversions() {
		this.conversions.clear();
		refreshListUI();
		this.list.refresh(true);
		enableButtons();
		preferenceValueChanged();
	}

	/** Refresh the UI list of type conversions.
	 */
	protected void refreshListUI() {
		final Display display = Display.getDefault();
		if (display.getThread().equals(Thread.currentThread())) {
			if (!this.list.isBusy()) {
				this.list.refresh();
			}
		} else {
			display.syncExec(new Runnable() {
				@SuppressWarnings("synthetic-access")
				@Override
				public void run() {
					if (!AbstractConversionTable.this.list.isBusy()) {
						AbstractConversionTable.this.list.refresh();
					}
				}
			});
		}
	}

	/**
	 * Sorts the type conversions by java type.
	 */
	private void sortBySourceColumn() {
		this.list.setComparator(new ViewerComparator() {
			@Override
			public int compare(Viewer viewer, Object e1, Object e2) {
				if (e1 != null  && e2 != null) {
					return e1.toString().compareToIgnoreCase(e2.toString());
				}
				return super.compare(viewer, e1, e2);
			}

			@Override
			public boolean isSorterProperty(Object element, String property) {
				return true;
			}
		});
		this.sort = Column.SOURCE;
	}

	/**
	 * No Sorts the type conversions.
	 */
	private void noSort() {
		this.list.setComparator(null);
		this.sort = null;
	}

	/**
	 * Sorts the type conversions by target type.
	 */
	private void sortByTargetColumn() {
		this.list.setComparator(new ViewerComparator() {
			@Override
			public int compare(Viewer viewer, Object e1, Object e2) {
				if (e1 != null  && e2 != null) {
					return e1.toString().compareToIgnoreCase(e2.toString());
				}
				return super.compare(viewer, e1, e2);
			}

			@Override
			public boolean isSorterProperty(Object element, String property) {
				return true;
			}
		});
		this.sort = Column.TARGET;
	}

	/** Replies the key to be used for saving the column widths into the dialog widths.
	 *
	 * @return the key.
	 */
	protected abstract String getColumnWidthDialogSettingsKey();

	/**
	 * Restores the column widths from dialog settings.
	 *
	 * @param settings the settings to read.
	 */
	private void restoreColumnWidths(IDialogSettings settings) {
		final int columnCount = this.table.getColumnCount();
		for (int i = 0; i < columnCount; i++) {
			int width = -1;
			try {
				width = settings.getInt(getPreferenceContainerID() + getColumnWidthDialogSettingsKey() + i);
			} catch (NumberFormatException exception) {
				//
			}

			if ((width <= 0) || (i == this.table.getColumnCount() - 1)) {
				this.table.getColumn(i).pack();
			} else {
				this.table.getColumn(i).setWidth(width);
			}
		}
	}

	/** Replies the key to be used for saving the column sort critera into the dialog widths.
	 *
	 * @return the key.
	 */
	protected abstract String getColumnSortCriteraDialogSettingsKey();

	/**
	 * Restore table settings from the given dialog store using the
	 * given key.
	 *
	 * @param settings dialog settings store
	 */
	private void restoreColumnSettings(IDialogSettings settings) {
		this.list.getTable().layout(true);
		restoreColumnWidths(settings);
		if (!this.isSortedElements) {
			this.sort = Column.SOURCE;
			try {
				final String columnName = settings.get(getPreferenceContainerID() + getColumnSortCriteraDialogSettingsKey());
				if (!Strings.isEmpty(columnName)) {
					this.sort = Column.valueOf(columnName);
					if (this.sort == null) {
						this.sort = Column.SOURCE;
					}
				}
			} catch (Throwable exception) {
				//
			}
			assert this.sort != null;
			switch (this.sort) {
			case SOURCE:
				sortBySourceColumn();
				break;
			case TARGET:
				sortByTargetColumn();
				break;
			default:
			}
		} else {
			noSort();
		}
	}

	/** Definition of the columns in the table of the type conversions.
	 *
	 * @author $Author: sgalland$
	 * @version $FullVersion$
	 * @mavengroupid $GroupId$
	 * @mavenartifactid $ArtifactId$
	 * @since 0.6
	 */
	private enum Column {

		/** The source column.
		 */
		SOURCE,

		/** The target column.
		 */
		TARGET;

	}

	/** Definition of the conversion mapping.
	 *
	 * @author $Author: sgalland$
	 * @version $FullVersion$
	 * @mavengroupid $GroupId$
	 * @mavenartifactid $ArtifactId$
	 * @since 0.6
	 */
	protected static class ConversionMapping implements Comparable<ConversionMapping> {

		private String from;

		private String to;

		/** Constructor.
		 *
		 * @param from the source of the mapping.
		 * @param to the target of the mapping.
		 */
		public ConversionMapping(String from, String to) {
			this.from = Strings.emptyIfNull(from);
			this.to = Strings.emptyIfNull(to);
		}

		/** Replies the source of the mapping.
		 *
		 * @return the source.
		 */
		public String getSource() {
			return this.from;
		}

		/** Replies the target of the mapping.
		 *
		 * @return the target.
		 */
		public String getTarget() {
			return this.to;
		}

		/** Change the source of the mapping.
		 *
		 * @param from the source.
		 */
		public void setSource(String from) {
			this.from = Strings.emptyIfNull(from);
		}

		/** Change the target of the mapping.
		 *
		 * @param to the target.
		 */
		public void setTarget(String to) {
			this.to = Strings.emptyIfNull(to);
		}

		@Override
		public String toString() {
			return getSource() + "->" + getTarget(); //$NON-NLS-1$
		}

		@Override
		public int compareTo(ConversionMapping mapping) {
			if (mapping == null) {
				return Integer.MAX_VALUE;
			}
			return getSource().compareTo(mapping.getSource());
		}

		@Override
		public boolean equals(Object obj) {
			if (obj == this) {
				return true;
			}
			if (obj instanceof ConversionMapping) {
				return ((ConversionMapping) obj).getSource().equals(getSource());
			}
			return false;
		}

		@Override
		public int hashCode() {
			return getSource().hashCode();
		}

	}

	/**
	 * Label provider for type conversion list.
	 *
	 * @author $Author: sgalland$
	 * @version $FullVersion$
	 * @mavengroupid $GroupId$
	 * @mavenartifactid $ArtifactId$
	 * @since 0.6
	 */
	protected static class LabelProvider extends BaseLabelProvider implements ITableLabelProvider {

		private final Image image;

		/** Construct the provider of labels.
		 *
		 * @param image the image of the target language.
		 */
		LabelProvider(Image image) {
			this.image = image;
		}

		@Override
		public Image getColumnImage(Object element, int columnIndex) {
			switch (columnIndex) {
			case 0:
				return JavaUI.getSharedImages().getImage(ISharedImages.IMG_OBJS_CLASS);
			case 1:
				return this.image;
			default:
			}
			return null;
		}

		@Override
		public String getColumnText(Object element, int columnIndex) {
			final ConversionMapping pair = (ConversionMapping) element;
			if (columnIndex == 1) {
				return pair.getTarget();
			}
			return pair.getSource();
		}

	}

	/**
	 * Content provider to show a list of type conversions.
	 *
	 * @author $Author: sgalland$
	 * @version $FullVersion$
	 * @mavengroupid $GroupId$
	 * @mavenartifactid $ArtifactId$
	 * @since 0.6
	 */
	protected class ContentProvider implements IStructuredContentProvider {

		/** Construct a provider of JREs' list.
		 */
		ContentProvider() {
			//
		}

		@SuppressWarnings("synthetic-access")
		@Override
		public Object[] getElements(Object input) {
			return AbstractConversionTable.this.conversions.toArray();
		}

		@Override
		public void inputChanged(Viewer viewer, Object oldInput, Object newInput) {
			//
		}

		@Override
		public void dispose() {
			//
		}

	}

	/**
	 * Cell modifier.
	 *
	 * @author $Author: sgalland$
	 * @version $FullVersion$
	 * @mavengroupid $GroupId$
	 * @mavenartifactid $ArtifactId$
	 * @since 0.6
	 */
	protected static class CellModifier implements ICellModifier {

		private final WeakReference<AbstractConversionTable> table;

		private final String sourceColumnPropertyName;

		private final String targetColumnPropertyName;

		/** Constructor.
		 *
		 * @param parent the parent of this modifier.
		 * @param sourceColumnPropertyName the property name of the source column.
		 * @param targetColumnPropertyName the property name of the target column.
		 */
		CellModifier(AbstractConversionTable parent, String sourceColumnPropertyName, String targetColumnPropertyName) {
			this.table = new WeakReference<>(parent);
			this.sourceColumnPropertyName = sourceColumnPropertyName;
			this.targetColumnPropertyName = targetColumnPropertyName;
		}

		@Override
		public boolean canModify(Object element, String property) {
			return true;
		}

		@Override
		public Object getValue(Object element, String property) {
			final ConversionMapping pair = (ConversionMapping) element;
			if (Strings.equal(this.targetColumnPropertyName, property)) {
				return pair.getTarget();
			}
			if (Strings.equal(this.sourceColumnPropertyName, property)) {
				return pair.getSource();
			}
			return createDefaultValue();
		}

		/** Create the default value that is used when the edited column is not supported.
		 *
		 * @return the default value.
		 */
		@SuppressWarnings("static-method")
		protected String createDefaultValue() {
			return new String();
		}

		@Override
		public void modify(Object element, String property, Object value) {
			final TableItem item = (TableItem) element;
			if (Strings.equal(this.targetColumnPropertyName, property)) {
				((ConversionMapping) item.getData()).setTarget(Objects.toString(value));
				this.table.get().refreshListUI();
			} else if (Strings.equal(this.sourceColumnPropertyName, property)) {
				((ConversionMapping) item.getData()).setSource(Objects.toString(value));
				this.table.get().refreshListUI();
			}
		}

	}

	/** Iterator on type conversions.
	 *
	 * @author $Author: sgalland$
	 * @version $FullVersion$
	 * @mavengroupid $GroupId$
	 * @mavenartifactid $ArtifactId$
	 * @since 0.6
	 */
	private class TypeConversionIterator implements Iterator<String> {

		private final Iterator<ConversionMapping> iterator;

		private String value;

		@SuppressWarnings("synthetic-access")
		TypeConversionIterator() {
			this.iterator = AbstractConversionTable.this.conversions.iterator();
		}

		@Override
		public boolean hasNext() {
			return this.value != null || this.iterator.hasNext();
		}

		@Override
		public String next() {
			if (this.value != null) {
				final String next = this.value;
				this.value = null;
				return next;
			}
			final ConversionMapping mapping = this.iterator.next();
			this.value = mapping.getTarget();
			return mapping.getSource();
		}

	}

}
