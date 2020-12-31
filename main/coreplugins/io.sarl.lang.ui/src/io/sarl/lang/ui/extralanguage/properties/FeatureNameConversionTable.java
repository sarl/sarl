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

import org.eclipse.jface.preference.IPreferenceStore;
import org.eclipse.jface.viewers.CellEditor;
import org.eclipse.swt.graphics.Image;

import io.sarl.lang.ui.extralanguage.preferences.ExtraLanguagePreferenceAccess;

/** Table for feature name conversion definition.
 *
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 * @since 0.6
 */
@SuppressWarnings("checkstyle:classdataabstractioncoupling")
public class FeatureNameConversionTable extends AbstractConversionTable {

	private static final String FEATURE_NAME_CONVERTER_COLUMN_WIDTH_ID = ".featureNameConverterSection.columnWidth"; //$NON-NLS-1$

	private static final String FEATURE_NAME_CONVERTER_SORT_COLUMN = ".featureNameConverterSection.sortColumn"; //$NON-NLS-1$

	/** Constructor.
	 *
	 * @param controller the controller.
	 * @param languageImage for the target language (16x16).
	 * @param preferenceStore the preference store to be used.
	 * @param preferenceContainerID the identifier of the generator's preference container.
	 */
	FeatureNameConversionTable(IExtraControlController controller, Image languageImage,
			IPreferenceStore preferenceStore, String preferenceContainerID) {
		super(controller, languageImage, preferenceStore, preferenceContainerID, true);
	}

	@Override
	protected String getSourceColumnLabel() {
		return Messages.FeatureNameConversionTable_1;
	}

	@Override
	protected String getTargetColumnLabel() {
		return Messages.FeatureNameConversionTable_2;
	}

	@Override
	protected CellEditor createSourceColumnEditor() {
		return createTextCellEditor();
	}

	@Override
	protected CellEditor createTargetColumnEditor() {
		return createTextCellEditor();
	}

	@Override
	protected String getPreferenceKey() {
		return ExtraLanguagePreferenceAccess.getPrefixedKey(getPreferenceContainerID(),
				ExtraLanguagePreferenceAccess.FEATURE_NAME_CONVERSION_PROPERTY);
	}

	@Override
	protected String getColumnWidthDialogSettingsKey() {
		return FEATURE_NAME_CONVERTER_COLUMN_WIDTH_ID;
	}

	@Override
	protected String getColumnSortCriteraDialogSettingsKey() {
		return FEATURE_NAME_CONVERTER_SORT_COLUMN;
	}

	@Override
	protected String getIntroductionLabel() {
		return Messages.FeatureNameConversionTable_0;
	}

}
