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

package io.sarl.lang.extralanguage.compiler;

import java.util.List;

import org.eclipse.xtext.common.types.JvmType;
import org.eclipse.xtext.xbase.compiler.AbstractStringBuilderBasedAppendable;
import org.eclipse.xtext.xbase.compiler.ImportManager;

/** Appendable for extra languages.
 *
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 * @since 0.6
 */
public class ExtraLanguageAppendable extends AbstractStringBuilderBasedAppendable {

	private final ImportManager importManager;

	/** Constructor.
	 */
	public ExtraLanguageAppendable() {
		this(null);
	}

	/** Constructor.
	 *
	 * @param indentation the indentation string.
	 * @param lineSeparator the line separator string.
	 */
	public ExtraLanguageAppendable(String indentation, String lineSeparator) {
		this(indentation, lineSeparator, null);
	}

	/** Constructor.
	 *
	 * @param importManager the import manager.
	 */
	public ExtraLanguageAppendable(ImportManager importManager) {
		super(false);
		ImportManager im = importManager;
		if (im == null) {
			im = new ImportManager(true);
		}
		this.importManager = im;
	}

	/** Constructor.
	 *
	 * @param indentation the indentation string.
	 * @param lineSeparator the line separator string.
	 * @param importManager the import manager.
	 */
	public ExtraLanguageAppendable(String indentation, String lineSeparator, ImportManager importManager) {
		super(indentation, lineSeparator, false);
		ImportManager im = importManager;
		if (im == null) {
			im = new ImportManager(true);
		}
		this.importManager = im;
	}

	/** Replies the line separator.
	 *
	 * @return the line separator.
	 */
	@Override
	public String getLineSeparator() {
		// Change the visibility of the method.
		return super.getLineSeparator();
	}

	@Override
	protected void appendType(final JvmType type, StringBuilder builder) {
		getImportManager().appendType(type, builder);
	}

	@Override
	protected void appendType(final Class<?> type, StringBuilder builder) {
		getImportManager().appendType(type, builder);
	}

	/** {@inheritDoc}
	 * @deprecated {@link #getImportManager()}.{@link ImportManager#getImports()}.
	 */
	@Deprecated
	@Override
	public List<String> getImports() {
		return getImportManager().getImports();
	}

	/** Replies the import manager.
	 *
	 * @return the import manager.
	 */
	public ImportManager getImportManager() {
		return this.importManager;
	}

	@Override
	public String toString() {
		return super.toString().trim();
	}

}
