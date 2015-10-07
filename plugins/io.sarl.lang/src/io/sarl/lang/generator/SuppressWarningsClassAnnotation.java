/*
 * $Id$
 *
 * SARL is an general-purpose agent programming language.
 * More details on http://www.sarl.io
 *
 * Copyright (C) 2014-2015 the original authors or authors.
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

package io.sarl.lang.generator;

import java.util.ArrayList;
import java.util.List;

import org.eclipse.xtext.generator.IClassAnnotation;
import org.eclipse.xtext.util.Strings;

/**
 * A class annotation configuration for the <code>@SuppressWarnings</code> annotation.
 *
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 */
public class SuppressWarningsClassAnnotation implements IClassAnnotation {

	private final List<String> warnings = new ArrayList<>();

	@Override
	public String toString() {
		StringBuilder stringBuilder = new StringBuilder("@SuppressWarnings("); //$NON-NLS-1$
		if (!this.warnings.isEmpty()) {
			for (int i = 0; i < this.warnings.size(); ++i) {
				if (i > 0) {
					stringBuilder.append(", "); //$NON-NLS-1$
				}
				stringBuilder.append("\""); //$NON-NLS-1$
				stringBuilder.append(Strings.convertToJavaString(this.warnings.get(i)));
				stringBuilder.append("\""); //$NON-NLS-1$
			}
		} else {
			stringBuilder.append("\"all\""); //$NON-NLS-1$
		}
		return stringBuilder.append(')').toString();
	}

	@Override
	public String getAnnotationImport() {
		return "java.lang.SuppressWarnings"; //$NON-NLS-1$
	}

	/**
	 * Add a warning to the annotation.
	 *
	 * @param warning - the warning to ignore.
	 */
	public void addWarning(String warning) {
		if (!Strings.isEmpty(warning)) {
			this.warnings.add(warning);
		}
	}

}
