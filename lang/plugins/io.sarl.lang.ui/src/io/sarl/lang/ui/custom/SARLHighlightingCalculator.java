/*
 * Copyright 2014 Sebastian RODRIGUEZ, Nicolas GAUD, Stéphane GALLAND
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */
package io.sarl.lang.ui.custom;

import io.sarl.lang.SARLKeywords;

import java.util.Map;

import org.eclipse.xtext.ui.editor.syntaxcoloring.DefaultHighlightingConfiguration;
import org.eclipse.xtext.xbase.ui.highlighting.XbaseHighlightingCalculator;

/**
 * *
 * <p>
 * A base implementation of the semantic highlighting calculation.
 * </p>
 * 
 * <p>
 * Uses syntax highlighting from {@link XbaseHighlightingCalculator} and
 * adds SARL specific keywords, e.g. <code>occurrence</code> 
 * </p>
 * 
 * @author $Author: srodriguez$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 */
public class SARLHighlightingCalculator extends XbaseHighlightingCalculator {
	/**
	 * {@inheritDoc}
	 */
	@Override
	protected Map<String, String> initializeHighlightedIdentifiers() {
		Map<String, String> result = super.initializeHighlightedIdentifiers();
		result.put(SARLKeywords.KEYWORD_OCCURRENCE, DefaultHighlightingConfiguration.KEYWORD_ID);
		return result;
	}
}
