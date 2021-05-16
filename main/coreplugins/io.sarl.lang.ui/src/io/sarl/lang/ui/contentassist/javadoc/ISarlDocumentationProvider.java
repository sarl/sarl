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

package io.sarl.lang.ui.contentassist.javadoc;

import com.google.inject.ImplementedBy;
import org.eclipse.emf.ecore.EObject;
import org.eclipse.jface.text.ITextSelection;
import org.eclipse.xtext.ui.editor.model.IXtextDocument;

/** Provides SARL documentation for JVM elements.
 *
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 * @since 0.12
 */
@ImplementedBy(SarlDocumentationProvider.class)
public interface ISarlDocumentationProvider {

	/** Generate the documentation if possible.
	 *
	 * @param document the document to change.
	 * @param selection the selected area.
	 * @throws Exception if something wrong append during the change application.
	 */
	void generateDocumentationIfPossible(IXtextDocument document, ITextSelection selection) throws Exception;

	/** Replies if the given element is supported by this documentation generator.
	 *
	 * @param element the element to test.
	 * @return {@code true} if the element is supported.
	 */
	boolean isValidElement(EObject element);

}
