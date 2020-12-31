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

package io.sarl.lang.ui.contentassist.templates;

import org.eclipse.xtext.ui.editor.templates.CrossReferenceTemplateVariableResolver;
import org.eclipse.xtext.xbase.ui.templates.XbaseTemplateContextType;

/** SARL context in which templates are resolved.
 *
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 */
public class SARLTemplateContextType extends XbaseTemplateContextType {

	@Override
	public void setCrossReferenceResolver(CrossReferenceTemplateVariableResolver resolver) {
		// don't register the CrossReferenceTemplateVariableResolver for Xtend, then for SARL
		// see https://bugs.eclipse.org/bugs/show_bug.cgi?id=462917
	}

}
