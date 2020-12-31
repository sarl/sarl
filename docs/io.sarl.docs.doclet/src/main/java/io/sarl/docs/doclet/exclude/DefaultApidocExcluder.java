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

package io.sarl.docs.doclet.exclude;

import com.sun.javadoc.Doc;
import com.sun.javadoc.ProgramElementDoc;

import io.sarl.docs.doclet.utils.Utils;

/** Check if an element should be ignored into the API doc.
 *
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 * @since 0.7
 */
public class DefaultApidocExcluder implements ApidocExcluder {

	/** Tag that could be used for excluding an element from the API documentation.
	 */
	public static final String EXCLUDE_FROM_JAVADOC_TAG = "@ExcludeFromApidoc"; //$NON-NLS-1$

	@Override
	public boolean isExcluded(Doc doc) {
		if (Utils.isHiddenMember(doc.name())) {
			return true;
		}
		if (doc.tags(EXCLUDE_FROM_JAVADOC_TAG).length > 0) {
			return true;
		}
		if (doc instanceof ProgramElementDoc) {
			final ProgramElementDoc element = (ProgramElementDoc) doc;
			if (element.containingPackage().tags(EXCLUDE_FROM_JAVADOC_TAG).length > 0) {
				return true;
			}
			if (Utils.findFirst(element.annotations(), it ->
				Utils.qualifiedNameEquals(
						Utils.getKeywords().getSyntheticMemberAnnotationName(),
						it.annotationType().qualifiedName())) != null) {
				return true;
			}
		}
		// nothing above found a reason to exclude
		return false;
	}

	@Override
	public boolean isTranslatableToTag(Doc doc) {
		if (doc instanceof ProgramElementDoc) {
			final ProgramElementDoc element = (ProgramElementDoc) doc;
			if (Utils.findFirst(element.annotations(), it ->
				Utils.qualifiedNameEquals(
						Utils.getKeywords().getSyntheticMemberAnnotationName(),
						it.annotationType().qualifiedName())) != null) {
				return true;
			}
		}
		return false;
	}

}
