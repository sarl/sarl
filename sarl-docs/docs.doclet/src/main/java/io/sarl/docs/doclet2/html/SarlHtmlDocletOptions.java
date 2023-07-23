/*
 * $Id$
 *
 * SARL is an general-purpose agent programming language.
 * More details on http://www.sarl.io
 *
 * Copyright (C) 2014-2023 SARL.io, the Original Authors and Main Authors
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

package io.sarl.docs.doclet2.html;

/** Options for the SARL Doclet.
 *
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 * @since 0.13
 */
public final class SarlHtmlDocletOptions {

	private SarlHtmlDocletOptions() {
		//
	}

	/** Option {@code -copyright}.
	 */
	public static final String COPYRIGHT_OPTION = "-copyright"; //$NON-NLS-1$

	/** Option {@code -directory}.
	 */
	public static final String LONG_DIRECTORY_OPTION = "-directory"; //$NON-NLS-1$

	/** Option {@code -d}.
	 */
	public static final String SHORT_DIRECTORY_OPTION = "-d"; //$NON-NLS-1$

	/** Option {@code -directory} or {@code -d}.
	 */
	public static final String DIRECTORY_OPTION = LONG_DIRECTORY_OPTION + " " + SHORT_DIRECTORY_OPTION; //$NON-NLS-1$

	/** Option {@code -fake}.
	 */
	public static final String FAKE_OPTION = "-fake"; //$NON-NLS-1$

	/** Option {@code -group}.
	 */
	public static final String GROUP_OPTION = "-group"; //$NON-NLS-1$

	/** Option {@code -htmlcomments}.
	 */
	public static final String HTMLCOMMENTS_OPTION = "-htmlcomments"; //$NON-NLS-1$

	/** Option {@code -link}.
	 */
	public static final String LINK_OPTION = "-link"; //$NON-NLS-1$

	/** Option {@code -tag}.
	 */
	public static final String TAG_OPTION = "-tag"; //$NON-NLS-1$

	/** Option {@code -taglet}.
	 */
	public static final String TAGLET_OPTION = "-taglet"; //$NON-NLS-1$

	/** Option {@code -title}.
	 */
	public static final String TITLE_OPTION = "-title"; //$NON-NLS-1$

	/** Option {@code -offline}.
	 */
	public static final String OFFLINE_OPTION = "-offline"; //$NON-NLS-1$

	/** Option {@code -nodeprecated}.
	 */
	public static final String NODEPRECATED_OPTION = "-nodeprecated"; //$NON-NLS-1$

	/** Option {@code -nosince}.
	 */
	public static final String NOSINCE_OPTION = "-nosince"; //$NON-NLS-1$

	/** Option {@code -version}.
	 */
	public static final String VERSION_OPTION = "-version"; //$NON-NLS-1$

	/** Option {@code -author}.
	 */
	public static final String AUTHOR_OPTION = "-author"; //$NON-NLS-1$

}
