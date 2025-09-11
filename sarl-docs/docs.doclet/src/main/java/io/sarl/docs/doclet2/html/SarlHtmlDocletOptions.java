/*
 * $Id$
 *
 * SARL is an general-purpose agent programming language.
 * More details on http://www.sarl.io
 *
 * Copyright (C) 2014-2025 SARL.io, the Original Authors and Main Authors.
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
 *
 *------- FORKED SOURCE CODE:
 *
 * THIS CODE IS FORKED FROM JDK.JAVADOC INTERNAL PACKAGE AND ADAPTED TO THE SARL PURPOSE.
 * THE FORK WAS NECESSARY BECAUSE IT IS IMPOSSIBLE TO SUBCLASS THE TYPES FOR THE.
 * STANDARD HTML DOCLET THAT IS PROVIDED BY JDK.JAVADOC MODULE.
 *
 * Copyright (c) 2003, 2021, Oracle and/or its affiliates. All rights reserved.
 * DO NOT ALTER OR REMOVE COPYRIGHT NOTICES OR THIS FILE HEADER.
 *
 * This code is free software; you can redistribute it and/or modify it
 * under the terms of the GNU General Public License version 2 only, as
 * published by the Free Software Foundation.  Oracle designates this
 * particular file as subject to the "Classpath" exception as provided
 * by Oracle in the LICENSE file that accompanied this code.
 *
 * This code is distributed in the hope that it will be useful, but WITHOUT
 * ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
 * FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
 * version 2 for more details (a copy is included in the LICENSE file that
 * accompanied this code).
 *
 * You should have received a copy of the GNU General Public License version
 * 2 along with this work; if not, write to the Free Software Foundation,
 * Inc., 51 Franklin St, Fifth Floor, Boston, MA 02110-1301 USA.
 *
 * Please contact Oracle, 500 Oracle Parkway, Redwood Shores, CA 94065 USA
 * or visit www.oracle.com if you need additional information or have any
 * questions.
 */

package io.sarl.docs.doclet2.html;

/** Options for the SARL Doclet.
 *
 * @author <a href="http://www.ciad-lab.fr/stephane_galland">St&eacute;phane Galland</a>
 * @version docs.doclet 0.15.1 20250911-224827
 * @mavengroupid io.sarl.docs
 * @mavenartifactid docs.doclet
 * @since 0.13
 */
public final class SarlHtmlDocletOptions {

	private SarlHtmlDocletOptions() {
		//
	}

	/** Option {@code -copyright}.
	 */
	public static final String COPYRIGHT_OPTION = "-copyright"; //$NON-NLS-1$

	/** Option {@code -bottom}. Synonym of {@code -copyright}.
	 */
	public static final String BOTTOM_OPTION = "-bottom"; //$NON-NLS-1$

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

	/** Option {@code -doctitle}.
	 */
	public static final String DOCTITLE_OPTION = "-doctitle"; //$NON-NLS-1$

	/** Option {@code -windowtitle}.
	 */
	public static final String WINDOWTITLE_OPTION = "-windowtitle"; //$NON-NLS-1$

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

	/** Option {@code -charset}.
	 */
	public static final String CHARSET_OPTION = "-charset"; //$NON-NLS-1$


	/** Option {@code -docencoding}.
	 */
	public static final String DOCENCODING_OPTION = "-docencoding"; //$NON-NLS-1$

}
