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

package io.sarl.docs.doclet2.html.framework;

import java.nio.file.Path;
import java.util.List;
import java.util.Set;
import java.util.regex.Pattern;

import org.eclipse.xtext.xbase.lib.Pair;

/** Options provided on the CLI to the doclet.
 *
 * @author $Author: sgalland$
 * @version docs.doclet 0.15.0 20250909-115750
 * @mavengroupid io.sarl.docs
 * @mavenartifactid docs.doclet
 * @since 0.13
 */
public interface DocletOptions {

	/** User tag that is used to ignore elements.
	 */
	public static final String EXCLUDEFROMAPIDOC_TAG = "excludefromapidoc"; //$NON-NLS-1$
	
	/** User tag that is used to specify the Maven group id.
	 */
	public static final String MAVENGROUPID_TAG = "mavengroupid"; //$NON-NLS-1$

	/** User tag that is used to specify the Maven artifact id.
	 */
	public static final String MAVENARTIFACTID_TAG = "mavenartifactid"; //$NON-NLS-1$

	/** User tag that is used to specify a private API element.
	 */
	public static final String PRIVATEAPI_TAG = "privateapi"; //$NON-NLS-1$

	/** Replies the output directory.
	 *
	 * @return the output directory.
	 */
	Path getOutputDirectory();
	
	/** Change the output directory.
	 *
	 * @param output the output directory.
	 */
	void setOutputDirectory(Path output);

	/** Replies if outputs are fake.
	 *
	 * @return {@code true} if outputs are fake.
	 */
	boolean isFakeOutput();
	
	/** Set if the outputs are fake.
	 *
	 * @param fake the fake flag.
	 */
	void setFakeOutput(boolean fake);

	/** Add a user tag from the CLI configuration. The user tags will be recognized by the doclet and no error will be generated when
	 * they are used.
	 *
	 * <p>The location must be a sequence of characters in: <ul>
	 * <li>{@code a} for everywhere</li>
	 * <li>{@code o} for only in the overview page</li>
	 * <li>{@code p} for on package documentation</li>
	 * <li>{@code t} for class or interface documentation</li>
	 * <li>{@code c} for constructors</li>
	 * <li>{@code m} for methods</li>
	 * <li>{@code f} for fields</li>
	 * <li>{@code X} for disabling the tag</li>
	 * </ul>
	 *
	 * @param tagDescription the description of the tag in format {@code name:locations:header}.
	 */
	void addUserTag(String tagDescription);

	/** Replies the user tags from the CLI configuration. The user tags will be recognized by the doclet and no error will be generated when
	 * they are used.
	 *
	 * <p>The location must be a sequence of characters in: <ul>
	 * <li>{@code a} for everywhere</li>
	 * <li>{@code o} for only in the overview page</li>
	 * <li>{@code p} for on package documentation</li>
	 * <li>{@code t} for class or interface documentation</li>
	 * <li>{@code c} for constructors</li>
	 * <li>{@code m} for methods</li>
	 * <li>{@code f} for fields</li>
	 * <li>{@code X} for disabling the tag</li>
	 * </ul>
	 *
	 * @return the descriptions of the tags in format {@code name:locations:header}.
	 */
	Set<String> getUserTags();

	/** Change the offline status of the doclet.
	 *
	 * @param offline is {@code true} to force the doclet to be offline.
	 */
	void setOffline(boolean offline);

	/** Replies if the doclet is offline or not.
	 *
	 * @return {@code true} to force the doclet to be offline.
	 */
	boolean isOffline();

	/** Change the flag that indicates if the {@code @comment} tags are translated to HTML comments or to HTML blocks that are hidden.
	 *
	 * @param enable is {@code true} to convert {@code @comment} to HTML comment.
	 */
	void setHtmlCommentsEnabled(boolean enable);

	/** Replies if the {@code @comment} tags are translated to HTML comments or to HTML blocks that are hidden.
	 *
	 * @return {@code true} to convert {@code @comment} to HTML comment.
	 */
	boolean isHtmlCommentsEnabled();

	/** Change the flag that indicates if the deprecated features are translated to HTML elements.
	 *
	 * @param enable is {@code true} to convert deprecated to HTML elements.
	 */
	void setDeprecatedFeaturesEnabled(boolean enable);

	/** Replies if the deprecated features are translated to HTML elements.
	 *
	 * @return {@code true} to convert deprecated to HTML elements.
	 */
	boolean isDeprecatedFeaturesEnabled();

	/** Change the flag that indicates if the {@code @since} tags are translated to HTML elements.
	 *
	 * @param enable is {@code true} to convert {@code @since} tags to HTML elements.
	 */
	void setSinceTagsEnabled(boolean enable);

	/** Replies if the {@code @since} tags are translated to HTML elements.
	 *
	 * @return {@code true} to convert {@code @since} tags to HTML elements.
	 */
	boolean isSinceTagsEnabled();

	/** Change the flag that indicates if the {@code @version} tags are translated to HTML elements.
	 *
	 * @param enable is {@code true} to convert {@code @version} tags to HTML elements.
	 */
	void setVersionTagsEnabled(boolean enable);

	/** Replies if the {@code @version} tags are translated to HTML elements.
	 *
	 * @return {@code true} to convert {@code @version} tags to HTML elements.
	 */
	boolean isVersionTagsEnabled();

	/** Change the flag that indicates if the {@code @author} tags are translated to HTML elements.
	 *
	 * @param enable is {@code true} to convert {@code @author} tags to HTML elements.
	 */
	void setAuthorTagsEnabled(boolean enable);

	/** Replies if the {@code @author} tags are translated to HTML elements.
	 *
	 * @return {@code true} to convert {@code @author} tags to HTML elements.
	 */
	boolean isAuthorTagsEnabled();

	/** Change the copyright text.
	 *
	 * @param text the text.
	 */
	void setCopyrightText(String text);

	/** Replies the copyright text.
	 *
	 * @return the text.
	 */
	String getCopyrightText();

	/** Change the title.
	 *
	 * @param text the text.
	 */
	void setTitle(String text);

	/** Replies the title.
	 *
	 * @return the text.
	 */
	String getTitle();

	/** Add a group of package that is used on the overview page.
	 *
	 * @param heading the title of the group.
	 * @param groupPatterns the package patterns.
	 */
	void addGroup(String heading, String... groupPatterns);

	/** Replies all the groups of packages.
	 *
	 * @return the groups of packages.
	 */
	List<Pair<String, Pattern>> getGroups();

	/** Change the charset of the generated documentation.
	 *
	 * @param name the name of the charset.
	 */
	void setCharset(String name);

	/** Replies the charset of the generated documentation.
	 *
	 * @return the charset.
	 */
	String getCharset();

}
