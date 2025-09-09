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

package io.sarl.docs.doclet2.framework;

import java.io.PrintWriter;
import java.io.StringWriter;
import java.lang.ref.WeakReference;
import java.util.Collections;
import java.util.Locale;
import java.util.Set;

import javax.lang.model.SourceVersion;
import javax.lang.model.element.Element;
import javax.tools.Diagnostic.Kind;

import org.eclipse.xtext.util.JavaVersion;

import com.google.common.base.Strings;
import com.google.inject.Inject;
import com.sun.source.util.DocTreePath;

import io.sarl.lang.core.SARLVersion;
import jdk.javadoc.doclet.Doclet;
import jdk.javadoc.doclet.DocletEnvironment;
import jdk.javadoc.doclet.Reporter;

/** An abstract implementation of a Doclet.
 *
 * @author <a href="http://www.ciad-lab.fr/stephane_galland">St&eacute;phane Galland</a>
 * @version docs.doclet 0.15.0 20250909-115750
 * @mavengroupid io.sarl.docs
 * @mavenartifactid docs.doclet
 * @since 0.13
 */
public abstract class AbstractDoclet implements Doclet {

	private final WeakReference<Doclet> parent;
	
	private Reporter reporter = new IddleReporter();

	private SourceVersion sourceVersion;

	private ElementUtils elementUtils;

	private TagletManager tagletManager;

	private CustomTagParser customTagParser;

	private ExternalLinkManager externalLinkManager;

	/** Constructor.
	 *
	 * @param parent the parent doclet.
	 */
	public AbstractDoclet(Doclet parent) {
		if (parent == null) {
			this.parent = null;
		} else {
			this.parent = new WeakReference<>(parent);
		}
	}

	/** Change the taglet manager.
	 *
	 * @param manager the manager.
	 */
	@Inject
	public void setTagletManager(TagletManager manager) {
		this.tagletManager = manager;
	}

	/** Replies the taglet manager.
	 *
	 * @return the manager.
	 */
	public TagletManager getTagletManager() {
		return this.tagletManager;
	}

	/** Change the parser of custom tag descriptions.
	 *
	 * @param parser the parser.
	 */
	@Inject
	public void setCustomTagParser(CustomTagParser parser) {
		this.customTagParser = parser;
	}

	/** Replies the parser of custom tag descriptions.
	 *
	 * @return the parser.
	 */
	public CustomTagParser getCustomTagParser() {
		return this.customTagParser;
	}

	/** Change the external link manager.
	 *
	 * @param manager the manager.
	 */
	@Inject
	public void setExternalLinkManager(ExternalLinkManager manager) {
		this.externalLinkManager = manager;
	}

	/** Replies the external link manager.
	 *
	 * @return the manager.
	 */
	public ExternalLinkManager getExternalLinkManager() {
		return this.externalLinkManager;
	}

	/** Change the element utilities.
	 *
	 * @param utils the element utilities.
	 */
	@Inject
	public void setElementUtils(ElementUtils utils) {
		this.elementUtils = utils;
	}

	/** Replies the element utilities.
	 *
	 * @return the utilities.
	 */
	public ElementUtils getElementUtils() {
		return this.elementUtils;
	}

	/** Replies the parent doclet.
	 *
	 * @return the parent.
	 */
	public Doclet getParent() {
		return this.parent == null ? null : this.parent.get();
	}

	/** Replies the message reporter to be used by this doclet.
	 *
	 * @return the reporter.
	 */
	public Reporter getReporter() {
		return this.reporter;
	}

	@Override
	public String getName() {
		return "SARL-HTML"; //$NON-NLS-1$
	}

	@Override
	public void init(Locale locale, Reporter reporter) {
		this.reporter = reporter;
	}

	@Override
	public Set<? extends Option> getSupportedOptions() {
		return Collections.emptySet();
	}

	/** Compute the source version.
	 *
	 * <p>This function is invoked by {@link #getSupportedSourceVersion()}.
	 *
	 * @return the source version.
	 */
	@SuppressWarnings("static-method")
	protected SourceVersion determineSourceVersion() {
		final var version = JavaVersion.fromQualifier(SARLVersion.MINIMAL_JDK_VERSION_IN_SARL_PROJECT_CLASSPATH);
		if (version != null) {
			switch (version) {
			case JAVA21:
				return SourceVersion.RELEASE_21;
			case JAVA17:
				return SourceVersion.RELEASE_17;
			case JAVA11:
				return SourceVersion.RELEASE_11;
			case JAVA10:
				return SourceVersion.RELEASE_10;
			case JAVA9:
				return SourceVersion.RELEASE_9;
			case JAVA8:
				return SourceVersion.RELEASE_8;
			default:
				throw new UnsupportedClassVersionError();
			}
		}
		return SourceVersion.latestSupported();
	}
	
	@Override
	public SourceVersion getSupportedSourceVersion() {
		if (this.sourceVersion == null) {
			this.sourceVersion = determineSourceVersion();
		}
		return this.sourceVersion;
	}

	@Override
	public final boolean run(DocletEnvironment environment) {
		try {
			if (environment instanceof SarlDocletEnvironment sarlEnvironment) {
				getElementUtils().setElements(sarlEnvironment.getElementUtils());
				getElementUtils().setTypes(sarlEnvironment.getTypeUtils());
				getTagletManager().init(sarlEnvironment, this);
				return generate(sarlEnvironment);
			}
		} catch (Exception ex) {
			final var rep = getReporter();
			if (rep == null) {
				throw new RuntimeException(ex);
			}
			final var writer = new StringWriter();
			try (final var printWriter = new PrintWriter(writer)) {
				var msg = ex.getLocalizedMessage();
				if (Strings.isNullOrEmpty(msg)) {
					msg = ex.getMessage();
				}
				if (Strings.isNullOrEmpty(msg)) {
					msg = ex.getClass().getName();
				}
				printWriter.println(msg);
				ex.printStackTrace(printWriter);
			}
			rep.print(Kind.ERROR, writer.toString());
		}
		return false;
	}

	/** Generate the HTML documentation.
	 * 
	 * @param environment the generation environment.
	 * @return {@code true} on success.
	 * @throws Exception if some error occured during the generation.
	 */
	protected abstract boolean generate(SarlDocletEnvironment environment) throws Exception;

	/** Reporter that does nothing.
	 *
	 * @author <a href="http://www.ciad-lab.fr/stephane_galland">St&eacute;phane Galland</a>
	 * @version docs.doclet 0.15.0 20250909-115750
	 * @mavengroupid io.sarl.docs
	 * @mavenartifactid docs.doclet
	 * @since 0.13
	 */
	private static class IddleReporter implements Reporter {

		@Override
		public void print(Kind kind, String msg) {
			//
		}

		@Override
		public void print(Kind kind, DocTreePath path, String msg) {
			//
		}

		@Override
		public void print(Kind kind, Element e, String msg) {
			//
		}
		
	}

}
