/*
 * $Id$
 *
 * SARL is an general-purpose agent programming language.
 * More details on http://www.sarl.io
 *
 * Copyright (C) 2014-2024 SARL.io, the Original Authors and Main Authors
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

package io.sarl.docs.doclet2;

import java.util.Arrays;
import java.util.Locale;
import java.util.Set;

import javax.lang.model.SourceVersion;

import com.google.inject.Injector;
import jdk.javadoc.doclet.DocletEnvironment;
import jdk.javadoc.doclet.Reporter;

import io.sarl.docs.doclet2.framework.DelegateErrorReporter;
import io.sarl.docs.doclet2.framework.DocletEnvironmentFactory;
import io.sarl.docs.doclet2.guice.SarlJavadocModule;
import io.sarl.lang.SARLStandaloneSetup;

/** SARL Doclet that is generated the API documentation.
 *
 * <p>This version of the SARL doc let is an adaptation of the
 * previous SARL doclet (for Java 8) to Java 11 and higher API.
 *
 * <p>This version of the Doclet uses Inversion of Control (injector).
 *
 * <p>This doclet also ignore the hidden features according to the SARL specification.
 *
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 * @since 0.13
 */
public class Doclet implements jdk.javadoc.doclet.Doclet {

	private Injector mainInjector;

	private Injector injector;
	
	private jdk.javadoc.doclet.Doclet doclet;

	/** Replies the injector.
	 *
	 * @return the injector.
	 */
	protected synchronized Injector getInjector() {
		if (this.injector == null) {
			this.mainInjector = SARLStandaloneSetup.doSetup();
			this.injector = this.mainInjector.createChildInjector(Arrays.asList(new SarlJavadocModule(this)));
		}
		return this.injector;
	}
	
	/** Replies the injected doclet.
	 *
	 * @return the injected doclet.
	 */
	protected synchronized jdk.javadoc.doclet.Doclet getInjectedDoclet() {
		if (this.doclet == null) {
			this.doclet = getInjector().getInstance(jdk.javadoc.doclet.Doclet.class);
		}
		return this.doclet;
	}

	@Override
	public void init(Locale locale, Reporter reporter) {
		getInjectedDoclet().init(locale, new DelegateErrorReporter(reporter));
	}

	@Override
	public String getName() {
		return getInjectedDoclet().getName();
	}

	@Override
	public Set<? extends Option> getSupportedOptions() {
		return getInjectedDoclet().getSupportedOptions();
	}

	@Override
	public SourceVersion getSupportedSourceVersion() {
		return getInjectedDoclet().getSupportedSourceVersion();
	}

	@Override
	public boolean run(DocletEnvironment environment) {
		final DocletEnvironmentFactory factory = getInjector().getInstance(DocletEnvironmentFactory.class);
		return getInjectedDoclet().run(factory.newDocletEnvironment(environment));
	}

}
