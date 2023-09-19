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

import java.lang.reflect.Method;

/** Tester for the SARL Doclet.
 *
 * <p>This utility class launch the Javadoc tool with the SARL doclet. Paths are hard coded.
 *
 * @author <a href="http://www.ciad-lab.fr/stephane_galland">St&eacute;phane Galland</a>
 * @version docs.doclet 0.13.0 20230919-093059
 * @mavengroupid io.sarl.docs
 * @mavenartifactid docs.doclet
 * @since 0.13
 */
public final class DocletTester {

	private DocletTester() {
		//
	}

	/** Run the Javadoc generator.
	 *
	 * @param args arguments.
	 * @throws RuntimeException a runtime exception.
	 */
	public static void main(String[] args) {
		try {
			//System.setProperty("http.proxyHost", "proxy.utbm.fr"); //$NON-NLS-1$ //$NON-NLS-2$
			//System.setProperty("http.proxyPort", "3128"); //$NON-NLS-1$ //$NON-NLS-2$
			final String[] params = new String[] {
				"-private", //$NON-NLS-1$
				"-source", "11", //$NON-NLS-1$ //$NON-NLS-2$
				"-doclet", Doclet.class.getName(), //$NON-NLS-1$
				"-sourcepath", "/home/sgalland/git/sarl.dsl/main/coreplugins/io.sarl.lang.core/src", //$NON-NLS-1$ //$NON-NLS-2$
				"-sourcepath", "/home/sgalland/git/sarl.dsl/main/coreplugins/io.sarl.lang/src", //$NON-NLS-1$ //$NON-NLS-2$
				//"-sourcepath", "/home/sgalland/git/sarl.dsl/main/apiplugins/io.sarl.api.bootiquebase/src/main/generated-sources/sarl", //$NON-NLS-1$ //$NON-NLS-2$
				"-sourcepath", "/home/sgalland/git/sarl.dsl/main/apiplugins/io.sarl.api.naming/src/main/generated-sources/sarl", //$NON-NLS-1$ //$NON-NLS-2$
				"-subpackages", ":io", //$NON-NLS-1$ //$NON-NLS-2$
				"-link", "https://docs.oracle.com/en/java/javase/11/docs/api/", //$NON-NLS-1$ //$NON-NLS-2$
				"-d", "/home/sgalland/tmp/gen-site", //$NON-NLS-1$ //$NON-NLS-2$
				//"-fake",
				//"-offline",
				"-copyright", "ABC", //$NON-NLS-1$ //$NON-NLS-2$
				"-group", "Standard Development Kit and Other Utilities", "io.sarl.core*:io.sarl.util*:io.sarl.api*:io.sarl.maven.bootiqueapp*:io.sarl.maven.compiler*:io.sarl.javafx*", //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$
				"-group", "SARL Language", "io.sarl.lang*", //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$
				"-group", "Generic Tools for SARL Run-time Environments", "io.sarl.bootstrap*:io.sarl.sarlspecification", //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$
				"-group", "Janus Run-time Environment", "io.sarl.sre*", //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$
				"-group", "API Documentation Generator", "io.sarl.maven.docs*", //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$
				"-title", "The title", //$NON-NLS-1$ //$NON-NLS-2$
				//"-help",
			};
			final Class<?> type = Class.forName("jdk.javadoc.internal.tool.Main");  //$NON-NLS-1$
			final Method method = type.getDeclaredMethod("execute", String[].class); //$NON-NLS-1$
			method.setAccessible(true);
			method.invoke(null, (Object) params);
		} catch (Throwable ex) {
			throw new RuntimeException(getCause(ex));
		}
	}

	/** Replies the cause of the given exception.
	 *
	 * @param thr the exception.
	 * @return the cause.
	 */
	public static Throwable getCause(Throwable thr) {
		Throwable cause = thr.getCause();
		while (cause != null && cause != thr && cause != cause.getCause() && cause.getCause() != null) {
			cause = cause.getCause();
		}
		return cause == null ? thr : cause;
	}

}
