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

package io.sarl.docs.doclet.j11;

import jdk.javadoc.internal.tool.Main;

/** Tester for the SARL Doclet.
 *
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 * @since 0.11
 */
public final class SarlDocletTester {

	private SarlDocletTester() {
		//
	}

	/** Run the Javadoc generator.
	 *
	 * @param args arguments.
	 * @throws RuntimeException a runtime exception.
	 */
	@SuppressWarnings("checkstyle:all")
	public static void main(String[] args) {
		try {
			//System.setProperty("http.proxyHost", "proxy.utbm.fr"); //$NON-NLS-1$ //$NON-NLS-2$
			//System.setProperty("http.proxyPort", "3128"); //$NON-NLS-1$ //$NON-NLS-2$
			Main.execute(new String[] {
				"-private", //$NON-NLS-1$
				"-source", "11", //$NON-NLS-1$ //$NON-NLS-2$
				"-doclet", SarlDoclet.class.getName(), //$NON-NLS-1$
				"-sourcepath", "/home/sgalland/git/sarl.dsl/main/coreplugins/io.sarl.lang.core/src", //$NON-NLS-1$ //$NON-NLS-2$
				//"-sourcepath", "/home/sgalland/git/sarl/main/apiplugins/io.sarl.core/src-gen", //$NON-NLS-1$ //$NON-NLS-2$
				//"-sourcepath", "/home/sgalland/git/sarl/main/coreplugins/io.sarl.lang.core/src:/home/sgalland/git/sarl/main/apiplugins/io.sarl.core/src-gen", //$NON-NLS-1$ //$NON-NLS-2$
				"-d", "/home/sgalland/tmp/gen-site", //$NON-NLS-1$ //$NON-NLS-2$
				"-subpackages", "io", //$NON-NLS-1$ //$NON-NLS-2$
				"-link", "http://docs.oracle.com/javase/11/docs/api/", //$NON-NLS-1$ //$NON-NLS-2$
				"-tag", "mavengroupid", //$NON-NLS-1$ //$NON-NLS-2$
				"-tag", "mavenartifactid", //$NON-NLS-1$ //$NON-NLS-2$
				"-tag", "optionalparam", //$NON-NLS-1$ //$NON-NLS-2$
				"-tag", "fires", //$NON-NLS-1$ //$NON-NLS-2$
				"-nohelp", //$NON-NLS-1$
			});
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
