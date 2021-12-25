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

package io.sarl.docs.doclet2;

import java.lang.reflect.Method;

/** Tester for the SARL Doclet.
 *
 * <p>This utility class launch the Javadoc tool with the SARL doclet. Paths are hard coded.
 *
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
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
	@SuppressWarnings("checkstyle:all")
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
				"-copyright", "ABC",
				"-group", "Standard Development Kit and Other Utilities", "io.sarl.core*:io.sarl.util*:io.sarl.api*:io.sarl.maven.bootiqueapp*:io.sarl.maven.compiler*:io.sarl.javafx*",
				"-group", "SARL Language", "io.sarl.lang*",
				"-group", "Generic Tools for SARL Run-time Environments", "io.sarl.bootstrap*:io.sarl.sarlspecification",
				"-group", "Janus Run-time Environment", "io.sarl.sre*",
				"-group", "API Documentation Generator", "io.sarl.maven.docs*",
				"-title", "The title",
				//"-help",
			};
			final Class<?> type = Class.forName("jdk.javadoc.internal.tool.Main");  //$NON-NLS-1$
			final Method method = type.getDeclaredMethod("execute", String[].class);
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
