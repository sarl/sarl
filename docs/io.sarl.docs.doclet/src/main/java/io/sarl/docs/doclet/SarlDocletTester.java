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

package io.sarl.docs.doclet;

import com.sun.tools.javadoc.Main;

import io.sarl.docs.doclet.utils.Utils;

/** Tester for the SARL Doclet.
 *
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 * @since 0.7
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
			System.setProperty("http.proxyHost", "proxy.utbm.fr"); //$NON-NLS-1$ //$NON-NLS-2$
			System.setProperty("http.proxyPort", "3128"); //$NON-NLS-1$ //$NON-NLS-2$
			Main.execute(new String[] {
				"-private", //$NON-NLS-1$
				"-source", "1.8", //$NON-NLS-1$ //$NON-NLS-2$
				"-doclet", SarlDoclet.class.getName(), //$NON-NLS-1$
				"-sourcepath", "/home/sgalland/git/sarl/main/coreplugins/io.sarl.lang.core/src", //$NON-NLS-1$ //$NON-NLS-2$
				//"-sourcepath", "/home/sgalland/git/sarl/main/apiplugins/io.sarl.core/src-gen", //$NON-NLS-1$ //$NON-NLS-2$
				//"-sourcepath", "/home/sgalland/git/sarl/main/coreplugins/io.sarl.lang.core/src:/home/sgalland/git/sarl/main/apiplugins/io.sarl.core/src-gen", //$NON-NLS-1$ //$NON-NLS-2$
				"-d", "/home/sgalland/tmp/gen-site", //$NON-NLS-1$ //$NON-NLS-2$
				"-subpackages", "io", //$NON-NLS-1$ //$NON-NLS-2$
				"-link", "http://docs.oracle.com/javase/8/docs/api/", //$NON-NLS-1$ //$NON-NLS-2$
				"-link", "http://download.eclipse.org/modeling/tmf/xtext/javadoc/2.9/", //$NON-NLS-1$ //$NON-NLS-2$
				"-tag", "mavengroupid", //$NON-NLS-1$ //$NON-NLS-2$
				"-tag", "mavenartifactid", //$NON-NLS-1$ //$NON-NLS-2$
				"-tag", "optionalparam", //$NON-NLS-1$ //$NON-NLS-2$
				"-tag", "fires", //$NON-NLS-1$ //$NON-NLS-2$
			});
		} catch (Throwable ex) {
			final Throwable cause = Utils.getCause(ex);
			throw new RuntimeException(cause);
		}
	}

}
