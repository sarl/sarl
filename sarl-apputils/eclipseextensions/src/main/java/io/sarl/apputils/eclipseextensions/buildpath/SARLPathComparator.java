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
 */

package io.sarl.apputils.eclipseextensions.buildpath;

import java.util.Comparator;

import org.eclipse.core.runtime.IPath;

import io.sarl.lang.SARLConfig;

/** Comparator of paths according to the standard SARL project structure.
 *
 * <p>The order assumed by this comparator is based on:
 * <table>
 * <caption>Order index per category of resource</caption>
 * <thead><tr><th>Category</th><th>Subcategory</th><th>Description</th><th>Example</th></tr></thead>
 * <tr><td><strong>0</strong></td><td></td><td><strong>Regular code</strong></td><td></td></tr>
 * <tr><td></td><td>0</td><td>SARL code</td><td><code>.../src/main/sarl</code></td></tr>
 * <tr><td></td><td>20</td><td>Other code</td><td><code>.../src/main/...</code></td></tr>
 * <tr><td></td><td>40</td><td>Resources</td><td><code>.../src/main/resources</code></td></tr>
 * <tr><td><strong>200</strong></td><td></td><td><strong>Unit tests</strong></td><td></td></tr>
 * <tr><td></td><td>200</td><td>SARL tests</td><td><code>.../src/test/sarl</code></td></tr>
 * <tr><td></td><td>220</td><td>Other tests</td><td><code>.../src/test/...</code></td></tr>
 * <tr><td></td><td>240</td><td>Test resources</td><td><code>.../src/test/resources</code></td></tr>
 * <tr><td><strong>400</strong></td><td></td><td><strong>Integration tests</strong></td><td></td></tr>
 * <tr><td></td><td>400</td><td>SARL integration tests</td><td><code>.../src/it/sarl</code></td></tr>
 * <tr><td></td><td>420</td><td>Other integration tests</td><td><code>.../src/it/...</code></td></tr>
 * <tr><td></td><td>440</td><td>Test integration resources</td><td><code>.../src/it/resources</code></td></tr>
 * <tr><td><strong>600</strong></td><td></td><td><strong>Generated code</strong></td><td></td></tr>
 * <tr><td></td><td>600</td><td>Any generated code</td><td><code>.../generated-sources/...</code></td></tr>
 * </table>
 *
 * @author <a href="http://www.ciad-lab.fr/stephane_galland">St&eacute;phane Galland</a>
 * @version eclipseextensions 0.15.1 20250911-224825
 * @mavengroupid io.sarl.apputils
 * @mavenartifactid eclipseextensions
 * @since 0.15
 */
public class SARLPathComparator implements Comparator<IPath> {

	private static SARLPathComparator singleton;

	private static final int CODE_SECTION = 0;

	private static final int TEST_SECTION = 200;

	private static final int INTEGRATION_TEST_SECTION = 400;

	private static final int GENERATED_SOURCE_SECTION = 600;
	
	private static final int SARL_SUBSECTION = 0;

	private static final int CODE_SUBSECTION = 20;

	private static final int RESOURCE_SUBSECTION = 40;

	/** Replies the singleton instance for this comparator.
	 *
	 * @return the comparator, never {@code null}.
	 */
	public static SARLPathComparator getSingleton() {
		if (singleton == null) {
			singleton = new SARLPathComparator();
		}
		return singleton;
	}

	@Override
	public int compare(IPath path1, IPath path2) {
		if (path1 == path2) {
			return 0;
		}
		if (path1 == null) {
			return Integer.MIN_VALUE;
		}
		if (path2 == null) {
			return Integer.MAX_VALUE;
		}

		final var class1 = categorizes(path1);
		final var class2 = categorizes(path2);
		if (class1 != class2) {
			return class1 - class2;
		}

		final var name1 = path1.toPortableString();
		final var name2 = path2.toPortableString();
		return name1.compareTo(name2);
	}

	/** Replies the category of the given path according to the category table that is described in the document of this class.
	 *
	 * @param path the path to analyze. It must not be {@code null}.
	 * @return the category according to the category table.
	 */
	public static int categorizes(IPath path) {
		assert path != null;

		final var cnt = path.segmentCount();

		if (cnt >= 2) {
			final var segment0 = path.segment(cnt - 2);
			if (cnt >= 3 && SARLConfig.FOLDER_MAVEN_SRC_PREFIX.equals(path.segment(cnt - 3))) {
				// .../src/*/*
				if (SARLConfig.TEST_FOLDER_SIMPLENAME.equals(segment0)) {
					return subcategorizes(TEST_SECTION, path);
				} else if (SARLConfig.INTEGRATION_TEST_FOLDER_SIMPLENAME.equals(segment0)) {
					return subcategorizes(INTEGRATION_TEST_SECTION, path);
				}
			} else if (SARLConfig.GENERATED_SOURCE_ROOT_FOLDER_SIMPLENAME.equals(segment0)) {
				return GENERATED_SOURCE_SECTION;
			}
		}
		return subcategorizes(CODE_SECTION, path);
	}

	private static int subcategorizes(int section, IPath path) {
		final var cnt = path.segmentCount();
		if (cnt >= 0) {
			final var lastSegment = path.segment(cnt - 1);
			if (SARLConfig.SARL_FOLDER_SIMPLENAME.equals(lastSegment)) {
				return section + SARL_SUBSECTION;
			}
			if (SARLConfig.RESOURCE_FOLDER_SIMPLENAME.equals(lastSegment)) {
				return section + RESOURCE_SUBSECTION;
			}
		}
		return section + CODE_SUBSECTION;
	}

}
