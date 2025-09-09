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
 * <thead><tr><th>Category</th><th>Description</th></tr></thead>
 * <tr><td>0</td><td>Regular source code</td></tr>
 * <tr><td>20</td><td>Regular resource</td></tr>
 * <tr><td>50</td><td>Generated source code</td></tr>
 * <tr><td>100</td><td>Source code for (unit) tests</td></tr>
 * <tr><td>120</td><td>Resource for (unit) tests</td></tr>
 * <tr><td>150</td><td>Generated source code for (unit) tests</td></tr>
 * <tr><td>200</td><td>Source code for integration tests</td></tr>
 * <tr><td>220</td><td>Resource for integration tests</td></tr>
 * <tr><td>250</td><td>Generated source code for integration tests</td></tr>
 * </table>
 *
 * @author $Author: sgalland$
 * @version eclipseextensions 0.15.0 20250909-115749
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
	protected static int categorizes(IPath path) {
		assert path != null;

		final var cnt = path.segmentCount();

		if (cnt >= 1 && SARLConfig.FOLDER_MAVEN_SRC_PREFIX.equals(path.segment(0))) {
			if (cnt >= 3 && SARLConfig.GENERATED_SOURCE_ROOT_FOLDER_SIMPLENAME.equals(path.segment(2))) {
				return GENERATED_SOURCE_SECTION;
			}
			if (cnt >= 2) {
				if (SARLConfig.TEST_FOLDER_SIMPLENAME.equals(path.segment(1))) {
					return subcategorizes(TEST_SECTION, path);
				}
				if (SARLConfig.INTEGRATION_TEST_FOLDER_SIMPLENAME.equals(path.segment(1))) {
					return subcategorizes(INTEGRATION_TEST_SECTION, path);
				}
			}
		}
		return subcategorizes(CODE_SECTION, path);
	}

	private static int subcategorizes(int section, IPath path) {
		final var cnt = path.segmentCount();
		if (cnt >= 3) {
			if (SARLConfig.SARL_FOLDER_SIMPLENAME.equals(path.segment(2))) {
				return section + SARL_SUBSECTION;
			}
			if (SARLConfig.RESOURCE_FOLDER_SIMPLENAME.equals(path.segment(2))) {
				return section + RESOURCE_SUBSECTION;
			}
		}
		return section + CODE_SUBSECTION;
	}

}
