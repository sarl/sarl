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

package io.sarl.eclipse.util;

import java.util.Iterator;

import com.google.common.base.Strings;
import org.eclipse.core.runtime.IPath;
import org.eclipse.jdt.core.IClasspathAttribute;
import org.eclipse.jdt.core.IClasspathEntry;
import org.eclipse.jdt.core.IType;
import org.eclipse.jdt.core.ITypeParameter;
import org.eclipse.jdt.core.JavaCore;
import org.eclipse.jdt.core.JavaModelException;
import org.eclipse.jdt.internal.core.ClasspathEntry;
import org.eclipse.jdt.internal.corext.util.JavaModelUtil;
import org.osgi.framework.Bundle;
import org.osgi.framework.Version;

import io.sarl.lang.SARLConfig;

/** Utilities.
 *
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 */
public final class Utilities {

	/** Empty string.
	 */
	public static final String EMPTY_STRING = ""; //$NON-NLS-1$

	private Utilities() {
		//
	}

	/** Replies if the given path is nested in one of the given paths.
	 *
	 * @param path the path to search for.
	 * @param rootPaths the root paths.
	 * @return {@code true} if the {@code path} is nested into one of the {@code rootPaths}.
	 * @since 0.10
	 */
	public static boolean isNested(IPath path, Iterator<IPath> rootPaths) {
		while (rootPaths.hasNext()) {
			final IPath other = rootPaths.next();
			if (other.isPrefixOf(path)) {
				return true;
			}
		}
		return false;
	}

	/** Null-safe version parser.
	 *
	 * @param version the version string.
	 * @return the version.
	 */
	public static Version parseVersion(String version) {
		if (!Strings.isNullOrEmpty(version)) {
			try {
				return Version.parseVersion(version);
			} catch (Throwable exception) {
				//
			}
		}
		return null;
	}

	/** Null-safe compare a version number to a range of version numbers.
	 *
	 * <p>The minVersion must be strictly lower to the maxVersion. Otherwise
	 * the behavior is not predictible.
	 *
	 * @param version the version to compare to the range; must not be {@code null}.
	 * @param minVersion the minimal version in the range (inclusive); could be {@code null}.
	 * @param maxVersion the maximal version in the range (exclusive); could be {@code null}.
	 * @return a negative number if the version in lower than the minVersion.
	 *     A positive number if the version is greater than or equal to the maxVersion.
	 *     <code>0</code> if the version is between minVersion and maxVersion.
	 */
	public static int compareVersionToRange(Version version, Version minVersion, Version maxVersion) {
		assert minVersion == null || maxVersion == null || minVersion.compareTo(maxVersion) < 0;
		if (version == null) {
			return Integer.MIN_VALUE;
		}
		if (minVersion != null && compareVersionsNoQualifier(version, minVersion) < 0) {
			return -1;
		}
		if (maxVersion != null && compareVersionsNoQualifier(version, maxVersion) >= 0) {
			return 1;
		}
		return 0;
	}

	private static int compareVersionsNoQualifier(Version firstVersion, Version secondVersion) {
		if (firstVersion == secondVersion) {
			return 0;
		}

		int result = firstVersion.getMajor() - secondVersion.getMajor();
		if (result != 0) {
			return result;
		}

		result = firstVersion.getMinor() - secondVersion.getMinor();
		if (result != 0) {
			return result;
		}

		return firstVersion.getMicro() - secondVersion.getMicro();
	}

	/** Null-safe comparison.
	 *
	 * @param <T> - type of the comparable element.
	 * @param object1 the first object.
	 * @param object2 the second object.
	 * @return Negative number if a lower than b.
	 *     Positive number if a greater than b.
	 * <code>0</code> if a is equal to b.
	 */
	public static <T> int compareTo(Comparable<T> object1, T object2) {
		if (object1 == object2) {
			return 0;
		}
		if (object1 == null) {
			return Integer.MIN_VALUE;
		}
		if (object2 == null) {
			return Integer.MAX_VALUE;
		}
		assert object1 != null && object2 != null;
		return object1.compareTo(object2);
	}

	/** Replies the fully qualified name with generic parameters.
	 *
	 * @param type the type. Never {@code null}.
	 * @return the qualified name.
	 */
	public static String getNameWithTypeParameters(IType type) {
		assert type != null;
		final String superName = type.getFullyQualifiedName('.');
		if (!JavaModelUtil.is50OrHigher(type.getJavaProject())) {
			return superName;
		}
		try {
			final ITypeParameter[] typeParameters = type.getTypeParameters();
			if (typeParameters != null && typeParameters.length > 0) {
				final StringBuffer buf = new StringBuffer(superName);
				buf.append('<');
				for (int k = 0; k < typeParameters.length; ++k) {
					if (k != 0) {
						buf.append(',').append(' ');
					}
					buf.append(typeParameters[k].getElementName());
				}
				buf.append('>');
				return buf.toString();
			}
		} catch (JavaModelException e) {
			// ignore
		}
		return superName;

	}

	/** Create the classpath library linked to the bundle with the given name.
	 *
	 * @param bundle the bundle to point to. Never {@code null}.
	 * @param precomputedBundlePath the path to the bundle that is already available. If {@code null},
	 *      the path is computed from the bundle with {@link BundleUtil}.
	 * @param javadocURLs the mappings from the bundle to the javadoc URL. It is used for linking the javadoc to the bundle if
	 *      the bundle platform does not know the Javadoc file. If {@code null}, no mapping is defined.
	 * @return the classpath entry.
	 */
	public static IClasspathEntry newLibraryEntry(Bundle bundle, IPath precomputedBundlePath, BundleURLMappings javadocURLs) {
		assert bundle != null;
		final IPath bundlePath;
		if (precomputedBundlePath == null) {
			bundlePath = BundleUtil.getBundlePath(bundle);
		} else {
			bundlePath = precomputedBundlePath;
		}
		final IPath sourceBundlePath = BundleUtil.getSourceBundlePath(bundle, bundlePath);
		final IPath javadocPath = BundleUtil.getJavadocBundlePath(bundle, bundlePath);

		final IClasspathAttribute[] extraAttributes;
		if (javadocPath == null) {
			if (javadocURLs != null) {
				final String url = javadocURLs.getURLForBundle(bundle);
				if (!Strings.isNullOrEmpty(url)) {
					final IClasspathAttribute attr = JavaCore.newClasspathAttribute(
							IClasspathAttribute.JAVADOC_LOCATION_ATTRIBUTE_NAME,
							url);
					extraAttributes = new IClasspathAttribute[] {attr};
				} else {
					extraAttributes = ClasspathEntry.NO_EXTRA_ATTRIBUTES;
				}
			} else {
				extraAttributes = ClasspathEntry.NO_EXTRA_ATTRIBUTES;
			}
		} else {
			final IClasspathAttribute attr = JavaCore.newClasspathAttribute(
					IClasspathAttribute.JAVADOC_LOCATION_ATTRIBUTE_NAME,
					javadocPath.makeAbsolute().toOSString());
			extraAttributes = new IClasspathAttribute[] {attr};
		}

		return JavaCore.newLibraryEntry(
				bundlePath,
				sourceBundlePath,
				null,
				null,
				extraAttributes,
				false);
	}

	/** Create the classpath output location.
	 *
	 * @param bundle the bundle to point to. Never {@code null}.
	 * @param precomputedBundlePath the path to the bundle that is already available. If {@code null},
	 *      the path is computed from the bundle with {@link BundleUtil}.
	 * @param javadocURLs the mappings from the bundle to the javadoc URL. It is used for linking the javadoc to the bundle if
	 *      the bundle platform does not know the Javadoc file. If {@code null}, no mapping is defined.
	 * @return the classpath entry.
	 */
	public static IClasspathEntry newOutputClasspathEntry(Bundle bundle, IPath precomputedBundlePath, BundleURLMappings javadocURLs) {
		assert bundle != null;
		final IPath bundlePath;
		if (precomputedBundlePath == null) {
			bundlePath = BundleUtil.getBundlePath(bundle);
		} else {
			bundlePath = precomputedBundlePath;
		}
		final IPath sourceBundlePath = BundleUtil.getSourceBundlePath(bundle, bundlePath);
		final IPath javadocPath = BundleUtil.getJavadocBundlePath(bundle, bundlePath);

		final IClasspathAttribute[] extraAttributes;
		if (javadocPath == null) {
			if (javadocURLs != null) {
				final String url = javadocURLs.getURLForBundle(bundle);
				if (!Strings.isNullOrEmpty(url)) {
					final IClasspathAttribute attr = JavaCore.newClasspathAttribute(
							IClasspathAttribute.JAVADOC_LOCATION_ATTRIBUTE_NAME,
							url);
					extraAttributes = new IClasspathAttribute[] {attr};
				} else {
					extraAttributes = ClasspathEntry.NO_EXTRA_ATTRIBUTES;
				}
			} else {
				extraAttributes = ClasspathEntry.NO_EXTRA_ATTRIBUTES;
			}
		} else {
			final IClasspathAttribute attr = JavaCore.newClasspathAttribute(
					IClasspathAttribute.JAVADOC_LOCATION_ATTRIBUTE_NAME,
					javadocPath.makeAbsolute().toOSString());
			extraAttributes = new IClasspathAttribute[] {attr};
		}

		return new ClasspathEntry(ClasspathEntry.K_OUTPUT, IClasspathEntry.CPE_LIBRARY,
				bundlePath,
				ClasspathEntry.INCLUDE_ALL,
                ClasspathEntry.EXCLUDE_NONE,
                sourceBundlePath,
                null, null, false, null,
                false,
                extraAttributes);
	}

	/** Define a mapping from bundles to URLs.
	 *
	 * @author $Author: sgalland$
	 * @version $FullVersion$
	 * @mavengroupid $GroupId$
	 * @mavenartifactid $ArtifactId$
	 */
	@FunctionalInterface
	public interface BundleURLMappings {

		/** Replies the URL for the given bundle.
		 *
		 * @param bundle the bundle, never {@code null}.
		 * @return the URL, or {@code null} if no URL is defined.
		 */
		String getURLForBundle(Bundle bundle);

	}

	/** Define a mapping from bundles to URLs.
	 *
	 * @author $Author: sgalland$
	 * @version $FullVersion$
	 * @mavengroupid $GroupId$
	 * @mavenartifactid $ArtifactId$
	 */
	public static class SARLBundleJavadocURLMappings implements BundleURLMappings {

		private static final String SARL_PREFIX = "io.sarl."; //$NON-NLS-1$

		@Override
		public String getURLForBundle(Bundle bundle) {
			if (bundle.getSymbolicName().startsWith(SARL_PREFIX)) {
				return SARLConfig.JAVADOC_URL;
			}
			return null;
		}

	}

}
