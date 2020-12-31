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
package io.sarl.eclipse.tests.util;

import static org.junit.Assert.*;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

import com.google.common.base.Objects;
import org.eclipse.core.runtime.IPath;
import org.eclipse.core.runtime.Platform;
import org.junit.Assume;
import org.junit.Before;
import org.junit.ComparisonFailure;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.junit.runners.Suite;
import org.junit.runners.Suite.SuiteClasses;
import org.osgi.framework.Bundle;
import org.osgi.framework.Version;

import io.sarl.eclipse.util.BundleUtil;
import io.sarl.eclipse.util.BundleUtil.IBundleDependencies;
import io.sarl.eclipse.util.Utilities.BundleURLMappings;
import io.sarl.lang.SARLVersion;
import io.sarl.tests.api.AbstractSarlTest;
import io.sarl.tests.api.TestScope;

/**
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 */
@RunWith(Suite.class)
@SuiteClasses({
	BundleUtilTest.PathTests.class,
	BundleUtilTest.ResolverTests.class,
})
@SuppressWarnings("all")
public class BundleUtilTest {

	/**
	 * @author $Author: sgalland$
	 * @version $FullVersion$
	 * @mavengroupid $GroupId$
	 * @mavenartifactid $ArtifactId$
	 */
	public static class PathTests extends AbstractSarlTest {

		private static void assertEndsWith(String expected, String actual) {
			if (actual == null || !actual.endsWith(expected)) {
				throw new ComparisonFailure("Invalid postfix", expected, actual);
			}
		}
		
		@Test
		public void getBundlePath() {
			Bundle bundle = Platform.getBundle("io.sarl.lang.core");
			Assume.assumeNotNull(bundle);
			//
			IPath path = BundleUtil.getBundlePath(bundle);
			assertNotNull(path);
			if (isEclipseRuntimeEnvironment()) {
				assertEndsWith("io.sarl.lang.core/target/classes/", path.toPortableString());
			} else {
				assertEquals("io.sarl.lang.core-" + SARLVersion.SARL_RELEASE_VERSION_MAVEN + ".jar", path.lastSegment());
			}
		}

		@Test
		public void getJavadocBundlePath() {
			Bundle bundle = Platform.getBundle("io.sarl.lang.core");
			Assume.assumeNotNull(bundle);
			IPath bundlePath = BundleUtil.getBundlePath(bundle);
			Assume.assumeNotNull(bundlePath);
			//
			IPath path = BundleUtil.getJavadocBundlePath(bundle, bundlePath);
			if (isEclipseRuntimeEnvironment()) {
				assertNotNull(path);
				assertEndsWith("io.sarl.lang.core/", path.toPortableString());
			} else {
				assertNull(path);
			}
		}

		@Test
		public void getSourceBundlePath() {
			Bundle bundle = Platform.getBundle("io.sarl.lang.core");
			Assume.assumeNotNull(bundle);
			IPath bundlePath = BundleUtil.getBundlePath(bundle);
			Assume.assumeNotNull(bundlePath);
			//
			IPath path = BundleUtil.getSourceBundlePath(bundle, bundlePath);
			if (isEclipseRuntimeEnvironment()) {
				assertNotNull(path);
				assertEndsWith("io.sarl.lang.core/", path.toPortableString());
			} else {
				assertNull(path);
			}
		}

	}

	/**
	 * @author $Author: sgalland$
	 * @version $FullVersion$
	 * @mavengroupid $GroupId$
	 * @mavenartifactid $ArtifactId$
	 */
	public static class ResolverTests extends AbstractSarlTest {

		@Test
		public void resolveBundleDependenciesBundleBundleURLMappingsStringArray_noRootDependencies() {
			Bundle bundle = Platform.getBundle("io.sarl.lang.core");
			Assume.assumeNotNull(bundle);
			//
			IBundleDependencies dependencies = BundleUtil.resolveBundleDependencies(bundle, (BundleURLMappings) null);
			assertNotNull(dependencies);
			assertEquals("io.sarl.lang.core", dependencies.getBundleSymbolicName());
			assertOsgiVersionEquals(Version.parseVersion(SARLVersion.SARL_RELEASE_VERSION_OSGI), dependencies.getBundleVersion());
			assertPartlyContains(dependencies.getDirectSymbolicNames(),
					"io.sarl.lang.core",
					"javax.inject",
					"org.eclipse.xtext.xbase.lib");
			assertPartlyContains(dependencies.getTransitiveSymbolicNames(false),
					"io.sarl.lang.core",
					"javax.inject",
					"org.eclipse.xtext.xbase.lib",
					"com.google.guava",
					"org.eclipse.osgi");
			if (isEclipseRuntimeEnvironment()) {
				assertPartlyContains(dependencies.getTransitiveSymbolicNames(true),
						"io.sarl.lang.core",
						"org.eclipse.xtext.xbase.lib",
						"javax.inject",
						"com.google.guava",
						"org.eclipse.osgi",
						"org.eclipse.osgi.compatibility.state"
						);
			} else {
				assertPartlyContains(dependencies.getTransitiveSymbolicNames(true),
						"io.sarl.lang.core",
						"javax.inject",
						"org.eclipse.xtext.xbase.lib",
						"com.google.guava",
						"org.eclipse.osgi");
			}
		}

		@Test
		public void resolveBundleDependenciesBundleBundleURLMappingsStringArray_rootDependencies() {
			Bundle bundle = Platform.getBundle("io.sarl.lang.core");
			Assume.assumeNotNull(bundle);
			//
			IBundleDependencies dependencies = BundleUtil.resolveBundleDependencies(bundle, (BundleURLMappings) null,
					"org.eclipse.xtext.xbase.lib");
			assertNotNull(dependencies);
			assertEquals("io.sarl.lang.core", dependencies.getBundleSymbolicName());
			assertOsgiVersionEquals(Version.parseVersion(SARLVersion.SARL_RELEASE_VERSION_OSGI), dependencies.getBundleVersion());
			assertPartlyContains(dependencies.getDirectSymbolicNames(),
					"io.sarl.lang.core",
					"org.eclipse.xtext.xbase.lib");
			assertPartlyContains(dependencies.getTransitiveSymbolicNames(false),
					"io.sarl.lang.core",
					"org.eclipse.xtext.xbase.lib",
					"com.google.guava",
					"org.eclipse.osgi");
			if (isEclipseRuntimeEnvironment()) {
				assertPartlyContains(dependencies.getTransitiveSymbolicNames(true),
						"io.sarl.lang.core",
						"org.eclipse.xtext.xbase.lib",
						"com.google.guava",
						"org.eclipse.osgi",
						"org.eclipse.osgi.compatibility.state"
						);
			} else {
				assertPartlyContains(dependencies.getTransitiveSymbolicNames(true),
						"io.sarl.lang.core",
						"org.eclipse.xtext.xbase.lib",
						"com.google.guava",
						"org.eclipse.osgi");
			}
		}

	}

}
