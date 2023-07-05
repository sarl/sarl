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
 */
package io.sarl.eclipse.m2e.tests;

import static io.sarl.tests.api.tools.TestAssertions.*;
import static io.sarl.tests.api.tools.TestUtils.*;
import static org.junit.jupiter.api.Assumptions.*;


import com.google.common.base.Strings;
import org.eclipse.core.resources.IMarker;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.Platform;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Tag;
import org.junit.jupiter.api.Test;
import org.osgi.framework.Bundle;
import org.osgi.framework.Version;

import io.sarl.tests.api.TestScope;


/**
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 */
@DisplayName("SARL Project configurator")
@Tag("unit")
@Tag("eclipse")
@Tag("eclipse-unit")
@SuppressWarnings("all")
public final class SARLProjectConfiguratorTest extends AbstractSarlMavenTest {

	private static final String SARL_LANG_BUNDLE_NAME = "io.sarl.lang.core"; //$NON-NLS-1$

	private String bundleVersion;
	
	@BeforeEach
	public void setUp() throws Exception {
		Bundle bundle = Platform.getBundle(SARL_LANG_BUNDLE_NAME);
		Version osgiVersion = bundle.getVersion();
		this.bundleVersion = Integer.toString(osgiVersion.getMajor())
				+ "." + Integer.toString(osgiVersion.getMinor())
				+ "." + Integer.toString(osgiVersion.getMicro());
		if (!Strings.isNullOrEmpty(osgiVersion.getQualifier())) {
			this.bundleVersion += "-SNAPSHOT";
		}
		assertNotNullOrEmpty(this.bundleVersion);
	}
	
	@Test
	@DisplayName("With compatible SARL API")
	public void sarlApiVersionCompatibility_ok() throws Exception {
		IMarker[] errors = createMavenProject(multilineString(
				"<project xmlns=\"http://maven.apache.org/POM/4.0.0\" xmlns:xsi=\"http://www.w3.org/2001/XMLSchema-instance\"",
				"         xsi:schemaLocation=\"http://maven.apache.org/POM/4.0.0 http://maven.apache.org/xsd/maven-4.0.0.xsd\">",
				"<modelVersion>4.0.0</modelVersion>",
				"<groupId>io.sarl.m2e.tests</groupId>",
				"<artifactId>test</artifactId>",
				"<version>1.0-SNAPSHOT</version>",
				"<dependencies>",
				"    <dependency>",
				"        <groupId>io.sarl.lang</groupId>",
				"        <artifactId>io.sarl.lang.core</artifactId>",
				"        <version>" + this.bundleVersion.toString() + "</version>",
				"    </dependency>",
				"</dependencies>",
				"<build>",
				"    <plugins>",
				"        <plugin>",
				"            <groupId>io.sarl.maven</groupId>",
				"            <artifactId>sarl-maven-plugin</artifactId>",
				"            <version>" + this.bundleVersion.toString() + "</version>",
				"            <extensions>true</extensions>",
				"            <configuration>",
				"                <source>1.8</source>",
				"                <target>1.8</target>",
				"            </configuration>",
				"        </plugin>",
				"    </plugins>",
				"</build>",
				"</project>",
				""));
		assertNotContainsMarker("has a too old version", errors);
		assertNotContainsMarker("has a too early version", errors);
	}

	@Test
	@DisplayName("With too old SARL API 1")
	public void sarlApiVersionCompatibility_ko1() throws Exception {
		try {
			IMarker[] errors = createMavenProject(multilineString(
					"<project xmlns=\"http://maven.apache.org/POM/4.0.0\" xmlns:xsi=\"http://www.w3.org/2001/XMLSchema-instance\"",
					"         xsi:schemaLocation=\"http://maven.apache.org/POM/4.0.0 http://maven.apache.org/xsd/maven-4.0.0.xsd\">",
					"<modelVersion>4.0.0</modelVersion>",
					"<groupId>io.sarl.m2e.tests</groupId>",
					"<artifactId>test</artifactId>",
					"<version>1.0-SNAPSHOT</version>",
					"<dependencies>",
					"    <dependency>",
					"        <groupId>io.sarl.lang</groupId>",
					"        <artifactId>io.sarl.lang.core</artifactId>",
					"        <version>" + this.bundleVersion.toString() + "</version>",
					"    </dependency>",
					"</dependencies>",
					"<build>",
					"    <plugins>",
					"        <plugin>",
					"            <groupId>io.sarl.maven</groupId>",
					"            <artifactId>sarl-maven-plugin</artifactId>",
					"            <version>0.2.0</version>",
					"            <extensions>true</extensions>",
					"            <configuration>",
					"                <source>1.8</source>",
					"                <target>1.8</target>",
					"            </configuration>",
					"        </plugin>",
					"    </plugins>",
					"</build>",
					"</project>",
					""));
			assumeMavenRunning(errors);
			assertContainsMarker("has a too old version", errors);
			assertNotContainsMarker("has a too early version", errors);
		} catch (CoreException e) {
			// Thrown when Maven is unable to run the test
			abort(e.getLocalizedMessage());
		}
	}

	@Test
	@DisplayName("With too old SARL API 2")
	public void sarlApiVersionCompatibility_ko2() throws Exception {
		try {
			IMarker[] errors = createMavenProject(multilineString(
					"<project xmlns=\"http://maven.apache.org/POM/4.0.0\" xmlns:xsi=\"http://www.w3.org/2001/XMLSchema-instance\"",
					"         xsi:schemaLocation=\"http://maven.apache.org/POM/4.0.0 http://maven.apache.org/xsd/maven-4.0.0.xsd\">",
					"<modelVersion>4.0.0</modelVersion>",
					"<groupId>io.sarl.m2e.tests</groupId>",
					"<artifactId>test</artifactId>",
					"<version>1.0-SNAPSHOT</version>",
					"<dependencies>",
					"    <dependency>",
					"        <groupId>io.sarl.lang</groupId>",
					"        <artifactId>io.sarl.lang.core</artifactId>",
					"        <version>0.2.0</version>",
					"    </dependency>",
					"</dependencies>",
					"<build>",
					"    <plugins>",
					"        <plugin>",
					"            <groupId>io.sarl.maven</groupId>",
					"            <artifactId>sarl-maven-plugin</artifactId>",
					"            <version>" + this.bundleVersion.toString() + "</version>",
					"            <extensions>true</extensions>",
					"            <configuration>",
					"                <source>1.8</source>",
					"                <target>1.8</target>",
					"            </configuration>",
					"        </plugin>",
					"    </plugins>",
					"</build>",
					"</project>",
					""));
			assumeMavenRunning(errors);
			assertContainsMarker("has a too old version", errors);
			assertNotContainsMarker("has a too early version", errors);
		} catch (CoreException e) {
			// Thrown when Maven is unable to run the test
			abort(e.getLocalizedMessage());
		}
	}

}