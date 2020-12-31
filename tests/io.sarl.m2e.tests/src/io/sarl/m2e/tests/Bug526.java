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
package io.sarl.m2e.tests;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNotNull;
import static org.mockito.Mockito.spy;

import com.google.common.base.Strings;
import org.apache.maven.artifact.Artifact;
import org.eclipse.core.resources.IMarker;
import org.eclipse.core.runtime.Platform;
import org.eclipse.jdt.annotation.NonNullByDefault;
import org.junit.After;
import org.junit.Before;
import org.junit.Test;
import org.osgi.framework.Bundle;
import org.osgi.framework.Version;

import io.sarl.m2e.M2EUtilities;
import io.sarl.m2e.SARLMavenEclipsePlugin;
import io.sarl.tests.api.AbstractSarlTest;


/**
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 */
@SuppressWarnings("all")
public final class Bug526 extends AbstractSarlMavenTest {

	private static final String SARL_LANG_BUNDLE_NAME = "io.sarl.lang.core"; //$NON-NLS-1$

	@NonNullByDefault
	private String sarlBundleVersion;

	@NonNullByDefault
	private String janusBundleVersion;

	@Before
	public void setUp() throws Exception {
		Bundle bundle = Platform.getBundle(SARL_LANG_BUNDLE_NAME);
		Version osgiVersion = bundle.getVersion();
		this.sarlBundleVersion = Integer.toString(osgiVersion.getMajor())
				+ "." + Integer.toString(osgiVersion.getMinor())
				+ "." + Integer.toString(osgiVersion.getMicro());
		if (!Strings.isNullOrEmpty(osgiVersion.getQualifier())) {
			this.sarlBundleVersion += "-" + Artifact.SNAPSHOT_VERSION;
		}
		assertNotNullOrEmpty(this.sarlBundleVersion);
		this.janusBundleVersion = "2." + this.sarlBundleVersion;
		assertNotNullOrEmpty(this.janusBundleVersion);
	}
	
	@Test
	public void sarlApiVersionCompatibility_ko() throws Exception {
		IMarker[] errors = createMavenProject(multilineString(
				"<project xmlns=\"http://maven.apache.org/POM/4.0.0\" xmlns:xsi=\"http://www.w3.org/2001/XMLSchema-instance\" xsi:schemaLocation=\"http://maven.apache.org/POM/4.0.0 http://maven.apache.org/xsd/maven-4.0.0.xsd\">",
				"<modelVersion>4.0.0</modelVersion>",
				"<groupId>io.sarl.tests</groupId>",
				"<artifactId>testMaven</artifactId>",
				"<version>0.0.1-SNAPSHOT</version>",
				"<properties>",
				"	<janus.version>" + this.janusBundleVersion + "</janus.version>",
				"	<sarl.version>" + this.sarlBundleVersion + "</sarl.version>",
				"</properties>",
				"<build>",
				"	<sourceDirectory>src/main/sarl</sourceDirectory>",
				"	<testSourceDirectory>src/test/sarl</testSourceDirectory>",
				"	<resources>",
				"		<resource>",
				"			<directory>src/main/sarl</directory>",
				"			<excludes>",
				"				<exclude>**/*.java</exclude>",
				"			</excludes>",
				"		</resource>",
				"		<resource>",
				"			<directory>src/main/resources</directory>",
				"			<excludes>",
				"				<exclude>**/*.java</exclude>",
				"			</excludes>",
				"		</resource>",
				"		<resource>",
				"			<directory>src/main/generated-sources/sarl</directory>",
				"			<excludes>",
				"				<exclude>**/*.java</exclude>",
				"			</excludes>",
				"		</resource>",
				"	</resources>",
				"	<plugins>",
				"		<plugin>",
				"			<artifactId>maven-compiler-plugin</artifactId>",
				"			<configuration>",
				"				<source>1.8</source>",
				"				<target>1.8</target>",
				"			</configuration>",
				"		</plugin>",
				"		<plugin>",
				"			<groupId>io.sarl.maven</groupId>",
				"			<artifactId>sarl-maven-plugin</artifactId>",
				"			<version>${sarl.version}</version>",
				"			<extensions>true</extensions>",
				"			<configuration>",
				"				<source>1.8</source>",
				"				<target>1.8</target>",
				"				<encoding>UTF-8</encoding>",
				"			</configuration>",
				"		</plugin>",
				"	</plugins>",
				"</build>",
				"<dependencies>",
				"	<dependency>",
				"		<groupId>io.janusproject</groupId>",
				"		<artifactId>io.janusproject.kernel</artifactId>",
				"		<version>${janus.version}</version>",
				"	</dependency>",
				"	<dependency>",
				"		<groupId>org.arakhne.afc.core</groupId>",
				"		<artifactId>math</artifactId>",
				"		<version>13.0</version>",
				"	</dependency>",
				"</dependencies>",
				"</project>",
				""));
		assertContainsMarker("Unexpected version for the artifact com.google.guava:guava", errors);
	}

	@Test
	public void sarlApiVersionCompatibility_ok() throws Exception {
		IMarker[] errors = createMavenProject(multilineString(
				"<project xmlns=\"http://maven.apache.org/POM/4.0.0\" xmlns:xsi=\"http://www.w3.org/2001/XMLSchema-instance\" xsi:schemaLocation=\"http://maven.apache.org/POM/4.0.0 http://maven.apache.org/xsd/maven-4.0.0.xsd\">",
				"<modelVersion>4.0.0</modelVersion>",
				"<groupId>io.sarl.tests</groupId>",
				"<artifactId>testMaven</artifactId>",
				"<version>0.0.1-SNAPSHOT</version>",
				"<properties>",
				"	<janus.version>" + this.janusBundleVersion + "</janus.version>",
				"	<sarl.version>" + this.sarlBundleVersion + "</sarl.version>",
				"</properties>",
				"<build>",
				"	<sourceDirectory>src/main/sarl</sourceDirectory>",
				"	<testSourceDirectory>src/test/sarl</testSourceDirectory>",
				"	<resources>",
				"		<resource>",
				"			<directory>src/main/sarl</directory>",
				"			<excludes>",
				"				<exclude>**/*.java</exclude>",
				"			</excludes>",
				"		</resource>",
				"		<resource>",
				"			<directory>src/main/resources</directory>",
				"			<excludes>",
				"				<exclude>**/*.java</exclude>",
				"			</excludes>",
				"		</resource>",
				"		<resource>",
				"			<directory>src/main/generated-sources/sarl</directory>",
				"			<excludes>",
				"				<exclude>**/*.java</exclude>",
				"			</excludes>",
				"		</resource>",
				"	</resources>",
				"	<plugins>",
				"		<plugin>",
				"			<artifactId>maven-compiler-plugin</artifactId>",
				"			<configuration>",
				"				<source>1.8</source>",
				"				<target>1.8</target>",
				"			</configuration>",
				"		</plugin>",
				"		<plugin>",
				"			<groupId>io.sarl.maven</groupId>",
				"			<artifactId>sarl-maven-plugin</artifactId>",
				"			<version>${sarl.version}</version>",
				"			<extensions>true</extensions>",
				"			<configuration>",
				"				<source>1.8</source>",
				"				<target>1.8</target>",
				"				<encoding>UTF-8</encoding>",
				"			</configuration>",
				"		</plugin>",
				"	</plugins>",
				"</build>",
				"<dependencies>",
				"	<dependency>",
				"		<groupId>io.janusproject</groupId>",
				"		<artifactId>io.janusproject.kernel</artifactId>",
				"		<version>${janus.version}</version>",
				"	</dependency>",
				"	<dependency>",
				"		<groupId>org.arakhne.afc.core</groupId>",
				"		<artifactId>math</artifactId>",
				"		<version>13.0</version>",
				"	</dependency>",
				"</dependencies>",
				"<dependencyManagement>", 
				"	<dependencies>",
				"		<dependency>",
				"			<groupId>com.google.guava</groupId>",
				"			<artifactId>guava</artifactId>",
				"			<version>20.0</version>",
				"		</dependency>",
				"	</dependencies>",
				"</dependencyManagement>",
				"</project>",
				""));
		assertNotContainsMarker("Unexpected version for the artifact com.google.guava:guava", errors);
	}

}
