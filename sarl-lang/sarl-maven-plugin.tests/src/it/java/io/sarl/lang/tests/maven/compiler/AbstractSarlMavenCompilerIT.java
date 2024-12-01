/*
 * $Id$
 *
 * SARL is an general-purpose agent programming language.
 * More details on http://www.sarl.io
 *
 * Copyright (C) 2014-2024 SARL.io, the Original Authors and Main Authors
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
package io.sarl.lang.tests.maven.compiler;

import static io.sarl.tests.api.tools.TestUtils.multilineString;
import static org.junit.jupiter.api.Assertions.*;

import java.io.File;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.List;
import java.util.regex.Pattern;

import com.google.common.io.Files;
import org.apache.maven.shared.invoker.DefaultInvocationRequest;
import org.apache.maven.shared.invoker.DefaultInvoker;
import org.apache.maven.shared.invoker.InvokerLogger;
import org.apache.maven.shared.invoker.MavenInvocationException;
import org.arakhne.afc.vmutil.FileSystem;
import org.arakhne.afc.vmutil.OperatingSystem;
import org.eclipse.xtext.validation.Issue;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Tag;

import io.sarl.lang.core.SARLVersion;
import io.sarl.lang.tests.api.AbstractSarlTest;

/**
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 */
@SuppressWarnings("all")
public abstract class AbstractSarlMavenCompilerIT extends AbstractSarlTest {

	private static final String POM_CODE = multilineString(
			"<project xmlns=\"http://maven.apache.org/POM/4.0.0\" xmlns:xsi=\"http://www.w3.org/2001/XMLSchema-instance\" xsi:schemaLocation=\"http://maven.apache.org/POM/4.0.0 http://maven.apache.org/xsd/maven-4.0.0.xsd\">",
			"  <modelVersion>4.0.0</modelVersion>",
			"  <groupId>io.sarl.lang.tests.maven.compiler.example</groupId>",
			"  <artifactId>example_code</artifactId>",
			"  <version>0.0.1</version>",
			"  <properties>",
			"    <sarl.version>@SARL_VERSION@</sarl.version>",
			"    <target.jdk.version>@USER_JAVA_VERSION@</target.jdk.version>",
			"  </properties>",
			"  <dependencies>",
			"    <dependency>",
			"      <groupId>io.sarl.lang</groupId>",
			"      <artifactId>core</artifactId>",
			"      <version>${sarl.version}</version>",
			"    </dependency>",
			"  </dependencies>",
			"  <build>",
			"    <plugins>",
			"      <plugin>",
			"        <groupId>org.apache.maven.plugins</groupId>",
			"        <artifactId>maven-compiler-plugin</artifactId>",
			"        <version>3.13.0</version>",
			"        <configuration>",
			"          <source>${target.jdk.version}</source>",
			"          <target>${target.jdk.version}</target>",
			"          <encoding>${project.build.sourceEncoding}</encoding>",
			"        </configuration>",
			"      </plugin>",
			"      <plugin>",
			"        <groupId>io.sarl.lang</groupId>",
			"        <artifactId>sarl-maven-plugin</artifactId>",
			"        <version>${sarl.version}</version>",
			"        <extensions>true</extensions>",
			"        <configuration>",
			"          <source>${target.jdk.version}</source>",
			"        </configuration>",
			"      </plugin>",
			"    </plugins>",
			"  </build>",
			"</project>");

	protected File makeFolder(File root, String... elements) {
		File output = root;
		for (final String element : elements) {
			output = new File(output, element);
		}
		return output;
	}
	
	/**
	 * @author $Author: sgalland$
	 * @version $FullVersion$
	 * @mavengroupid $GroupId$
	 * @mavenartifactid $ArtifactId$
	 */
	protected interface Callback {

		void apply(File sarlcOutputDirectory, List<String> errorStream) throws Exception;

	}

	protected void runCompiler(String testId, String sarlCode, boolean expectedSuccess, Callback callback) throws Exception {
		File tempDirectory = FileSystem.createTempDirectory("sarltests_" + getClass().getSimpleName() + "_" + testId + "_", "tmp");
		try {
			// Create folders
			File sourceDirectory = makeFolder(tempDirectory, "src", "main", "sarl");
			sourceDirectory.mkdirs();
			// Create source file
			File sarlFile = makeFolder(sourceDirectory, "test.sarl");
			Files.write(sarlCode.getBytes(), sarlFile);
			// Create Maven pom
			File pomFile = makeFolder(tempDirectory, "pom.xml");
			Files.write(POM_CODE
					.replaceAll("@SARL_VERSION@", SARLVersion.SARL_RELEASE_VERSION_MAVEN)
					.replaceAll("@USER_JAVA_VERSION@", SARLVersion.MINIMAL_JDK_VERSION_FOR_SARL_COMPILATION_ENVIRONMENT)
					.getBytes(), pomFile);
			// Compile
			List<String> errorStream = new ArrayList<>();
			final int result = runMavenCompiler(tempDirectory, null, errorStream);
			if (expectedSuccess) {
				assertEquals(0, result, "Unexpected return code for the batch compiler");
			} else {
				assertNotEquals(0, result, "Unexpected return code for the batch compiler");
			}
			//
			File sarlcOutputDirectory = makeFolder(tempDirectory, "src", "main", "generated-sources", "sarl");
			callback.apply(sarlcOutputDirectory, errorStream);
		} finally {
			FileSystem.delete(tempDirectory);
		}
	}

	private int runMavenCompiler(File basePath, List<String> outputStream, List<String> errorStream) throws Exception {
		var request = new DefaultInvocationRequest();
		request.setBaseDirectory(basePath);
		request.setPomFile(new File(basePath, "pom.xml"));
		request.setGoals(Arrays.asList("clean", "compile"));
		request.setBatchMode(true);
		var invoker = new DefaultInvoker();
		invoker.setOutputHandler(line -> {
			if (outputStream != null) {
				outputStream.add(line);
			}
		});
		invoker.setErrorHandler(line -> {
			if (errorStream != null) {
				errorStream.add(line);
			}
		});
		invoker.setLogger(new InvokerLogger() {

			@Override
			public void debug(String message) {
				//
			}

			@Override
			public void debug(String message, Throwable throwable) {
				//
			}

			@Override
			public boolean isDebugEnabled() {
				return false;
			}

			@Override
			public void info(String message) {
				//
			}

			@Override
			public void info(String message, Throwable throwable) {
				//
			}

			@Override
			public boolean isInfoEnabled() {
				return false;
			}

			@Override
			public void warn(String message) {
				//
			}

			@Override
			public void warn(String message, Throwable throwable) {
				//
			}

			@Override
			public boolean isWarnEnabled() {
				return false;
			}

			@Override
			public void error(String message) {
				if (errorStream != null) {
					errorStream.add(message);
				}
			}

			@Override
			public void error(String message, Throwable throwable) {
				if (errorStream != null) {
					errorStream.add(message);
				}
			}

			@Override
			public boolean isErrorEnabled() {
				return true;
			}

			@Override
			public void fatalError(String message) {
				if (errorStream != null) {
					errorStream.add(message);
				}
			}

			@Override
			public void fatalError(String message, Throwable throwable) {
				if (errorStream != null) {
					errorStream.add(message);
				}
			}

			@Override
			public boolean isFatalErrorEnabled() {
				return true;
			}

			@Override
			public void setThreshold(int threshold) {
				//
			}

			@Override
			public int getThreshold() {
				return 0;
			}
			
		});
		var os = OperatingSystem.getCurrentOS();
		if (os.isUnixCompliant()) {
			invoker.setMavenHome(new File("", "usr"));
		} else {
			invoker.setMavenHome(findMavenHome());
		}
		var result = invoker.execute(request);
		if (result == null) {
			return 255;
		}
		var ex = result.getExecutionException();
		if (ex != null) {
			throw new Error(ex);
		}
		return result.getExitCode();
	}

	private static File findMavenHome() {
		for (final var path : System.getenv("PATH").split(Pattern.quote(File.pathSeparator))) {
			final var pathFile = FileSystem.convertStringToFile(path);
			final var command = FileSystem.join(pathFile, "mvn");
			if (command != null && command.isFile()) {
				return pathFile.getParentFile();
			}
		}
		return null;
	}
}
