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
package io.sarl.lang.maven.compiler;

import static io.sarl.tests.api.tools.TestAssertions.assertEqualsExceptNewLines;
import static org.junit.jupiter.api.Assertions.assertNotNull;

import java.io.File;
import java.nio.file.FileSystems;
import java.nio.file.Files;
import java.nio.file.Path;

import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Tag;
import org.junit.jupiter.api.Test;

import io.sarl.lang.core.SARLVersion;
import io.sarl.lang.maven.compiler.utils.MavenHelper;
import io.sarl.lang.sarl.SarlPackage;
import io.sarl.tests.api.tools.AbstractEmbeddedMavenMojoTest;
import io.sarl.tests.api.tools.Verifier;

/**
 * @author <a href="http://www.ciad-lab.fr/stephane_galland">St&eacute;phane Galland</a>
 * @version sarl-maven-plugin.tests 0.13.0 20230919-093056
 * @mavengroupid io.sarl.lang
 * @mavenartifactid sarl-maven-plugin.tests
 */
@SuppressWarnings("all")
@DisplayName("Bug #504")
@Tag("maven")
@Tag("mvn-run")
public class Bug504Test extends AbstractEmbeddedMavenMojoTest {

	@Override
	protected String getPluginGroupId() {
		return MavenHelper.getSarlMavenPluginGroupId(getLogger());
	}

	@Override
	protected String getPluginArtefactId() {
		return MavenHelper.getSarlMavenPluginArtifactId(getLogger());
	}

	@Override
	protected String getPluginVersion() {
		return MavenHelper.getSarlMavenPluginVersion(getLogger());
	}

	@Test
	public void compile() throws Exception {
		Verifier verifier = executeMojo("bug504", "compile", false);
		verifier.assertErrorFreeLog();

		Path path = FileSystems.getDefault().getPath(
				"src", "main", "generated-sources", "sarl",
				"io", "sarl", "elevatorsim", "SimulatorInteraction.java");
		assertNotNull(path);
		verifier.assertFilePresent(path);
		File file = new File(verifier.getBaseDir(), path.toString());
		String fileContent = new String(Files.readAllBytes(file.toPath()));
		assertEqualsExceptNewLines(multilineString(
				"package io.sarl.elevatorsim;",
				"",
				"import io.sarl.elevatorsim.SimulatorPush;",
				"import io.sarl.elevatorsim.events.SendCarAction;",
				"import io.sarl.lang.core.Agent;",
				"import io.sarl.lang.core.Skill;",
				"import io.sarl.lang.core.annotation.SarlElementType;",
				"import io.sarl.lang.core.annotation.SarlSpecification;",
				"import io.sarl.lang.core.annotation.SyntheticMember;",
				"import java.io.IOException;",
				"import org.eclipse.xtext.xbase.lib.Exceptions;",
				"",
				"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
				"@SarlElementType(" + SarlPackage.SARL_SKILL + ")",
				"@SuppressWarnings(\"all\")",
				"public class SimulatorInteraction extends Skill implements SimulatorPush {",
				"  public void sendCar(final int a, final int b, final int c, final Object d, final Object e) {",
				"  }",
				"  ",
				"  @Override",
				"  public void pushSendCarAction(final SendCarAction action) {",
				"    try {",
				"      this.sendCar(action.car, action.floor, action.nextDirection, ",
				"        null, null);",
				"    } catch (final Throwable _t) {",
				"      if (_t instanceof IOException) {",
				"        final IOException e = (IOException)_t;",
				"        e.printStackTrace();",
				"      } else {",
				"        throw Exceptions.sneakyThrow(_t);",
				"      }",
				"    }",
				"  }",
				"  ",
				"  @SyntheticMember",
				"  public SimulatorInteraction() {",
				"    super();",
				"  }",
				"  ",
				"  @SyntheticMember",
				"  public SimulatorInteraction(final Agent arg0) {",
				"    super(arg0);",
				"  }",
				"}"), fileContent);
	}

}
