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

import java.text.MessageFormat;

import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Tag;
import org.junit.jupiter.api.Test;

import io.sarl.lang.maven.compiler.abstractmojos.Messages;
import io.sarl.lang.maven.compiler.utils.MavenHelper;
import io.sarl.tests.api.tools.AbstractEmbeddedMavenMojoTest;
import io.sarl.tests.api.tools.Verifier;

/**
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 */
@SuppressWarnings("all")
@DisplayName("mojo: compile")
@Tag("maven")
@Tag("mvn-run")
public class CompileMojoTest extends AbstractEmbeddedMavenMojoTest {

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
	@DisplayName("Valid SARL project 1")
	public void invalidXtext() throws Exception {
		final Verifier verifier = executeMojo("prj1", "compile", false);
		verifier.assertErrorFreeLog();
	}

	@Test
	@DisplayName("Invalid SARL SDK")
	public void invalidSdk() throws Exception {
		executeMojoWithError("prj2", "compile", true, MessageFormat.format(Messages.AbstractCompileMojo_6, ""));
	}

	@Test
	@DisplayName("Valid SARL project 2")
	public void validCode() throws Exception {
		final Verifier verifier = executeMojo("prj3", "compile", true);
		verifier.assertErrorFreeLog();
	}

}