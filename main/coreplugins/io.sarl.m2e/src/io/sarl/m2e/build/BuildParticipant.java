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

package io.sarl.m2e.build;

import static io.sarl.m2e.Constants.SARL_ARTIFACT_ID;
import static io.sarl.m2e.Constants.SARL_GROUP_ID;
import static io.sarl.m2e.Constants.SARL_LANG_BUNDLE_NAME;
import static io.sarl.m2e.Constants.SARL_MAVENLIB_ARTIFACT_ID;
import static io.sarl.m2e.Constants.SARL_MAVENLIB_GROUP_ID;
import static io.sarl.m2e.Constants.SARL_PLUGIN_ARTIFACT_ID;
import static io.sarl.m2e.Constants.SARL_PLUGIN_GROUP_ID;

import java.text.MessageFormat;
import java.util.Map;
import java.util.Map.Entry;
import java.util.Objects;
import java.util.Set;
import java.util.TreeMap;

import com.google.common.base.Strings;
import org.apache.maven.artifact.Artifact;
import org.apache.maven.artifact.ArtifactUtils;
import org.eclipse.aether.graph.DependencyNode;
import org.eclipse.aether.graph.DependencyVisitor;
import org.eclipse.core.resources.IProject;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.Platform;
import org.eclipse.core.runtime.SubMonitor;
import org.eclipse.m2e.core.MavenPlugin;
import org.eclipse.m2e.core.project.IMavenProjectFacade;
import org.eclipse.m2e.core.project.configurator.AbstractBuildParticipant;
import org.eclipse.m2e.core.project.configurator.AbstractBuildParticipant2;
import org.osgi.framework.Bundle;
import org.osgi.framework.Version;
import org.sonatype.plexus.build.incremental.BuildContext;

import io.sarl.eclipse.util.Utilities;
import io.sarl.m2e.utils.M2EUtilities;

/** Build participant for detecting invalid versions of SARL components.
 *
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 */
public class BuildParticipant extends AbstractBuildParticipant2 {

	private static final int NSTEPS = 4;

	private final boolean isEclipsePlugin;

	/** Construct a build participant.
	 *
	 * @param isEclipsePlugin indicates if the build participant is created for an Eclipse plugin project.
	 */
	public BuildParticipant(boolean isEclipsePlugin) {
		this.isEclipsePlugin = isEclipsePlugin;
	}

	@Override
	public Set<IProject> build(int kind, IProgressMonitor monitor) throws Exception {
		if (kind == AbstractBuildParticipant.AUTO_BUILD || kind == AbstractBuildParticipant.FULL_BUILD) {
			final SubMonitor subm = SubMonitor.convert(monitor, Messages.BuildParticipant_0, NSTEPS);
			getBuildContext().removeMessages(getMavenProjectFacade().getPomFile());
			subm.worked(1);
			validateSARLCompilerPlugin();
			subm.worked(2);
			if (!this.isEclipsePlugin) {
				validateSARLLibraryVersion();
			}
			subm.worked(3);
			validateSARLDependenciesVersions(subm.newChild(1));
			subm.worked(NSTEPS);
		}
		return null;
	}

	private Bundle validateSARLVersion(String groupId, String artifactId, String artifactVersion) {
		final Bundle bundle = Platform.getBundle(SARL_LANG_BUNDLE_NAME);
		if (bundle == null) {
			getBuildContext().addMessage(
					getMavenProjectFacade().getPomFile(),
					-1, -1,
					MessageFormat.format(Messages.BuildParticipant_4, SARL_LANG_BUNDLE_NAME),
					BuildContext.SEVERITY_ERROR,
					null);
			return bundle;
		}

		final Version bundleVersion = bundle.getVersion();
		if (bundleVersion == null) {
			getBuildContext().addMessage(
					getMavenProjectFacade().getPomFile(),
					-1, -1,
					MessageFormat.format(Messages.BuildParticipant_5, SARL_LANG_BUNDLE_NAME),
					BuildContext.SEVERITY_ERROR,
					null);
			return bundle;
		}

		final Version minVersion = new Version(bundleVersion.getMajor(), bundleVersion.getMinor(), 0);
		final Version maxVersion = new Version(bundleVersion.getMajor(), bundleVersion.getMinor() + 1, 0);
		assert minVersion != null && maxVersion != null;

		final Version mvnVersion = M2EUtilities.parseMavenVersion(artifactVersion);
		final int compare = Utilities.compareVersionToRange(mvnVersion, minVersion, maxVersion);
		if (compare < 0) {
			getBuildContext().addMessage(
					getMavenProjectFacade().getPomFile(),
					-1, -1,
					MessageFormat.format(Messages.BuildParticipant_6,
							groupId, artifactId, artifactVersion, minVersion.toString()),
					BuildContext.SEVERITY_ERROR,
					null);
		} else if (compare > 0) {
			getBuildContext().addMessage(
					getMavenProjectFacade().getPomFile(),
					-1, -1,
					MessageFormat.format(Messages.BuildParticipant_3,
							groupId, artifactId, artifactVersion, maxVersion.toString()),
					BuildContext.SEVERITY_ERROR,
					null);
		}
		return bundle;
	}

	/** Validate the version of the SARL library in the dependencies.
	 *
	 * <p>The test works for standard Java or Maven projects.
	 *
	 * <p>Caution: This function should not be called for Eclipse plugins.
	 *
	 * @throws CoreException if internal error occurs.
	 */
	protected void validateSARLLibraryVersion() throws CoreException {
		final Map<String, Artifact> artifacts = getMavenProjectFacade().getMavenProject().getArtifactMap();
		final Artifact artifact = artifacts.get(ArtifactUtils.versionlessKey(SARL_GROUP_ID, SARL_ARTIFACT_ID));
		if (artifact != null) {
			validateSARLVersion(SARL_GROUP_ID, SARL_ARTIFACT_ID, artifact.getVersion());
		} else {
			getBuildContext().addMessage(
					getMavenProjectFacade().getPomFile(),
					-1, -1,
					Messages.BuildParticipant_3,
					BuildContext.SEVERITY_ERROR,
					null);
		}
	}

	/** Validate the version of the SARL compiler in the Maven configuration.
	 *
	 * @return the SARL bundle.
	 * @throws CoreException if internal error occurs.
	 */
	protected Bundle validateSARLCompilerPlugin() throws CoreException {
		final Map<String, Artifact> plugins = getMavenProjectFacade().getMavenProject().getPluginArtifactMap();
		final Artifact pluginArtifact = plugins.get(ArtifactUtils.versionlessKey(SARL_PLUGIN_GROUP_ID,
				SARL_PLUGIN_ARTIFACT_ID));
		if (pluginArtifact == null) {
			getBuildContext().addMessage(
					getMavenProjectFacade().getPomFile(),
					-1, -1,
					Messages.BuildParticipant_2,
					BuildContext.SEVERITY_ERROR,
					null);
		} else {
			final String version = pluginArtifact.getVersion();
			if (Strings.isNullOrEmpty(version)) {
				getBuildContext().addMessage(
						getMavenProjectFacade().getPomFile(),
						-1, -1,
						Messages.BuildParticipant_2,
						BuildContext.SEVERITY_ERROR,
						null);
			} else {
				return validateSARLVersion(
						SARL_PLUGIN_GROUP_ID, SARL_PLUGIN_ARTIFACT_ID,
						version);
			}
		}
		return null;
	}

	/** Validate the versions of the libraries that are in the project dependencies have compatible versions
	 * with the specific dependencies of the SARL library.
	 *
	 * <p>The nearest-win strategy of the dependency resolver may select invalid version for artifacts
	 * that are used by the SARL libraries.
	 *
	 * @param monitor the progress monitor.
	 * @throws CoreException if internal error occurs.
	 */
	protected void validateSARLDependenciesVersions(IProgressMonitor monitor) throws CoreException {
		final SubMonitor subm = SubMonitor.convert(monitor, 3);

		final Map<String, String> neededArtifactVersions = new TreeMap<>();
		final IMavenProjectFacade facade = getMavenProjectFacade();
		final DependencyNode root = MavenPlugin.getMavenModelManager().readDependencyTree(
				facade, facade.getMavenProject(),
				Artifact.SCOPE_COMPILE,
				subm.newChild(1));
		final DependencyNode[] sarlNode = new DependencyNode[] {null};
		root.accept(new DependencyVisitor() {
			@Override
			public boolean visitLeave(DependencyNode node) {
				if (sarlNode[0] == null
						&& node.getDependency() != null
						&& Objects.equals(node.getDependency().getArtifact().getGroupId(), SARL_MAVENLIB_GROUP_ID)
						&& Objects.equals(node.getDependency().getArtifact().getArtifactId(), SARL_MAVENLIB_ARTIFACT_ID)) {
					sarlNode[0] = node;
					return false;
				}
				return true;
			}

			@Override
			public boolean visitEnter(DependencyNode node) {
				return sarlNode[0] == null;
			}
		});

		subm.worked(1);

		if (sarlNode[0] != null) {
			sarlNode[0].accept(new DependencyVisitor() {
				@Override
				public boolean visitLeave(DependencyNode node) {
					if (node.getDependency() != null) {
						final String grId = node.getDependency().getArtifact().getGroupId();
						final String arId = node.getDependency().getArtifact().getArtifactId();
						final String key = ArtifactUtils.versionlessKey(grId, arId);
						final String vers = neededArtifactVersions.get(key);
						if (vers == null
								|| M2EUtilities.compareMavenVersions(vers, node.getVersion().toString()) < 0) {
							neededArtifactVersions.put(key, node.getVersion().toString());
						}
					}
					return true;
				}

				@Override
				public boolean visitEnter(DependencyNode node) {
					return true;
				}
			});
		}

		subm.worked(2);
		final SubMonitor subm2 = SubMonitor.convert(subm, neededArtifactVersions.size());
		int i = 0;
		final Map<String, Artifact> artifacts = getMavenProjectFacade().getMavenProject().getArtifactMap();

		for (final Entry<String, String> neededDependency : neededArtifactVersions.entrySet()) {
			final Artifact artifact = artifacts.get(neededDependency.getKey());
			if (artifact != null) {
				final int cmp = M2EUtilities.compareMavenVersions(neededDependency.getValue(), artifact.getVersion());
				if (cmp > 1) {
					getBuildContext().addMessage(
							getMavenProjectFacade().getPomFile(),
							-1, -1,
							MessageFormat.format(
									Messages.BuildParticipant_1,
									artifact.getGroupId(),
									artifact.getArtifactId(),
									artifact.getVersion(),
									neededDependency.getValue()),
							BuildContext.SEVERITY_ERROR,
							null);
				}
			}
			subm2.worked(i);
			++i;
		}

		subm.worked(3);
	}
}
